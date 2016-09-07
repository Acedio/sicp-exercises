(define-syntax cons-stream (syntax-rules () ((cons-stream head rest) (cons head (delay rest)))))
(define (car-stream stream) (car stream))
(define (cdr-stream stream) (force (cdr stream)))

; ex 3.59
(define (integrate-series series)
  (define (iter den series)
    (cons-stream
      (/ (car-stream series) den)
      (iter (+ 1 den) (cdr-stream series))))
  (iter 1 series))

(define (print-first-n n stream)
  (if (> n 0)
    (begin
      (display (car-stream stream))
      (newline)
      (print-first-n (- n 1) (cdr-stream stream)))
    'done))

(define exp-series
  (cons-stream
    1 (integrate-series exp-series)))

(define (negate-series series)
  (cons-stream (- (car-stream series))
               (negate-series (cdr-stream series))))

(define cosine-series
  (cons-stream 1 (negate-series (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (resolve-series-to-exponent series exponent x)
  (define (iter series left powered)
    (if (< left 0)
      0
      (+ (* (car-stream series) powered)
         (iter (cdr-stream series) (- left 1) (* powered x)))))
  (iter series exponent 1))

(define ones
  (cons-stream 1 ones))

(define (scale-stream s c)
  (cons-stream
    (* c (car-stream s))
    (scale-stream (cdr-stream s) c)))

(define (add-streams s1 s2)
  (cons-stream
    (+ (car-stream s1)
       (car-stream s2))
    (add-streams
      (cdr-stream s1)
      (cdr-stream s2))))

; ex 3.60
(define (mul-series s1 s2)
  ; We don't find (cdr-stream s1) or (cdr-stream s2) here because
  ; that would screw up the necessary delayed evaluation.
  (let ((c1 (car-stream s1))
        (c2 (car-stream s2)))
    (cons-stream
      (* c1 c2)
      (add-streams
        (cons-stream 0 (mul-series (cdr-stream s1) (cdr-stream s2)))
        (add-streams
          (scale-stream (cdr-stream s1) c2)
          (scale-stream (cdr-stream s2) c1))))))

; ex 3.61
(define (invert-unit-series s)
  (define inverted-unit-series
    (cons-stream
      1
      (scale-stream
        (mul-series (cdr-stream s)
                    inverted-unit-series)
        -1)))
  inverted-unit-series)

; ex 3.62
(define (invert-non-unit-series s)
  (define inverted-unit-series
    (if (eq? 0 (car-stream s))
      (error "Cannot invert stream with zero constant.")
      (let ((inv-c (/ 1 (car-stream s))))
        (cons-stream
          inv-c
          (scale-stream
            (mul-series (cdr-stream s)
                        inverted-unit-series)
            (- inv-c))))))
  inverted-unit-series)

(define (div-series num den)
  (mul-series num (invert-non-unit-series den)))

(define tangent-series (div-series sine-series cosine-series))

(define (stream-map fn stream)
  (cons-stream
    (fn (car-stream stream))
    (stream-map fn (cdr-stream stream))))

(define (sqrt-improve guess x)
  ;(display "improve ")
  (/ (+ guess (/ x guess)) 2))

(define (bad-sqrt-stream x)
  (cons-stream
    1.0
    (stream-map
      (lambda (guess)
        (sqrt-improve guess x))
      (bad-sqrt-stream x))))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map
        (lambda (guess)
          (sqrt-improve guess x))
        guesses)))
  guesses)

; 3.64
(define (stream-limit s tolerance)
  (define (stream-limit-impl s0 s1 rest)
    (if (<= (abs (- s0 s1)) tolerance)
      s1
      (stream-limit-impl s1 (car-stream rest) (cdr-stream rest))))
  (let ((s0 (car-stream s))
        (rest (cdr-stream s)))
    (stream-limit-impl s0 (car-stream rest) (cdr-stream rest))))

; 3.65
(define (terms-impl denom)
  (let ((next (if (> denom 0)
                (- (+ 1 denom))
                (+ 1 (- denom)))))
    (cons-stream (/ 1 denom) (terms-impl next))))
(define ln2-terms
  (terms-impl 1))

(define (partial-sums s)
  (define (impl s sum)
    (let ((new-sum (+ sum (car-stream s))))
      (cons-stream 
        new-sum 
        (impl (cdr-stream s) new-sum))))
  (impl s 0))

(define ln2-stream
  (partial-sums ln2-terms))

; 3.66
; (x, y), x and y >= 1
; if x == y then n = 2^y - 1
; if x > y then n = 2^y - 1 + 2^(y-1) + (x - y - 1)*2^y

(define (stream-null? s)
  (null? s))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (car-stream s1)
       (interleave s2 (cdr-stream s1)))))

(define (louis-pairs s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (car-stream s) x))
    t)
   (louis-pairs (cdr-stream s)
                (cdr-stream t))))

(define integers
  (cons-stream 1 (add-streams ones integers)))

; 3.68
; louis-pairs hits a stack overflow because both arguments to `interleave` must
; resolve before being passed. Resolving the second argument, (louis-pairs ...),
; requires yet another set of arguments to resolve, involving another call to
; louis-pairs, and so on and so forth. Stack overflowwwww.

(define (pairs s t)
  (cons-stream 
    (list (car-stream s) (car-stream t))
    (interleave
     (stream-map
      (lambda (x) 
        (list (car-stream s) x))
      (cdr-stream t))
     (pairs (cdr-stream s)
            (cdr-stream t)))))

(define (triples s t u)
  (cons-stream
    (list (car-stream s) (car-stream t) (car-stream u))
    (interleave
      (interleave
        (stream-map
          (lambda (x)
            (list (car-stream s) (car-stream t) x))
          (cdr-stream u))
        (stream-map
          (lambda (x)
            (cons (car-stream s) x))
          (pairs (cdr-stream t) (cdr-stream u))))
      (triples (cdr-stream s) (cdr-stream t) (cdr-stream u)))))

(define (wiki-triples s t u)
  (cons-stream
    (list (car-stream s) (car-stream t) (car-stream u))
    (interleave
      (stream-map
        (lambda (x)
          (cons (car-stream s) x))
        (cdr-stream (pairs t u)))
      (wiki-triples (cdr-stream s) (cdr-stream t) (cdr-stream u)))))

(define (filter-stream s pred)
  (if (pred (car-stream s))
    (cons-stream (car-stream s)
                 (filter-stream (cdr-stream s) pred))
    (filter-stream (cdr-stream s) pred)))

(define (square x)
  (* x x))

(define (is-pythagorean-triple t)
  (= (+ (square (car t)) (square (cadr t))) (square (caddr t))))

; ex 3.69
(define pythagorean-triples
  (filter-stream (triples integers integers integers) is-pythagorean-triple))

(define (stream-find s pred)
  (define (iter s pred n)
    (if (stream-null? s)
      (cons -1 '())
      (if (pred (car-stream s))
        (cons n (car-stream s))
        (iter (cdr-stream s) pred (+ n 1)))))
  (iter s pred 0))

; ex 3.73
; Cheated on this one because I completely forgot how approximate integration works :P
(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ 1 C))
                v0
                dt))))

; ex 3.74
(define (sign-change-detector i0 i1)
  (cond ((and (< i0 0)
              (>= i1 0))
         1)
        ((and (>= i0 0)
              (< i1 0))
         -1)
        (else 0)))

; Alyssa's initial version
(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (car-stream input-stream) 
    last-value)
   (make-zero-crossings 
    (cdr-stream input-stream)
    (car-stream input-stream))))

(define (multi-stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map car-stream argstreams))
       (apply multi-stream-map
              (cons proc 
                    (map cdr-stream
                         argstreams))))))

(define zero-crossings
  (multi-stream-map sign-change-detector 
                    sense-data 
                    (cdr-stream sense-data)))
