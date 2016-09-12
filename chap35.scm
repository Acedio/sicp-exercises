(define-syntax cons-stream (syntax-rules () ((cons-stream head rest) (cons head (delay rest)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

; ex 3.59
(define (integrate-series series)
  (define (iter den series)
    (cons-stream
      (/ (stream-car series) den)
      (iter (+ 1 den) (stream-cdr series))))
  (iter 1 series))

(define (print-first-n n stream)
  (if (> n 0)
    (begin
      (display (stream-car stream))
      (newline)
      (print-first-n (- n 1) (stream-cdr stream)))
    'done))

(define exp-series
  (cons-stream
    1 (integrate-series exp-series)))

(define (negate-series series)
  (cons-stream (- (stream-car series))
               (negate-series (stream-cdr series))))

(define cosine-series
  (cons-stream 1 (negate-series (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (resolve-series-to-exponent series exponent x)
  (define (iter series left powered)
    (if (< left 0)
      0
      (+ (* (stream-car series) powered)
         (iter (stream-cdr series) (- left 1) (* powered x)))))
  (iter series exponent 1))

(define ones
  (cons-stream 1 ones))

(define (scale-stream s c)
  (cons-stream
    (* c (stream-car s))
    (scale-stream (stream-cdr s) c)))

(define (add-streams s1 s2)
  (cons-stream
    (+ (stream-car s1)
       (stream-car s2))
    (add-streams
      (stream-cdr s1)
      (stream-cdr s2))))

; ex 3.60
(define (mul-series s1 s2)
  ; We don't find (stream-cdr s1) or (stream-cdr s2) here because
  ; that would screw up the necessary delayed evaluation.
  (let ((c1 (stream-car s1))
        (c2 (stream-car s2)))
    (cons-stream
      (* c1 c2)
      (add-streams
        (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
        (add-streams
          (scale-stream (stream-cdr s1) c2)
          (scale-stream (stream-cdr s2) c1))))))

; ex 3.61
(define (invert-unit-series s)
  (define inverted-unit-series
    (cons-stream
      1
      (scale-stream
        (mul-series (stream-cdr s)
                    inverted-unit-series)
        -1)))
  inverted-unit-series)

; ex 3.62
(define (invert-non-unit-series s)
  (define inverted-unit-series
    (if (eq? 0 (stream-car s))
      (error "Cannot invert stream with zero constant.")
      (let ((inv-c (/ 1 (stream-car s))))
        (cons-stream
          inv-c
          (scale-stream
            (mul-series (stream-cdr s)
                        inverted-unit-series)
            (- inv-c))))))
  inverted-unit-series)

(define (div-series num den)
  (mul-series num (invert-non-unit-series den)))

(define tangent-series (div-series sine-series cosine-series))

(define (stream-map fn stream)
  (cons-stream
    (fn (stream-car stream))
    (stream-map fn (stream-cdr stream))))

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
      (stream-limit-impl s1 (stream-car rest) (stream-cdr rest))))
  (let ((s0 (stream-car s))
        (rest (stream-cdr s)))
    (stream-limit-impl s0 (stream-car rest) (stream-cdr rest))))

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
    (let ((new-sum (+ sum (stream-car s))))
      (cons-stream 
        new-sum 
        (impl (stream-cdr s) new-sum))))
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
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (louis-pairs s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (louis-pairs (stream-cdr s)
                (stream-cdr t))))

(define integers
  (cons-stream 1 (add-streams ones integers)))

; 3.68
; louis-pairs hits a stack overflow because both arguments to `interleave` must
; resolve before being passed. Resolving the second argument, (louis-pairs ...),
; requires yet another set of arguments to resolve, involving another call to
; louis-pairs, and so on and so forth. Stack overflowwwww.

(define (pairs s t)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (interleave
     (stream-map
      (lambda (x) 
        (list (stream-car s) x))
      (stream-cdr t))
     (pairs (stream-cdr s)
            (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (interleave
        (stream-map
          (lambda (x)
            (list (stream-car s) (stream-car t) x))
          (stream-cdr u))
        (stream-map
          (lambda (x)
            (cons (stream-car s) x))
          (pairs (stream-cdr t) (stream-cdr u))))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (wiki-triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map
        (lambda (x)
          (cons (stream-car s) x))
        (stream-cdr (pairs t u)))
      (wiki-triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (filter-stream s pred)
  (if (pred (stream-car s))
    (cons-stream (stream-car s)
                 (filter-stream (stream-cdr s) pred))
    (filter-stream (stream-cdr s) pred)))

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
      (if (pred (stream-car s))
        (cons n (stream-car s))
        (iter (stream-cdr s) pred (+ n 1)))))
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
(define (sign-change-detector current last)
  (cond ((and (< last 0)
              (>= current 0))
         1)
        ((and (>= last 0)
              (< current 0))
         -1)
        (else 0)))

; Alyssa's initial version
(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (stream-car input-stream) 
    last-value)
   (make-zero-crossings 
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define (multi-stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply multi-stream-map
              (cons proc 
                    (map stream-cdr
                         argstreams))))))

(define (zero-crossings sense-data)
  (multi-stream-map sign-change-detector 
                    sense-data 
                    (stream-cdr sense-data)))

; ex 3.75
; Louis sets last-value = avpt, which causes the average to be used in the next average.
(define (make-zero-crossings 
         input-stream last-value last-avpt)
  (let ((avpt 
         (/ (+ (stream-car input-stream) 
               last-value) 
            2)))
    (cons-stream 
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings 
      (stream-cdr input-stream) (stream-car input-stream) avpt))))

; ex 3.76
(define (smooth input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
      avpt
      (smooth (stream-cdr input-stream)
              (stream-car input-stream)))))

; ex 3.77
(define (integral
         delayed-integrand initial-value dt)
  (cons-stream 
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral 
          (stream-cdr integrand)
          (+ (* dt (stream-car integrand))
             initial-value)
          dt)))))

; ex 3.81
(define (xor-bits bits x)
  (define (iter bits result)
    (if (null? bits)
      result
      (iter (cdr bits)
            (not (eq? (logbit? (car bits) x)
                      result)))))
  (iter bits #f))

(define (lfsr-next max bits x)
  (modulo (+ (* 2 x)
             (if (xor-bits bits x)
               1
               0))
          max))

(define (lfsr-stream max bits seed)
  (let ((next (lfsr-next max bits seed)))
    (cons-stream
      next
      (lfsr-stream max bits next))))

(define (lfsr-16bit-stream seed)
  (lfsr-stream 65536 (list 15 14 12 3) seed))

(define (rand-update x)
  (lfsr-next 65536 (list 15 14 12 3) x))

(define (rand commands)
  (define (process-next commands last-rand)
    (if (null? commands)
      '()
      (let ((next-rand (if (eq? (stream-car commands) 'generate)
                         (rand-update last-rand)
                         (stream-car commands))))
        (cons-stream next-rand
                     (process-next (stream-cdr commands) next-rand)))))
  (process-next commands 0))

(define (rand-int-stream seed)
  (define stream
    (cons-stream
      (rand-update seed)
      (stream-map rand-update stream)))
  stream)

(define (rand-stream seed)
  (scale-stream (rand-int-stream seed) (/ 1.0 65536)))

; ex 3.82
; Both the cesaro stream and the integral stream seem to be broken.
; Running the Monte carlo on them winds up with extremely round numbers.
; TODO: Fix it :P
(define (monte-carlo-stream experiment-stream)
  (define (iter experiment-stream passed total)
    (let ((new-passed (if (stream-car experiment-stream)
                        (+ passed 1)
                        passed))
          (new-total (+ total 1)))
      (cons-stream
        (/ new-passed new-total)
        (iter (stream-cdr experiment-stream) new-passed new-total))))
  (iter experiment-stream 0 0))

(define (stream-ref n s)
  (if (= n 0)
    (stream-car s)
    (stream-ref (- n 1) (stream-cdr s))))

(define (grab-n n s)
  (if (= n 0)
    (cons-stream
      '()
      s)
    (let ((result (grab-n (- n 1) (stream-cdr s))))
      (cons-stream
        (cons (stream-car s)
              (stream-car result))
        (stream-cdr result)))))

(define (groups-of-n n s)
  (let ((result (grab-n n s)))
    (cons-stream
      (stream-car result)
      (groups-of-n n (stream-cdr result)))))

(define (cesaro-test r1 r2)
   (= (gcd r1 r2) 1))

(define cesaro-stream
  (stream-map
    (lambda (rands)
      (apply cesaro-test rands))
    (groups-of-n 2 (rand-int-stream 2342))))

(define (integral-test pred x1 x2 y1 y2)
  (lambda (r1 r2)
    (let ((x-range (- x2 x1))
          (y-range (- y2 y1)))
      (let ((x (+ (* r1 x-range) x1))
            (y (+ (* r2 y-range) y1)))
        (pred x y)))))

(define (integral-test-stream pred x1 x2 y1 y2)
  (define test
    (integral-test pred x1 x2 y1 y2))
  (stream-map (lambda (rands)
                (test (car rands) (cadr rands)))
              (groups-of-n 2 (rand-stream 3452))))

(define (circle-pred x y)
  (<= (+ (square (- x 5.0)) (square (- y 7.0))) (square 3.0)))
