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
