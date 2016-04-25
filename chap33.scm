; ex 3.17

(define (bad-count-pairs x)
  (if (not (pair? x))
      0
      (+ (bad-count-pairs (car x))
         (bad-count-pairs (cdr x))
         1)))

(define (count-pairs x)
  (define (contains col item)
    (if (not (pair? col))
      #f
      (if (eq? (car col)
               item)
        #t
        (contains (cdr col) item))))
  (let ((seen '()))
    (define (iter cur)
      (if (and (pair? cur)
               (not (contains seen cur)))
        (begin (set! seen (cons cur seen))
               (+ (iter (car cur))
                  (iter (cdr cur))
                  1))
        0))
    (iter x)))

(define c (cons '() (cons 1 2)))
(set-car! c (cons (cdr c) (cdr c)))

(define d3 (cons 1 2))
(define d2 (cons d3 d3))
(define d1 (cons d2 d2))
