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

; ex 3.23

(define (make-node prev next data)
  (list prev next data))

(define (get-node-prev node)
  (car node))
(define (set-node-prev! node prev)
  (set-car! node prev))

(define (get-node-next node)
  (cadr node))
(define (set-node-next! node next)
  (set-car! (cdr node) next))

(define (get-node-data node)
  (caddr node))
(define (set-node-data! node data)
  (set-car! (cddr node) data))

(define (make-deque)
  (cons '() '()))
(define (empty-deque? deque)
  (null? (car deque)))
(define (front-deque deque)
  (if (empty-deque? deque)
    (error "front-deque on empty deque!")
    (get-node-data (car deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "rear-deque on empty deque!")
    (get-node-data (cdr deque))))

(define (print-deque deque)
  (define (iter node)
    (if (null? node)
      '()
      (begin
        (display (get-node-data node))
        (display " ")
        (iter (get-node-next node)))))
  (if (empty-deque? deque)
    (display "empty!")
    (iter (car deque))))

(define (front-insert-deque! deque data)
  (if (empty-deque? deque)
    (begin
      (set-car! deque (make-node '() '() data))
      (set-cdr! deque (car deque)))
    (let ((next (car deque)))
      (set-car! deque (make-node '() next data))
      (set-node-prev! next (car deque)))))
(define (front-delete-deque! deque)
  (if (empty-deque? deque)
    (error "front-delete-deque! on empty deque!")
    (let ((next (get-node-next (car deque))))
      (set-car! deque next)
      (if (empty-deque? deque)
        '()
        (set-node-prev! next '())))))

(define (rear-insert-deque! deque data)
  (if (empty-deque? deque)
    (begin
      (set-car! deque (make-node '() '() data))
      (set-cdr! deque (car deque)))
    (let ((prev (cdr deque)))
      (set-cdr! deque (make-node prev '() data))
      (set-node-next! prev (cdr deque)))))
(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
    (error "rear-delete-deque! on empty deque!")
    (let ((prev (get-node-prev (cdr deque))))
      (if (null? prev)
        (set-car! deque '())
        (begin
          (set-cdr! deque prev)
          (set-node-next! prev '()))))))
