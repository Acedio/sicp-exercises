(define (make-rat n d)
  (let ((divisor (gcd n d)))
    (if (< d 0)
      (cons (- (/ n divisor))
            (- (/ d divisor)))
      (cons (/ n divisor)
            (/ d divisor)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x)
                  (denom y))
               (* (numer y)
                  (denom x)))
            (* (denom x)
               (denom y))))

(define (sub-rat x y)
  (add-rat x
           (make-rat (- (numer y))
                     (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x)
               (numer y))
            (* (denom x)
               (denom y))))

(define (div-rat x y)
  (mul-rat x
           (make-rat (denom y)
                     (numer y))))

(define (equal-rat? x y)
  (= (* (numer x)
        (denom y))
     (* (numer y)
        (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define (prod-cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (divides? x y)
  (= 0 (modulo y x)))

(define (num-factors factor num)
  (if (divides? factor num)
    (+ 1 (num-factors factor (/ num factor)))
    0))

(define (prod-car z)
  (num-factors 2 z))

(define (prod-cdr z)
  (num-factors 3 z))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (church-add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval a b)
  (make-interval (+ (lower-bound a)
                    (lower-bound b))
                 (+ (upper-bound a)
                    (upper-bound b))))

(define (sub-interval a b)
  (make-interval (- (lower-bound a)
                    (upper-bound b))
                 (- (upper-bound a)
                    (lower-bound b))))

(define (interval-lt? interval val)
  (< (upper-bound interval) val))

(define (interval-gt? interval val)
  (> (lower-bound interval) val))

(define (interval-contains? interval val)
  (and (<= (lower-bound interval) val)
       (>= (upper-bound interval) val)))

(define (mul-interval x y)
  (cond ((interval-lt? x 0)
         (cond ((interval-lt? y 0)
                (make-interval (* (upper-bound x)
                                  (upper-bound y))
                               (* (lower-bound x)
                                  (lower-bound y))))
               ((interval-gt? y 0)
                (make-interval (* (lower-bound x)
                                  (upper-bound y))
                               (* (upper-bound x)
                                  (lower-bound y))))
               (else (make-interval (* (lower-bound x)
                                       (upper-bound y))
                                    (* (lower-bound x)
                                       (lower-bound y))))))
        ((interval-gt? x 0)
         (cond ((interval-lt? y 0)
                (make-interval (* (upper-bound x)
                                  (lower-bound y))
                               (* (lower-bound x)
                                  (upper-bound y))))
               ((interval-gt? y 0)
                (make-interval (* (lower-bound x)
                                  (lower-bound y))
                               (* (upper-bound x)
                                  (upper-bound y))))
               (else (make-interval (* (upper-bound x)
                                       (lower-bound y))
                                    (* (upper-bound x)
                                       (upper-bound y))))))
        (else
          (cond ((interval-lt? y 0)
                 (make-interval (* (upper-bound x)
                                   (lower-bound y))
                                (* (lower-bound x)
                                   (lower-bound y))))
                ((interval-gt? y 0)
                 (make-interval (* (lower-bound x)
                                   (upper-bound y))
                                (* (upper-bound x)
                                   (upper-bound y))))
                (else 
                  (let ((lxly (* (lower-bound x)
                                 (lower-bound y)))
                        (lxuy (* (lower-bound x)
                                 (upper-bound y)))
                        (uxly (* (upper-bound x)
                                 (lower-bound y)))
                        (uxuy (* (upper-bound x)
                                 (upper-bound y))))
                    (make-interval (min lxuy uxly)
                                   (max lxly uxuy))))))))

(define (div-interval x y)
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
    (error "wtf")
    (mul-interval x 
                  (make-interval 
                   (/ 1.0 (upper-bound y)) 
                   (/ 1.0 (lower-bound y))))))

; (define lt0 (make-interval -3 -2))
; (define gt0 (make-interval 4 5))
; (define contains0 (make-interval -10 11))

(define (make-center-percent center tolerance)
  (let ((half-width (* (abs center) tolerance)))
    (make-interval (- center half-width)
                   (+ center half-width))))

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(define (reverse l)
  (define (iter left reversed)
    (if (null? left)
      reversed
      (iter (cdr left)
            (cons (car left) reversed))))
  (iter l '()))

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (first-denomination cvs)
    (car cvs))
  (define (except-first-denomination cvs)
    (cdr cvs))
  (define (no-more? cvs)
    (null? cvs))
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (same-parity . w)
  (define (only-parity parity left)
    (cond ((null? left) '())
          ((= parity (modulo (car left) 2))
           (cons (car left)
                 (only-parity parity (cdr left))))
          (else
            (only-parity parity (cdr left)))))
  (if (null? w)
    '()
    (cons (car w)
          (only-parity (modulo (car w) 2)
                       (cdr w)))))

(define (deep-reverse l)
  (define (iter left result)
    (cond ((null? left) result)
          ((not (pair? left)) (error "wrong type"))
          ((pair? (car left)) (iter (cdr left) (cons (deep-reverse (car left)) result)))
          (else (iter (cdr left) (cons (car left) result)))))
  (iter l '()))

(define (fringe tree)
  (cond ((null? tree) `())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define test-mobile
  (make-mobile (make-branch 10
                            (make-mobile (make-branch 5 10)
                                         (make-branch 5 10)))
               (make-branch 20 10)))

(define (mobile-balanced? mobile)
  (cond ((null? mobile) #t)
        ((not (pair? mobile)) #t)
        (else (let ((left-structure (branch-structure (left-branch mobile)))
                    (right-structure (branch-structure (right-branch mobile))))
                (and
                  (mobile-balanced? left-structure)
                  (mobile-balanced? right-structure)
                  (= (* (total-weight left-structure)
                        (branch-length (left-branch mobile)))
                     (* (total-weight right-structure)
                        (branch-length (right-branch mobile)))))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (acc-map f seq)
  (accumulate (lambda (x y)
                (cons (f x)
                      y))
              '() seq))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acc-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* 2 higher-terms)))
    0
    coefficient-sequence))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (x)
                     (cond ((pair? x) (count-leaves x))
                           (else 1)))
                   t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (reverse-fr sequence)
  (fold-right
    (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-fl sequence)
  (fold-left
    (lambda (x y) (append (list y) x)) '() sequence))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval i j)
  (if (> i j)
    '()
    (cons i (enumerate-interval (+ i 1) j))))

(define (prime? n)
  (define (divides? x y)
    (= 0 (modulo y x)))
  (define (find-divisor n test)
    (cond ((> (* test test) n)
           n)
          ((divides? test n)
           test)
          (else (find-divisor n (+ test 1)))))
  (= n (find-divisor n 2)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (cond ((< n 2) '())
        (else (append (map (lambda (x)
                             (list x n))
                           (enumerate-interval 1 (- n 1)))
                      (unique-pairs (- n 1))))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

(define (unique-triples n)
  (cond ((< n 3) '())
        (else (append (map (lambda (x)
                             (append x (list n)))
                           (unique-pairs (- n 1)))
                      (unique-triples (- n 1))))))

(define (triple-sums n s)
  (filter (lambda (x)
            (= s (accumulate + 0 x)))
          (unique-triples n)))

(define (queens board-size)
  (define empty-board '())
  (define (safe? positions)
    (define (collide? new old)
      (or (= (cadr new) (cadr old))
          (= (abs (- (car new) (car old)))
             (abs (- (cadr new) (cadr old))))))
    (null? (filter (lambda (position)
                 (collide? position (car positions)))
               (cdr positions))))
  (define (adjoin-position new-row k rest-of-queens)
    (cons (list k new-row) rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))

(define (split large-compose small-compose)
  (define (iter painter n)
    (if (= n 0)
      painter
      (let ((smaller (iter painter (- n 1))))
        (large-compose painter
                (small-compose smaller smaller)))))
  iter)

(define (make-vect x y)
  (list x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cadr v))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))
(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (ycor-vect a) (ycor-vect b))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

(define (make-segment start end)
  (list start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))
(define (midpoint-segment segment)
  (scale-vect 0.5 (add-vect (start-segment segment) (end-segment segment))))

(define (draw-line start end)
  (display start)
  (display '->)
  (display end)
  (display "\n"))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

(define (make-segment-loop points)
  (if (null? points)
    '()
    (let ((first (car points)))
      (define (iter point rest)
        (if (null? rest)
          (list (make-segment point first))
          (cons (make-segment point (car rest))
                (iter (car rest) (cdr rest)))))
      (iter (car points) (cdr points)))))

(define (make-segment-line points)
  (define (iter point rest)
    (if (null? rest)
      '()
      (cons (make-segment point (car rest))
            (iter (car rest) (cdr rest)))))
  (if (null? points)
    '()
    (iter (car points) (cdr points))))

(define outline-painter
  (let ((p00 (make-vect 0 0))
        (p01 (make-vect 0 1))
        (p11 (make-vect 1 1))
        (p10 (make-vect 1 0)))
    (segments->painter (make-segment-loop (list p00 p01 p11 p10)))))

(define x-painter
  (let ((p00 (make-vect 0 0))
        (p01 (make-vect 0 1))
        (p11 (make-vect 1 1))
        (p10 (make-vect 1 0)))
    (segments->painter (list (make-segment p00 p11)
                             (make-segment p01 p10)))))

(define diamond-painter
  (let ((p00 (make-vect 0.5 0))
        (p01 (make-vect 1 0.5))
        (p11 (make-vect 0.5 1))
        (p10 (make-vect 0 0.5)))
    (segments->painter (make-segment-loop (list p00 p01 p11 p10)))))

(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (flip-vert painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top (transform-painter
                       painter1
                       split-point
                       (make-vect 1.0 0.5)
                       (make-vect 0.0 1.0)))
          (paint-bottom (transform-painter
                          painter2
                          (make-vect 0.0 0.0)
                          (make-vect 1.0 0.0)
                          split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (roto-below painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

(define unit-frame
  (make-frame (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              (make-vect 0.0 1.0)))
