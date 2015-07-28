(define (equal? a b)
  (if (and (pair? a) (pair? b))
    (and (equal? (car a) (car b))
         (equal? (cdr a) (cdr b)))
    (eq? a b)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product
             (exponent exp)
             (make-exponentiation (base exp)
                                  (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? a b)
  (and (variable? a)
       (variable? b)
       (eq? a b)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s)
  (cadr s))
(define (augend s)
  (if (null? (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (cons '* (cddr p))))

(define (make-exponentiation base exponent)
  (cond ((= exponent 1) base)
        ((= exponent 0) 1)
        (else (list '** base exponent))))

(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (car exp) '**)))

(define (base exponentiation)
  (cadr exponentiation))
(define (exponent exponentiation)
  (caddr exponentiation))

; Infix version (no parens needed, ugly :)
(define (variable? x) (symbol? x))

(define (same-variable? a b)
  (and (variable? a)
       (variable? b)
       (eq? a b)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (op-precedence op)
  (cond ((eq? op '+) 1)
        ((eq? op '*) 2)
        ((eq? op '**) 3)
        (else 4)))
(define (precedes op1 op2)
  (not (< (op-precedence op1)
          (op-precedence op2))))

(define (grab-before sym exp)
  (define (iter exp)
    (cond ((eq? sym (car exp)) '())
          (else (cons (car exp)
                      (grab-before sym (cdr exp))))))
  (let ((before (iter exp)))
    (if (= 1 (length before))
      (car before)
      before)))
(define (grab-after sym exp)
  (let ((after (cdr (memq sym exp))))
    (if (= 1 (length after))
      (car after)
      after)))

(define (all-precede op exp)
  (cond ((not (pair? exp)) #t)
        ((precedes (car exp) op)
         (all-precede op (cdr exp)))
        (else #f)))

(define (is-op-type exp op)
  (and (memq op exp)
       (all-precede op exp)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? exp)
  (and (pair? exp)
       (is-op-type exp '+)))

(define (addend s)
  (grab-before '+ s))
(define (augend s)
  (grab-after '+ s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? exp)
  (and (pair? exp)
       (is-op-type exp '*)))

(define (multiplier p)
  (grab-before '* p))
(define (multiplicand p)
  (grab-after '* p))

(define (make-exponentiation base exponent)
  (cond ((= exponent 1) base)
        ((= exponent 0) 1)
        (else (list base '** exponent))))

(define (exponentiation? exp)
  (and (pair? exp)
       (is-op-type exp '**)))

(define (base exponentiation)
  (grab-before '** exponentiation))
(define (exponent exponentiation)
  (grab-after '** exponentiation))

; Sets as unordered lists with no dupes

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

; with dupes

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (union-set set1 set2)
  (append set1 set2))
