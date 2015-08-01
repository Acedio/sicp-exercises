(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv)
  (define (d-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (d-product exp var)
    (make-sum (make-product
                (multiplier exp)
                (deriv (multiplicand exp) var))
              (make-product
                (deriv (multiplier exp) var)
                (multiplicand exp))))
  (define (d-exp exp var)
    (make-product
        (make-product (exponent exp)
                      (make-exponentiation var (- (exponent exp)
                                                  1)))
        (deriv (base exp) var)))
  (put 'deriv '+ d-sum)
  (put 'deriv '* d-product)
  'done)
