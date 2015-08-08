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

(define (get-record name file)
  (apply-generic 'get-record name file))

(define (get-salary record)
  (apply-generic 'get-salary record))

(define (find-employee-record name files)
  (if (null? files)
    #f
    (let ((record (get-record name (car files))))
      (if record
        record
        (find-employee-record name (cdr files))))))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)
