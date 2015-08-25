(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define name-list '())

(define (make-sig op tags)
  (list op tags))
(define (sig-op sig)
  (car sig))
(define (sig-tags sig)
  (cadr sig))

(define (make-fn-entry sig fn)
  (list sig fn))
(define (fn-entry-sig entry)
  (car entry))
(define (fn-entry-fn entry)
  (cadr entry))

(define (get op tags)
  (define (get-helper sig name-list)
    (if (null? name-list)
      #f
      (if (equal? (fn-entry-sig (car name-list))
                  sig)
        (fn-entry-fn (car name-list))
        (get-helper sig (cdr name-list)))))
  (get-helper (make-sig op tags) name-list))

(define (put op tags fn)
  ; Constructs a new name-table with the fn included
  (define (put-helper sig name-list)
    (cond ((null? name-list)
           (list (make-fn-entry (make-sig op tags) fn)))
          ((equal? sig (fn-entry-sig (car name-list)))
           (cons (make-fn-entry sig fn)
                 (cdr name-list)))
          (else (cons (car name-list)
                      (put-helper sig (cdr name-list))))))
  (set! name-list (put-helper (make-sig op tags)
                              name-list)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'negate '(scheme-number)
       (lambda (x) (- x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-integer-package)
  ; Defined as a list to avoid casting reals to integers
  (define (make-int x) (list (floor x)))
  (define (int-value x) (car x))
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (make-int (+ (int-value x) (int-value y)))))
  (put 'sub '(integer integer)
       (lambda (x y) (make-int (- (int-value x) (int-value y)))))
  (put 'mul '(integer integer)
       (lambda (x y) (make-int (* (int-value x) (int-value y)))))
  (put 'div '(integer integer)
       (lambda (x y) (make-int (floor (/ (int-value x) (int-value y))))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= (int-value x) (int-value y))))
  (put '=zero? '(integer)
       (lambda (x) (= (int-value x) (make-int 0))))
  (put 'negate '(integer)
       (lambda (x) (make-int (- (value x)))))
  (put 'make 'integer
       (lambda (x) (tag (make-int x))))
  (put 'value 'integer
       (lambda (x) (int-value x)))
  'done)
(install-integer-package)

(define (make-integer n)
  ((get 'make 'integer) n))
(define (value x) ((get 'value 'integer) x))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))
  (put 'negate '(rational)
       (lambda (x) (tag (make-rat (- (numer x)) (denom x)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag
        'complex)
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang
        'complex)
   r a))

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ?-complex z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero?-complex z)
    ; Could probably just use equ?-complex here.
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  (define (negate-complex z)
    (make-from-real-imag (- (real-part z)) (- (imag-part z))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (equ?-complex z1 z2)))
  (put '=zero? '(complex)
       (lambda (x)
         (=zero?-complex x)))
  (put 'negate '(complex)
       (lambda (x) (negate-complex x)))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Being tricksy and reusing the global table.
(define (put-coercion from to fn)
  (put to from fn))
(define (get-coercion from to)
  (get to from))

(put-coercion 'scheme-number
              'rational
              (lambda (int)
                (make-rational (contents int) 1)))

; Coercing version, 2.81
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (= type1 type2))
                  (let ((t1->t2
                         (get-coercion type1
                                       type2))
                        (t2->t1
                         (get-coercion type2
                                       type1)))
                    (cond (t1->t2
                           (apply-generic
                            op (t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic
                            op a1 (t2->t1 a2)))
                          (else
                           (error "No method for these types"
                                  (list op type-tags)))))
                  (error "No method for these types"
                         (list op type-tags))))
              (error "No method for these types"
               (list op type-tags)))))))

(define (all x)
  (cond ((null? x) #t)
        ((car x) (all (cdr x)))
        (else #f)))

; Coercing version, 2.82
(define (get-coercions type-tags to-type)
      (let ((coercions
              (map (lambda (from-type)
                     (if (eq? from-type to-type)
                       (lambda (x) x)
                       (get-coercion from-type to-type)))
                   type-tags)))
        (if (all coercions)
          coercions
          #f)))

(define (apply-coercions coercions args)
  (if (null? coercions)
    '()
    (cons ((car coercions)
           (car args))
          (apply-coercions
            (cdr coercions)
            (cdr args)))))

(define (repeat x n)
  (cond ((< n 1) '())
        (else (cons x (repeat x (- n 1))))))

(define (apply-generic op . args)
  (define (try-coercions type-tags to-try)
    (if (null? to-try)
      (error "No method for these types"
             (list op type-tags))
      (let ((coercions
              (get-coercions type-tags (car to-try))))
        (if coercions
          (let ((proc (get op (repeat (car to-try)
                                      (length args)))))
            (if proc
              (apply proc
                     (map contents
                          (apply-coercions coercions args)))
              (try-coercions type-tags (cdr to-try))))
          (try-coercions type-tags (cdr to-try))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-coercions type-tags type-tags)))))

; 2.83
(define (install-raise-package)
  (put 'raise '(integer)
       (lambda (x)
         (make-rational (value x) 1)))
  (put 'raise '(rational)
       (lambda (x)
         (make-scheme-number (/ (numer x) (denom x) 1.0))))
  (put 'raise '(scheme-number)
       (lambda (x)
         (make-complex-from-real-imag x 0)))
  'done)
(install-raise-package)
(define (raise x) (apply-generic 'raise x))

; 2.84
(define (install-level-package)
  (put 'level 'integer 1.0)
  (put 'level 'rational 2.0)
  (put 'level 'scheme-number 3.0)
  (put 'level 'complex 4.0)
  'done)
(install-level-package)

(define (level x) (get 'level (type-tag x)))

(define (raise-to x target-level)
  (cond ((= (level x)
            target-level)
         x)
        ((< (level x)
            target-level)
         (raise-to (raise x) target-level))
        (else (error "LEVEL IS ALREADY HIGHER"
                     (list (level x) target-level)))))

(define (apply-generic op . args)
  (define (apply-helper op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (let ((levels (map level args)))
            (if (apply = levels)
              (error
                "No method for these types or raised: APPLY-GENERIC"
                (list op type-tags))
              (let ((max-level (apply max levels)))
                (let ((raised-args (map (lambda (arg) (raise-to arg max-level)) args)))
                  (apply-helper op raised-args)))))))))
  (apply-helper op args))

;2.85
(define (install-project-package)
  (put 'project 'rational (lambda (x) (make-integer (floor (/ (numer x) (denom x))))))
  (put 'project 'scheme-number (lambda (x) (make-rational (floor (* 1000000 x)) 1000000)))
  (put 'project 'complex (lambda (x) (real-part x)))
  'done)
(install-project-package)

(define (drop x)
  (let ((project (get 'project (type-tag x))))
    (if project
      (let ((projected (project (contents x))))
        (if (equ? x (raise projected))
          (drop projected)
          x))
      x)))

;2.86
(define (square x) (mul x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))
  (define (angle z)
    (arctangent (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  ; Cosine and sine are defined below
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
          (arctangent y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)

(define (install-trig-package)
  (put 'sin 'scheme-number (lambda (x) (sin x)))
  (put 'cos 'scheme-number (lambda (x) (cos x)))
  (put 'atan 'scheme-number (lambda (x y) (atan x y)))
  (put 'sqrt 'scheme-number (lambda (x) (sqrt x)))
  'done)
(install-trig-package)

(define (sine x) ((get 'sin 'scheme-number) (raise-to x (level 0))))
(define (cosine x) ((get 'cos 'scheme-number) (raise-to x (level 0))))
(define (arctangent x y) ((get 'atan 'scheme-number) (raise-to x (level 0)) (raise-to y (level 0))))
(define (square-root x) ((get 'sqrt 'scheme-number) (raise-to x (level 0))))

;2.5.3
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1
                     (add-terms (rest-terms L1)
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms
                      L1
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term
                      (order t1)
                      (add (coeff t1)
                           (coeff t2)))
                     (add-terms
                      (rest-terms L1)
                      (rest-terms L2)))))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
                ADD-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms
         (mul-term-by-all-terms
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms
            t1
            (rest-terms L))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
                MUL-POLY"
               (list p1 p2))))

  (define (negate-poly p)
    (define (negate-terms terms)
      (if (empty-termlist? terms)
        (the-empty-termlist)
        (let ((term (first-term terms)))
          (adjoin-term (make-term (order term)
                                  (negate (coeff term)))
                       (negate-terms (rest-terms terms))))))
    (make-poly
      (variable p)
      (negate-terms (term-list p))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 (negate-poly p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (empty-termlist? (term-list p))))
  (put 'negate '(polynomial)
       (lambda (x)
         (tag (negate-poly x))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; 2.88, negate dispersed throughout
(define (negate x)
  (apply-generic 'negate x))

; 2.89
;; representation of terms and term lists for dense polys
(define (adjoin-term term term-list)
  (define (add-zeroes lst num)
    (if (> num 0)
      (add-zeroes (cons 0 lst) (- num 1))
      lst))
  (if (=zero? (coeff term))
    term-list
    (let ((delta (- (order term) (length term-list))))
      (if (< delta 0)
        (error "Tried to adjoin a term with an order lower than highest order term.")
        (cons (coeff term) (add-zeroes term-list delta ))))))
(define (the-empty-termlist) '())
(define (first-term term-list) (make-term (length (cdr term-list))
                                          (car term-list)))
(define (rest-terms term-list) 
  (define (skip-zeroes lst)
    (cond ((null? lst) '())
          ((=zero? (car lst)) (skip-zeroes (cdr lst)))
          (else lst)))
  (skip-zeroes (cdr term-list)))
(define (empty-termlist? term-list)
  (null? term-list))
