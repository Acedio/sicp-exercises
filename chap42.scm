; Ex 4.25
; This will not work in an applicative-order Scheme, because every argument to
; `unless` must be evaluated before `unless` is. This means that an infinite
; stack of calls to `factorial` (going negative) will occur.
; This will work fine in a normal-order Scheme as long as the `if` internal to
; `unless` is treated as a primitive.

; Ex 4.26
; Example of a derived expression.
(define (unless->if exp)
  (make-if (unless-predicate exp)
           ; Just flip the order of the unless results.
           (unless-alternative exp)
           (unless-natural exp)))
; Can't think of non-contrived uses for using unless in a higher order
; procedure:
(define choose-first (list #f #t #t))
(define first (list 'chips 'pear' carrot))
(define second (list 'apple 'burgers 'fries))
(define healthy-foods (map unless choose-first first second))
