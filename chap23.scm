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

; sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set))
         (set))
        ((> x (car set))
         (cons (car set)
               (adjoin-set x (cdr set))))
        (else (cons x set))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2)
               (union-set set1 (cdr set2))))
        (else (cons (car set1)
                    (union-set (cdr set1) (cdr set2))))))

; sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (union-set set1 set2)
  ; NOTE: THIS DOESN'T WORK.
  ; Better answer to 2.65 is to convert to a sorted list,
  ; perform the needed operation, then convert back.
  ; See the Master Theorem for why 2.64 is O(n).
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (entry set1) (entry set2))
         (make-tree (entry set1)
                    (union-set (left-branch set1)
                               (left-branch set2))
                    (union-set (right-branch set1)
                               (right-branch set2))))
        ((< (entry set1) (entry set2))
         (make-tree (entry set1)
                    (left-branch set1)
                    (make-tree (entry set2)
                               (union-set (right-branch set1)
                                          (left-branch set2))
                               (right-branch set2))))
        (else (make-tree (entry set1)
                         (make-tree (entry set2)
                                    (left-branch set2)
                                    (union-set (left-branch set1)
                                               (right-branch set2)))
                         (right-branch set1)))))

(define (make-kv key value)
  (list key value))
(define (key kv)
  (car kv))
(define (value kv)
  (cadr kv))

(define (tree-lookup given-key tree)
  (cond ((null? tree) #f)
        ((= given-key (key (entry tree)))
         (entry tree))
        ((< given-key (key (entry tree)))
         (tree-lookup given-key (left-branch tree)))
        (else (tree-lookup given-key (right-branch tree)))))

; Huffman trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (cond ((leaf? tree) '())
        ((memq sym (symbols (left-branch tree))) 
         (cons 0 (encode-symbol sym (left-branch tree))))
        ((memq sym (symbols (right-branch tree)))
         (cons 1 (encode-symbol sym (right-branch tree))))
        (else (error "wtf mate"))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge set)
  (cond ((null? set) '())
        ((null? (cdr set)) (car set))
        (else (successive-merge (adjoin-set (make-code-tree (car set)
                                                            (cadr set))
                                            (cddr set))))))

(define pop-tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
(define pop-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
