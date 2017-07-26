; 4.1
; left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values (rest-operands exps) env)))
          (cons first second)))))

; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first second)))))

; 4.2
; Moving application? to the top of the cond will result in everything being
; evaluated as an application (they're all pairs). No special forms will work.
; To correct this you'd just have to change the definition of application?:
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

; 4.3
(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((get-syntax (car exp))
         => (lambda (eval-exp)
              (eval-exp exp env)))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (install-syntax)
  (put-syntax 'quote
              (lambda (exp env)
                (text-of-quotation exp)))
  (put-syntax 'set!
              (lambda (exp env)
                (eval-assignment exp env)))
  (put-syntax 'define
              (lambda (exp env)
                (eval-definition exp env)))
  (put-syntax 'if
              (lambda (exp env)
                (eval-if exp env)))
  (put-syntax 'lambda
              (lambda (exp env)
                (make-procedure
                  (lambda-parameters exp)
                  (lambda-body exp)
                  env)))
  (put-syntax 'begin
              (lambda (exp env)
                ((begin? exp)
                 (eval-sequence 
                   begin-actions exp) 
                 env)))
  (put-syntax 'cond
              (lambda (exp env)
                (eval (cond->if exp) env))))
(install-syntax)

; 4.4
(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))
(define (make-lambda parameters body)
  (list 'lambda parameters body))
(define (make-let parameters body)
  (list 'let parameters body))

(define (and->if exp)
  (define (iter preds)
    (cond ((null? preds) 'true)
          ((null? (cdr preds)) (car preds))
          (else (make-if (car preds)
                         (iter (cdr preds))
                         'false))))
  (iter (cdr exp)))

; Looking around online, seems people always do the derived form of 'or
; incorrectly. It needs to return the first non-'false value, not 'true!
; Solution comes with help from https://stackoverflow.com/questions/18284610/sicp-can-or-be-defined-in-lisp-as-a-syntactic-transformation-without-gensym
; This ended up being much more difficult than anticipated o.O
(define (or->if exp)
  ; Generates the list of lambdas that represent the predicates.
  ; Requires at least one predicate, but only >=2 will ever be passed.
  (define (make-lambda-list preds)
    (let ((first-lambda (make-lambda '() (car preds))))
      (cond ((null? (cdr preds)) (list first-lambda))
            (else (cons first-lambda
                        (make-lambda-list (cdr preds)))))))
  ; Generates an series of nested lets and ifs of the following structure:
  ;   (let ((v ((car lambdas)))
  ;         (lambdas (cdr lambdas)))
  ;        (if v v (... further predicates ...)))
  (define (make-let-if-structure len)
    (cond ((= len 1) '((car lambdas)))
          (else (make-let '((v ((car lambdas)))
                            (lambdas (cdr lambdas)))
                          (make-if 'v 'v (make-let-if-structure (- len 1)))))))
  (cond 
    ; Empty -> false
    ((null? (cdr exp)) 'false)
    ; One pred -> pred
    ((null? (cddr exp)) (cadr exp))
    ; Two+ preds -> fancy unrolled loop.
    (else (make-let (list (list 'lambdas (cons 'list (make-lambda-list (cdr exp)))))
                    (make-let-if-structure (length (cdr exp)))))))

(define (install-and-or)
  (put-syntax 'and
              (lambda (exp env)
                (eval (and->if exp) env)))
  (put-syntax 'or
              (lambda (exp env)
                (eval (or->??? exp) env))))

; 4.5
; Cond can be done similarly to or, where a set of lambdas capturing the
; environment for each predicate is passed to a waterfall of ifs.
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond->if exp)
  ; Generates the list of lambdas that represent the predicates.
  ; Requires at least one predicate, but only >=2 will ever be passed.
  (define (transform-case case)
    (let ((l (length case)))
      (cond ((< l 1) (error "empty case not supported"))
            ((= l 1) (list 'list (make-lambda '() (car case))))
            ((and (= l 3)
                  (eq? '=> (cadr case)))
             (list 'list
                   (make-lambda '() (car case))
                   ''=>
                   (make-lambda '() (caddr case))))
            ((eq? 'else (car case))
             (list 'list
                   ''else
                   (make-lambda '() (sequence->exp (cdr case)))))
            (else (list 'list
                        (make-lambda '() (car case))
                        (make-lambda '() (sequence->exp (cdr case))))))))
  (define (transform-cases cases)
    (define (iter rest)
      (if (null? rest)
        rest
        (let* ((case (car rest))
               (rest (cdr rest))
               (transformed (transform-case case)))
          (if (and (eq? 'else (cadr transformed))
                   (not (null? rest)))
            (error "non-terminal else disallowed")
            (cons transformed (iter rest))))))
    (if (= 0 (length cases))
      (error "empty cond statement not supported")
      (iter cases)))
  (define (make-let-if-structure transformed-cases)
    (if (null? transformed-cases)
      'false
      ; car is the first case, cdar is the first case sans 'list
      (let* ((case (cdar transformed-cases))
             (l (length case)))
        (cond ((= l 1)
               (make-let '((pred ((caar cases)))
                           (cases (cdr cases)))
                         (make-if 'pred 'pred (make-let-if-structure (cdr transformed-cases)))))
              ((eq? ''else (car case))
               '((cadar cases)))
              ((= l 2)
               (make-let '((pred ((caar cases)))
                           (consequent (cadar cases))
                           (cases (cdr cases)))
                         (make-if 'pred '(consequent) (make-let-if-structure (cdr transformed-cases)))))
              ((= l 3)
               (make-let '((pred ((caar cases)))
                           (consequent (caddar cases))
                           (cases (cdr cases)))
                         (make-if 'pred '((consequent) pred) (make-let-if-structure (cdr transformed-cases)))))
              (else (error "wtf"))))))
  (let ((transformed-cases (transform-cases (cdr exp))))
    (make-let (list (list 'cases (cons 'list transformed-cases)))
              (make-let-if-structure transformed-cases))))

; 4.6

(define (let->lambda exp)
  (list (make-lambda
          (map car (cadr exp))
          (caddr exp))
        (map cadr (cadr exp))))

; 4.7
; Should be fine to just add (eval (let*->nested-lets exp) env) to eval.
; Further iterations will process the resulting nested lets.

(define (let*->nested-lets exp)
  (let iter ((bindings (cadr exp)) (body (caddr exp)))
    (let ((l (length bindings)))
      (cond ((= l 0)
             body)
            (else
             (make-let (list (car bindings))
                       (iter (cdr bindings) body)))))))
