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
