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
