; Evaluator circa exercise 4.14.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define syntax-list '())
(define (put-syntax tag eval-exp)
  (cond ((assoc tag syntax-list)
         (error "Syntax tag already exists!"
                tag))
        (else (set!
                syntax-list
                (cons (cons tag eval-exp)
                      syntax-list)))))
(define (get-syntax tag)
  (cond ((assoc tag syntax-list)
         => (lambda (tagged-eval-exp) (cdr tagged-eval-exp)))
        (else false)))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (variable? exp) (symbol? exp))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define false #f)

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; --------------------------
; Eval and special syntaxes.
; --------------------------

(define eval-in-underlying-scheme eval)
(define apply-in-underlying-scheme apply)
; apply is defined later, but sticking a placeholder in the
; current scope so eval doesn't bind to the underlying apply.
(define (apply proc args) 'false)

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
  (put-syntax 'list
              (lambda (exp env)
                (eval-list exp env)))
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
                (eval-sequence
                  (begin-actions exp) env)))
  (put-syntax 'let
              (lambda (exp env)
                (eval (let->lambda exp) env)))
  (put-syntax 'cond
              (lambda (exp env)
                (eval (cond->if exp) env))))
(install-syntax)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))
(define (list-elements exp)
  (cdr exp))
(define (eval-list exp env)
  (list-of-values (list-elements exp) env))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))
(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (cadddr exp))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (make-let parameters body)
  (cons 'let (cons parameters body)))
(define (let-parameters exp)
  (cadr exp))
(define (let-body exp)
  (cddr exp))
(define (let->lambda exp)
  (cons (make-lambda
          (map car (let-parameters exp))
          (let-body exp))
        (map cadr (let-parameters exp))))

(define (make-begin seq) (cons 'begin seq))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (cond-cases exp)
  (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

; TODO: Handle the total fallthrough case.
; TODO: Copy over to chap41.scm after bugs are fixed.
(define (cond->if exp)
  (define (transform-cases cases)
    (if (null? cases)
      '(begin)
      (let ((case (car cases))
            (cases (cdr cases)))
        (let ((l (length case)))
          (cond ((< l 1) (error "empty case not supported"))
                ((= l 1)
                 (make-let (list (list 'pred (car case)))
                           (list (make-if
                                   'pred
                                   'pred
                                   (transform-cases cases)))))
                ((and (= l 3)
                      (eq? '=> (cadr case)))
                 (error "unsupported cond type: =>"))
                ((eq? 'else (car case))
                 (if (null? cases)
                   (list (sequence->exp (cdr case)))
                   (error "non-terminal else found")))
                (else
                 (make-let (list (list 'pred (car case))
                                 (list 'consequent
                                       (make-lambda '() (list (sequence->exp (cdr case))))))
                           (list (make-if
                                   'pred
                                   '(consequent)
                                   (transform-cases cases))))))))))
  (transform-cases (cdr exp)))

; ------------------
; Apply and friends.
; ------------------

; apply was already defined above, so just modify that var.
(set! apply
  (lambda (procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure
             procedure
             arguments))
          ((compound-procedure? procedure)
           (eval-sequence
             (procedure-body procedure)
             (extend-environment
               (procedure-parameters 
                procedure)
               arguments
               (procedure-environment 
                procedure))))
          (else
            (error "Unknown procedure type: APPLY"
                   procedure)))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) 
  (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; ---------------------------------------
; Environments and variable manipulation.
; ---------------------------------------

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

; Return the pair with cdr = frame[var], error if not found
(define (find-var-in-frame var frame)
  (define (scan vars vals)
    (cond ((null? vars) '())
          ((eq? var (car vars)) vals)
          (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame) (frame-values frame)))

; Finds the first matching var and returns the pair with cdr = frame[var]
(define (find-var-in-env var env)
  (if (null? env)
    '()
    (let* ((frame (first-frame env))
           (val-pair (find-var-in-frame var frame)))
      (if (null? val-pair)
        (find-var-in-env var (enclosing-environment env))
        val-pair))))

(define (lookup-variable-value var env)
  (let ((val-pair (find-var-in-env var env)))
    (if (null? val-pair)
      (error "Could not find variable"
             var)
      (car val-pair))))

(define (set-variable-value! var val env)
  (let ((val-pair (find-var-in-env var env)))
    (if (null? val-pair)
      (error "Could not find variable"
             var)
      (set-car! val-pair val))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (val-pair (find-var-in-frame var frame)))
    (if (null? val-pair)
      (add-binding-to-frame! var val frame)
      (set-car! val-pair val))))
; -----------------------------------------
; The global environment and primitive ops.
; -----------------------------------------

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'false false initial-env)
    initial-env))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define the-global-environment 
  (setup-environment))

; ----
; REPL
; ----

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output 
           (eval input 
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))
