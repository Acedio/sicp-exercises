; Evaluator circa exercise 4.14.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define syntax-list '())
(define (put-syntax tag analyze-exp)
  (cond ((assoc tag syntax-list)
         (error "Syntax tag already exists!"
                tag))
        (else (set!
                syntax-list
                (cons (cons tag analyze-exp)
                      syntax-list)))))
(define (get-syntax tag)
  (cond ((assoc tag syntax-list)
         => (lambda (tagged-analyze-exp) (cdr tagged-analyze-exp)))
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

(define (show desc exp)
  (display desc)
  (display exp)
  (newline)
  (newline))

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (lambda (env) exp))
        ((variable? exp)
         (lambda (env)
           (lookup-variable-value exp env)))
        ((get-syntax (car exp))
         => (lambda (analyze-exp)
              (let ((analyzed-exp (analyze-exp exp)))
                (lambda (env)
                  (analyzed-exp env)))))
        ((application? exp)
         (begin
           (analyze-application exp)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env) (a env) (b env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence: ANALYZE")
      (loop (car procs) (cdr procs)))))

(define (install-syntax)
  (put-syntax 'quote
              (lambda (exp)
                (let ((quotation-text (text-of-quotation exp)))
                  (lambda (env) quotation-text))))
  (put-syntax 'set!
              (lambda (exp)
                (analyze-assignment exp)))
  (put-syntax 'define
              (lambda (exp)
                (analyze-definition exp)))
  (put-syntax 'if
              (lambda (exp)
                (analyze-if exp)))
  (put-syntax 'lambda
              (lambda (exp)
                (analyze-lambda exp)))

  ; The below are handled as syntax transforms.
  (let ((put-sugar (lambda (tag transform-fn)
                     (put-syntax
                       tag
                       (lambda (exp)
                         (let ((analyzed (analyze (transform-fn exp))))
                           (lambda (env)
                             (analyzed env))))))))
    (put-sugar 'list (lambda (exp) (list->cons exp)))
    (put-sugar 'begin (lambda (exp) (begin->lambda exp)))
    (put-sugar 'let (lambda (exp) (let->lambda exp)))
    (put-sugar 'letrec (lambda (exp) (letrec->let exp)))
    (put-sugar 'cond (lambda (exp) (cond->if exp)))))
(install-syntax)

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (cadddr exp))
(define (analyze-if exp)
  (let ((pred (analyze (if-predicate exp)))
        (cnsq (analyze (if-consequent exp)))
        (alt (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pred env))
        (cnsq env)
        (alt env)))))

; parameters and body are lists of params and body expressions, respectively.
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
; parameters and body are lists of params and body expressions, respectively.
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure params bproc env))))

(define (make-cons a b)
  (list 'cons a b))
(define (list->cons exp)
  (define (loop exps)
    (if (null? exps)
      (quote '())
      (make-cons (car exps) (loop (cdr exps)))))
  (loop (cdr exp)))

(define (begin->lambda exp)
  (list (make-lambda '() (cdr exp))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond->if exp)
  ; transform-cases converts a list of cond cases into a list of
  ; (pred consequent) lists. pred is a zero-argument lambda that returns a
  ; non-false value if the consequent should be executed, or #f otherwise.
  ; consequent is a one-argument lambda that takes the predicate result and
  ; executes the behavior.
  (define (transform-cases cases)
    (if (null? cases)
      '()
      (let* ((case (car cases))
             (rest (cdr cases))
             (l (length case))
             (transformed
               (cond ((< l 1) (error "cond: empty case not supported"))
                     ((= l 1)
                      (list 'list
                            (make-lambda '() (list (car case)))
                            (make-lambda (list 'pred) (list 'pred))))
                     ((and (= l 3)
                           (eq? '=> (cadr case)))
                      (list 'list
                            (make-lambda '() (list (car case)))
                            (make-lambda (list 'pred)
                                         (list (list (caddr case) 'pred)))))
                     ((eq? 'else (car case))
                      (if (not (null? rest))
                        (error "non-terminal else found")
                        (list 'list
                              (make-lambda '() (list (list 'quote '#t)))
                              (make-lambda (list 'pred)
                                           (list (sequence->exp (cdr case)))))))
                     (else
                      (list 'list
                            (make-lambda '() (list (car case)))
                            (make-lambda (list 'pred)
                                         (list (sequence->exp (cdr case)))))))))
        (cons transformed (transform-cases rest)))))
  ; try-cases recurses through cases, first evaluating the predicate and, if
  ; true, passing the predicate to the consequent lambda and returning the
  ; result. If false, moves to the next.
  (define try-cases
    (make-lambda (list 'cases)
                 (list (make-if '(null? cases)
                                (list 'quote 'error-cond-not-handled)
                                (make-let (list '(pred ((car (car cases))))
                                                '(consequent (cadr (car cases)))
                                                '(rest (cdr cases)))
                                          (list (make-if 'pred
                                                         '(consequent pred)
                                                         '(try-cases rest))))))))
  (list 'letrec
        (list (list 'cases (cons 'list (transform-cases (cdr exp))))
              (list 'try-cases try-cases))
        (list 'try-cases 'cases)))

; ex 4.20
(define (letrec->let exp)
  (define (make-sets params body)
    (if (null? params)
      body
      (let ((name (car (car params)))
            (init (cadr (car params)))
            (rest (cdr params)))
        (cons (list 'set! name init) (make-sets rest body)))))
  (let ((params (cadr exp))
        (body (cddr exp)))
    (make-let (map (lambda (param)
                     (list (car param) (list 'quote 'unset!)))
                   params)
              (make-sets params body))))


; ------------------
; Apply and friends.
; ------------------

(define (analyze-application exp)
  (let ((analyzed-operator (analyze (operator exp)))
        (analyzed-operands (map analyze (operands exp))))
    (lambda (env)
      (execute-apply (analyzed-operator env)
                     (map (lambda (analyzed-operand)
                            (analyzed-operand env))
                          analyzed-operands)))))
(define (execute-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           arguments))
        ((compound-procedure? procedure)
         ((procedure-body procedure)
          (extend-environment
            (procedure-parameters 
             procedure)
            arguments
            (procedure-environment 
             procedure))))
        (else
          (error "Unknown procedure type: APPLY"
                 procedure))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) 
  (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

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
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
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
