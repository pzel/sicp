;; The evaluator & sub-evaluators
(define (%apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        (else
         (error "%apply does not support" procedure))))

(define (%eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((begin? exp)      (eval-sequence (begin-actions exp) env))
        ((definition? exp) (eval-definition exp env))
	((variable? exp)   (lookup-variable-value exp env))
	((quoted? exp)     (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
        ;; ((application? exp)
        ;;  (apply (eval (operator exp) env)
        ;;         (list-of-values (operands exp) env)))
        (else
         (error "%eval: unknown expression" exp))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (%eval (assignment-value exp) env)
		       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                 (%eval (definition-value exp) env)
                 env))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (%eval (first-exp exps) env))
        (else
         (%eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;; Assignment
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


;; Definitions

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-value exp)
  (if (symbol? (cadr exp)) 
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) ; we're defining a symbol
      (caadr exp))) ; we're using sugar to define a function


;; Environments
(define %null-env '())
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
(define (enclosing-environment env) (cdr env))
(define (extend-environment vars vals env)
  (let [(nvars (length vars)) (nvals (length vals))]
    (if (= nvars nvals)
        (cons (make-frame vars vals) env)
        (if (< nvars nvals)
            (error "extend-environment: too few variables")
            (error "extend-environment: too few values")))))
(define (first-frame env) 
  (if (null? env) 
      (error "first-frame: NULL environment") 
      (car env)))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env %null-env)
	(error "Undefined variable: " var)
	(let [(frame (first-frame env))]
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env %null-env)
	(error "Undefined variable: " var)
	(let [(frame (first-frame env))]
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; Frames
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (make-frame variables values) (cons variables values))

;; Sequences
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (first-exp seq) (car seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (rest-exps seq) (cdr seq))

;; Lambdas
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

;; Application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; Data representation
(define (quoted? exp) (tagged-list? exp 'quote))
(define (self-evaluating? exp) (or (string? exp) (number? exp)))
(define (tagged-list? l tag) (and (pair? l) (eq? tag (car l))))
(define (text-of-quotation exp) (cadr exp))
(define (variable? v) (symbol? v))

;; Set the base environment for evaluation
(define %base-env (extend-environment '() '() %null-env))
