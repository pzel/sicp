    
(define (%eval exp env) 
  (let ((m (get-eval-method (type-of exp) eval-methods)))
    (m exp env)))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (%eval (assignment-value exp) env)
		       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                 (%eval (definition-value exp) env)
                 env))

(define (eval-if exp env)
  (if (true? (%eval (if-predicate exp) env))
      (%eval (if-consequent exp) env)
      (%eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (%eval (first-exp exps) env))
        (else
         (%eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (list-of-values exps env)
  (if (no-operands? exps) 
      '()
      (cons (%eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; Ex. 4.3 -- this would require registering eval-methods for certain types via
;; a (register 'quoted (lambda(exp enn) (blah exp)), function or a macro such as
;; (define-eval (begin exp env) (blah exp)) but I'll just test the representation.
(define eval-methods
  (list
   (cons 'self-evaluating   (lambda(exp env) exp))
   (cons 'quote             (lambda(exp env) (text-of-quotation exp)))
   (cons 'begin             (lambda(exp env) (eval-sequence (begin-actions exp) env)))
   (cons 'define            (lambda(exp env) (eval-definition exp env)))
   (cons 'set!              (lambda(exp env) (eval-assignment exp env)))
   (cons 'variable          (lambda(exp env) (lookup-variable-value exp env)))
   (cons 'if                (lambda(exp env) (eval-if exp env)))
   (cons 'cond              (lambda(exp env) (eval-if (cond->if exp) env)))
   (cons 'lambda            (lambda(exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
   (cons 'and               (lambda(exp env) (eval-and (and-actions exp) env)))
   (cons 'dand              (lambda(exp env) (eval-if (and->if exp) env)))
   (cons 'or                (lambda(exp env) (eval-or (or-actions exp) env)))
   (cons 'dor               (lambda(exp env) (eval-if (or->if exp) env)))
   (cons 'let               (lambda(exp env) (%eval (let->combination exp) env)))
   (cons 'let*              (lambda(exp env) (%eval (let*->nested-let exp) env)))
   (cons 'application       (lambda(exp env) (%apply (%eval (operator exp) env) (list-of-values (operands exp) env))))))

;; Application
(define (application? exp) (and (pair? exp) 'application))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? exp) (null? exp))
(define (first-operand exp) (car exp))
(define (rest-operands exp) (cdr exp))

(define (%apply proc arguments)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc arguments))
	((compound-procedure? proc)
	 (eval-sequence
	  (procedure-body proc)
	  (extend-environment (procedure-parameters proc) 
			      arguments
			      (procedure-environment proc))))
        (else (error "%apply does not support" proc))))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) 
			      args))
(define (apply-in-underlying-scheme p args) (apply p args))

;; Assignment
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; Boolean values
(define %t #t)
(define %f #f)
(define (true? x) (eq? x %t))
(define (false? x) (not (true? x)))

;; Conditionals
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cadr exp)) ;; <- sicp says (cdr exp), i disagree
(define (cond-else-clause? exp) (eq? (cond-predicate exp) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      '%f
      (let [(first (car clauses))
	    (rest (cdr clauses))]
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE is not last"))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;; Data representation
(define (quoted? exp) (tagged-list? exp 'quote))
(define (self-evaluating? exp) (if (or (string? exp) (number? exp))
                                   'self-evaluating
                                   #f))
  
(define (tagged-list? l tag) (if (and (pair? l) (eq? tag (car l)))
                                 tag
                                 #f))
(define (text-of-quotation exp) (cadr exp))
(define (variable? v) (and (symbol? v) 'variable))

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

;; Ifs
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) 
  (if (not (null? (cdddr exp))) (cadddr exp) %f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; Lambdas
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

;; Procedures
(define (make-procedure params body env) (list 'procedure params body env))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-body proc) (caddr proc))
(define (procedure-environment proc) (cadddr proc))
(define (primitive-procedure? p) (tagged-list? p 'primitive))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (primitive-implementation p) (cadr p))

;; Sequences
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (first-exp seq) (car seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

;; Scheme backend plumbing
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'false? false?)
        (list 'display display)
        (list '+ +)))

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) 
  (map (lambda(proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; Set the base environment for evaluation
(define (setup-environment!)
  (let [(initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     %null-env))]
    (define-variable! '%t #t initial-env)
    (define-variable! '%f #f initial-env)
    initial-env))
  
(define %base-env (setup-environment!))

;; ex. 4.1
;; TODO: blog about this
(define-syntax r-eval
  (syntax-rules ()
    ((r-eval l)
     (reverse (map force (reverse l))))))

(define-syntax r-cons
  (syntax-rules ()
    ((r-cons a b)
     (r-eval (cons (delay a) b)))))

(define (list-of-values-backend-dependent exps env backend-type)
  (if (no-operands? exps)
      '()
      (if (eq? backend-type 'right)
          (r-cons (%eval (first-operand exps) env) 
                  (list-of-values-backend-dependent (rest-operands exps) env backend-type))
          (cons (%eval (first-operand exps) env) 
                  (list-of-values-backend-dependent (rest-operands exps) env backend-type)))))

(define (list-of-values-backend-agnostic exps env backend-type)
  (if (no-operands? exps)
      '()
      (let [(a (%eval (first-operand exps) env))
            (b (list-of-values-backend-agnostic (rest-operands exps) env backend-type))]
        (if (eq? backend-type 'right)
            (r-cons a b)
            (cons a b)))))

; ex. 4.3
(define (first pred l)
  (cond ((null? l) #f)
        (else (let ((el (car l)))
                (or (pred el)
                    (first pred (cdr l)))))))

(define (type-of exp)
  (or (first (lambda(f) (f exp))
             (list quoted? begin? self-evaluating? assignment? 
                   definition? if? cond? lambda? and? dand? or? dor? 
                   let? let*?
                   variable? application?))
      (error "could not determine the type of" exp)))

(define (get-eval-method type table) 
  (cond ((null? table) (error "could not find eval method for" type))
        ((eq? (caar table) type) (cdar table))
        (else
         (get-eval-method type (cdr table)))))

;; ex. 4.4
;; AND and OR special forms
(define (and? exp) (tagged-list? exp 'and))
(define (and-actions exp) (cdr exp))
(define (eval-and exps env)
  (cond ((last-exp? exps) (%eval (first-exp exps) env))
        ((false? (%eval (first-exp exps) env)) %f)
        (else
         (eval-and (rest-exps exps) env))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-actions exp) (cdr exp))
(define (eval-or exps env)
  (cond ((last-exp? exps) 
         (%eval (first-exp exps) env))
        ((true? 
          (%eval (first-exp exps) env)) %t)
        (else
         (eval-or (rest-exps exps) env))))

;; Ex. 4.4 -- AND and OR as derived expressions
(define (dand? exp) (tagged-list? exp 'dand))
(define (and->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        '%t
        (let [(first (car clauses))
              (rest (cdr clauses))]
          (make-if first
                   (expand-clauses rest)
                   '%f))))
  (expand-clauses (and-actions exp)))

(define (dor? exp) (tagged-list? exp 'dor))
(define (or->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        '%f
        (let [(first (car clauses))
              (rest (cdr clauses))]
          (make-if first
                   '%t
                   (expand-clauses rest)))))
  (expand-clauses (or-actions exp)))
  
;; Ex. 4.5 TODO

;; Ex. 4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (let-bindings exp)))
(define (let-vals exp) (map cadr (let-bindings exp)))
(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp)) 
        (let-vals exp)))

  
;; Ex. 4.7
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-let exp)
  (cond ((null? (let-bindings exp))
         (car (let-body exp)))
        (else
         (list 'let
               (list (car (let-bindings exp)))
               (let*->nested-let (list 'let 
                                       (cdr (let-bindings exp)) 
                                       (car (let-body exp))))))))
(define (let*->combination exp)
  (let->combination (let*->nested-let exp)))
