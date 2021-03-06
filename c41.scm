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

(define (eval-unbind exp env)
  (unbind-variable! (unbound-var exp) env))

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
   (cons 'make-unbound!     (lambda(exp env) (eval-unbind exp env)))
   (cons 'variable          (lambda(exp env) (lookup-variable-value exp env)))
   (cons 'if                (lambda(exp env) (eval-if exp env)))
   (cons 'cond              (lambda(exp env) (eval-if (cond->if exp) env)))
   (cons 'lambda            (lambda(exp env) (make-procedure (lambda-parameters exp)
                                                             (lambda-body exp) env)))
   (cons 'and               (lambda(exp env) (eval-and (and-actions exp) env)))
   (cons 'dand              (lambda(exp env) (eval-if (and->if exp) env)))
   (cons 'or                (lambda(exp env) (eval-or (or-actions exp) env)))
   (cons 'dor               (lambda(exp env) (eval-if (or->if exp) env)))
   (cons 'let               (lambda(exp env) (%eval (let->combination exp) env)))
   (cons 'letrec            (lambda(exp env) (%eval (letrec->let exp) env)))
   (cons 'let*              (lambda(exp env) (%eval (let*->nested-let exp) env)))
   (cons 'for               (lambda(exp env) (%eval (for->let exp) env)))
   (cons 'application       (lambda(exp env) (%apply (%eval (operator exp) env) 
                                                     (list-of-values (operands exp) env))))))

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
  (apply-in-underlying-scheme (primitive-implementation proc) args))
(define (apply-in-underlying-scheme p args) (apply p args))

;; Assignment
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; Boolean values
(define %t #t)
(define %f #f)
(define (true? x) (not (false? x)))
(define (false? x) (eq? x %f))

;; Conditionals
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? exp) (eq? (cond-predicate exp) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (arrow-syntax? exp) (and (pair? exp) (eq? '=> (car exp))))
(define (arrow->exp pred func) (cons (cadr func) (list pred)))

;; EX 4.5
(define (expand-clauses clauses)
  (if (null? clauses)
      '%f
      (let* [(first (car clauses))
             (rest (cdr clauses))
             (pred (cond-predicate first))
             (action (cond-actions first))]
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp action)
                (error "ELSE is not last"))
            (make-if pred
                     (if (arrow-syntax? action)
                         (arrow->exp pred action)
                         (sequence->exp action))
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
(define (null-env? env) (eq? %null-env env))
(define (extend-environment vars vals env)
  (let [(nvars (length vars)) (nvals (length vals))]
    (if (= nvars nvals)
        (cons (make-aframe vars vals) env)
        (if (< nvars nvals)
            (error "extend-environment: too few variables: " vars vals)
            (error "extend-environment: too few values: " vars vals)))))

(define (first-frame env) 
  (if (null-env? env) 
      (error "first-frame: NULL environment") 
      (car env)))

(define (traverse-env var env f enclosing-loop not-found)
  (define (scan frame)
    (cond ((null-frame? frame) (enclosing-loop))
          ((eq? var (first-var frame)) (f (first-binding frame) frame))
          (else (scan (next-binding frame)))))
  (if (null-env? env) (not-found)
      (let ((frame (first-frame env)))
        (scan frame))))

(define (enclosing-environment env) (cdr env))

;; Ex. 4.12
(define (define-variable! var val env)
  (traverse-env var env
                (lambda(b _) (set-binding-val! b val))
                (lambda() (add-binding-to-aframe! var val (first-frame env)))
                (lambda() (error "DEFINE-VARIABLE: THIS CODE SHOULDNT BE REACHED"))))

(define (lookup-variable-value var env)
  (define (err)(error "Undefined variable: " var))
  (traverse-env var env 
                (lambda(b _) (if (eq? '%unassigned (cdr b)) (err) (cdr b)))
                (lambda() (lookup-variable-value var (enclosing-environment env)))
                err))

(define (unbind-variable! var env)
  (traverse-env var env 
                (lambda(_ f) (destroy-first-binding! f))
                (lambda() (error "Variable not bound: " var))
                (lambda() (error "UNBIND-VARIABLE: THIS CODE SHOULDNT BE REACHED"))))

(define (set-variable-value! var val env)
  (traverse-env var env 
                (lambda(b _) (set-binding-val! b val))
                (lambda() (set-variable-value! var val (enclosing-environment env)))
                (lambda() (error "SET-VARIABLE-VALUE: THIS CODE SHOULDNT BE REACHED"))))

;; Frames -- see Ex. 4.11

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
(define (make-procedure params body env) 
  (list 'procedure params (scan-out-defines body) env))
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
        (list 'reverse reverse)
        (list 'false? false?)
        (list 'display display)
        (list 'empty (lambda() (list)))
        (list '= =)
        (list '- -)
        (list '+ +)))

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) 
  (map (lambda(proc) (list 'primitive (cadr proc)))
       primitive-procedures))

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

;; Ex. 4.3
(define (first pred l)
  (cond ((null? l) #f)
        (else (let ((el (car l)))
                (or (pred el)
                    (first pred (cdr l)))))))

(define (type-of exp)
  (or (first (lambda(f) (f exp))
             (list quoted? begin? self-evaluating? assignment? 
                   definition? unbinding? if? cond? lambda? and? dand? or? dor? 
                   let? let*? letrec? for? 
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

;; Ex. 4.5 DONE. See: expand-clauses

;; Ex. 4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (let-bindings exp)))
(define (let-vals exp) (map cadr (let-bindings exp)))
(define (let->combination exp)
  (if (named-let? exp)
      (named-let->seq exp)
      (cons (make-lambda (let-vars exp) (let-body exp))
            (let-vals exp))))

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

;; Ex. 4.8
(define (named-let? exp) (if (and (tagged-list? exp 'let)
                                  (not (null? (cdr exp)))
                                  (symbol? (cadr exp)))
                             'named-let
                             #f))

(define (named-let-bindings exp) (let-bindings (cdr exp)))
(define (named-let-body exp) (let-body (cdr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-vals exp) (let-vals (cdr exp)))
(define (named-let-vars exp) (let-vars (cdr exp)))
(define (named-let->seq exp)
   (list 'begin
         (list 'define 
               (named-let-name exp)
               (make-lambda (named-let-vars exp)
                            (named-let-body exp)))
         (cons (named-let-name exp)
               (named-let-vals exp))))

;; Ex 4.9 Looping constructs
;; A for loop
(define (for? exp) (tagged-list? exp 'for))
(define (for-var exp) (caadr exp))
(define (for-start exp) (cadadr exp))
(define (for-end exp) (car (cddadr exp)))
(define (for-body exp) (caddr exp))
(define (for->let exp)
  ;; Poor-man's hygiene
  (let ((loop-name (string->symbol (string-append "loop-" (symbol->string (for-var exp)))))
        (res-name  (string->symbol (string-append "res-" (symbol->string (for-var exp))))))
    `(let ,loop-name
       ,(list `(,res-name (empty))
              `( ,(for-var exp) ,(for-start exp)))
       ,(make-if `(= ,(for-var exp) ,(for-end exp))
                 `(reverse ,res-name)
                 `(let ((,res-name (cons ,(for-body exp) ,res-name)))
                    (,loop-name ,res-name (+ 1 ,(for-var exp))))))))


;; Ex. 4.10 TODO

;; Ex. 4.11

(define %null-frame (cons '%end-of-frame '%unassigned))
(define %NF %null-frame)
(define (null-frame? af) 
  (cond ((null? af) (error "expected NULL FRAME, got empty list"))
        (else       (equal? %null-frame (car af)))))
(define (make-aframe vars vals)
  (append (map make-binding vars vals) (list %null-frame)))

(define (aframe-get af var)
  (cond ((null-frame? af)
         (error "undefined variable: " var))
        ((eq? (first-var af) var) (first-val af))
        (else
         (aframe-get (cdr af var)))))

(define (first-var af) (caar af))
(define (first-val af) (cdar af))
(define (first-binding af) (car af))
(define (next-binding af) 
  (cond ((null-frame? af)
         (error "cannot get next binding of empty frame"))
        (else 
         (cdr af))))
(define (make-binding var val) (cons var val))
(define (set-binding-val! b val) (set-cdr! b val))
(define (add-binding-to-aframe! var val af)
  (let* ((old-head (car af))
         (old-tail (cdr af))
         (new-frame (cons (make-binding var val)
                          (cons old-head old-tail)))) ;; NOTE: cons makes a new cell.
    (set-car! af (car new-frame))
    (set-cdr! af (cdr new-frame))))

(define (destroy-first-binding! af)
  (if (null-frame? af) 
      (error "cannot remove binding from empty frame")
      (let* ((old-tail (cdr af))
             (last-var? (null? old-tail))
             (new-head (if last-var? '() (car old-tail)))
             (new-tail (if last-var? '() (cdr old-tail)))
             (new-frame (cons new-head new-tail)))
        (set-car! af (car new-frame))
        (set-cdr! af (cdr new-frame)))))

;; Ex. 4.12 DONE. See set-variable-value! & friends


;; Ex. 4.13 make-unbound!
;; This will only remove the binding from the current environment.
;; It is a serious violation of the principle of least surprise to
;; remove bindings from parent environments.
;; This operator is horrible all-around. All the more fun to write it!
(define (unbinding? exp) (tagged-list? exp 'make-unbound!))
(define (unbound-var exp) (cadr exp))

;; Ex. 4.14 defining map
;; Our evaluator cannot use the underlying scheme's `map`,
;; because our abstracted procedures cannot be applied
;; in the underlying scheme.
;; We must define `map` in the frontend.

;; Ex. 4.15 the halting problem
;; Assuming we have the halts? operator
#|

> (define (run-forever) (run-forever))
> (define (try p)     
    (if (halts? p p)  ← b.
        (run-forever) ← c.
        'halted))     ← d.

> (try try) ← a.

Case A:
-------
a. halts

If a. halts then b. returns c.
b. returns c. therefore a. keeps running forever
a. does not halt, but this goes against the first statement (a. halts)

Ergo: (try try) cannot produce a consistent answer.

Case B:
-------
a. runs forever

If a. runs forever, then b. returns d.
b. returns d., and halts the computation
a. halts, but this goes against the first statement (a. runs forever)

Ergo: (try try) cannot produce a consistent answer.


(try try) demonstrates that a referentially-transparent halts? predicate cannot exist.

|#


;; Ex. 4.16
(define (filter p l)
  (cond ((null? l)  '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else        (filter p (cdr l)))))

(define (find-defines exp)
  (filter (lambda(s)(eq? (type-of s) 'define)) exp))

(define (filter-out-defines exp)
  (filter (lambda(s)(not (eq? (type-of s) 'define))) exp))

(define (make-undefined-vars exp-body)
  (map (lambda(def) (list (definition-variable def) '(quote %unassigned)))
       (find-defines exp-body)))

(define (make-set!-vars exp-body)
  (map (lambda(def) 
         (list 'set! (definition-variable def) (definition-value def)))
       (find-defines exp-body)))

(define (has-defines? exp-body)
  (not (null? (find-defines exp-body))))

(define (scan-out-defines exp-body)
  (if (has-defines? exp-body)
      (let ((head (list 'let (make-undefined-vars exp-body))))
        (list (append head
                      (make-set!-vars exp-body)
                      (filter-out-defines exp-body))))
      exp-body))

;; Ex. 4.17 TODO

#| Ex. 4.18 

Let's consider both cases.
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; 'The method in the text'
(lambda (f y0 dt)
  (let ((y ’*unassigned*)
        (dy ’*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

; The method from 4.18
(lambda (f y0 dt)
  (let ((y ’*unassigned*)
        (dy ’*unassigned*))
    (let ((uniq1 (integral (delay dy) y0 dt))
          (uniq2 (stream-map f y))) 
      (set! y uniq1)
      (set! dy uniq2))
    y))  ===
(lambda (f y0 dt)
  (let ((y ’*unassigned*)
        (dy ’*unassigned*))
    ((lambda (uniq1 uniq2)
      (set! y uniq1)
      (set! dy uniq2)
      y)
     (integral (delay dy) y0 dt) 
     (stream-map f y))))         <- `y` is still unassigned

|#

;; Ex 4.19 skip

;; Ex 4.20
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (cons 'let
        (cons (letrec-vars exp)
              (append (letrec-assignments exp) (letrec-body exp)))))
(define (extract-vars exp)
  (map car (cadr exp)))
(define (letrec-body exp)
  (cddr exp))
(define (letrec-vars exp)
  (map (lambda(var) (list var (quote '*unassigned*)))
       (extract-vars exp)))
(define (letrec-assignments exp)
  (map (lambda (var body) (list 'set! var body))
       (extract-vars exp)
       (extract-bodies exp)))
(define (extract-bodies exp) 
  (map cadr (cadr exp)))

;; Ex 4.21 -- see c41-tests.scm:404


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set the base environment for evaluation
;; Leave this at the end so all non-function bindings are visible.
(define (setup-environment!)
  (let [(initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             %null-env))]
    (define-variable! '%t #t initial-env)
    (define-variable! '%f #f initial-env)
    initial-env))

(define %base-env (setup-environment!))
