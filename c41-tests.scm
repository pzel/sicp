(load "./c41.scm")
(load "./test.scm")

(run-tests
 '(
   ;; Unit tests

   ;; Data predicates
   (=? '(quoted? '(quote hello)) #t)
   (=? '(self-evaluating? 3) #t)
   (=? '(self-evaluating? "hello") #t)
   (=? '(self-evaluating? '(1 23)) #f)
   (=? '(tagged-list? '() 'tag) #f)
   (=? '(tagged-list? '(vv) 'vv) #t)
   (=? '(tagged-list? '(yy 1 2 3) 'yy) #t)
   (=? '(variable? 'x) #t)
   (=? '(variable? 2) #f)

   ;; Definitions
   (=? '(definition? '(define x 1)) #t)
   (=? '(definition? '(define (f x) x)) #t)
   (=? '(definition-variable '(define x 1)) 'x)
   (=? '(definition-variable '(define (f y) y)) 'f)
   (=? '(definition-value '(define x 0)) '0)
   (=? '(definition-value '(define (h x) x)) '(lambda (x) x))

   ;; Assignment
   (=? '(assignment? '(set! x 2)) #t)

   ;; Lambdas
   (=? '(lambda? '(hello world)) #f)
   (=? '(lambda? '(lambda (x) x)) #t)
   (=? '(lambda-parameters '(lambda (x y) 2)) '(x y))
   (=? '(lambda-body '(lambda (x y) x)) '(x))
   (=? '(lambda-body '(lambda (x y) (f x y))) '((f x y)))
   (=? '(make-lambda '(x y) '((+ x y))) '(lambda (x y) (+ x y)))
   
   ;; Procedures
   (=? '(make-procedure '(a b) '((f a b)) %base-env)
       `(procedure (a b) ((f a b)) ,%base-env))
   (=? '(procedure-parameters (make-procedure '(a b) '((f a b)) %base-env))
       '(a b))
   (=? '(procedure-body (make-procedure '(a b) '((f a b)) %base-env))
       '((f a b)))
   (=? '(procedure-environment (make-procedure '(a b) '((f a b)) %base-env))
       %base-env)

   ;; Environments
   (=? '%null-env '())
   (=? '(make-frame '(a b) '(1 2)) '((a b) 1 2))
   (=? '(frame-variables (make-frame '(a b) '(1 2))) '(a b))
   (=? '(frame-values    (make-frame '(a b) '(1 2))) '(1 2))
   (=? '(let [(f (make-frame '() '()))] (add-binding-to-frame! 'z 9 f) f)
       (make-frame '(z) '(9)))
   (=? '(extend-environment '(a) '(1) %null-env) '(((a) 1)))
   (=?e '(extend-environment '(a) '() %null-env) "extend-environment: too few values")
   (=?e '(extend-environment '() '(1) %null-env) "extend-environment: too few variables")
   (=? '(first-frame (extend-environment '(a) '(1) %null-env)) '((a) 1))
   (=? '(letrec [(e1 (extend-environment '(a) '(1) %null-env))
		 (e2 (extend-environment '(b) '(2) e1))]
	  (enclosing-environment e2)) '(((a) 1)))

   ;; Lookups
   (=? '(lookup-variable-value 'test (extend-environment '(test) '(0) %null-env))
       0)
   (=?e '(lookup-variable-value 'not-here (extend-environment '(test) '(0) %null-env))
	"Undefined variable: ")
   ;; Quotes
   (=? '(text-of-quotation '(quote t1)) 't1)

   ;; Assignment 
   (=? '(assignment? '(set x 3)) #f)
   (=? '(assignment? '(set! x 3)) #t)
   (=? '(assignment-variable '(set! y 55)) 'y)
   (=? '(assignment-value '(set! y 55)) 55)

   ;; Ifs
   (=? '(if? '(if a b c)) #t)
   (=? '(if? '(fi a b c)) #f)
   (=? '(if-predicate '(if a b c)) 'a)
   (=? '(if-consequent '(if a b c)) 'b)
   (=? '(if-alternative '(if a b c)) 'c)
   (=? '(if-alternative '(if a b)) '%f)
   (=? '(make-if 'a 'b 'c) '(if a b c))

   ;; Conditional expressions
   (=? '(cond? '(cond ((a 1) (b 2)))) #t)
   (=? '(cond-clauses '(cond ((a 1) (b 2)))) '((a 1) (b 2)))
   (=? '(cond-predicate '(a 1)) 'a)
   (=? '(cond-actions '(a 1)) '(1))
   (=? '(cond-else-clause? '(else whatever)) #t)
   (=? '(cond->if '(cond ((a 1) (else 2)))) '(if a 1 2))
   (=? '(cond->if '(cond ((a 1)))) '(if a 1 %f))  ;; This is ugly...
   (=? '(cond->if '(cond ((a 1) (b 2) (else 3)))) '(if a 1 (if b 2 3)))
   (=? '(cond->if '(cond ((a 1) (b 2) (else 3)))) '(if a 1 (if b 2 3)))
   (=?e '(cond->if '(cond ((a 1) (else 3) (b 2)))) "ELSE is not last")

   ;; Sequences
   (=? '(sequence->exp '(1)) '1)
   (=? '(sequence->exp '(1 1)) '(begin 1 1))

   ;; Bools
   (=? '(true? '%t) #t)
   (=? '(true? '%f) #f)
   (=? '(false? '%f) #t)
   (=? '(false? '%t) #f)

   ;; Application/Evaluation
   (=? '(application? '((lambda(x) x) 3)) #t)
   (=? '(application? '(+ 3 4)) #t)
   (=? '(operator '(+ 1 2)) '+)
   (=? '(operands '(+ 1 2)) '(1 2))
   (=? '(no-operands? '()) #t)
   (=? '(no-operands? '(1 2)) #f)
   (=? '(first-operand '(1 2)) 1)
   (=? '(rest-operands '(1 2)) '(2))
   (=? '(list-of-values '(x y z) (extend-environment '(x y z) '(1 2 3) %base-env))
       '(1 2 3))

   ;; High-level Evaluation -- “acceptance” tests
   (=? '(%eval 3 %null-env) 3)
   (=? '(%eval '(begin 1 2 3 4)  %null-env) 4)
   (=? '(%eval '(begin (define x 77) x) %base-env) 77)
   (=? '(%eval '(quote x) %base-env) 'x)
   (=? '(%eval '(begin (define x 77) (set! x 66) x) %base-env) 66)
   (=? '(%eval '(if '%t 'conseq dont-eval-me) %base-env) 'conseq)
   (=? '(%eval '(if '%f dont-eval-me 'alt) %base-env) 'alt)
   (=? '(%eval '(cond (('%f dont-eval1) ('%f dont-eval2) ('%t 'yes))) %base-env) 'yes)
   (=? '(%eval '(lambda (x) x) %base-env) `(procedure (x) (x) ,%base-env))
   (=? '(%eval '((lambda(x) x) 3) %base-env) 3)
   (=? '(%eval '((lambda(x y) x) 1 2) %base-env) 1)
   (=? '(%eval '(+ 2 2) %base-env) 4)
   (=? '(%eval '(cons 2 (cons 3 '())) %base-env) '(2 3))
   (=?e '(%eval '(list 1 2 3) %base-env) "Undefined variable: ")

   ; Ex. 4.1
   (=?o '(list-of-values '((display 1)(display 2)(display 3)) %base-env)
       "123")
   (=?o '(list-of-values-backend-dependent
          '((display 1)(display 2)(display 3)) %base-env 'right)
        "321")
   (=?o '(list-of-values-backend-dependent
          '((display 1)(display 2)(display 3)) %base-env 'left)
        "123")
   (=?o '(list-of-values-backend-agnostic 
          '((display 1)(display 2)(display 3)) %base-env 'right)
        "123")
   (=?o '(list-of-values-backend-agnostic 
          '((display 1)(display 2)(display 3)) %base-env 'left)
        "123")

))
