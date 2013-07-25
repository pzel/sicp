(load "./c41.scm")
(load "./test.scm")

(run-tests
 '(
   ;; Unit tests
   ;; Data predicates
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
   ;; Lambdas
   (=? '(lambda? '(hello world)) #f)
   (=? '(lambda? '(lambda (x) x)) #t)
   (=? '(lambda-parameters '(lambda (x y) 2)) '(x y))
   (=? '(lambda-body '(lambda (x y) x)) '(x))
   (=? '(lambda-body '(lambda (x y) (f x y))) '((f x y)))
   (=? '(make-lambda '(x y) '((+ x y))) '(lambda (x y) (+ x y)))
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
   (=? '(lookup-variable-value 'test
			       (extend-environment '(test) '(0) %null-env))
       0)
   (=?e '(lookup-variable-value 'not-here
				(extend-environment '(test) '(0) %null-env))
	"Undefined variable: ")

   ;; High-level Evaluation
   (=? '(%eval 3 %null-env) 3)
   (=? '(%eval '(begin 1 2 3 4)  %null-env) 4)
   (=? '(%eval '(begin (define x 77) x) %base-env) 77)
;   (=? '(%eval '(+ 2 2) %null-env) 4)
))
