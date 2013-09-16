(load "./c41.scm")
(load "./c41-4.2b.scm") ;; Ex. 4.2b: Louis Reasoner's LISP-2-ish evaluator
(load "./test.scm")

(run-tests
 '(
   ;; Unit tests

   ;; Data predicates
   (=? '(quoted? '(quote hello)) 'quote)
   (=? '(self-evaluating? 3) 'self-evaluating)
   (=? '(self-evaluating? "hello") 'self-evaluating)
   (=? '(self-evaluating? '(1 23)) #f)
   (=? '(tagged-list? '() 'tag) #f)
   (=? '(tagged-list? '(vv) 'vv) 'vv)
   (=? '(tagged-list? '(yy 1 2 3) 'yy) 'yy)
   (=? '(variable? 'x) 'variable)
   (=? '(variable? 2) #f)

   ;; Definitions
   (=? '(definition? '(define x 1)) 'define)
   (=? '(definition? '(define (f x) x)) 'define)
   (=? '(definition-variable '(define x 1)) 'x)
   (=? '(definition-variable '(define (f y) y)) 'f)
   (=? '(definition-value '(define x 0)) '0)
   (=? '(definition-value '(define (h x) x)) '(lambda (x) x))

   ;; Assignment
   (=? '(assignment? '(set! x 2)) 'set!)

   ;; Lambdas
   (=? '(lambda? '(hello world)) #f)
   (=? '(lambda? '(lambda (x) x)) 'lambda)
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
   (=? '(assignment? '(set! x 3)) 'set!)
   (=? '(assignment-variable '(set! y 55)) 'y)
   (=? '(assignment-value '(set! y 55)) 55)

   ;; Ifs
   (=? '(if? '(if a b c)) 'if)
   (=? '(if? '(fi a b c)) #f)
   (=? '(if-predicate '(if a b c)) 'a)
   (=? '(if-consequent '(if a b c)) 'b)
   (=? '(if-alternative '(if a b c)) 'c)
   (=? '(if-alternative '(if a b)) %f)
   (=? '(make-if 'a 'b 'c) '(if a b c))

   ;; Conditional expressions
   (=? '(cond? '(cond (a 1) (b 2))) 'cond)
   (=? '(cond-clauses '(cond (a 1) (b 2))) '((a 1) (b 2)))
   (=? '(cond-clauses '(cond (else 0))) '((else 0)))
   (=? '(cond-predicate '(a 1)) 'a)
   (=? '(cond-actions '(a 1)) '(1))
   (=? '(cond-else-clause? '(else whatever)) #t)
   (=? '(cond->if '(cond (else 2))) '2)
   (=? '(cond->if '(cond (a 1) (else 2))) '(if a 1 2))
   (=? '(cond->if '(cond (a 1))) '(if a 1 %f))  
   (=? '(cond->if '(cond (a 1) (b 2) (else 3))) '(if a 1 (if b 2 3)))
   (=? '(cond->if '(cond (a 1) (b 2) (else 3))) '(if a 1 (if b 2 3)))
   (=?e '(cond->if '(cond (a 1) (else 3) (b 2))) "ELSE is not last")

   ;; Sequences
   (=? '(sequence->exp '(1)) '1)
   (=? '(sequence->exp '(1 1)) '(begin 1 1))

   ;; Bools
   (=? '(true? %t) #t)
   (=? '(true? %f) #f)
   (=? '(false? %f) #t)
   (=? '(false? %t) #f)

   ;; Application/Evaluation
   (=? '(application? '((lambda(x) x) 3)) 'application)
   (=? '(application? '(+ 3 4)) 'application)
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
   (=? '(%eval '(false? %t) %base-env) %f)
   (=? '(%eval '(if %t 'conseq dont-eval-me) %base-env) 'conseq)
   (=? '(%eval '(if %f dont-eval-me 'alt) %base-env) 'alt)
   (=? '(%eval '(cond (%f dont-eval1) (%f dont-eval2) (%t 'yes)) %base-env) 'yes)
   (=? '(%eval '(lambda (x) x) %base-env) `(procedure (x) (x) ,%base-env))
   (=? '(%eval '((lambda(x) x) 3) %base-env) 3)
   (=? '(%eval '((lambda(x y) x) 1 2) %base-env) 1)
   (=? '(%eval '(+ 2 2) %base-env) 4)
   (=? '(%eval '(cons 2 (cons 3 '())) %base-env) '(2 3))
   (=?e '(%eval '(list 1 2 3) %base-env) "Undefined variable: ")

   ;; Ex. 4.1
   (=?o '(list-of-values '((display 1)(display 2)(display 3)) %base-env)
        "123")
   (=?o '(list-of-values-backend-dependent
          '((display 1)(display 2)(display 3)) %base-env 'left)
        "123")
   ;; Backend evals RTL, we're in trouble.
   (=?o '(list-of-values-backend-dependent
          '((display 1)(display 2)(display 3)) %base-env 'right)
        "321")

   (=?o '(list-of-values-backend-agnostic 
          '((display 1)(display 2)(display 3)) %base-env 'left)
        "123")
   ;; Backend evals RTL, but we get values in source order.
   (=?o '(list-of-values-backend-agnostic 
          '((display 1)(display 2)(display 3)) %base-env 'right)
        "123")

   ;; Ex. 4.2b
   (=?e '(%eval42b '(+ 1 2) %base-env42b) "%eval: unknown expression")
   (=? '(%eval42b '(call + 1 2) %base-env42b) 3)
   (=? '(%eval42b '(call + (call + 10 5) (call (lambda(x) 5) 'whatever)) %base-env42b) 20)
   (=? '(%eval42b '(begin (define (f x) (call + 1 x)) (call f 5)) %base-env42b) 6)

   ;; Ex. 4.3
   (=? '(type-of '(quote x)) 'quote)
   (=? '(type-of '(begin a b c)) 'begin)
   (=? '(type-of 3) 'self-evaluating)
   (=? '(type-of '(set! x 3)) 'set!)
   (=? '(type-of '(define x 3)) 'define)
   (=? '(type-of 'x) 'variable)
   (=? '(type-of '(if x y z)) 'if)
   (=? '(type-of '(cond (x y) z)) 'cond)
   (=? '(type-of '(lambda(x) (+ x x))) 'lambda)
   (=? '(type-of '(+ 1 2)) 'application)
   (=? '(first (lambda(x) (= x 0)) '(1 2 3)) #f)
   (=? '(first (lambda(x) (= x 0)) '(1 2 0 3)) #t)
   (=? '(get-eval-method 'quoted '((quoted . 0))) 0)
   (=? '(get-eval-method 'quoted '((not-here . #f) (quoted . 0))) 0)

   ;; Ex 4.4
   ;; ANDs 
   (=? '(and? '(and #t #t)) 'and)
   (=? '(and? '(blah #t #t)) #f)
   (=? '(and-actions '(and a b)) '(a b))
   (=? '(%eval '(and %t %t) %base-env) %t)
   (=? '(%eval '(and %t %f) %base-env) %f)
   (=?o '(%eval '(and (begin (display "1") %t)
                      (begin (display "2") %f)
                      (begin (display "3") %t)) %base-env) "12")
   ;; derived and
   (=? '(%eval '(dand %t %t) %base-env) %t)
   (=? '(%eval '(dand %t %f) %base-env) %f)
   (=?o '(%eval '(dand (begin (display "1") %t)
                       (begin (display "2") %f)
                       (begin (display "3") %t)) %base-env) "12")

   ;; ORs 
   (=? '(or? '(or #t #t)) 'or)
   (=? '(or? '(blah #t #t)) #f)
   (=? '(or-actions '(or a b)) '(a b))
   (=? '(%eval '(or %t %t) %base-env) %t)
   (=? '(%eval '(or %f %t) %base-env) %t)
   (=? '(%eval '(or %f %f) %base-env) %f)
   (=?o '(%eval '(or (begin (display "1") %f)
                     (begin (display "2") %t)
                     (begin (display "3") %f)) %base-env) "12")
   ;;   derived or
   (=? '(%eval '(dor %t %t) %base-env) %t)
   (=? '(%eval '(dor %f %t) %base-env) %t)
   (=? '(%eval '(dor %f %f) %base-env) %f)
   (=?o '(%eval '(dor (begin (display "1") %f)
                      (begin (display "2") %t)
                      (begin (display "3") %f)) %base-env) "12")

   ;; 4.5 Cond arrow syntax
   (=? '(arrow-syntax? '(=> grue)) #t)
   (=? '(arrow->exp '(cons 1 2) '(=> car)) '(car (cons 1 2)))
   (=? '(cond->if '(cond (a 1) (else 2))) '(if a 1 2))
   (=? '(cond->if '(cond ((cons 1 2) => car) (else 0))) '(if (cons 1 2) (car (cons 1 2)) 0))
   (=? '(%eval '(cond ((cons 1 2) => cdr) (else %f)) %base-env) '2)
   (=? '(%eval '(cond (%f => never-run) ((cons 2 3) => car) (else 0)) %base-env) '2)
   (=? '(%eval '(cond (%f => never-run) (else 'exit)) %base-env) 'exit)

   ;; 4.6 Let-to-lambda
   (=? '(let? '(let ((a 1)) a)) 'let)
   (=? '(let-bindings '(let ((a 1)) a)) '((a 1)))
   (=? '(let-body '(let ((a 1)) a)) '(a))
   (=? '(let-vars '(let ((a 1) (b 2)) a)) '(a b))
   (=? '(let-vals    '(let ((a 1) (b 2)) a)) '(1 2))
   (=? '(let->combination '(let ((a 1)) (f a))) '((lambda (a) (f a)) 1))
   (=? '(let->combination '(let ((a 1) (b 2)) (f a b))) '((lambda (a b) (f a b)) 1 2))
   (=? '(%eval '(let ((a 11)) a) %base-env) 11)
   (=? '(%eval '(let ((a 11) (b 22)) (+ a b)) %base-env) 33)

   ;; 4.7 let*
   (=? '(let*? '(let* ((a 1) (b a)) b)) 'let*)
   (=? '(let*->nested-let '(let* ((a 1) (b a)) b))
       '(let ((a 1)) (let ((b a)) b)))
   (=? '(%eval '(let* ((a 11) (b a) (c b)) 11) %base-env) 11)

   ;; If the evaluator already supports let, then it's enough that we transform
   ;; let* to let, and make the evaluator handle the expansion when it
   ;; gets to the generated let-clause.

   ;; 4.8 Named-let

   (=? '(let? '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a))))) 'let)
   (=? '(named-let? '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a))))) 'named-let)
   (=? '(named-let-name '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a)))))
       'foo)
   (=? '(named-let-bindings '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a))))) 
       '((a 1)))
   (=? '(named-let-vars '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a))))) 
       '(a))
   (=? '(named-let-vals '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a))))) 
       '(1))
   (=? '(named-let-body '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a)))))
       '((if (= a 10) (cons a "done") (foo (+ 1 a)))))
   (=? '(named-let->seq '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a)))))
       '(begin 
           (define foo (lambda (a) (if (= a 10) (cons a "done") (foo (+ 1 a)))))
           (foo 1)))

   (=? '(%eval '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a)))) %base-env)
       '(10 . "done"))
   ))
