(import (scheme small))
(include "./c42.scm")
(include "./test.scm")

; helper method
(define (%eval/env exp) (actual-value exp %base-env))

(run-tests
 '(
   (=? '(%eval 3 %null-env) 3)
   (=? '(%eval '(begin 1 2 3 4)  %null-env) 4)
   (=? '(%eval/env '(begin (define x 77) x)) 77)
   (=? '(%eval/env '(quote x)) 'x)
   (=? '(%eval/env '(begin (define x 77) (set! x 66) x)) 66)
   (=? '(%eval/env '(false? %t)) %f)
   (=? '(%eval/env '(if %t 'conseq dont-eval-me)) 'conseq)
   (=? '(%eval/env '(if %f dont-eval-me 'alt)) 'alt)
   (=? '(%eval/env '(cond (%f dont-eval1) (%f dont-eval2) (%t 'yes))) 'yes)
   (=? '(%eval  '(lambda (x) x) %base-env) `(procedure (x) (x) ,%base-env))
   (=? '(%eval/env '((lambda(x) x) 3)) 3)
   (=? '(%eval/env '((lambda(x y) x) 1 2)) 1)
   (=? '(%eval/env '(+ 2 2)) 4)
   (=? '(%eval/env '(cons 2 (cons 3 '()))) '(2 3))
   (=?e '(%eval/env '(undef 1 2 3)) "Undefined variable: ")
   (=? '(%eval/env '(and %t %t)) %t)
   (=? '(%eval/env '(and %t %f)) %f)
   (=?o '(%eval/env '(and (begin (display "1") %t)
                      (begin (display "2") %f)
                      (begin (display "3") %t))) "12")
   (=? '(%eval/env '(or %t %t)) %t)
   (=? '(%eval/env '(or %f %t)) %t)
   (=? '(%eval/env '(or %f %f)) %f)
   (=?o '(%eval/env '(or (begin (display "1") %f)
                     (begin (display "2") %t)
                     (begin (display "3") %f))) "12")
   (=? '(%eval/env '(cond ((cons 1 2) => cdr) (else %f))) '2)
   (=? '(%eval/env '(cond (%f => never-run) ((cons 2 3) => car) (else 0))) '2)
   (=? '(%eval/env '(cond (%f => never-run) (else 'exit))) 'exit)
   (=? '(%eval/env '(let () 3)) 3)
   (=? '(%eval/env '(let ((a 11)) a)) 11)
   (=? '(%eval/env '(let ((a 11) (b 22)) (+ a b))) 33)
   (=? '(%eval/env '(let* ((a 11) (b a) (c b)) 11)) 11)
   (=? '(%eval/env '(let foo ((a 1))(if (= a 10) (cons a "done") (foo (+ 1 a)))))
       '(10 . "done"))
   (=? '(%eval/env '(begin (define (f x)
                            (define (even? x) (if (= x 0) %t (odd? (- x 1))))
                            (define (odd? x) (if (= x 0) %f (even? (- x 1))))
                            (even? x)) (f 8)))
      %t)
   (=? '(%eval/env '(begin (define (f x)
                       (letrec ((even? (lambda(x) (if (= x 0) %t (odd? (- x 1)))))
                                (odd? (lambda(x) (if (= x 0) %f (even? (- x 1))))))
                         (even? x))) (f 8)))
      %t)
  
  ;; Ex. 4.25
  ;; The call will never complete in a strict language,
  ;; because all arguments to unless will be evaluated, inluding the
  ;; recursive call to factorial.
  ;; It will work perfectly well in a lazy language.

  ;; Ex. 4.26
  ;; transform (unless c a b) -> (if (not c) a b)

  ;;  Lazy application
  (=? '(%eval/env '(begin (define (try a b) (if (= 0 a) 1 b)) (try 0 (/ 1 0))))
      1)

  ;; ;; Memoization  
  (=?o '(%eval/env '(begin
		      (define (a) (display "a") 1)
		      (let ((x (a))) (+ x x x))))
        "a")


  ;; Ex. 4.27
  (=?o '(%eval/env 
	 '(begin
	    (define count 0)
	    (define (id x) (set! count (+ count 1)) x)
	    (define w (id (id 10)))
	    (display count) (display " ")
	    (display w) (display " ")
	    (display count)))
       "1 10 2")

  ;; Ex. 4.28 ; this will break if apply does not receive an (actual-value)'d operator
  (=? '(%eval/env 
	'(begin
	   (define (ap f arg)
                      ;; f is a thunk at this point. (before ap's body is exec'd)
	     (f arg)) ;; f must be strictly evaluated at this point in the body,
	              ;; because the apply-*-procedure functions cannot work with a thunk
	   (ap (lambda(x) (+ 1 x)) 1)))
      2)
  
  ;; Ex. 4.29
  (=?o '(%eval/env 
	 '(begin
	    (define count 0)
	    (define (id x) (set! count (+ count 1)) x)
	    (define (square x) (* x x))
	    (display (square (id 10))) (display " ")
	    (display count)))
       "100 1")

  ;; Ex. 4.30
  ;; I agree with the proposed modification. If we need a sequence of statements,
  ;; it means the statements below depend on those on top. If the statements need 
  ;; not be executed in order 
  ;; let(*) should be used instead.
  (=?o '(%eval/env
	 '(begin
	    (define (p2 x) (define (p e) e x) 
	      (p (begin (display "i ") (set! x (cons x '(2))) (display "ran "))))
	    (display (p2 1))))
       "i ran (1 2)")

  (=? '(%eval/env 
	'(begin
	   (define (nonseq-statements x y)
	     (let* ((square-x (* x x))
		    (square-y (* y y))
		    (sum-of-squares (+ square-x square-y)))
	       sum-of-squares))
	   (nonseq-statements 1 2)))
       5)

  ;; Ex. 4.31 skip

  ))
