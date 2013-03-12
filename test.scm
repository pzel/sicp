;------------------------------------------
; A simple test suite.
; Copyright 2010-13 pzel (Simon Zelazny)
; This code is licenced under the MIT licence
;------------------------------------------
;
; USAGE:
;  (test *list-of-test-cases*)
; 
; The *list-of-test-cases* contains n sexps
; containing the following:
; (=? '(function-name arg1 ... argn) expected-result)
;
; [EXAMPLE]
; (test '(
;    (=?   '(which-power? 64 2) 6)
;    (=?   '(cdr3 (cons3 23 56)) 56)
;    (=?~  '(sqrt 3) 1.732))))
;    (=?e  '(car '()) "bad argument type")
;    (=?s  '(<stream1>) <stream2>)
; 
; Tested in:
; * CHICKEN Version 4.7.0 linux-unix-gnu-x86 [ manyargs dload ptables ]

(define (=? is should)
  (test-compare is should equal?))

(define (=?~ is should)
  (test-compare is should test-close-enough?))

(define (=?e is msg)
  (test-compare-error is msg))

(define (=?s is should)
  (test-compare is should test-stream-equal?))

; Internals
(define (test-eval exp)
  (eval exp (interaction-environment)))

(define (catch proc)
  (call/cc 
   (lambda(k)
     (with-exception-handler
      (lambda(exn) (k (mk-error exn)))
      proc))))

(define (cmp-error e1 e2)
  (and (equal? 'error (car e1))
       (equal? (show (cadr e1))
               (show (cadr e2)))))

(define (mk-error exn)
  (list 'error
        (show ((condition-property-accessor 'exn 'message) exn))
        (show ((condition-property-accessor 'exn 'arguments) exn))))

(define (test-compare-error is msg)
  (let ((result (catch (lambda() (test-eval is)))))
    (list (test-error-equal? result msg)
          result
          msg
          is)))

(define (test-close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (test-error-equal? e msg)
  (cmp-error e (list 'error msg)))

(define (test-stream-equal? s1 s2)
  (cond ((and (null? s1) (null? s2)) #t)
        ((or (null? s1) (null? s2)) #f)
        ((eq? (car s1) (car s2))
         (test-stream-equal? (force (cdr s1))
                             (force (cdr s2))))))

(define (test-compare is should matcher)
  (let ((result (test-eval is)))
    (list (matcher result should)
          result
          should
          is)))

(define (show obj)
  (with-output-to-string 
    (lambda() (display obj))))

(define (test-philter p l)
  (define (iter p l res)
    (if (null? l)
        (reverse res)
        (if (p (car l))
            (iter p (cdr l) (cons (car l) res))
            (iter p (cdr l) res))))
  (iter p l '()))

(define (display-failure l)
  (newline)
  (display " * In expression:    ")
  (display (cadddr l))  (newline)
  (display "   Expected:         ")
  (display (caddr l))  (newline)  
  (display "   Got:              ")
  (display (cadr l)) (newline))

(define (results l) 
  (map test-eval l))

(define (test l)
  (letrec ((total (length l))
           (r (results l))
           (errors (test-philter (lambda(x) (not (equal? (car x) #t))) r)))
    (if (null? errors)
        (show-success total)
        (show-errors errors total))))

(define (show-success n)
  (display  (format " * All tests OK (~s)~n" n))
  #t)

(define (show-errors errors total)
  (map display-failure errors)
  (letrec ((err (length errors))
           (ok (- total err)))
    (display (format "~n * Failed: ~s\tPassed: ~s\t Total: ~s.~n" err ok total))
  #f))
