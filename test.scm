;------------------------------------------
; A simple test suite.
; Copyright 2010-13 pzel (Simon Zelazny)
; This code is licenced under the MIT licence
;------------------------------------------
;
; USAGE:
;  (run-tests *list-of-test-cases*)
; 
; The *list-of-test-cases* contains n sexps
; with the following structure
; (<TEST-MATCHER> '(<EXPR>) <EXPECTED-RESULT>)
;
; [EXAMPLE]
; (run-tests '(
;    (=?   '(+ 2 2) 4)                     ; test result of expression
;    (=?~  '(sqrt 3) 1.732)                ; test numeric result with delta
;    (=?o  '(display "hello") "hello")     ; test output
;    (=?e  '(car '()) "bad argument type") ; test error message
;    (=?s  '(<stream1>) <stream2>)         ; test stream equality (delay/force)
;  ))
;
; Tested in:
; * CHICKEN Version 4.7.0 linux-unix-gnu-x86 [ manyargs dload ptables ]


; TEST MATCHERS

(define (=? is should)
  (test-compare is should equal?))

(define (=?~ is should)
  (test-compare is should test-close-enough?))

(define (=?e is msg)
  (test-compare-error is msg))

(define (=?s is should)
  (test-compare is should test-stream-equal?))

(define (=?o is should)
  (test-compare-output is should ))

; INTERNALS
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

(define dev/null
  (make-output-port (lambda(in) #t)
                    (lambda(close) #t)))

(define (mk-error exn)
  (list 'error
        (show ((condition-property-accessor 'exn 'message) exn))
        (show ((condition-property-accessor 'exn 'arguments) exn))))

(define (show obj) (with-output-to-string (lambda() (display obj))))

(define (test-compare is should matcher)
  (let ((result (test-eval is 'hide-output)))
    (list (matcher result should)
          result
          should
          is)))

(define (test-compare-error is msg)
  (let ((result (catch (lambda() (test-eval is 'hide-output)))))
    (list (test-error-equal? result msg)
          result
          msg
          is)))

(define (test-compare-output is should)
  (let ((result (test-eval is 'capture-output)))
    (list (test-string-equal? result should)
          result
          should
          is)))

(define (test-eval exp io)
  (cond  ((eq? io 'hide-output)
          (with-output-to-port dev/null 
            (lambda()
              (eval exp (interaction-environment)))))
         ((eq? io 'capture-output)
          (with-output-to-string 
            (lambda()
              (eval exp (interaction-environment)))))
         (else
          (error "test.scm: test-eval doesn't know how to treat output"))))
    
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

(define (test-string-equal? s1 s2)
  (equal? s1 s2))


(define (test-philter p l)
  (define (iter p l res)
    (if (null? l)
        (reverse res)
        (if (p (car l))
            (iter p (cdr l) (cons (car l) res))
            (iter p (cdr l) res))))
  (iter p l '()))


; TEST RUNNER + DISPLAY

(define (run-tests l)
  (letrec ((total (length l))
           (r (get-results l))
           (errors (test-philter (lambda(x) (not (equal? (car x) #t))) r)))
    (if (null? errors)
        (show-success total)
        (show-errors errors total))))

(define (get-results l)
  (map (lambda(exp) (test-eval exp 'hide-output)) l))

(define (show-errors errors total)
  (map show-error errors)
  (letrec ((err (length errors))
           (ok (- total err)))
    (display (format "~n * Failed: ~s\tPassed: ~s\t Total: ~s.~n" err ok total))
  #f))

(define (show-error l)
  (newline)
  (display " * In expression:    ")
  (display (cadddr l))  (newline)
  (display "   Expected:         ")
  (display (caddr l))  (newline)  
  (display "   Got:              ")
  (display (cadr l)) (newline))

(define (show-success n)
  (display  (format " * All tests OK (~s)~n" n))
  #t)
