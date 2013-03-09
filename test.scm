;------------------------------------------
; A simple test suite.
; Copyright 2010 pzel
; This code is licenced under the GPLv3.
; http://www.gnu.org/licenses/gpl-3.0.txt 
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
; 
; Tested in:
; * CHICKEN Version 4.6.0 linux-unix-gnu-x86 [ manyargs dload ptables ]
; * CHICKEN Version 4.7.0 linux-unix-gnu-x86 [ manyargs dload ptables ]

(define (test-eval exp)
  (eval exp (interaction-environment)))

(define (=? is should)
  (test-compare is should equal?))

(define (=?~ is should)
  (test-compare is should test-close-enough?))

(define (=?e is msg)
  (test-for-error is msg))

; Internals
(define (test-close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (test-compare is should matcher)
  (let ((result (test-eval is)))
    (list (matcher result should)
          result
          should
          is)))

(define (show obj)
  (with-output-to-string 
    (lambda() (display obj))))

(define (test-for-error is msg)
  (let ((result (call/cc 
                 (lambda(k)
                   (with-exception-handler
                    (lambda(exn)
                      (k ((condition-property-accessor 'exn 'message) exn)))
                    (lambda() (test-eval is)))))))
        (list (string=? (show result) (show msg))
              result
              msg
              is)))

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

