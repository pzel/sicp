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
(use fmt fmt-color)


(define (=? is should)
  (test-compare is should equal?))

(define (=?~ is should)
  (test-compare is should test-close-enough?))

; Internals
(define (test-close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (test-compare is should matcher)
  (let ((result (eval is)))
    `(,(matcher result should)
      ,result
      ,(eval should)
      ,is)))

(define (filter_ p l)
  (define (iter p l res)
    (if (null? l) 
	res 
	(if (p (car l))
	    (iter p (cdr l) (cons (car l) res))
	    (iter p (cdr l) res))))
  (iter p l '()))

(define (show-red s)
  (fmt #t (fmt-red s)))

(define (show-bold s)
  (fmt #t (fmt-bold (fmt-red s))))

(define (display-failure l)
  (display "In expression: ")
  (show-red (cadddr l))  (newline)
  (display "Expected:\t")
  (show-red (caddr l))  (newline)  
  (display "Got:\t\t")
  (show-bold (cadr l))   (newline)
)

(define (results l)
  (map eval l))

(define (test l)
  (if (null? 
       (map 
        display-failure 
        (filter_ 
         (lambda (x)(not (equal? (car x) #t))) 
         (results l))))
      (begin (fmt #t (fmt-blue  (dsp "All tests OK.")) nl) 
             #t)
      #f))
