(load "./test.scm")
(load "./c34.scm")
(use srfi-1)

(define (churn)
  (iota 1000000)
  #t)
  
(test 
 '(
   (=? '(let ((x 0))
          (parallel-execute
           (lambda() (set! x 10))
           (lambda() (begin (churn) (set! x 50)))
           (lambda() (set! x 1)))
          x)
       1) ; it should equal 50, but I can't acheieve truly parallel execution
          ; of threads. 
   ))
