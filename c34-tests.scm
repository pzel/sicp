(load "./c34.scm")
(load "./test.scm")

(run-tests
 '(
   ;; Rset! and rambda will always be slower than set! and lambda!
   (=? '(begin
          (define x 0)
          (parallel-execute (rambda() (rset! x 2)) (lambda() (set! x 1)))
          x)
       2)
))
