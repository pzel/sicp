(include "./c34.scm")
(include "./test.scm")

(run-tests
 '(
   ;; Rset! and rambda will always be slower than set! and lambda!
   (=? '(begin
          (define x 0)
          (parallel-execute (rambda() (rset! x 2)) 
                            (lambda() (set! x 1))
                            (lambda() (set! x 0)))
          x)
       2)
))
