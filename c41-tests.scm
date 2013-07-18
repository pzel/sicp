(load "./c41.scm")
(load "./test.scm")

(run-tests
 '(
   (=? '(%eval 3 %null-env) 3)))
