(load "./c33.scm")
(load "./test.scm")

(test 
 '(
   (=? '(mystery (list 'a 'b 'c 'd)) (list 'd 'c 'b 'a))
))
