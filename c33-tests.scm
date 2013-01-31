(load "./c33.scm")
(load "./test.scm")

(test 
 '(
   (=? '(mystery (list 'a 'b 'c 'd)) (list 'd 'c 'b 'a))
   (=? '(count-pairs (list 1 2 3)) 3)
   (=? '(letrec ((x (cons 'a (cons 'a (cons 'a '())))))
          (count-pairs x)) 3)
))
