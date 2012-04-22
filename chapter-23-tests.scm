(load "chapter-23.scm")
(load "test.scm")


(test 
 '(
   (=? '(memq? 'apple '()) #f)
   (=? '(memq? 'apple (list 'apple 'banana 'cranberry)) (list 'apple 'banana 'cranberry))
   (=? '(memq? 'apple (list 'banana 'pomegranate)) #f)
   
   ;; ex. 2.54
   (=? '(_equal? '(this is a list) '(this is a lista)) #f)
   (=? '(_equal? '(this is a list) '(this is a list)) #t)

   ; 2.3.2 Symbolic differatiation

   ;; ex 2.56
   (=? '(deriv '(+ x 3) 'x) 1)
   (=? '(deriv '(* x y) 'x) 'y)
   (=? '(deriv '(** x 3) 'x) '(* 3 (** x 2)))

   ;; ex 2.57
   (=? '(deriv '(+ x 1 2) 'x) 1)
   (=? '(deriv '(* (* x y) (+ x 3)) 'x)  '(+ (* x y) (* (+ x 3) y)))
   (=? '(deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
))