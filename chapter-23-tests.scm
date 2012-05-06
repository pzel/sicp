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
   
   ;; ex 2.58 skipped

   ;; buildup to ex. 2.59
   (=? '(element-of-set? 1 (list 1 2 3)) '(1 2 3))
   (=? '(element-of-set? 4 (list 1 2 3)) #f)
   (=? '(adjoin-set 4 (list 1 2 3)) '(4 1 2 3))
   (=? '(intersection-set (list 1 2 3) (list 1 2 3 4)) '(1 2 3))
   (=? '(intersection-set '() (list 1 2 3)) '())
   ; 2.59
   (=? '(union-set '() '()) '())
   (=? '(union-set '() (list 1 2)) '(1 2))
   (=? '(union-set (list 1 2) '()) '(1 2))
   (=? '(union-set (list 1 2) (list 3 4)) '(1 2 3 4))

   ; 2.60
   (=? '(elem-of-dset? 1 (list 2 3 4 8 7 1 3 4 1)) #t)
   (=? '(elem-of-dset? 6 (list 2 3 4 8 7 1 3 4 1)) #f)
   (=? '(adjoin-dset 4 (list 1 2 3)) '(4 1 2 3))
   (=? '(intersection-dset (list 1 2 3) (list 1 2 3 4)) '(1 2 3))
   (=? '(intersection-dset '() (list 1 2 3)) '())
   (=? '(union-dset '() '()) '())
   (=? '(union-dset '() (list 1 2)) '(1 2))
   (=? '(union-dset (list 1 2) '()) '(1 2))
   (=? '(union-dset (list 1 2) (list 3 4)) '(1 2 3 4))

))