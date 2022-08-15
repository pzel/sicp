(import (scheme small))
(include "c23.scm")
(include "test.scm")

(run-tests
 '(
   (=? '(memq? 'apple '()) #f)
   (=? '(memq? 'apple (list 'apple 'banana 'cranberry)) (list 'apple 'banana 'cranberry))
   (=? '(memq? 'apple (list 'banana 'pomegranate)) #f)
   
   ;; ex. 2.54
   (=? '(_equal? '(this is a list) '(this is a lista)) #f)
   (=? '(_equal? '(this is a list) '(this is a list)) #t)

   ;; 2.3.2 Symbolic differatiation
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

   ;; 2.59
   (=? '(union-set '() '()) '())
   (=? '(union-set '() (list 1 2)) '(1 2))
   (=? '(union-set (list 1 2) '()) '(1 2))
   (=? '(union-set (list 1 2) (list 3 4)) '(1 2 3 4))

   ;; 2.60
   (=? '(elem-of-dset? 1 (list 2 3 4 8 7 1 3 4 1)) #t)
   (=? '(elem-of-dset? 6 (list 2 3 4 8 7 1 3 4 1)) #f)
   (=? '(adjoin-dset 4 (list 1 2 3)) '(4 1 2 3))
   (=? '(intersection-dset (list 1 2 3) (list 1 2 3 4)) '(1 2 3))
   (=? '(intersection-dset '() (list 1 2 3)) '())
   (=? '(union-dset '() '()) '())
   (=? '(union-dset '() (list 1 2)) '(1 2))
   (=? '(union-dset (list 1 2) '()) '(1 2))
   (=? '(union-dset (list 1 2) (list 3 4)) '(1 2 3 4))

   ;; buildup to 2.61
   (=? '(oset 4 2 1 3) (list 1 2 3 4))
   (=? '(elem-of-oset? 3 (oset 8 74 3)) #t)
   (=? '(intersection-oset (oset 1 2 3 4 5 6) (oset 4 5 6 7 8)) (oset 4 5 6))
   (=? '(intersection-oset '() (oset 1 2 3)) '())
   (=? '(intersection-oset (oset 7 8 9) (oset 1 2 3)) '())
   (=? '(adjoin-oset 2 '()) (oset 2))
   (=? '(adjoin-oset 2 (oset 1 3)) (oset 1 2 3))
   (=? '(union-oset '() '()) '())
   (=? '(union-oset '() (oset 1 2)) '(1 2))
   (=? '(union-oset (oset 1 2) '()) '(1 2))
   (=? '(union-oset (oset 1 2) (oset 8 9)) (oset 1 2 8 9))

   ;; 2. 65
   (=? '(union-bset (bset '()) (bset '())) (bset '()))
   (=? '(union-bset (bset '()) (bset '(1 2))) (bset '(1 2)))
   (=? '(union-bset (bset '(1 2)) (bset '())) (bset '(1 2)))
   (=? '(union-bset (bset '(1 2)) (bset '(8 9))) (bset '(1 2 8 9)))
   (=? '(intersection-bset (bset '(1 2 3 4 5 6)) (bset '(5 4 6 7 8))) (bset '( 4 5 6)))
   (=? '(intersection-bset (bset '()) (bset '(1 2 3))) (bset '()))
   (=? '(intersection-bset (bset '(7 8 9)) (bset '(1 2 3))) (bset '()))

   ;; 2.66
   (=? '(lookup 2 (bset '())) #f)
   (=? '(lookup 2 (bset '(1 2 3))) 2)
   (=? '(lookup 99 (bset '(1 2 3 4 5 6 77 88 99 100 122))) 99)

   ;; 2.68 
   (=? '(h-encode-symbol 'A sample-tree) '(0))
   (=? '(h-encode-symbol 'D sample-tree) '(1 1 0))
   (=? '(h-encode '(A D A B B C A) sample-tree) sample-message)
   (=? '(h-encode (h-decode sample-message sample-tree) sample-tree) sample-message)

   ;; 2.69
   (=? '(generate-h-tree sample-pairs) sample-tree)
   (=? '(h-encode '(A D A B B C A) (generate-h-tree sample-pairs)) sample-message)
   (=? '(h-encode (h-decode sample-message sample-tree) (generate-h-tree sample-pairs)) sample-message)

   ;; 2.70

   
))
