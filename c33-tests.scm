(load "./c33.scm")
(load "./test.scm")

(test 
 '(
   (=? '(mystery (list 'a 'b 'c 'd)) (list 'd 'c 'b 'a))
   (=? '(count-pairs 316-proper-list) 3)
   (=? '(count-pairs 316-pathological-1) 4)
   (=? '(count-pairs 316-pathological-2) 7)
   ;(=? '(count-pairs 316-pathological-3) 'will-hang)
   ; ex 3.18
   (=? '(is-cyclical (list 1 2 3)) #f)
   (=? '(is-cyclical (list 1 2 2 3)) #f)
   (=? '(is-cyclical l1) #t)
   (=? '(is-cyclical 316-pathological-3) #t)
   ; ex 3.19
   (=? '(is-cyclical-f (list 1 2 3)) #f)
   (=? '(is-cyclical-f (list 1 2 2 3)) #f)
   (=? '(is-cyclical-f l1) #t)
   (=? '(is-cyclical-f 316-pathological-3) #t)
   
   ;ex 3.21
   (=? '(let ((q1 (make-queue)))
          (insert-queue! q1 'a))
       (cons (list 'a) (list 'a)))

   (=? '(show-queue (make-queue))
       "Queue []")

   (=? '(show-queue
         (let ((q1 (make-queue)))
          (insert-queue! q1 'a)
          (insert-queue! q1 'b)))
       "Queue (a b)")

   (=? '(show-queue
         (let ((q1 (make-queue)))
          (insert-queue! q1 'a)
          (delete-queue! q1)))
       "Queue []")

))
