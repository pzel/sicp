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

  ;tables
   
   (=? '(make-table)
       (list 'Table))

   (=? '(let ((t1 (make-table)))
          (lookup 'some-key t1))
       #f)
             
   (=? '(let ((eu (make-table)))
          (insert! 'poland 'warsaw eu)
          (lookup 'poland eu))
       'warsaw)

   (=? '(let ((world (make-table)))
          (insert2! 'eu 'germany 'berlin world)
          (lookup2 'eu 'germany world))
       'berlin)

   (=? '(let ((world (make-table)))
          (insert2! 'eu 'germany 'berlin world)
          (lookup2 'eu 'brazil world))
       #f)

   (=? '(let ((world (make-table-obj)))
          ((world 'insert!) 'eu 'sweden 'stockholm)
          ((world 'lookup) 'eu 'sweden))
       'stockholm)

   (=? '(let ((world (make-table-obj)))
          ((world 'insert!) 'eu 'germany 'berlin)
          ((world 'lookup) 'eu 'switzerland))
       #f)

   (=? '(letrec ((good-enuff? (lambda(x y) (< (abs (- x y)) 0.1)))
                (loose-table (make-table-comp good-enuff?)))
          ((loose-table 'insert!) 1.0 2.0 3)
          ((loose-table 'lookup) 1.001 2.0009))
       3)

))
