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
   ;;unit test nested table functons
   ;empty key list has no effect
   (=? '(let ((t (make-table)))
          (v-insert! eq? t '() 'hello)
          t)
       (list 'Table))

   ;one item in list makes a key in current table
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'one) 'hello) 
          t)
       (list 'Table (cons 'one 'hello)))

   ;using insert on the same key overwrites the value
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'one) 'hello) 
          (v-insert! eq? t (list 'one) 'hello-world) 
          t)
       (list 'Table (cons 'one 'hello-world)))

   ;passing two keys gives a 2d table
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'one 'two) 'hello)
          t)
       (list 'Table (list 'one (cons 'two 'hello))))

   ;a new table is added even in previous table had a key only
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'one) 'hello)
          (v-insert! eq? t (list 'one 'two) 'hello2)
          t)
       (let ((l (list 'one (cons 'two 'hello2))))
         (set-cdr! (cdr l) 'hello)
       (list 'Table l)))

   ; empty lookup list always returns false, no matter the table
   (=? '(let ((t (make-table)))
          (v-insert! eq? t '() 'hello) 
          (v-lookup eq? t '()))
       #f)

   ; simple case
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'erykah) 'badu) 
          (v-lookup eq? t (list 'erykah)))
       'badu)

   ; lookup only returns the value if it is atomic
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'world 'africa 'mozambique) 'maputo) 
          (v-lookup eq? t (list 'world)))
       #f)

   ; lookup returns the value if it atomic in deeply nested tables
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'world 'africa 'mozambique) 'maputo) 
          (v-lookup eq? t (list 'world 'africa 'mozambique)))
       'maputo)

   ; insert overwrited deep table entries
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'world 'africa 'madagascar) 'antanananarivo) 
          (v-insert! eq? t (list 'world 'africa 'madagascar) 'antananarivo)
          (v-lookup eq? t (list 'world 'africa 'madagascar)))
       'antananarivo)

   ;; ex. 3.25
   (=? '(let ((world (make-v-table-obj)))
          ((world 'insert!) (list 'eu 'sweden) 'stockholm)
          ((world 'lookup) (list 'eu 'sweden)))
       'stockholm)

))
