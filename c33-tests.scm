(load "./c33.scm")
(load "./test.scm")

; for testing stateful simulations
(define-syntax run-simulation
  (syntax-rules ()
    ((run-simulation <body>)
     (begin
       (define a (make-agenda))
       (define get-current-agenda (lambda() a))
       <body>)
     )))

(run-tests
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

   (=? '(let* ((good-enuff? (lambda(x y) (< (abs (- x y)) 0.1)))
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
          (v-insert! eq? t (list 'one) 'value) 
          (v-lookup eq? t (list 'one)))
       'value)

   ; lookup only returns the value if it is atomic
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'world 'africa 'mozambique) 'maputo) 
          (v-lookup eq? t (list 'world)))
       #f)

   ; if table is made 2-dimensional, the 1d value for a key is forgotten
   (=? '(let ((t (make-table)))
          (v-insert! eq? t (list 'one) 'hello)
          (v-insert! eq? t (list 'one 'two) 'hello2)
          (v-lookup eq? t (list 'one)))
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


   ;;   circuits
   (=? '(let ((w (make-wire)))
          (w 'get-signal))
       0)

   (=? '(let ((w (make-wire)))
          ((w 'set-signal!) 1)
          (w 'get-signal))
       1)

   (=? '(run-simulation 
          (let* ((in (make-wire))
                   (out (make-wire))
                   (inv (inverter in out)))
            (set-signal! in 0)
            (propagate)
            (get-signal out)))
       1)

   (=? '(run-simulation
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (and-g (and-gate in1 in2 out)))
          (set-signal! in1 1)
          (propagate)
          (get-signal out)))
       0)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (and-g (and-gate in1 in2 out)))
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       0)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (and-g (and-gate in1 in2 out)))
          (set-signal! in1 1)
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       1)

   ; ex. 3.28
   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                  (in2 (make-wire))
                  (out (make-wire))
                  (or-g (or-gate in1 in2 out)))
           (propagate)
          (get-signal out)))
       0)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (or-g (or-gate in1 in2 out)))
          (set-signal! in1 1)
          (propagate)
          (get-signal out)))
       1)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (or-g (or-gate in1 in2 out)))
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       1)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (or-g (or-gate in1 in2 out)))
          (set-signal! in1 1)
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       1)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (nand-g (nand-gate in1 in2 out)))
           (propagate)                 
          (get-signal out)))
       1)
   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (nand-g (nand-gate in1 in2 out)))
          (set-signal! in1 1)
          (propagate)
          (get-signal out)))
       1)
   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (nand-g (nand-gate in1 in2 out)))
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       1)
   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (nand-g (nand-gate in1 in2 out)))
          (set-signal! in1 1)
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       0)
   ; ex. 3.29
   ; slow implementation, takes 2* NAND gate delay
   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (or-g (or-gate-on-nand in1 in2 out)))
           (propagate)
           (get-signal out)))
       0)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (or-g (or-gate-on-nand in1 in2 out)))
          (set-signal! in1 1)
          (propagate)
          (get-signal out)))
       1)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (or-g (or-gate-on-nand in1 in2 out)))
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       1)

   (=? '(run-simulation 
         (let* ((in1 (make-wire))
                 (in2 (make-wire))
                 (out (make-wire))
                 (or-g (or-gate-on-nand in1 in2 out)))
          (set-signal! in1 1)
          (set-signal! in2 1)
          (propagate)
          (get-signal out)))
       1)

   (=? '(run-simulation 
         (let* ((a (make-wire))
                 (b (make-wire))
                 (sum (make-wire))
                 (carry (make-wire))
                 (ha (half-adder a b sum carry)))
          (set-signal! a 1)
          (propagate)
          (list (get-signal sum) (get-signal carry))))
       (list 1 0))

   (=? '(run-simulation 
         (let* ((a (make-wire))
                  (b (make-wire))
                  (sum (make-wire))
                  (carry (make-wire))
                  (ha (half-adder a b sum carry)))
           (set-signal! b 1)
           (propagate)
          (list (get-signal sum) (get-signal carry))))
       (list 1 0))

   (=? '(run-simulation 
         (let* ((a (make-wire))
                  (b (make-wire))
                  (sum (make-wire))
                  (carry (make-wire))
                  (ha (half-adder a b sum carry)))
           (set-signal! a 1)
           (set-signal! b 1)
           (propagate)
          (list (get-signal sum) (get-signal carry))))
       (list 0 1))
   
   (=? '(run-simulation 
         (let* ((a (make-wire))
                  (b (make-wire))
                  (carry-in (make-wire))
                  (carry-out (make-wire))
                  (sum (make-wire))
                  (fa (full-adder a b carry-in sum carry-out)))
           (set-signal! a 1)
           (propagate)
           (list (get-signal sum) (get-signal carry-out))))
       (list 1 0))

   (=? '(run-simulation 
         (let* ((a (make-wire))
                  (b (make-wire))
                  (carry-in (make-wire))
                  (carry-out (make-wire))
                  (sum (make-wire))
                  (fa (full-adder a b carry-in sum carry-out)))
           (set-signal! a 1)
           (set-signal! b 1)
           (propagate)
           (list (get-signal sum) (get-signal carry-out))))
       (list 0 1))

   (=? '(run-simulation 
         (let* ((a (make-wire))
                  (b (make-wire))
                  (carry-in (make-wire))
                  (carry-out (make-wire))
                  (sum (make-wire))
                  (fa (full-adder a b carry-in sum carry-out)))
           (set-signal! carry-in 1)
           (propagate)
           (list (get-signal sum) (get-signal carry-out))))
       (list 1 0))
   
   (=? '(run-simulation 
         (let* ((a (make-wire))
                  (b (make-wire))
                  (carry-in (make-wire))
                  (carry-out (make-wire))
                  (sum (make-wire))
                  (fa (full-adder a b carry-in sum carry-out)))
           (set-signal! a 1)
           (set-signal! b 1)
           (set-signal! carry-in 1)
           (propagate)
           (list (get-signal sum) (get-signal carry-out))))
         (list 1 1))

   ; ex. 3.30
   (=? '(run-simulation 
         (let ((wires (make-wires (list 1 0 0))))
           (propagate)
           (get-signals wires)))
         (list 1 0 0))

   (=? '(run-simulation 
         (let ((wires (make-wires (list 1 0 0))))
          (set-signals! wires (list 0 1 1))
          (propagate)
          (get-signals wires)))
       (list 0 1 1))

   (=? '(repeat 4 (lambda() 'x))
       (list 'x 'x 'x 'x))

   (=? '(run-simulation 
         (let* ((a (make-wires (list 0)))
                 (b (make-wires (list 1)))
                 (c (make-wire))
                 (sum (make-wires (list 0)))
                 (rca (ripple-carry-adder a b c sum)))
           (propagate)
           (get-signals sum)))
       (list 1))
   
   (=? '(run-simulation 
         (let* ((a (make-wires (list 0 1 1 1 0)))
                 (b (make-wires (list 0 0 0 1 0)))
                 (c (make-wire))
                 (sum (make-wires (list 0 0 0 0 0)))
                 (rca (ripple-carry-adder a b c sum)))
           (propagate)
           (get-signals sum)))
       (list 1 0 0 0 0))

   ; probes
   (=?o '(run-simulation
         (let* ((in1 (make-wire))
                  (in2 (make-wire))
                  (sum (make-wire))
                  (carry (make-wire))
                  (ps (probe 'sum sum))
                  (pc (probe 'carry carry))
                  (ha (half-adder in1 in2 sum carry)))
           (set-signal! in1 1)
           (propagate)))
        "sum at: 0 New value = 0\ncarry at: 0 New value = 0\nsum at: 8 New value = 1\n")


   (=?o '(run-simulation
         (let* ((in1 (make-wire))
                  (in2 (make-wire))
                  (sum (make-wire))
                  (carry (make-wire))
                  (ps (probe 'sum sum))
                  (pc (probe 'carry carry))
                  (ha (half-adder in1 in2 sum carry)))
           (set-signal! in1 1)
           (propagate)
           (set-signal! in2 1)
           (propagate)))
        "sum at: 0 New value = 0\ncarry at: 0 New value = 0\nsum at: 8 New value = 1\ncarry at: 11 New value = 1\nsum at: 16 New value = 0\n")


   ; Celsius-Fahrenheit converter
   (=?o '(run-simulation
         (let* ((c (make-connector))
                  (f (make-connector)))
           (celsius-fahrenheit-converter c f)
           (probe-connector "fahrenheit temperature" f)
           (set-value! c 25 'user)))
       "Probe: fahrenheit temperature = 77\n")

   (=?e '(run-simulation
          (let* ((c (make-connector))
                   (f (make-connector)))
            (celsius-fahrenheit-converter c f)
            (probe-connector "fahrenheit temperature" f)
            (set-value! c 25 'user)
            (set-value! f 888 'user)))
        '("contradiction" 77 888))

   (=?o '(run-simulation
         (let* ((c (make-connector))
                  (f (make-connector)))
           (celsius-fahrenheit-converter c f)
           (probe-connector "fahrenheit temperature" f)
           (set-value! c 25 'user)
           (forget-value! c 'user)))
       "Probe: fahrenheit temperature = 77\nProbe: fahrenheit temperature = ?\n")

   (=?o '(run-simulation
         (let* ((c (make-connector))
                  (f (make-connector)))
           (celsius-fahrenheit-converter c f)
           (probe-connector "celsius temperature" c)
           (set-value! c 25 'user)
           (forget-value! c 'user)
           (set-value! f 212 'user)))
       "Probe: celsius temperature = 25\nProbe: celsius temperature = ?\nProbe: celsius temperature = 100\n")

   ; ex. 3.33
   (=?o '(run-simulation
         (let* ((a (make-connector))
                  (b (make-connector))
                  (avg (make-connector)))
           (averager a b avg)
           (probe-connector "average" avg)
           (set-value! a 3 'user)
           (set-value! b 0 'user)))
       "Probe: average = 1.5\n")

   ; ex. 3.34
   ; The two a's represent the same wire, but (multiplier) is 
   ; implemented in a way that requires its three connectors to be unique.
   (=?o '(run-simulation
         (let* ((a (make-connector))
                  (b (make-connector)))
           (squarer34 a b)
           (probe-connector "sqrt" a)
           (set-value! b 25 'user)))
       "") ; no cond predicate is satisfied inside the multiplier, 
           ; information doesn't reach conn a 

   ; ex. 3.35 
   (=?e '(let* ((a (make-connector))
                 (b (make-connector)))
          (squarer a b)
          (set-value! b -5 'user))
       '("squarer: square less than 0:" -5))

   (=?o '(run-simulation
         (let* ((a (make-connector))
                  (b (make-connector)))
           (squarer a b)
           (probe-connector "sqrt" a)
           (set-value! b 25 'user)))
       "Probe: sqrt = 5.0\n") 

   (=?o '(run-simulation
         (let* ((a (make-connector))
                  (b (make-connector)))
           (squarer a b)
           (probe-connector "square" b)
           (set-value! a 2 'user)))
       "Probe: square = 4\n") 

   (=?o '(run-simulation
         (let* ((a (make-connector))
                  (b (make-connector)))
           (squarer a b)
           (probe-connector "square" b)
           (set-value! a 2 'user)
           (forget-value! a 'user)))
       "Probe: square = 4\nProbe: square = ?\n") 

   ; ex 3.36
   ; skipped

   ; ex 3.37
   (=?o '(run-simulation
         (let* ((c (make-connector))
                  (f (celsius->fahrenheit-converter c)))
           (probe-connector "fahrenheit" f)
           (set-value! c 25 'user)))
       "Probe: fahrenheit = 77.0\n")

;   (=? '(car x) "unbound variable")
   
))
