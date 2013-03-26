(load "./c35.scm")
(load "./test.scm")
(load "./prime.scm")

(run-tests
 '(
   (=? 'ES '())
   (=? '(s-null? ES) #t)
   (=? '(s-car (s-cons 3 ES)) 3)
   (=? '(s-cdr (s-cons 3 ES)) ES)
   (=? '(s-car (s-cdr (s-cons 3 (s-cons 4 ES)))) 4)

   (=? '(let ((s1 (s-cons 10 ES)))
          (s-ref 0 s1))
       10)

   (=? '(let ((s1 (s-cons 10 (s-cons 20 ES))))
          (s-ref 1 s1))
       20)

   (=?s '(let ((s1 (s-cons 10 (s-cons 20 ES))))
           (s-map (lambda(x) (* x x)) s1))
        (s-cons 100 (s-cons 400 ES)))

   (=? '(let ((res (cons 'result '())))
          (s-for-each (lambda(x) (set-cdr! res (cons x (cdr res))))
                      (s-cons 10 (s-cons 20 ES)))
          res)
        (list 'result 20 10))

   (=?o '(s-display (s-cons 1 (s-cons 2 ES)))
        "1\n2\n")

   (=?s '(s-cons 1 (s-cons 2 (s-cons 3 ES)))
        (list 1 2 3))

   (=?s '(s-take 2 (s-cons 1 (s-cons 2 (s-cons 3 ES))))
       (list 1 2))

   (=?s '(s-enumerate-interval 5 8)
        (list 5 6 7 8))

   (=?s '(s-filter (lambda(el) (eq? el 3))
                        (s-cons 1 (s-cons 2 (s-cons 3 ES))))
        (list 3))

   (=?s '(s-take 2 (s-filter
                  prime? 
                  (s-enumerate-interval 10000 1000000)))
        (list 10007 10009))

   ; ex. 3.50 : general s-map
   (=?s '(s-map (lambda(x y z) (+ x y z))
                (s-cons 0  (s-cons 1  (s-cons 2 ES)))
                (s-cons 10 (s-cons 11 (s-cons 12 ES)))
                (s-cons 90 (s-cons 100 (s-cons 120 ES))))
        (list 100 112 134))

   ; ex. 3.51 : lazy evaluation w/ memoization
   (=?o '(letrec ((show (lambda(x) (display x) x))
                  (s (s-map show (s-enumerate-interval 0 10))))
           (s-ref 5 s)
           (s-ref 7 s))
        "01234567")

   ; ex. 3.52
   (=?o '(begin
           (define (d x) (display x) (display " "))
           (define sum 0)
           (define (accum x) (set! sum (+ x sum)) sum)
           (define seq (s-map accum (s-enumerate-interval 1 20)))
           (d sum) 

           (define y (s-filter even? seq))
           (d sum) 

           (define z (s-filter (lambda(x) (= (remainder x 5) 0)) seq))
           (d sum) 

           (s-ref 7 y)
           (d sum) 

           (with-output-to-string (lambda() (s-display z))) ; don't print
           (d sum))
        "1 6 10 136 210 ")

   (=?s '(s-take 117 integers)
        (s-enumerate-interval 1 117))

   (=? '(s-ref 100 no-sevens)
       117)

   (=? '(s-ref 5 fibs)
       5)

   ; sieve of Eratosthenes
   (=? '(s-ref 50 primes)
       233)

   (=? '(s-ref 5 fibs_)
       5)
   
   ; lazy self-referential primes
   (=? '(s-ref 50 primes_)
       233)
   
   ; Ex. 3.53 will return powers of two
   (=? '(begin
          (define s (s-cons 1 (add-streams s s)))
          (s-ref 6 s))
       64)

   ; repeat
   (=?s '(s-take 5 (s-repeat 1))
        (list 1 1 1 1 1))

   ; Ex. 3.54
   (=?s '(s-take 5 (mul-streams (integers-from 1)
                               (integers-from 2)))
        (list 2 6 12 20 30))


   (=?s '(s-take 8 factorials)
        (list 1 1 2 6 24 120 720 5040))

   ; stream to list (for debugging)
   (=? '(s-to-list 3 (s-cons 1 (s-cons 2 ES)))
       (list 1 2))

   ; ex. 3.55
   (=? '(s-to-list 5 (partial-sums integers))
        (list 1 3 6 10 15))

   (=? '(s-to-list 5 (partial-sums (s-repeat 1)))
        (list 1 2 3 4 5))

   ; ex. 3.56: Hamming numbers
   (=?s '(s-merge (s-cons 1 (s-cons 3 ES))
                  (s-cons 2 (s-cons 3 (s-cons 4 ES))))
        (list 1 2 3 4))

   (=? '(s-to-list 5 (s-scale 2 integers))
        (list 2 4 6 8 10))
        
   (=? '(s-to-list 20 hamming)
       (list 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36))


   ; ex. 3.57 n times

   ; ex. 3.58
   (=? '(s-to-list 7 (s-expand 1 7 10))
       (list 1 4 2 8 5 7 1))

   ; exs. 3.59-3.62 -- TODO


   (=? '(s-ref 1 (sqrt-s 2))
       1.5)

   (=?~ '(s-ref 10 (sqrt-s 2))
        1.414213)

   (=?~ '(s-ref 4 pi-s)
        3.33968253968254)

   (=?~ '(s-ref 4 (euler-t pi-s))
        3.14271284271284)

   (=?~ '(s-ref 4 (accel-seq euler-t pi-s))
        3.1415927140337)

   ; ex. 3.63
   ; This implementation has to map the lambda(guess) function for all 
   ; previous elelments of the stream every time stream-cdr is called.
   ; The performance hit would remain if the implementation of delay was not
   ;  memoized.


   ; ex. 3.64
   (=?~ '(s-limit 0.01 pi-s)
        3.146)

   (=?~ '(sqrt_ 16 4.0)
        5.191)

   (=?~ '(sqrt_ 16 0.01)
        4.00)

   ; ex. 3.65
   ; lg(2) is ~ 0.69314718
   (=?~ '(s-ref 4 naive-ln2)
        0.78333)

   (=?~ '(s-ref 4 (euler-t naive-ln2))
        0.69358)

   (=?~ '(s-ref 2 (accel-seq euler-t naive-ln2))
       0.69314718)

  (=? '(s-to-list 8 prime-sum-pairs-s)
    '( (1 . 1) (1 . 2) (2 . 3) (1 . 4) (1 . 6) (3 . 4) (2 . 5) (1 . 10 )))
    
  ; ex. 3.66
  (=? '(s-find (lambda(x) (= x -1)) (s-take 10 integers))
        #f)
  (=? '(s-find (lambda(x) (= x 1)) integers)
        0)
  (=? '(s-find (lambda(x) (= x 10)) integers)
        9)
  (=? '(s-find (lambda(x) (equal? x '(1 . 100))) (pairs-s integers integers))
        197)

  ; ex. 3.67
  (=? '(s-to-list 5 (all-pairs-s integers integers))
      '( (1 . 1) (1 . 2) (2 . 1) (1 . 3) (2 . 2)))
  (=? '(s-find (lambda(x) (equal? x `(1 . 5))) (all-pairs-s integers integers))
       7)
  (=? '(s-find (lambda(x) (equal? x `(5 . 1))) (all-pairs-s integers integers))
       14)
   ))
