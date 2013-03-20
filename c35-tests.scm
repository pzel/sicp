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
          (s-ref s1 0))
       10)

   (=? '(let ((s1 (s-cons 10 (s-cons 20 ES))))
          (s-ref s1 1))
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
           (s-ref s 5)
           (s-ref s 7))
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

           (s-ref y 7)
           (d sum) 

           (with-output-to-string (lambda() (s-display z))) ; don't print
           (d sum))
        "1 6 10 136 210 ")

   (=?s '(s-take 117 integers)
        (s-enumerate-interval 1 117))

   (=? '(s-ref no-sevens 100)
       117)

   (=? '(s-ref fibs 5)
       5)

   ; sieve of Eratosthenes
   (=? '(s-ref primes 50)
       233)
   
   ))
