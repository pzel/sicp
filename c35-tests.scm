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

   (=?s '(s-take 2 (s-cons 1 (s-cons 2 (s-cons 3 ES))))
       (s-cons 1 (s-cons 2 ES)))

   (=?s '(s-enumerate-interval 5 8)
        (s-cons 5 (s-cons 6 (s-cons 7 (s-cons 8 ES)))))

   (=?s '(s-filter (lambda(el) (eq? el 3))
                        (s-cons 1 (s-cons 2 (s-cons 3 ES))))
        (s-cons 3 ES))

   (=?s '(s-take 2 (s-filter
                  prime? 
                  (s-enumerate-interval 10000 1000000)))
        (s-cons 10007 (s-cons 10009 ES)))

; ex. 3.50 : general s-map
   (=?s '(s-map (lambda(x y z) (+ x y z))
                (s-cons 0  (s-cons 1  (s-cons 2 ES)))
                (s-cons 10 (s-cons 11 (s-cons 12 ES)))
                (s-cons 90 (s-cons 100 (s-cons 120 ES))))
        (s-cons 100 (s-cons 112 (s-cons 134 ES))))

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

           (with-output-to-string (lambda() (s-display z))) ; don't test for this
           (d sum))

        "1 6 10 136 210 ")

   ))
