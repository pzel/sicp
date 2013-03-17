(load "./c35.scm")
(load "./test.scm")

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
           (s-map s1 (lambda(x) (* x x))))
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

   ))
