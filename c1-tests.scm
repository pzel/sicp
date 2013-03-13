(load "./c1.scm")
(load "./test.scm")

(run-tests '(
   (=?  '(abs -2) 2)
   (=?  '(abs  2) 2)
   (=?  '(abs  0) 0)
   (=?~ '(half-interval-method sin 2.0 4.0) 
        3.1415)
   (=?~ '(half-interval-method (lambda(x) (- (* x x x) (* 2 x) 3)) 1.0 2.0) 
        1.8930)
   (=?~ '(fixed-point cos 0.1)
        0.73908)
   (=?~ '(fixed-point (lambda(y) (+ (sin y) (cos y))) 1.0)
        1.2587)
   (=?~ '(sqrt 25) 5.0)
   (=?~ '(fixed-point (lambda(y) (+ 1 (/ 1 y))) 0.1) golden-mean)
   (=?~ '(f136-damp) 4.55554)   ; Thanks, Wolfram Alpha!
   (=?~ '(f136-nodamp) 4.55554) 
   (=?~ '(cont-frac (lambda(i) 1.0) (lambda(i) 1.0) + 8) (/ 1 golden-mean))
   (=?~ '(cont-fracc (lambda(i) 1.0) (lambda(i) 1.0) + 8) (/ 1 golden-mean))
   (=?~ '(cont-frac (lambda(i) 1.0) euler-denom + 8) (- E 2))
   (=?~ '(tan-cf 1 5) 1.5574)
   (=?~ '(tan-cf 2 10) -2.185)
   (=?~ '(tan-cf 3 15) -0.1425)
   (=?~ '(newtons-method (cubic 1 2 3) 0.1) -1.27568)
   (=?~ '(newtons-method (cubic 3 7 11) 0.1)  -2.13473) 
   (=?  '((double incr) 0) 2)
   (=?  '(((double (double double)) incr) 0) 16)
   (=?  '(((double (double (double double))) incr) 0) 256)
   (=?  '((compose square incr) 2) 9)
   (=?  '((repeated square 0) 2) 2)
   (=?  '((repeated square 1) 2) 4)
   (=?  '((repeated square 3) 2) 256)
   ))
