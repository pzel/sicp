; SICP, chapter 1
(define (decr x)
  (- x 1))

(define (incr x)
  (+ x 1))

(define (/= a b)
  (not (= a b)))

(define (average x y)
  (/ (+ x y)
     2.0))

(define (close-enough? x y)
  (< (abs (- x y))
      0.00001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ( (test-value (f midpoint)) )
          (cond ( (positive? test-value)
                  (search f neg-point midpoint))
                ( (negative? test-value)
                  (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Function values have the same sign.")))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess steps)
    (define next (f guess))
    (if (close-enough? next guess)
        next
        (try next (+ steps 1))))
  (try first-guess 0))

(define (fixed-point-debug f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess steps)
    (let ((next (f guess)))
    (if (close-enough? next guess)
        (begin (display steps) (newline) next)
        (try next (+ steps 1)))))
  (try first-guess 0))

(define log1000 (log 1000))                                

(define (f136-nodamp)
  (fixed-point (lambda(y) (/ log1000 (abs (log y)))) 0.1))

(define (f136-damp)
  (fixed-point (lambda(y) (average y (/ log1000 (abs (log y))))) 0.1))
   
(define golden-mean 1.61803)

(define (cont-frac n d d-operation k)
  (define (iter i)
      (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (d-operation (d i)
                     (iter (+ 1 i))))))
  (iter 1))


(define (cont-fracc n d d-operation k)
  (define (iter i res)
    (if (= 1 i)
        res
        (iter (decr i) 
              (/ (n i) 
                 (d-operation (d i) res)))))
  (iter k (/ (n k) 
             (d k))))


(define E 2.71828)
;ii:         1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21
;ii/3        0  0  1  1  1  2  2  2  3  3  3  4  4  4  5  5  5  6  6  6  7
;(ii/3)-2         -1        0        1        2        3        4        5  
;ii/3 * 3          3        6        9        12       15       18       21 
;want:       1  1  4  1  1  6  1  1  8  1  1  10 1  1  12 1  1  14 1  1  16

(define (euler-denom i)
  (if (<= i 2) 
      i
      (letrec ((ii (- i 2))
               (mod3  (modulo ii 3))
               (div3  (quotient ii 3)))
        (if (/= 0 mod3)
            1
            (- (* 3 div3)
               (- div3 2))))))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x (* x x)))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d - k))


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda(x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.0001)


(define (newton-transform g)
  (lambda(x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda(y) (- (* y y) x))
                            newton-transform
                            1))

(define (cubic a b c)
  (lambda(x) 
    (+ (* x x x)
       (* a (* x x))
       (* b x)
       c))
    )

; Tests
(load "./test.scm")
(test '(
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
   ))