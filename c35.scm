; use prime.scm for prime-sum-pairs-s
(load "./prime.scm")

(define-syntax s-cons
  (syntax-rules ()
    ((s-cons <val> <str>)
     (cons <val> (delay <str>)))))

(define ES '())
(define s-null? null?)
(define (s-car s) (car s))
(define (s-cdr s) (force (cdr s)))

(define (s-ref n s)
  (if (<= n 0)
      (s-car s)
      (s-ref (- n 1)
             (s-cdr s))))

(define (s-map f . ss)
  (if (s-null? (car ss))
      ES
      (s-cons 
       (apply f (map s-car ss))
       (apply s-map
              (cons f (map s-cdr ss))))))

(define (s-for-each f s)
  (if (s-null? s)
      #t
      (begin 
        (f (s-car s))
        (s-for-each f (s-cdr s)))))

(define (s-display s)
  (s-for-each (lambda(line) (display line) (newline)) 
              s))

(define (s-take n s)
  (if (or (<= n 0) (s-null? s))
      ES
      (s-cons (s-car s)
              (s-take (- n 1) (s-cdr s)))))
      
(define (s-enumerate-interval low high)
  (if (> low high)
      ES
      (s-cons low
              (s-enumerate-interval (+ low 1) high))))

(define (s-filter p s)
  (cond ((s-null? s) ES)
        ((p (s-car s))
         (s-cons (s-car s) (s-filter p (s-cdr s))))
        (else
         (s-filter p (s-cdr s)))))
         
(define (integers-from n)
  (s-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (divisible? x y) (= 0 (remainder x y)))

(define no-sevens
  (s-filter (lambda(x) (not (divisible? x 7)))
            integers))

(define (fibgen a b)
  (s-cons a
          (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (s-cons (s-car stream)
          (sieve (s-filter (lambda(x)
                             (not (divisible? x (s-car stream))))
                           (s-cdr stream)))))
(define primes
  (sieve (integers-from 2)))

(define (add-streams s1 s2)
  (s-map + s1 s2))

(define fibs_
  (s-cons 0
          (s-cons 1
                  (add-streams fibs
                               (s-cdr fibs)))))

(define primes_
  (s-cons 2
          (s-filter prime?_
                    (integers-from 3))))

(define (prime?_ x)
  (define (iter x ps)
    (cond ((> (square (s-car ps)) x) #t)
          ((divisible? x (s-car ps)) #f)
          (else
           (iter x (s-cdr ps)))))
  (iter x primes_))

; repeat
(define (s-repeat n)
  (s-cons n (s-repeat n)))

; to-list
(define (s-to-list limit s)
  (define (iter s)
    (if (s-null? s)
        (begin (display s)'())
        (cons (s-car s)
              (iter (s-cdr s)))))
  (iter (s-take limit s)))

;ex 3.54
(define (mul-streams s1 s2)
  (s-map * s1 s2))

(define factorials
  (s-cons 1
          (mul-streams integers
                       factorials)))
;ex 3.55
(define (partial-sums s)
  (s-cons (s-car s)
          (add-streams (s-cdr s)
                       (partial-sums s))))

;ex 3.56
(define (s-scale factor s)
  (s-map (lambda(x) (* x factor))
         s))

(define (s-merge s1 s2)
  (cond ((s-null? s1) s2)
        ((s-null? s2) s1)
        (else
         (let ((s1car (s-car s1))
               (s2car (s-car s2)))
           (cond ((< s1car s2car)
                  (s-cons s1car
                          (s-merge (s-cdr s1) s2)))
                 ((> s1car s2car)
                  (s-cons s2car
                          (s-merge s1 (s-cdr s2))))
                 (else
                  (s-cons s1car
                          (s-merge (s-cdr s1)
                                   (s-cdr s2)))))))))

(define hamming
  (s-cons 1
          (s-merge (s-scale 2 hamming)
                   (s-merge (s-scale 3 hamming)
                            (s-scale 5 hamming)))))

; ex 3.58
(define (s-expand num den radix)
  (s-cons
   (quotient (* num radix) den)
   (s-expand (remainder (* num radix) den) den radix)))

; 3.59-3.62 -- TODO: understand the math, complete the exercises.
;

(define (avg . args)
  (/ (apply + args)
     (length args)))

(define (sqrt-improve guess x)
  (avg guess 
       (/ x guess)))

(define (sqrt-s x)
  (define guesses
    (s-cons 1.0
            (s-map (lambda(g)
                     (sqrt-improve g x))
                   guesses)))
  guesses)

(define (pi-summands x)
  (s-cons (/ 1.0 x)
          (s-map - (pi-summands (+ x 2)))))

(define pi-s
  (s-scale 4 (partial-sums (pi-summands 1))))

(define (euler-t s)
  (let ((s0 (s-ref 0 s))
        (s1 (s-ref 1 s))
        (s2 (s-ref 2 s)))
    (s-cons (- s2
               (/ (square (- s2 s1))
                  (+ s2 (- s0 (* 2 s1)))))
            (euler-t (s-cdr s)))))
            
(define (make-tableau t s)
  (s-cons s
          (make-tableau t (t s))))

(define (accel-seq t s)
  (s-map s-car (make-tableau t s)))


; ex: 3.64
(define (s-limit delta s)
  (if (< (abs (- (s-car s) (s-car (s-cdr s)))) delta)
      (s-car (s-cdr s))
      (s-limit delta (s-cdr s))))
           
(define (sqrt_ x tolerance)
  (s-limit tolerance (sqrt-s x)))

;ex 3.65

(define (ln2-summands x)
  (s-cons (/ 1.0 x)
          (s-map - (ln2-summands (+ x 1)))))

(define naive-ln2
  (partial-sums (ln2-summands 1)))

(define (mk-pair a b)
  (cons a b))
(define (fst p) (car p))
(define (snd p) (cdr p))

(define (pairs-s s1 s2)
  (s-cons (mk-pair (s-car s1) (s-car s2))
          (interleave (s-map (lambda(x) (mk-pair (s-car s1) x)) (s-cdr s2))
                      (pairs-s (s-cdr s1)
                               (s-cdr s2)))))         

(define (interleave s1 s2)
  (if (s-null? s1)
      s2
      (s-cons (s-car s1)
              (interleave s2 (s-cdr s1)))))

(define prime-sum-pairs-s
  (s-filter (lambda(p) (prime? (+ (fst p) (snd p))))
            (pairs-s integers integers)))

; ex. 3.66
(define (s-find matcher s)
  (define (iter s count)
    (cond ((s-null? s)
           #f)
          ((matcher (s-car s))
           count)
          (else 
            (iter (s-cdr s) (+ 1 count)))))
  (iter s 0))

; ex. 3.67
(define (all-pairs-s s1 s2)
  (s-cons (mk-pair (s-car s1) (s-car s2))
          (interleave (s-map (lambda(x) (mk-pair (s-car s1) x)) (s-cdr s2))
                      (interleave (s-map (lambda(x) (mk-pair x (s-car s1))) 
                                         (s-cdr s2))
                                  (all-pairs-s (s-cdr s1) (s-cdr s2))))))

; ex. 3.68
; this will loop forever
(define (naive-pairs-s s1 s2)
  (interleave
    (s-map (lambda(x) (mk-pair (s-car s1) x)) s2)
    (naive-pairs-s (s-cdr s1) (s-cdr s2))))

;ex 3.69
(define (p-triple? t)
 (let ((a (car t))
       (b (cadr t))
       (c (caddr t)))
   (and (> b a)
        (= (+ (* a a) (* b b))
           (* c c)))))

(define (interleave3 s t u)
  (if (s-null? s)
      (interleave t u)
      (s-cons (s-car s)
              (interleave3 t u (s-cdr s)))))

(define (triples-s s t u)
 (let ((a (s-car s))
       (b (s-car t))
       (c (s-car u)))
   (s-cons 
    (list a b c)
    (interleave3
     (s-map (lambda(b* c*) (list a b* c*)) t         (s-cdr u))
     (s-map (lambda(b* c*) (list a b* c*)) (s-cdr t) (s-cdr u))
     (triples-s (s-cdr s) (s-cdr t) (s-cdr u))))))
                      
(define p-triples-s 
  (s-filter p-triple? (triples-s integers integers integers)))

; for testing
(define (s-monotonic? limit s)
  (define (iter n last s)
    (cond ((= n 0) #t)
          ((> last (s-car s)) #f)
          (else
           (iter (- n 1) (s-car s) (s-cdr s)))))
  (iter limit 0 s))

;ex 3.70
;a
(define (merge-by w s1 s2)
  (let ((p1 (s-car s1))
        (p2 (s-car s2)))
    (cond ((< (w p1) (w p2))
           (s-cons p1 (merge-by w (s-cdr s1) s2)))
          ((= (w p1) (w p2))
           (s-cons p1 (merge-by w (s-cdr s1) s2)))
          (else
           (s-cons p2 (merge-by w s1 (s-cdr s2)))))))

(define (pairs-by w s1 s2)
  (s-cons (mk-pair (s-car s1) (s-car s2))
          (merge-by w
                    (s-map (lambda(x) (mk-pair (s-car s1) x)) (s-cdr s2))
                    (pairs-by w (s-cdr s1) (s-cdr s2)))))

(define (pair-sum p)
  (+ (fst p)
     (snd p)))

(define (pair-prod p)
  (* (fst p)
     (snd p)))

;b
(define (pair-235 p)
  (+ (* 2 (fst p))
     (* 3 (snd p))
     (* 5 (fst p) (snd p))))

(define no235s
  (s-filter (lambda(x) (and (not (divisible? x 2))
                            (not (divisible? x 3))
                            (not (divisible? x 5))))
            integers))

; ex. 3.71
(define (s-conseq n f s)
  (define (iter prev s)
    (cond ((null? prev)
           (iter (list (s-car s)) (s-cdr s)))
          ((= n (length prev))
           (s-cons prev
                   (iter (list (s-car s)) (s-cdr s))))
          ((f (car prev) (s-car s))
           (iter (cons (s-car s) prev) (s-cdr s)))
          (else
           (iter (list (s-car s)) (s-cdr s)))))
  (iter '() s))


(define (cube x) (* x x x))

(define (sum-of-cubes p)
  (+ (cube (fst p))
     (cube (snd p))))

(define (eq-sum-of-cubes p1 p2)
  (= (sum-of-cubes p1)
     (sum-of-cubes p2)))

(define cube-weighted-pairs
  (pairs-by sum-of-cubes
            integers
            integers))

(define ramanujan
  (s-conseq 2 eq-sum-of-cubes cube-weighted-pairs))

;ex. 3.72
(define (s-zip s1 s2)
  (s-map cons s1 s2))

(define (sum-of-squares p)
  (+ (square (fst p))
     (square (snd p))))

(define (eq-sum-of-squares p1 p2)
  (= (sum-of-squares p1)
     (sum-of-squares p2)))

(define square-weighted-pairs
  (pairs-by sum-of-squares integers integers))

(define square-triplets
  (s-conseq 3 eq-sum-of-squares square-weighted-pairs))
