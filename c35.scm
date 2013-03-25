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