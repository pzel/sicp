
(define-syntax s-cons
  (syntax-rules ()
    ((s-cons <val> <str>)
     (cons <val> (delay <str>)))))

(define ES '())
(define s-null? null?)
(define (s-car s) (car s))
(define (s-cdr s) (force (cdr s)))

(define (s-ref s n)
  (if (<= n 0)
      (s-car s)
      (s-ref (s-cdr s)
             (- n 1))))

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
  (if (<= n 0)
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

