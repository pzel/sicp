
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

(define (s-map s f)
  (if (s-null? s)
      ES
      (s-cons (f (s-car s))
              (s-map (s-cdr s) f))))

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
