(define empty-stream '())
(define null-stream? null?)

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream <val> <str>)
     (cons <val>
           (delay <str>)))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s)
                  (- n 1))))
(define (stream-map s f)
  (if (null-stream? s)
      empty-stream
      (cons-stream (f (stream-car s))
                   (stream-map (stream-cdr s) f))))

(define (stream-for-each f s)
  (if (null-stream? s)
      #t
      (begin 
        (f (stream-car s))
        (stream-for-each f (stream-cdr s)))))
      

