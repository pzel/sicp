(define empty-stream '())
(define null-stream? null?)

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream <val> <str>)
     (cons <val>
           (delay <str>)))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
