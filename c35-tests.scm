(load "./c35.scm")
(load "./test.scm")

(test 
 '(
   (=? 'empty-stream '())
   (=? '(null-stream? empty-stream) #t)
   (=? '(stream-car (cons-stream 3 empty-stream)) 3)
   (=? '(stream-cdr (cons-stream 3 empty-stream)) empty-stream)
   (=? '(stream-car (stream-cdr (cons-stream 3 (cons-stream 4 empty-stream)))) 4)

   (=? '(let ((s1 (cons-stream 10 empty-stream)))
          (stream-ref s1 0))
       10)

   (=? '(let ((s1 (cons-stream 10 (cons-stream 20 empty-stream))))
          (stream-ref s1 1))
       20)

   (=?s '(let ((s1 (cons-stream 10 (cons-stream 20 empty-stream))))
           (stream-map s1 (lambda(x) (* x x))))
        (cons-stream 100 (cons-stream 400 empty-stream)))

   ))
