(load "./c35.scm")
(load "./test.scm")

(test 
 '(
   (=? 'empty-stream '())
   (=? '(null-stream? empty-stream) #t)
   (=? '(stream-car (cons-stream 3 empty-stream)) 3)
   (=? '(stream-cdr (cons-stream 3 empty-stream)) empty-stream)
   (=? '(stream-car (stream-cdr (cons-stream 3 (cons-stream 4 empty-stream)))) 4)
))
