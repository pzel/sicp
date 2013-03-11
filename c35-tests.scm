(load "./c35.scm")
(load "./test.scm")

(test 
 '(
   (=? 'the-empty-stream '())
   (=? '(stream-null? the-empty-stream) #t)

))
