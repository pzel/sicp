(load "./c31.scm")
(load "./test.scm")

(run-tests
 '(
   (=? '(let ((a (make-accumulator 0))) 
   				(list (a 10) (a 10) (a 34))) 
       	(list 10 20 54))
       		
   (=? '(let ((m-sqrt (make-monitored sqrt))) 
   				(list (m-sqrt 16) (m-sqrt 'show-n)))
       	(list 4.0 1))
   (=? '(let ((m-list (make-monitored list))) 
   				(list (m-list 1 2 3) (m-list 'show-n)))
       (list (list 1 2 3) 1))
   (=? '(let ((acc (make-account 1))) 
   				(list ((acc 'deposit) 1) ((acc 'withdraw) 10)))
       (list 2 "insufficient funds"))
))
