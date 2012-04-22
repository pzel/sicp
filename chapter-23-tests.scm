(load "chapter-23.scm")
(load "test.scm")


(test 
 '(
   (=? '(memq? 'apple '()) #f)
   (=? '(memq? 'apple (list 'apple 'banana 'cranberry)) (list 'apple 'banana 'cranberry))
   (=? '(memq? 'apple (list 'banana 'pomegranate)) #f)
   
   ;; ex. 2.54
   (=? '(_equal? '(this is a list) '(this is a lista)) #f)
;   (=? '(_equal? '(this is a list) '(this is a list)) #t)

))