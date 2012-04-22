(define (memq? sym x)
  (cond ((null? x) #f)
        ((eq? sym (car x)) x)
        (else
         (memq? sym (cdr x)))))


; Exercise 2.54
(define (_equal? l1 l2)
  (cond ((and (null? l1) (null? l2))
         #t)
        ((and (atom? l1) (atom? l2))
         (eq? l1 l2))
        (else (and (_equal? (car l1) (car l2))
                   (_equal? (cdr l1) (cdr l2))))))
        