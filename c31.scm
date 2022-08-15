(import (scheme small))
; ex. 3.1

(define (make-accumulator a)
  (lambda(x) (set! a (+ a x)) a))

; ex 3.2

(define (make-monitored f)
  (let ((n 0))
    (lambda x 
      (if (eq? (car x) 'show-n)
          n
          (begin
            (set! n(+ n 1))
            (apply f x))))))

; ex 3.3
(define (make-account balance)
  (define (withdraw x)
    (if (>= balance x)
        (begin (set! balance (- balance x))
               balance)
        "insufficient funds"))
  (define (deposit x)
    (set! balance (+ balance x))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unknown method" m))))
  dispatch)
    
