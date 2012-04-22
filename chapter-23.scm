(use srfi-1)
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
        

; 2.3.2 Symbolic Differentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)

         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var))))
        ((expt? exp)
         (make-product (exponent exp)
                       (make-product (make-expt (base exp) 
                                                (make-sum (exponent exp) -1))
                                     (deriv (base exp) var))))
                       



        (else
         (error "Unknown expression type -- DERIV" exp))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (numbers? . vars)
  (=  (length (filter number? vars))
      (length vars)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((numbers? a1 a2) (+ a1 a2))
        (else 
         (list '+ a1 a2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (cond ((= 3 (length s)) (caddr s))
        ((> (length s) 3)
         (cons '+ 
               (cons (caddr s)
                     (cdddr s))))))


(define (make-product m1 m2 . ms) 
  (cond  ((or (=number? m1 0) (=number? m2 0)) 0)
         ((pair? ms)
          (pp ms)(newline)
          (list '* m1
                (make-product m2 (car ms) (cdr ms))))
         ((=number? m1 1) m2)
         ((=number? m2 1) m1)
         ((numbers? m1 m2) (+ m1 m2))
         (else (list '* m1 m2))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier s) (cadr s))
(define (multiplicand s)
  (cond ((= 3 (length s)) (caddr s))
        ((> (length s) 3)
         (cons '*
               (cons (caddr s)
                     (cdddr s))))))


(define (make-expt b e)
  (cond ((=number? b 0) 0)
        ((=number? b 1) 1)
        ((=number? e 1) b)
        ((=number? e 0) 1)
        ((numbers? b e) (expt b e))
        (else
         (list '** b e))))
(define (expt? e) (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))