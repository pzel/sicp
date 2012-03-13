;; SICP chapter 2
(use srfi-13) ; string methods (for displaying objects)

(define (not-pair? x)
  (not (pair? x)))

(define (avg x y)
  (/ (+ x y)
     2.0))
(define (abs x)
  (if (<  x 0)
      (* -1 x)
      x))
(define (pow x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        (#t
         (* x
            (pow x (- n 1))))))

; 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (showpoint p)
  (let ((x (number->string (x-point p)))
        (y (number->string (y-point p))))
    (string-join (list x y) ",")))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let ((new-x (avg (x-point (start-segment s))
                    (x-point (end-segment s))))
        (new-y (avg (y-point (start-segment s))
                    (y-point (end-segment s)))))
    (make-point new-x new-y)))

(define (len-segment s)
  (abs (- (x-point (start-segment s))
          (x-point (end-segment s)))))
(define (hei-segment s)
  (abs (- (y-point (start-segment s))
          (y-point (end-segment s)))))

(define (showsegment s)
  (let ((start (showpoint (start-segment s)))
        (end   (showpoint (end-segment s))))
    (string-join (list start end) ":")))

;2.3
;; (define (make-rect p1 p2 p3 p4)
;;   (cons (make-segment p1 p2)
;;         (make-segment p3 p4)))

;; (define (horiz-rect r)
;;   (car r))
;; (define (vert-rect r)
;;   (cdr r))

(define (make-rect p1 p2)
  (cons p1 p2))

(define t-rect1 (make-rect (make-point 0 2) (make-point 2 0)))

(define (horiz-rect r)
  (let ((p1x (x-point (car r)))
        (p1y (y-point (car r)))
        (p2x (x-point (cdr r)))
        (p2y (y-point (cdr r))))
    (make-segment (make-point p1x p1y)
                  (make-point p2x p1y))))

(define (vert-rect r)
  (let ((p1x (x-point (car r)))
        (p1y (y-point (car r)))
        (p2x (x-point (cdr r)))
        (p2y (y-point (cdr r))))
    (make-segment (make-point p1x p1y)
                  (make-point p1x p2y))))

(define (len-rect r)
  (len-segment (horiz-rect r)))

(define (hei-rect r)
  (hei-segment (vert-rect r)))
                   
(define (perim-rect r)
  (* 2
     (+ (len-rect r) (hei-rect r))))

(define (area-rect r)
  (* (len-rect r) (hei-rect r)))

; 2.4
(define (kons x y)
  (lambda(m) (m x y)))
(define (kar m)
  (m (lambda(x y) x)))
(define (kdr m)
  (m (lambda(x y) y)))

; 2.5
(define (divisible? n m)
  (if (or (= n 0))
      #f
      (= 0
         (remainder n m))))

(define (which-power? n m)
  (define (iter n m acc)
    (if (divisible? n m)
        (iter (/ n m) m (+ 1 acc))
        acc))
  (iter n m 0))
  
(define (qons x y)
  (* (pow 2 x)
     (pow 3 y)))

(define (qdr m)
  (which-power? m 3))

(define (qar m)
  (let ((x (/ m (pow 3 (qdr m)))))
    (which-power? x 2)))

; 2.17
(define (last-pair l)
  (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        (#t (last-pair (cdr l)))))

; 2.18
(define (reverse_ l)
  (define (iter l1 acc)
    (if (null? l1) 
        acc
        (iter (cdr l1) 
              (cons (car l1) acc))))
  (iter l '()))

(define (reverse__ l)
  (if (null? l)
      l
      (append (reverse__ (cdr l))
              (list (car l)))))

; 2.19
(define us-coins (list 50 25 10 5 1))
(define us-rev-coins  (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coins)
  (let ((no-more? null?)
        (except-first-demonination cdr)
        (first-denomination car))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coins)) 0)
          (#t
           (+ (cc amount 
                  (except-first-demonination coins))
              (cc (- amount (first-denomination coins)) 
                  coins))))))
  

; 2.20
(define (filter_ pred list)
  (define (iter list acc)
    (if (null? list)
        (reverse_ acc)
        (if (pred (car list))
            (iter (cdr list) 
                  (cons (car list) acc))
            (iter (cdr list) 
                  acc))))
  (iter list '()))

(define (same-parity x . rest)
  (if (even? x)
      (cons x (filter_ even? rest))
      (cons x (filter_ odd? rest))))

; 2.21
(define (square-list_ l)
  (map (lambda(x) (* x x))
       l))
(define (square-list__ l)
  (if (null? l)
      '()
      (cons (* (car l) (car l))
            (square-list__ (cdr l)))))

; 2.22
(define (square-list3-broken l)
  (let ((square (lambda(x) (* x x))))
    (define (iter l answer)
      (if (null? l)
          answer
          (iter (cdr l)
                (cons (square (car l))
                      answer))))
    (iter l '())))

(define (square-list3-fixed l)
  (let ((square (lambda(x) (* x x))))
    (define (iter l answer)
      (if (null? l)
          (reverse_  answer)
          (iter (cdr l)
                (cons (square (car l))
                      answer))))
    (iter l '())))

(define (square-list3-fixed_ l)
  (let ((square (lambda(x) (* x x))))
    (define (iter l acc)
      (if (null? l)
          acc
          (iter (cdr l)
                (append acc 
                        (list (square (car l)))))))

    (iter l '())))

; 2.23
(define (for-each_ pred l)
  (cond ((null? l) '())
        (#t
         (pred (car l))
         (for-each_ pred (cdr l)))))
               

; 2.25
(define (expr1 w) (car (cdr (car (cdr (cdr w))))))
(define (expr2 w) (car (car w)))
(define (expr3 w) (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr w)))))))))))))

; 2.26 
(define x26 (list 1 2 3))
(define y26 (list 4 5 6))


; 2.27
(define x27 (list (list 1 2) (list 3 4)))
(define (deep-reverse l)
  (cond ((null? l) 
         '())
        ((list? (car l))
         (append (deep-reverse (cdr l)) 
                 (list (deep-reverse (car l)))))
        (else
         (append (deep-reverse (cdr l)) 
                 (list (car l))))))

; 2.28
(define x28 (list (list 1 2) (list 3 4)))
(define (fringe l)
  (cond ((null? l)
         '())
        ((list? (car l))
         (append (fringe (car l))
                 (fringe (cdr l))))
        (else
         (append (list (car l))
                 (fringe (cdr l))))))

; 2.29
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch s)
  (car s))
(define (right-branch s)
  (cdr s))
(define (branch-length s)
  (car s))
(define (branch-structure s)
  (cadr s))

(define (is-branch? s)
  (if (null? s) 
      #f
      (not (pair? (car s)))))

(define (is-structure? s)
  (cond ((null? s) #f)
        ((not (pair? s)) #t)
        ((and (pair? s)
              (not (is-branch? s))) #t)
        (else 
         #f)))

(define (total-weight s)
  (cond ((null? s) '())
        ((not (pair? s)) s)
        ((is-branch? s)
         (total-weight (branch-structure s)))
        (else (+ (total-weight (left-branch s))
                 (total-weight (right-branch s))))))

(define (even-forces? s)
  (= (* (branch-length (left-branch s))
        (total-weight  (left-branch s)))
     (* (branch-length (right-branch s))
        (total-weight  (right-branch s)))))

(define (is-balanced? s)
  (cond ((not-pair? s) #t)
        ((is-structure? s)
         (and (even-forces? s)
              (and (is-balanced? (right-branch s))
                   (is-balanced? (left-branch s)))))
        ((is-branch? s)
         (is-balanced? (branch-structure s)))
        (else 
         #f)))

; 2.30
(define (square x) (* x x))
(define (skuare-tree t)
  (cond ((null? t) '())
        ((not-pair? t) (square t))
        (else
         (cons (skuare-tree (car t))
               (skuare-tree (cdr t))))))

(define (scuare-tree t)
  (map (lambda(x)
         (if (not-pair? x) 
             (square x)
             (scuare-tree x)))
       t))

; 2.31
(define (m-square-tree t)
  (tree-map square t))

(define (tree-map f t)
  (map (lambda(element)
         (if (not-pair? element)
             (f element)
             (tree-map f element)))
       t))


; 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda(x)(cons (car s) x))
                          rest)))))

(define (accumulate op acc seq)
  (if (null? seq)
      acc
      (op (car seq)
          (accumulate op acc (cdr seq)))))


(define (accumulate-debug op acc seq)
  (if (null? seq)
      (begin (newline)(display "Returning ")(display acc) acc)
      (begin ;debug
        (newline)
        (display (car seq))(display ": ")
        (display (cdr seq))
        (op (car seq)
            (accumulate-debug op acc (cdr seq))))))


; 2.33
(define (map32 f xs)
  (accumulate (lambda(y ys) (cons (f y) ys)) 
              '() 
              xs))

(define (append32 xs ys) 
  (accumulate cons ys xs))

(define (length32 xs)
  (accumulate (lambda(_ y) (+ 1 y))
              0 xs))

; 2.34
(define (horner-eval x coefficients)
  (accumulate (lambda(this-coeff higher-terms) 
                (+ (* x higher-terms)
                      this-coeff))
              0
              coefficients))

; 2.35
(define (count-leaves-acc t)
  (accumulate (lambda(x rest)
                (+ (length (fringe x)) 
                   rest))
              0
              (map list t)))
                    
; 2.36
(define (accumulate-n op acc seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op acc (map car seqs))
            (accumulate-n op acc (map cdr seqs)))))
             