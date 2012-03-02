;; SICP chapter 2
(use srfi-13) ; strings
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

;==================================================================
(load "./test.scm")
(define t-rect1 (make-rect (make-point 0 2) (make-point 2 0)))

(test '(
        ; ex. 2.2
        (=? '(y-point (make-point 1 9)) 9)
        (=? '(x-point (make-point 1 9)) 1)
        (=? '(showpoint (make-point 5 7)) "5,7")
        (=? '(showsegment (make-segment (make-point 0 0)(make-point 2 2))) 
            "0,0:2,2")
        (=? '(showpoint (midpoint-segment (make-segment (make-point 0 0)(make-point 2 2))))
            "1.0,1.0")
        (=? '(showpoint (midpoint-segment (make-segment (make-point -2 -4)(make-point 2 2))))
            "0.0,-1.0")

        ; ex. 2.3
        (=? '(len-segment (make-segment (make-point 0 0)(make-point 2 2))) 2)
        (=? '(showsegment (horiz-rect t-rect1)) "0,2:2,2")
        (=? '(showsegment (vert-rect  t-rect1)) "0,2:0,0")
        (=? '(len-rect t-rect1) 2)
        (=? '(hei-rect t-rect1) 2)
        (=? '(hei-rect (make-rect (make-point 1 5)(make-point 5 1))) 4)
        (=?~ '(area-rect (make-rect 
                          (make-point 0 2)(make-point 5 0)))
             10.0)
        (=?~  '(perim-rect (make-rect (make-point 0 2)(make-point 5 0))) 14.0)

        ;ex 2.4
        (=? '(kar (kons 1 2)) 1)
        (=? '(kdr (kons 1 2)) 2)

        ;ex 2.5
        (=? '(which-power? 3 3) 1)
        (=? '(which-power? 9 3) 2)
        (=? '(which-power? 27 3) 3)
        (=? '(which-power? 2 2) 1)
        (=? '(which-power? 4 2) 2)
        (=? '(which-power? 32 2) 5)
        (=? '(which-power? 64 2) 6)

        (=? '(qar (qons 1 2)) 1)
        (=? '(qdr (qons 1 2)) 2)

        (=? '(qar (qons 2 3)) 2)
        (=? '(qdr (qons 2 3)) 3)

        (=? '(qar (qons 5 8)) 5)
        (=? '(qdr (qons 5 8)) 8)
        
        ; ex 2.17
        (=? '(last-pair (list 4 5 6 7)) 7)
        (=? '(last-pair (list 1)) 1)
        (=? '(last-pair '()) '())

        ;ext 2.18
        (=? '(reverse_ (list 1 2 3 4)) '(4 3 2 1))
        (=? '(reverse_ '()) '()) 
        (=? '(reverse__ (list 1 2 3 4)) '(4 3 2 1))
        (=? '(reverse__ '()) '())

        ))