(use srfi-1)
;; Older stuff
(define (tree-map f t)
  (map (lambda(element)
         (if (not-pair? element)
             (f element)
             (tree-map f element)))
       t))
;;;;;;;;;;;;;;;;;;;;;;;


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


;; Buildup to 2.59
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) set)
        (else
         (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
          (cons (car s1)
                (intersection-set (cdr s1) s2)))
        (else
         (intersection-set (cdr s1) s2))))

;; ex. 2.59
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else
         (cons (car s1)
               (union-set (cdr s1) s2)))))

;; ex 2.60
(define (elem-of-dset? x dset)
  (cond ((null? dset) #f)
        ((equal? x (car dset)) #t)
        (else
         (elem-of-dset? x (cdr dset)))))

(define (adjoin-dset x dset)
  (cons x dset))

(define (intersection-dset s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((elem-of-dset? (car s1) s2)
          (cons (car s1)
                (intersection-dset (cdr s1) s2)))
        (else
         (intersection-dset (cdr s1) s2))))

(define (union-dset s1 s2)
  (append s1 s2))


; buildup to 2.61
(define (oset . xs)
  (sort xs <))

(define (elem-of-oset? x oset)
  (cond ((null? oset) #f)
        ((< x (car oset)) #f)
        ((= x (car oset)) #t)
        (else
         (elem-of-oset? x (cdr oset)))))

(define (intersection-oset s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((< (car s1) (car s2))
         (intersection-oset (cdr s1) s2))
        ((> (car s1) (car s2))
         (intersection-oset s1 (cdr s2)))
        ((= (car s1) (car s2))
         (cons (car s1)
               (intersection-oset (cdr s1) (cdr s2))))))

;; ex. 2.61
(define (adjoin-oset x s)
  (cond ((null? s) (list x))
        ((< (car s) x)
         (cons (car s)
               (adjoin-oset x (cdr s))))
        ((> (car s) x)
         (cons x s))
        ((= (car s) x) s)
        (else (error "adjoin-oset failed for set: " s))))


;; ex. 2.62
(define (union-oset s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((> (car s2) (car s1))
         (cons (car s1)
               (union-oset (cdr s1) s2)))
        ((< (car s2) (car s1))
         (cons (car s2)
               (union-set s1 (cdr s2))))
        ((= (car s1) (car s2))
         (cons (car s1)
               (union-oset (cdr s1) (cdr s2))))))

;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (elem-of-tset? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((> x (entry set))
         (elem-of-tset? x (right-branch set)))
        ((< x (entry set))
         (elem-of-tset? x (left-branch set)))))

(define (adjoin-tset x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tset x (right-branch set))))
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tset x (left-branch set))
                    (right-branch set)))))

;; Trees from Figure 2.16
(define (make-leaf x)
  (make-tree x '() '()))
  
(define tree216-1
  (make-tree 7
             (make-tree 3
                        (make-leaf 1)
                        (make-leaf 5))
             (make-tree 9
                        '()
                        (make-leaf 11))))
(define tree216-2
  (make-tree 3
             (make-leaf 1)
             (make-tree 7 
                         (make-leaf 5)
                         (make-tree 9
                                    '()
                                    (make-leaf 11)))))
                         
(define tree216-3
  (make-tree 5
             (make-tree 3
                        (make-leaf 1)
                        '())
             (make-tree 9
                        (make-leaf 7)
                        (make-leaf 11))))



;; Ex. 2.63

(define (tree->list1 tree)
  (if (null? tree) 
      '()
      (append (tree->list1 (left-branch tree))
              (cons (entry tree)
                    (tree->list1 (right-branch tree))))))

(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define (make-tree1 this l r)
  (make-tree this l r))

(define (list->bbtree elements)
  (car (partial-tree (sort elements <) (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ 1 left-size))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree1 this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (bset xs)
  (list->bbtree (sort (delete-duplicates xs)
                      <)))

(define (union-bset t1 t2)
  (bset (append (tree->list1 t1)
                (tree->list1 t2))))

(define (intersection-bset t1 t2)
  (bset (intersection-oset (tree->list1 t1)
                           (tree->list1 t2))))
