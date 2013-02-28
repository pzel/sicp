(define show
  (lambda args 
    (map (lambda(a) (begin (display a) (display "\t"))) args)
    (newline)))

; ex. 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define 316-proper-list
  (cons 'a (cons 'b (cons 'c '()))))

(define 316-pathological-1
  (let ((l (list 'a 'b 'c)))
    (set-car! l (cddr l))
    l))

(define 316-pathological-2
  (let ((l (list 'a 'b 'c)))
    (set-car! l (cdr l))
    (set-car! (cdr l) (cddr l))
    l))

; This will hang the procedure
(define 316-pathological-3
  (let ((l (list 'a 'b 'c)))
    (set-cdr! (cddr l) (cdr l))
    l))

;3.18
(define (is-cyclical l)
  (define (check l visited)
    (cond ((null? l) #f)
          ((any (lambda(x) (eq? (cdr l) x)) visited) #t)
          (else (check (cdr l) (cons (cdr l) visited)))))
  (check l '()))

(define (any pred l)
  (member #t (map pred l)))

(define (take l n)
  (define (iter l acc n)
    (cond ((null? l) acc)
          ((= n 0) acc)
          (else
           (iter (cdr l) (append acc (list (car l))) (- n 1)))))
  (iter l '() n))

;3.19
(define (is-cyclical-f l)
  (let ((fast-p (list '()))
        (slow-p (list '())))
    (define (set-fast-p! p l)
      (if (and (pair? l) (pair? (cdr l)))
          (begin (set-cdr! p (cddr l)) #t)
          #f))
    (define (loop sl fl)
      (if (null? sl)
          #f
          (if (set-fast-p! fast-p fl)
              (begin
                (set-cdr! slow-p sl)
;                (show (cadr fast-p) (cadr slow-p))
                (if (eq? (cdr slow-p) (cdr fast-p))
                    #t
                    (loop (cdr sl) (cddr fl))))
              #f)))
    (loop l l)))

(define (make-cyclical! l)
  (let ((head-p l))
    (define (loop l)
      (cond ((null? (cdr l))
             (set-cdr! l (cdr head-p)))
            (else
             (loop (cdr l)))))
    (loop l)))
; Benchmarking & tests  for ex. 3.18 & 3.19
(use srfi-1)
(define l1 (iota 1000))
(make-cyclical! l1)
; Check it out for various sizes of l1
; $ csi ./c33.scm -e "(time (is-cyclical-f l1))" 
; $ csi ./c33.scm -e "(time (is-cyclical l1))" 

; queues 
(define (front-p q) (car q))
(define (rear-p q) (cdr q))
(define (set-front-p! q el) (set-car! q el))
(define (set-rear-p! q el) (set-cdr! q el))

(define (empty-queue? q) (null? (front-p q)))
(define (make-queue) (cons '() '()))
(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called for empty queue")
      (car (front-p q))))

(define (insert-queue! q el)
  (let ((new-pair (cons el '())))
    (cond ((empty-queue? q) 
           (begin
             (set-front-p! q new-pair)
             (set-rear-p! q new-pair)
             q))
          (else 
           (begin
             (set-cdr! (rear-p q) new-pair)
             (set-rear-p! q new-pair)
             q)))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "DELETE called for empty queue"))
        (else 
         (begin
           (set-front-p! q (cdr (front-p q)))
           q))))
         
(define (show-queue q)
  (with-output-to-string 
   (lambda ()
     (display "Queue ")
     (cond ((empty-queue? q) (display "[]"))
           (else
            (display (front-p q)))))))

; tables
(define (assoc-with comp key table)
  (cond ((null? table) #f)
        ((atom? table) #f) ;; For variably-nested tables
        ((comp key (caar table)) (car table))
        (else (assoc-with comp key (cdr table)))))

(define (assoc key table)
  (assoc-with equal? key table))

(define (lookup key table)
  (v-lookup eq? table (list key)))

(define (insert! key value table)
  (v-insert! eq? table (list key) value))

(define (make-table) (list 'Table))

(define (lookup2 k1 k2 table)
  (v-lookup eq? table (list k1 k2)))

(define (insert2! k1 k2 value table)
  (v-insert! eq? table (list k1 k2) value))

(define (make-table-obj)
  (make-table-comp equal?))

(define (make-table-comp comp)
  (let ((local-table (list 'Table)))
    (define (lkp k1 k2)
      (v-lookup comp local-table (list k1 k2)))
    (define (ins! k1 k2 value)
      (v-insert! comp local-table (list k1 k2) value))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lkp)
            ((eq? m 'insert!) ins!)
            (else (error "Unknown option -- TABLE"))))
    dispatch))

(define (make-v-table-obj)
  (let ((local-table (list 'Table)))
    (define (lkp keys)
      (v-lookup eq? local-table keys))
    (define (ins! keys value)
      (v-insert! eq? local-table keys value))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lkp)
            ((eq? m 'insert!) ins!)
            (else (error "Unknown option -- V-TABLE"))))
    dispatch))

(define (v-insert! comp t keys val)
  (cond ((null? keys) #f)
        ((null? (cdr keys))
         (letrec ((key (car keys))
                  (rec (assoc-with comp key (cdr t))))
           (if rec
               (set-cdr! rec val)
               (set-cdr! t (cons (cons key val) (cdr t))))
           #t))
        (else
         (if (null? (cdr t))
             (set-cdr! t (cons (cons (car keys) '()) (cdr t))))
         (v-insert! comp (assoc-with comp (car keys) (cdr t)) (cdr keys) val))))

(define (v-lookup comp t keys)
  (if (null? keys) 
      #f
      (let ((rec (assoc-with comp (car keys) (cdr t))))
        (cond ((eq? rec #f)      #f)
              ((atom? (cdr rec)) (cdr rec))
              (else
               (v-lookup comp rec (cdr keys)))))))


; circuits
(define (call-each procs)
  (begin
    (map (lambda(proc) (proc)) procs)
    #t))


(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures)))
      #t)
    (define (accept-action-procedure proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure)
            (else (error (list "wire: unknown message" m)))))
    dispatch))
          
    
