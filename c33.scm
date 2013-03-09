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

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda() (set-signal! output new-value)))))
  (add-action! input invert-input)
  #t)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda() (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  #t)

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda() (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  #t)

(define (or-gate-on-nand o1 o2 output)
  (let ((j (make-wire))
        (k (make-wire)))
    (nand-gate o1 o1 j)
    (nand-gate o2 o2 k)
    (nand-gate j k output)
    #t))

(define (nand-gate n1 n2 output)
  (let ((j (make-wire)))
    (and-gate n1 n2 j)
    (inverter j output)
    #t))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    #t))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    #t))

(define (ripple-carry-adder n1-readable n2-readable c0 sum-readable)
  (letrec ((n1 (reverse n1-readable))
           (n2 (reverse n2-readable))
           (sum (reverse sum-readable))
           (couts (make-n-wires (length sum)))
           (cs (cons c0 couts))
           (bundles (zip n1 n2 cs sum couts)))
    (map (lambda(b)
           (letrec ((in1  (car b))
                    (in2  (cadr b))
                    (cin  (caddr b))
                    (sum  (cadddr b))
                    (cout (car (cddddr b))))
             (full-adder in1 in2 cin sum cout)))
         bundles))
  #t)

(define (logical-not n)
  ((fmap-boolean not) n))

(define (logical-and a b)
  (define (andf x y) (and x y))
  ((fmap-boolean2 andf) a b))

(define (logical-or a b)
  (define (orf x y) (or x y))
  ((fmap-boolean2 orf) a b))

(define (fmap-boolean func)
  (lambda (x)
    (bool->num (func (num->bool x)))))

(define (fmap-boolean2 func)
  (lambda (x y)
    (bool->num (func (num->bool x)
                     (num->bool y)))))

(define (after-delay delay action)
  (let ((the-agenda (get-current-agenda)))
    (add-to-agenda! the-agenda
                    (+ delay (current-time the-agenda))
                    action)))

(define (propagate)
  (let ((the-agenda (get-current-agenda)))
    (if (empty-agenda? the-agenda)
        #t
        (let ((first-item (first-agenda-item the-agenda)))
          (first-item)
          (remove-first-agenda-item! the-agenda)
          (propagate)))))

(define inverter-delay 2)
(define and-gate-delay 3)
(define  or-gate-delay 5)

;syntax
(define (get-signal wire)         (wire 'get-signal))
(define (set-signal! wire sig)   ((wire 'set-signal!) sig))
(define (add-action! wire proc)  ((wire 'add-action!) proc))

; bools to binary numbers
(define (num->bool x)
  (cond ((= x 0) #f)
        ((= x 1) #t)
        (else (error (list "num->bool: not binary digit" x)))))

(define (bool->num x)
  (cond ((eq? x #t) 1)
        ((eq? x #f) 0)
        (else (error (list "bool->num: not boolean" x)))))

(define (make-n-wires n)
  (repeat n make-wire))

(define (make-wires signal-values)
  (letrec ((wires (map (lambda(_) (make-wire)) signal-values))
           (wsigs (zip wires signal-values)))
    (map (lambda(ws) (begin (set-signal! (car ws) (cadr ws))
                            (car ws)))
         wsigs)))

(define (get-signals wires)
  (map get-signal wires))

(define (set-signals! wires signals)
  (let ((wsigs (zip wires signals)))
    (map (lambda(ws) (set-signal! (car ws) (cadr ws)))
         wsigs)))

(define (repeat n proc)
  (define (work n res)
    (if (= n 0)
        res
        (work (- n 1) (cons (proc) res))))
  (work n '()))

; the agenda
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (head-segment agenda)  (car (segments agenda)))
(define (tail-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! agenda time action)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                       segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (head-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (tail-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "first-agenda-item: empty agenda")
      (let ((first-seg (head-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


(define (probe name wire)
  (add-action! wire
               (lambda()
                   (set-cdr! (get-current-textbuf)
                         (with-output-to-string
                           (lambda()
                             (display name)
                             (display " at: ")
                             (display (current-time (get-current-agenda)))
                             (display " New value = ")
                             (display (get-signal wire))))))))

; constraint system

(define (has-value? conn) 
  (conn 'has-value?))

(define (get-value conn)
  (conn 'value))

(define (set-value! conn newval informant)
  ((conn 'set-value!) newval informant))

(define (forget-value! conn retractor) 
  ((conn 'forget) retractor))

(define (connect conn constraints)
  ((conn 'connect) constraints))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2)) me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1)) me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me msg)
    (cond ((eq? msg 'I-have-a-value)
           (process-new-value))
          ((eq? msg 'I-lost-my-value)
           (process-forget-value))
          (else
           (error (list "adder received unknown message" msg)))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2 (/ (get-value product) (get-value m1)) me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1 (/ (get-value product) (get-value m2)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me msg)
    (cond ((eq? msg 'I-have-a-value)
           (process-new-value))
          ((eq? msg 'I-lost-my-value)
           (process-forget-value))
          (else
           (error (list "multiplier received unknown message" msg)))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value conn)
  (define (me msg)
    (error (list "constant received message" msg)))
  (connect conn me)
  (set-value! conn value me)
  me)

(define (probe-connector name conn)
  (define (print-probe value)
    (begin
      (set-cdr! (get-current-textbuf)
                (with-output-to-string
                  (lambda()
                    (display "Probe: ")
                    (display name)
                    (display " = ")
                    (display value))))))
  (define (process-new-value)
    (print-probe (get-value conn)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me msg)
    (cond ((eq? msg 'I-have-a-value)
           (process-new-value))
          ((eq? msg 'I-lost-my-value)
           (process-forget-value))
          (else
           (error (list "probe received unknown message" msg)))))
  (connect conn me)
  me)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (begin (set! value newval)
                    (set! informant setter)
                    (for-each-except setter
                                     inform-about-value
                                     constraints)))
            ((not (= value newval))
             (error (list "contradiction" value newval)))
            (else #f)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          #f))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      #t)
    (define (me msg)
      (cond ((eq? msg 'has-value?)
             (if informant #t #f))
            ((eq? msg 'value) value)
            ((eq? msg 'set-value!) set-my-value)
            ((eq? msg 'forget) forget-my-value)
            ((eq? msg 'connect) connect)
            (else (error (list "connector received unknown message" msg)))))
    me))

(define (for-each-except exception proc list)
  (define (loop items)
    (cond ((null? items) #t)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (begin (proc (car items))
                       (loop (cdr items))))))
  (loop list))

(define (celsius-farenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder      v y f)
    (constant     9 w)
    (constant     5 x)
    (constant    32 y)
    #t))

(define (averager a b avg)
  (let ((c (make-connector))
        (d (make-connector)))
    (adder      a   b c)
    (multiplier d avg c)
    (constant       2 d)
    #t))


(define (squarer34 a b)
  (multiplier a a b))

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error (list "squarer: square less than 0:" (get-value b)))
            (set-value! a (sqrt (get-value b)) me)))
    (if (has-value? a)
        (set-value! b (* (get-value a) 
                         (get-value a)) 
                    me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me msg)
    (cond ((eq? msg 'I-have-a-value)
           (process-new-value))
          ((eq? msg 'I-lost-my-value)
           (process-forget-value))
          (else
           (error (list "squarer received unknown message" msg)))))
  (connect a me)
  (connect b me)
  me)
