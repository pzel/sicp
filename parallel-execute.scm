;; works on chez
;; Exports: parallel-execute, random-wait, ramda, rset!

(define (random-wait)
  (sleep (make-time 'time-duration (random (/ 100000000 100)) 0)))

;; A lambda with a random execution delay
(define-syntax rambda
  (syntax-rules ()
    ((rambda () . body)
     (lambda () (random-wait) . body))))

;; A set! with delayed assignment
(define-syntax rset!
  (syntax-rules ()
    ((rset! a b)
     (let ((tmp #f))
       (random-wait) (set! tmp b)
       (random-wait) (set! a tmp)))))

;; Parallel execution of threads
(define (parallel-execute . thunks)
  (define thread-count (length thunks))
  (define finished-count 0)
  (define parallel-execute-mutex (make-mutex 'parallel-execute-mutex))

  (define (ensure-end)
    (let ((finished (with-mutex parallel-execute-mutex finished-count)))
      (if (eq? thread-count finished)
          #t
          (begin
            ;;(display (list 'waiting thread-count finished))
            ;; (newline)
            (ensure-end)))))

  (for-each (lambda(thunk)
         (fork-thread
          (lambda() (begin
                      (thunk) ; run the thunk, then bump counter
                      (with-mutex parallel-execute-mutex
                        ;;(display "finished") (newline)
                        (set! finished-count (+ 1 finished-count)))))))

         thunks)
  (ensure-end))
