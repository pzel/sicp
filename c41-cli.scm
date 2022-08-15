(import (scheme small))
(include "./c41.scm")

(define (prompt-for-input) (display "ε: "))
(define (announce-output) (display "> "))

(define (lambda->list l)
  (list 'lambda
	(procedure-parameters l)
	(car (procedure-body l))
	'<env>'))

(define (user-print obj)
  (if (compound-procedure? obj)
      (display (lambda->list obj))
      (display obj))
  (newline))

(define (start-repl)
  (display "SICP scheme. Type \"q\" or ^D to exit cleanly.")
  (newline)
  (repl))

(define (quit-repl)
  (display "Exiting")
  (newline)
  (exit))

(define (repl)
  (prompt-for-input)
  (let ((input (read)))
    (if (or (eq? input 'q) (eq? input #!eof)) (quit-repl))
    (let ((output (%eval input %base-env)))
      (announce-output)
      (user-print output))
    (repl)))

(start-repl)
