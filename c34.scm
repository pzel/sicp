(use srfi-18)

; ex. 3.38
; a) Possible results: {35,40,45,50}
; b) 80, 55, ...

; ex. 3.39
; {100,101,121}


; ex. 3.40
; all possibilities: {100, 1000, 10000, 100000, 1000000}
; serialized:                                  {1000000}

; ex. 3.41
; In the current implementation, the destructive operations are serialized and therefore safe, so the balances can be read at anytime. They will reflect the 'current' balance, which may happen to change right after it is read.

; ex. 3.42
; Yes, the two implementations are the same thing, because they lift the functions to the same serializer instance.



(define (parallel-execute . procs)
  (map thread-join!
       (map thread-start! procs)))

  
