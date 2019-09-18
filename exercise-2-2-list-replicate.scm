; Exercise 2.2: A list replication function.

; Create a new, eventually cylcing, list of length `n` from `lst` .
;
; Examples
;   >>> (replicate-to-length '(1 2 3) 4)
;   >>> '(1 2 3 1)
(define (replicate-to-length lst n)
  (replicate-to-length-helper lst n 0))

(define (replicate-to-length-helper lst n current)
  (if (= n 0)
      '()
      (cons (list-ref lst current)
            (replicate-to-length-helper lst
                                        (- n 1)
                                        (modulo (+ current 1)
                                                (length lst))))))
