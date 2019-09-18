; Exercise 1.5: Every second element of a list

; Returns a list of every `n`th element in `lst`.
;
; Examples:
;   >>> (every-nth-element 3 '(1 2 3 4 5 6))
;   >>> '(1 4)
(define (every-nth-element n lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (if (< (length (cdr lst)) n)
                '()
                (every-nth-element n (list-tail lst n))))))

; Returns every second element in `lst`.
;
; Examples:
;   >>> (every-nth-element '(1 2 3 4))
;   >>> '(1 3)
(define (every-second-element lst)
  (every-nth-element 2 lst))
