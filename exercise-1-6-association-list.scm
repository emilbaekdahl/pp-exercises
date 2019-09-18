; Exercise 1.6: Creation of association lists.

; Creates an association list based on `keys` and `values`.
;
; Examples:
;   >>> (pair-up '(a b c) '(1 2 3))
;   >>> '((a . 1) (b . 2) (c . 3))
(define (pair-up keys vals)
  (if (or (null? keys) (null? vals))
      '()
      (cons (cons (car keys) (car vals))
            (pair-up (cdr keys) (cdr vals)))))
