; Exercise 1.3: A proper list predicate.

; Determines if `lst` is a proper list.
;
; Examples:
;   >>> (proper-list? '(1 2 3 4))
;   >>> #t
;
;   >>> (proper-list? '((a b c) . d))
;   >>> #f
(define (proper-list? lst)
  (or (null? lst)
      (and (pair? lst)
           (proper-list? (cdr lst)))))
