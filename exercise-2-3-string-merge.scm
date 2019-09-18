; Exercise 2.3: More about string-merge.

; Tail-recursivly zips two lists of strings `lst1` and `lst2`.
;
; Examples:
;   >>> (string-merge (list "One " "Three" ) (list "Two " "Four"))
;   >>> "One Two Three Four"
(define (string-merge lst1 lst2)
  (string-merge-helper lst1 lst2 ""))

(define (string-merge-helper lst1 lst2 res)
  (cond ((and (null? lst1) (null? lst2)) res)
        ((null? lst1) (string-merge-helper '()
                                           (cdr lst2)
                                           (string-append res (car lst2))))
        ((null? lst2) (string-merge-helper (cdr lst1)
                                           '()
                                           (string-append res (car lst1))))
        (else (string-merge-helper (cdr lst1)
                                   (cdr lst2)
                                   (string-append res
                                                  (string-append (car lst1)
                                                                 (car lst2)))))))
