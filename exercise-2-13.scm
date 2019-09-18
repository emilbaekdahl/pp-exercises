; Exercise 2.13: Iterative mapping function.

(define (mymap f lst)
  )

(define (mymap-helper f lst acc)
  (if (null? lst) '() (mymap-helper f (cdr lst) (cons acc (f (car lst))))
