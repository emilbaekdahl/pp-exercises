; Exercise 1.11: A counterpart to list-tail.

(define (my-list-tail lst n)
  (if (= n 0)
      lst
      (my-list-tail (cdr lst) (- n 1))))

(define (list-prefix lst n)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst)
            (list-prefix (cdr lst) (- n 1)))))
