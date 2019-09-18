; Exercise 2.22: The cartesian product of two sets.

(define (reduce f init lst)
  (if (null? lst)
      init
      (f (car lst) (reduce f init (cdr lst)))))

(define (cartesian-product lsta lstb)
  (reduce append '() (map (lambda (a)
                            (map (lambda (b)
                                   (cons a b)) lstb)) lsta)))
