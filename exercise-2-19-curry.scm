; Exercise 2.19: Generalization of curry-2 and curry-3.

(define (curry f n)
  (if (= n 1)
      f
      (lambda (x) ((f (curry f (- n 1) x))))))

