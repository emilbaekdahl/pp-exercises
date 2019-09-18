; Exercise 2.20: Generalized compose.

; Applies function `f` to itself `n` number of times.
(define (self-compose* f n)
  (if (= n 1)
      f
      (lambda (x) (f ((self-compose* f (- n 1)) x)))))

; Composes functions in `funcs`.
(define (compose* funcs)
  (if (null? funcs)
      (lambda (x) x)
      (lambda (x) ((car funcs) ((compose* (cdr funcs)) x)))))
