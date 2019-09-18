; Exercise 2.23: Powerset.

(define (powerset lst)
  (if (null? lst)
      '(())
      (let ((rest (powerset (cdr lst))))
        (append (map (lambda (el) (cons (car lst) el)) rest) rest))))

(powerset '(a b c))
