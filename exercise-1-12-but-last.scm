; Exercise 1.12: The function butlast

(define (but-n-last lst n)
  (if (= n (length lst))
      '()
      (cons (car lst) (but-n-last (cdr lst) n))))

(define (but-last lst)
  (but-n-last lst 1))

(but-n-last '(1 2 3 4) 1)
