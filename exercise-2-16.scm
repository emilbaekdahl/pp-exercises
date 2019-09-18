; Exercise 2.16: Quantifier Functions.

(define (for-all lst p)
  (if (null? lst)
      #t
      (and (p (car lst)) (for-all (cdr lst) p))))

(define (there-exists lst p)
  (if (null? lst)
      #f
      (if (p (car lst))
          #t
          (there-exists (cdr lst) p))))

(define (there-exists-1 lst p)
  (there-exists-1-helper lst p #f))

(define (there-exists-1-helper lst p acc)
  (if (null? lst)
      acc
      (if (and acc (p (car lst)))
          #f
          (there-exists-1-helper (cdr lst) p (p (car lst))))))

(there-exists-1 '(a 1 a) number?)
