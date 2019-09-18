(define (alist? lst)
  (if (null? lst)
      #t
      (and (pair? (car lst)) (alist (cdr lst)))))
