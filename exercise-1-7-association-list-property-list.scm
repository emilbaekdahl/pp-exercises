; Exercise 1.7: Association lists and property lists.

; Converts an association list `assoc-lst` to a property list.
;
; Examples:
;   >>> (assoc-to-prop-lst '((a 1) (b 2) (c 3)))
;   >>> '(a 1 b 2 c 3)
(define (assoc-to-prop-lst assoc-lst)
  (if (null? assoc-lst)
      '()
      (let ((element (car assoc-lst)))
        (append (list (car element) (cdr element))
              (assoc-to-prop-lst (cdr assoc-lst))))))

(define (prop-to-assoc-lst prop-lst)
  (if (null? prop-lst)
      '()
      (let ((first-two (take prop-lst 2))
            (rest (drop prop-lst 2)))
        (cons (cons (car first-two)
                    (cdr first-two))
              (prop-to-assoc-lst rest)))))

(assoc-to-prop-lst '((a 1) (b 2) (c 3)))
