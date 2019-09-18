; Miscilanious helper function for the project.

(define (make-alist-type-predicate name)
  (lambda (lst) (and (list? lst)
                     (eq? (cdr (assq 'type lst))
                          name))))

; Returns a function that selects the value corresponding to the `key` in an
; association list `alist`.
;
; Examples:
;   >>> (define id-selector (make-alist-selector 'id))
;   >>> (id-selector '((id . 1234)))
;   >>> 1234
(define (make-alist-selector key)
  (lambda (alist)
    (cdr (assq key alist))))

; Determines wether every element in `lst` fulfils `predicate`.
;
; Examples:
;   >>> (all? '(1 2 3 4) number?)
;   >>> #t
;
;   >>> (all? '(-2 -1 0 1) positive?)
;   >>> #f
(define (all? lst predicate)
  (if (null? lst)
      #t
      (and (predicate (car lst))
           (all? (cdr lst)
                 predicate))))

; Returns `lst` without `element`.
;
; Examples:
;   >>> (remove '(a b c d) c)
;   >>> '(a b d)
(define (remove lst element)
  (cond ((null? lst) '())
        ((equal? (car lst) element) (cdr lst))
        (else (cons (car lst) (remove (cdr lst) element)))))

; Returns `n` random elements from `lst`.
; TODO: Implement actual random functionality.
;
; Examples:
;   >>> (pick-n-random '(a b c d) 2)
;   >>> '(b a)
(define (pick-n-random lst n)
  (if (= n 0)
      '()
      (let ((element (car lst)))
        (cons element
              (pick-n-random (remove lst element)
                             (- n 1))))))

; Returns a random element from `lst`.
;
; Examples:
;   >>> (pick-random '(a b c d))
;   >>> c
(define (pick-random lst)
  (car (pick-n-random lst 1)))

; Returns the first `n` elements in `lst`
;
; Examples:
;   >>> (take 2 '(a b c d))
;   >>> '(a b)
(define (take n lst)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst)
            (take (- n 1)
                  (cdr lst)))))
