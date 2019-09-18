(load "group.scm")
(load "helpers.scm")

; Defines functions related to groupings. A grouping is a list of
; (group-id . student-id) pairs describing a one-to-many relationship
; between a group and its students.

; Grouping constructor.
(define (grouping groups)
  (map (lambda (g)
         (group-student-ids group))
       groups))

; Random grouping
(define (random-grouping sl gsl)
  (let ((gsl-valid? (all? gsl (lambda (element)
                                (and (number? element)
                                     (positive? element)))))
        (sl-valid? (= (apply + gsl)
                      (length sl))))
    sl))
