(load "helpers.scm")

; Student constructor.
; Returns an association list representation of a student.
;
; Examples:
;   >>> (student "12345678" "Emil Bækdahl" "male" "Danish" 22)
;   >>> '((type . student) (id . "12345678") (name . "Emil Bækdahl") 'sex . "male") (age . 22))
(define (student id name sex ethnicity age)
  (list (cons 'type 'student)
        (cons 'id id)
        (cons 'name name)
        (cons 'sex sex)
        (cons 'ethnicity ethnicity)
        (cons 'age age)))

; Predicate for determining wether a list is a student.
(define student?
  (make-alist-type-predicate 'student))

; Selector function for the id, name, sex, ethnicity and age for a student.
(define student-id
  (make-alist-selector 'id))

(define student-name
  (make-alist-selector 'name))

(define student-sex
  (make-alist-selector 'sex))

(define student-ethnicity
  (make-alist-selector 'ethnicity))

(define student-age
  (make-alist-selector 'age))
