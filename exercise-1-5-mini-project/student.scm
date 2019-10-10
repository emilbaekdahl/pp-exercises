; Emil Bækdahl <ebakda16@student.aau.dk> (20165378)

(load "helpers.scm")

; Student constructor.
; Returns an association list representation of a student.
;
; Parameters:
;   id (string):
;   name (string):
;   sex (string):
;   ethnicity (string):
;   age (number):
;
; Examples:
;   >>> (student "12345678" "Emil Bækdahl" "male" "Danish" 22)
;   >>> '((type . student) (id . "12345678") (name . "Emil Bækdahl") ('sex . "male") (age . 22))
(define (student id name sex ethnicity age)
  (list (cons 'type 'student)
        (cons 'id id)
        (cons 'name name)
        (cons 'sex sex)
        (cons 'ethnicity ethnicity)
        (cons 'age age)))


; Predicate for determining wether a subject is a student.
(define student?
  (make-alist-type-predicate 'student))


; Maps a list of students to their id.
;
; Examples:
;   >>> (student-id-mapper students)
;   >>> '("123" "234" "345" ...)
(define student-id-mapper
  (make-alist-mapper 'id))


; Selector functions for the id, name, sex, ethnicity and age for a student.
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


; Finds a student with a given id in a list of students.
;
; Parameters:
;   students (list): A list of students
;   student-id (number): The id to look for.
(define (student-find students student-id)
  (car (filter students
               (make-alist-predicate 'id
                                     student-id))))


; Pretty printer for a student.
(define (print-student s)
  (begin (display (student-name s))
         (display " (")
         (display (student-id s))
         (display "): ")
         (display (student-sex s))
         (display ", ")
         (display (student-ethnicity s))
         (display " of age ")
         (display (student-age s))))
