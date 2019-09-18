(load "student.scm")
(load "helpers.scm")

; Defines functions related to groups. A group is represented as an association
; list with an id and a list of students.

; Group constructor.
; Returns an association list representation of a group.
(define (group id students)
  (list (cons 'type 'group)
        (cons 'id id)
        (cons 'students students)))

; Predicate to determine if an object is a group.
(define group?
  (make-alist-type-predicate 'group))

; Selctors for the id and students of a group.
(define group-id
  (make-alist-selector 'id))

(define group-students
  (make-alist-selector 'students))

; Returns the ids of the students in a group.
(define (group-student-ids group)
  (map (lambda (student)
         (student-id student))
       (group-students group)))
