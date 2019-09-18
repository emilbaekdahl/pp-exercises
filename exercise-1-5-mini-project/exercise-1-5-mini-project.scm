; Exercise 1.15: AAU group formation

(load "data.scm")
(load "group.scm")
(load "student.scm")
(load "helpers.scm")

; Map raw students to the assoication list construct described in student.scm.
(define students
  (map (lambda (s)
         (apply student s))
       students))
