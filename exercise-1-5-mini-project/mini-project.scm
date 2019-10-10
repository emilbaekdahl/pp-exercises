; Emil Bækdahl <ebakda16@student.aau.dk> (20165378)

; The project has been solved using Scheme R5RS. This Scheme standard has no
; built-in functionality for generating random numbers. Thus, I have
; implemented this my self but in a pure functional way. Since this means no
; side effects, a call to the random function with a given seed will always
; give the same result.

; Throughout the different files in the project there will be more or less
; documentation of definitions in the form of comments. If a definition is
; non-trival, examples and information about parameters are included.

; In this file, I present solutions to the four tasks in the mini project. The
; acutal implementation of the grouping functionality is present in grouping.scm.

(load "helpers.scm")
(load "student.scm")
(load "group.scm")
(load "grouping.scm")
(load "data.scm")

; Map raw students from the data.scm file to the assoication list construct
; described in student.scm.
(define students
  (map (lambda (s)
         (apply student s))
       data))


; TASK 1: Random grouping
; First, we create the list of group sizes. In this case, 40 groups of 5.
(define task-1-group-sizes
  (repeat '(5) 40))

; Now we call the random-grouping function.
(define task-1-grouping
  (random-grouping students task-1-group-sizes))


; TASK 2: Grouping by counting.

; In this case, we want 31 groups.
(define task-2-grouping
  (grouping-by-counting students 31))


; TASK 3: Balanced grouping by counting.
; Here, we want to balance ethnicity
(define task-3-grouping
  (balanced-grouping-by-counting students 31 'sex 'ethnicity))


; TASK 4: Random grouping with group predicate.
; First, we create the appropiate predicates.
(define (make-n-of-age-predicate n age)
  (lambda (group)
    (>= (length (filter (group-students group)
                        (lambda (student)
                          (>= (student-age student) age))))
        n)))

(define (all-female-predicate group)
  (all? (group-students group)
        (make-alist-predicate 'sex "female")))

(define (no-same-age-predicate group)
  (let ((ages ((make-alist-mapper 'age) (group-students group))))
    (= (length ages)
       (length (unique ages)))))

; Now we can call the grouping function with one of these predicates.
(define task-4-grouping
  (random-grouping-predicate students task-1-group-sizes all-female-predicate))


; The result of all of the above tasks can be printed with print-grouping
; (print-grouping task-1-grouping students)
; (print-grouping task-2-grouping students)
; (print-grouping task-3-grouping students)
; (print-grouping task-4-grouping students)
