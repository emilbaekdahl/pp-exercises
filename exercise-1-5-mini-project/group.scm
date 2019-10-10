; Emil Bækdahl <ebakda16@student.aau.dk> (20165378)

(load "student.scm")
(load "helpers.scm")

; Group constructor. Returns an association list representation of a group.
;
; Parameters:
;   id (any): The id to assign to the group.
;   students (list): The students that the group consists of.
;
; Examples:
;   >>> (group 42 (take 5 students))
;   >>> '((type . group) (id . 42) (students . (((type . student) ...) ...))'
(define (group id students)
  (list (cons 'type 'group)
        (cons 'id id)
        (cons 'students students)))


; Predicate to determine if a subject is a group.
(define group?
  (make-alist-type-predicate 'group))


; Returns the id of a group.
(define group-id
  (make-alist-selector 'id))


; Returns the students of a group.
(define group-students
  (make-alist-selector 'students))


; Returns the ids of the students in a group.
(define (group-student-ids group)
  (map (lambda (student)
         (student-id student))
       (group-students group)))


; Finds a group with a given id in a list of groups. Returns #f if a group
; with the given id could not be found.
;
; Parameters:
;   groups (list): The list of groups to search.
;   id (number): The id of the group at interest.
(define (group-find groups id)
  (cond ((null? groups) #f)
        (else (let ((current-group (car groups)))
                (cond ((= (group-id current-group) id) current-group)
                      (else (group-find (cdr groups) id)))))))


; Pretty printer for a group
(define (print-group group)
  (begin (display "Group #")
         (display (group-id group))
         (display " (")
         (display (length (group-students group)))
         (display " members)")
         (for-each (lambda (s)
                     (begin (newline)
                            (print-student s)))
                   (group-students group))))
