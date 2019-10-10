; Emil Bækdahl <ebakda16@student.aau.dk> (20165378)

; The first section of this file defines a number of functions for helping with
; the construction and inspection of groupings. The second section defines
; functions for the actual grouping tasks to be carried out in the project.
; These are random-grouping, grouping-by-counting, balanced-grouping-by-counting
; and random-grouping-predicate.

(load "group.scm")
(load "helpers.scm")
(load "random.scm")

; Grouping constructor. A grouping represents the relationship between multiple
; students and groups as a list of pairs on the form (group-id . student-id).
; This function returns a grouping based on a list of groups.
;
; Examples:
;   >>> (grouping groups)
;   >>> '((1 . "123") (1 . "234") (2 . "345") (2 . "345") ...)
(define (grouping groups)
  (reduce append
          (map (lambda (group)
                 (map (lambda (student-id)
                        (cons (group-id group) student-id))
                      (group-student-ids group)))
               groups)
          '()))


; Determines if a subject is a grouping.
(define (grouping? g)
  (cond ((null? g) #t)
        (else (and (pair? (car g))
                   (grouping? (cdr g))))))


; Returns all group ids in a grouping.
(define (grouping-group-ids grouping)
  (unique (grouping-group-ids-helper grouping)))


; Helper function for grouping-group-ids
(define (grouping-group-ids-helper grouping)
  (cond ((null? grouping) '())
        (else (cons (caar grouping)
                    (grouping-group-ids-helper (cdr grouping))))))


; Get the ids of the students in a group in a grouping with a given id.
;
; Parameters:
;   grouping (list): The grouping at interest.
;   group-id (number): The id of the group we want the student ids from.
(define (grouping-group-student-ids grouping group-id)
  (let ((pairs (filter grouping
                       (lambda (pair)
                         (= (car pair)
                            group-id)))))
    (map (lambda (pair)
           (cdr pair))
         pairs)))


; Get a list of groups in a grouping based on a list of students. This extracts
; the group ids and their corresponding student ids from a grouping and creates
; groups with the group function.
;
; Parameters:
;   grouping (list): The grouping at interest.
;   students (list): A list of students which will be used to map the student
;     ids in the grouping to an actual student association list.
(define (grouping-groups grouping students)
  (let ((group-ids (grouping-group-ids grouping)))
    (map (lambda (group-id)
           (let* ((student-ids (grouping-group-student-ids grouping
                                                          group-id))
                  (students (map (lambda (student-id)
                                   (student-find students student-id))
                                 student-ids)))
             (group group-id students)))
           group-ids)))


; Get a single group by id from a grouping. Works by calling grouping-groups
; followed by a group-find with the passed id.
;
; Parameters:
;   grouping (list): The grouping to find the group in.
;   group-id (number): The id og the group to find.
;   students (list): A list of students. This is needed since a grouping only
;     contains the ids of the students and not all their information.
(define (grouping-group grouping group-id students)
  (let ((groups (grouping-groups grouping students)))
    (group-find groups group-id)))


; Return the number of groups in a grouping
(define (grouping-number-of-groups grouping)
  (length (grouping-group-ids grouping)))


; Returns a list of group sizes in a grouping
(define (grouping-group-sizes grouping)
  (let ((group-ids (grouping-group-ids grouping)))
    (map (lambda (id)
           (length (grouping-group-student-ids grouping id)))
         group-ids)))


; Returns the size of the largest group in a grouping.
(define (grouping-max-group-size grouping)
  (apply max (grouping-group-sizes grouping)))


; Returns the size of the smallest group in a grouping.
(define (grouping-min-group-size grouping)
  (apply min (grouping-group-sizes grouping)))


; Pretty printer for a grouping
(define (print-grouping grouping students)
  (let ((groups (grouping-groups grouping students)))
    (for-each (lambda (group)
                (begin (print-group group)
                       (newline)
                       (newline)))
              groups)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grouping formation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Grouping by counting. Generates a grouping of students into k groups. This
; works by creating a list of same length as students that contains a repeating
; pattern of the numbers 1 to k in ascending order. This list is then zipped
; with a list of student ids, using pair-zip, and the result is returned.
;
; Parameters:
;   students (list): A list of students created with the student function.
;   k (number): The number of groups to create.
;
; Examples:
;   >>> (grouping-by-counting students 3)
;   >>> '((1 . "0001") (2 . "0002") (3 . "0003") (1 . "0004"))
(define (grouping-by-counting students k)
  (let ((numbering (repeat (range 1 k) (length students)))
        (student-ids (student-id-mapper students)))
    (pair-zip numbering student-ids)))


; Balanced grouping by counting. This function works in the same way as
; grouping-by-counting but the list of students is first sorted by one or more
; keys. In this way we ensure that students with different values for the
; passed keys are distributed between the groups.
;
; Parameters:
;   students (list): A list of students.
;   k (number): The number of groups to form.
;   key (symbol): The key to balance the students in the groups by. This has
;     to correspond to a key in a student association list.
(define (balanced-grouping-by-counting students k . keys)
  (let ((sorted (reduce (lambda (key lst)
                          (sort-by lst key))
                        keys
                        students)))
    (grouping-by-counting sorted k)))


; This is a helper function for the last two grouping functions,
; random-grouping and random-grouping-predicate, that generate a list of groups
; from a list of students based on a list of group sizes. This works by, in
; each iteration, shuffling the list of students and creating a group from the
; first n students in that list (n being the current group size). The function
; returns #f if the list of groups sizes does not match the number of students.
;
; Parameters:
;   students (list): A list of students.
;   group-sizes (list): A list of group sizes to be created.
(define (random-groups students group-sizes)
  (if (= (sum group-sizes)
         (length students))
      (cond ((null? students) '())
            (else (let* ((shuffled (shuffle students))
                         (current-size (car group-sizes))
                         (remaining-sizes (cdr group-sizes))
                         (selected-students (take current-size students))
                         (remaining-students (drop current-size students)))
                    (cons (group (length group-sizes)
                                 selected-students)
                          (random-groups remaining-students
                                         remaining-sizes)))))
      #f))


; Generate a random grouping of students based on a list of group sizes. This
; works by generating a random list of groups using the random-groups function
; and converting the result to a grouping.
;
; Parameters:
;   students (list): A list of students.
;   group-sizes (list): A list of group sizes to form.
(define (random-grouping students group-sizes)
  (grouping (random-groups students group-sizes)))


; Generates a random grouping where each group should fulfil a predicate. If a
; result does not fulfil the predicate, the function is retried - up to 100
; times.
;
; Parameters:
;   students (list): A list of students.
;   group-sizes (list): A list of group sizes to form.
;   predicate (procedure): A boolean predicate function that takes a group as
;     parameter. We try to get all groups to fulfill this predicate.
(define (random-grouping-predicate students group-sizes predicate)
  (grouping (random-grouping-predicate-helper students group-sizes predicate 1000)))


; Helper function for random-grouping-predicate which uses a limimt parameter
; to determine how many retries we allow at maximum. If this limit is hit, we
; return the latest grouping.
(define (random-grouping-predicate-helper students group-sizes predicate limit)
  (let ((groups (random-groups students group-sizes)))
    (cond ((or (= limit 0)
               (all? groups predicate)) groups)
          (else (random-grouping-predicate-helper (shuffle students)
                                                  group-sizes
                                                  predicate
                                                  (- limit 1))))))
