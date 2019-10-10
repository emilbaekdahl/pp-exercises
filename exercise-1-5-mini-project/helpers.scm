; Emil Bækdahl <ebakda16@student.aau.dk> (20165378)

; This file defines a number of helper functions that are used throughout the
; other files in the project.

(load "random.scm")

; Determines is a subject is an association list.
;
; Parameters:
;   lst (any): The input to check.
(define (alist? lst)
  (and (list? lst)
       (or (null? lst)
           (and (pair? (car lst))
                (alist? (cdr lst))))))


; Returns a function that selects the value corresponding to the `key` in an
; association list.
;
; Parameters:
;   key (symbol): The key to select for each alist.
;
; Examples:
;   >>> (define id-selector (make-alist-selector 'id))
;   >>> (id-selector '((id . 1234)))
;   >>> 1234
(define (make-alist-selector key)
  (lambda (alist)
    (cdr (assq key alist))))


; Returns a boolean predicate function that, on an association list, checks
; if the value of `key` is equal to `value`.
;
; Parameters:
;   key (symbol):
;   value (any):
;
; Examples:
;   >>> (define male-predicate (make-alist-predicate 'sex "male"))
;   >>> (make-alist-predicate male-student)
;   >>> #t
(define (make-alist-predicate key value)
  (let ((selector (make-alist-selector key)))
    (lambda (alist)
      (equal? (selector alist)
              value))))


; Returns a function that checks if an assocation list has a specific value in
; its type key. The function simply wraps make-alist-predicate.
(define (make-alist-type-predicate value)
  (make-alist-predicate 'type value))


; Returns a function that maps a list of alists to a list of their
; corresponding value of key.
;
; Parameters:
;   key (symbol): Key of the property to select from each alist.
;
; Examples:
;   >>> (define id-mapper (make-alist-selector 'id))
;   >>> (id-mapper students)
;   >>> '(1 2 3 4)
(define (make-alist-mapper key)
  (let ((selector (make-alist-selector key)))
    (lambda (lst)
      (map (lambda (alist)
             (selector alist))
           lst))))


; Returns a list of elements fulfilling a predicate.
;
; Parameters:
;   lst (list): The list of elements to check.
;   predicate (procedure): The boolean predicate function.
;
; Examples:
;   >>> (filter '(1 2 a b) number?)
;   >>> '(1 2)
(define (filter lst predicate)
  (if (null? lst)
      '()
      (let ((rest (filter (cdr lst) predicate)))
        (if (predicate (car lst))
            (cons (car lst) rest)
            rest))))


; Reduces a list based on a function and an initial value.
;
; Parameters:
;   fn (procedure): A reducer function that takes an element from lst
;     and the current result (initially init) as parameters.
;   lst (list): The list of elements to reduce.
;   init (any): The initial value for the result.
;
; Examples:
;   >>> (reduce + '(1 2 3 4) 0)
;   >>> 10
(define (reduce fn lst init)
  (if (null? lst)
      init
      (fn (car lst)
          (reduce fn
                  (cdr lst)
                  init))))


; Helper function for all? that checks if a single element fulfils a list of
; boolean predicate functions.
(define (fulfils-all? item predicates)
  (reduce (lambda (predicate acc)
            (and acc (predicate item)))
          predicates
          #t))


; Determines wether every element in a list fulfils an abritrary number of
; boolean predicate functions.
;
; Parameters:
;   lst (list): The list of elements to check.
;   predicates (procedure): Boolean predicates functions.
;
; Examples:
;   >>> (all? '(1 2 3 4) number? positive?)
;   >>> #t
(define (all? lst . predicates)
  (if (null? lst)
      #t
      (and (fulfils-all? (car lst)
                         predicates)
           (apply all?
                  (cons (cdr lst)
                        predicates)))))


; Determines wether an element is present in a list.
;
; Parameters:
;   lst (list): The list to look in.
;   element (any): The element to look for.
;
; Examples:
;   >>> (in? '(a b c) 'a)
;   >>> #t
(define (in? lst element)
  (cond ((null? lst) #f)
        (else (or (equal? (car lst) element)
                  (in? (cdr lst) element)))))


; Returns a list of unqiue elements in a list.
;
; Examples:
;   >>> (unique '(1 2 3 1))
;   >>> '(1 2 3)
(define (unique lst)
  (unique-helper lst '()))


; Helper function for unique which keep a list of unique values which is added
; to doing the iterations.
(define (unique-helper lst acc)
  (cond ((null? lst) acc)
        ((in? acc (car lst)) (unique-helper (cdr lst)
                                            acc))
        (else (unique-helper (cdr lst)
                             (cons (car lst)
                                   acc)))))


; Returns the first n elements in list.

; Examples:
;   >>> (take 2 '(a b c d))
;   >>> '(a b)
(define (take n lst)
  (cond ((= n 0) '())
        (else (cons (car lst)
                    (take (- n 1)
                          (cdr lst))))))


; Returns a list without the first n elements in a list. This is just an alias
; for the `list-tail` function with flipped parameters to fit the format of take.
;
; Examples:
;   >>> (drop 2 '(1 2 3 4))
;   >>> '(3 4)
(define (drop n lst)
  (cond ((> n (length lst)) '())
        (else (list-tail lst n))))


; Sums the numbers in a list.
(define (sum lst)
  (apply + lst))


; Returns a range from n to m.
;
; Examples:
;   >>> (range 1 5)
;   >>> '(1 2 3 4 5)
(define (range n m)
  (cond ((> n m) '())
        (else (cons n
                    (range (+ n 1)
                           m)))))


; Returns a list that repeats a list until the result has length n.
;
; Examples:
;   >>> (repeat '(a b c) 5)
;   >>> '(a b c a b)
(define (repeat lst n)
  (cond ((< n (length lst)) (take n lst))
        (else (repeat (append lst lst)
                      n))))


; Returns a list of pairs by zipping the elements of two lists.
;
; Examples:
;   >>> (pair-zip '(1 2 3) '(a b c))
;   >>> '((1 . a) (2 . b) (3 . c))
(define (pair-zip lst1 lst2)
  (cond ((or (null? lst1) (null? lst2)) '())
        (else (cons (cons (car lst1)
                          (car lst2))
                    (pair-zip (cdr lst1)
                              (cdr lst2))))))


; take equivalent for streams.
(define (stream-take n stream)
  (cond ((= n 0) '())
        (else (cons (car stream)
                    (stream-take (- n 1)
                                 (force (cdr stream)))))))


; map equivalent for streams.
(define (stream-map fn stream)
  (cons (fn (car stream))
        (delay (stream-map fn
                           (force (cdr stream))))))


; An abstraction on inexact->exact that converts a floating point number
; between 0 and 1 to an exact number between lower and upper. This is used for
; the random number generation functionality in random.scm.
(define (inexact->exact-between lower upper n)
  (inexact->exact (floor (+ (* n
                               (+ (- upper lower)
                                  1))
                            lower))))


; Returns a list with elements from list but in different order.
;
; Examples:
;   >>> (shuffle '(1 2 3 4))
;   >>> '(4 3 1 2)
(define (shuffle lst)
  (shuffle-helper lst (length lst)))


; Helper function for shuffle that, in a recursive manner, picks random
; elements from a list and generates a seed for the next random pick.
(define (shuffle-helper lst seed)
  (cond ((null? lst) '())
        (else (let* ((rand (random-between 0
                                           (- (length lst) 1)
                                           (* seed (length lst))))
                      (index (car rand))
                      (next-seed (cdr rand)))
                (cons (list-ref lst index)
                      (shuffle-helper (append (take index lst)
                                              (drop (+ index 1) lst))
                                      next-seed))))))


; Sorts a list of assocation lists by a giving key.
;
; Remark:
;   This is a very inefficient implementation since the input list is iterated
;   for each unique value for the passed key.
;
; Parameters:
;   lst (list): A list of assocation lists to sort.
;   key (symbol): The key to sort by.
(define (sort-by lst key)
  (let* ((unique-values (unique ((make-alist-mapper key) lst)))
         (sorted (map (lambda (value)
                        (filter lst (make-alist-predicate key value)))
                      unique-values)))
    (reduce append sorted '())))

