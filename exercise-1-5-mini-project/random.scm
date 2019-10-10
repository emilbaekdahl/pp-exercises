; Emil Bækdahl <ebakda16@student.aau.dk> (20165378)

; This file define pseudo-random number generation functionality in a pure
; functional way. As mentioned in the main file this has its limitation by the
; implmentation of a good pseudo-random number gernator has not been the focus
; of this project.

; Returns a pseudo random number and a new seed.
;
; Parameters:
;   seed (number): A number of starting the generation process.
;
; Examples:
;   >>> (random 100)
;   >>> '(87 . 124)
(define (random seed)
  (let ((number (* seed (modulo seed 11))))
    (cons (/ (modulo number 100) 100.0)
          (+ number 42))))


; Returns a pseudo random number between in the range of a lower and upper
; bound and a new seed.
(define (random-between lower upper seed)
  (let ((rand (random seed)))
    (cons (inexact->exact-between lower
                                  upper
                                  (car rand))
          (cdr rand))))


; Creates a stream of pseudo random floating point numbers between 0 and 1
; bootstrapped with a seed.
;
; Examples:
;   >>> (stream-take 4 (make-random-stream 1))
;   >>> '(0.01 0.04 0.6 0.44)
(define (make-random-stream seed)
  (let ((rand (random (* seed
                         (round (sqrt seed))))))
    (cons (car rand)
          (delay (make-random-stream (cdr rand))))))


; Creates a stream of pseudo random numbers in the range of a lower and upper
; bound.
;
; Examples:
;   >>> (stream-take 4 (make-random-stream-between 1 10 123))
;   >>> '(1 3 5 5)
(define (make-random-stream-between lower upper seed)
  (let ((random-stream (make-random-stream seed))
        (mapper (lambda (number)
                  (inexact->exact-between lower upper number))))
    (stream-map mapper random-stream)))
