; Exercise 4.2: A stream of factorial numbers

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail stream)
  (force (cdr stream)))

(define empty-stream? null?)

(define the-empty-stream '())

(define (stream-section n stream)
  (cond ((= n 0) '())
        (else (cons (head stream)
                    (stream-section (- n 1)
                                    (tail stream))))))

(define (add-streams s1 s2)
  (let ((h1 (head s1))
        (h2 (head s2)))
    (cons-stream (+ h1 h2)
                 (add-streams (tail s1)
                              (tail s2)))))

(define ones (cons-stream 1 ones))

(define nat-nums
  (cons-stream 1 (add-streams ones nat-nums)))

(define (combine-streams s t operator)
  (let ((h1 (head s))
        (h2 (head t)))
    (cons-stream (operator h1 h2)
                 (combine-streams (tail s)
                                  (tail t)
                                  operator))))

(define (mult-streams s t)
  (combine-streams s t *))

(define facts
  (cons-stream 1
               (mult-streams (tail nat-nums)
                             facts)))
