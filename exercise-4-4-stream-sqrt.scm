; Exercise 4.4: A stream that converges to the square root a number

(define (improve-sqrt-guess guess x)
  (/ (+ guess (/ x guess)) 2))

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
                    (stream-section
                      (- n 1)
                      (tail stream))))))

(define (map-stream f stream)
  (cond ((empty-stream? stream) the-empty-stream)
        (else (cons-stream (f (head stream))
                           (map-stream f
                                       (tail stream))))))

(define (sqrt-stream x)
  (sqrt-stream-helper x 1.0))

(define (sqrt-stream-helper x g)
  (cons-stream g
               (sqrt-stream-helper x
                                   (improve-sqrt-guess g x))))
