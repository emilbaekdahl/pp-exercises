; Exercise: Stream appending and stream merging

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail stream) (force (cdr stream)))

(define empty-stream? null?)

(define the-empty-stream '())

(define (stream-section n stream)
  (cond ((= n 0) '())
        (else (cons (head stream)
                    (stream-section (- n 1)
                                    (tail stream))))))

(define (stream-append s t)
  (if (empty-stream? s)
      t
      (cons-stream (head s)
                   (stream-append (tail s)
                                  t))))

(define (stream-merge s t)
  (cons-stream (head s)
               (cons-stream (head t)
                            (stream-merge (tail s)
                                          (tail t)))))
