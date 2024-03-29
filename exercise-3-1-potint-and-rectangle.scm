; Exercise 3.1: Points and Rectangle.

; Sends `message` to `obj` with parameters `par`
(define (send message obj . par)
  (let ((method (obj message)))
    (apply method par)))

; Object-like representation of a point.
(define (point x y)
  (letrec ((getx (lambda () x))
           (gety (lambda () y))
           (add (lambda (p)
                  (point (+ x (send 'getx p))
                         (+ y (send 'gety p)))))
           (move (lambda (dx dy)
                   (point (+ x dx)
                          (+ y dy))))
           (type-of (lambda () 'point)))
    (lambda (message)
      (cond ((eq? message 'getx) getx)
            ((eq? message 'gety) gety)
            ((eq? message 'add) add)
            ((eq? message 'move) move)
            ((eq? message 'type-of) type-of)
            (else (error "Message not understood"))))))

; Object-like representation of a rectangle
(define (rectangle p1 p2)
  (letrec ((getp1 (lambda () p1))
           (getp2 (lambda () p2))
           (move (lambda (dx dy)
                   (rectangle (send 'move p1 dx dy)
                              (send 'move p2 dx dy))))
           (area (lambda ()
                   (* (abs (- (send 'getx p1)
                              (send 'getx p2)))
                      (abs (- (send 'gety p1)
                              (send 'gety p2))))))
           (type-of (lambda () 'rectangle)))
    (lambda (message)
      (cond ((eq? message 'getp1) getp1)
            ((eq? message 'getp2) getp2)
            ((eq? message 'move) move)
            ((eq? message 'area) area)
            ((eq? message 'type-of) type-of)
            (else (error "Message not understood"))))))
