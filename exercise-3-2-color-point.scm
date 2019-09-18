; Exercise 3.2: Color Point Extensions

(define (new-instance class . parameters)
  (let ((instance (apply class parameters)))
    (virtual-operations instance)
    instance))

; Arrange for virtual operations in object
(define (virtual-operations object)
  (send 'set-self! object object))

(define (new-part class . parameters)
  (apply class parameters))

(define (method-lookup object selector)
  (cond ((procedure? object) (object selector))
        (else (error "Inappropriate object in method-lookup: " object))))

(define (send message object . args)
  (let ((method (method-lookup object message)))
    (cond ((procedure? method) (apply method args))
          ((null? method) (error "Message not understood: " message))
          (else (error "Inappropriate result of method lookup: " method)))))


(define (object)
  (let ((super '())
        (self 'nil))

    (define (set-self! object-part)
      (set! self object-part))

    (define (dispatch message)
      (cond ((eqv? message 'set-self!) set-self!)
            (else '())))

    (set! self dispatch)
    self))


(define (point x y)
  (let ((super (new-part object))
        (self 'nil))
    (let ((x x)
          (y y))

      (define (getx) x)

      (define (gety) y)

      (define (add p)
        (point
          (+ x (send 'getx p))
          (+ y (send 'gety p))))

      (define (type-of) 'point)

      (define (point-info)
        (list (send 'getx self) (send 'gety self) (send 'type-of self)))

      (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

      (define (dispatch message)
        (cond ((eqv? message 'getx) getx)
              ((eqv? message 'gety) gety)
              ((eqv? message 'add)  add)
              ((eqv? message 'point-info)  point-info)
              ((eqv? message 'type-of) type-of)
              ((eqv? message 'set-self!) set-self!)
              (else (method-lookup super message))))

      (set! self dispatch))
    self))


(define (color-point x y color)
  (let ((super (new-part point x y))
        (self 'nil))
    (let ((color color))

      (define (get-color) color)

      (define (type-of) 'color-point)

      (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

      (define (dispatch message)
        (cond ((eqv? message 'get-color) get-color)
              ((eqv? message 'type-of) type-of)
              ((eqv? message 'set-self!) set-self!)
              (else (method-lookup super message))))

      (set! self dispatch))
    self))
