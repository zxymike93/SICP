#lang racket

; Finding fixed point of function
; f(x) = x

(define tolerance 0.0001)

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f guess)
  (let ([next-guess (f guess)])
    ; close-enough?
    (if ((lambda (a b)
           (< (abs (- a b)) tolerance))
         guess next-guess)
        next-guess
        (fixed-point f next-guess))))


(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)


(define (sqrt x)
  (fixed-point (lambda (y)
                 ; average damping
                 (average y (/ x y)))
               1.0))

(sqrt 4)