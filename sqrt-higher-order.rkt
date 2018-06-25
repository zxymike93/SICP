#lang sicp

[define tolerance 0.001]


(define (average a b)
  (/ (+ a b) 2))


(define (average-damp f)
  (lambda (x) (average x (f x))))


; x = f(x)
; do (f(f(f(f(f(f(x).... by
; `try` (f x) to get `next-guess`
; until `close-enough`
(define (fixed-point f first-guess)

  (define (close-enough? guess next-guess)
    (< (abs (- guess next-guess)) tolerance))

  (define (try guess)
    (let ([next-guess (f guess)])
      (if (close-enough? guess next-guess)
          next-guess
          (try next-guess))))

  (try first-guess))


; 1)
;   square-root of y = f(y) = x/y
;   thus, use `fixed-point`
; 2)
;   According to [next-guess (f guess)] 
;   y1
;   y2 = x / y1
;   y3 = x / y2 = x / (x / y1) = y1
;   so we need `average-damp` to avoid infinitely looping
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))


(sqrt 4)