#lang sicp

(define (square x) (* x x))


(define (fixed-point f guess)

  [define tolerance 0.001]
  
  (define (close-enough? a b)
    (if (< (abs (- a b)) tolerance)
        #t
        #f))

  (let ([next-guess (f guess)])
    (if (close-enough? guess next-guess)
        next-guess
        (fixed-point f next-guess)))
)


; 导数 Derive
; If `dx` is small enough
; Dg(x) = [g(x+dx) - g(x)] / dx

[define dx 0.00001]

(define (D g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))


; Newton's method
; If g(x) is deritive,
; then, the root of g(x) ( the answer of g(x)=0 )
; is the fixed point of f(x) ( satisfies x=f(x) )
; f(x) = x - [g(x) / Dg(x)]

(define (newtons-method g)
  (lambda (x)
    (- x
       (/ (g x) ((D g) x)))))

(define (newtons-by-fp g guess)
  (fixed-point (newtons-method g) guess))


; To compute square root of x,
; find the root (zero point) of: y^2 - x = 0

(define (sqrt x)
  (newtons-by-fp (lambda (y) (- (square y) x))
                 1.0))


(sqrt 4)