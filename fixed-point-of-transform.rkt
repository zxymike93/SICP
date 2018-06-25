#lang sicp


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

(define (fixed-point-of-transform g trans guess)
  (fixed-point (trans g) guess))


;
(define (deriv g)
  [define dx 0.00001]
  
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx))
)

(define (newtons-method g)
  (lambda (x)
    (- x
       (/ (g x) ((deriv g) x)))))

(define (sqrt-by-newtons x)
  (fixed-point-of-transform
   (lambda (y) (- (* y y) x))
   newtons-method
   1.0))

(sqrt-by-newtons 4)


;
(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (sqrt-by-averdamp x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(sqrt-by-averdamp 4)