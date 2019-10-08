#lang sicp

(define (fixed-point f first-guess)
  
  (define (close-enough? a b)
    (> 0.00001
       (abs (- a b))))
  
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx))
                    (g x))
                 dx)))

(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x)))))

(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

(define (newtons-method g guess)
  (fixed-point-of-transform g newton-transform guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a (* x x))
                 (* b x)
                 c)))

;; tests
(newtons-method (cubic 1 1 6) 1)
(newtons-method (cubic 4 3 -8) 1)
(newtons-method (cubic 1 3 -18) 1)