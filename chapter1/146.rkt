#lang sicp

(define (iterative-improve good-enough? improve)
  (lambda (x) (if (good-enough? x)
                  x
                  ((iterative-improve good-enough? improve) (improve x)))))


;; sqrt in 1.1.7
(define (sqrt x)
  
  (define (improve guess)
    (/ (+ guess (/ x guess))
       2))
  
  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) 0.001))

  ((iterative-improve good-enough? improve) 1.0))


;; fixed-point in 1.3.3
(define (fixed-point f first-guess)

  (define (improve guess)
    (f guess))

  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) 0.00001))

  ((iterative-improve good-enough? improve) first-guess))

;; tests
(sqrt 4)
(fixed-point cos 1.0)