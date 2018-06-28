#lang sicp


(define (compose f g)
  (lambda (x) (f (g x))))

; Repeat f() n times
(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define (smooth f)
  (let ([dx 0.0001])
    (lambda (x) (/ (+
                    (f (+ x dx))
                    (f x)
                    (f (- x dx)))
                   3))))

(define (n-fold f n)
  ((repeated smooth n) f))


((n-fold inc 3) 3)