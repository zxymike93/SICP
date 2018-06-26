#lang sicp


(define (compose f g)
  (lambda (x) (f (g x))))

; Repeat f() n times
(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))


(define (square x) (* x x))

((repeated square 2) 5)