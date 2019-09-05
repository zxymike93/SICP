#lang sicp

;; Define a procedure that takes three numbers as arguments and
;; returns the sum of the squares of the two larger numbers.

(define (sum a b) (+ a b))

(define (square x) (* x x))

(define (sum-of-squares p q) (sum (square p) (square q)))

(define (func x y z)
    (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
          ((and (<= y x) (<= y z)) (sum-of-squares x z))
          (else (sum-of-squares x y))))


(func 0 -2 3)