#lang sicp

;; 定义一个过程，它接收三个参数，并求其中最大两者只和。

(define (sum a b) (+ a b))

(define (square x) (* x x))

(define (sum-of-squares p q) (sum (square p) (square q)))

(define (func x y z)
    (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
          ((and (<= y x) (<= y z)) (sum-of-squares x z))
          (else (sum-of-squares x y))))


(func 0 -2 3)