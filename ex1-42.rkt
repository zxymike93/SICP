#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc x) (+ x 1))

(define (square x) (* x x))


((compose square inc) 6)
; Should be, (square (inc 6))