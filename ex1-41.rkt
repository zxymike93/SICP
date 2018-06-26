#lang sicp

(define (inc x)
  (+ x 1))

(define (double pcd)
  (lambda (x)
    (pcd (pcd x))))

;Value 21
(((double (double double)) inc) 5)