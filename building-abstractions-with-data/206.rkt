#lang sicp

(define zero
  (lambda (f)
    (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

;; 其中 ((n f) x) 表示数值 n
;; 设 ((m f) y) 表示数值 m
;; 其中 y = n
(define (add n m)
  (lambda (f)
    (lambda (x) ((m f) ((n f) x)))))

;; test
(define (church->int ch)
  ((ch (lambda (x) (+ x 1))) 0))

(church->int zero)
(church->int one)
(church->int two)

(church->int (add one two))