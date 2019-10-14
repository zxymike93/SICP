#lang sicp

(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))