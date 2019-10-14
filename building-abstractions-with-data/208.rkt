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

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; tests
(print-interval
 (sub-interval (make-interval -1 3)
               (make-interval 0 4)))