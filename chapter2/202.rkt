#lang sicp

;; point
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; segment
(define (make-segment start end)
  (cons start end))

(define (start-point s)
  (car s))

(define (end-point s)
  (cdr s))

;; answer
(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (make-point (average (x-point (start-point s))
                       (x-point (end-point s)))
              (average (y-point (start-point s))
                       (y-point (end-point s)))))

;; tests
(define a (make-point 0 0))
(define b (make-point 1 1))
(print-point a)
(print-point b)

(define l (make-segment a b))
(define c (midpoint-segment l))
(print-point c)