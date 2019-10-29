#lang sicp

;; Vector
(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

;; Segment
(define (make-segment start end)
  (cons start end))

(define start-segment car)

(define end-segment cdr)