#lang sicp

;; version 1
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)

(define xcor-frame cadr)

(define ycor-frame caddr)

;; version 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)

(define xcor-frame cadr)

(define ycor-frame cddr)