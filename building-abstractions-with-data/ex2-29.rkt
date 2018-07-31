#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; 1
(define (left-branch br)
  (car br))

(define (right-branch br)
  (cadr br))

(define (branch-length br)
  (car br))

(define (branch-structure br)
  (cadr br))

; 2
(define (total-weight mob)
  (+ (weight (left-branch mob))
     (weight (right-branch mob))))

(define (weight br)
  (let ([right (branch-structure br)])
    (if (is-weight? right)
        right
        (total-weight right))))

(define (is-weight? x)
  (not (pair? x)))

; Test
(define a (make-branch 10 10))
(define b (make-branch 5 5))
(define m1 (make-mobile a b))

(define c (make-branch 1 m1))
(define m2 (make-mobile a c))

(define d (make-branch 2 m2))
(define m3 (make-mobile c d))

(total-weight m3)

;