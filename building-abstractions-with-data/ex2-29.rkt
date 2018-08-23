#lang sicp

; Speaking of `mobile` and `branch` here,
; use full spell for naming procedures
; but `mob` and `br` for naming arguments

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

#|
; 4
; Suppose we change the constructor of mobile and branch
; We only need to change the selector correspondingly

(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))
|#

; 2
(define (total-weight mob)
  (+ (branch-weight (left-branch mob))
     (branch-weight (right-branch mob))))

(define (is-weight? x) (not (pair? x)))

(define (branch-weight br)
  (let ([right (branch-structure br)])
    (if (is-weight? right)
        right
        (total-weight right))))

; 3
(define (torque br)
  (* (branch-length br)
     (branch-weight br)))

(define (same-torque? mob)
  (= (torque (left-branch mob))
     (torque (right-branch mob))))

(define (balanced? mob)
  (if (not (pair? mob))
      #t
      (and (same-torque? mob)
           (and (balanced? (branch-structure (left-branch mob)))
                (balanced? (branch-structure (right-branch mob)))))))

; Test
[define M1 (make-mobile (make-branch 10 (make-mobile (make-branch 2 3)
                                                     (make-branch 2 3)))
                        (make-branch 12 5))]

[define M2 (make-mobile (make-branch 10 6)
                        (make-branch 12 (make-mobile (make-branch 2 3)
                                                     (make-branch 3 2))))]

[define M3 (make-mobile (make-branch 2 M1)
                        (make-branch 3 M2))]

(total-weight M1)
(total-weight M2)
(total-weight M3)

(balanced? M1)
(balanced? M2)
(balanced? M3)
