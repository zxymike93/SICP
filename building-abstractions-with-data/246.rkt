#lang sicp

(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

;; (x1,y1) + (x2,y2) = (x1+x2,y1+y2)
(define (add-vect v1 v2)
  (let ([x (+ (xcor-vect v1) (xcor-vect v2))]
        [y [+ (ycor-vect v1) (ycor-vect v2)]])
    (make-vect x y)))

;; (x1,y1) - (x2,y2) = (x1-x2,y1-y2)
(define (sub-vect v1 v2)
  (let ([x (- (xcor-vect v1) (xcor-vect v2))]
        [y [- (ycor-vect v1) (ycor-vect v2)]])
    (make-vect x y)))

;; s * (x,y) = (sx,sy)
(define (scale-vect s v)
  (let ([x (* s (xcor-vect v))]
        [y (* s (ycor-vect v))])
    (make-vect x y)))