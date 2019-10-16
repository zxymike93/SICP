#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; 修改区间除法，当出现除数跨0时，发出error
(define (div-interval x y)
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
      (error "Can not divide by an interval that spans zero.")
      (mul-interval x
                    (make-interval
                     (/ 1. (upper-bound y))
                     (/ 1. (lower-bound y))))))

;; tests
(define a (make-interval 4 8))
(define b (make-interval -2 3))
(print-interval (div-interval a b))