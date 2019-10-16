#lang sicp

;; interval: cons
(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; num num -> interval
(define (make-center-percent c p)
  (let ([width (* c p)])
    (make-interval (- c width) (+ c width))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2))

(define (percent i)
  (abs (/ (/ (- (lower-bound i) (upper-bound i))
             2)
          (center i))))

(define (print-interval x)
  (display (center x))
  (display ",")
  (display (percent x))
  (newline))

;; test
(define a (make-center-percent 4 0.02))
(define b (make-center-percent 5 0.03))

(print-interval a)
(print-interval b)

(define c (mul-interval a b))
(print-interval c)

;; 通过观察发现：两个区间积的 percent 约等于各自 percent 的和