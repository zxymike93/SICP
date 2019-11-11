#lang sicp

;; 利用牛顿法求三次方根， improve 的公式为
;; [(x / y^2) + 2*y] / 3

(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (> 0.001
     (abs (/ (- (improve guess x)
                guess)
             guess))))

(define (improve y x)
  (/ (+ (/ x (* y y))
        (* 2 y))
     3))

;; tests
(cbrt 0)
(cbrt 1)
(cbrt 27)
(cbrt 0.0001)
(cbrt 10000000000000)