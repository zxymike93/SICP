#lang sicp

;; 1、举例证明:现在的 good-enough? 用于测试对非常小/大的数不实用。（解释在最后面）
;; 2、改进 good-enough?

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

;; The original good-enough?
;(define (good-enough? guess x)
;  (> 0.001
;     (abs (- x
;             (square guess)))))

;; 改进版本1：如果两次 improve 之间变化不大（差小于 guess * 0.001），就可以认为答案接近理想答案。
;(define (good-enough? guess x)
;  (> (* guess .001)
;     (abs (- (improve guess x)
;             guess))))

;; 改进版本2：如果 improve 函数的变化率很小，就可以认为答案理想。 （(b-a)/a < 0.001）
(define (good-enough? guess x)
  (> 0.001
     (abs (/ (- (improve guess x)
                guess)
             guess))))

(define (square a)
  (* a a))

(define (improve guess x)
  (average (/ x guess) guess))

(define (average a b)
  (/ (+ a b)
     2))

;; 对于非常小的数（如，x < tolerance）
(sqrt 0.0001)
;; => 0.03230844833048122
;; 以上面为例，理想的答案应该是接近 0.01，但当 guess 接近 0.03 的时候
;; 因为测试条件是 (> 0.001 (abs (- 0.001 (square guess))))
;; (x)0.001 - 0.0009 < (tolerance)0.001

;; 对于非常大的数
(sqrt 10000000000000)
;; 程序会进入死循环，(sqrt-iter (improve guess x) x)
;; 但是内存不会溢出，因为上面的写法虽然是递归（调用）的，但过程的估值是遍历的。(*注意*:和1.6不同)
;; (sqrt-iter 1.0 10000000000000)
;; (sqrt-iter 5000000000000.5 10000000000000)
;; (sqrt-iter 2500000000001.25 10000000000000)
;; 理想情况下，答案应该接近 3162333（之类的），但是受限于精度，特别是计算除法的时候，
;; (average (/ x guess) guess) 无法合理迫近理想答案，而导致 (good-enough? guess x) 一直得出 #f 陷入死循环。