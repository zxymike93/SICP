#lang sicp

;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

;; 上面对于 + 的两种不同定义，它们的估值过程是递归的？还是迭代的？请解释。

;; 第一种，递归过程
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;; 第二种，迭代过程
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9