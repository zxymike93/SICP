#lang sicp

(define (sqrt x)
  (sqrt-iter 1.0 x))

;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (> 0.001
     (abs (- (square guess)
             x))))

(define (square x)
  (* x x))

(define (improve guess x)
  (average (/ x guess)
           guess))

(define (average a b)
  (/ (+ a b)
     2))

;; Exercise
;; 将 sqrt-iter 中的 if 替换为 new-if，会得到什么执行结果？
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) guess
          (sqrt-iter (improve guess x) x)))

;; Tests
(sqrt 2)
(sqrt 4)
(sqrt 9)

;; Solution

;; 根据应用序，执行 new-if 的时候，会对三个参数估值。
;; (sqrt-iter 1 2)
;; (new-if (good-enough? 1 2) 1 (sqrt-iter (improve 1 2) 2))
;; (new-if (good-enough? 1 2) 1 (sqrt-iter 1.5 2))
;; (new-if (good-enough? 1 2) 1 (new-if (good-engouh? 1.5 2) 1.5 (sqrt-iter (improve 1.5 2) 2)))
;; (new-if (good-enough? 1 2) 1 (new-if (good-engouh? 1.5 2) 1.5 (sqrt-iter 1.4 2)))
;; (new-if (good-enough? 1 2) 1 (new-if (good-engouh? 1.5 2) 1.5 (new-if (good-enough? 1.4 2) 1.4 (sqrt-iter (improve 1.4 2) 2))))
;; 可见，结果会陷入无限循环。

;; 虽然 if 可以备看作是 cond 的语法糖，但是根据 <predicate> 真假选择执行哪个结果
;; if <predicate> <then-clause> <else-clause>
;; 这里的 <then-clause> 作为 base case 会终止程序。（递归程序必须要有终止的情况）
;; 而题目里的 new-if 因为是过程，所以会一直递归下去。
;; => Interactions disabled; out of memory