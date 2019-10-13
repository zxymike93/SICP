#lang sicp

;;; 1 + 2 + 3 + ... + (n-2) + (n-1) + n
;(define (sum-int a b)
;  (if (> a b)
;      0
;      (+ a
;         (sum-int (+ a 1) b))))
;
;;; 1^2 + 3^2 + 5^2 + ... + (n-4)^2 + (n-2)^2 + n^2
;(define (sum-sq a b)
;  (if (> a b)
;      0
;      (+ (sq a)
;         (sum-sq (+ a 2) b))))
;
;;; 对于上面2个计算过程，它们的形式基本一致：从 lower bound 加到 upper bound
;;; 从程序抽象的角度，可以将两个过程提取出来，作为参数传递。(lisp 正好提供这种特性）
;;; 从数学的角度，早已有累加公式来表达 sum(a~b)-fn = fa + ... + fb，下面的过程正是表示这样的计算。
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))
;
;;; 有了这项公式，只要表达累加公式里面的 f 和 a~b的变化规律
;;; 知道累加的 lower bound 和 upper bound
;;; 就可表达 sum-int, sum-sq, sum-xxx 这类累加类型的计算过程。
;(define (indentify n) n)
;(define (inc n) (+ n 1))
;(define a 1)
;(define b 10)
;(sum-int indentify a inc b)
;
;(define (square n) (* n n))
;(define (next-n n) (+ n 2))
;(define a 1)
;(define b 101)
;(sum-sq square a next-n b)
;
;;; 甚至可以表示更复杂的积分公式，因为积分的计算过程也是一个累加的过程
;;; integral(a~b)-fn = [f(a+dx/2) + f(a+dx/2+dx) + f(a+dx/2+2dx) + ...] * dx = sum(a~b)-fn * dx
;(define (integral f a b dx)
;  (define (next x) (+ x dx))
;
;  (* (sum f (+ a (/ dx 2)) next b)
;     dx))
;
;;; 那么计算 cube 0~1 的积分可以这样表示
;(integral cube 0 1 0.01)

;; 另一个求积分的方法是 Simpson 提出的，对于偶数n
;; integral(a~b)-yn = [y0 + 4y1 + 2y2 + ... + 2yn-2 + 4yn-1 + yn] * h/3
;; integral(a~b)-fk = [f(a+0h) + 4f(a+1h) + 2f(a+2h) + .... + f(a+nh)] * (b-a)/3n
;; *注意：这里的 k 是下标，n是项数（作用和dx差不多，控制精度）*
;; 那么就知道
;(define h (/ (- b a) n))
;(define (next x) (+ a (* k h)))
;; 其中 k = {0, n}
;; 但是还要注意，f(a+0h) + 4f(a+1h) + 2f(a+2h) + .... + f(a+nh) 并不是 f0 + f1 + f2 + ... 的形式
(define (get-factor k n)
  (cond [(or (= k 0) (= k n)) 1]
        [(even? k) 4]
        [else 2]))

(define (sum-simpson term a next b k n)
  (if (> a b)
      0
      (+ (* (get-factor k n) (term a))
         (sum-simpson term (next a) next b (+ k 1) n))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ a (* n h)))

  (* (/ h 3)
     (sum-simpson term a next b 0 100)))

(simpson cube 0 1 100)
  