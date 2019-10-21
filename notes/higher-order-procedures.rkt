#lang sicp

;; todo: procedures as arguments


;; fixed-point 满足 f(x)=x
;; 利用这个概念，可以对某些函数求解，比如：求 x 的平方根 y
;; 即 y^2=x，可知 y=x/y
;; 所以只要求得 f(y)=x/y 的 fixed-point 即可得到 x 的平方根

;; 通过将过程作为参数传递，可以实现 fixed-point 的概念
(define (average a b)
  (/ (+ a b)
     2))

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (> 0.00001
       (abs (- a b))))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 2)

;; 上面使用了 (average y (/ x y)) 而不是 (/ x y)，
;; 是因为如果使用后者，在计算中会“摇摆”，而无法“收敛”出答案来：
;; guess: y , next: (/ x y)
;; guess: (/ x y), next: (/ x (/ x y))

;; 于是引入一个叫 average damping 的计算方法：在影响方程解的情况下，减少演进的变化程度。
;; 比如这里求 y=x/y 的解，改为求 y = y/2 + x/2y 的解（如上 average 过程所示）
;; 又比如改为 y = y/3 + 2x/3y，
;; 又或者求 x 的三次方根， y = y * x/y^2
;; 所以 average-damping 可以抽象为一个更高级的过程

;; 因为我们不关心求的是 x 的平方根还是三次方根，只需要表达 x 和 f(x) 的平均值。
;; 因为要传给 fixed-point 第一个参数，average-damp 的返回值为过程
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cbrt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; 说明 procedures as return values 的另一个例子是导数
;; 比如：x^3 的导数 3x^2 ，均是关于 x 的函数
;; 那么导数的表示方式为 (D x f)=>fi （既原函数为参数，返回其导数）

;; 牛顿法就使用了导数这个概念，
;; 如果关于 x 的函数 g(x) 可导，那么等式 g(x) = 0 的解为
;; 函数 f(x) = x - g(x)/Dg(x) 的 fixed point（其中 Dg(x) 为导数）

;; 根据导数的定义，函数gx在点x1的导数 Dg(x1) = [g(x2) - g(x1)] / (x2-x1)
;; 设 x2-x1 = dx
(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx))
                    (g x))
                 dx)))

(define (newtons-method g)
  (lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x)))))

(define (root-of-function g guess)
  (fixed-point (newtons-method g)
               guess))

;; 实际上，求 x 的平方根，也可以用牛顿法
;; g(y) = x - y^2 的解即 x 的平方根
(define (sqrt x)
  (root-of-function (lambda (y) (- x (square y)))
                    1.0))

;; 以上两个例子，无论是 fixed point search 还是 newtons method 来求 sqrt
;; 实际上都是利用 x = y^2 这条等式的变体，通过 fixed-point 这个过程来求解的
;; 可以定义一个表达这一概念的过程
(define (fixed-point-transform f transform guess)
  (fixed-point (transform f) guess))

;; sqrt: using average damping
(define (sqrt x)
  (fixed-point-transform (lambda (y) (/ x y))
                         average-damp
                         1.0))

;; sqrt: using newton's method
(define (sqrt x)
  (fixed-point-transform (lambda (y) (- (square y) x))
                         newtons-method
                         1.0))