#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  
  (define (loop g count)
    (if (= count 0)
        (lambda (x) (g x))
        (loop (compose f g) (- count 1))))
  
  (loop (lambda (x) x) n))

(define (average a b c)
  (/ (+ a b c)
     3))

;; 存在函数 f，点 x 在 f(x) 上的 smooth version 等于 f(x-dx), f(x), f(x+dx) 的平均值
(define dx 0.00001)

(define (smooth f)
  (lambda (x) (average (f (- x dx))
                       (f x)
                       (f (+ x dx)))))

(define (nfold-smoothed f n)
  ((repeated smooth n) f))

;; tests
((nfold-smoothed (lambda (x) (* x x))
                10) 2.1)
((lambda (x) (* x x)) 2.1)   