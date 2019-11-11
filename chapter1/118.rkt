#lang sicp

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

;; * 的迭代版
;; even: p + 2a * b/2
;; odd: (p+a) + a * (b-1)
;; 计算结果记录在 p 里面
(define (* a b)
  (define (iter product a b)
    (cond [(= b 0) product]
          [(even? b) (iter product (double a) (halve b))]
          [(odd? b) (iter (+ product a) a (- b 1))]))
  (iter 0 a b))

;; tests
(* 2 0)
(* 2 3)
(* 0 10)