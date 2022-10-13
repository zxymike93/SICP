#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (cond [(> (square d) n) n]
        [(divides? n d) d]
        [else (find-divisor n (+ d 1))]))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder a b) 0))

;; 计算 199 1999 19999 的最小因子
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)