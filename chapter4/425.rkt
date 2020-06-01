#lang sicp

;; 假设有这么一个过程，而后求值 (factorial 5)
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
;; 其中 unless 的定义如下
(define (unless predicate alternative consequence)
  (if predicate
      consequence
      alternative))

;; 如果语言是应用序的话
(factorial 5)
(unless (= 5 1) (* 5 (factorial (- 5 1))) 1)
(unless #f (* 5 (unless (= 4 1) (* 4 (factorial (- 4 1))) 1)) 1)
;; 根据应用序，要对过程参数求值后再代入过程体，于是将不断求值 (factorial (- n 1))
;; 结果为 (factorial (- 3 1)), (factorial (- 2 1)), (factorial (- 1 1))...
;; 于是不会得到答案而是永远循环下去

;; 如果使用正则序，则可以正常计算
(factorial 5)
(unless (= 5 1) (* 5 (factorial (- 5 1))) 1)
(if (= 5 1) 1 (* 5 (factorial (- 5 1))))y
(if false 1 (* 5 (factorial 4)))
(* 5 (factorial 4))