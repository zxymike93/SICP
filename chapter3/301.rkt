#lang sicp

(define (make-accumulator sum)
  (lambda [arg]
    (begin (set! sum (+ sum arg))
           sum)))

;; test
(define A (make-accumulator 5))
(define B (make-accumulator 0))
(A 10)
(B 10)
(A 10)
(B 10)

;; # explanation
;; 1、我们有一个 Global 环境(global-frame)：+ - * / ....
;; define 将 make-accumulator 绑定到这个 Global 里面去
;; 同理，define A / B 也绑定到了 Global-frame 中
;; 2、调用 make-accumulator 时，因为它返回一个 lambda
;; lambda 产生一个 frame，这个环境里面包含 lambda 的参数
;; 所以我们有了 A-frame(sum = 5) , B-frame(sum = 0)
;; 3、当我们调用 A，
;; 首先从 Global-frame 里面找到 A
;; 然后发现 A-frame 里面 sum=5