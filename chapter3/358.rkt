#lang sicp

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; 对于 (expand 1 7 10)
;; (1 (4 (2 (8 (5 (7 (1 (4 (2 (8 ...

;; 对于 (expand 3 8 10)
;; (3 (7 (5 (0 (0 (0 ....

;; 补充：根据网上大神的解答，结果是 num/den 以 radix 为基数的浮点数表示
;; 1/7 = 0.142847142857
;; 3/8 = 0.375000