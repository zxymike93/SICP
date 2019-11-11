#lang sicp

;; 计算b^n的快速过程，利用 a*b^n 不变来迭代。
;; n为偶数：a*b^n = a * b^2^(n/2)
;; n为奇数：a*b^n = (a*b) * b^(n-1)
(define (fast-expt b n)
  (define (iter a b n)
    (cond [(= n 0) a]
          [(even? n) (iter a (* b b) (/ n 2))]
          [else (iter (* a b) b (- n 1))]))
  (iter 1 b n))

;; tests
(fast-expt 0 4)
(fast-expt 1 1)
(fast-expt 2 7)