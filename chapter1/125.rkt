#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (define (iter product b n)
    (cond [(= n 0) product]
          [(even? n) (iter product (* b b) (/ n 2))]
          [else (iter (* product b) b (- n 1))]))
  (iter 1 b n))

(define (expmod base exp mod)
  (remainder (fast-expt base exp) mod))

;; tests
(expmod 5 100 101)

;; 事实上，采用以上方法改写 expmod 可以得到正确的答案，但是
;; 结果是通过计算 (r 7888609052210118054117285652827862296732064351090230047702789306640625 101) 得到的
;; 可以看出，这个方法因为先计算 b^n 而消耗更多计算资源

;; 相较而言
;; (fast-expmod 5 101 101)
;; (r (sq (fast-expmod 5 50 101)) 101)
;; ...
;; 线性递归的下半部，计算 (r (sq (r (sq ...
;; 每次计算 square 的值都会因为经过 remainder 计算而小于 101