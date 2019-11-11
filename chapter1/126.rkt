#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

;; Louis Reasoner 的写法
(define (expmod base exp mod)
  (cond [(= exp 0)
         1]
        [(= exp 1)
         base]
        [(even? exp)
         (remainder (* (expmod base (/ exp 2) mod)
                       (expmod base (/ exp 2) mod))
                    mod)]
        [else
         (remainder (* (expmod base 1 mod) (expmod base (- exp 1) mod))
                    mod)]))


;; 以 (expmod 2 4 3) 为例：
;(r (* (expmod 2 2 3)
;      (expmod 2 2 3)) 3)
;(r (* (r (* (expmod 2 1 3)
;            (expmod 2 1 3)) 3)
;      (r (* (expmod 2 1 3)
;            (expmod 2 1 3))) 3))

;; 书本的写法（略）
;; (expmod 2 4 3)
;(r (sq (expmod 2 2 3)))
;(r (sq (r (sq (expmod 2 1 3)) 3)) 3)

;; 可见 Louis Reasoner 的写法将 expmod 从线性递归变成了树状递归
;; 时间复杂度为 (log.exp)^N = exp*log.exp
;; 也就是说，随着 exp 的规模增大，耗时以 exp*log.exp 增加
;; 相当于线性递增