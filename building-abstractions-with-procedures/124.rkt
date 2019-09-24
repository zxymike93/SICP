#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

;; X*Y mod M = [(X mod M) * (Y mod M)] mod M
;; b^n mod m = {[b^(n-1) mod m] * (b mod m)} mod m , n为奇数
;;           = {[b^(n/2) mod m]^2} mod m           , n为偶数
;;           = b mod m                             , n = 1
;;           = 1 mod m                             , n = 0
(define (expmod base exp mod)
  (cond [(= exp 0)
         1]
        [(= exp 1)
         base]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) mod))
                    mod)]
        [else
         (remainder (* (expmod base 1 mod) (expmod base (- exp 1) mod))
                    mod)]))

;; 对于 1 < a < n-1
;; 如果 (a^n) mod m = a
;; n 很可能是素数（存在的a越多可能性越大）
(define (fermat-test n)
  (define (try a)
    (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) #t]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else #f]))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      ))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

;; 根据22题的测试结果，选择下面测试用例
;; 2 用来排除第一次计算耗费的时间
(timed-prime-test 2)
;; (由于随机数限制的问题只测试了一组)
(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)
;; 作为对比选择了缩小10倍
(timed-prime-test 100000007)
;; 作为对比选择缩小一半倍数级
(timed-prime-test 10037)

;; 测得数据如下
;1000000007 *** 74
;1000000009 *** 70
;1000000021 *** 71
;100000007 *** 64
;10037 *** 36

;; 观察得知，10倍差距所耗费的时间差距已经变得很小
;; 而倍数级的作用符合 log(10000) + log(10000) = log(100000000)