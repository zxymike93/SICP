#lang sicp

;; 书中例子给出的素数测试过程
(define (prime? n)
  (= (smallest-divisor n) n))

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

;; 题目中给出的计算 prime? 耗时的过程
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (odd? n)
  (= (remainder n 2) 1))

;; 写一个过程，测试指定 range 内奇数的素性
(define (search-for-primes lower upper)
  (define (iter n lower upper)
    (if (odd? n)
        (timed-prime-test n))
    (if (and (>= n lower) (< n upper))
        (iter (+ n 1) lower upper)))
  (iter lower lower upper))

;; tests
(search-for-primes 1000 1019)
(search-for-primes 10000 10037)
(search-for-primes 100000 100043)
(search-for-primes 1000000 1000037)
;; 由于运行速度太快很难得出0毫秒以上的结果，下面测试更大的数
(search-for-primes 1000000000 1000000021)
(search-for-primes 10000000000 10000000061)
(search-for-primes 100000000000 100000000057)
(search-for-primes 1000000000000 1000000000063)
;; 测试数据如下
;1000000007 *** 2992
;1000000009 *** 2991
;1000000021 *** 2992
;10000000019 *** 8975
;10000000033 *** 8976
;10000000061 *** 8973
;100000000003 *** 29920
;100000000019 *** 30907
;100000000057 *** 30916
;1000000000039 *** 96739
;1000000000061 *** 147115
;1000000000063 *** 122671
;; 可见，问题规模每增加 10 倍，计算耗时大致在 3 左右，大致符合 sqrt(10)
;; 这道题是为了测试，计算耗时和目前用于时间复杂度（使用计算步骤来估算）的模型是一致的。