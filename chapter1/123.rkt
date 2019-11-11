#lang sicp

(define (odd? n)
  (= (remainder n 2) 1))

(define (divides? a b)
  (= (remainder a b) 0))

(define (square x)
  (* x x))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (find-divisor n d)
  (cond [(> (square d) n) n]
        [(divides? n d) d]
        [else (find-divisor n (next d))]))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      ))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

;; 根据22题的测试结果，选择下面几个测试用例
(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)
(timed-prime-test 10000000019)
(timed-prime-test 10000000033)
(timed-prime-test 10000000061)
(timed-prime-test 100000000003)
(timed-prime-test 100000000019)
(timed-prime-test 100000000057)
(timed-prime-test 1000000000039)
(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)

;; 测得数据如下
;1000000007 *** 2992
;1000000009 *** 1993
;1000000021 *** 1995
;10000000019 *** 6981
;10000000033 *** 6981
;10000000061 *** 5983
;100000000003 *** 21942
;100000000019 *** 24933
;100000000057 *** 19946
;1000000000039 *** 104720
;1000000000061 *** 59841
;1000000000063 *** 76795

;; 和题目22相比，耗时并不是 1:2，而是更接近于 2:3
;; 因为问题的规模虽然随 (+ d 1) => (+ d 2) 缩小了一半，但增加的 (= d 2) 也会耗时。