#lang sicp

; 要找零总额 amount 和 n 种硬币，求有多少种找零的方式？
; Refer to `count-change.scm`
;   这里使用 list 来取代自己造的 "字典"

(define (no-more? cs) (null? cs))

(define (except-first-deomination cs) (cdr cs))

(define (first-deomination cs) (car cs))

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc amount (except-first-deomination coin-values))
            (cc (- amount (first-deomination coin-values)) coin-values)))))

; Test
[define us-coins (list 50 25 10 5 1)]
[define uk-coins (list 100 50 20 10 5 2 1 0.5)]

(cc 100 us-coins)
; 292
(cc 100 uk-coins)
; 104561

(cc 100 (list 25 50 10 5 1))
(cc 100 (list 25 5 1 50 10))
; 由此可见，改变列表的先后顺序并不会改变结果