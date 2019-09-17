#lang sicp

;; 唱零钱程序:对于总值为 amount 的钱，如果有 kinds-of-coins 种面值的硬币，求有多少种零钱唱法？
;; 使用分治的思路，将问题分为2块:
;; 唱法总数 = 
;;   1. （一定）使用最大面值的硬币来唱零钱
;;   2. 不使用最大面值的硬币来唱零钱
;; 循环使用这个过程，就可以将问题的规模缩小。
;; 另外就是 base case：
;;   1. 总额为0，唱法只有1种
;;   2. 总额小于0，唱法0种
;;   3. 零钱种类为0，唱法0种

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond [(= amount 0)
           1]
          [(or (< amount 0)
               (= kinds-of-coins 0))
           0]
          [else
           (+ (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
              (cc amount (- kinds-of-coins 1)))]))
  (cc amount 5))

;; Helper Function：设有5种硬币,分别是1、5、10、25、50元
;; 硬币种数 -> 最大面值
(define (first-denomination kinds-of-coins)
  (cond [(= kinds-of-coins 1) 1]
        [(= kinds-of-coins 2) 5]
        [(= kinds-of-coins 3) 10]
        [(= kinds-of-coins 4) 25]
        [(= kinds-of-coins 5) 50]))

;; 问：画出 (count-change 11) 估值过程的树图。 count-change 的时间、空间复杂度分别是什么？
(count-change 11)
;; 首先，估值过程可以视为一个不完全二叉树。
;; 参考课程讲解，以 (count-change 11) 为例，设计算这棵树的时间为 k，
;; 那么计算 (count-change 61) 必然会计算2倍（以上）规模的 (count-change 11)。
;; 粗略地认为，当 n 线性递增时，计算时间以 k 倍递增。
;; 因此，初略地估计：
;;   1. 时间复杂度 O(k^n)
;;   2. 空间复杂度 O(n)
;; 实际上，k 的值随 n 的值变化，更概括的说法——时间复杂度是指数型的。
;; 而最深的路径恰好因为 (cc n 1) 的存在（使用1元来兑换n元），空间复杂度正好是 n。