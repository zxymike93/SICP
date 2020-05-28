#lang sicp

;; p 是接收一个参数的过程，如果 (p a) 有返回值，则称为 halt
;; 假设要实现一个过程 halts?，它判断 (p a) 是否有返回值（是否 halt）
;; 请（基于下面两个过程）说明，halts? 不可能被实现。

;; 一个无限循环（违反 halt）
(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;; 如果执行 (try try) （为了便于区分实参 try 形式化地改为 try-p，try-p2 ...）
(try try-p)
(if (halts? try-p try-p) ..)
(if (halts? (if (halts? try-p2 try-p2) ..)) ..)
;; 假设 halts? 是可以实现的，那么（比如上面的 try-p2 就会判断为真，继而）执行下面的 (run-forever)，
;; 也就是说在上一级 (halts? try try) 看来，这个 predicate 是假（返回 'halted）。
;; 这种说法反过来也是一样的，所以无论假定 (halts? try try) 为真或者为假，执行结果都会有悖假设。