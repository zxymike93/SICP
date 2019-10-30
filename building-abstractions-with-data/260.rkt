#lang sicp

;; 可重复的 "set"
;; {2 3 2 1 3 2 2}
;; 比较像 Python 的 list，适用于 队列、栈 可用的场景

;; 和 set 实现相同、效率相同
(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

;; 时间复杂度：O(n^2)->O(1)
(define (adjoin-set x set)
  (cons x set))

;; 和 set 实现相同、效率相同
(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2))
         '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2))]
        [else
         (intersection-set (cdr set1) set2)]))

;; 时间复杂度：O(n^2)->O(1)
(define (union-set set1 set2)
  (append set1 set2))

;; tests
(define uo1 (list 3 2 7 1 1 0 5))
(define uo2 (list 2 3 2 1 3 2 2))

(element-of-set? 0 uo1)
(element-of-set? 0 uo2)

(adjoin-set 4 uo1)
(adjoin-set 4 uo2)

(intersection-set uo1 uo2)

(union-set uo1 uo2)