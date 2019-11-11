#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))

;; 通过应用序展示 cdr 的正确性
;; (define c (cons 1 2))
;; (cdr c)
;; (cdr (cons 1 2))
;; cons 返回 procedure
;; (cdr [lambda (m) (m 1 2)])
;; ([lambda (m) (m 1 2)] [lambda (x y) y])
;; 第二个 procedure 作为实参传入第一个 procedure
;; ([lambda (x y) y] 1 2)
;; 2