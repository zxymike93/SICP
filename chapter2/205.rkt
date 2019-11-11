#lang sicp

;; 证明可以只用数和算数运算2种基本元素，以 2^a * 3^b 为表现形式，即可表示任意 pair (a, b)。
;; 即实现其 cons, car, cdr 过程

;; helper functions
(define (expt b e)
  (if (= e 0)
      1
      (* b (expt b (- e 1)))))

(define (divides? a b)
  (= (remainder a b) 0))

;; constructor
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; selectors: iterative version
;(define (iter pair n base)
;  (if (= (remainder pair base) 0)
;      (iter (/ pair base) (+ n 1) base)
;      n))
;
;(define (car p)
;  (iter p 0 2))
;
;(define (cdr p)
;  (iter p 0 3))

;; selectors: recursive version
(define (rcsv p base)
  (if (divides? p base)
      (+ 1 (car (/ p base)))
      0))

(define (car p)
  (rcsv p 2))

(define (cdr p)
  (rcsv p 3))

;; tests
(car (cons 0 0))
(cdr (cons 0 0))

(car (cons 1 0))
(cdr (cons 0 1))

(car (cons 99 100))
(cdr (cons 99 100))

