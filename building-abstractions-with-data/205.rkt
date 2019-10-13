#lang sicp

;; 证明可以只用数和算数运算2种基本元素，以 2^a * 3^b 为表现形式，即可表示任意 pair (a, b)。（即实现其 cons, car, cdr 过程）
(define (expt b e)
  (if (= e 0)
      1
      (* b (expt b (- e 1)))))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (divides? a b)
  (= (remainder a b) 0))

(define (car p)
  (define (count p n)
    (if (divides? p 2)
        (count (/ p 2) (+ n 1))
        n))
  (count p 0))

(define (cdr p)
  (define (count p n)
    (if (divides? p 3)
        (count (/ p 3) (+ n 1))
        n))
  (count p 0))

;; tests
(car (cons 0 0))
(cdr (cons 0 0))

(car (cons 1 0))
(cdr (cons 0 1))

(car (cons 99 100))
(cdr (cons 99 100))

