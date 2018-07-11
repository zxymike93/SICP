#lang sicp

(define (expt base n)
  (if (= n 0)
      1
      (* base (expt base (- n 1)))))

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

#|
(define (iter-helper pair n base)
  (if (= (remainder pair base) 0)
      (iter-helper (/ pair base) (+ n 1) base)
      n))

(define (car pair)
  (iter-helper pair 0 2))

(define (cdr pair)
  (iter-helper pair 0 3))
|#

; recursive version
(define (car pair)
  (if (= (remainder pair 2) 0)
      (+ 1 (car (/ pair 2)))
      0))

(define (cdr pair)
  (if (= (remainder pair 3) 0)
      (+ 1 (cdr (/ pair 3)))
      0))

; test
[define x (cons 2 3)]

(car x)
(cdr x)