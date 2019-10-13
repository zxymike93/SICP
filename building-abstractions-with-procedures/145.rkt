#lang sicp

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b))
       0.00001))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x))
                 2)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (loop g count)
    (if (= count 0)
        (lambda (x) (g x))
        (loop (compose f g) (- count 1))))  
  (loop (lambda (x) x) n))

(define (expt b n)
  (if (= n 0)
      1
      (* b
         (expt b (- n 1)))))

;; 1th: 0
;; 2th: 1
;; 3th: 1
;; 4~7th: 2
;; 8~15th: 3
;; 根据实验得出的规律，计算 n 次方根，需要 i 次，其中 i = log2n向下取整
(define (log2n-int n)
  (define (loop i)
    (if (> (expt 2 i) n)
        (- i 1)
        (loop (+ i 1))))
  (loop 0))

(define (nthrt x n)
  (fixed-point ((repeated average-damp (log2n-int n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; 这样求任意开方根都能收敛出答案
(nthrt 3849849017841 25)