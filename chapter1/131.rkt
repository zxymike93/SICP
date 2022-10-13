#lang sicp

;; 参考累加，实现一个累乘算法
(define (product-rcsv term next a b)
  (if (> a b)
      1
      (* (term a)
         (product-rcsv term next (next a) b))))

(define (product-iter term next a b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n)
              (* result (term n)))))
  (iter a 1))

;; tests

;; factorial
(define (identify x)
  x)
(define (inc x)
  (+ x 1))
(product-rcsv identify inc 1 10)
(product-iter identify inc 1 10)

;; pi/4
(define (add-two x)
  (+ x 2))

(define (f x)
  (* (/ (- x 1) x)
     (/ (+ x 1) x)))

(* 4 (product-rcsv f add-two 3.0 7.0))
(* 4 (product-iter f add-two 3.0 7.0))