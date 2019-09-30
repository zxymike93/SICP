#lang sicp

(define (accumulate-rcsv combiner null-value term next a b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rcsv combiner null-value term next (next a) b))))

(define (accumulate-iter combiner null-value term next a b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i)
              (combiner result (term i)))))
  (iter a null-value))

;; tests
(define (identify x)
  x)

(define (inc x)
  (+ x 1))

;; recursive
(define (sum-rcsv term next a b)
  (accumulate-rcsv + 0 term next a b))

(define (product-rcsv term next a b)
  (accumulate-rcsv * 1 term next a b))

(sum-rcsv identify inc 1 10)
(product-rcsv identify inc 1 10)

;; iterative
(define (sum-iter term next a b)
  (accumulate-iter + 0 term next a b))

(define (product-iter term next a b)
  (accumulate-iter * 1 term next a b))

(sum-iter identify inc 1 10)
(product-iter identify inc 1 10)