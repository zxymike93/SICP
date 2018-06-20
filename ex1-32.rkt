#lang sicp

; We have `sum` and `product`
; and we can abstract to a higher level
; named `accumulate` using their common pattern

(define (accumulate combiner null-value term lower next upper)
  (if (> lower upper)
      null-value
      (combiner (term lower)
                (accumulate
                 combiner null-value term (next lower) next upper))))


; The iterative version
(define (iacml combiner null-value term lower next upper)
  (define (iter lower result)
    (if (> lower upper)
        result
        (iter (next lower)
              (combiner (term lower) result))))
  (iter lower null-value))

; Re-define `sum` and `product` using this higher-order procedure
(define (sum term a next b)
  (accumulate + 0 term a next b))
  ; (iacml + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


; Tests
(define (identify x) x)

(define (inc x) (+ x 1))

(define (gs n)
  (sum identify 0 inc n))

(define (factorial n)
  (product identify 1 inc n))


(gs 10)
(factorial 5)