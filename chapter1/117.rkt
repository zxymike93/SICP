#lang sicp

;;(define (* a b)
;;  (if (= b 0)
;;      0
;;      (+ a (* a (- b 1)))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

;; 假设只有 + double halve，写一个类似于 fast-expt 的 log 复杂度的过程。

(define (* a b)
  (cond [(= b 0) 0]
        [(even? b) (* (double a) (halve b))]
        [else (+ a (* a (- b 1)))]))

;; tests
(* 2 0)
(* 2 3)
(* 0 10)