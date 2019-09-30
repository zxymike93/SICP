#lang sicp

(define (sum term next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))

;; tests
(sum (lambda (x) x)
     (lambda (x) (+ x 1))
     1
     100)