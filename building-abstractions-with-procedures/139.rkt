#lang sicp

(define (cont-frac-rcsv n d k)
  (define (loop i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (loop (+ i 1))))))
  (loop 1))

;; tan(x),x 用弧度制表示
(define (tan-cf x k)
  (cont-frac-rcsv (lambda (i) (cond [(= i 1) x] [else (- (* x x))]))
                  (lambda (i) (- (* 2 i) 1))
                  k))

;; Tests
(= (tan-cf 1.0 10)
   (tan 1.0))