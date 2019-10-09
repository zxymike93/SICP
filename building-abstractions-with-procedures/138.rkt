#lang sicp

(define (cont-frac n d k)
  (define (loop i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (loop (+ i 1))))))
  (loop 1))

;; 求自然数 e
;; 其中 N 恒等于 1，D 是序列 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8...
(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (cond [(= i 1)
                    1.0]
                   [(= i 2)
                    2.0]
                   [(= (remainder (- i 2) 3) 0)
                    (* 2.0 (/ (+ i 1) 3))]
                   [else
                    1.0]))
           10)