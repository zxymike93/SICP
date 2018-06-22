#lang sicp

(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i)
           (+ (d i) (frac (+ i 1))))))
  (frac 1))

(define (e k)
  (define (d i)
    (if (< i 3)
        i
        (if (= (remainder (- i 2) 3) 0)
            (* 2 (/ (+ i 1) 3))
            1)))
  (+ 2
     (cont-frac (lambda (i) 1.0)
                d
                k)))

(e 100)