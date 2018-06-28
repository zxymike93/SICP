#lang sicp

(define (cont-frac n d k)
    (define (cf-iter i result)
    (if (= i 1)
        result
        (cf-iter (- i 1)
                 (/ (n (- i 1))
                    (+ (d (- i 1)) result)))))
    (cf-iter k (/ (n k) (d k)))
)

(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (D i)
    (- (* i 2.0) 1))

  (cont-frac N D k)
)

; Compare with built-in `tan`
(tan-cf 10 100)
(tan 10)