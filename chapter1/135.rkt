#lang sicp

(define (fixed-point f first-guess)

  (define (close-enough? a b)
    (> 0.00001 (abs (- a b))))

  (define (try guess)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

;; golden-ratio
(fixed-point
 (lambda (x) (+ 1 (/ 1 x)))
 0.0)