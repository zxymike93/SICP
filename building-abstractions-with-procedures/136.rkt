#lang sicp

(define (fixed-point f first-guess)

  (define (close-enough? a b)
    (> 0.00001 (abs (- a b))))

  (define (try guess)
    (let [(next (f guess))]
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

;; without average-damping
;; takes 37 steps
(fixed-point
 (lambda (x) (/ (log 1000)
                (log x)))
 1.1)

;; using average-damping
(define (average a b)
  (/ (+ a b) 2))
;; takes 14 steps
(fixed-point
 (lambda (x) (average x
                      (/ (log 1000) (log x))))
 1.1)