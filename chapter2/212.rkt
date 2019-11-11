#lang sicp

;; interval: cons
(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]")
  (newline))

;; num num -> interval
(define (make-center-percent c p)
  (let ([width (* c p)])
    (make-interval (- c width) (+ c width))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2))

(define (percent i)
  (abs (/ (/ (- (lower-bound i) (upper-bound i))
             2)
          (center i))))

;; test
(define a (make-center-percent 3.5 0.1))
(print-interval a)
(center a)
(percent a)