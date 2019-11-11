#lang sicp

(define (sum-y term next a b n)
  (define (iter x k)
    (cond [(> x b)
           0]
          [(or (= k 0) (= k n))
           (+ (term x)
              (iter (next x) (+ k 1)))]
          [(even? k)
           (+ (* 4 (term x))
              (iter (next x) (+ k 1)))]
          [else
           (+ (* 2 (term x))
              (iter (next x) (+ k 1)))]))

  (iter a 0))

(define (integral f a b n)
  
  (define h (/ (- b a) n))

  (define (add-h x)
    (+ x h))

  (* (/ h 3)
     (sum-y f add-h a b n)))


;; tests
(define (cube x)
  (* x x x))

(integral cube .0 1 100)
(integral cube .0 1 1000)