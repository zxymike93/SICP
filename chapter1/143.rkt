#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  
  (define (loop g count)
    (if (= count 0)
        (lambda (x) (g x))
        (loop (compose f g) (- count 1))))
  
  (loop (lambda (x) x) n))

;; tests
(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(= ((repeated inc 5) 0)
   5)

(= ((repeated square 2) 5)
   625)

(= ((repeated cube 0) 2)
   2)