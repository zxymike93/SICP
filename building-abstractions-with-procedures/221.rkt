#lang sicp

(define square
  (lambda (x) (* x x)))

(define (square-list-v1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-v1 (cdr items)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list-v2 items)
  (map square items))

;; tests
(square-list-v1 (list 1 2 3 4))
(square-list-v2 (list 1 2 3 4))