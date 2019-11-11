#lang sicp

;; (last-pair (list 23 72 149 34)) -> (34)
(define (last-pair l)
  (define (loop last-item l)
    (if (null? l)
        (list last-item)
        (loop (car l) (cdr l))))
  (loop (car l) (cdr l)))

;; test
(last-pair (list 23 72 149 34))