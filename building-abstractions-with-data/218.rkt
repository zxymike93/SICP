#lang sicp

;; (reverse (list 1 4 9 16 25)) -> (25 16 9 4 1)
(define (reverse l)
  (define (loop old new)
    (if (null? old)
        new
        (loop (cdr old)
              (cons (car old) new))))
  (loop l nil))

;; test
(reverse (list 1 4 9 16 25))