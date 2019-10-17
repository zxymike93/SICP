#lang sicp

(define (filter predicate items)
  (if (null? items)
      nil
      (if (predicate (car items))
          (cons (car items) (filter predicate (cdr items)))
          (filter predicate (cdr items)))))

(define (same-parity . l)
  (if (odd? (car l))
      (filter odd? l)
      (filter even? l)))

;; tests
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)