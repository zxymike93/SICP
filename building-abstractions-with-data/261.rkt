#lang sicp

;; Sets as ordered lists

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(= x (car set)) true]
        [(< x (car set)) false]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (define (adjoin x set1 set2)
    (cond [(null? set1)
           (append set2 (list x))]
          [(= x (car set1))
           (append set2 set1)]
          [(> x (car set1))
           (adjoin x (cdr set1) (append set2 (list (car set1))))]
          [else
           (append set2 (list x) set1)]))
  (adjoin x set '()))

;; tests
(define s (list 2 3 6 10))

(element-of-set? 10 s)
(element-of-set? 4 s)

(adjoin-set 1 s)
(adjoin-set 4 s)
(adjoin-set 11 s)
(adjoin-set 3 s)