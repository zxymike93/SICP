#lang sicp

;; Sets as unordered lists

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2))
         '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2))]
        [else
         (intersection-set (cdr set1) set2)]))

(define (union-set set1 set2)
  (define (union set1 set2 set3)
    (cond [(or (null? set1) (null? set2))
           (append set2 set3)]
          [(not (element-of-set? (car set1) set2))
           (union (cdr set1) set2 (cons (car set1) set3))]
          [else
           (union (cdr set1) set2 set3)]))
  (union set1 set2 '()))

;; tests
(define uo1 (list 3 9 7 0 5))
(define uo2 (list 9 8 2 1))

(element-of-set? 8 uo1)
(element-of-set? 8 uo2)

(adjoin-set 4 uo1)

(intersection-set uo1 uo2)

(union-set uo1 uo2)