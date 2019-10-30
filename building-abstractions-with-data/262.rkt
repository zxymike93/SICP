#lang sicp

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond [(= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2)))]
              [(< x1 x2)
               (intersection-set (cdr set1) set2)]
              [(> x1 x2)
               (intersection-set set1 (cdr set2))]))))

(define (union-set set1 set2)
  (cond [(null? set1)
         set2]
        [(null? set2)
         set1]
        [else
         (let ([x1 (car set1)]
               [x2 (car set2)])
           (cond [(= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2)))]
                 [(< x1 x2)
                  (cons x1 (union-set (cdr set1) set2))]
                 [(> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))]))]))

;; tests
(define s1 (list 2 3 6 10))
(define s2 (list 3 7 10 11))

(intersection-set s1 s2)
(union-set s1 s2)