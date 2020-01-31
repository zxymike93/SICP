#lang sicp

(define (make-record key value) (cons key value))
(define key car)
(define value cdr)

(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (adjoin-set x set)
  (cond ((null? set)
         (make-tree x '() '()))
        ((= (key x) (key (entry set)))
         (begin (set-cdr! (entry set) (value x))
                set))
        ((< (key x) (key (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (key x) (key (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (lookup k records)
  (cond [(null? records)
         false]
        [(= k (key (entry records)))
         (entry records)]
        [(< k (key (entry records)))
         (lookup k (left-branch records))]
        [else
         (lookup k (right-branch records))]))

(define (insert! key value table)
  (adjoin-set (make-record key value) table))

(define empty-table '())

;; 根节点为 7
(define a
  (insert!
   11 '11
   (insert!
    9 '9
    (insert!
     5 '5
     (insert!
      1 '1
      (insert!
       3 '3
       (insert!
        7 '7
        empty-table)))))))

(define b
  (insert!
   1 '1
   (insert!
    5 '5
    (insert!
     11 '11
     (insert!
      9 '9
      (insert!
       3 '3
       (insert!
        7 '7
        empty-table)))))))

(insert! 9 'nine a)
(insert! 7 'seven b)

;; 根节点为 3
(define c
  (insert!
   11 '11
   (insert!
    9 '9
    (insert!
     5 '5
     (insert!
      1 '1
      (insert!
       7 '7
       (insert!
        3 '3
        empty-table)))))))

(define d
  (insert!
   9 '9
   (insert!
    11 '11
    (insert!
     5 '5
     (insert!
      7 '7
      (insert!
       1 '1
       (insert!
        3 '3
        empty-table)))))))

c
d