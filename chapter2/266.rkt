#lang sicp

(define (make-record key value) (cons key value))
(define key car)
(define value cdr)

(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (key x) (key (entry set))) set)
        ((< (key x) (key (entry set)))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> (key x) (key (entry set)))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

;; looking up binary tree like this
;;     (k, v)
;;     /    \
;;  (k, v) (k, v)
(define (lookup k records)
  (cond [(null? records)
         false]
        [(= k (key (entry records)))
         (entry records)]
        [(< k (key (entry records)))
         (lookup k (left-branch records))]
        [else
         (lookup k (right-branch records))]))

;; tests
(define empty-set '())
(define t
  (adjoin-set '(7 seven)
              (adjoin-set '(3 three)
                          (adjoin-set '(5 five)
                                      (adjoin-set '(9 nine)
                                                  (adjoin-set '(1 one)
                                                              empty-set))))))
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(tree->list t)
(lookup -1 t)
(lookup 1 t)
(lookup 3 t)
(lookup 4 t)
(lookup 5 t)
(lookup 7 t)
(lookup 9 t)
(lookup 11 t)