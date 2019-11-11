#lang sicp

(define (make-tree entry left right)
  (list entry left right))

(define entry car)

(define left-branch cadr)

(define right-branch caddr)

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
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

;; 1、两个过程产生的 list 是一样的，因为它们都采用中序遍历：先访问 left-branch，再访问 root，最后访问 right-branch
;; 相应的作用于图2.16的三棵树，结果如下（注意：使用了 list 的两种不同写法）：
(define tree1 (list 7
                    (list 3 (list 1 nil nil) (list 5 nil nil))
                    (list 9 nil (list 11 nil nil))))
(define tree2 '(3
                (1 () ())
                (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5
                (3 (1 () ()) ())
                (9 (7 () ()) (11 () ()))))

(tree->list-1 tree1)
(tree->list-2 tree1)

(tree->list-1 tree2)
(tree->list-2 tree2)

(tree->list-1 tree3)
(tree->list-2 tree3)