#lang sicp

(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

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

(define (list->tree elts)
  (car (partial-tree elts (length elts))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let
          ([left-size (quotient (- n 1) 2)])
        (let
            ([left-result (partial-tree elts left-size)])
          (let
              ([left-tree (car left-result)]
               [non-left-elts (cdr left-result)]
               [right-size (- n (+ left-size 1))])
            (let
                ([this-entry (car non-left-elts)]
                 [right-result (partial-tree (cdr non-left-elts) right-size)])
              (let
                  ([right-tree (car right-result)]
                   [remaining-elts (cdr right-result)])
                (cons
                 (make-tree this-entry left-tree right-tree)
                 remaining-elts))))))))

;; 以图2-17为例， 可以看到
;; tree->list 会生成一个无序列表
;; list->tree 会生成一个平衡二叉树
(define a (make-tree 3 nil (make-tree 1 nil (make-tree 2 nil nil))))
;(tree->list a)
;(list->tree (tree->list a))
(define b (make-tree 1 nil (make-tree 2 nil (make-tree 3 nil (make-tree 4 nil (make-tree 5 nil (make-tree 6 nil (make-tree 7 nil nil))))))))
;(tree->list b)
;(list->tree (tree->list b))

;; 引入 unordered-list 的操作
(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (intersection set1 set2)
  (cond [(or (null? set1) (null? set2))
         '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2))]
        [else
         (intersection (cdr set1) set2)]))

(define (union set1 set2)
  (define (un set1 set2 set3)
    (cond [(or (null? set1) (null? set2))
           (append set2 set3)]
          [(not (element-of-set? (car set1) set2))
           (un (cdr set1) set2 (cons (car set1) set3))]
          [else
           (un (cdr set1) set2 set3)]))
  (un set1 set2 '()))

;; 对于用 binary-tree 来表示的 sets
;; 使用63/64习题的转换过程确保它们 balanced

(define (intersection-set set1 set2)
  (list->tree (intersection (tree->list set1) (tree->list set2))))

(define (union-set set1 set2)
  (list->tree (union (tree->list set1) (tree->list set2))))

;; tests
(intersection-set a b)
(union-set a b)