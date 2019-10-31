#lang sicp

(define (make-tree entry left right)
  (list entry left right))

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

(list->tree elements)
(car (partial-tree elements elements-size))
... into partial-tree ...
left-size: (half elements-size) ; divide into two parts without entry
left-result: (partial-tree elts left-size) ; left side recurse
left-tree: (car left-result) ; car of left side recurse
non-left-elts: (cdr left-result) ; cdr of left side recurse (the rest)
right-size: (- n (+ left-size 1)) ; n - left-size - entry
this-entry: (car non-left-elts) ; car of the rest elements
right-result: (partial-tree (cdr non-left-elts) right-size) ; right side recurse
right-tree: (car right-result) ; car of right side recurse
remaining-elts: (cdr right-result) ; cdr of right side recurse
(partial)recurse: (tree elts) ; tree, and, remaining elements


;; 对于 list (1 3 5 7 9 11)
(define x '(1 3 5 7 9 11))
(list->tree x)
;; 结果
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;; 树状图
;;    5
;;  /   \
;; 1     9
;;  \   / \
;;  3  7  11