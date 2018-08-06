#lang sicp

(define (tree-map proc tree)
  (map [lambda (subtree)
         (if (pair? subtree)
             (tree-map proc subtree)
             (proc subtree))]
       tree))

(define (square-tree tree)
  (define (square x) (* x x))

  (tree-map square tree))

(define (scale-tree factor tree)
  (tree-map [lambda (x) (* x factor)] tree))

; Test
[define t
  (list 1
        (list 2
              (list 3 4)
              5)
        (list 6 7))]

(square-tree t)
(scale-tree t 10)