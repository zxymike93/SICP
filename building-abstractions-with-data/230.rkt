#lang sicp

(define (square-tree-v1 tree)
  (let ([square
         [lambda (x) (* x x)]])
    (cond ((null? tree)
           nil)
          ((not (pair? tree))
           (square tree))
          (else
           (cons (square-tree-v1 (car tree))
                 (square-tree-v2 (cdr tree)))))))

(define (square-tree-v2 tree)
  (map [lambda (x) (if (pair? x)
                       (square-tree-v2 x)
                       (* x x))]
       tree))

; Test
[define t
  (list 1
        (list 2
              (list 3 4)
              5)
        (list 6 7))]

(square-tree-v1 t)
(square-tree-v2 t)