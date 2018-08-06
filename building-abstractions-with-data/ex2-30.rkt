#lang sicp

(define (square-tree-raw tree)
  (let ([square
         [lambda (x) (* x x)]])
    (cond ((null? tree)
           nil)
          ((not (pair? tree))
           (square tree))
          (else
           (cons (square-tree-raw (car tree))
                 (square-tree-raw (cdr tree)))))))

(define (square-tree tree)
  (map [lambda (x) (if (pair? x)
                       (square-tree x)
                       (* x x))]
       tree))

; Test
[define t
  (list 1
        (list 2
              (list 3 4)
              5)
        (list 6 7))]

(square-tree-raw t)
(square-tree t)