#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (tree-to-list tree)
  (cond ((null? tree)
         nil)
        ((pair? tree)
         (append (tree-to-list (car tree))
                 (tree-to-list (cdr tree))))
        (else
         (list tree))))

(define (count-leaves tree)
  (accumulate [lambda (x y) (+ 1 y)]
              0
              (tree-to-list tree)))

; Test
[define t (list (list 1 2)
                (list 3 (list 4 5)))]

(tree-to-list t)
(count-leaves t)