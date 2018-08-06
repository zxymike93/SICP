#lang sicp

#|
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
|#


; List
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map-scale-list items factor)
  (map [lambda (x) (* factor x)]
       items))

; Test
[define lst (list 1 2 3 4 5)]

(scale-list lst 10)
; (map-scale-list lst 10)


; Tree                       
(define (scale-tree tree factor)
  (cond ((null? tree)
         nil)
        ((not (pair? tree))
         (* factor tree))
        (else
         (cons (scale-tree (car tree) factor)
               (scale-tree (cdr tree) factor)))))

(define (map-scale-tree tree factor)
  (map [lambda (x) (if (not (pair? x))
                       (* factor x)
                       (map-scale-tree x factor))]
       tree))

; Test
[define t
  (list (list 10 (list (list 2 3)
                       (list 2 3)))
        (list 12 5))]

; t
(scale-tree t 2)
(map-scale-tree t 2)