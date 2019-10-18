#lang sicp

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)
;; (append (1 2 3 .) (4 5 6 .)
;; (1 2 3 4 5 6 .)

(cons x y)
;; (cons (1 2 3 .) (4 5 6 .))
;; ((1 2 3 .) 4 5 6 .)

(list x y)
;; (list (1 2 3 .) (4 5 6 .))
;; ((1 2 3) (4 5 6) .)