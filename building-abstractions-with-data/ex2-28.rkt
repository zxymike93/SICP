#lang sicp

(define (fringe x)
  (cond ((null? x)
         nil)
        ((not (pair? x))
         (list x))
        (else
         (append (fringe (car x))
                 (fringe (cdr x))))))

   
(define tree (list (list 1 2) (list 3 4)))

(fringe tree)
(fringe (list tree tree))