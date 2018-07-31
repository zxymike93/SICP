#lang sicp

(define (last-pair items)
  (cond ((null? items)
         (error "The given list is empty."))
        ((= (length items) 1)
         items)
        (else
         (last-pair (cdr items)))))

; Test
[define l (list 1 2 3 4)]
[define empty '()]

(last-pair l)
(last-pair empty)