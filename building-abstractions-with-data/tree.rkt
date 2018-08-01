#lang sicp

[define tre (cons (list 1 2) (list 3 4))]

(define (length x)
  (if (null? x)
      0
      (+ (length (cdr x))
         1)))

(define (count-leaf x)
  (cond ((null? x)
         0)
        ((not (pair? x))
         1)
        (else
         (+ (count-leaf (car x))
            (count-leaf (cdr x))))))


(length tre)
(length (cons tre tre))
(count-leaf tre)
(count-leaf (cons tre tre))