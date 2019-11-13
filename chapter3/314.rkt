#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
v
;; '(a b c d)
(define w (mystery v))
w
;; '(d c b a)
v
;; (loop (a b c d) ())
;; (let ((temp (b c d)))
;; (set-cdr! (a b c d) ())
;; (loop (b c d) (a))
;; '(a)