#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-circle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-circle (list 'a 'b 'c)))

;; tests
z
;; 在 racket repl 里，读出来是 #0=(a b c . #0#)
(last-pair z)
;; 会陷入死循环，因为为指针指向头指针