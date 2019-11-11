#lang sicp

(define (f g)
  (g 2))

(f f)
;; 应用序
;; (f 2)
;; because 2 is not a procedure, an error occurs.