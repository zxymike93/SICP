#lang sicp

(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -)
   a
   b))

;; operator / operand 都可以使用 combination
;; (<operator> a b) 中的 <operator> 是 (if (> b 0) + -)
;; evaluate 的过程如下：

(a-plus-abs-b 1 2)
;; (a-plus-abs-b 1 2)
;; ((if (> 2 0) + -) 1 2)
;; ((if #t + -) 1 2)
;; (+ 1 2)
;; 3

(a-plus-abs-b 1 -2)
;; (a-plus-abs-b 1 -2)
;; ((if (> -2 0) + -) 1 -2)
;; ((if #f + -) 1 -2)
;; (- 1 -2)
;; 3