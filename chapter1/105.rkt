#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; 应用序因为总是要对参数求值，第二个参数 (p) 会被无限递归调用。
;; (test 0 (p)) => (test 0 (p)) => (test 0 (p))

;; 正则序直接将参数代入，由于 <if> 的缘故，<alternative> 部分不会被执行。
;; (test 0 (p)) => (if (= 0 0) 0 (p)) => (if true 0 (p)) => 0
