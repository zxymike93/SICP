#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))

(define (greater-than-zero x)
  (> (lower-bound x) 0))

(define (less-than-zero x)
  (< (upper-bound x) 0))

;; 如果通过测试 x y 的区间范围，将原来 mul-interval 分为9种情况
;; 只有（x和y均为跨0区间）一种情况下需要多于1次 * 运算
(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (cond [(greater-than-zero x)
           (cond [(greater-than-zero y) (make-interval p1 p4)]
                 [(less-than-zero y) (make-interval p3 p2)]
                 [else (make-interval p3 p4)])]
          [(less-than-zero x)
           (cond [(greater-than-zero y) (make-interval p2 p3)]
                 [(less-than-zero y) (make-interval p4 p1)]
                 [else (make-interval p2 p1)])]
          [else
           (cond [(greater-than-zero y) (make-interval p2 p4)]
                 [(less-than-zero y) (make-interval p3 p1)]
                 [else (make-interval (min p2 p3) (max p1 p4))])])))

;; tests
(define a (make-interval 4 7))
(define b (make-interval -5 8))
(define c (make-interval -3 -1))

(print-interval (mul-interval a a))
(print-interval (mul-interval a b))
(print-interval (mul-interval a c))

(print-interval (mul-interval b a))
(print-interval (mul-interval b b))
(print-interval (mul-interval b c))

(print-interval (mul-interval c a))
(print-interval (mul-interval c b))
(print-interval (mul-interval c c))

;[16,49]
;[-35,56]
;[-21,-4]
;[-35,56]
;[-40,64]
;[-24,15]
;[-21,-4]
;[-24,15]
;[1,9]
