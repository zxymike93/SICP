#lang sicp

(define make-interval cons)

(define upper-bound cdr)

(define lower-bound car)

(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (wid-interval x)
  (/ (- (upper-bound x) (lower-bound x))
     2))

;; 设有区间 a=[a1, a2] 区间 b=[b1, b2]
;; 根据区间加/减法的定义
;; a+b = [a1+b1, a2+b2]，其宽度 w(a+b) = [(a2+b2)-(a1+b1)]/2 = [(a2-a1)+(b2-b1)]/2 = w(a) + w(b)
;; 同理 w(a-b) = w(a) - w(b)
;; 所以两个区间的和/差的宽度，是关于两个区间的宽度的函数。 f(w(a),w(b)) -> w(a-b)

;; 对于区间的乘除运算
(define x1 (make-interval 4 5))
(define y1 (make-interval 1 2))
(define x2 (make-interval 0 1))
(define y2 (make-interval -2 -1))
;; 以上两组区间 x1 y1 以及 x2 y2，其 w(x1), w(y1) 和 w(x2), w(y2) 是一致的
(wid-interval x1)
(wid-interval y1)
(wid-interval x2)
(wid-interval y2)
;; 但 f(w(x1),w(y1)) 不等于 f(w(x2),w(y2))
(wid-interval (mul-interval x1 y1))
(wid-interval (mul-interval x2 y2))
(wid-interval (div-interval x1 y1))
(wid-interval (div-interval x2 y1))
;; 所以对于两个区间乘/除的宽度w(a*b) w(a/b)，并不是关于两个区间的宽度w(a),w(b)的函数。