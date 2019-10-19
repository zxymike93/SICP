#lang sicp

;; point
(define (make-point x y)
  (cons x y))

(define (x-coor p)
  (car p))

(define (y-coor p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-coor p))
  (display ",")
  (display (y-coor p))
  (display ")"))

;; segment
(define (make-segment start end)
  (cons start end))

(define (start-point s)
  (car s))

(define (end-point s)
  (cdr s))

;; rectangle v1
;; 使用 长、宽 表示 矩形
;(define (make-rectangle h w)
;  (cons h w))
;
;(define (height r)
;  (car r))
;
;(define (width r)
;  (cdr r))

;; rectangle v2
;; 使用 左上角、右下角 两个点来表示 矩形
;; 这样的矩形实际上水平与 x轴
(define (make-rectangle tl dr)
  (let ([dl (make-point (x-coor tl) (y-coor dr))]
        [tr (make-point (x-coor dr) (y-coor tl))])
    (cons (make-segment tl dl)
          (make-segment dl dr))))

(define (top-left r)
  (car (car r)))

(define (down-right r)
  (cdr (cdr r)))

(define (height r)
  (- (y-coor (top-left r))
     (y-coor (down-right r))))

(define (width r)
  (- (x-coor (down-right r))
     (x-coor (top-left r))))

;; answer
(define (perimeter r)
  (+ (* 2 (height r))
     (* 2 (width r))))

(define (area r)
  (* (height r) (width r)))

;; tests
(define r (make-rectangle (make-point 1 2)
                          (make-point 2 0)))
(perimeter r)
(area r)