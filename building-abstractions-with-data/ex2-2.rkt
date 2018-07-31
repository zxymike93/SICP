#lang sicp


(define (make-segment start end) (cons start end))

(define (start-segment line) (car line))

(define (end-segment line) (cdr line))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (make-point x y) (cons x y))

(define (midpoint-segment line)
  (let ([start (start-segment line)]
        [end (end-segment line)])
    (let ([x1 (x-point start)]
          [y1 (y-point start)]
          [x2 (x-point end)]
          [y2 (y-point end)])
      (make-point (/ (+ x1 x2) 2)
                  (/ (+ y1 y2) 2)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


; test
(define line (make-segment (make-point 2 3)
                           (make-point 10 15)))


(print-point (midpoint-segment line))