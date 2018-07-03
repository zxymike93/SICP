#lang sicp

; helpers
(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)

  (define (good_enough? guess x)
    (> 0.001 (abs (- x (square guess)))))

  (define (improve guess x) (average guess (/ x guess)))

  (define (guess_loop guess x)
    (if (good_enough? guess x) guess
        (guess_loop (improve guess x) x)))

  (guess_loop 1.0 x)
)


; rectangle libs
(define (make-rectangle a-seg b-seg) (cons a-seg b-seg))

(define (a-segment rect) (car rect))

(define (b-segment rect) (cdr rect))

(define (third-leg rect)
  (let ([Lab (a-segment rect)]
        [Lbc (b-segment rect)])
    (let ([Pa (start-segment Lab)]
          [Pb (end-segment Lab)]
          [Pc (end-segment Lbc)])
      (make-segment Pc Pa)))
)


; segment libs
(define (make-segment start end) (cons start end))

(define (start-segment line) (car line))

(define (end-segment line) (cdr line))

(define (length seg)

  (let ([start (start-segment seg)]
        [end (end-segment seg)])

    (let ([x1 (x-point start)]
          [y1 (y-point start)]
          [x2 (x-point end)]
          [y2 (y-point end)])

      (sqrt (+ (square (- x1 x2))
               (square (- y1 y2))))))
)


; point libs
(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (make-point x y) (cons x y))


; perimeter
(define (perimeter rect)
  (+ (length (a-segment rect))
     (length (b-segment rect))
     (length (third-leg rect))))


; Rectangle Instance
(define Pa (make-point 0 0))
(define Pb (make-point 1 1))
(define Pc (make-point 0 2))

(define Lab (make-segment Pa Pb))
(define Lbc (make-segment Pb Pc))

(define Rect (make-rectangle Lab Lbc))


; test
(define (print-perimeter rect)
  (newline)
  (display "Perimeter of rect:")
  (display (perimeter rect)))

(print-perimeter Rect)