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


;;;;;;;; Segments & Points (from ex2-2)
(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (make-point x y) (cons x y))

(define (make-segment start end) (cons start end))

(define (start-segment line) (car line))

(define (end-segment line) (cdr line))

; Compute length of a segment
(define (seg-len line)
  (let ([a-coordinate (start-segment line)]
        [b-coordinate (end-segment line)])
    (let ([x1 (x-point a-coordinate)]
          [x2 (x-point b-coordinate)]
          [y1 (y-point a-coordinate)]
          [y2 (y-point b-coordinate)])
      (sqrt (+ (square (- x1 x2))
               (square (- y1 y2)))))))


;;;;;;;; Constructor & Selector A

; Rectangle ... baterrier a
;  |
; Segment ... baterrier b
;  |
; Point ... baterrier c

#|
(define (make-rectangle width length) (cons width length))

(define (width rect)
  (seg-len (car rect)))

(define (length rect)
  (seg-len (cdr rect)))
|#


;;;;;;;; Constuctors & Selectors B

; Rectangle ... baterrier a
;  |
; Pair ... baterrier b
;  |
; Segment ... baterrier c
;  |
; Point ... baterrier d

(define (make-rect wid-pair len-pair) (cons wid-pair len-pair))

(define (wid-pair rect) (car rect))

(define (len-pair rect) (cdr rect))

(define (make-pair l) (cons l l))

(define (seperate-pair pair) (car pair))

(define (width rect)
  (let ([wid (seperate-pair (wid-pair rect))])
    (seg-len wid)))

(define (length rect)
  (let ([len (seperate-pair (len-pair rect))])
    (seg-len len)))


;;;;;;;; Rectangle Instance A

[define pa (make-point 0 0)]
[define pb (make-point 1 0)]
[define pc (make-point 0 2)]

[define l1 (make-segment pa pb)]
[define l2 (make-segment pa pc)]

; [define rect-inst (make-rectangle l1 l2)]


;;;;;;;; Rectangle instance B

[define w-pair (make-pair l1)]
[define l-pair (make-pair l2)]

[define rect-inst (make-rect w-pair l-pair)]


; Talking about rectangle, there are two formula we already know...
; perimeter = 2 * (width + length)
; area = width * length

(define (perimeter rect)
  (* 2 (+ (width rect)
          (length rect))))

(define (area rect)
  (* (width rect)
     (length rect)))

; Print results...

(define (print-perimeter rect)
  (newline)
  (display "Perimeter of rect: ")
  (display (perimeter rect)))

(define (print-area rect)
  (newline)
  (display "Area of rect: ")
  (display (area rect)))


(print-perimeter rect-inst)
(print-area rect-inst)