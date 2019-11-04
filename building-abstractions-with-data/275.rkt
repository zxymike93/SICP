#lang racket

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond [(eq? op 'real-part) x]
          [(eq? op 'imag-part) y]
          [(eq? op 'magnitude) (sqrt (+ (square x) (square y)))]
          [(eq? op 'angle) (atan y x)]
          [else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)]))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [else (error "Unkown op: MAKE-FROM-MAG-ANG" op)]))
  dispatch)

(define (apply-generic op obj) (obj op))

(define (real-part z) (apply-generic 'real-part z))

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

;; tests
(define c1 (make-from-real-imag 1 2))
(define c2 (make-from-real-imag 3 4))

;; 因为 constructor 返回一个过程，除非显式 apply 一些操作，否则终端只会输出 #procedure
(define r1 (add-complex c1 c2))
(real-part r1)
(imag-part r1)
(magnitude r1)
(angle r1)
(define r2 (mul-complex c1 c2))
(real-part r2)
(imag-part r2)
(magnitude r2)
(angle r2)