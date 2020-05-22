#lang sicp

(define (memorize proc)
  (let ([run? #f] [result #f])
    (lambda ()
      (if (not run?)
          (begin (set! result (proc))
                 (set! run? #t)
                 result)
          result))))

;; promise
(define-syntax delay-stream
  (syntax-rules ()
    [(delay-stream s) (memorize (lambda () s))]))

(define (force-stream s) (s))

;; stm
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream a b) (cons a (delay-stream b))]))

(define (car-stream s) (car s))

(define (cdr-stream s) (force-stream (cdr s)))

(define (cadr-stream s) (car-stream (cdr-stream s)))

(define empty-stream '())

(define (null-stream? s) (null? s))

(define (map-stream f s)
  (if (null-stream? s)
      empty-stream
      (cons-stream (f (car-stream s))
                   (map-stream f (cdr-stream s)))))

;; num -> stm
(define (sqrt-stream x)
  (define (sqrt-improve guess x)
    (/ (+ guess (/ x guess)) 2))
  (define guesses
    (cons-stream 1.0
                 (map-stream (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

;; 遍历一个流并比较相邻的两个元素之间的差，将差小于 tolerance 的后一个元素返回
;; stm num -> num
(define (limit-stream s tolerance)
  (if (> tolerance (abs (- (car-stream s) (cadr-stream s))))
      (cadr-stream s)
      (limit-stream (cdr-stream s) tolerance)))

;; 求 x 的平方根，允许误差为 tolerance
;; num num -> num
(define (sqrt x tolerance)
  (limit-stream (sqrt-stream x) tolerance))

;; tests
(sqrt 2 0.01)
(sqrt 2 0.001)