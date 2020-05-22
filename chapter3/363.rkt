#lang sicp

(define (memorize proc)
  (let ([run? #f]
        [result #f])
    (lambda ()
      (if (not run?)
          (begin (set! result (proc))
                 (set! run? #t)
                 result)
          result))))

(define-syntax delay-stream
  (syntax-rules ()
    ;[(delay-stream s) (memorize (lambda () s))]))
    [(delay-stream s) (lambda () s)]))

(define (force-stream s)
  (s))

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream a b) (cons a (delay-stream b))]))

(define (car-stream s)
  (car s))

(define (cdr-stream s)
  (force-stream (cdr s)))

(define empty-stream '())

(define (null-stream? s)
  (null? s))

(define (map-stream f s)
  (if (null-stream? s)
      empty-stream
      (cons-stream (f (car-stream s))
                   (map-stream f (cdr-stream s)))))

(define (ref-stream s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (- n 1))))

;; 修改这里的内容来观察计算过程
(define counter 0)
(define (sqrt-improve guess x)
  (begin (set! counter (+ counter 1))
         (/ (+ guess (/ x guess)) 2)))

;; 这是书中给出的定义
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (map-stream (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)
;; 它返回的流（预期）是这样计算的
;; (1.0 (improve 1.0 (improve 1.5 (improve 1.416 ...
;; 得益于 memorize，原本 (improve 1.5 应该是 (improve (1.0 (improve 1.0
;; 但后面的过程被它自己局部的 memorize 记录下来了，在调用的时候直接返回了结果

;; 这是 LR 的定义
;(define (sqrt-stream x)
;  (cons-stream 1.0
;               (map-stream (lambda (guess) (sqrt-improve guess x))
;                           (sqrt-stream x))))
;; 它返回的流（预期）是这样计算的
;;(1.0 (improve 1.0                             ; 1.5
;;              (1.0 (improve (1.0 (improve ... ; 1.416

;; 我们来验证
(define S (sqrt-stream 2))
(ref-stream S 5)
counter
;; 使用定义 1：counter 的值为 5
;; 使用定义 2：counter 的值为 15

;; 但如果我们不使用 memorize，两种定义方式的复杂度是一样的。