#lang racket

;; 使用函数式风格，用 list 来表示区间
(define (enumerate-interval n m)
  (if (> n m)
      '()
      (cons n
            (enumerate-interval (+ n 1) m))))

(define (filter pred items)
  (cond [(null? items)
         '()]
        [(pred (car items))
         (cons (car items) (filter pred (cdr items)))]
        [else
         (filter pred (cdr items))]))

;; 那么当区间很大时，时间和空间复杂度都会很高
;; 比如我们只需要求其中第二个奇数，要浪费很多不必要的空间
;(car (cdr (filter odd? (enumerate-interval 10000 1000000))))

;; 约定
;; (car-stream (cons-stream a b)) => a
;; (cdr-stream (cons-stream a b)) => b
;; empty-stream

;; 基于这个约定，可以实现一些通用过程

(define (null-stream? s)
  (null? s))

(define (ref-stream s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (- n 1))))

(define (map-stream s f)
  (if (null-stream? s)
      empty-stream
      (cons-stream (f (car-stream s))
                   (map-stream (cdr-stream s) f))))

(define (for-each-stream s f)
  (if (null-stream? s)
      'done
      (begin (f (car-stream s))
             (for-each-stream (cdr-stream s) f))))

(define (filter-stream s p?)
  (cond [(null-stream? s)
         empty-stream]
        [(p? (car-stream s))
         (cons-stream (car-stream s)
                      (filter-stream (cdr-stream s) p?))]
        [else
         (filter-stream (cdr-stream s) p?)]))

(define (enumerate-interval-stream n m)
  (if (> n m)
      empty-stream
      (cons-stream n
                   (enumerate-interval-stream (+ n 1) m))))

;; stream 的实现
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream a b) (cons a (delay-stream b))]))

(define (car-stream s)
  (car s))

(define (cdr-stream s)
  (force-stream (cdr s)))

(define empty-stream '())

;; delay / force 的实现
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
    [(delay-stream s) (memorize (lambda () s))]))

(define (force-stream s)
  (s))

(car-stream (cdr-stream (enumerate-interval-stream 10000 1000000)))
;(car-stream (cdr-stream (filter-stream (enumerate-interval-stream 10000 1000000) odd?)))
;(enumerate-interval-stream 1 20)
;(cons-stream 1 (enumerate-interval-stream 2 20))
;(cons 1 (delay-stream (enumerate-interval-stream 2 20)))
;(cons 1 (lambda () (enumerate-interval-stream 2 20)))