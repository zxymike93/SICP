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
    [(delay-stream s) (memorize (lambda () s))]))

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

(define (ref-stream s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (- n 1))))

(define (map-stream proc . argstreams)
  (if (null-stream? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map car-stream argstreams))
       (apply map-stream
              (cons proc (map cdr-stream argstreams))))))

(define (add-stream s1 s2)
  (map-stream + s1 s2))

(define (mul-stream s1 s2)
  (map-stream * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-stream ones integers)))

;; (ref-stream factorials n) = 1 * 2 * ... * (n + 1)
;; (define factorials (cons-stream 1 (mul-stream factorials (add-stream ones integers))))
;; 或者
(define factorials (cons-stream 1 (mul-stream factorials (cdr-stream integers))))

;; tests
(ref-stream factorials 0)
(ref-stream factorials 1)
(ref-stream factorials 2)
(ref-stream factorials 3)
(ref-stream factorials 4)
(ref-stream factorials 5)