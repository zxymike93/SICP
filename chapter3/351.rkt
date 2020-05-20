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

(define (map-stream proc . argstreams)
  (if (null-stream? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map car-stream argstreams))
       (apply map-stream
              (cons proc (map cdr-stream argstreams))))))

(define (enumerate-interval-stream n m)
  (if (> n m)
      empty-stream
      (cons-stream n
                   (enumerate-interval-stream (+ n 1) m))))

(define (ref-stream s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (- n 1))))

(define (show x)
  (display x)
  x)

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 empty-stream))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 empty-stream))))
(define s3 (cons-stream 700 (cons-stream 800 (cons-stream 900 empty-stream))))

(define x
  (map-stream show (enumerate-interval-stream 0 10)))
;; 0

(ref-stream x 5)
;; 12345
;; => 5
(ref-stream x 7)
;; 67
;; => 7