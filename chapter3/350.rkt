#lang racket

(define-syntax delay-stream
  (syntax-rules ()
    [(delay-stream p) (lambda () p)])) 

(define (force-stream p)
  (p))

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

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 empty-stream))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 empty-stream))))
(define s3 (cons-stream 700 (cons-stream 800 (cons-stream 900 empty-stream))))

(map-stream + s1 s2 s3)
;'(741 . #<procedure:...chapter3/350.rkt:12:31>)