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

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-stream ones integers)))

;; (ref-stream (partial-sums integers) n) = 1 + 2 + 3 + ... + n
(define (partial-sums s)
  (cons-stream (car-stream s)
               (add-stream (partial-sums s) (cdr-stream s))))

;; test
(define psi (partial-sums integers))
(ref-stream psi 0)
(ref-stream psi 1)
(ref-stream psi 2)
(ref-stream psi 3)
(ref-stream psi 4)
(ref-stream psi 5)