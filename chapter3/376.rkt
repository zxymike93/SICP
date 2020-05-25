#lang racket

(define (smooth smooth-proc s)
  (map-stream smooth-proc (cdr-stream s) s))

(define (make-zero-crossings input-stream)
  (map-stream sign-change-detector
              (cdr-stream input-stream)
              input-stream))

(define zero-crossings (make-zero-crossings (smooth
                                             (lambda (a b) (/ (+ a b) 2))
                                             sense-data)))