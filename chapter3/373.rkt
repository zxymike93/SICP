#lang racket

(define (rc r c dt)
  (lambda (i v0)
    (add-stream (integral (scale-stream (/ 1 c) i)
                          v0
                          dt)
                (scale-stream r i))))