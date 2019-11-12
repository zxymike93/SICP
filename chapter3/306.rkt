#lang sicp

(define (rand arg)
  (let ([x 7])
    (cond [(eq? arg 'reset)
           (lambda [value] (set! x value) x)]
          [(eq? arg 'generate)
           (begin
             (set! x (random x))
             x)]
          [else
           (error "Command not supported.")])))