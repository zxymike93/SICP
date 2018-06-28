#lang racket

(define (fixed-point f guess)
  (let ([next-guess (f guess)])
    (display next-guess)
    (newline)
    ; close-enough?
    (if ((lambda (a b)
           (< (abs (- a b)) 0.001))
         guess next-guess)
        next-guess
        (fixed-point f next-guess))))

(fixed-point (lambda (x)
               (/ (log 1000)
                  (log x)))
             1.1)

