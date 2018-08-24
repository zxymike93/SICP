#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (horner-eval x coefficient-sequence)
  (accumulate [lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x))]
              0
              coefficient-sequence))

; 1 + 3x + 5x^3 + x^5 , x=2
(horner-eval 2 (list 1 3 0 5 0 1))