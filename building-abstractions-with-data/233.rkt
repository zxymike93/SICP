#lang sicp

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1))
              0
              sequence))

;; tests
(define l (list 1 2 3 4))

(map - l)
; (-1 -2 -3 -4)
(append l l)
; (1 2 3 4 1 2 3 4)
(length l)
; 4