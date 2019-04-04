#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; vectors (1 2 3 4)
; matrics ((1 2 3 4) (5 6 7 8))

#|
(define (dot-product v w)
  (if (or (null? v) (null? w))
      0
      (+ (* (car v) (car w))
         (dot-product (cdr v) (cdr w)))))

(map +
     (list 1 2 3)
     (list 4 5 6)
     (list 7 8 9))
; (12 15 18)

|#

(define (dot-product v w)
  (accumulate + 0 (map * v w)))



; tests
(dot-product (list 1 2 3 4) (list 5 6 7 8))