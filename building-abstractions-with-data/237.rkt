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

;; vector
(define v (list 1 2 3 4))
(define w (list 5 6 7 8))
;; matrix
(define m (list (list 1 1 1)
                (list 2 2 2)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v))
       m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ([n-cols (transpose n)])
    (map (lambda (m-row) (matrix-*-vector n-cols m-row))
         m)))

;; tests

(dot-product v w)
; (+ (* 1 5) (* 2 6) (* 3 7) (* 4 8))

(matrix-*-vector m v)
; ((1 1 1)  *  (1 2 3 4)  = ((1*1+1*2+1*3)
;  (2 2 2))                  (2*1+2*2+2*3))

(transpose m)
; ((1 2)
;  (1 2)
;  (1 2))

(matrix-*-matrix m m)
; ((1 2)  *  (1 1 1)  =  ((1*1+1*2 1*1+1*2 1*1+1*2)
;  (1 2)     (2 2 2)      (2*1+2*2 2*1+2*2 2*1+2*2))
;  (1 2))