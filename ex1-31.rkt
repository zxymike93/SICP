#lang sicp

(define (product-recu term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recu term (next a) next b))))


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(define (factorial n)
  (define (indentify x) x)
  (define (increase x) (+ x 1))

  ; (product-recu indentify 1 increase n))
  (product-iter indentify 1 increase n))

(factorial 5)

; numerator
; denominator
; pi/4 = (2*4*4*6*6*8...) / (3*3*5*5*7*7...)
;         1 2 3 4 5 6        1 2 3 4 5 6
(define (numerator n)
  (define (term k)
    (if (odd? k)
        (+ k 1)
        (+ k 2)))
  
  (define (next k) (+ k 1))
  
  (product-recu term 1 next n))


(define (denominator n)
  (define (term k)
    (if (odd? k)
        (+ k 2)
        (+ k 1)))

  (define (next k) (+ k 1))

  ; (product-recu term 1 next n))
  (product-iter term 1 next n))


(define (pi n)
  (* 4
     ; Can use `exact->inexact` for converting to float 
     (/ (numerator n) (denominator n))))


(pi 10000)