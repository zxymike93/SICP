#lang sicp

;; (make-rat n d) -> x
;; (numer x) -> n
;; (denom x) -> d


(define (make-rat n d)
  ; (cons n d))
  (if (< d 0)
      (cons (- n) (- d))
      (cons n d)))


(define (numer x) (car x))


(define (denom x) (cdr x))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))


(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))


(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


; try
(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(define neg-two-fifth (make-rat 2 -5))

(print-rat one-half)
(print-rat neg-two-fifth)