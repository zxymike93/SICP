#lang sicp

; (define (sum term a next b)
;   (if (> a b)
;       0
;       (+ (term a)
;          (sum term (next a) next b))))


(define (isum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


; f = [f(a+dx/2) + f(a+dx+dx/2) + ...]dx
(define (integral f a b dx)
  (define (add-dx n) (+ n dx))

  (* dx
     (isum f (+ a (/ dx 2)) add-dx b)))


(define (cube x) (* x x x))

(integral cube 0 1 0.01)