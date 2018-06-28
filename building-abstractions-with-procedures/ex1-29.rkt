#lang sicp


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpson f a b n)
  
  (define h (/ (- b a) n))

  ; yk = f(a+kh)
  (define (y k)
    (f (+ a (* k h))))

  ; 1, 4, 2, 4, 2, ..., 1 (factor)
  ; 0, 1, 2, 3, 4, ..., n (k)
  (define (factor k)
    (cond ((or (= k 0) (= k n))
           1)
          ((odd? k)
           4)
          ((even? k)
           2)))

  ; y*factor (k: 0~n)
  (define (term n)
    (* (factor n)
       (y n)))

  (define (next n)
    (+ n 1))

  (if (even? n)
      (* (sum term a next b)
         (/ h 3))
      (error "n must be an even number")))


(define (cube x)
  (* x x x))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)