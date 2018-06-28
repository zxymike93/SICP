#lang sicp

(define (average a b)
  (/ (+ a b)
     2)
)


(define (iter-improve good-enough? improve-guess)
  [lambda (first-guess)

    (define (iter x)
      (let ([y (improve-guess x)])
        (if (good-enough? x y)
            y
            (iter y))))

    (iter first-guess)]
)


(define (fixed-point f first-guess)
  (define (improve x) (f x))

  (define (good-enough? a b) (> 0.0001 (abs (- a b))))
  
  ((iter-improve good-enough? improve ) first-guess)
)


(define (new-sqrt x first-guess)
  (fixed-point [lambda (y) (average y (/ x y))]
               first-guess)
)


; (new-sqrt 4)
(new-sqrt 16 1.0)