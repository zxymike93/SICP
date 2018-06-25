#lang sicp


(define (fixed-point f guess)
  (let ([tolerance 0.001]
        [next (f guess)])
    (define (close? a b) (< (abs (- a b)) tolerance))

    (if (close? guess next)
        next
        (fixed-point f next)))
)


(define (newton g)

  (define (D g)
    (let ([dx 0.00001])
      (lambda (x) (/ (- (g (+ x dx)) (g x))
                     dx))))

  (lambda (x) (- x
                 (/ (g x) ((D g) x))))
)


(define (transform g trans guess)
  (fixed-point (trans g) guess))


(define (cubic a b c)
  (lambda (x) (+
               (* x x x)
               (* a x x)
               (* b x)
               c)))


(transform (cubic 1 3 -18) newton 1)