(define (* a b)
    (multi a b 0)
)

(define (multi a b product)
    (cond ((= b 0)
            product)
          ((is-even b)
            (multi (double a) (halve b) product))
          (else
            (multi a (- b 1) (+ product a)))
    )
)

(define (is-even x)
    (= (remainder x 2) 0)
)

(define (double x) (+ x x))
(define (halve x) (/ x x))
