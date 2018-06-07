; If n is not prime, that n must have a divisor
; less than or equal to root of n.
; Thus the steps in find-divisor reduce
; to root of n instead of n.

(define (prime n)
    (= (smallest-divisor n) n)
)

(define (smallest-divisor x)
    (find-divisor x 2)
)

(define (find-divisor x y)
    (cond ((> (square y) x)
            x)
          ((no-remainder x y)
            y)
          (else
            (find-divisor x (+ y 1)))
    )
)

(define (no-remainder a b)
    (= (remainder a b) 0)
)
