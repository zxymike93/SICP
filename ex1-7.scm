(define (average a b)
    (/ (+ a b) 2)
)

; (y + x/y) / 2
(define (improve guess x)
    (average guess (/ x guess))
)

; Stop guessing if new guess is a small fraction of the old one
(define (good-enough? guess new_guess)
    (< (abs (- new_guess guess)) 0.001)
)

(define (sqrt-iter guess x)
    (define new_guess (improve guess x))

    (if (good-enough? guess new_guess) guess
        (sqrt-iter new_guess x)
    )
)

(define (sqrt x)
    (sqrt-iter 1.0 x)
)
