(define (square x)
    (* x x)
)

(define (average x y)
    (/ (+ x y) 2)
)

; improve / guess / sqrt-iter are not commonly used
; nest them as block structure
(define (sqrt x)

    (define (improve guess)
        (average guess (/ x guess))
    )

    (define (good-enough? guess)
        (> 0.001 (abs (- x (square guess))))
    )

    (define (sqrt-iter guess)
        (if (good-enough? guess) guess
            (sqrt-iter (improve guess))
        )
    )

    (sqrt-iter 1.0)
)

