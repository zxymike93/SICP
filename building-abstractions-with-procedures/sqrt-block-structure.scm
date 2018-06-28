(define (square x)
    (* x x)
)

(define (average x y)
    (/ (+ x y) 2)
)

; improve / guess / sqrt-iter are not commonly used
; nest them as block structure
(define (sqrt x)

    (define (improve guess x)
        (average guess (/ x guess))
    )

    (define (good-enough? guess x)
        (> 0.001 (abs (- x (square guess))))
    )

    (define (sqrt-iter guess x)
        (if (good-enough? guess x) guess
            (sqrt-iter (improve guess x) x)
        )
    )

    (sqrt-iter 1 x)
)

