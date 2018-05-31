(define (square x)
    (* x x)
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (improve guess x)
    (average guess (/ x guess))
)

(define (abs x)
    (if (< x 0) (- x)
        x
    )
)

(define (good_enough? guess x)
    (> 0.001 (abs (- x (square guess))))
)

(define (new_if predicate then_clause else_clause)
    (cond (predicate then_clause)
        (else else_clause)
    )
)

(define (guess_loop guess x)
    (new_if (good_enough? guess x) guess
        (guess_loop (improve guess x) x)
    )
)

(define (sqrt x)
    (guess_loop 1 x)
)
