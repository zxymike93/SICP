; Newton's method for cube root
; |(x/guess^2 + 2guess) / 3 - x| < 0.001

(define (cube x)
    (* x x x)
)

(define (square x)
    (* x x)
)

(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001)
)

(define (curt-iter guess x)
    (if (good-enough? guess x) guess
        (curt-iter (improve guess x) x)
    )
)

(define (curt x)
    (curt-iter 1.0 x)
)
