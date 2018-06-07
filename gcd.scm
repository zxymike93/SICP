; Greatest Common Divisor

; if b % a == r
; (gdc a b) == (gdc b r)

(define (gcd a b)
    (if (= b 0) a
        (gcd b (remainder a b))
    )
)
