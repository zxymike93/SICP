; b^n = b^2^(n/2) if n is even
; (expt (square b) (/ n 2))

(define (is-even x)
    (= (remainder x 2) 0)
)

(define (expt b n)
    ; Assume `expt-iter` gets `b^n` and lets `a*b^n` unchanged.
    ; Actually, `a` keeps all `b`s from odd.
    (define (expt-iter base n a)
        (cond ((= n 0)
                a)
              ((is-even n)
                (expt-iter (square base) (/ n 2) a))
              (else
                (expt-iter base (- n 1) (* base a)))
        )
    )

    (expt-iter b n 1)
)
