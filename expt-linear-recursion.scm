(define (expt b n)
    (if (= n 0) 1
        (* b (expt (- n 1)))
    )
)
