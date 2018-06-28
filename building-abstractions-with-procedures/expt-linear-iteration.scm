(define (expt base n)
    (define (expt-iter count product)
        (if (= count 0) product
            (expt-iter (- count 1) (* product base))
        )
    )
    (expt-iter n 1)
)
