(define (fib-iter x y n)
    (if (= n 0) x
        (fib-iter y (+ x y) (- n 1))
    )
)

(define (fib n)
    (fib-iter 0 1 n)
)
