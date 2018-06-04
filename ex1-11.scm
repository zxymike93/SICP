; Tree recursion version
(define (f-rec n)
    (if (< n 3) n
        (+ (f-rec (- n 1))
           (* 2 (f-rec (- n 2)))
           (* 3 (f-rec (- n 3)))
        )
    )
)

; Linear iteration version
(define (f n)
    (define (f-iter a b c count)
        (if (= count 0) c
            (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))
        )
    )

    (f-iter 2 1 0 n)
)
