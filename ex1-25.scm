(define (odd? n) (= (remainder n 2) 1))
(define (even? n) (= (remainder n 2) 0))

(define (fast-expt b n)
    (cond ((= n 0)
            1)
          ((even? n)
            (square (fast-expt b (/ n 2)))
          )
          (else
            (* b (fast-expt b (- n 1)))
          )
    )
)

; base^exp (mod m)
(define (expmod base exp m)
    (remainder (fast-expt base exp) m)
)

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)
    )

    (try-it (+ 1 (random (- n 1))))
)

; Prime test using fermat-test with specific times
(define (prime? n times)
    (cond ((= times 0)
            true)
          ((fermat-test n)
            (prime? n (- times 1)))
          (else
            false)
    )
)

(define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time)
)

; Displays time cost if the input number is prime.
(define (timed-prime-test n)
    (define (start-prime-test n start-time)
        (if (prime? n 1000)
            (report-prime n (- (runtime) start-time))
            false
        )
    )

    (start-prime-test n (runtime))
)

; Checks the primality of consecutive odd ints in a range
(define (search-for-primes n counter)
    (if (> counter 0)
        (if (timed-prime-test n)
            (search-for-primes (+ n 2) (- counter 1))
            (search-for-primes (+ n 2) counter)
        )
    )
)

(define (smallest-primes n counter)
    (if (odd? n)
        (search-for-primes (+ n 2) counter)
        (search-for-primes (+ n 1) counter)
    )
)


(smallest-primes 1000000000000000000 1)
(smallest-primes 10000000000000000000 1)
(smallest-primes 100000000000000000000 1)
(smallest-primes 1000000000000000000000 1)