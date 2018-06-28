; Fermat's test for prime
; It is a probabilistic algorithm with order of growth O(log n)

; a is prime, and a ≥ n
; a^n (congruent to) a
; a^n % n == a % n
; remainder of a divided by n == remainder of a modulo n == a mod n

; a (mod) n == a % n

; (expmod a^n n) % n

; (base^n mod n)^2 % n

(define (expmod base exp m)
    (cond ((= exp 0)
            1)
          ((is-even exp)
            (remainder (square (expmod base (/ exp 2) m))
                        m))
          (else
            (remainder (* base (expmod base (- exp 1) m)) m))
    )
)

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)
    )

    (try-it (+ 1 (random (- n 1))))
)

; run the test
(define (is-prime n times)
    (cond ((= times 0)
            true)
          ((fermat-test n)
            (is-prime n (- times 1)))
          (else
            false)
    )
)
