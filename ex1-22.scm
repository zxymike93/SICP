(define (smallest-primes n counter)
    (if (odd? n)
        (search-for-primes (+ n 2) counter)
        (search-for-primes (+ n 1) counter)
    )
)

(define (odd? n) (= (remainder n 2) 1))

; Checks the primality of consecutive odd ints in a range
(define (search-for-primes n counter)
    (if (> counter 0)
        (if (timed-prime-test n)
            (search-for-primes (+ n 2) (- counter 1))
            (search-for-primes (+ n 2) counter)
        )
    )
)

; Displays time cost if the input number is prime.
(define (timed-prime-test n)
    (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime n (- (runtime) start-time))
        false
    )
)

(define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time)
)

(define (prime? n)
    (= (smallest-divisor n) n)
)

; A primality test process with O(root-of-n)
(define (smallest-divisor n)
    (find-divisor n 2)
)

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
            n)
          ((divides? n test-divisor)
            test-divisor)
          (else
            (find-divisor n (+ test-divisor 1)))
    )
)

(define (divides? a b)
    (= (remainder a b) 0)
)

(smallest-primes 100000000000 3)
(smallest-primes 1000000000000 3)
(smallest-primes 10000000000000 3)
(smallest-primes 100000000000000 3)

; 100000000003 *** .45000000000000007
; 100000000019 *** .44999999999999996
; 100000000057 *** .44999999999999973
; 1000000000039 *** 1.4299999999999997
; 1000000000061 *** 1.4
; 1000000000063 *** 1.3899999999999997
; 10000000000037 *** 4.660000000000001
; 10000000000051 *** 4.41
; 10000000000099 *** 4.419999999999998
; 100000000000031 *** 13.649999999999999
; 100000000000067 *** 13.589999999999996
; 100000000000097 *** 13.61999999999999