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

(define (smallest-divisor n)
    (find-divisor n 2)
)

; Skip even numbers
(define (next n)
    (if (= n 2)
        3
        (+ n 2)
    )
)

; A primality test process with O(√n)
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
            n)
          ((divides? n test-divisor)
            test-divisor)
          (else
            ; (find-divisor n (+ test-divisor 1)))
            ; Use `next` to reduce the cost
            (find-divisor n (next test-divisor)))
    )
)

(define (divides? a b)
    (= (remainder a b) 0)
)

(smallest-primes 100000000000 3)
(smallest-primes 1000000000000 3)
(smallest-primes 10000000000000 3)
(smallest-primes 100000000000000 3)

; Please compare with `ex1-22`
; The result of this program is:

; 100000000003 *** .27000000000001023
; 100000000019 *** .2799999999999869
; 100000000057 *** .269999999999996
; 1000000000039 *** .8599999999999994
; 1000000000061 *** .8599999999999994
; 1000000000063 *** .8700000000000045
; 10000000000037 *** 2.6400000000000006
; 10000000000051 *** 2.6599999999999966
; 10000000000099 *** 2.6400000000000006
; 100000000000031 *** 8.399999999999991
; 100000000000067 *** 8.810000000000002
; 100000000000097 *** 8.41000000000001

; It's not running twice as fast as that in `ex1-22`
