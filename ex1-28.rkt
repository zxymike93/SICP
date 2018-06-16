#lang sicp

; n , is prime
; 0 < a < n
; a^(n-1) = 1 (mod n)

; However,
; 

(define (square n) (* n n))

(define (exp b e)
  (if (= e 0) 1
      (exp b (- e 1))))

(define (nontrivial-sqrt? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (square a) n))))

(define (expmod b e m)
  (cond ((= e 0)
         1)
        ((nontrivial-sqrt? b m)
         0)
        ((even? e)
         (remainder (square (exp b (/ e 2)))
                    m))
        (else
         (remainder (* b (exp b (- e 1)))
                    m))))

(define (miller-robin n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))

  (try-it (+ 1 (random (- n 1))))
)

(define (test n times)
  (cond ((= times 0)
         #t)
        ((miller-robin n)
         (test n (- times 1)))
        (else
         #f)))

(test 561 1000)
(test 1105 1000)
(test 1729 1000)
(test 2465 1000)
(test 2821 1000)
(test 6601 1000)