#lang sicp

(define (square x) (* x x))

(define (is-even n) (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0)
          1)
        ((is-even exp)
          (remainder (square (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (iter-fermat n)
  (define (fermat-test a n)
    (cond ((and (= (expmod a n n) a) (< a n))
            (fermat-test (+ a 1) n))
          ((= a n)
            #t)
          (else
            #f)))

  (fermat-test 2 n))

(define (carmichael-test n expect)
  (newline)
  (display n)
  (display (iter-fermat n))
  (display expect))


(carmichael-test 2 true)
(carmichael-test 3 true)
(carmichael-test 7 true)
(carmichael-test 99 false)

(carmichael-test 561 false)
(carmichael-test 1105 false)
(carmichael-test 1729 false)
(carmichael-test 2465 false)
(carmichael-test 2821 false)
(carmichael-test 6601 false)
