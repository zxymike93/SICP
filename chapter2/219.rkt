#lang sicp

(define no-more? null?)

(define expect-first-denomination cdr)

(define first-denomination car)

(define (cc amount coin-values)
  (cond [(= amount 0)
         1]
        [(or (< amount 0)
             (no-more? coin-values))
         0]
        [else
         (+ (cc amount (expect-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values))]))

;; tests
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 uk-coins)