#lang sicp

;(define (make-monitor f)
;  (let ([count 0])
;    (lambda [arg]
;      (cond [(eq? arg 'reset-count)
;             (set! count 0)]
;            [(eq? arg 'how-many-calls?)
;             count]
;            [else
;             (begin (set! count (+ count 1))
;                    (f arg))]))))

;; 也可以显式地命名
(define (make-monitor f)
  (define count 0)
  (define (mf arg)
    (cond
      [(eq? arg 'reset-count)
       (set! count 0)]
      [(eq? arg 'how-many-calls?)
       count]
      [else
       (begin (set! count (+ count 1))
              (f arg))]))
  mf)

;; tests
(define s (make-monitor sqrt))
(s 100)
(s 121)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)