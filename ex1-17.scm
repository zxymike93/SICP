; Suppose that there isn't * operation in Scheme
; We got this linear-recursive * by merely using +
; (define (* a b)
;     (if (= b 0) 0
;         (+ a (* a (- b 1)))
;     )
; )
; Now if including + / double / halve
; design a * that uses logarithmic time

(define (* a b)
    (cond ((= b 0)
            0)
          ((is-even b)
            (double (* a (halve b))))
          (else
            (+ a (* a (- b 1))))
    )
)

(define (is-even x)
    (= (remainder x 2) 0)
)

(define (double x) (+ x x))
(define (halve x) (/ x 2))
