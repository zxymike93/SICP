; b^n = b^(n/2)^2 , if n is even
; b^n = b*b^(n-1) , if n is odd

(define (is-even n)
    (= (remainder n 2) 0)
)

(define (expt b n)
    (cond ((= n 0) 1)
          ((is-even n) (square (expt b (/ n 2))))
          (else (* b (expt b (- n 1))))
    )
)
