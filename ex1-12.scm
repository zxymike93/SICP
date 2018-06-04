; Yang Hui's triangle (aka Pascal's triangle)
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1

(define (pascal row col)
    (cond ((or (= col 1) (= row col)) 1)
          (else (+ (pascal (- row 1) col)
                   (pascal (- row 1) (- col 1))))))
