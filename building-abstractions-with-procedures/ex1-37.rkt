#lang sicp


(define (cont-frac n d k)

  ; Recursive version:
  ;   Define a function cf(i), represeting the real continued-fraction, such that
  ;   cf(1) = (/ N1 (+ D1 cf(2)) = (/ N1 (+ D1 (/ N2 (+ D2 cf(3))))) ...
  (define (cf-rec i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i)
           (+ (d i) (cf-rec (+ i 1))))))
  ; (cf-rec 1)

  ; Iterative version:
  ;   cf(i) = cf(i-1)
  (define (cf-iter i result)
    (if (= i 1)
        result
        (cf-iter (- i 1)
                 (/ (n (- i 1))
                    (+ (d (- i 1)) result)))))
    (cf-iter k (/ (n k) (d k)))
)

; Golden-ratio: gs
; gs^2 = gs + 1 (map to) gs = 1 + 1/gs
(define (golden-ratio k)
  (+ 1
     (cont-frac (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 k)))


(golden-ratio 11)