#lang sicp

;; f(n) = n, n<3
;;      = f(n-1) + 2f(n-2) + 3f(n-3)

;; given:  0 1 2 3 4  5  6  7   ...
;; expect: 0 1 2 4 11 25 59 142 ...

;; (Tree) Recursive Process
(define (f-recu n)
  (if (< n 3)
      n
      (+ (f-recu (- n 1))
         (* 2 (f-recu (- n 2)))
         (* 3 (f-recu (- n 3))))))

;; (Linear) Iterative Process
(define (f-iter n)
  (define (iter a b c n)
    (if (= n 0)
        a
        (iter b
              c
              (+ (* 3 a) (* 2 b) c)
              (- n 1))))
  (iter 0 1 2 n))

;; tests
(f-recu 0)
(f-recu 7)
(f-iter 0)
(f-iter 7)