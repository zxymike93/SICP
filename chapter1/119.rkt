#lang sicp

;; ai = bq + aq + ap = bq + a(p+q)
;; bi = bp + aq

;; aii = biq + ai(p+q) = b(q^2 + 2pq) + a([q^2 + 2pq] + [q^2 + p^2])
;; bi = bip + aiq = b(p^2 + q^2) + a(q^2 + 2pq)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q n)
  (cond [(= n 0)
         b]
        [(even? n)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))    ;; pi
                   (+ (* 2 p q) (* q q))  ;; qi
                   (/ n 2))]
        [else
         (fib-iter (+ (* b q)
                      (* a q)
                      (* a p))
                   (+ (* b p)
                      (* a q))
                   p
                   q
                   (- n 1))]))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)