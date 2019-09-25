#lang sicp

(define modulo remainder)

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

;; 非凡平方根
(define (nontrivial-sqrt? a b n)
  (if (or (not (= a b))
          (not (= a (- n b))))
      #f
      (if (= (modulo (square a) n)
             b)
          #t
          #f)))

(define (expmod base exp mod)
  (cond [(= exp 0)
         1]
        [(= exp 1)
         base]
        [(even? exp)
         (if (nontrivial-sqrt? base 1 mod)
             0
             (modulo (square (expmod base (/ exp 2) mod))
                     mod))]
        [else
         (modulo (* (expmod base 1 mod) (expmod base (- exp 1) mod))
                 mod)]))

;; 1、如果 n 是素数，整数 1 < a < n
;;   那么 a^(n-1) mod n = 1 mod n (也就是 = 1)
;; 2、如果存在一个 nontrival (not)= 1 | n - 1
(define (miller-test n)
  (define (try a)
    (let ([x (expmod a (- n 1) n)])
      (cond [(= x (modulo 1 n)) #t]
            [(= x 0) #f]
            [else #f])))
  (try (+ 1 (random (- n 1)))))

(define (prime? n)
  (define (fast-prime? n times)
    (cond [(= times 0) #t]
          [(miller-test n) (fast-prime? n (- times 1))]
          [else #f]))
  (fast-prime? n 100))

;; tests
(prime? 199)
(prime? 1999)
(prime? 19999)
(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)