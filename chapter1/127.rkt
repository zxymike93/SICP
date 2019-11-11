#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (expmod base exp mod)
  (cond [(= exp 0)
         1]
        [(= exp 1)
         base]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) mod))
                    mod)]
        [else
         (remainder (* (expmod base 1 mod) (expmod base (- exp 1) mod))
                    mod)]))

;; 如果 1 < a < n-1 全部有， a^n mod n = a，则视为通过费马测试。
(define (fermat-test n)
  (define (iter a n)
    (if (= a n)
        #t
        (if (= (expmod a n n) a)
            (iter (+ a 1) n)
            #f)))
  (iter 1 n))

;; 某些数能通过费马测试，但并不是素数。这样的数叫 Carmichael Number
(define (carmichael n expect)
  (define (report-carmichael x)
    (display "Carmichael Number *** ")
    (display x)
    (newline))

  (if (not (eq? (fermat-test n) expect))
      (report-carmichael n)
      ))

;; 测试，证明费马测试正常
(carmichael 199 #t)
(carmichael 1999 #t)
(carmichael 19999 #f)
;; 测试，证明 carmichael 
(carmichael 561 #f)
(carmichael 1105 #f)
(carmichael 1729 #f)
(carmichael 2465 #f)
(carmichael 2821 #f)
(carmichael 6601 #f)