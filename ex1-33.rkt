#lang sicp


(define (filtered-accumulate
         filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate
                     filter combiner null-value term (next a) next b))
          (filtered-accumulate
           filter combiner null-value term (next a) next b))))


(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (identify x) x)


(define (prime? n)
  (define (iter a n)
    (cond ((> (square a) n)
           #t)
          ((= (remainder n a) 0)
           #f)
          (else
           (+ a 1))))

  (if (= n 1)
      #t
      (iter 2 n)))


(define (sum-of-prime-square a b)  
  (filtered-accumulate
   prime? + 0 square a inc b))


; (sum-of-prime-square 1 3)


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (product-of-relative-prime n)
  (define (relative-prime? x)
    (= (gcd n x) 1))
  
  (filtered-accumulate
   relative-prime? * 1 identify 1 inc n))


(display "relative prime:")
(product-of-relative-prime 10)