#lang sicp

;; 辅助函数
(define (divides? a b)
  (= (remainder a b) 0))

(define (find-divisor n d)
  (cond [(> (square d) n) n]
        [(divides? n d) d]
        [else (find-divisor n (+ d 1))]))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (identify x)
  x)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; recursive version
(define (filtered-accumulate combiner null-value
                             filter term next a b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value
                                         filter term next (next a) b))
          (filtered-accumulate combiner null-value
                             filter term next (next a) b))))

;; 1. 表达 [a, b] 所有素数的平方的和
(define (sum-prime-square a b)
  (filtered-accumulate + 0 prime? square inc a b))
;; expect: 87
(sum-prime-square 2 10)

;; 2. 所有 与n是相对素数 的正整数数 的积
;; *注：a、b是相对素数(a<b)：a、b最大公因数为1*
(define (product-relatively-prime n)
  (define (relative-prime? x)
    (= (gcd n x) 1))
  (filtered-accumulate * 1 relative-prime? identify inc 1 n))
;; expect: 189
(product-relatively-prime 10)