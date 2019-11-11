#lang sicp

;; 根据习题35可以得到黄金分割律（读作 phi）的求值过程
(define (fixed-point f first-guess)

  (define (close-enough? a b)
    (> 0.00001 (abs (- a b))))

  (define (try guess)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

;; golden-ratio
(define phi
  (fixed-point
   (lambda (x) (+ 1 (/ 1 x)))
   0.0))

;; 表达一个叫做 infinite continued fraction 的数，形式如下
;(/ N1
;   (+ D1 (/ N2
;            (+ D2 (/ N3
;                     (+ D3 + (/ Ni ...
;; 其中 1<=i<=k

(define (cont-frac-rcsv n d k)
  (define (loop i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (loop (+ i 1))))))
  (loop 1))

(define (cont-frac-iter n d k)
  (define (loop i result)
    (if (= i k)
        result
        (loop (+ i 1) (/ (n i)
                         (+ (d i) result)))))
  (loop 1 (/ (n 1) (d 1))))

;; 从题目得知，当所有 Ni = Di = 1 时，可以渐进黄金分割率的倒数 1/phi，表示为
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           k)
;; 问 k=? 可以得到准确到小数点后4位的 1/phi
(cont-frac-rcsv (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)
;; 利用题目35的黄金分割率求 1/phi 作为对比
(/ 1 phi)