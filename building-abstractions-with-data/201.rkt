#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; data
(define (make-rat n d)
  ;; 令 g 与 d 同号，则 d/g 正正得正/负负得正，n/g 与 n/d 正负相同
  (let ([g ((if (< d 0) - +)
            (abs (gcd n d)))])
    (cons (/ n g)
          (/ d g))))

(define (numer r)
  (car r))

(define (denom r)
  (cdr r))

;; operations
(define (print-rat r)
  (newline)
  (display (numer r))
  (display "/")
  (display (denom r)))

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
               (* (numer b) (denom a)))
            (* (denom a) (denom b))))

(define (sub-rat a b)
  (make-rat (- (* (numer a) (denom b))
               (* (numer b) (denom a)))
            (* (denom a) (denom b))))

(define (mul-rat a b)
  (make-rat (* (numer a) (numer b))
            (* (denom a) (denom b))))

(define (div-rat a b)
  (make-rat (* (numer a) (denom b))
            (* (denom a) (numer b))))

;; tests
(define one-half (make-rat 6 -9))
(print-rat one-half)
(print-rat (add-rat one-half one-half))
(print-rat (sub-rat one-half one-half))
(print-rat (mul-rat one-half one-half))
(print-rat (div-rat one-half one-half))