#lang sicp

;; int -> int
(define (square x)
  (* x x))

;; int, int -> bool
(define (divides? a b)
  (= (remainder a b) 0))

;; int -> int
(define (smallest-divisor n)
  (define (find-divisor n d)
    (cond [(> (square d) n) n]
          [(divides? n d) d]
          [else (find-divisor n (+ d 1))]))  
  (find-divisor n 2))

;; int -> bool
(define (prime? n)
  (= (smallest-divisor n) n))

;; (a, b) -> (a, b, a+b)
;; list -> list
(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

;; list -> list
(define (filter predicate seq)
  (cond [(null? seq) nil]
        [(predicate (car seq)) (cons (car seq)
                                     (filter predicate (cdr seq)))]
        [else (filter predicate (cdr seq))]))
;; (2 3 4 5 6 7) -> (2 3 5 7)
;; (filter prime? (list 2 3 4 5 6 7))

;; list -> list
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

;; int, int -> list
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))

;; int -> list
(define (unique-pair n)
  (accumulate append
              nil
              (map (lambda (i) (map (lambda (j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

;; int -> list
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pair n))))

(prime-sum-pairs 6)