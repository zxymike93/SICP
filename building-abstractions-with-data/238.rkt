#lang sicp

(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(fold-right / 1 (list 1 2 3))
; (/ 1 (/ 2 (/ 3 1))) -> 3/2

(fold-left / 1 (list 1 2 3))
; (/ (/ (/ 1 1) 2) 3) -> 1/6

(fold-right list nil (list 1 2 3))
; (list 1 (list 2 (list 3 (list nil)))) -> (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
; (list (list (list nil 1) 2) 3) -> (((() 1) 2) 3)

;; op 满足交换律和结合律