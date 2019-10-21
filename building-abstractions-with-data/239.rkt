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

(define (reverse-v1 seq)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              seq))

(define (reverse-v2 seq)
  (fold-left (lambda (x y) (append (list y) x))
             nil
             seq))

(define x (list 1 2 3 4))

(reverse-v1 x)
(reverse-v2 x)