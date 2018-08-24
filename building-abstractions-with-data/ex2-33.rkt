#lang sicp


(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))


(define (map p sequence)
  (accumulate [lambda (x y) (cons (p x) y)]
              nil
              sequence))

; (cons ([lamda] (car sequence))
;       (cons ([lambda] (car sequence))
;             (cons ([lambda] (car sequence))....
(map [lambda (x) (* x x)] (list 1 2 3 4))


(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

; (cons (car seq1)
;       (cons (car seq1) ... (cons (...) seq2))..)
(append (list 1 2 3) (list 4 5 6))


(define (length sequence)
  (accumulate [lambda (x y) (+ 1 y)]
              0
              sequence))

; (+ 1 (+ 1 (+ 1 ....)
(length (list 1 2 3 4 5 100 33))