#lang sicp

(define x (list 1 3 (list 5 7) 9))
(define y (list (list 7)))
(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; Pick 7 using car/cdr

(car (cdr (car (cdr (cdr x)))))

(car (car y))

; **list ends up with a nil**
(define deeper
  (lambda (x) (car (cdr x))))

(deeper (deeper (deeper (deeper (deeper (deeper z))))))