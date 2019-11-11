#lang sicp

(define (equal? x y)
  (cond
    ;; x y are both symbols
    [(and (not (pair? x)) (not (pair? y)))
     (eq? x y)]
    ;; one of them not symbols
    [(and (pair? x) (pair? y))
     (and (equal? (car x) (car y))
          (equal? (cdr x) (cdr y)))]
    [else
     #f]))

;; test
(equal? '(this is a list) 
        '(this is a list))

(equal? '(this is a list) 
        '(this (is a) list))

(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6))

(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)) 