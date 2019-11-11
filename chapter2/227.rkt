#lang sicp

(define (reverse l)
  (define (loop old new)
    (if (null? old)
        new
        (loop (cdr old)
              (cons (car old) new))))
  (loop l '()))

(define (deep-reverse l)
  (if (not (pair? l))
      l
      (reverse (map deep-reverse l))))

;; tests
(define x (list (list 1 2) (list 3 4)))
(define y (list (list (list 1 2) (list 3 4)) (list 5 6)))
(define z (list 1 (list (list 2 3)
                        (list (list (list 4 5) 6) 7)
                        8 9)))

(deep-reverse x)
;; ((4 3) (2 1))
(deep-reverse y)
;; ((6 5) ((4 3) (2 1)))
(deep-reverse z)
;; ((9 8 (7 (6 (5 4))) (3 2)) 1)