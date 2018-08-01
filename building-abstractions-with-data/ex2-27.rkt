#lang sicp

(define (reverse items)
  (define (iter old new)
    (if (null? old)
        new
        (iter (cdr old)
              (cons (car old) new))))
  (iter items '())
)


(define (deep-reverse x)
  (if (pair? x)
      (reverse (map deep-reverse x))
      x))


(define x (list (list 1 2)
                (list 3 4)))


(define y (list (list (list 1 2)
                      (list 3 4))
                (list (list 5 6)
                      (list 7 8))))


(deep-reverse x)
(deep-reverse y)