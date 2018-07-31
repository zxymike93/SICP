#lang sicp

(define (is-even? n)
  (= 0
     (remainder n 2)))

(define (is-odd? n)
  (not (is-even? n)))

(define (same-parity x . z)
  (define (same? a b)
    (or (and (is-even? a) (is-even? b))
        (and (is-odd? a) (is-odd? b))))

  (define (parse old-list new-list)
    (if (null? old-list)
        (reverse new-list)
        (let ([next (car old-list)]
              [rest (cdr old-list)])
          (if (same? x next)
              (parse rest (cons next new-list))
              (parse rest new-list)))))

  (parse z (list x))
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)