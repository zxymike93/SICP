(define (reverse items)
  (define (iter old new)
    (if (null? old)
        new
        (iter (cdr old)
              (cons (car old) new))))
  (iter items '())
)


; Test
(reverse (list 1 4 9 16 25))