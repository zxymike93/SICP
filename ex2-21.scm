(define (square x) (* x x))

(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

;; Test
(square-list-1 (list 1 2 3 4))
;; (square-list-2 (list 1 2 3 4))
