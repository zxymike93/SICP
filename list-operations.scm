(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


(define (length items)
  (define (iter items counter)
    ; null? -> is a list '() ?
    (if (null? items)
        counter
        (iter (cdr items) (+ counter 1))))
  (iter items 0))


; (define (append l1 l2)
;   (if (null? l1)
;       l2
;       (cons (car l1)
;             (append (cdr l1) l2))))


;; Test
(define l
  (list 1 2 3 4))

(list-ref l 3)

(length l)

(append (list 1 2) (list 3 4))
(append (list 3 4 5) (list 2 1))
