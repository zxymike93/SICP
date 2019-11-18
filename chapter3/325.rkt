#lang sicp

(define (make-table) (list '*table*))

(define (assoc key records)
  (cond [(not (pair? records))
         #f]
        [(equal? key (caar records))
         (car records)]
        [else
         (assoc key (cdr records))]))

(define (lookup table keys)
  (let ([record (assoc (car keys) (cdr table))])
    (cond [(null? (cdr keys))
           (if record
               (cdr record)
               false)]
          [else
           (lookup record (cdr keys))])))

(define (insert! table keys value)
  (let ([record (assoc (car keys) (cdr table))])
    (cond [(null? (cdr keys))
           (cond [record
                  (set-cdr! record value)
                  table]
                 [else
                  (set-cdr! table (cons (cons (car keys) value)
                                        (cdr table)))
                  table])]
          [else
           (if record
               (insert! record (cdr keys) value)
               (set-cdr! table (cons (insert! (list (car keys)) (cdr keys) value)
                                     (cdr table))))])))

;; tests
(define t (make-table))

(insert! t (list 'letters 'a) 97)
(insert! t (list 'letters 'b) 97)
(insert! t (list 'letters 'b) 98)
(insert! t (list 'math '*) 42)
(insert! t (list 'math '-) 45)
(insert! t (list 'math '+) 43)

(lookup t (list 'letters 'a))
(lookup t (list 'letters 'b))
(lookup t (list 'math '*))
(lookup t (list 'math '-))
(lookup t (list 'math '+))


(insert! t (list 'math) 1)
(lookup t (list 'math '+))
(lookup t (list 'math))

(insert! t (list 'math 'is 'awesome) 'right!)
(lookup t (list 'math))