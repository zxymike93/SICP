#lang sicp

(define (make-table same-key?)

  (define (assoc key records)
    (cond ((null? records)
           false)
          ((same-key? key (caar records))
           (car records))
          (else
           (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value)) (cdr local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))

    dispatch))

(define (=? a b)
  (let ((tolerance 0.1))
    (if (and (number? a) (number? b))
        (> tolerance (abs (- a b)))
        (equal? a b))))

;; test
(define sample-table (make-table =?))
(define get (sample-table 'lookup-proc))
(define put (sample-table 'insert-proc!))

(put 1.0 1.0 'a)
(put 1.01 1.0 'b)
(get 1.0 1.0)