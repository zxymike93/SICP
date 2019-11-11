#lang sicp

;(define (for-each proc items)
;  (cond [(null? items)
;         (newline)
;         (display "done")]
;        [else
;         (proc (car items))
;         (for-each proc (cdr items))]))

(define (for-each proc items)
  (if (null? items)
      (begin
        (newline)
        (display "done"))
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

;; test
(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))