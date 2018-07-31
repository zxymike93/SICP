#lang sicp


;; if 语句只支持
;; if <cond> .. <expr> .. <expr>
;; 因此使用 begin 来执行多步骤的调用
(define (for-each proc items)
  (if (not (null? items))
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

;; 或者使用 cond (<cond> .. <expr1> <expr2> ...)
(define (for-each-2 proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each-2 proc (cdr items)))))


;; Test
(for-each
 (lambda (x) (newline) (display x))
 (list 57 32 88))

(for-each-2
 (lambda (x) (newline) (display x))
 (list 57 32 88))
