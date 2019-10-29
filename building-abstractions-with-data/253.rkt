#lang sicp

(list 'a 'b 'c)
; (a b c)

(list (list 'george))
; (list '(george))
; ((george))

(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; (y1 y1)

(pair? (car '(a short list)))
; (pair? 'a)
; #f

(memq 'red '((red shoes) (blue socks)))
; (eq? 'red (car '((red shoes) (blue socks)))) => #f
; (eq? 'red (car '((blue socks)))) => #f
; ((null? '()) => #t
; false

(memq 'red '(red shoes blue socks))
; (eq? 'red (car '(red shoes blue socks))) => #t
; (red shoes blue socks)