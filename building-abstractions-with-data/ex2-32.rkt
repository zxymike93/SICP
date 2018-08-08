#lang sicp

; all subsets = all subsets without the first element
;               + all subsets with the first element

(define (subsets s)
  (if (null? s)
      (list s)
      (let ([rest (subsets (cdr s))])
        (append rest
                (map [lambda (x) (cons (car s) x)] rest)))))

#| Have a closer look by evolve the procedure.

; f = (lambda (x) (cons (car s) x))

(subsets (list 1 2 3))

; s1 = (list 1 2 3)
; rest1 = (subsets (list 2 3))
(append rest1
        (map (lambada (x) (cons (car s1) x))
             rest1))

; s2 = (list 2 3)
; rest2 = (subsets (list 3))
(append (append rest2
                (map (lambda (x) (cons (car s2) x))
                     rest2))
        (map (lambada (x) (cons (car s1) x))
             rest1))

; s3 = (list 3)
; rest3 = (subsets nil)
(append (append (append rest3
                        (map (lambda (x) (cons (car s3) x))
                             rest3))
                (map (lambda (x) (cons (car s2) x))
                     rest2))
        (map (lambada (x) (cons (car s1) x))
             rest1))

(append (append (append ('())
                        ; (map (lambda (x) (cons (car (list 3) x)) ('())))
                        (3))
                        (map (lambda (x) (cons (car s2) x))
                     rest2))
        (map (lambada (x) (cons (car s1) x))
             rest1))
; rest2 = (('()) (3))

(append (append (('()) (3))
                ; (map (lambda (x) (cons (car (list 2 3)) x)) (('()) (3))))
                ((2) (2 3)))
        (map (lambada (x) (cons (car s1) x))
             rest1))
; rest1 = (('()) (3) (2) (2 3))

(append (('()) (3) (2) (2 3))
        ; (map (lambada (x) (cons (car (list 1 2 3)) x)) (('()) (3) (2) (2 3))))
        ((1) (1 3) (1 2) (1 2 3)))

(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
|#

; Tests
(subsets (list 1 2 3))
