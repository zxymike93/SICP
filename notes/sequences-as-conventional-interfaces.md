#lang sicp

;;;;;;;;;;;;;;;;; BASICS ;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(define (even? x) (= 0 (remainder x 2)))

(define (odd? x) (not (even? x)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

#|
;;;;;;;;;;;;;;; Raw ;;;;;;;;;;;;;;;;;;

; enumerate a tree and square its leaves which are odd
(define (sum-odd-squares tree)
  (cond ((null? tree)
         0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else
         (+ (sum-odd-squares (car tree))
            (sum-odd-squares (cdr tree))))))

; enumerate 0~n and get their fib-number
; then filter the even ones and put them into a list
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ([f (fib k)])
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

; If we compare these two procedures, they are distinct from one another.
; However, we can discribe them like this:
|#

; 1. *enumerate* the leaves of a tree
; 2. *filter* and select the odd ones
; 3. *for each* odd ones, square it
; 4. *accumulate* the result with +

(define (sum-odd-squares tree)
  (accumulate
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

; 1. *enumerate* integers 0~n
; 2. *for each* integers, get there fib-number
; 3. *filter* and selet the even ones
; 4. *accumulate* the result with cons (meaning that as a list)

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))

; Tests
#|(sum-odd-squares (list
                  (list 1 2)
                  (list 3 (list 4 5))))

(even-fibs 10)|#

;;;;;;;;;;;;;;;;; My Version ;;;;;;;;;;;;;;;

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval
                 (+ low 1)
                 high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accumulate proc base sequence)
  (if (null? sequence)
      base
      (proc (car sequence)
            (accumulate proc base (cdr sequence)))))

(define (filter test sequence)
  (define (iter old new)
    (if (null? old)
        (reverse new)
        (let ([x (car old)])
          (if (test x)
              (iter (cdr old) (cons x new))
              (iter (cdr old) new)))))
  (iter sequence nil))

#|
(define (filter test sequence)
  (cond ((null? sequence)
         nil)
        ((test (car sequence))
         (cons (car sequence) (filter test (cdr sequence))))
        (else
         (filter test (cdr sequence)))))
|#

; Tests
; (fib 3)
; (accumulate * 1 (list 1 2 3))
; (accumulate cons nil (list 1 2 3))
; (odd? 2)
; (filter even? (list 1 2 3 4))
; (enumerate-tree (list (list 1 2)
;                       (list 3 (list 4 5))))
; (enumerate-interval 1 5)


;;;;;;;;;;;;;;;;;; Conventional Interface ;;;;;;;;;;;;;;;;;;;

; 1. combine processing modules as lists
; 2. localize data-stucture dependencies to just sequence operations

; a list of the squares of the first n+1 fib-number
(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square
        (map fib
             (enumerate-interval 0 n)))))

(list-fib-squares 2)
; (0 1 1 4 9 25)


; compute product of the squares of odd-int in a sequence
(define (product-of-squares-of-odd-elements sequence)
  (accumulate
   *
   1
   (map square (filter odd? sequence))))

; (product-of-squares-of-odd-elements (list 1 2 3 4 5))
; 225

#|
; conventional data-processing apps using sequence
(define (salary-of-highest-paid-programmer records)
  (accumulate
   max
   0
   (map salary (filter programmer? records))))
|#