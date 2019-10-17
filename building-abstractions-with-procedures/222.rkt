#lang sicp

(define square
  (lambda (x) (* x x)))

;; 第一个版本
;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons (square (car things))
;                    answer))))
;  (iter items nil))

;; 通过模拟估值过程可以解释为什么列表会调转
;(square-list (list 1 2 3 4))
;(iter (1 2 3 4) nil)
;(iter (2 3 4) (1))
;(iter (3 4) (4 1))
;(iter (4) (9 4 1))
;(iter () (16 9 4 1))
;(16 9 4 1)

;; 第二个版本
;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons answer
;                    (square 
;                     (car things))))))
;  (iter items nil))

;; 模拟其估值过程如下
;(square-list (list 1 2 3 4))
;(iter (1 2 3 4) nil)
;(iter (cdr (1 2 3 4)) (cons nil 1))
;(iter (cdr (2 3 4)) (cons (nil 1) 4))
;(iter (cdr (3 4)) (cons ((nil 1) 4) 9))
;(iter (cdr (4)) (cons (((nil 1) 4) 9) 16))
;(iter () ((((nil 1) 4) 9) 16))
;((((() . 1) . 4) . 9) . 16)
;; *注意*：最后的结果采用了 DrRacket 的打印方式