#lang sicp

;; 以书中的例子来展开解释
(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
  (* x z))
;; 如果用 let 的形式，被展开为
(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))

;; 如果将 let* 作为 let 的语法糖来实现 let*->nested-lets，需要以下过程
(define (let*->nested-lets exp)
  (define (to-lets params body)
    (let ([rest (cdr params)])
      (if (not (null? rest))
          (cons (list 'let (list (car params)))
                (to-lets rest body))
          (list 'let (list (car params)) body))))
  (to-lets (cadr exp) (cddr exp)))

;; 如果要直接实现 let*，但因为这里 Lisp 的实现还是基于 lambda 表达式
;; 也就是说，还是要像 4.6 一样将其转换为 lambda 的写法
;; 这样效率并不会有提升，会重复代码，而且可读性没有使用 let*->nested-lets 这样来得好
