#lang racket

;; 书中 eval 使用了 list-of-values 来对组合表达式求值
;; ((application? exp)
;;  (apply (eval (operator exp) env)
;;         (list-of-values (operands exp) env)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 这里 apply 到底是从左到右对 operands 求值，还是从右到左，取决于 Lisp 的具体实现
;; 改为明确的从左到右估值
(define (list-of-values-l2r exps env)
  (cond [(no-operands? exps) '()]
        [else (let ([left (eval (first-operand exps) env)])
                (cons left (list-of-values (rest-operands exps) env)))]))
;; 改为明确的从右到左估值
(define (list-of-values-r2l exps env)
  (cond [(no-operands? exps) '()]
        [else (let ([right (list-of-values (rest-operands exps) env)])
                (cons (eval (first-operand exps) env) right))]))

;; 补充：我认为题目所说的*实现*特指 cons 而不是整个 Lisp 实现，否则包括 let 在内的语法的语义都是不能确定的。