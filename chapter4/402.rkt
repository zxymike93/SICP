#lang sicp

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env)]
        [(begin? exp) (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [(application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env))]
        [else
         (error "Unknown type of expression: " exp)]))

;; 问题 1：如果将上面 cond 的子句 application? 放到 assignment? 前面，会出现什么错误？
;; 从 application? 和其它相关的过程定义可以看出，相当多的表达式会被断言为真
;; （所以将 application? 放在条件的最后一个子句（如果不加 error 的话）实际上是作为默认的情况）
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;; 那么，比如执行 (define x 3)
;; 会被解释为 (apply (eval 'define env) (list-of-values '(x 3) env))
;; 是错误的解析行为

;; 问题 2：使用 (call + 1 2) 这样的语法来修改 application?，使得可以将它正常地放在 assignment? 前面。
;; 使用其它判断子句所用的「带 tag」的方法来标明，是可以变动 assignment? 的顺序
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (application? exp)
  (tagged-list? exp 'call))