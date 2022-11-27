#lang sicp

;;; 本例是一个前缀计算器，支持 Scheme 中的数值的四则运算。
;;; 它是学习 Scheme 解释器的第一步，介绍了 repl/eval/apply 三个基本过程。
;;; 1. repl 是一个无限循环，将读取的表达式传入 eval 解析，并打印结果。
;;; 2. eval 解析表达式，把它们拆解成过程、参数再由 apply 来执行。
;;; 3. apply 接收过程、参数列表，并对相应过程。

(define (calc-repl)
  (display (calc-eval (read)))
  (newline)
  (calc-repl))

;; '3 => 3
;; '(* (/ 6 2) (+ 1 3)) => 12
(define (calc-eval expr)
  (cond ((number? expr) expr)
        ;; 采用应用序，先 eval 算式中的子算式。
        ;; 由于算式首元素是 '+ '- '* '/，无需对其 eval。
        ((pair? expr) (calc-apply (car expr)
                                  (map calc-eval (cdr expr))))
        (else (error "Bad expr: " expr))))

;; '+ '(2 4) => 6
(define (calc-apply proc args)
  (cond ((eq? proc '+) (accumulate + 0 args))
        ((eq? proc '-) (cond ((null? args) (error "Bad args: " args))
                                ((= 1 (length args)) (- (car args)))
                                (else (accumulate - (car args) (cdr args)))))
        ((eq? proc '*) (accumulate * 1 args))
        ((eq? proc '/) (cond ((null? args) (error "Bad args: " args))
                                ((= 1 (length args) (/ (car args))))
                                (else (accumulate / (car args) (cdr args)))))
        (else (error "Bad proc: " proc))))

(define (accumulate term next args)
  (if (null? args)
      next
      (accumulate term (term next (car args)) (cdr args))))

(calc-repl)