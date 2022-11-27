#lang sicp

;;; 本例是一个使用替换模型的 Scheme 解释器，即不包含环境、赋值等特性。
;;; 具体地说，仅包含 Scheme 的基础类型、基本过程，以及 if quote lambda。
;;; 而其中基本过程，使用原生的解释器提供的过程。见 scheme-eval scheme-apply。

;;; 本例主要在 calc 的基础上进一步说明 eval-apply 是如何构成求值循环（树状递归）的。
;;; 1. eval 包含四种情况：原子性的（常量、变量）和列表形式（关键字、过程调用）。
;;;    其中，过程调用通过保留 lambda 传递给 apply 实现过程抽象。
;;; 2. apply 包含两种情况：内置过程（以过程对象作为参数传递）、用户定义过程（以 '(lambda ...) 作为参数传递）。
;;;    其中，用户定义过程即上述 lambda 表达式，经过替换模型再一次 eval 表达式。

;;; 另外，通过本例可以看到，解释器主要可以实现：
;;; 1. 语法定义，比如 if quote lambda
;;; 2. 求值顺序，比如 call-expr? 条件下定义先 eval 再 apply
;;; 但解释器不解决更底层的问题：
;;; 1. 最基本的数据如何表示、基本的过程指令有哪些
;;; 2. 对象（数据、过程等）如何存储

(define (tiny-repl)
  (display (tiny-eval (read)))
  (newline)
  (tiny-repl))

(define (tiny-eval expr)
  (cond ((self-eval? expr) expr)
        ((global-var? expr) (scheme-eval expr))  ;; Underlying MIT-Scheme
        ((special-form? expr) (special-eval expr))
        ((call-expr? expr) (tiny-apply (tiny-eval (car expr))
                                       (map tiny-eval (cdr expr))))
        (else (error "Bad expr: " expr))))

(define (tiny-apply proc args)
  (cond ((procedure? proc) (scheme-apply proc args))  ;; Underlying MIT-Scheme
        ((lambda? proc) (tiny-eval (substitute (caddr proc)
                                               (cadr proc)
                                               args)))
        (else (error "Bad proc: :" proc))))

;;; 基本类型

;; '3
;; '"hello"
;; '#t
(define (self-eval? expr)
  (or (number? expr) (string? expr) (boolean? expr)))

;;; 变量

(define global-var? symbol?)

;; 'cons => #<procedure:cons>
;; 'x => #<value:x>
(define (scheme-eval expr)
  (eval expr (scheme-report-environment 5)))

;;; 关键字

(define (expr-type? expr symb) (and (pair? expr) (eq? symb (car expr))))
(define (quote? expr) (expr-type? expr 'quote))
(define (if? expr) (expr-type? expr 'if))
(define (lambda? expr) (expr-type? expr 'lambda))


(define (special-form? expr)
  (or (quote? expr)
      (if? expr)
      (lambda? expr)
      ))

;; '(lambda (x) (* x x)) => '(lambda (x) (* x x))
;; '(if <predicate> <consequence> <alternative>) => #<value:consequence>
;; '(quote x) => 'x
(define (special-eval expr)
  (cond ((quote? expr) (cadr expr))
        ((if? expr) (if (tiny-eval (cadr expr))
                        (tiny-eval (caddr expr))
                        (tiny-eval (cadddr expr))))
        ((lambda? expr) expr)))  ;; to substitute

;;; 过程调用

(define call-expr? pair?)

;; #<procedure:cons '(1 2) => (1 2)
;; #<procedure:+ '(1 2) => 3
(define (scheme-apply proc args)
  (apply proc args))

;;; 替换模型

(define local-var? symbol?)

;; '(* x x) 'x '5 => (list '+ 5 5)
(define (substitute expr params args)
  (cond ((self-eval? expr) expr)
        ((local-var? expr) (lookup expr params args))
        (else (map (lambda (subexpr) (substitute subexpr params args))
                   expr))))

(define (lookup name params args)
  (cond ((null? params) name)
        ((eq? name (car params)) (car args))
        (else (lookup name (cdr params) (cdr args)))))

(tiny-repl)
