#lang sicp

;; environment
(define (make-frame vars vals) (cons vars vals))
(define frame-vars car)
(define frame-vals cdr)

(define (bind! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (first-frame env) (car env))
(define (external-env env) (cdr env))
(define empty-env '())

(define (extend-env vars vals env)
  (cond [(= (length vars) (length vals))
         (cons (make-frame vars vals) env)]
        [else
         (error "Too many / Too few args: " vars vals)]))

(define (lookup var env)
  (define (loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (loop (external-env env))]
            [(eq? var (car vars))
             (car vals)]
            [else
             (scan (cdr vars) (cdr vals))]))
    (cond [(null? env)
           (error "Unbound variable: " var)]
          [else
           (let ([scope (first-frame env)])
             (scan (frame-vars scope)
                   (frame-vals scope)))]))
  (loop env))

(define (set-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars)
             (bind! var val frame)]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else
             (scan (cdr vars) (cdr vals))]))
    (scan (frame-vars frame) (frame-vals frame))))

;; primitives in global-env
(define prims
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)))

(define (prim-names) (map car prims))
(define (prim-procs) (map (lambda (p) (list 'prim (cadr p))) prims))

(define (setup-global-env)
  (extend-env (prim-names) (prim-procs) empty-env))

(define (primitive? exp) (eq? (car exp) 'prim))
(define (apply-prim exp args) (apply (cadr exp) args))

;; syntax / expression
(define (self-eval? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))

(define (var? exp) (symbol? exp))

(define (quote? exp) (eq? (car exp) 'quote))
(define (text-of exp) (cadr exp))

(define (lambda? exp) (eq? (car exp) 'lambda))
(define (make-lambda params body) (cons 'lambda (cons params body)))
(define (make-proc params body env) (list 'proc params body env))

(define (func? exp) (eq? (car exp) 'func))
(define (func-var exp) (caadr exp))
(define (func-val exp) (make-lambda (cdadr exp) (cddr exp)))

(define (compound? exp) (eq? (car exp) 'proc))
(define (proc-params exp) (cadr exp))
(define (proc-body exp) (caddr exp))
(define (proc-env exp) (cadddr exp))

(define (if? exp) (eq? (car exp) 'if))
(define (p-of exp) (cadr exp))
(define (c-of exp) (caddr exp))
(define (a-of exp) (cadddr exp))

(define (application? exp) (pair? exp))
(define (operator-of exp) (car exp))
(define (operands-of exp) (cdr exp))

;; 3 -> 3
;; x -> 4 ; car -> #proc
;; 'foo => (quote foo) -> foo
;; (cond ...)
;; (lambda (x) (+ x 1)) -> #proc
;; (+ 2 3 4)
(define eval
  (lambda (exp env)
    (cond [(self-eval? exp) exp]
          [(var? exp) (lookup exp env)]
          [(quote? exp) (text-of exp)]
          ;; assign? evassign
          [(if? exp) (eval-if exp env)]
          ;;[(cond? exp) (eval-cond exp env)]
          [(lambda? exp) (make-proc (cadr exp) (cddr exp) env)]
          ;; begin? evseq
          [(func? exp) (eval-func exp env)]
          [(application? exp)
           (meta-apply (eval (operator-of exp) env)
                  (eval-list (operands-of exp) env))]
          [else
           (error "Unknown type of expression: " exp)])))

(define meta-apply
  (lambda (proc args)
    (cond [(primitive? proc)
           (apply-prim proc args)]
          [(compound? proc)
           (eval-seq (proc-body proc)
                     (extend-env (proc-params proc)
                                 args
                                 (proc-env proc)))] ;; bind
          [else
           (error "Unkonwn procedure: " proc)])))

(define (eval-if exp env)
  (cond [(eval (p-of exp) env)
         (eval (c-of exp) env)]
        [else
         (eval (a-of exp) env)]))

(define (eval-list operands env)
  (cond [(null? operands)
         '()]
        [else
         (cons (eval (car operands) env)
               (eval-list (cdr operands) env))]))

(define (eval-seq exps env)
  (cond [(null? (cdr exps))
         (eval (car exps) env)]
        [else
         (eval (car exps) env)
         (eval-seq (cdr exps) env)]))

(define (eval-func exp env)
  (set-variable! (func-var exp)
                 (eval (func-val exp) env)
                 env))
;; REPL
(define global-env (setup-global-env))

(define (repl-loop)
  (newline)
  (display "<<<")
  (newline)
  (let ([in (read)])
    (let ([out (eval in global-env)])
      (display ">>>")
      (newline)
      (display out)))
  (repl-loop))

(repl-loop)

;; test cases
;(eval '(+ 1 2) global-env)
;(eval '(* (+ 1 2) (+ 3 4 5) 6) global-env)
;(eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) global-env)