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
(define (tagged-list? data tag)
  (if (pair? data)
      (eq? (car data) tag)
      false))

(define (self-eval? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))

(define (var? exp) (symbol? exp))

(define (quote? exp) (tagged-list? exp 'quote))
(define (text-of exp) (cadr exp))

(define (lambda? exp) (tagged-list? exp  'lambda))
(define (make-lambda params body) (cons 'lambda (cons params body)))
(define (make-proc params body env) (list 'proc params body env))

(define (func? exp) (tagged-list? exp 'func))
(define (func-var exp) (caadr exp))
(define (func-val exp) (make-lambda (cdadr exp) (cddr exp)))

(define (compound? exp) (tagged-list? exp 'proc))
(define (proc-params exp) (cadr exp))
(define (proc-body exp) (caddr exp))
(define (proc-env exp) (cadddr exp))

(define (if? exp) (tagged-list? exp 'if))
(define (p-of exp) (cadr exp))
(define (c-of exp) (caddr exp))
(define (a-of exp) (cadddr exp))

(define (application? exp) (pair? exp))
(define (operator-of exp) (car exp))
(define (operands-of exp) (cdr exp))

;; define
(define (define? exp) (tagged-list? exp 'define))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars)
             (bind! var val frame)]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else
             (scan (cdr vars) (cdr vals))]))
    (scan (frame-vars frame) (frame-vals frame))))

(define (def-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (def-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (eval-definition exp env)
  (define-variable!
    (def-variable exp)
    (eval (def-value exp) env)
    env))

;; set!
(define (assignment? exp) (tagged-list? exp 'set!))

(define (set-variable-value! var val env)
  (define (loop env)
    (define (scan vars vals)
      (cond [(null? vars) (loop (external-env env))]
            [(eq? var (car vars)) (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env empty-env)
        (error "Unbound variable: " var)
        (let ([frame (first-frame env)])
          (scan (frame-vars frame) (frame-vals frame)))))
  (loop env))

(define (eval-assignment exp env)
  (set-variable-value! (cadr exp)
                       (eval (caddr exp) env)
                       env))

;; thunk
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (evaluated-thunk? obj) (tagged-list? obj 'evluated-thunk))
(define (thunk-exp obj) (cadr obj))
(define (thunk-val obj) (cadr obj))
(define (thunk-env obj) (caddr obj))

(define (actual-value exp env)
  (force-it (eval exp env)))

;(define (force-it obj)
;  (if (thunk? obj)
;      (actual-value (thunk-exp obj) (thunk-env obj))
;      obj))
(define (force-it obj)
  (cond [(thunk? obj)
         (let ([result (actual-value (thunk-exp obj) (thunk-env obj))])
           (set-car! obj 'evaluated-thunk) ; change tag
           (set-car! (cdr obj) result) ; replace exp with value
           (set-cdr! (cdr obj) '()) ; forget env
           result)]
        [(evaluated-thunk? obj) (thunk-val obj)]
        [else obj]))
     
(define (delay-it exp env)
  (list 'thunk exp env))

;; delayed if
(define (eval-if exp env)
  (if (actual-value (p-of exp) env)
      (eval (c-of exp) env)
      (eval (a-of exp) env)))

(define (eval-list operands env)
  (cond [(null? operands)
         '()]
        [else
         (cons (actual-value (car operands) env) ; to get real value
               (eval-list (cdr operands) env))]))

(define (uneval-list operands env)
  (if (null? operands)
      '()
      (cons (delay-it (car operands) env)
            (uneval-list (cdr operands) env))))

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
          [(if? exp) (eval-if exp env)]
          [(lambda? exp) (make-proc (cadr exp) (cddr exp) env)]
          [(define? exp) (eval-definition exp env)]
          [(assignment? exp) (eval-assignment exp env)]
          [(func? exp) (eval-func exp env)]
          ;; lazy-eval here
          [(application? exp)
           (meta-apply (actual-value (operator-of exp) env) ; force
                       (operands-of exp) ; not evaluating operands here
                       env)]
          [else
           (error "Unknown type of expression: " exp)])))

;; apply needs env to get the real-value of args
(define meta-apply
  (lambda (proc args env)
    (cond [(primitive? proc)
           (apply-prim proc
                       (eval-list args env))] ; force
          [(compound? proc)
           (eval-seq (proc-body proc)
                     (extend-env (proc-params proc)
                                 (uneval-list args env) ; delayed
                                 (proc-env proc)))] ;; bind
          [else
           (error "Unkonwn procedure: " proc)])))

;; test cases
(define global-env (setup-global-env))
(eval '(+ 1 2) global-env)
(eval '(* (+ 1 2) (+ 3 4 5) 6) global-env)
(eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) global-env)

;; REPL
(define (repl-loop)
  (newline)
  (display "<<<")
  (newline)
  (let ([in (read)])
    (let ([out (actual-value in global-env)])
      (display ">>>")
      (newline)
      (display out)))
  (repl-loop))

(repl-loop)