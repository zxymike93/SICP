#lang sicp

;; TABLE
(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value)) (cdr local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define table (make-table))
(define get (table 'lookup-proc))
(define put (table 'insert-proc!))

(define (type obj)
  (if (pair? obj)
      (car obj)
      (error "Bad Tagged Object." obj)))

(define (contents obj)
  (if (pair? obj)
      (cdr obj)
      (error "Bad Tagged Object." obj)))

;; PKG
; quote
(define (install-package-quote)
  (define (eval-quote exp env)
    (cadr exp))
  (put 'eval 'quote eval-quote))

; set!
(define (install-package-assignment)
  (define (eval-assignment exp env)
    (set-variable-value! (cadr exp)
                         (meta-eval (caddr exp) env)
                         env))
  (put 'eval 'set! eval-assignment))

; def
(define (install-package-definition)
  ;; form 1: (def x 1)
  ;; form 2: (def (fn x) x)
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
      (meta-eval (def-value exp) env)
      env))
  (put 'eval 'def eval-definition))

; lambda
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (install-package-lambda)
  (define (eval-lambda exp env)
    (make-proc (cadr exp) (cddr exp) env))
  (put 'eval 'lambda eval-lambda))

; condition
(define (install-package-condition)
  ; if
  (define (make-if p c a) (list 'if p c a))
  (define (if-predication exp) (cadr exp))
  (define (if-consequence exp) (caddr exp))
  (define (if-alternation exp) (cadddr exp))
  (define (eval-if exp env)
    (cond [(meta-eval (if-predication exp) env)
           (meta-eval (if-consequence exp) env)]
          [else
           (meta-eval (if-alternation exp) env)]))
  ; (cond (<p> <c>) (else <a>)) => (if <p> <c> <a>)
  (define (expand-cond clauses)
    (if (null? clauses) #f
        (let ([first (car clauses)] [rest (cdr clauses)])
          (if (eq? (car first) 'else)
              (if (null? rest)
                  (sequence->exp (cdr first))
                  (error "Clauses after else"))
              (make-if (car first)
                       (sequence->exp (cdr first))
                       (expand-cond rest))))))
  (define (cond->if exp) (expand-cond (cdr exp)))
  (define (eval-cond exp env)
    (meta-eval (cond->if exp) env))
  ; (and (> x 0) (< x 100)) => (if (> x 0) (if (< x 100) #t #f) #f)
  (define (eval-and exp env)
    (define (and->if exp) (expand-and (cdr exp)))
    (define (expand-and exps)
      (cond [(null? exps) 'true]
            [(null? (cdr exps)) (car exps)]
            [else (make-if (car exps)
                           (expand-and (cdr exps))
                           'false)]))
    (meta-eval (and->if exp) env))
  ; (or (= x 0) (< x 0)) => (if (= x 0) #t (if (< x 0) #t #f))
  (define (eval-or exp env)
    (define (or->if exp) (expand-or (cdr exp)))
    (define (expand-or exps)
      (cond [(null? exps) 'false]
            [(null? (cdr exps)) (car exps)]
            [else (make-if (car exps)
                           'true
                           (expand-or (cdr exps)))]))
    (meta-eval (or->if exp) env))
  (put 'eval 'if eval-if)
  (put 'eval 'cond eval-cond)
  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or))

; begin
(define (install-package-begin)
  (define (eval-begin exp env)
    (eval-sequence (cdr exp) env))
  (put 'eval 'begin eval-begin))

(define (install-packages)
  (install-package-quote)
  (install-package-assignment)
  (install-package-definition)
  (install-package-lambda)
  (install-package-condition)
  (install-package-begin))

;; PRIMITIVE
(define prims
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)))

(define (prim-names) (map car prims))
(define (prim-procs) (map (lambda (p) (list 'prim (cadr p))) prims))
(define (primitive? exp) (eq? (car exp) 'prim))
(define (apply-prim exp args) (apply (cadr exp) args))

;; ENV
(define (make-frame vars vals) (cons vars vals))
(define (frame-vars frame) (car frame))
(define (frame-vals frame) (cdr frame))
(define empty-env '())
(define (first-frame env) (car env))
(define (external-env env) (cdr env))

(define (bind! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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

(define (lookup-variable-value var env)
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

(define (extend-env vars vals env)
  (cond [(= (length vars) (length vals))
         (cons (make-frame vars vals) env)]
        [else
         (error "Too many / Too few args: " vars vals)]))

(define (setup-global-env)
  (let ([builtins (extend-env (prim-names) (prim-procs) empty-env)])
    (define-variable! 'true true builtins)
    (define-variable! 'false false builtins)
    builtins))

;; COMPOUND
(define (make-proc params body env) (list 'proc params body env))
(define (compound? exp) (eq? (car exp) 'proc))
(define (proc-params exp) (cadr exp))
(define (proc-body exp) (caddr exp))
(define (proc-env exp) (cadddr exp))

;; HELPER
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(null? (cdr seq)) (car seq)]
        [else (cons 'begin seq)]))

(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))

(define (variable? exp)
  (symbol? exp))

(define (list-of-values operands env)
  (cond [(null? operands)
         '()]
        [else
         (cons (meta-eval (car operands) env)
               (list-of-values (cdr operands) env))]))

(define (eval-sequence exps env)
  (cond [(null? (cdr exps))
         (meta-eval (car exps) env)]
        [else
         (meta-eval (car exps) env)
         (eval-sequence (cdr exps) env)]))

;; EVAL-APPLY
(define (meta-eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(pair? exp) (cond [(get 'eval (type exp)) ((get 'eval (type exp)) exp env)]
                            [else (meta-apply (meta-eval (car exp) env)
                                              (list-of-values (cdr exp) env))])]
        [else (error "Unknown type of expression: " exp)]))

(define (meta-apply proc args)
  (cond [(primitive? proc)
         (apply-prim proc args)]
        [(compound? proc)
         (eval-sequence (proc-body proc)
                        (extend-env (proc-params proc)
                                    args
                                    (proc-env proc)))] ;; bind
        [else
         (error "Unkonwn procedure: " proc)]))

;; TEST
(install-packages)
(define global-env (setup-global-env))

(meta-eval '3 global-env)
(meta-eval 'x (extend-env (list 'x 'y) (list 2 4) global-env))
(meta-eval (quote (cons 1 2)) global-env)
(meta-eval '(begin (def z 0) (set! z 23)) global-env)
(meta-eval '(begin (def x 0) (set! x 35)) global-env)
(meta-eval '(def (inc x) (+ x 1)) global-env)
(meta-eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) global-env)
(meta-eval '(if (= x 0) x z) global-env)
(meta-eval '(cond ((= x 0) '0) ((> x 0) 'x) (else 'z)) global-env)
(meta-eval '(cond ((< x z) 'x) (else 'z)) global-env)
(meta-eval '(and (< 1 0) (> 2 1)) global-env)
(meta-eval '(and (> 1 0) (> 2 3)) global-env)
(meta-eval '(and (= 1 1) (= 0 0)) global-env)
(meta-eval '(or (= 1 2) (= 1 1)) global-env)
(meta-eval '(or (= 1 2) (= 1 3)) global-env)
(meta-eval '(+ 7 11) global-env)
(meta-eval '(inc 100) global-env)