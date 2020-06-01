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

;; PKG (Syntax)
(define (install-packages)
  ;; quote
  (define (eval-quote exp env)
    (cadr exp))
  ;; assignment
  (define (eval-assignment exp env)
    (set-variable-value! (cadr exp)
                         (meta-eval (caddr exp) env)
                         env))
  ;; define
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
  ;; lambda
  (define (make-lambda params body)
    (cons 'lambda (cons params body)))
  (define (eval-lambda exp env)
    (make-proc (cadr exp) (cddr exp) env))
  ;; let
  (define (let-variables exp)
    (map car (cadr exp)))
  (define (let-expressions exp)
    (map cadr (cadr exp)))
  (define (let-body exp)
    (cddr exp))
  (define (let->combination exp)
    (cons (make-lambda (let-variables exp) (let-body exp))
          (let-expressions exp)))
  (define (eval-let exp env)
    (meta-eval (let->combination exp) env))
  ;; if
  (define (make-if p c a) (list 'if p c a))
  (define (if-predication exp) (cadr exp))
  (define (if-consequence exp) (caddr exp))
  (define (if-alternation exp) (cadddr exp))
  (define (eval-if exp env)
    (cond [(meta-eval (if-predication exp) env)
           (meta-eval (if-consequence exp) env)]
          [else
           (meta-eval (if-alternation exp) env)]))
  ;; cond
  (define (eval-cond exp env)
    (define (cond->if exp)
      (expand-cond (cdr exp)))
    (define (expand-cond clauses)
      (cond [(null? clauses) #f]
            [else
             (let ([first (car clauses)] [rest (cdr clauses)])
               (cond [(eq? 'else (car first))
                      (if (null? rest)
                          (sequence->exp (cdr first))
                          (error "Clauses after else"))]
                     [else
                      (if (eq? '=> (cadr first))
                          (make-if (car first)
                                   (sequence->exp (cons (caddr first)
                                                        (meta-eval (car first) env)))
                                   (expand-cond rest))
                          (make-if (car first)
                                   (sequence->exp (cdr first))
                                   (expand-cond rest)))]))]))
    (meta-eval (cond->if exp) env))
  ;; begin
  (define (eval-begin exp env)
    (eval-sequence (cdr exp) env))

  (put 'eval 'quote eval-quote)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'def eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'cond eval-cond)
  (put 'eval 'begin eval-begin)
  (put 'eval 'let eval-let))

;; PRIMITIVE
(define prims
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'assoc assoc)))

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

(define (repl-loop)
  (newline)
  (display "<<<")
  (newline)
  (let ([in (read)])
    (let ([out (meta-eval in global-env)])
      (display ">>>")
      (newline)
      (display out)))
  (repl-loop))

;(repl-loop)

;(meta-eval '3 global-env)
;(meta-eval 'x (extend-env (list 'x 'y) (list 2 4) global-env))
;(meta-eval (quote (cons 1 2)) global-env)
;(meta-eval ''a global-env)
;(meta-eval '(begin (def z 0) (set! z 23)) global-env)
;(meta-eval '(begin (def x 0) (set! x 35)) global-env)
;(meta-eval '(def (inc x) (+ x 1)) global-env)
;(meta-eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) global-env)
;(meta-eval '(if (= x 0) x z) global-env)
;(meta-eval '(cond ((= x 0) '0) ((> x 0) 'x) (else 'z)) global-env)
;(meta-eval '(cond ((< x z) 'x) (else 'z)) global-env)
;(meta-eval '(cond ((assoc ''b '(('a 1) ('b 2))) => cadr) (else false)) global-env)
;(meta-eval '(+ 7 11) global-env)
;(meta-eval '(inc 100) global-env)
(meta-eval '(let ((a 1) (b 2)) (+ a b)) global-env)