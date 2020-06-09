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

(define (cond->if exp)
  (define (make-if p c a)
    (list 'if p c a))
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
  (expand-cond (cdr exp)))

(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))

(define (variable? exp)
  (symbol? exp))

(define (quote? exp)
  (eq? 'quote (car exp)))

(define (assignment? exp)
  (eq? 'set! (car exp)))

(define (definition? exp)
  (eq? 'define (car exp)))

(define (if? exp)
  (eq? 'if (car exp)))

(define (lambda? exp)
  (eq? 'lambda (car exp)))

(define (begin? exp)
  (eq? 'begin (car exp)))

(define (cond? exp)
  (eq? 'cond (car exp)))

(define (application? exp)
  (pair? exp))

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

;; ANALYZER
(define (analyze-self exp)
  (lambda (env) exp))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-quote exp)
  (let ([qval (cadr exp)])
    (lambda (env) qval)))

(define (analyze-assignment exp)
  (let ([var (cadr exp)]
        [vproc (meta-analyze (caddr exp))])
    (lambda (env)
      (set-variable-value! var (vproc env) env))))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (meta-analyze (definition-value exp))])
    (lambda (env)
      (define-variable! var (vproc env) env))))

(define (analyze-if exp)
  (let ([pproc (meta-analyze (cadr exp))]
        [cproc (meta-analyze (caddr exp))]
        [aproc (meta-analyze (cadddr exp))])
    (lambda (env)
      (if (pproc env)
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ([vars (cadr exp)]
        [bproc (analyze-sequence (cddr exp))])
    (lambda (env)
      (make-proc vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially p1 p2)
    (lambda (env) (p1 env) (p2 env)))
  (define (loop first-p rest-p)
    (if (null? rest-p)
        first-p
        (loop (sequentially first-p (car rest-p))
              (cdr rest-p))))
  (let ([procs (map meta-analyze exps)])
    (if (null? procs)
        (error "An empty sequence expression to analyze.")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ([proc (meta-analyze (car exp))]
        [args (map meta-analyze (cdr exp))])
    (lambda (env)
      (meta-execute (proc env)
                    (map (lambda (arg) (arg env)) args)))))

;; ANALYZE-EXECUTE
(define (meta-eval exp env)
  ((meta-analyze exp) env))

(define (meta-analyze exp)
  (cond [(self-evaluating? exp) (analyze-self exp)]
        [(variable? exp) (analyze-variable exp)]
        [(quote? exp) (analyze-quote exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (cdr exp))]
        [(cond? exp) (meta-analyze (cond->if exp))]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown type of expression: " exp)]))

(define (meta-execute proc args)
  (cond [(primitive? proc)
         (apply-prim proc args)]
        [(compound? proc)
         ((proc-body proc)
          (extend-env (proc-params proc) args (proc-env proc)))]
        [else
         (error "Unkonwn procedure: " proc)]))

;; TEST
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
;(meta-eval '(quote (cons 1 2)) global-env)
;(meta-eval ''a global-env)
;(meta-eval '(begin (define z 0) (set! z 23)) global-env)
;(meta-eval '(begin (define x 0) (set! x 35)) global-env)
;(meta-eval '(define (inc x) (+ x 1)) global-env)
;(meta-eval '((lambda (y) (+ 3 y)) 4) global-env)
;(meta-eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) global-env)
;(meta-eval '(if (= x 0) x z) global-env)
;(meta-eval '(cond ((= x 0) '0) ((> x 0) 'x) (else 'z)) global-env)
;(meta-eval '(cond ((< x z) 'x) (else 'z)) global-env)
;(meta-eval '(+ 7 11) global-env)
;(meta-eval '(inc 100) global-env)

(meta-eval '(define (loop n) (if (> n 0) (loop (- n 1)) 'ok)) global-env)
(meta-eval '(loop 1000000) global-env)