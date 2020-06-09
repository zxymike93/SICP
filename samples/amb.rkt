#lang sicp
(#%require (only racket/base make-hash hash-ref hash-set!))

;; table
(define *table* (make-hash))
(define (get op type) (hash-ref *table* (list op type) #f))
(define (put op type proc) (hash-set! *table* (list op type) proc))

;; primitive
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

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #t))

;; environment
(define (make-frame vars vals) (cons vars vals))
(define (frame-vars frame) (car frame))
(define (frame-vals frame) (cdr frame))

(define empty-env '())
(define (first-frame env) (car env))
(define (external-env env) (cdr env))

(define (bind! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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

;; data structure
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? tag (car exp))
      false))

(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (quoted-text exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (cond? exp) (tagged-list? exp 'cond))
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

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq) (cons 'begin seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (primitive? exp) (eq? (car exp) 'prim))
(define (prim-names) (map car prims))
(define (prim-procs) (map (lambda (p) (list 'prim (cadr p))) prims))

(define (compound? exp) (eq? (car exp) 'proc))
(define (proc-params exp) (cadr exp))
(define (proc-body exp) (caddr exp))
(define (proc-env exp) (cadddr exp))
(define (make-proc params body env) (list 'proc params body env))

;; amb
;; (amb <e1> <e2> ... <en>) -> 任一元素 e
;; (amb) -> error
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; expression types
(define (analyze-self-evaluating exp)
  (lambda [env succeed fail] (succeed exp fail)))

(define (analyze-quoted exp)
  (let ([qval (quoted-text exp)])
    (lambda [env succeed fail] (succeed qval fail))))

(define (analyze-variable exp)
  (lambda [env succeed fail]
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    ;(lambda [env]
    ;  (set-variable-value! 
    ;   var (vproc env) env)
    ;  'ok)))
    (lambda [env succeed fail]
      (vproc env
             (lambda [val fail2] ; succ continuation
               (let ([old-val (lookup-variable-value var env)])
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda [] ; fail continuation
                            (set-variable-value! var old-val env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    ;(lambda [env]
    ;  (define-variable! var (vproc env) env)
    ;  'ok)))
    (lambda [env succeed fail]
      (vproc env
             (lambda [val fail2]
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    ;(lambda (env)
    ;  (if (true? (pproc env))
    ;      (cproc env)
    ;      (aproc env)))))
    (lambda [env succeed fail]
      (pproc env
             (lambda [pred-value fail2]
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    ;(lambda (env) (proc1 env) (proc2 env)))
    (lambda [env succeed fail]
      (proc1 env
             (lambda [a-value fail2] (proc2 env succeed fail2))
             fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda [env succeed fail]
      (succeed (make-proc vars bproc env) fail))))

;; key
(define (analyze-amb exp)
  (let ([cprocs (map analyze (amb-choices exp))])
    (lambda [env succeed fail]
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda [] (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-application exp)
  (define (get-args aprocs env succeed fail)
    (if (null? aprocs)
        (succeed '() fail)
        ((car aprocs) env
                      (lambda [arg fail2]
                        (get-args (cdr aprocs)
                                  env
                                  (lambda [args fail3] (succeed (cons arg args) fail3))
                                  fail2))
                      fail)))
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    ;(lambda [env]
    ;  (execute-application 
    ;   (fproc env)
    ;   (map (lambda [aproc] (aproc env)) aprocs)))
    (lambda [env succeed fail]
      (fproc env
             (lambda [proc fail2]
               (get-args aprocs
                         env
                         (lambda [args fail3]
                           (execute-application proc args succeed fail3))
                         fail2))
             fail))))

;; eval
(define (ambeval exp env succeed fail) ; add succeed/fail continuation
  ((analyze exp) env succeed fail))

;; analyze
(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze-if (cond->if exp))]
        [(amb? exp) (analyze-amb exp)] ; new
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: " exp)]))

;; execute
(define (apply-primitive-procedure exp args)
  (apply (cadr exp) args))

;; 像 expressions 一样，只是简单地传递和调用 succeed/fail continuation
(define (execute-application proc args succeed fail)
  (cond [(primitive? proc)
         (succeed (apply-primitive-procedure proc args) fail)]
        [(compound? proc)
         ((proc-body proc) (extend-env (proc-params proc)
                                       args
                                       (proc-env proc))
                           succeed
                           fail)]
        [else
         (error "Unknown procedure type: " proc)]))

;; repl
(define the-global-environment (setup-global-env))
(define (repl)
  (define (prompt-for-input string)
    (newline) (newline) (display string) (newline))
  (define (announce-output string)
    (newline) (display string) (newline))
  (define (user-print object)
    (if (compound? object)
        (display (list 'compound-procedure
                       (proc-params object)
                       (proc-body object)
                       '<procedure-env>))
        (display object)))

  (define (loop try-again)
    (prompt-for-input ";;; M-Eval input:")
    (let ([input (read)])
      (if (eq? input 'try-again)
          (try-again)
          (begin (newline) (display ";;; Starting a new problem ")
                 (ambeval input
                          the-global-environment
                          (lambda [val next-alternative]
                            (announce-output ";;; M-Eval value:")
                            (user-print val)
                            (loop next-alternative))
                          (lambda []
                            (announce-output ";;; There are no more values of ")
                            (user-print input)
                            (repl)))))))

  (loop (lambda []
          (newline)
          (display ";;; There is no current problem")
          (repl))))