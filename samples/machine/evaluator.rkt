;;; ENVIRONMENT
 
(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         '())))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define primitive-procedures
  (list (list 'car car) (list 'cdr cdr) (list 'cons cons)
        (list 'null? null?) (list 'pair? pair?)
        (list 'list list) (list 'append append)
        (list '+ +) (list '- -) (list '* *) (list '/ /)
        (list '= =) (list '> >) (list '< <) (list '>= >=) (list '<= <=)
        ;; more
        ))

(define (define-variable! var val env)
  (let ((frame (first-frame  env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-bindings-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (extend-environment vars vals env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-bindings-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))


;;; OPERATIONS

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cons 'lambda (cons (cdadr exp) (cddr exp)))))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-proc parameters body env)
  (list 'procedure parameters body env))

(define (apply-primitive-procedure proc args)
  (apply (cadr proc) args))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print obj)
  (if (tagged-list? obj 'procedure)
      (display (list 'compound-procedure
                     (cadr obj)
                     (caddr obj)
                     (cadddr obj)))
      (display obj)))

(define eceval-operations
  (list
    (list 'self-evaluating? (lambda (exp) (or (number? exp) (string? exp))))
    (list 'variable? symbol?)
    (list 'quoted? (lambda (exp) (tagged-list? exp 'quote)))
    (list 'assignment? (lambda (exp) (tagged-list? exp 'set!)))
    (list 'definition? (lambda (exp) (tagged-list? exp 'define)))
    (list 'if? (lambda (exp) (tagged-list? exp 'if)))
    (list 'lambda? (lambda (exp) (tagged-list? exp 'lambda)))
    (list 'begin? (lambda (exp) (tagged-list? exp 'begin)))
    (list 'application? pair?)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'text-of-quotation cadr)
    (list 'assignment-variable cadr)
    (list 'assignment-value caddr)
    (list 'set-variable-value! set-variable-value!)
    (list 'definition-variable definition-variable)
    (list 'definition-value definition-value)
    (list 'define-variable! define-variable!)
    (list 'if-predicate cadr)
    (list 'if-consequent caddr)
    (list 'if-alternative if-alternative)
    (list 'true? (lambda (exp) (eq? exp true)))
    (list 'lambda-parameters cadr)
    (list 'lambda-body cddr)
    (list 'make-procedure make-proc)
    (list 'begin-actions cdr)
    (list 'first-exp car)
    (list 'rest-exps cdr)
    (list 'last-exp? (lambda (exp) (null? (cdr exp))))
    (list 'operator car)
    (list 'operands cdr)
    (list 'empty-arglist (lambda () '()))
    (list 'no-operands? null?)
    (list 'first-operand car)
    (list 'rest-operands cdr)
    (list 'last-operand? (lambda (exp) (null? (cdr exp))))
    (list 'adjoin-arg (lambda (arg argl) (append argl (list arg))))
    (list 'primitive-procedure? (lambda (exp) (tagged-list? exp 'primitive)))
    (list 'compound-procedure? (lambda (exp) (tagged-list? exp 'procedure)))
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'procedure-parameters cadr)
    (list 'procedure-environment cadddr)
    (list 'procedure-body caddr)
    (list 'extend-environment extend-environment)
    (list 'prompt-for-input prompt-for-input)
    (list 'read read)
    (list 'get-global-environment (lambda () the-global-environment))
    (list 'announce-output announce-output)
    (list 'user-print user-print)))


;;; EVAL MACHINE

(define eceval-machine
  '(;;; REPL
    read-eval-print-loop
      (perform (op init-stack))
      (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label eval-dispatch))

    print-result
      (perform (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))

    eval-dispatch
      (test (op self-evaluating?) (reg exp)) (branch (label ev-self-eval))
      (test (op variable?) (reg exp)) (branch (label ev-variable))
      (test (op quoted?) (reg exp)) (branch (label ev-quoted))
      (test (op assignment?) (reg exp)) (branch (label ev-assignment))
      (test (op definition?) (reg exp)) (branch (label ev-definition))
      (test (op if?) (reg exp)) (branch (label ev-if))
      (test (op lambda?) (reg exp)) (branch (label ev-lambda))
      (test (op begin?) (reg exp)) (branch (label ev-begin))
      (test (op application?) (reg exp)) (branch (label ev-application))
      (goto (label unknown-expression-type))

    ;; ev-self-eval ev-variable ev-quoted ev-lambda 为最基本的表达式
    ;; 它们均将求值结果保存到 val 后回到 continue 所保存的指令执行点
    ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))
    ev-variable
      (assign val (op lookup-variable-value) (reg exp) (reg env))
      (goto (reg continue))
    ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))
    ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
      (goto (reg continue))

    ;; 将原来的 continue 以及当前 exp env 入栈，对 predicate 求值
    ;; 根据求值结果进入 ev-if-consequent / ev-if-alternative 分支
    ev-if
      (save exp)
      (save env)
      (save continue)
      (assign continue (label ev-if-predicate))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))
    ev-if-predicate
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))
    ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))
    ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))

    ;; ev-assignment ev-definition 类似
    ;; 首先获取变量、表达式，在基本 ev 求值过后，将 val 赋给 var 绑定到 env
    ev-assignment
      (assign unev (op assignment-variable) (reg exp))
      (save unev)
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))
    ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op set-variable-value!) (reg unev) (reg val) (reg env))  ;; 为变量赋值
      (assign val (const ok))
      (goto (reg continue))
    ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))
    ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op define-variable!) (reg unev) (reg val) (reg env))  ;; 为变量赋值
      (assign val (const ok))
      (goto (reg continue))
      
    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))

    ;; ev-begin compound-apply 会跳转到此
    ;; ev-sequence ev-sequence-continue 形成循环
    ;; ev-sequence-last-exp 为 base case
    ev-sequence
      (assign exp (op first-exp) (reg unev))  ;; body 被保存在 unev
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      ;; 几个基本 ev 跳转到 continue，以此形成循环
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-last-exp
      (restore continue)
      ;; 不使用栈，实现尾递归优化
      (goto (label eval-dispatch))

    ;; ev-application 要分别对 exp 里的 operator operands 递归求值
    ev-application
      (save continue)
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label eval-dispatch))
    ev-appl-did-operator
      (restore unev)
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val))
      ;; 无参过程直接 apply
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      ;; 有参过程则将 proc 入栈
      (save proc)
    ;; ev-appl-operand-loop 遍历 operands
    ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label eval-dispatch))
    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))  ;; 将 operand 求值结果添加到 argl
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))
    ;; 尾递归优化
    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))
    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))

    ;;; APPLY
    ;; 进入 apply-dispatch 时已获得“参数” proc argl
    ;; 执行完成后则返回 continue 求值下一条指令
    apply-dispatch
      (test (op primitive-procedure?) (reg proc)) (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc)) (branch (label compound-apply))
      (goto (label unknown-procedure-type))

    ;; apply and goto eval-dispatch
    primitive-apply
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (restore continue)
      (goto (reg continue))

    compound-apply
      ;; 将 arguments 绑定到当前 environment
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
      ;; 对 body 使用 ev-sequence 求值
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))

    ;;; ERROR
    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    
    unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))

    unknown-procedure-type
      (restore continue)
      (assign val (const unkown-procedure-type-error))
      (goto (label signal-error))))


;;; START MACHINE

(define eceval
  (init-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   eceval-machine))

(define the-global-environment (setup-environment))

(start eceval)
