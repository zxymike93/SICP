#lang sicp


;;; COMPILE

(define (compile exp target linkage)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp) (compile-variable exp target linkage))
        ((assignment? exp) (compile-assignment exp target linkage))
        ((definition? exp) (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp) (compile-sequence (cdr exp) target linkage))
        ;((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp) (compile-application exp target linkage))
        (else (error "Unknown expression type -- COMPILE" exp))))

(define (self-evaluating? exp) (or (number? exp) (string? exp)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,exp))))))

(define (quoted? exp) (and (pair? exp) (eq? (car exp) 'quote)))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,(cadr exp)))))))

(define variable? symbol?)

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
    (make-instruction-sequence
    '(env)
    (list target)
    `((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))))

(define (assignment? exp) (and (pair? exp) (eq? (car exp) 'set!)))

(define (compile-assignment exp target linkage)
  (let ((var (cadr exp))
        (get-value-code (compile (caddr exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op set-variable-value!) (const ,var) (reg val) (reg env))
         (assign ,target (const ok))))))))

(define (definition? exp) (and (pair? exp) (eq? (car exp) 'define)))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence
                                   '(env val)
                                   (list target)
                                   `((perform (op define-variable!)
                                              (const ,var) (reg val) (reg env))
                                     (assign ,target (const ok))))))))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cons 'lambda (cons (cdadr exp) (cddr exp)))))

;(define (define-variable! var val env)
;  (let ((frame (first-frame env)))
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (add-bindings-to-frame! var val frame))
;            ((eq? var (car vars))
;             (set-car! vals val))
;            (else
;             (scan (cdr vars) (cdr vals)))))
;    (scan (frame-variables frame)
;          (frame-values frame))))

(define (if? exp) (and (pair? exp) (eq? (car exp) 'if)))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next)
                                  after-if
                                  linkage)))
      (let ((p-code (compile (cadr exp) 'val 'next))
            (c-code (compile (caddr exp) target consequent-linkage))
            (a-code (compile (cadddr exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence
                      '(val)
                      '()
                      `((test (op false?) (reg val))
                        (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(define (lambda? exp) (and (pair? exp) (eq? (car exp) 'lambda)))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next)
                              after-lambda
                              linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence (end-with-linkage lambda-linkage
                                                       (make-instruction-sequence
                                                        '(env)
                                                        (list target)
                                                        `((assign
                                                           ,target
                                                           (op make-compiled-procedure)
                                                           (label ,proc-entry)
                                                           (reg env)))))
                                     (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (cadr exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment)
                (const ,formals) (reg argl) (reg env))))
     (compile-sequence (caddr exp) 'val 'return))))

(define (begin? exp) (and (pair? exp) (eq? (car exp) 'begin)))

(define (compile-sequence exp target linkage)
  (if (last-exp? exp)
      (compile (car exp) target linkage)
      (preserving
       '(env continue)
       (compile (car exp) target 'next)
       (compile-sequence (cdr exp) target linkage))))

(define (last-exp? exp)
  (and (pair? exp) (eq? (cdr exp) '())))

(define application? pair?)

(define (compile-application exp target linkage)
  (let ((proc-code (compile (car exp) 'proc 'next))
        (operand-codes (map (lambda (operand) (compile operand 'val 'next))
                            (cdr exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '()
                                   '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val)
                                           '(argl)
                                           '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl)
                      '(argl)
                      '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))


;;; APPLY

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next)
                                after-call
                                linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc)
                                  '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences compiled-branch
                                      (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences primitive-branch
                                      (end-with-linkage
                                       linkage
                                       (make-instruction-sequence
                                                         '(proc argl)
                                                         (list target)
                                                         `((assign
                                                            ,target
                                                            (op apply-primitive-procedure)
                                                            (reg proc)
                                                            (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc)
                                    all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry) (reg proc))
                                      (goto (reg val)))))
         ((and (not (eq? target 'val))
               (not (eq? linkage 'return)))
          (let ((proc-return (make-label 'proc-return)))
            (make-instruction-sequence '(proc)
                                       all-regs
                                       `((assign continue (label ,proc-return))
                                         (assign val
                                                 (op compiled-procedure-entry)
                                                 (reg proc))
                                         (goto (reg val))
                                         ,proc-return
                                         (assign ,target (reg val))
                                         (togo (label ,linkage))))))
         ((and (eq? target 'val)
               (eq? linkage 'return))
          (make-instruction-sequence '(proc continue)
                                     all-regs
                                     '((assign val (op compiled-procedure-entry) (reg proc))
                                       (goto (reg val)))))
         ((and (not (eq? target 'val))
               (eq? linkage 'return))
          (error "return linkage, target not val -- COMPOLE" target))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue) instruction-sequence (compile-linkage linkage)))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '() '((goto (reg continue)))))
        ((eq? linkage 'next)
         (make-instruction-sequence '() '() '()))
        (else
         (make-instruction-sequence '() '() `((goto (label ,linkage)))))))


;;; 连接指令序列的三个方法

;; 顺序地连接多个指令
(define (append-instruction-sequences . seqs)
  ;; 连接两条指令
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1)
             (statements seq2))))
  ;; 连接多条指令
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))

  (append-seq-list seqs))

;; 根据寄存器依赖关系连接两条指令
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

;; 
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence (registers-needed seq)
                             (registers-modified seq)
                             (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence (list-union (registers-needed seq1)
                                         (registers-needed seq2))
                             (list-union (registers-modified seq1)
                                         (registers-modified seq2))
                             (append (statements seq1)
                                     (statements seq2))))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
        
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))


;;; INSTS

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (registers-needed insts)
  (if (symbol? insts)
      '()
      (car insts)))

(define (registers-modified insts)
  (if (symbol? insts)
      '()
      (cadr insts)))

(define (statements insts)
  (if (symbol? insts)
      '()
      (caddr insts)))

(define (needs-register? insts reg) (memq reg (registers-needed insts)))
(define (modifies-register? insts reg) (memq reg (registers-modified insts)))


;;; LABEL

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))


;;; REG

(define all-regs '(env proc argl val continue))
