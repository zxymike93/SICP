#lang sicp

;;; 1. eval 的过程不是从 expr->val 的过程，而是生产一个包含 frame 的 stream 的过程。
;;; 2. 由于 1，repl 并非直接打印结果，而是 display-stream。
;;; 3. 对于 1 的实现：
;;;   a. simple-query 对 assertion 做简单的 pattern matching，类似符号匹配。
;;;   b. compound-query 中 and/or/not 实际上是对 stream 作处理
;;; 4. 语言除了 query 以外的另一核心是 rule，由 unification 算法实现。
;;; 5. 至于用 stream 为基础数据结构的原因：
;;;   a. 可能存在无穷结果（递归逻辑定义）.
;;;   b. 效率以及 stream 更符合 and/or/not 的过滤模型。


(define (repl)
  (display-input-prompt)
  (let ((query (query-syntax-process (read))))
    (cond ((assertion? query)
           (add-rule-or-assertion! (add-assertion-body query))
           (newline) (display "Assertion added to database.")
           (repl))
          (else
           (newline) (dispaly-output-prompt)
           (display-stream (stream-map (lambda (frame)
                                         (make-instant query
                                                       frame
                                                       (lambda (v f)
                                                         (contract-question-mark v))))
                                       (qeval query (singleton-stream '()))))
           (repl)))))

(define (qeval expr env)
  (let ((qproc (get (type expr) 'qeval)))
    (if qproc
        (qproc (contents expr) env)  ;; special form
        (simple-query expr env))))

(define (qapply pattn frame)
    (stream-flatmap (lambda (rule)
                    (apply-rule rule pattn frame))
                  (fetch-rules pattn frame)))


;;; Query ;;;

;; (likes mike ?what) => (likes mike spirit)
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;; (and ...)
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

;; (or ...)
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;; (not ...)
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

;; To support (> ...) (< ...) ...
(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (make-instant
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (car exp) (scheme-report-environment 5))
         (cdr exp)))


;;; Frame and Binding (Database) ;;;

(define (make-instant expr frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else
           exp)))
  (copy expr))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

;(define (binding-in-frame variable frame)
;  (assoc variable frame))
;
;(define (extend variable value frame)
;  (cons (make-binding variable value) frame))


;;; Stream ;;;

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


;;; Table ;;;

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
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;;; Helper ;;;

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr varibale)))
                      (symbol->string (cadr variable))))))

(define (display-input-prompt)
  (newline) (newline) (display ";;; Query input:") (newline))

(define (display-output-prompt)
  (display ";;; Query output:") (newline))
