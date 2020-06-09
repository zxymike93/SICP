;;;; logic.rkt
;;;; *SICP* 中 4.4 所说明的逻辑语言的实现，以及用例。

#lang sicp
(#%require (only racket/base make-hash hash-ref hash-set!))

;;; table (get/put)
;;; 全局变量 TABLE 用来存储 assertions/rules，使用 (op type) 作为索引。

(define TABLE (make-hash))
(define (get op type)
  (hash-ref TABLE (list op type) #f))
(define (put op type stream)
  (hash-set! TABLE (list op type) stream))

;;; delay / force
;;; *SICP* 自带了 delay / force，但是一个 promise 对象，要用宏实现为 pair 才能应用 car / cdr 等过程。

(define (memorize proc)
  (let ([run? #f]
        [result #f])
    (lambda ()
      (if (not run?)
          (begin (set! result (proc))
                 (set! run? #t)
                 result)
          result))))

(define-syntax mdelay
  (syntax-rules ()
    [(mdelay s) (memorize (lambda () s))]))

(define-syntax mforce
  (syntax-rules ()
    [(mforce s) (s)]))

;;; stream
;;; expressions, rules, assertions 以 stream 的形式存在。

(define (cons-stream a b)
  (cons a (mdelay b)))
(define (stream-car s)
  (car s))
(define (stream-cdr s)
  (mforce (cdr s)))

(define the-empty-stream '())
(define (stream-null? s)
  (null? s))

;; single element stream
(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline) (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (mforce delayed-s2)
      (cons-stream (stream-car s1)
                   (stream-append-delayed (stream-cdr s1)
                                          delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (mforce delayed-s2)
      (cons-stream (stream-car s1)
                   (interleave-delayed (mforce delayed-s2)
                                       (mdelay (stream-cdr s1))))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map (stream-cdr s) proc))))

;; 去重的 map
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed (stream-car stream)
                          (mdelay (flatten-stream (stream-cdr stream))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

;;; frame / dict
;;; 查询中用来保存变量的结构。

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame var frame)
  (assoc var frame))

(define (extend var val frame)
  (cons (make-binding var val) frame))

;;; database
;;; 既保存 assertion 也保存 rule。由两个 stream 构成,一个有索引,一个无索引。

(define (get-stream k1 k2)
  (let ([s (get k1 k2)])
    (if s
        s
        the-empty-stream)))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat) ;; ?? rename
  (let ([key (car pat)])
    (if (var? key)
        '?
        key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;; (job (Bitdiddle Ben) (computer programmer))
(define THE-ASSERTIONS the-empty-stream)

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ([key (index-key-of assertion)])
        (let ([current-assertion-stream (get-stream key 'assertion-stream)])
          (put key 'asstion-stream
               (cons-stream assertion current-assertion-stream))))
      ))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ([old-assertions THE-ASSERTIONS])
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions))
    'ok))

;; (rule (boss ?x ?y)
;;       (and (supervisor ?x ?y) (not (job ?x (computer programmer)))))
(define THE-RULES the-empty-stream)

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append (get-stream (index-key-of pattern) 'rule-stream)
                 (get-stream '? 'rule-stream)))

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (store-rule-in-index rule)
  (let ([pattern (conclusion rule)])
    (if (indexable? pattern)
        (let ([key (index-key-of pattern)])
          (let ([current-rule-stream (get-stream key 'rule-stream)])
            (put key 'rule-stream current-rule-stream)))
        )))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ([old-rules THE-RULES])
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

;;; expression / query-syntax
;;; 查询语句的语法组成

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? tag (car exp))
      #f))

(define (var? exp)
  (tagged-list? exp '?))
(define (constant-symbol? exp)
  (symbol? exp))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

;; exp 可能是 (job (B Ben) (computer trainee))，那么就可能拆分为 job, (B Ben), 6000 ...
(define (map-over-symbols proc exp)
  (cond [(pair? exp) (cons (map-over-symbols proc (car exp))
                           (map-over-symbols proc (cdr exp)))]
        [(symbol? exp) (proc exp)]
        [else exp]))

;; (job ?x ?y) -> (job (? x) (? y))
(define (expand-question-mark symbol)
  (let ([chars (symbol->string symbol)])
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol (substring chars 1 (string-length chars))))
        symbol)))

(define (contract-question-mark var)
  (string->symbol (string-append "?"
                                 (if (number? (cadr var))
                                     (string-append (symbol->string (caddr var))
                                                    "-"
                                                    (number->string (cadr var)))
                                     (symbol->string (cadr var))))))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var id)
  (cons '? (cons id (cdr var))))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule)
  (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (empty-conjunction? exps)
  (null? exps))
(define (first-conjunct exps)
  (car exps))
(define (rest-conjuncts exps)
  (cdr exps))

(define (empty-disjunction? exps)
  (null? exps))
(define (first-disjunct exps)
  (car exps))
(define (rest-disjuncts exps)
  (cdr exps))

(define (negated-query exps)
  (car exps))
(define (predicate exps)
  (car exps))
(define (args exps)
  (cdr exps))

;; 一条查询语句有两种可能：简单的 match 以及 unify 的结果。
(define (simple-query query-pattern frame-stream)
  (stream-flatmap (lambda [frame] (stream-append-delayed
                                   (find-assertions query-pattern frame)
                                   (mdelay (apply-rules query-pattern frame)))) ;;
                  frame-stream))

;; compound query
;; and, or
(define (conjoin conjuncts frame-stream)
      ;; (and ...) all true
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
      ;; (or ...) all false
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed (qeval (first-disjunct disjuncts) frame-stream)
                          (mdelay (disjoin (rest-disjuncts disjuncts) frame-stream))))) ;;
(put 'or 'qeval disjoin)

;; filter
;; not, lisp-value
(define (negate operands frame-stream)
  (stream-flatmap (lambda [frame]
                    (if (stream-null? (qeval (negated-query operands)
                                             (singleton-stream frame)))
                        (singleton-stream frame)
                        the-empty-stream))
                  frame-stream))
(put 'not 'qeval negate)

;; execute applies lisp prime
(define user-initial-environment '())
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (lisp-value call frame-stream)
  (stream-flatmap (lambda [frame]
                    (if (execute (instantiate
                                   call
                                   frame
                                   (lambda [v f] (error "Unknown pat var: LISP-VALUE" v))))
                        (singleton-stream frame)
                        the-empty-stream))
                  frame-stream))
(put 'lisp-value 'qeval lisp-value)

;; empty rule body
(define (always-true ignore frame-stream)
  frame-stream)
(put 'always-true 'qeval always-true)

;;; check
;;;

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond [(var? e)
           ;; ?x <-> ?y
           (cond [(equal? var e)
                  #t]
                 [else (let ([b (binding-in-frame e frame)])
                         (if b
                             (tree-walk (binding-value b))
                             #f))])]
          [(pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e)))]
          [else
           #f]))
  (tree-walk exp))

(define (extend-if-possible var val frame)
  (let ([binding (binding-in-frame var frame)])
    (cond [binding
           (unify-match (binding-value binding) val frame)]
          ;; ?x->?y->5
          [(var? val)
           (let ([binding (binding-in-frame val frame)])
             (if binding
                 (unify-match var
                              (binding-value binding)
                              frame)
                 (extend var val frame)))]
          ;; ?x <-> ?y / fix-point
          [(depends-on? var val frame)
           'failed]
          ;; ?x->5
          [else
           (extend )])))

;;; matcher
;;; 负责所有简单模式的匹配
;;; (match pattern database frame) => extended-frame

(define (extend-if-consistent var dat frame)
  (let ([binding (binding-in-frame var frame)])
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (pattern-match pattern datum frame)
  (cond [(eq? frame 'failed)
         'failed]
        [(equal? pattern datum)
         frame]
        [(var? pattern)
         (extend-if-consistent pattern datum frame)]
        [(and (pair? pattern) (pair? datum))
         (pattern-match (cdr pattern)
                        (cdr datum)
                        (pattern-match (car pattern) (car datum) frame))]
        [else
         'failed]))

(define (check-an-assertion assertion query-pattern query-frame)
  (let ([match-result (pattern-match query-pattern assertion query-frame)])
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (find-assertions pattern frame)
  (stream-flatmap (lambda [datum] (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

;;; unifier
;;; variables are allowed on both sides of the match.

(define (unify-match p1 p2 frame)
  (cond [(eq? frame 'failed)
         'failed]
        [(equal? p1 p2)
         frame]
        [(var? p1)
         (extend-if-possible p1 p2 frame)]
        [(var? p2)
         (extend-if-possible p2 p2 frame)] ;***
        [(and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1) (car p2) frame))]
        [else
         'failed]))

(define (rename-variables-in rule)
  (let ([id (new-rule-application-id)])
    (define (tree-walk exp)
      (cond [(var? exp)
             (make-new-variable exp id)]
            [(pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp)))]
            [else
             exp]))
    (tree-walk rule)))

(define (apply-a-rule rule pattern frame)
  (let ([clean-rule (rename-variables-in rule)])
    (let ([unify-result (unify-match pattern (conclusion clean-rule) frame)])
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda [rule] (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

;;; evaluator
;;;

(define (qeval query frame-stream) ;=>stream
  (let ([qproc (get (type query) 'qeval)])
        ;; special form
    (if qproc
        (qproc (contents query) frame-stream)
        ;; simple query
        (simple-query query frame-stream))))

;;; initiater (instantiate query)
;;;

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
          ;; variable like ?x
    (cond [(var? exp)
           (let ([binding (binding-in-frame exp frame)]) ; lookup
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame)))]
          ;; pair like (?x ?y)
          [(pair? exp)
           (cons (copy (car exp))
                 (copy (cdr exp)))]
          ;; symbol
          [else exp]))
  (copy exp))

;;; driver-loop
;;; 循环地读取用户输入,或增加规则/断言到数据库,或执行查询语句。

(define (query-driver-loop)
  (newline)
  (newline)
  (display ";;; Query input:")
  (newline)
  (let ([q (query-syntax-process (read))])
          ;; add assertions / rules
    (cond [(assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to database.")
           (query-driver-loop)]
          ;; eval query
          [else
           (newline)
           (display ";;; Query result:")
           (display-stream (stream-map (lambda [frame] (instantiate
                                                         q
                                                         frame
                                                         (lambda [v f] (contract-question-mark v))))
                                       (qeval q (singleton-stream '()))))
           (query-driver-loop)])))

;;; test cases
(let ([q (query-syntax-process '(assert! (salary (Bitdiddle Ben) 6000)))])
  (add-rule-or-assertion! (add-assertion-body q)))

(get 'salary 'assertion-stream)

(let ([q (query-syntax-process '(salary (Bitdiddle Ben) ?amount))])
  (display q)
  (qeval q (singleton-stream '())))