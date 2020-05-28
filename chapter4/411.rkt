#lang sicp

;; env => (frame frame frame '())
(define the-empty-env '())

;; frame env -> env
(define (extend-env frame env)
  (cons frame env))

;; env -> frame
(define (first-frame env)
  (car env))

;; env -> (frame frame frame ...)
(define (rest-frames env)
  (cdr env))

;; (syb syb syb ..) (num num num ..) -> frame
;; (make-frame '(x y z) '(1 2 3))
;; >>> ((x . 1) (y . 2) (z . 3))
(define (make-frame vars vals)
  (define (make-kv vars vals)
    (if (null? vars)
        '()
        (let ([kv (cons (car vars) (car vals))])
          (cons kv
                (make-kv (cdr vars) (cdr vals))))))
  (make-kv vars vals))

;; frame -> (syb num)
(define (first-kv frame)
  (car frame))

;; frame -> ((syb num) (syb num) ..)
(define (rest-kvs frame)
  (cdr frame))

;; (syb num) -> syb
(define (variable kv)
  (car kv))

;; (syb num) -> num
(define (value kv)
  (cdr kv))

;; (lookup-variable-value <var> <env>) -> 返回对应的 val / false
(define (lookup-variable-value var env)
  (define (lookup var kvs)
    (cond [(null? kvs) false]
          [else (let ([first (first-kv kvs)])
                  (if (not (eq? var (variable first)))
                      (lookup var (rest-kvs kvs))
                      (value first)))]))
  (define (scan-first-frame var frames)
    (cond [(null? frames) false]
          [else (let ([first (first-frame frames)])
                  (let ([val (lookup var first)])
                    (if (not val)
                        (scan-first-frame var (rest-frames frames))
                        val)))]))
  (scan-first-frame var env))

;; 在链表的头插入元素
(define (bind! var val frame)
  (set-cdr! frame (cons (first-kv frame) (rest-kvs frame)))
  (set-car! frame (cons var val)))

;; (define-variable! <var> <val> <env>) -> 添加一个变量 / 修改一个变量的值
(define (define-variable! var val env)
  (let ([internal-frame (first-frame env)])
    (define (scan kvs)
      (cond [(null? kvs) (bind! var val internal-frame)]
            [else (let ([kv (first-kv kvs)])
                    (if (eq? var (variable kv))
                        (set-cdr! kv val)
                        (scan (rest-kvs kvs))))]))
    (scan internal-frame)))

;; (set-variable-value! <var> <val> <env>) -> 修改一个变量的值 / 如果变量不存在返回 false
(define (set-variable-value! var val env)
  (define (lookup var kvs)
    (cond [(null? kvs) false]
          [else (let ([kv (first-kv kvs)])
                  (if (eq? var (variable kv))
                      kv
                      (lookup var (rest-kvs kvs))))]))
  (define (scan-first-frame var val frames)
    (cond [(null? frames) (error "Unbound variable: " var)]
          [else (let ([first (first-frame frames)])
                  (let ([kv (lookup var first)])
                    (if (not kv)
                        (scan-first-frame var val (rest-frames frames))
                        (set-cdr! kv val))))]))
  (scan-first-frame var val env))

;; test
(define env (extend-env (make-frame '(x y z) '(1 2 3))
                        (extend-env (make-frame '(u v w) '(11 12 13)) the-empty-env)))
env
(define-variable! 'w 7 env)
env
(set-variable-value! 'w 8 env)
env
(set-variable-value! 'z 11 env)
env
(set-variable-value! 'v 9 env)
env