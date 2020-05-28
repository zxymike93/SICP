#lang sicp

;; 题目要求对 set-variable-value! define-variable! lookup-variable-value 三个过程中重复的运算做抽象
;; 4.11 对数据结构作了改动，这里基于新的 env 结构操作做修改

;; env => (frame frame frame '())
(define the-empty-env '())

;; frame env -> env
(define (extend-env frame env) (cons frame env))

;; env -> frame
(define (first-frame env) (car env))

;; env -> (frame frame frame ...)
(define (rest-frames env) (cdr env))

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
(define (first-kv frame) (car frame))

;; frame -> ((syb num) (syb num) ..)
(define (rest-kvs frame) (cdr frame))

;; (syb num) -> syb
(define (variable kv) (car kv))

;; (syb num) -> num
(define (value kv) (cdr kv))

;; 在链表的头插入元素
(define (bind! var val frame)
  (set-cdr! frame (cons (first-kv frame) (rest-kvs frame)))
  (set-car! frame (cons var val)))

(define (lookup key table)
  (let ([record (assoc key table)])
    (if record
        record
        false)))

(define (lookup-variable-value var env)
    (cond [(null? env) false]
          [else (let ([record (lookup var (first-frame env))])
                  (if record
                      (value record)
                      (lookup-variable-value var (rest-frames env))))]))

(define (define-variable! var val env)
  (let ([internal-frame (first-frame env)])
    (let ([record (lookup var internal-frame)])
      (if record
          (set-cdr! record val)
          (bind! var val internal-frame)))))

(define (set-variable-value! var val env)
  (cond [(null? env) (error "Unbound variable: " var)]
        [else (let ([record (lookup var (first-frame env))])
                (if record
                    (set-cdr! record val)
                    (set-variable-value! var val (rest-frames env))))]))

;; test
(define env (extend-env (make-frame '(x y z) '(1 2 3))
                        (extend-env (make-frame '(u v w) '(4 5 6)) the-empty-env)))
env
(lookup-variable-value 'v env)
(lookup-variable-value 'w env)
(lookup-variable-value 'z env)
(define-variable! 'w 7 env)
env
(set-variable-value! 'w 8 env)
env
(set-variable-value! 'z 11 env)
env
(set-variable-value! 'v 9 env)
env
(set-variable-value! 'm 0 env)