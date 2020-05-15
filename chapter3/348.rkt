#lang sicp

;; id 越小优先级越高
(define (make-account balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough balance"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ([s (make-serializer)])
    (define (dispatch msg)
      (cond [(eq? msg 'balance) balance]
            [(eq? msg 'id) id]
            [(eq? msg 'serializer) s]
            [(eq? msg 'withdraw) withdraw]
            [(eq? msg 'deposit) deposit]
            [else (error "Account: Unknown message")]))
    dispatch))

(define (exchange a1 a2)
  (let ([diff (- (a1 'balance) (a2 'balance))])
    ((a1 'withdraw) diff)
    ((a2 'deposit) diff)))

;; 假设有账户 acc1：10，acc2：20
(define acc1 (make-account 10 1))
(define acc2 (make-account 20 2))
;; Paul 将 a2 的钱交换到 a1,Peter 则相反。而且它们是并行执行的（比如现实世界中这些操作都是假设可以同步的）
(parallel-execute (serialized-exchange acc2 acc1)
                  (serialized-exchange acc1 acc2))

;; 如果使用书本例子
;(define (serialized-exchange a1 a2)
;  (let ([s1 (a1 'serializer)]
;        [s2 (a2 'serializer)])
;    ((s1 (s2 exchange)) a1 a2)))

;; 上面两个过程展开为 [] 中的过程体
(p-e [((acc2 'seri) ((acc1 'seri) exchange)) acc2 acc1]
     [((acc1 'seri) ((acc2 'seri) exchange)) acc1 acc2])
;; 它们同时访问 acc2, acc1 导致死锁

;; 修改 serialized-exchange，比较 id 大小来判断执行优先级，避免死锁
(define (serialized-exchange a1 a2)
  (let ([id1 (a1 'id)]
        [id2 (a2 'id)]
        [s1 (a1 'serializer)]
        [s2 (a2 'serializer)])
    (if (< id1 id2)
        ((s1 (s2 exchange)) a1 a2)
        ((s2 (s1 exchange)) a1 a2))))