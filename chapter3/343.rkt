(define (make-account balance)
  ;; (withdraw account amount)
  (set! balance (- balance amount))

  ;; (deposit account amount)
  (set! balance (+ balance amount))

  (let ((s (make-serializer)))
    (define (dispatch m)
      ;; serialize
      (s withdraw)
      (s deposit))))

(define (exchange acc1 acc2)
  (let ((diff (- (acc1 'balance)
                 (acc2 'balance))))
    ((acc1 'withdraw) diff)
    ((acc2 'deposit) diff)))

(define a1 10)
(define a2 20)
(define a3 30)

;; Peter
(exchange a1 a2)

;; Paul
(exchange a1 a3)

;; A1 如果这些操作都是顺序执行的，那么 a1 a2 a3 总是相互调换值，总是能保持 {10, 20, 30}

;; Q2 假设有上述三个账户 a1, a2, a3， 每个账户都使用了单独的 serializer。Peter，Paul 分别进行一笔转账。
;; A2 使用时序图说明，某种情况下三个账户的金额非 10, 20, 30。

;        a2    a1    a3
;        20    10    30
;diff       10    20     diff    ; a2-a1 算出的差额是 10，a3-a1 算出的差额是 20。因为两个 diff 是可以并发执行的。
;wd      10          10  wd      ; 转账之后，a2 a3 均为 10
;dp            20
;              40        dp

;; 如上图所示，如果仅每个账户使用独立的 seriralier。
;; 仅保证对 a1 的两次 deposit 按序执行，不保证结果为 10, 20, 30，而是 10, 40, 10

;; Q3 说明即便不能保证账户集的账面金额总是 {10, 20, 30}，但总能保证 a1 a2 a3 加起来的总账户金额不变。

;; Q4 说明如果连单个账户的 serializer 去除，三个账户的总金额也不能保证不变。