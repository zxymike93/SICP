;; Original version
(define (make-account balance)
  ;; ...
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ;; ...
            ))))

;; Ben's version
(define (make-account balance)
  ;; ...
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ;; ...
              )))))

(define (withdraw account amount)
  ((account 'withdraw) amount))

(define (deposit account amount)
  ((account 'deposit) amount))

;; Ben 的写法和 original 的写法结果是一样的。
;; 典型的 usage 如下（进行对 (make-account) 的并行计算）
(define acc (make-account))

(parallel-execute
  (lambda (amount) (withdraw acc amount))
  (lambda (amount) (deposit acc amount)))
