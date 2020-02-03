(define (make-account balance)
  ;; ...
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw)
             (protected withdraw))
            ((eq? m 'deposit)
             (protected deposit))
            ((eq? m 'balance)
             ;; Ben 修改了以下这行
             ((protected (lambda () balance))))
            (else ...)))))

;; 由于 balance 仅仅读取 balance 的值，并不修改。无需作此修改。
