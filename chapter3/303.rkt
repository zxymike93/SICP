#lang sicp

;; 注意参数、本地变量的命名不能相同，所以采用 password psswd pw
(define (make-account balance password)
  ;; 密码
  (define psswd password)
  ;; 取钱
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  ;; 存钱
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch pw m)
    (cond [(eq? pw psswd)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))]
          [else
           (error "Incorrect password")]))
  
  dispatch)


(define acc (make-account 100 'secret-key))
((acc 'secret-key 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)