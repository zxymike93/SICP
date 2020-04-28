#lang sicp

;; 注意参数、本地变量的命名不能相同,所以采用 password psswd pw
(define (make-account balance password)
  ;; 密码
  (define psswd password)
  (define (auth? pw)
    (eq? pw psswd))
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
    (let ([auth (auth? pw)])      
      (cond [(eq? m 'auth?)
             auth]
            [else
             (cond [auth
                    (cond ((eq? m 'withdraw) withdraw)
                          ((eq? m 'deposit) deposit)
                          ((eq? m 'auth?) auth?)
                          (else (error "Unknown request: MAKE-ACCOUNT" m)))]
                   [else
                    (error "Incorrect password")])])))
  
  dispatch)

(define (make-joint acc pwd joint-password)
  (define (dispatch pw m)
    (cond [(eq? pw joint-password)
           (acc pwd m)]
          [else
           (error "Incorrect joint password")]))

  (if (acc pwd 'auth?)
      dispatch
      (error "Can't make joint account: Incorrect password.")))

;; tests
;; 注释掉的过程都是出错测试
(define acc (make-account 100 'secret-key))
((acc 'secret-key 'withdraw) 40)
;((acc 'some-other-password 'deposit) 50)

(define jacc (make-joint acc 'secret-key 'sk))
((jacc 'sk 'deposit) 100)
;((jacc 'seck 'deposit) 100)

;(define wacc (make-joint acc 'sk 'jsk))