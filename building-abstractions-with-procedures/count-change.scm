; 如果有 kind 种硬币，deomination 返回最贵的硬币的面值
(define (deomination kind)
    (cond ((= kind 1) 1)
          ((= kind 2) 5)
          ((= kind 3) 10)
          ((= kind 4) 25)
          ((= kind 5) 50)))


; ∵ 不用硬币 A 的所有排法 + 一定用硬币 A 的所有排法 = 所有排法
; 又 ∵ 一定用 A = 至少用 1 个 A
; ∴ (= (cc amount kind-of-coin)
;      (+ (cc amount (- kind-of-coin 1)
;         (cc (- amount (一个 A 的面值)) kind-of-coin))))
(define (cc amount kind-of-coin)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kind-of-coin 0)) 0)
          (else (+ (cc amount (- kind-of-coin 1))
                   (cc (- amount (deomination kind-of-coin)) kind-of-coin)))))


(define (count-change amount)
    (cc amount 5))
