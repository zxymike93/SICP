#lang sicp

;; 一个递增的正整数组、数组中的所有元素不含 2, 3, 5 以外的素数因子
;; 换一种说法：
;; 假设这个正整数组是一个流 S
;; (scale-stream S 2), (scale-stream S 3), (scale-stream S 5) 均为 S 的子集
;; 它们的并集就是 S

;; 如果我们有一个 merge 过程（实际上就表示了并集）
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
           (cond ((< s1car s2car) (cons-stream
                                   s1car
                                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else (cons-stream
                        s1car
                        (merge (stream-cdr s1)
                               (stream-cdr s2)))))))))

;; 那么根据上面的定义
(define S
  (cons-stream
   1
   (merge (scale-stream S 2)
          (merge (scale-stream S 3)
                 (scale-stream S 5)))))