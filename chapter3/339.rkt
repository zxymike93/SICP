(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))

;; 设
;; p1: (lambda () (set! x ((s ...))))
;; p2: (lambda () (* x x))
;; p3: (lambda () (set! x (+ x 1)))

;; 已知 p2, p3 在 serializer 中，两者不能并行计算。且 p1 需要在 p2 计算完成后执行。
;; 那么可能的情况有

;; p2 -> p1 -> p3
;; p2 -> p3 -> p1
;; p2 -> p1 | p3
;; p3 -> p2 -> p1

;; 对应的结果有

;; (* 10 10) -> (set! x 100) -> (set! x (+ 100 1)) -> 101
;; (* 10 10) -> (set! x (+ 10 1)) -> (set! x 100) -> 100
;; (* 10 10) -> (set! x (+ 10 1)) | (set! x 100) -> 11 | 100
;; (set! x (+ 10 1)) -> (* 11 11) -> (set! x 121) -> 121
