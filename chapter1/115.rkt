#lang sicp

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; 计算 (sine 12.15) ，要应用 p 多少次？
;;(sine 12.15)
;;(p (sine 4.05))
;;(p (p (sine 1.35)))
;;(p (p (p (sine 0.45))))
;;(p (p (p (p (sine 0.15)))))
;;(p (p (p (p (p (sine 0.05))))))
;; 因为 0.05 <= 0.01 ，所以 p 应用了5次。

;; (sine a) 的时间空间复杂度？
;; 因为计算和记忆规模受 (/ angle 3.0) 影响，时间空间复杂度均为 O(log3a)