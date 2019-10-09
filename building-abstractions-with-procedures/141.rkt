#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

;; tests
(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)
;; 设 (f (f x)) 为 (g x)
;; (((double (lambda (x) (g (g x)))) inc ) 5)
;; 设 (g (g x)) 为 (h x)
;; (((lambda (x) (h (h x))) inc) 5)
;; 设 (h (h x)) 为 (i x)
;; (((lambda (x) (i x)) inc) 5)
;; (h (h 5))
;; (h (g (g 5)))
;; (g (g (g (g 5))))
;; (g (g (g (f (f 5)))))
;; (g (g (f (f (f (f 5))))))
;; (g (f (f (f (f (f (f (f (f 5)))))))))
;; (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f 5))))))))))))))))
;; 将 inc 代入 f
;; 得到 21