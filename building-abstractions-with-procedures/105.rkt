#lang sicp

(define (p) (p))

(define (test x y)
  ((if (= x 0) 0
       y)))

(test 0 (p))

;; 上述程序，假设无论用应用序还是正则序，if 都会先估值
;; 不同估值顺序的估值过程见
;; https://github.com/xinyuzheng7/SICP/blob/master/%E5%BA%94%E7%94%A8%E5%BA%8F%E5%92%8C%E6%AD%A3%E5%88%99%E5%BA%8F.rst#%E5%AF%B9%E6%AF%94