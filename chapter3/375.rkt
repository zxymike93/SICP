#lang racket

(define (make-zero-crossings input-stream last-value)
  (cons-stream (sign-change-detector (car-stream input-stream)
                                     last-value)
               (make-zero-crossings (cdr-stream input-stream)
                                    (car-stream input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

;; 将上面的过程使用「取平均」的方式过滤输入的「噪音」
;; 这里形成的「传感器流」应该是平均值的集合
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ([avpt (/ (+ (car-stream input-stream) last-value)
                 2)])
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (cdr-stream input-stream)
                                      (car-stream input-stream)
                                      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))