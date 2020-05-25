#lang racket

;; 一个物理传感器，接收任意（正负）的数
;; 当上一个数为负，传入的数为正时，返回 1
;; 当上一个数为正，传入的数为负时，返回 -1
;; 当传入的数和上一个传入的数同正负时，返回 0
(define (make-zero-crossings input-stream last-value)
  (cons-stream (sign-change-detector (car-stream input-stream)
                                     last-value)
               (make-zero-crossings (cdr-stream input-stream)
                                    (car-stream input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

;; 用 map-stream 改写上面的 zero-crossings 的实现
(define (map-stream proc . argstreams)
  (if (null-stream? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map car-stream argstreams))
       (apply map-stream
              (cons proc (map cdr-stream argstreams))))))

(define zero-crossings
  (map-stream sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))