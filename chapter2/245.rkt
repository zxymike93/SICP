#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split bigger-side smaller-side)
  (lambda [painter n]
    (if (= n 0)
        painter
        (let ([smaller ((split bigger-side smaller-side) painter (- n 1))])
          (bigger-side painter (smaller-side smaller smaller))))))
;; 定义一个过程 split 使得

;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ([smaller (right-split painter (- n 1))])
;        (beside painter (below smaller smaller)))))
;
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ([smaller (up-split painter (- n 1))])
;        (below painter (beside smaller smaller)))))

;; 可以表达为
(define right-split (split beside below))
(define up-split (split below beside))        

(paint (right-split einstein 1))
(paint (up-split einstein 1))