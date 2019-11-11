#lang sicp

;; Yang Hui's triangle (aka Pascal's triangle)
;;     1              (1 1)
;;    1 1          (2 1) (2 2)
;;   1 2 1      (3 1) (3 2) (3 3)
;;  1 3 3 1   (4 1) (4 2) (4 3) (4 4)
;; 1 4 6 4 1          ...

;; 表示杨辉三角的数据结构
;; row column -> value
(define (pascal-triangle row col)
  (if (or (= col 1)
          (= col row))
      1
      (+ (pascal-triangle (- row 1) (- col 1))
         (pascal-triangle (- row 1) col))))

;; tests
(pascal-triangle 1 1)
(pascal-triangle 5 3)
(pascal-triangle 15 7)