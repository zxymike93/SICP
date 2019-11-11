#lang sicp

;; 首先，position 是一个 pair 序列
;; ((1 1) (2 2) ....)

;; 其次，我们需要一些 helper functions

(define (utest fname f)
  ;(display fname)
  ;f
  (newline))

;; int, int -> list
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))
;; (enumearte-interval 2 7) -> (2 3 4 5 6 7)
(utest "enumerate-interval: " (enumerate-interval 2 7))

(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq)
            (accumulate proc init (cdr seq)))))
;; (accumulate append nil (list (list 1 2) (list 3 4)))
;; -> (1 2 3 4)
(utest "accumulate: " (accumulate append nil (list (list 1 2) (list 3 4))))

;; proc, list -> list
(define (map proc seq)
  (if (null? seq)
      nil
      (cons (proc (car seq))
            (map proc (cdr seq)))))
;; (map - (list 1 2 3)) -> (-1 -2 -3)
(utest "map: " (map - (list 1 2 3)))

;; proc, (list (list) (list) ...) -> (append (proc list) (proc list) ...)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;; (flatmap lambda (list (list 1 2) (list 3 4))) -> (append (lambda (1 2)) (lambda (3 4)))
(utest "flatmap: " (flatmap (lambda (i) (list (car i))) (list (list 1 2) (list 3 4))))

(define (filter predicate seq)
  (cond [(null? seq)
         nil]
        [(predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq)))]
        [else
         (filter predicate (cdr seq))]))
;; (filter odd? (list 1 2 3)) -> (1 3)
(filter odd? (list 1 2 3))

;; 然后，定义题目要求的过程

;; 添加新的 position 到 position 序列中
;; int, int, position -> position
(define (adjoin-position row col positions)
  (cons (list row col) positions))
;; (adjoin-position 1 3 (list (list 4 2) (list 6 1)))
;; -> (list (list 1 3) (list 4 2) (list 6 1))
(utest "adjoin-position: " (adjoin-position 1 3 (list (list 4 2) (list 6 1))))
;; (adjoin-position 5 4 (list (list 1 3) (list 4 2) (list 6 1)))
;; -> (list (list 5 4) (list 1 3) (list 4 2) (list 6 1))
(utest "adjoin-position: " (adjoin-position 5 4 (list (list 1 3) (list 4 2) (list 6 1))))

(define empty-board nil)

;; int, ((3 3) (3 2) (3 1)) ->
;; (#t #t #t)
(accumulate (lambda (x y) (and x y)) #t (map (lambda (p) (not (= 4 (cadr p)))) (list (list 3 3) (list 3 2) (list 3 1))))

(define (safe? k positions)
  (define k-queen (car positions))
  (define k-1-queens (cdr positions))
  (and
   (accumulate (lambda (x y) (and x y))
               #t
               (map (lambda (q) (not (= (car q) (car k-queen))))
                    k-1-queens))
   (accumulate (lambda (x y) (and x y))
               #t
               (map (lambda (q) (not (= (cadr q) (cadr k-queen))))
                    k-1-queens))
   (accumulate (lambda (x y) (and x y))
               #t
               (map (lambda (q) (not (= (+ (car k-queen) (cadr k-queen))
                                        (+ (car q) (cadr q)))))
                    k-1-queens))
   (accumulate (lambda (x y) (and x y))
               #t
               (map (lambda (q) (not (= (- (car k-queen) (cadr k-queen))
                                        (- (car q) (cadr q)))))
                    k-1-queens))))

(define (queens board-size)
  
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda [positions] (safe? k positions))
         ;; 将 k-1 个子放到前 k-1 列
         (flatmap (lambda [rest-of-queens]
                    ;; 第 k 个子放在第 k 列有多少种选择（第几行）
                    (map (lambda [new-row]
                           (adjoin-position new-row k rest-of-queens))

                         (enumerate-interval 1 board-size)))

                  (queen-cols (- k 1))))))

  (queen-cols board-size))

(length (queens 8))

;; 为了清晰地知道数据结构，中途 debug 拆分了中间的函数

;(define (f k)
;  (if (= k 0)
;      (list nil)
;      (flatmap (lambda [rest-of-queens]
;                 (map (lambda [new-row]
;                        (adjoin-position new-row k rest-of-queens))
;                      (enumerate-interval 1 3)))
;               (f (- k 1)))))
;(f 3)

;(((1 3) (1 2) (1 1))
; ((2 3) (1 2) (1 1))
; ((3 3) (1 2) (1 1))
; ((1 3) (2 2) (1 1))
; ((2 3) (2 2) (1 1))
; ((3 3) (2 2) (1 1))
; ((1 3) (3 2) (1 1))
; ((2 3) (3 2) (1 1))
; ((3 3) (3 2) (1 1))
; ((1 3) (1 2) (2 1))
; ((2 3) (1 2) (2 1))
; ((3 3) (1 2) (2 1))
; ((1 3) (2 2) (2 1))
; ((2 3) (2 2) (2 1))
; ((3 3) (2 2) (2 1))
; ((1 3) (3 2) (2 1))
; ((2 3) (3 2) (2 1))
; ((3 3) (3 2) (2 1))
; ((1 3) (1 2) (3 1))
; ((2 3) (1 2) (3 1))
; ((3 3) (1 2) (3 1))
; ((1 3) (2 2) (3 1))
; ((2 3) (2 2) (3 1))
; ((3 3) (2 2) (3 1))
; ((1 3) (3 2) (3 1))
; ((2 3) (3 2) (3 1))
; ((3 3) (3 2) (3 1)))
