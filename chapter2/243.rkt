#lang sicp

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

;; proc, list -> (and (proc a1) (proc a2) ....)
(define (andmap proc seq)
  (accumulate (lambda [x y] (and x y))
              #t
              (map proc seq)))

(define (filter predicate seq)
  (cond [(null? seq)
         nil]
        [(predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq)))]
        [else
         (filter predicate (cdr seq))]))
;; (filter odd? (list 1 2 3)) -> (1 3)
(utest "filter: " (filter odd? (list 1 2 3)))

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

(define (safe? k positions)
  (let ([k-queen (car positions)]
        [k-1-queens (cdr positions)])
    (and
     ;; 不同一行
     (andmap (lambda [q] (not (= (car q) (car k-queen))))
             k-1-queens)
     ;; 不同一列
     (andmap (lambda [q] (not (= (cadr q) (cadr k-queen))))
             k-1-queens)
     ;; 不同一对角线
     (andmap (lambda [q] (not (= (- (car q) (cadr q))
                                 (- (car k-queen) (cadr k-queen)))))
             k-1-queens)
     (andmap (lambda [q] (not (= (+ (car q) (cadr q))
                                 (+ (car k-queen) (cadr k-queen)))))
             k-1-queens))))

;; 假设使用这个过程计算八皇后问题，耗时为T
;(define (queens board-size)  
;  (define (queen-cols k)
;    (if (= k 0)
;        (list empty-board)
;        (filter
;         (lambda [positions] (safe? k positions))
;         ;; 将 k-1 个子放到前 k-1 列
;         (flatmap (lambda [rest-of-queens]
;                    ;; 第 k 个子放在第 k 列有多少种选择（第几行）
;                    (map (lambda [new-row]
;                           (adjoin-position new-row k rest-of-queens))
;                         (enumerate-interval 1 board-size)))
;                  (queen-cols (- k 1))))))
;  (queen-cols board-size))

;; 如果改为以下过程，耗时多少？
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda [positions] (safe? k positions))
         (flatmap (lambda [new-row]
                    (map (lambda [rest-of-queens]
                           (adjoin-position new-row k rest-of-queens))
                         (queen-cols (- k 1))))
                  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; Original
(flatmap (map join (1 ... 8)) (q-cols 7))
(flatmap (map join (1 ... 8)) (flatmap (map join (1 ... 8)) (q-cols 6)))
...
(map join 8 (map join 8 (map join 8 (map join 8 (map join 8 (map join 8 (map join 8 (map join 8 (q-cols 0)))))))))
(map join 8 (map join 8 (map join 8 (map join 8 (map join 8 (map join 8 (map join 8 1)))))))
(map join 8 (map join 8 (map join 8 (map join 8 (map join 8 (map join 8 2))))))
...
8*7*6*5*4*3*2*1

;; Louis
(flatmap (map join (q-cols 7)) (1 ... 8))
(flatmap (map join (flatmap (map join (q-cols 6)) (1 ... 8))) (1 ... 8))
...
(map join (map join (map join (map join (map join (map join (map join (map join (q-cols 0) 8) 8) 8) 8) 8) 8) 8) 8)
...
8*8*8*8*8*8*8*8
