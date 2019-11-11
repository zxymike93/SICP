#lang sicp

;; mobile: left-branch + right-branch
;; branch: length + structure
;; structure: weight / mobile
;; length: int
;; weight: int

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ([s (branch-structure branch)])
    (if (not (pair? s))
        s
        (total-weight s))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (sub-balanced? branch)
  (let ([s (branch-structure branch)])
    (if (pair? s)
        (balanced? s)
        #t)))

(define (balanced? mobile)
  (let ([left (left-branch mobile)]
        [right (right-branch mobile)])
    (and (= (torque left) (torque right))
         (sub-balanced? left)
         (sub-balanced? right))))

;; 对于最后一问,如果将 mobile / branch 的实现修改为 cons 结构
;; 只需将对应的 selectors 修改即可(请自行测试)
;(define (make-branch length structure) (cons length structure))
;(define (branch-length branch) (car branch))
;(define (branch-structure branch) (cdr branch))
;(define (make-mobile left right) (cons left right))
;(define (left-branch mobile) (car mobile))
;(define (right-branch mobile) (cdr mobile))

;; tests
(define b1 (make-branch 1 1))
(define m1 (make-mobile b1 b1))
(define b2 (make-branch 1 m1))
(define m2 (make-mobile b1 b2))
(define m3 (make-mobile b2 b2))
(define m4 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8)))))
(define m5 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4)))))

(eq? (total-weight m1) 2)
(eq? (total-weight m2) 3)
(eq? (total-weight m3) 4)
(eq? (total-weight m4) 21)
(eq? (total-weight m5) 18)

(eq? (balanced? m1) #t)
(eq? (balanced? m2) #f)
(eq? (balanced? m3) #t)