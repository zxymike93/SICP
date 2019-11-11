#lang sicp

;; 不使用 enumerate-tree 的方法
;; (define (count-leaves t)
;;   (accumulate +
;;               0
;;               (map (lambda (node)
;;                      (if (not (pair? node))
;;                          1
;;                          (count-leaves node)))
;;                    t)))
;; 估值过程
;; (count-leaves ((1 2) (3 4)))
;; (accumulate + 0 (cons (count-leaves (1 2))
;;                       (count-leaves (3 4))))
;; (accumulate + 0 (cons (accumulate + 0 (cons 1
;;                                             (count-leaves (2))))
;;                       (accumulate + 0 (cons 1
;;                                             (count-leaves (4))))))
;; (accumulate + 0 (cons (accumulate + 0 (cons 1
;;                                             (accumulate + 0 (cons 1 0))))
;;                       (accumulate + 0 (cons 1
;;                                             (accumulate + 0 (cons 1 0))))))
;; (accumulate + 0 (cons (accumulate + 0 (cons 1 1))
;;                       (accumulate + 0 (cons 1 1))))
;; (accumulate + 0 (cons 2 2))
;; 4

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-tree t)
  (cond [(null? t) nil]
        [(not (pair? t)) (list t)]
        [else (append (enumerate-tree (car t))
                      (enumerate-tree (cdr t)))]))

(define (count-leaves x)
  (accumulate +
              0
              (map
               (lambda (x) 1)
               (enumerate-tree x))))

;;; tests
(define x (list (list 1 2) (list 3 4)))
(count-leaves x)