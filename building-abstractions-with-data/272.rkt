#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? 'leaf (car object)))

(define (symbol-leaf object) (cadr object))

(define (weight-leaf object) (caddr object))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (encode-symbol char tree)
  (define element-of-set? memq)
  (define (choose-branch char branch)
    (if (leaf? branch)
        (eq? char (symbol-leaf branch))
        (element-of-set? char (symbols branch))))
  (cond [(leaf? tree)
         (if (not (eq? char (symbol-leaf tree)))
             (error "[encode-symbol] BAD SYMBOL: " char)
             '())]
        [(choose-branch char (left-branch tree))
         (cons 0 (encode-symbol char (left-branch tree)))]
        [else
         (cons 1 (encode-symbol char (right-branch tree)))]))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define n5
   (make-code-tree
    (make-leaf 'E 16)
    (make-code-tree
     (make-leaf 'D 8)
     (make-code-tree
      (make-leaf 'C 4)
      (make-code-tree
       (make-leaf 'B 2)
       (make-leaf 'A 1))))))

;; TODO: 平衡二叉树和完全不平衡二叉树的复杂度
;; 在 O(n*logn) ~ O(n^2) 之间