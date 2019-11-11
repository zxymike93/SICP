#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? 'leaf (car object)))

(define (symbol-leaf object)
  (cadr object))

(define (weight-leaf object)
  (caddr object))

;; (left-branch right-branch {A B C D E F G H} 17)
(define (make-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; represent a set of leaves and trees as a list of elements,
;; arranged in increasing order of weight.
(define (adjoin-set x set)
  (cond [(null? set)
         (list x)]
        [(< (weight x) (weight (car set)))
         (cons x set)]
        [else
         (cons (car set)
               (adjoin-set x (cdr set)))]))

;; initialize ((A 4) (B 2) (C 1) (D 1)) as ordered set
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair) (cdr pair))
                    (make-leaf-set (cdr pairs))))))

;; list, tree -> list
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; huffman code: 001000010000011000100000101
(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "[choose-branch] BAD BIT." bit)]))

;; 使用 sample-tree 定义的 code 树
(define sample-tree
  (make-tree 
   (make-leaf 'A 4)
   (make-tree
    (make-leaf 'B 2)
    (make-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

sample-tree
;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

;; decode 下面的 message
(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; 0 110 0 10 10 111 0
;; A  D  A B  B   C  A
(decode sample-message sample-tree)