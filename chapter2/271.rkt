#lang sicp

;; weight for frequency
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

;; 定义一个 huffman 树，里面有保存字母表的 n 个字母
;; 每个字母对应的 frequency 分别是 1, 2, 4, ..., 2^(n-1)
(define (expt base n)
  (cond [(= base 0) 0]
        [(= n 0) 1]
        [(= n 1) base]
        [else (* base (expt base (- n 1)))]))
(expt 2 4)
(expt 0 1)
(expt 2 0)
(expt 3 1)


;; N = 5
;; {(A 1) (B 2) (C 4) (D 8) (E 16)}
(define n5
   (make-code-tree
    (make-leaf 'E (expt 2 4))
    (make-code-tree
     (make-leaf 'D (expt 2 3))
     (make-code-tree
      (make-leaf 'C (expt 2 2))
      (make-code-tree
       (make-leaf 'B (expt 2 1))
       (make-leaf 'A (expt 2 0)))))))

;; N = 10
;; a b c d e f g h i j
(define n10
  (make-code-tree
   (make-leaf 'J (expt 2 9))
   (make-code-tree
    (make-leaf 'I (expt 2 8))
    (make-code-tree
     (make-leaf 'H (expt 2 7))
     (make-code-tree
      (make-leaf 'G (expt 2 6))
      (make-code-tree
       (make-leaf 'F (expt 2 5))
       n5))))))

;; tests
n5
n10
(encode '(E) n5)
;; (0) => 1 bit
(encode '(A) n5)
;; (1 1 1 1) => 4 bits
(length (encode '(J) n10))
;; 1
(length (encode '(A) n10))
;; 10

;; 因此，频率最高的 symbol 只需要 1 bit 来编码，频率最低的需要 n-1 bits
;; 相应的 encode 数组（对应词频高到底）是 1, 2, 3, ..., n-1