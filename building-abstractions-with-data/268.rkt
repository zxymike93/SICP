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

;; A D A B B C A -> 0 110 0 10 10 111 0
(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

;; check
(define (encode-symbol char tree)
  (cond [(leaf? tree)
         (if (not (eq? char (symbol-leaf tree)))
             (error "[encode-symbol] BAD SYMBOL: " char)
             '())]
        [(check-branch char (left-branch tree))
         (cons 0 (encode-symbol char (left-branch tree)))]
        [else
         (cons 1 (encode-symbol char (right-branch tree)))]))

;; decide which branch to follow by testing to see
;; which branch either is the leaf node for the symbol
;; or contains the symbol in its set
(define (check-branch char branch)
  (define element-of-set? memq)
  (if (leaf? branch)
      (eq? char (symbol-leaf branch))
      (element-of-set? char (symbols branch))))

;; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
(define sample-tree
  (make-tree 
   (make-leaf 'A 4)
   (make-tree
    (make-leaf 'B 2)
    (make-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define another-tree
  (make-tree (make-leaf 'A 8)
             (make-tree (make-tree (make-leaf 'B 3)
                                   (make-tree (make-leaf 'C 1)
                                              (make-leaf 'D 1)))
                        (make-tree (make-tree (make-leaf 'E 1)
                                              (make-leaf 'F 1))
                                   (make-tree (make-leaf 'G 1)
                                              (make-leaf 'H 1))))))

(encode '(A D A B B C A) another-tree)
(encode '(F A B C D) another-tree)

(encode '(A D A B B C A) sample-tree)
(encode '(F A B C D) sample-tree)