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

(define (adjoin-set x set)
  (cond [(null? set)
         (list x)]
        [(< (weight x) (weight (car set)))
         (cons x set)]
        [else
         (cons (car set)
               (adjoin-set x (cdr set)))]))

(define (successive-merge pairs)
  (if (= (length pairs) 1)
      (car pairs)
      (let ([left (car pairs)]
            [right (cadr pairs)]
            [rest (cddr pairs)])
        (successive-merge (adjoin-set (make-code-tree left right) rest)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; Generate a corresponding Huffman tree
;; A    2    NA  16
;; BOOM 1    SHA  3
;; GET  2    YIP  9
;; JOB  2    WAH  1
(define set
   (list '(A 2) '(NA 16) '(BOOM 1) '(SHA 3)
         '(GET 2) '(YIP 9) '(JOB 2) '(WAH 1)))

(define h-tree (generate-huffman-tree set))

;; Encode the following song
;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip 
;; yip yip yip yip yip
;; Sha boom
(define msg
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP
        YIP YIP YIP YIP YIP
        SHA BOOM))
;;(1 1 1 1 1 1 1 0 0 1 1 1 1 0
;; 1 1 1 0 0 0 0 0 0 0 0 0
;; 1 1 1 1 1 1 1 0 0 1 1 1 1 0
;; 1 1 1 0 0 0 0 0 0 0 0 0
;; 1 1 0 1 0 1 0 1 0 1 0 1 0
;; 1 0 1 0 1 0 1 0 1 0
;; 1 1 1 0 1 1 0 1 1)
(length (encode msg h-tree))
;; 84

;;; How many bits are required for the encoding?
;;; 这首歌有36个单词,所以需要 36bits
;
;;; 如果使用定长的 code table 来 encode 最短需要多长？
;;; 每个单词需要 3bits
;;; A 000 NA 001 ...
