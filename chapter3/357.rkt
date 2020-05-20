#lang sicp

;; 当我们计算第 n 个斐波那契数的时候，比如：(ref-stream fibs n)，需要做多少次加法？
;; （这里不回答具体的数字，而是分析时间复杂度）
;(fibs (0 (1 (add (cdr fibs) fibs)))) ;1
;(fibs (0 (1 (add (0 (1 (cdr (add (cdr fibs) fibs)))) ;2
;                 (0 (1 (add (cdr fibs) fibs))))))) ;3
;...

;; 如果使用 memo-proc 来优化 delay，(cdr-stream fibs) 总会来自与缓存，n+1 只会增加 1 次加法操作
;; 因此，它的时间复杂度是 O(n)

;; 如果不使用 memo-proc 来优化 delay，则 (add-stream (cdr-stream fibs) fibs) 会以树状展开
;; 因此，它的（时间复杂度）计算次数会呈指数增长

;; 我们用代码来分析计算次数
(define (memorize proc)
  (let ([run? #f]
        [result #f])
    (lambda ()
      (if (not run?)
          (begin (set! result (proc))
                 (set! run? #t)
                 result)
          result))))

(define-syntax delay-stream
  (syntax-rules ()
    ;[(delay-stream s) (memorize (lambda () s))]))
    [(delay-stream s) (lambda () s)]))

(define (force-stream s)
  (s))

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream a b) (cons a (delay-stream b))]))

(define (car-stream s)
  (car s))

(define (cdr-stream s)
  (force-stream (cdr s)))

(define empty-stream '())

(define (null-stream? s)
  (null? s))

(define (map-stream proc . argstreams)
  (if (null-stream? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map car-stream argstreams))
       (apply map-stream
              (cons proc (map cdr-stream argstreams))))))

(define counter 0)
(define (add-stream s1 s2)
  (map-stream (lambda (x y)
                (begin (set! counter (+ counter 1))
                       (+ x y)))
              s1 s2))

(define (ref-stream s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (- n 1))))

;; 斐波那契数列的流定义如下
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-stream (cdr-stream fibs) fibs))))

;; 查看 add 的计算次数
(define (display-line x)
  (display x)
  (newline))
(ref-stream fibs 3)
(display-line counter)
(ref-stream fibs 5)
(display-line counter)
(ref-stream fibs 7)
(display-line counter)
(ref-stream fibs 9)
(display-line counter)
(ref-stream fibs 15)
(display-line counter)
(ref-stream fibs 21)
(display-line counter)
(ref-stream fibs 33)
(display-line counter)
;; 可以发现复杂度和上述的解释一致