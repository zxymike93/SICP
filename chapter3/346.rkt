#lang sicp

(define (make-serialier)
  (let ([mutex (make-mutex)])
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ([val (apply p args)])
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ([cell (list #f)])
    (define (mutex m)
      (cond [(eq? m 'acquire)
             (if (test-and-set! cell)
                 (mutex 'acquire)
                 'acquired)]
            [(eq? m 'release)
             (clear! cell)]))
    mutex))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(define (clear! cell)
  (set-car! cell #f))


;; 假设我们有这样一个例子
(define s (make-serializer))
(define (foo) (set! x (* x x)))
(define (bar) (set! x (+ x 1)))
;; 并发地执行这两个的过程
(parallel-execute (s foo)
                  (s bar))
;; 因为 serializer 返回 serialized-p，实际展开为
;; 注意，这两个 [] 括起来的过程体所在的环境均有一个 mutex
;; mutex 中 cell 是这样的一个结构 (#f) / (#t)
(parallel-execute [(mutex 'acquire)
                   (let ([val (apply foo)])
                     (mutex 'release))]
                  [(mutex 'acquire)
                   (let ([val (apply bar)])
                     (mutex 'release))])
;; 在初次运行的时候，执行一个过程体的时候，首先 (mutex 'acquire)
(if (test-and-set! cell) ; false
    ...)
;; 对 if 的谓词进行估值
(test-and-set! cell)
(begin (set-car! cell #t)
       #f)
;; 那么另一个过程体对 (mutex 'acquire) 估值
(if (test-and-set! cell) ; true
    (mutex 'acquire)
    ...)
;; 对 if 的谓词进行估值
(if (car cell) ; true
    #t
    ...)
;; 于是执行 (mutex 'acquire) 来“询问” mutex 是否可以获取
;; 当第一个过程体执行完，调用 (mutex 'release)，另一个过程体才得以执行
(clear! cell)
(set-car! cell #f)

;; 相反地，如果 test-and-set! 没有实现原子化，那么上面 [] 中的两个过程体同时
(mutex 'acquire)
(test-and-set! cell)
;; 并不妨碍它们的同时执行，则 mutex 就失去了其作用。

;; 这里补充一句个人理解：
;; cell 或者 mutex 的实现取决于解释器（基于机器指令集）的实现，利用实现上原子性的元素。
;; 比如注释里面提到的「采用时间片模型但处理器的 MIT-Scheme」。目前书中给出的只是概念和思路。