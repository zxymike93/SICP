#lang sicp

;; Ben 和 Alyssa 对实现 unless 是否依赖于 lazy eval 持不同观点，

;; Ben 认为：即便在 applicative order 的 Scheme 中也能实现 unless，它只不过是 special form
;; 比如说像在书中所描述那样，利用 if 来作为 unless 的替换，那么在 eval 里面做 unless->if 的转换即可
(define (eval-unless exp env)
  (eval (unless->if exp) env))

(define (unless->if exp)
  (define (predicate exp) (cadr exp))
  (define (alternative exp) (caddr exp))
  (define (consequence exp) (cadddr exp))
  (make-if (predicate exp)
           (consequence exp)
           (alternative exp)))

;; Alyssa 则认为：即便这样看起来能实现 unless，它仅仅是一种语法形式，而不是能被应用于高阶过程的过程
;; 比如说，4.26 里面我们用（应用序的） unless 定义了 factorial
;; 但假设求一个 (map factorial '(1 2 3 4 5))，根据 26 的论述是不能得到结果的

;; 不过考虑下面一个例子
(map unless '(#t #t #t #t) '(1 2 3 4) '(5 6 7 8))
'((unless #t 1 5)
  (unless #t 2 6)
  (unless #t 3 7)
  (unless #t 4 8))
'(5 6 7 8)
;; 可以看到，某种情况下 unless 也可以作为过程被高阶过程调用