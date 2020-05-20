#lang racket

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
    [(delay-stream s) (memorize (lambda () s))]))

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

(define (ref-stream s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (- n 1))))

(define (for-each-stream s f)
  (if (null-stream? s)
      'done
      (begin (f (car-stream s))
             (for-each-stream (cdr-stream s) f))))

(define (filter-stream s p?)
  (cond [(null-stream? s)
         empty-stream]
        [(p? (car-stream s))
         (cons-stream (car-stream s)
                      (filter-stream (cdr-stream s) p?))]
        [else
         (filter-stream (cdr-stream s) p?)]))

(define (map-stream s f)
  (if (null-stream? s)
      empty-stream
      (cons-stream (f (car-stream s))
                   (map-stream (cdr-stream s) f))))

(define (enumerate-interval-stream n m)
  (if (> n m)
      empty-stream
      (cons-stream n
                   (enumerate-interval-stream (+ n 1) m))))

(define (display-stream s)
  (for-each-stream s (lambda (x)
                       (newline)
                       (display x))))

;; 我们有 sum
(define sum 0)

(define (accum x)
  (set! sum (+ sum x))
  sum)

;; 给出执行下面各表达式后 sum 的值
;(define seq (map-stream (enumerate-interval-stream 1 20) accum))
;; seq 的预期是 {1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210}
;; 但实际 map-stream 只执行了一次 accum，然后返回 (1, promise)
;; 因此 sum = 0 + 1
;> sum
;1
;(define y (filter-stream seq even?))
;; y 的预期是 {6, 10, 28, 36, 66, 78, 120, 136, 190, 210}
;; 但 filter-stream 只会往下 cdr-stream 直到满足 even? 为止
;; 然后返回 (6, promise)，因为 memorize 的原因，第一次 accum 的值无需再计算
;; 所以 accum 再执行了两次，因此 sum = 1 + 2 + 3
;> sum
;6
;(define z (filter-stream seq (lambda (x) (= (remainder x 5) 0))))
;; z 的预期是 {10, 15, 45, 55, 105, 120, 190, 210}
;; 类似上面，实际返回 (10, promise)，accum 再执行一次
;; 因此 sum = 6 + 4
;> sum
;10
;(ref-stream y 7)
;=> 136
;; 上面的表达式打印 y 的第 8 个元素，相应地 seq 也会 force 到 (136, promise)，因此
;> sum
;136
;(display-stream z)
;=> 10 15 45 55 105 120 190 210
;; 因为 z 的实际是 (10, promise)，其打印的输出如上。这条表达式没有调用 accum，因此 sum 的值不变。
;> sum
;136

;; 如果不使用 memorize 来「优化」delay
(define seq (map-stream (enumerate-interval-stream 1 20) accum))
;; sum = 1
(define y (filter-stream seq even?))
;; sum = 1 + 2 + 3 = 6
(define z (filter-stream seq (lambda (x) (= (remainder x 5) 0))))
;; 因为没有 memorize 导致了重复地 force delay（注意 x = 1 是没有包含在 promise 里面的）
;; 重复计算了 (delay .. 2 ..) 和 (delay .. 3 ..)，所以 sum = 6 + 2 + 3 + 4
;; sum = 6 + 2 + 3 + 4 = 15
(ref-stream y 7)
;; => 162
;; 因为前面的重复计算，seq 的预期值也发生了变化，因为此时 y = (1, promise), sum = 6
;; 那么求 seq 的第 8 个偶数 {1, 8, 11, 15, 19, 24, 30, 37, 45, 54, 64, 75, 87, 100, 114, 129, 145, 162, ...}
;; 此时 sum = 162
(display-stream z)
;; => 15 180 230 305
;; 此时 z = (15, promise), sum = 162，z 所含的 interval = {4 ... 20}
;; 那么 seq 的预期会变为 {15, 167, 173, 180, 188, 197, 207, 218, 230, 243, 257, 272, 288, 305, 323, 342, 362}
;; sum = 362