## Lisp 介绍

> 实际上这部分讲的是通用语言所需要的特性，虽然以 Scheme 为例。

一门通用语言需要的三个基本特性：

1. primitive objects: + - * / > < = 1 1.0
2. means of combination: () cond else if
3. means of abstraction: define


### 1.2.2 Tree Recursion

#### 斐波那契数列

```racket
  Fib(n) = 0, n = 0
         = 1, n = 1
         = Fib(n-1) + Fib(n-2)
```

很直观地就可以写出一个过程：

```racket
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))
```

但是这个估值过程是树状递归的，有大量计算重复，比如下面 [] 框出的地方， [+ (fib 1) (fib 0)] 就分别计算了2次。另外空间复杂度也是 O(n)，因为要用栈保存计算状态。

```racket
(fib 4)

(+ (fib 3)
   (fib 2))

(+ (+ (fib 2)
      (fib 1))
   (+ (fib 1)
      (fib 0)))

(+ (+ [+ (fib 1)
         (fib 0)]
      (fib 1))
   [+ (fib 1)
      (fib 0)])
```

将递归的估值过程转换为迭代的估值过程，可以手动做“计数”。

```racket
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib 4)
(fib-iter 1 0 4)
(fib-iter 1 1 3)
(fib-iter 2 1 2)
(fib-iter 3 2 1)
(fib-iter 5 3 0)
3
```

#### 唱零钱问题

```racket
;; 如果有 kind 种硬币,deomination 返回最贵的硬币的面值
(define (deomination kind)
  (cond [(= kind 1) 1]
        [(= kind 2) 5]
        [(= kind 3) 10]
        [(= kind 4) 25]
        [(= kind 5) 50]))

; ∵ 不用硬币 A 的所有排法 + 一定用硬币 A 的所有排法 = 所有排法
; 又 ∵ 一定用 A = 至少用 1 个 A
; ∴ (= (cc amount kind-of-coin)
;      (+ (cc amount (- kind-of-coin 1)
;         (cc (- amount (一个 A 的面值)) kind-of-coin))))
(define (cc amount kind-of-coin)
  (cond [(= amount 0)
         1]
        [(or (< amount 0) (= kind-of-coin 0))
         0]
        [else
         (+ (cc amount (- kind-of-coin 1))
            (cc (- amount (deomination kind-of-coin)) kind-of-coin))]))

(define (count-change amount)
  (cc amount 5))
```

### 1.2.6 素数测试

> 通过介绍 2 种素数测试的方法，展示时间复杂度分别为 √ 和 log 的过程

```racket
;; 测试素数，两种复杂度的算法
;; 1、根号n
;; 2、log2n

;; 如果 d 是 n 的除数，那么 n/d 也为 n 的除数。d 和 n/d 不能同时小于 根号n
;; 因此，这个直接“遍历”的测试方法只需测试 1~根号n 个数，复杂度为 根号n

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (cond [(> (square d) n) n]
        [(divides? d n) d]
        [else (find-divisor n (+ d 1))]))

(define (divides? n d)
  (= 0 (remainder n d)))

;; a < n & n is prime. then, remainder(a^n, n) = remainder(a, n)
;; 如果n不是素数，大部分a都不会符合，所以有了费马测试
;; 测试较多的a，随着测试的数越多，越能证明n是素数

(define (fermat-test n)
  (define (try a)
    ;; 
    (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))))
```