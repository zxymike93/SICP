# 魔法师入门指南

> “I think that it’s extraordinarily important that we in computer science keep fun in computing.
> When it started out, it was an awful lot of fun. Of course, the paying customers got shafted every now and then,
> and after a while we began to take their complaints seriously. We began to feel as if
> we really were responsible for the successful, error-free perfect use of these machines.
> I don’t think we are. I think we’re responsible for stretching them, setting them off in new directions,
> and keeping fun in the house. I hope the field of computer science never loses its sense of fun.
> Above all, I hope we don’t become missionaries. Don’t feel as if you’re Bible salesmen. The world has too many of those already.
> What you know about computing other people will learn. Don’t feel as if the key to successful computing is only in your hands.
> What’s in your hands, I think and hope, is intelligence:
> the ability to see the machine as more than when you were first led up to it, that you can make it more.”
>
> —Alan J. Perlis (April 1, 1922 – February 7, 1990)

## 如何开始？

1. 《计算机程序的构造和解析》课本，或者 [the HTML Version](http://sarabander.github.io/sicp/) 。
2. 下载 [Racket](https://download.racket-lang.org/) 来完成课本中的习题。
3. 和基友交流，以及参考 schemewiki 上[大家的解答](http://community.schemewiki.org/?SICP-Solutions) 。

## 三大主题

### 抽象 *(Black-box Abstraction)*

1. primitive objects
  - primitive procedures
  - primitive data
2. means of combination
  - procedure composition
  - construction of compound data
3. means of abstraction
  - procedure defination
  - simple data abstraction
4. capturing common patterns
  - high-order procedures
  - data as abstraction

### 接口 *(Conventional Interfaces)*
> 如何控制程序的复杂度

1. generic operations
2. large-scale structure and modularity
3. object-oriented programming
4. operations on aggregates

### 元语言抽象 *(Meta-linguistic Abstraction)*
> 如何构建一门新语言

1. interpretation
2. logic programming
3. register machines

## Lisp 介绍

> 实际上这部分讲的是通用语言所需要的特性，虽然以 Scheme 为例。

一门通用语言需要的三个基本特性：

1. primitive objects: + - * / > < = 1 1.0
2. means of combination: () cond else if
3. means of abstraction: define

### 1.2.2 Tree Recursion

根据斐波那契的定义，

  Fib(n) = 0, n = 0
         = 1, n = 1
         = Fib(n-1) + Fib(n-2)

很直观地就可以写出一个过程：

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))

但是这个估值过程是树状递归的，有大量计算重复，比如下面 [] 框出的地方， [+ (fib 1) (fib 0)] 就分别计算了2次。另外空间复杂度也是 O(n)，因为要用栈保存计算状态。

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

将递归的估值过程转换为迭代的估值过程，可以手动做“计数”。

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