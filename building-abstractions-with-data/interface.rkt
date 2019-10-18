#lang sicp

;; 在高阶函数部分，是通过程序结构的相似性来抽象出高阶过程的
;; 而对于复合数据，也有对应的抽象层次 conventional interface

;; 不过一般不能简单地通过观察函数结构抽取出来
;; 比如下面两个例子，均由 enumerate, filter, map, accumulate 组成


;; 1 求树中奇数的平方和
;; [enumerate] -> [filter]  ->  [map]  -> [accumulate]
;; tree leaves      odd?       square      +, 0
;; 遍历树叶，判断是否为奇数，将奇数平方，求和

(define (sum-odd-squares tree)
  (cond [(null? tree)
         0]
        [(not (pair? tree))
         (if (odd? tree)
             (square tree)
             0)]
        [else
         (+ (sum-odd-tree (car tree))
            (sum-odd-tree (cdr tree)))]))

;; 2 将 fib(n) 数列中的偶元素重组
;; [enumerate] -> [map]   -> [filter] -> [accumulate]
;;    k            fib         even?        cons, ()
;; 遍历下标k，求其对应的fib值，其中的偶数值，重组到()中

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (if (even? (fib k))
            (cons (fib k)
                  (next (+ k 1)))
            (next (+ k 1)))))
  (next 0))

;; 它们的结构不同由于以下两点
;; 1、几个抽象部分顺序不同（如上所示）
;; 2、各部分在实际写法中不是简单的一一对应（如遍历树叶就分为 null? pair? 两个测试）

;; 如果我们可以抽象出一种形式，使得过程可以按 signal(enumerate, filter, ...) 传递，那就可以抽象出更高阶的过程
;; 通过使用 (list) 作为数据层，可以设计一些针对 list 传递的、通用的 signals

(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items))
            (map f (cdr items)))))

(define (filter predicate items)
  (cond [(null? items)
         nil]
        [(predicate (car items))
         (cons (car items) (filter predicate (cdr items)))]
        [else
         (filter predicate (cdr items))]))

(define (accumulate f init items)
  (if (null? items)
      init
      (f (car items)
         (accumulate f init (cdr items)))))

;; 这里 enumerate 作为程序“入口”要差别设计，比如
;; 对于树，要做数据结构转换 tree -> list
;; 对于区间， int, int -> list
;; 等等...

(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

;; 这样定义开头的两个过程，就可以在 list 数据层之上定义

;; [enumerate] -> [filter]  ->  [map]  -> [accumulate]
;; tree leaves      odd?       square      +, 0
(define (sum-odd-square tree)
  (accumulate
   +
   0
   (map square (filter odd? (enumerate-tree tree)))))

(define (even-fib n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

;; hint:将 list 视为一个类，其实是面向对象的思想