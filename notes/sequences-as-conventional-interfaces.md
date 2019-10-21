# 序列作为约定界面

在高阶函数部分，是通过程序结构的相似性来抽象出高阶过程的。而对于复合数据，也有对应的抽象层次 conventional interface。

## 原始例子

不过一般不能简单地通过观察函数结构抽取出来。比如下面两个例子，均由 enumerate, filter, map, accumulate 组成：

1. 求``树``中奇数的平方和

   > [enumerate] -> [filter]  ->  [map]  -> [accumulate]
   >
   > ​    tree leaves      odd?       square            +, 0

```racket
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
```

2. 将 fib(n) ``数列``中的偶元素重组

   > [enumerate] -> [map]   -> [filter] -> [accumulate]
   >              k                fib         even?         cons, ()

```racket
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
```



它们的结构不同由于以下两点

- 几个抽象部分顺序不同（如上所示）
- 各部分在实际写法中不是简单的一一对应（如遍历树叶就分为 null? pair? 两个测试）

## 使用 Sequence 作为数据层

### 针对 Sequence 传递的高阶过程

如果我们可以抽象出一种形式，使得过程可以按 signal(enumerate, filter, ...) 传递，那就可以抽象出更高阶的过程。

通过使用 (list) 作为数据层，可以设计一些针对 list 传递的、通用的 signals：

```
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
```

由于上面两个例子**原始数据**不同（一个是``树``一个是``区间``），但是最终都要表示为``序列``这种抽象。

### 构造 Sequence 界面

因此分别构造两个 ``enumerate`` 作为界面，使得通过这一界面访问任意数据（无视其本身实现），都可以将其视为``序列``：

- ``(enumerate tree) -> sequence``
- ``(enumerate interval) -> sequence``

```racket
(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))
```

### 使用

**从其他语言的角度来看，

```racket
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
```

## 结语

从其他语言的视角来看，我们类似于将``sequence``视为一个类/ADT，几个 signals 相当于它的操作/方法。