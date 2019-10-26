# 应用序和正则序

**应用序先对参数求值后展开，正则序先展开后求值。**

## 例子

为了引入应用序和正则序这两种求值模型，我们先看一个过程(precedure)：

```racket
    (define (square x) (* x x))

    (define (sum-of-square x y)
        (+ (square x) (square y)))
```

采用应用序模型求 ``(sum-of-square (+ 5 1) (* 5 2))`` 过程如下：

```racket
    (sum-of-square (+ 5 1) (* 5 2))

    (sum-of-square 6 10)

    (+ (square 6) (square 10))

    (+ (* 6 6) (* 10 10))

    (+ 36 100)

    => 136
```

反之，使用正则序过程如下：

```racket
    (sum-of-square (+ 5 1) (* 5 2))

    (+ (square (+ 5 1)) (square (* 5 2)))

    (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))

    (+ (* 6 6) (* 10 10))

    (+ 36 100)

    => 136
```

大部分情况下，两种求值模型都能得到相同的解。但大致上，
Lisp 解释器使用应用序。

其中一个原因是，（我们看到第 3 步，）使用正则序可能会
对同一个表达式求值多次。

## 对比

少数情况下，两种求值模型会导致不同的结果，例如：

```racket
    (define (p) (p))

    (define (test x y)
        (if (= x 0)
            x
            y))
```

在这一过程 ``test`` 中，如果按应用序求 ``(test 0 (p))`` 的值，
会导致无限循环：

```racket
    (test 0 (p))

    ; 因为要对过程 (p) 求值，而求值的结果仍然是过程 (p)
    ; 结果永远会得到 (p)

    (test 0 (p))
    (test 0 (p))
    (test 0 (p))
    (test 0 (p))
    ...
```

但是按正则序的求值模型，

```racket
    (test 0 (p))

    (if (= 0 0)
        0
        (p))

    ; 到这里，if 会先对 (= 0 0) 求值以决定返回 0 还是 (p)

    (if #t
        0
        (p))

    => 0
```
    
这样的结果虽然跟 ``if`` 是特殊形式（以及它的被赋予了特殊过程
（第一个假设为真，则跳过下一个返回值））有关系。但我们可以看到
两种求值模型有可能带来不同的求值结果。

## if 是一种特殊形式

既然上面提到了 *if 是一种特殊形式* ，可以看如果自行定义一个过程
充当 ``if`` ，把它视为普通过程的一个特例会有什么结果。

*同时，下面的例子也起到和上面的例子类似的、说明应用序和正则序会产生
不同求值结果的作用。*

```racket
    (define (new_if predicate then_clause else_clause)
        (cond (predicate then_clause)
            (else else_clause)))
```

这样看起来问题不大，就算把这个过程用于执行 ``(new-if (= 2 3) 0 5)``
也能得到期望的结果。但是，试着重新定义 ``sqrt`` ，如下：

```racket
    (define (guess_loop guess x)
        (new_if (good_enough guess x)
                guess
                (guess_loop (improve guess x) x)))

根据应用序，运行过程如下：

```racket
    (sqrt 9)

    (guess_loop 1 9)

    (new_if (good_enough 1 9) 1 (guess_loop (improve 1 9) 9))

    ; 应用序先求值，于是在这里求值出现了问题
    ; 即便 (new_if #t ...) 也不会停止对 guess_loop 的调用
    
    (new_if #f 1            ; (guess_loop 5 9)
        (new_if #f 5        ; (guess_loop 3.4 9)
            (new_if #f 3,4
    ...

    (sqrt 9)

    (guess_loop 1 9)

    (new_if (good_enough 1 9) 1 (guess_loop (improve 1 9) 9))

    (cond (good_enough 1 9) 1
        (else (guess_loop (improve 1 9) 9)))

    ...
```

若干次循环后的某个 ``good_enough`` 返回一个真值，使 ``cond`` 通过测试，不再执行
后面的 ``else`` ，终止调用 ``guess_loop`` 。
