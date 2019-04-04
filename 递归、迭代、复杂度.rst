递归、迭代、复杂度
===================

    “学会下棋的规则 ≠
    成为象棋高手”。一名好的程序员应该像一名好的摄影师——在拍照前就在脑海中构建好作品的样子，以此推算拍摄的位置、角度、光圈大小、曝光时间等等。

在开始本文之前，首先要梳理一些相关的概念。在 Scheme 中：

1. 有基本的数据类型 1 2 3 4
2. 可以进行基本的运算操作 + - * /
3. 1 和 2 组成表达式
4. 表达式可以组合起来，成为更复杂的表达式
5. 通过 ``define`` 可以给命名一个值，或者过程，也就可以把 1~4 抽象成一个名称
6. 过程又能调用过程，形成更复杂的过程
7. 一系列的过程 **procedure** 可以构成一个大的计算过程 **process**

本文主要讨论一些常见的 procedures 以及由它们所构成的 processes，
另外，会谈论到不同 procedure 时间、空间复杂度。

线性递归和迭代
---------------

.. code-block:: scheme

    (iexpt 2 4 1)
    (iexpt (square 2) (/ 4 2) 1)
    (iexpt (square 4) (/ 2 2) 1)
    (iexpt 16 (- 1 1) (* 16 1))

    ; 应用序，要先求参
    ; 可以看出，它是迭代的
    ; 另外一个角度，只要知道所有的三个参数， b n a，就可以恢复运算
    
    (expt 2 4)
    (square (expt 2 2))
    (square (square (expt 2 1)))
    (square (square (* b (expt b (- 1 1)))))
    (square (square (* b 1)))

    ; 反观，因为无法得知参数 expt，square 无法求值
    ; 就要保存运算状态占用空间
    ; 另一角度来看，也就是无法通过恢复某个状态下的所有参数，恢复整个运算`

复杂度
-------

    空间复杂度，指：延后进行的运算。
    时间复杂度，指：基本的步骤(numerals / built-in procedures / lambda
    expressions)

为了说明不同 procedure 的复杂度，会举几个例子。

.. code-block:: scheme

    ; Linear recursive
    (define (factorial n)
        (if (= n 1) 1
            (* n (factorial (- n 1)))))

    ; Linear iterative
    (define (factorial n)
        (define (fact-iter product counter max-count)
            (if (> counter max-count) product
                (fact-iter (* counter product) (+ counter 1) max-count)))
        (fact-iter 1 1 n)
    )
