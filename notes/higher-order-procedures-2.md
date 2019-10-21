高阶函数入门
===========

本文目标是：
把几个相同的 procedures 逐渐地抽象成更通用的过程，
借此从更整体的角度理解 *SICP* 中高阶函数章节。

最初的例子
---------

a~b 差为 1 的等差数列

.. code-block:: scheme

  (define (sum-int a b)
    (if (> a b)
        0
        (+ a
           (sum-int (+ a 1) b))))
           
        
计算 1/8 π

.. code-block:: scheme

  (define (sum-pi a b)
    (if (> a b)
        0
        (+ (/ 1 (* a (+ a 2)))
           (sum-pi (+ a 4) b))))
            
 共同点
 ~~~~~~
 
如果把计算的数学公式写下来，比较容易能观察得出 ``f(a1) + f(a2) + f(a3) + ... + f(an)`` ，
其中 ``f(x)`` 和 ``a1, a2, a3`` 分别对应两个过程 + 运算的两个元素。

上面的分析结论转为代码可以这样描述：

.. code-block:: scheme

  (define (sum a b)
    (+ (f a)
       (sum (next a) b)))
