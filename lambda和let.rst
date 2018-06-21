lambda 和 let
=============

lambda
------

``lambda`` 和使用 ``define`` 定义过程唯一的区别就是：
** ``lambda`` 不给过程绑定变量名。** （所以有的翻译也使用“匿名函数”）

因此，下面两个过程应用起来是等价的：

.. code-block:: scheme

  (define (sum a b) (+ a b))
  
  (sum 1 2)

  ;Value: 3

  ((lambda (a b) (+ a b)) 1 2)

  ;Value: 3
  
如果这样写： ``(define sum (lambda (a b) (+ a b)))`` 也是可以给这个过程绑定变量名的。

上面列举的例子在 `Python 的 lambda`_ 中有类似的写法，据说这是 Python 借鉴了 Scheme 的地方，
所以无论从定义、调用还是含义上都和 Scheme 的 lambda 几乎一样。

.. _Python 的 lambda: https://github.com/gayu-mike/python-level-up/blob/master/tutorial/lambda.rst

let
---

.. code-block:: scheme

  (let ([var1 expr1]
        [var2 expr2]
        ...
        [var expr])
    body)

上面是 ``let`` 的书写形式，在 Scheme 中，它是借助 `lambda` 来实现的，
上面的过程会被替换为：

.. code-block:: scheme

  ((lambda (var1 var2 ... var) (body))
    expr1 expr2 ... expr)
    
let 中变量的 scope 问题
~~~~~~~~~~~~~~~~~~~~~~

特别指出 scope 的问题是因为 ``let`` 有个“坑”。

之所以称之为坑只是相对而言，是比较偏颇的说法。我在学习 ``let`` 的时候只了解并习惯了
``define`` 这种形式（如果正确理解 let 的实现就不容易掉坑）。

我们继续从实际例子出发，假设定义好 ``(define x 2)`` ：

.. code-block:: scheme

  (+ (let ([x 3])
       (+ x (* x 10)))
     x)
     
  ;Value 35
  
在 *body* ``(+ x (* x 10))`` 估算的时候，引用的是 *var* （本地变量），即 ``[x 3]`` 绑定的值。
因此，计算过程为 ``(+ 3 (* 3 10)`` 。而另一个相加数 ``x`` 引用的是前面定义好的 ``(define x 2)`` 的值。

这个例子不容易计算出错，但有可能不完全正确地理解。（刚开始我错误地以为 ``(let ....)`` 的 *scope* 就在 ``()`` 中。）
实际上， **let 语句中，expr 的 scope 在 let 之外。**

如果不理解上面这段话，在碰到这类代码的时候就容易计算错误
（expr 中引用的变量名恰好和同一个 let 语句下的 var 同名）：

.. code-block:: scheme

  (let ([x 3]
        [y (+ x 2)])
    (* x y))
  
  ;Value 12
  
因为在给 ``y`` 绑定值的时候， ``[y (+ x 2)]`` 引用的是自由变量 ``x`` 而非 let 中的 ``x`` ，
即 ``[y (+ 2 2)]`` 。

其实根据上面小节所述，把 let 替换成 lambda 形式可能相对容易理解：

.. code-block:: scheme

  ((lambda (x y) (* x y)
   3 (+ x 2))
   
为了进一步验证这一点，我对上面的过程稍作了改动来测试。
通过这个测试，很容易看到，给 ``b`` 绑定值的表达式中，即便引用 ``a`` ，也不是 let 下定义的 ``a``：

.. code-block:: scheme

  (let ([a 3]
        [b (+ a 2)])
    (* a b))
  
  error => unbound identifier in module in: a
