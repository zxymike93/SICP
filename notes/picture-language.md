# Example: A picture language
racket 提供了一个包，便于学习和做2.2章里面的图形联系。下面就跟着书中的例子来熟悉这门语言。

## 基本元素

要使用画图语言，需要在程序前面添加这两行（注意使用DrRacket），以及使用爱因斯坦来替代校长。

```racket
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; 相对于原作里面的 wave，这个包提供了另一个基本图像 einstein
einstein
;; 可以看到它作为一个图像（数据）是以 #procedure 的形式存在的
;; 和前面提到的 过程-数据 混淆的情况一样
;; 需要显式地使用 paint 过程来作画
(paint einstein)

;; 书中的基本例子也运行一遍
(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4 (below einstein2 einstein2))
(paint einstein2)
(paint einstein4)

;; 像一般过程（数据）一样抽象成更高层次
(define (flipped-pairs painter)
  (let ([painter2 (beside painter (flip-vert painter))])
    (below painter2 painter2)))

(define e4 (flipped-pairs einstein))

;; 同样也可以递归
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (right-split painter (- n 1))])
        (beside painter (below smaller smaller)))))
(paint (right-split einstein 4))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))]
            [right (right-split painter (- n 1))])
        (let ([tl (beside up up)]
              [br (below right right)]
              [corner (corner-split painter (- n 1))])
          (beside (below painter tl)
                  (below br painter))))))
(paint (corner-split einstein 1))

(define (square-limit painter n)
  (let ([quarter (corner-split painter n)])
    (let ([half (beside (flip-horiz quarter) quarter)])
      (below (flip-vert half) half))))
(paint (square-limit einstein 4))
```

## 高阶操作

和前面 higher-order-procedure 一样，可以接收 painter 为参数，可以返回 painter。

```racket
(define (square-of-four tl tr bl br)
  (lambda [painter]
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))

(define (flipped-pairs-2 painter)
  (let ([combine4 (square-of-four identity flip-vert
                                  identity flip-vert)])
    (combine4 painter)))
(paint (flipped-pairs-2 einstein))

(define (square-limit-2 painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)])
    (combine4 (corner-split painter n))))
(paint (square-limit-2 einstein 4))
```

