;; 创造一门 pattern-match language, 它的语法如下
;; (foo bar) -> 一个含有两个元素 foo bar 的表。比如 (dd (?c c) (? v)) 含有3个元素 dd (?c c) (? v)
;; foo -> 表示自身。比如 + 即代表加法
;; (? x) -> 匹配一个元素（这个元素按法则1匹配，可以是任意值、也可以是表达式），命名为 x。其中，?c ?v 是特殊情况。
;; (: x) -> 从匹配到的元素里面找到 x，替换为被匹配的原值。

;; 将上述语言用于定义一套求导公式匹配规则
;; deriv-rules 是一个 list
(define deriv-rules
  '(;; dc/dv = 0
    ((dd (?c c) (? v)) 0)

    ;; dv/dv = 1
    ((dd (?v v) (? v)) 1)

    ;; du/dv = 0
    ((dd (?v u) (? v)) 0)

    ;; d(x1+x2)/dv = dx1/dv + dx2/dv
    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))

    ;; d(x1x2)/dv = x1(dx2/dv) + x2(dx1/dv)
    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (dd (: x1) (: v)) (: x2))))
    ))