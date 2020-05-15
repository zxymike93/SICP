;; Louis 的说法是错的，可以使用对自身串行过的账户（二不用对 transfer 串行）。
;; 1、exchange 依赖于 different 的值，也就是受不同账户的值影响，也就是这个过程受时间影响（因为这些值单独受时间影响）
;; 2、transfer 中 amount 是给定的