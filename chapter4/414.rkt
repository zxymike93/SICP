;; 自定义的 map 通过 define-variable! 被绑定到 global-env 中，当调用如 (map display '(1 2 3)) 的时候，被分派到 application? 下，通过查找变量 map 找到对应的过程来执行。
;; 如果使用内置的 map，同样会被分派到 application? 下，但无法找到对应的过程（除非用某种方式绑定到环境中，比如在 primitives 里面映射 (list 'map map)。
