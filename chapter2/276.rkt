;; dispatch
;; +type: 要使用 tag、独立的函数名称
;; +operation: 所有的 operation 也要跟着修改函数名

;; data-directed
;; +type: (install-package-...)
;; +operation: (put 'op '(type) obj)

;; message passing
;; +type: (make-from...) 同时在这个构造函数的时候已经选定了实现方式
;; +operation: (eq? op 'op) lambda...