;;; 参考 4.65 的问题，假设 Ben 实现的 accumulate-function 就是书中的 sum
;;; (sum ?amount (wheel ?who)) 不会求的所有 wheel 的薪金只和，因为 Oliver 就出现了 4 次

;;; 一种解决方法是：
;;; 原来 Ben 的想法下，比如 sum 是要被实现为形如
(sum <variable> <query>
     (accumulate + <empty-frame> (qeval <query>)))
;;; 通过增加一个 filter，比如
(sum <variable> <query>
     (accumulate + <empty-frame> (filter f (qeval <query))))
;;; 这里的 f 实现为一个唯一标识的收集器，比如记录 name