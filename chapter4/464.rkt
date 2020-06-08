;;;; 此题讨论 infinite loop 的问题

;;; 原来的 outranked-by 规则如下
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;;; 被不小心改成下面的定义
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

;;; 如果执行这条查询语句

(outranked-by (Bitdiddle Ben) ?who)

;;; 按照原来的规则定义：
;;; 1. 在 frame 里面，?staff-person: (B Ben) ?boss: ?who，应用到规则体中
;;; 2.1 (supervisor (B Ben) ?who) => frame，绑定 ?who
;;; 2.2.1 (supervisor (B Ben) ?middle-manager) => frame，绑定 ?middle-manager
;;; 2.2.2 frame => (outranked-by ?middle-manager ?boss) => frame，绑定 ?boss，其中输入的 frame 是 2.2.1
;;; 3. 2.2.2 里面查询的 outranked-by 将反复执行查询 2.1~2.2

;;; 按照新的规则定义：
;;; 1 2.1 同上
;;; 2.2.1 查询 (outranked-by ?middle-manager ?who) 要查询下一个 outranked-by，如此循环
;;; 因此无法传递一个 frame 给 (supervisor ?staff-person ?middle-manager)
;;; 因此 (and ...) 也不会有结果而无限循环下去