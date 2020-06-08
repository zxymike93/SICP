;;; 从 next-to 的规则定义发现它是一个递归的规则
;;; 一个 list 中符合 next-to 的两个元素，总是列表的头两个元素，或者子列表的头两个元素
(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;;; (?x next-to ?y in (1 (2 3) 4))

(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

;;; (?x next-to 1 in (2 1 3 1))

(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))