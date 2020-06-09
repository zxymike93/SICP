;;; 原始的函数定义

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;; 图像语言（by codybartfast from schemewiki）

;; data-path
;┌───────┐   ┌───────┐    ,─.    ┌───────┐
;│   p   │   │   c   ├──>( > )<──┤   n   │
;└─────┬─┘   └───────┘    `─'    └───────┘
;  ^   │       │   ^
;  │   │       │   │           ^
;  X   │       │   X          /1\
;  │   │       │   │          ─┬─
;  │   │       ├───│───┐       │
;  │   v       v   │   v       v
;  │  ───────────  │  ───────────
;  │   \  mul  /   │   \  add  /
;  │    ───┬───    │    ───┬───
;  └───────┘       └───────┘

;; controller
;      start
;        │
;        │
;        v
;        ^
;       / \ yes
;┌────>( > )───> done
;│      \ /
;│       V
;│       │
;│       │no
;│       v
;│  ┌─────────┐
;│  │  p<-mul │
;│  └────┬────┘
;│       │
;│       v
;│  ┌─────────┐
;│  │  c<-add │
;│  └────┬────┘
;└───────┘

;;; D-O-C 的设计方式

(data-paths
 (registers
  ((name n))
  ((name product)
   (buttons ((name product<-1) (source (constant 1)))
            ((name product<-mul) (source (operation mul)))))
  ((name counter)
   (buttons ((name counter<-1) (source (constant 1)))
            ((name counter<-add) (source (operation add)))))))

(operations
 ((name mul)
  (inputs (register counter) (register product)))
 ((name add)
  (inputs (register counter) (constant 1)))
 ((name >)
  (inputs (register counter) (register n))))

(controller
 (product<-1)
 (counter<-1)
 test-conter
   (test >)
   (branch (label fact-done))
   (product<-mul)
   (counter<-add)
 fact-done)

;;; 机器语言

(controller
 (assign product (constant 1))
 (assign counter (constant 1))
 test-conter
   (test (op >) (reg counter) (reg n))
   (branch (label fact-done))
   (assign product (op mul) (reg counter) (reg product))
   (assign counter (op add) (reg counter) (constant 1))
   (goto (label test-counter))
 fact-done)