#lang sicp

;;; 由于并发，同一资源可能会被一个以上过程修改。
;;; 以 (set! x 1) 为例，对应的机器指令有：
;;; 1. acquire register1 (for) x
;;; 2. set register1 (to value) 1
;;; 3. store (value of) register1 (to) x
;;; 上述步骤如果在多个过程对一个对象的修改中出现时序问题，就可能导致程序错误。

;;; 因此关于并发，要考虑的四个问题：
;;; - incorrect 所谓正确并非唯一答案，而是可以接受的结果。
;;;   通常只要并发执行的结果等于多个过程按某一顺序执行的结果，即认为正确。
;;; - inefficent 为了控制修改导致的不正确性，出现了锁(mutex)的机制。
;;;   如果锁的范围过大，会影响并发的效率；反之，会影响正确性。
;;; - deadlock 在互相获取资源的带锁过程中，可能出现一种情况，双方都在等对方资源释放而中止。
;;;   实践中的一种做法是，由另一监控线程终止并回滚其他过程，先让某一过程正常执行。
;;; - unfair 指并发队列中的某个过程总是优先获得资源/某个过程一直无法获得资源。
;;;   最简单的解决方法是轮询(round-robin)，即记住上次结束的过程从下一个队列元素开始。

;;; 补充一点关于锁需要了解的是其层次结构：
;;;    application     (PL level)
;;; ----[serializer]----
;;;  critical section  (OS level)
;;; ------[mutex]------
;;;   test-and-set!    (machine level)

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))