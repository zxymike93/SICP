#lang sicp

(define (make-queue)
  (cons '() '()))

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           ;; point cdr-last-pair to new-pair
           (set-cdr! (rear-ptr queue) new-pair)
           ;; point rear-ptr to new-pair
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DEL called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (print-queue queue)
  (front-ptr queue))

;; 解释：为什么 repl 打印出来的队列并不直观？
(define q (make-queue))

(insert-queue! q 'a)
;; ((a) a)
(insert-queue! q 'b)
;; ((a b) b)
(insert-queue! q 'c)
;; ((a b c) c)
(delete-queue! q)
;; ((b c) c)
(delete-queue! q)
;; ((c) c)
(delete-queue! q)
;; (() c)

;; 因为 repl 读取 queue 的时候会打印 front-ptr rear-ptr
;; front-ptr 指向第一个节点，第一个节点指向第二个节点，...，直到最后一个节点
;; 所以相当于多打印了 rear-ptr
;; 因此 print-queue 只需打印 front-ptr 即可使得 queue 的展示符合人的直观
(define p (make-queue))

(print-queue (insert-queue! p 'a))
;; (a)
(print-queue (insert-queue! p 'b))
;; (a b)
(print-queue (insert-queue! p 'c))
;; (a b c)
(print-queue (delete-queue! p))
;; (b c)
(print-queue (delete-queue! p))
;; (c)
(print-queue (delete-queue! p))
;; ()