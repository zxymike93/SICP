#lang sicp

(define (make-deque)
  (let ((name 'deque)
        (front '())
        (rear '()))
    (cons name (cons front rear))))

(define (value node)
  (car node))

(define (ptrs node)
  (cdr node))

(define (previous-node node)
  (car (ptrs node)))

(define (next-node node)
  (cdr (ptrs node)))

(define (front-ptr deque)
  (car (ptrs deque)))

(define (rear-ptr deque)
  (cdr (cdr deque)))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

;; O(n)
(define (print-deque deque)
  (define (print-node node)
    (cond ((not (null? (next-node node)))
           (cons (value node)
                 (print-node (next-node node))))
          (else
           (cons (value node) '()))))

  (if (empty-deque? deque)
      '()
      (print-node (front-ptr deque))))

(define (set-front-ptr! deque item)
  (set-car! (ptrs deque) item))

(define (set-rear-ptr! deque item)
  (set-cdr! (ptrs deque) item))

(define (front-insert-deque! deque item)
  (let ((new-node (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-car! (ptrs (front-ptr deque)) new-node)
           (set-cdr! (ptrs new-node) (front-ptr deque))
           (set-front-ptr! deque new-node)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-node (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-cdr! (ptrs (rear-ptr deque)) new-node)
           (set-car! (ptrs new-node) (rear-ptr deque))
           (set-rear-ptr! deque new-node)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DEL calls on empty deque" deque))
        (else
         (cond ((null? (next-node (front-ptr deque)))
                (set-front-ptr! deque '())
                (set-rear-ptr! deque '())
                deque)
               (else
                (set-front-ptr! deque (next-node (front-ptr deque)))
                (set-car! (ptrs (front-ptr deque)) '())
                deque)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DEL calls on empty deque" deque))
        (else
         (cond ((null? (previous-node (rear-ptr deque)))
                (set-rear-ptr! deque '())
                (set-front-ptr! deque '())
                deque)
               (else
                (set-rear-ptr! deque (previous-node (rear-ptr deque)))
                (set-cdr! (ptrs (rear-ptr deque)) '())
                deque)))))

;; tests
(define d (make-deque))
d
(front-ptr d)
(rear-ptr d)
(empty-deque? d)
(print-deque (front-insert-deque! d 'a))
(print-deque (front-insert-deque! d 'b))
(print-deque (front-insert-deque! d 'c))
(print-deque (rear-insert-deque! d 1))
(print-deque (rear-insert-deque! d 2))
(print-deque (rear-insert-deque! d 3))
(print-deque (front-delete-deque! d))
(print-deque (rear-delete-deque! d))
(print-deque (rear-delete-deque! d))
(print-deque (rear-delete-deque! d))
(print-deque (rear-delete-deque! d))
(print-deque (rear-delete-deque! d))
(print-deque (rear-insert-deque! d 'foo))
(print-deque (front-delete-deque! d))