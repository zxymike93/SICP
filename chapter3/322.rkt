#lang sicp

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    
    (define (set-front-ptr! item)
      (set! front-ptr item))
    
    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))

    (define (print-queue)
      front-ptr)

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))

    (define (delete-queue!)
        (cond ((empty-queue?)
               (error "DEL called with an empty queue"))
              (else
               (set-front-ptr! (cdr front-ptr))
               dispatch)))

    (define (dispatch m)
      (cond [(eq? m 'front-ptr) front-ptr]
            [(eq? m 'rear-ptr) rear-ptr]
            [(eq? m 'set-front-ptr!) set-front-ptr!]
            [(eq? m 'set-rear-ptr!) set-rear-ptr!]
            [(eq? m 'empty-queue?) empty-queue?]
            [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) delete-queue!]
            [(eq? m 'front-queue) front-queue]
            [(eq? m 'print-queue) print-queue]
            [else
             (error "Bad operation call on queue" m)]))
    dispatch))

(define (front-ptr queue)
  (queue 'front-ptr))

(define (rear-ptr queue)
  (queue 'rear-ptr))

(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))

(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (front-queue queue)
  (queue 'front-queue))

(define (print-queue queue)
  ((queue 'print-queue)))

;; tests
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