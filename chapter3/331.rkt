;; 为什么 (add-action!) 需要先调用一次 (new-action)？
;; 请通过解释 sample 的具体调用过程来说明。

#lang sicp

(define (make-wire)
  (let ((signal 0)
        (actions '()))
    ;; ...
    (define (accept-procs! new-action)
      (set! actions (cons new-action actions))
      (new-action))
    (define (dispatch m)
      (cond ((eq? m 'add-action!) accept-procs!)
            ;; ...
            ))
    dispatch))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define a (make-wire))
(define b (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(define (probe name wire)
  (add-action! 
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire)))))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; Sample

(probe 'sum sum)
;; sum 0  New-value = 0
(probe 'carry carry)
;; carry 0  New-value = 0
(half-adder input-1 input-2 sum carry)
;; ok
(set-signal! input-1 1)
;; ok
(propagate)
;; sum 8  New-value = 1
;; ok

;; Answer

;(probe 'sum sum)
;(add-action! sum (lambda () ...))
;((sum 'add-action) (lambda () ...))
;(accept-procs! (lambda () ...))
;(set! actions (cons (lambda () ...) '()))
;((lambda () ...))
;> sum 0 New-value = 0

;(probe 'carry carry) 同上