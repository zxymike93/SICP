#lang sicp


(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal " s))))


(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal " s1 s2))))


(define (inverter input output)

  (define (invert-proc)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))

  (add-action! input invert-proc)
  'ok)


(define (and-gate a1 a2 output)

  (define (and-proc)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))

  (add-action! a1 and-proc)
  (add-action! a2 and-proc)
  'ok)


;; 因为或门是 0 0 -> 0, 0 1 ->1, 1 0 -> 1, 1 1 -> 1
;; 而与门是 1 1 -> 1, 1 0 -> 0, 0 1 -> 0, 0 0 -> 0
;; 可以将输入 o1, o2 过非门、过与门，再将输出过非门，实现与门
;; 因此，or-delay 的时间是 3 * inverter-delay + 1 * and-gate-delay
(define (or-gate o1 o2 output)

  (define (invert in)
    (let ((out (make-wire)))
      (inverter in out)
      out))

  (define (or-proc)
    (let ((new-value (invert (logical-and (get-signal (invert o1))
                                          (get-signal (invert o2))))))
      (lambda () (set-signal! output new-value))))

  (add-action! o1 or-proc)
  (add-action! o2 or-proc)
  'ok)