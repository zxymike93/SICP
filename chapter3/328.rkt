#lang sicp

(define (or-gate o1 o2 output)

  (define (or-proc)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))

  (add-action! o1 or-proc)
  (add-action! o2 or-proc)
  'ok)