#lang sicp


(define (amb? exp)
  (tagged-list? exp 'amb))

(define (amb-choices exp)
  (cdr exp))

(define (amb-eval exp env succeed fail)
  ((analyze exp) env succeed fail))