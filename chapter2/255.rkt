#lang sicp

(car ''abracadabra)
;; (car (quote (quote abracadabra)))
;; 这里被 quote 起来的对象是 (quote abracadabra)
;; 因此 car 的结果是 quote，cdr 的结果是 abracadabra
