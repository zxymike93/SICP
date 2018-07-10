;; p = (cons x y)
;; x = (car p)
;; y = (cdr p)


(define (cons x y)
  (lambda (m) (m x y)))


(define (car z)
  (z (lambda (x y) x)))

;; (car (cons x y))
;; Evaluate argument:
;; 1.
;; (car
;;   (lambda (m) (m x y)))
;; 2.
;; ((z (lambda (p q) p))
;; 3.
;; (((lambda (m) (m x y)
;;        (lambda (p q) p)
;;   )))
;; 4.
;; ((
;;   ((lambda (p q) p)
;;    x y)
;; ))

(define (cdr z)
  (z (lambda (x y) y)))
