#lang sicp

;; 问题a，只需要将 predicate / constructor / selector 对应地改为 list 中间即可。

;; helper
(define (memq s t)
  (cond [(null? t) #f]
        [(eq? s (car t)) t]
        [else (memq s (cdr t))]))

(define (memp s t)
  (define (iter s t p)
    (cond [(null? t) #f]
          [(eq? s (car t)) p]
          [else (iter s (cdr t) (append p (list (car t))))]))
  (iter s t nil))

(define (operator! expr)
  (if (memq '+ expr)
      '+
      '*))

(define (operand! expr)
    (if (= (length expr) 1)
        (car expr)
        expr))

;; predicate
(define (variable? x)
  (symbol? x))

(define (same-variable? x y)
  (and (variable? x)
       (variable? y)
       (eq? x y)))

(define (number=? x num)
  (and (number? x)
       (= x num)))

(define (sum? x)
  (and (pair? x)
       (eq? '+ (operator! x))))

(define (product? x)
  (and (pair? x)
       (eq? '* (operator! x))))

;; constructor
(define (make-sum x y)
  (cond [(number=? x 0) y]
        [(number=? y 0) x]
        [(and (number? x) (number? y)) (+ x y)]
        [else (list x '+ y)]))

(define (make-product x y)
  (cond [(or (number=? x 0) (number=? y 0)) 0]
        [(number=? x 1) y]
        [(number=? y 1) x]
        [(and (number? x) (number? y)) (* x y)]
        [else (list x '* y)]))

;; selector
(define (addend s)
  (operand! (memp '+ s)))

(define (augend s)
  (operand! (cdr (memq '+ s))))

(define (multiplier p)
  (operand! (memp '* p)))

(define (multiplicand p)
  (operand! (cdr (memq '* p))))

;; application
(define (deriv expr var)
  (cond [(number? expr) 0]
        [(variable? expr) (if (same-variable? expr var)
                              1
                              0)]
        [(sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var))]
        [(product? expr) (make-sum (make-product (multiplier expr)
                                                 (deriv (multiplicand expr) var))
                                   (make-product (multiplicand expr)
                                                 (deriv (multiplier expr) var)))]
        [else (error "Unknown expression type: DERIV" expr)]))

;; tests
(deriv '(x + 3) 'x)
(deriv '(x * y) 'x)
(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)