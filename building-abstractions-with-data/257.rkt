#lang sicp

;; 使得 deriv 中求 加/乘 法可以接收任意多个参数
;; 题目给出了 hint：
;; （上面 deriv 里面接收的第一个参数（即 '(+ ...) 中 '+ 后面的））第一个参数作为 addend
;; 第其余参数的和作为 augend

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (number=? expr num)
  (and (number? expr)
       (= expr num)))

(define (variable? x)
  (symbol? x))

(define (same-variable? x y)
  (and (variable? x)
       (variable? y)
       (eq? x y)))

(define (make-sum x y)
  (cond [(number=? x 0) y]
        [(number=? y 0) x]
        [(and (number? x) (number? y))
         (+ x y)]
        [else
         (list '+ x y)]))

(define (addend s)
  (cadr s))

(define (augend s)
  (accumulate make-sum 0 (cddr s)))

(define (sum? x)
  (and (pair? x)
       (eq? (car x)
            '+)))

(define (make-product x y)
  (cond [(or (number=? x 0)
             (number=? y 0))
         0]
        [(number=? x 1)
         y]
        [(number=? y 1)
         x]
        [(and (number? x) (number? y))
         (* x y)]
        [else
         (list '* x y)]))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (accumulate make-product 1 (cddr p)))

(define (product? x)
  (and (pair? x)
       (eq? (car x)
            '*)))

(define (make-exponentiation base exp)
  (cond [(number=? exp 0)
         1]
        [(number=? exp 1)
         base]
        [(or (number=? base 1) (number=? base 0))
         base]
        [else
         (list '** base exp)]))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x)
            '**)))

(define (deriv expr var)
        ;; dc/dx = 0
  (cond [(number? expr)
         0]
        ;; dx/dx = 1
        [(variable? expr)
         (if (same-variable? expr var)
             1
             0)]
        ;; d(u+v)/dx = du/dx + dv/dx
        [(sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var))]
        ;; d(uv)/dx = u(du/dx) + v(dv/dx)
        [(product? expr)
         (make-sum (make-product (multiplier expr)
                                 (deriv (multiplicand expr) var))
                   (make-product (multiplicand expr)
                                 (deriv (multiplier expr) var)))]
        ;; d(u**n)/dx = n * u**n-1 * du/dx
        [(exponentiation? expr)
         (make-product
          (make-product (exponent expr)
                        (make-exponentiation (base expr)
                                             (make-sum (exponent expr) -1)))
          (deriv (base expr) var))]
        [else
         (error "Unknown expression type: DERIV" expr)]))

;; tests
(deriv '(* x x) 'x)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 2) 'x)
(deriv '(+ (** x 2) (* 2 (** x 3))) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(* x x x) 'x)