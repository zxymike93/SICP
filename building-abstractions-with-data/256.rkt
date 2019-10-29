#lang sicp

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
  (cond [(number=? x 0)
         y]
        [(number=? y 0)
         x]
        [(and (number? x) (number? y))
         (+ x y)]
        [else
         (list '+ x y)]))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

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
  (caddr p))

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
  (cond [(number? expr)
         0]
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