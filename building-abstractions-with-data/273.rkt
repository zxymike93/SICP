#lang racket

;; 由于 #lang sicp 不提供 put/get
;; 改用 racket-lang 的内置函数
(define table (make-hash))

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type) #f))

;; generic
(define (type obj)
  (if (pair? obj)
      (car obj)
      (error "Bad Tagged Object." obj)))

(define (contents obj)
  (if (pair? obj)
      (cdr obj)
      (error "Bad Tagged Object." obj)))

(define (operate op obj)
  (let ([proc (get op (type obj))])
    (if (not (null? proc))
        (proc (contents obj))
        (error "Bad Tagged Object." obj))))

;; auxiliary
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (number=? expr num)
  (and (number? expr)
       (= expr num)))

(define (make-sum x y)
  (cond [(number=? x 0) y]
        [(number=? y 0) x]
        [(and (number? x) (number? y))
         (+ x y)]
        [else
         (list '+ x y)]))

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

;; deriv
(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; A1、因为 number? variable? 不是对多项式求导，单独判断
(define (deriv expr var)
   (cond [(number? expr)
          0]
         [(variable? expr)
          (if (same-variable? expr var)
               1
               0)]
         [else ((get 'deriv (operator expr))
                (operands expr)
                var)]))

;; A2、将 sum / mul 当作单独的 type 来实现
(define (install-+)
  (define addend car)
  (define augend cadr)
  ;; put into `table`
  (put 'deriv
       '+
       (lambda [operands var]
         (make-sum (deriv (addend operands) var)
                   (deriv (augend operands) var)))))
(install-+)

(define (install-*)
  (define multiplier car)
  (define multiplicand cadr)
  ;; put into `table`
  (put 'deriv
       '*
       (lambda [operands var]
         (make-sum (make-product (multiplier operands)
                                 (deriv (multiplicand operands) var))
                   (make-product (multiplicand operands)
                                 (deriv (multiplier operands) var))))))
(install-*)

;; A3、additional deriv rule
(define (install-exp)
  (define (base x) (car x))
  (define (exponent x) (cadr x))
  (define (make-exponentiation base exp)
    (cond [(number=? exp 0) 1]
          [(number=? exp 1) base]
          [(or (number=? base 1) (number=? base 0)) base]
          [else (list '** base exp)]))
  (put 'deriv
       '**
       (lambda [operands var]
         (make-product
          (make-product (exponent operands)
                        (make-exponentiation (base operands)
                                             (make-sum (exponent operands) -1)))
          (deriv (base operands) var)))))
(install-exp)

;; A4、如果将 `deriv` 里面的 dispatch 改为
;; ((get (operator exp) 'deriv) 
;;  (operands exp) var)
;; 只需将 put / get 的两个参数调换顺序，所有的 package 都无需改动。

;; tests
(deriv '(* x x) 'x)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 2) 'x)
(deriv '(+ (** x 2) (* 2 (** x 3))) 'x)