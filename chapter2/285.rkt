#lang racket
;; drop：简化一个数据，使它在 level-tower 中的层次尽量降到最低。
;; 实现 drop 要用到的策略：先用 project 简化数据，再 raise 到原来的数据类型，
;; 如果结果和原来的数据一致。需要给每个数据类型写 project，以及用到 79 题的 equ?
;; 最后，使用 drop 改写 apply-generic

(define (square x) (* x x))

(define (attach-tag tag content)
  (cons tag content))

(define (get-tag datum)
  (if (pair? datum)
      (car datum)
      (error "get-tag: cannot get tag from" datum)))

(define (get-content datum)
  (if (pair? datum)
      (cdr datum)
      (error "get-content: cannot get content from" datum)))

;; Operations on a single type
(define *table* (make-hash))

(define (put op types items)
  (hash-set! *table* (list op types) items))

(define (get op types)
  (hash-ref *table* (list op types) #f))

;; 类型塔
(define (level datum)
  (let ([t (get-tag datum)])
    (cond [(eq? t 'int) 0]
          [(eq? t 'rat) 1]
          [(eq? t 'com) 2]
          [else (error "level: cannot find level of " datum)])))

;; Generic
(define (apply-generic op . args)
  (let ([tags (map get-tag args)])
    (let ([proc (get op tags)])
      (cond [proc
             (apply proc (map get-content args))]
            [else
             (error "apply-generic: no procedure for " (list op args))]))))

;; Packages' API
(define (make-int x) ((get 'make 'int) x))

(define (make-rat x y) ((get 'make 'rat) x y))

(define (make-com-from-real-imag x y) ((get 'make-from-real-imag 'com) x y))

(define (make-com-from-mag-ang x y) ((get 'make-from-mag-ang 'com) x y))

; (add (1 2 3))
(define (add first . rest)
  (let ([n (length rest)])
    (cond [(< n 1)
           (error "add: expected more than 1 argument, given " (list first rest))]
          [(= n 1)
           (apply-generic 'add first (car rest))]
          [else
           (add (apply-generic 'add first (car rest)) .
                (cdr rest))])))

;(define (project t) (apply-generic 'project t))


;; 整数
(define (install-integer-package)
  (define (make-int x) x)
  (define +n +)

  (put 'make 'int (lambda (x) (attach-tag 'int (make-int x))))
  (put 'add '(int int) (lambda (x y) (attach-tag 'int (+n x y))))
  "Integer package installed.")

;; 有理数
(define (install-rational-package)
  (define make-rat (lambda (n d) (attach-tag 'rat (cons n d))))
  (define numer car)
  (define denom cdr)

  (define (+r x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))

;  (define (project x)
;    (if (= 1 (denom x))
;        ((get 'make 'int) (numer x))
;        x))

  (put 'make 'rat make-rat)
  (put 'add '(rat rat) +r)
;  (put 'project '(rat) project))
    "Rational package installed.")

;; 复数
(define (install-complex-package)
  (install-rect-package)
  (install-polar-package)

  (define (make-from-real-imag x y)
    (attach-tag 'com ((get 'make-from-real-imag 'rect) x y)))
  (define (make-from-mag-ang r a)
    (attach-tag 'com ((get 'make-from-mag-ang 'polar) r a)))
  (define (real z) (apply-generic 'real z))
  (define (imag z) (apply-generic 'imag z))
  (define (ang z) (apply-generic 'ang z))
  (define (mag z) (apply-generic 'mag z))

  (define (+c z1 z2)
    (make-from-real-imag
     (+ (real z1) (real z2))
     (+ (imag z1) (imag z2))))

  (put 'make-from-real-imag 'com make-from-real-imag)
  (put 'make-from-mag-ang 'com make-from-mag-ang)
  (put 'add '(com com) +c)
  "Complex package installed.")

;; 复数的构造和选择函数-实部和虚部
(define (install-rect-package)

  (define (make-from-real-imag x y)
    (attach-tag 'rect (cons x y)))

  (define (make-from-mag-ang r a)
    (make-from-real-imag (* r (cos a)) (* r (sin a))))

  (define (real z) (car z))

  (define (imag z) (cdr z))

  (define (mag z)
    (sqrt (+ (square (real z))
             (square (imag z)))))

  (define (ang z)
    (atan (imag z)
          (real z)))

  (put 'make-from-mag-ang 'rect make-from-mag-ang)
  (put 'make-from-real-imag 'rect make-from-real-imag)
  (put 'real '(rect) real)
  (put 'imag '(rect) imag)
  (put 'mag '(rect) mag)
  (put 'ang '(rect) ang)
  "Rect-complex package installed.")

;; 复数的构造和选择函数-模和幅角
(define (install-polar-package)

  (define (make-from-mag-ang r a)
    (attach-tag 'polar (cons r a)))

  (define (make-from-real-imag x y)
    (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                             (atan y x))))

  (define (mag z) (car z))

  (define (ang z) (cdr z))

  (define (real z)
    (* (mag z)
       (cos (ang z))))

  (define (imag z)
    (* (mag z)
       (cos (ang z))))

  (put 'make-from-mag-ang 'polar make-from-mag-ang)
  (put 'make-from-real-imag 'polar make-from-real-imag)
  (put 'real 'polar real)
  (put 'imag 'polar imag)
  (put 'mag 'polar mag)
  (put 'ang 'polar ang)
  "Polar-complex package installed.")


;; tests
(install-complex-package)
(install-integer-package)
(install-rational-package)
(define a (make-com-from-real-imag 1 2))
(define b (make-com-from-mag-ang 1 (/ pi 4)))
(define c (make-int 1))
(define d (make-rat 2 3))

;(add c c)
(add c c c)