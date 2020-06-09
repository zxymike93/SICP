#lang racket

;; 为每个类型增加 raise 操作参考 [2][3]，实现通用的 raise 参考 [1]
;; 因为没有实现 real-package 这里忽略了该类型

(define square (lambda (x) (* x x)))

(define (attach-tag tag content)
  (cons tag content))

(define tag
  (lambda (x)
    (if (pair? x)
        (car x)
        (error "TAG: " x))))

(define content
  (lambda (x)
    (if (pair? x)
        (cdr x)
        (error "CONTENT: " x))))


;; Operations on a single type
(define *op-table* (make-hash))

(define (put op type item)
  (hash-set! *op-table* (list op type) item))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (apply-generic op . args)
  (let ((tags (map tag args)))
    (let ((proc (get op tags)));;(car tags))))
      (if proc
          (apply proc (map content args))
          (error "APPLY-GENERIC: " (list op args))))))


;; 通用操作
;;;;;;;;;;
(define (make-int x) ((get 'make 'int) x))
(define (make-rat x y) ((get 'make 'rat) x y))
(define (make-com-from-real-imag x y) ((get 'make-from-real-imag 'com) x y))
(define (make-com-from-mag-ang x y) ((get 'make-from-mag-ang 'com) x y))
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))

(define (raise t) (apply-generic 'raise t))  ;; [1]


;; 整数
;;;;;;
(define (install-integer-package)
  (define make-int (lambda (x) (attach-tag 'int x)))
  (define +n +)
  (define *n *)
  (define raise-int
    (lambda (x) (make-rat x 1)))

  (put 'make 'int make-int)
  (put 'add '(int int) +n)
  (put 'mul '(int int) *n)
  (put 'raise '(int) raise-int)  ;; [2]
  'int)


;; 有理数
;;;;;;;;
(define (install-rational-package)
  (define make-rat (lambda (n d) (attach-tag 'rat (cons n d))))
  (define numer car)
  (define denom cdr)

  (define (+r x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (*r x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (raise-rat x)
    (make-com-from-real-imag
     (/ (numer x) (denom x))
     0))

  (put 'make 'rat make-rat)
  (put 'add '(rat rat) +r)
  (put 'mul '(rat rat) *r)
  (put 'raise '(rat) raise-rat)  ;; [3]
  'rat)


;; 复数
;;;;;;
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
  ;; 复数的操作接口-四则运算
  (define (+c z1 z2)
    (make-from-real-imag
     (+ (real z1) (real z2))
     (+ (imag z1) (imag z2))))

  (define (*c z1 z2)
    (make-from-mag-ang
     (* (mag z1) (mag z2))
     (+ (ang z1) (ang z2))))

  (put 'make-from-real-imag 'com make-from-real-imag)
  (put 'make-from-mag-ang 'com make-from-mag-ang)
  (put 'add '(com com) +c)
  (put 'mul '(com com) *c)
  'com)


;; 复数的构造和选择函数-实部和虚部
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-rect-package)
  ;; internal procedures
  (define (make-from-real-imag x y)
    (attach-tag 'rect (cons x y)))
  ;; input: 模 幅角
  ;; output: 实部和虚部形式的复数
  (define (make-from-mag-ang r a)
    (make-from-real-imag (* r (cos a)) (* r (sin a))))

  (define (real z) (car z))

  (define (imag z) (cdr z))

  (define (mag z)
    (sqrt (+
           (square (real z))
           (square (imag z)))))

  (define (ang z)
    (atan
     (imag z)
     (real z)))
  ;; apis
  (put 'make-from-mag-ang 'rect make-from-mag-ang)
  (put 'make-from-real-imag 'rect make-from-real-imag)
  (put 'real 'rect real)
  (put 'imag 'rect imag)
  (put 'mag 'rect mag)
  (put 'ang 'rect ang)
  'rect)


;; 复数的构造和选择函数-模和幅角
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-polar-package)
  ;; internal procedures
  (define (make-from-mag-ang r a)
    (attach-tag 'polar (cons r a)))

  (define (make-from-real-imag x y)
    (attach-tag 'polar
                (cons
                 (sqrt (+ (square x) (square y)))
                 (atan y x))))

  (define (mag z) (car z))

  (define (ang z) (cdr z))

  (define (real z)
    (*
     (mag z)
     (cos (ang z))))

  (define (imag z)
    (*
     (mag z)
     (cos (ang z))))
  ;; external apis
  (put 'make-from-mag-ang 'polar make-from-mag-ang)
  (put 'make-from-real-imag 'polar make-from-real-imag)
  (put 'real 'polar real)
  (put 'imag 'polar imag)
  (put 'mag 'polar mag)
  (put 'ang 'polar ang)
  'polar)


;; tests
(install-complex-package)
(define a (make-com-from-real-imag 1 2))
(define b (make-com-from-mag-ang 1 (/ pi 4)))

(install-integer-package)
(install-rational-package)
(define c (make-int 1))
(define d (make-rat 2 3))

(add c c)

(raise (raise c))