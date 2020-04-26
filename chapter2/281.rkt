#lang racket

(define table (make-hash))

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type) #f))

(define coercion-table (make-hash))

(define (put-coercion op type item)
  (hash-set! coercion-table (list op type) item))

(define (get-coercion op type)
  (hash-ref coercion-table (list op type) #f))

(define (make-int n)
  ((get 'make 'int) n))

(define (make-com-from-real-imag x y)
  ((get 'make-from-real-imag 'com) x y))

(define (int->com n)
  (make-com-from-real-imag (content n) 0))
(put-coercion 'int 'com int->com)

(define (apply-generic op . args)
  (let ((tags (map tag args)))
    (let ((proc (get op tags)))
      (if proc
          (apply proc (map content args))
          (if (= (length args) 2)
              (let ((type1 (car tags)) (arg1 (car args))
                    (type2 (cadr tags)) (arg2 (cadr args)))
                ;; [2]
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 arg1) arg2))
                            (t2->t1
                             (apply-generic op arg1 (t2->t1 arg2)))
                            (else
                             (error "APPLY-GENERIC: No coercion for these types." args))))
                    (error "APPLY-GENERIC: No op for this type." (list op args))))
                (error "APPLY-GENERIC: " (list op args)))))))

(define (add a b)
  (apply-generic 'add a b))

;; [1]
(define (exp b n)
  (apply-generic 'exp b n))

(define (attach-tag tag content)
  (cons tag content))

(define (tag datum)
  (if (pair? datum)
      (car datum)
      (error "TAG: " datum)))

(define (content datum)
  (if (pair? datum)
      (cdr datum)
      (error "CONTENT: " datum)))

(define (install-int-package)
  (define (make-int n)
    (attach-tag 'int n))
  (define (add a b)
    (make-int (+ a b)))
  ;; [1]
  (define (exp b n)
    (make-int (expt b n)))
  (put 'make 'int make-int)
  (put 'add '(int int) add)
  (put 'exp '(int int) exp))

(define (install-com-package)
  (install-rect-package)
  (define (make-from-real-imag x y)
    (attach-tag 'com ((get 'make-from-real-imag 'rect) x y)))
  (define (real z)
    (apply-generic 'real z))
  (define (imag z)
    (apply-generic 'imag z))
  (define (add z y)
    (make-from-real-imag (+ (real z) (real y))
                         (+ (imag z) (imag y))))
  (put 'make-from-real-imag 'com make-from-real-imag)
  (put 'add '(com com) add))

(define (install-rect-package)
  (define (make-from-real-imag x y)
    (attach-tag 'rect (cons x y)))
  (define (real z)
    (car z))
  (define (imag z)
    (cdr z))
  (put 'make-from-real-imag 'rect make-from-real-imag)
  (put 'real '(rect) real)
  (put 'imag '(rect) imag))

(install-int-package)
(install-com-package)

;; 即便两个同类型的数 apply-generic 也可能 coercion
;; 比如，某个类型中没有相应的 op，`if proc` 返回 false 之后，程序就会执行到 coercion 部分

;; a. Louise 认为，既然存在某种情况下同类型也会 coercion，不如给每个类型定义自身到自身的强制转换
;(define (int->int n) n)
;(define (com->com z) z)
;(put-coercion 'int 'int int->int)
;(put-coercion 'com 'com com->com)
;; 如果作此修改，假设有通用操作 exp[1]，com 类型不支持。那么执行下面的语句会出现什么问题？
(define a (make-com-from-real-imag 1 1))
(exp a a)
;; 执行该语句会无限循环。正如上面背景所说「程序会执行到 coercion 部分」
;(apply-generic 'exp (com->com a) a)
;(apply-generic 'exp a a)
;; 上面两个语句是等价的，程序会在两个语句间不断估值、转换。

;; b. 同上

;; c. 修改 apply-generic 使两个同类型的参数不会进行 coercion[2]