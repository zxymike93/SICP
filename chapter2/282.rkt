#lang racket

;; single/same type operation
(define op-table (make-hash))

(define (get-op op types)
  (hash-ref op-table (list op types) #false))

(define (put-op op types values)
  (hash-set! op-table (list op types) values))

;; coercion
(define co-table (make-hash))

(define (get-co type1 type2)
  (hash-ref co-table (list type1 type2) #false))

(define (put-co type1 type2 args)
  (hash-set! co-table (list type1 type2) args))

;; tag
(define (attach-tag tag content)
  (cons tag content))

(define (get-tag datum)
  (if (pair? datum)
      (car datum)
      (error "get-tag: " datum)))

(define (get-content datum)
  (if (pair? datum)
      (cdr datum)
      (error "get-content: " datum)))

;; apply-generic: op arg1 arg2
;; 尝试强制所有类型到第一个运算对象
(define (coerce-to args)
  (map (lambda (x)
         (let ([tn->t1 (get-co (get-tag x) (get-tag (car args)))])
           (if tn->t1
               (tn->t1 x)
               x)))
       args))

;; 尝试强制所有类型到第一个运算对象，否则第二个、第三个...
(define (apply-coercion op args)
  (cond [(null? args)
         (error "apply-generic: can't coerce types ")]
        [else
         (let ([coerced (coerce-to args)])
           (let ([tags (map get-tag coerced)])
             (let ([proc (get-op op tags)])
               (cond [proc (apply proc (map get-content coerced))]
                     [else (apply-coercion
                            op
                            (list (car args)
                                  (apply-coercion op (cdr args))))]))))]))

;; 通用的操作函数
(define (apply-generic op . args)
  (let ([tags (map get-tag args)])
    (let ([proc (get-op op tags)])
      (cond [proc
             (apply proc (map get-content args))]
            [else
             (apply-coercion op args)]))))

;; api
(define (add x y)
  (apply-generic 'add x y))

(define (int->rat n)
  (make-rat (get-content n) 1))
(put-co 'int 'rat int->rat)

;; int
(define (make-int n)
  ((get-op 'make 'int) n))

(define (install-int)
  
  (define (make-int n)
    (attach-tag 'int n))
  
  (define (add-int n m)
    (make-int (+ n m)))
  
  (put-op 'make 'int make-int)
  (put-op 'add '(int int) add-int)
  'int)

;; rat
(define (make-rat n d)
  ((get-op 'make 'rat) n d))

(define (install-rat)

  (define (make-rat n d)
    (let ([g (gcd n d)])
      (attach-tag 'rat (cons (/ n g) (/ d g)))))

  (define (numer r) (car r))
  (define (denom r) (cdr r))

  (define (add-rat r1 r2)
    (make-rat (+ (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
              (* (denom r1) (denom r2))))

  (put-op 'make 'rat make-rat)
  (put-op 'add '(rat rat) add-rat)
  'rat)

;; test op-table
(put-op 'add '(int int) +)
(get-op 'add '(int int))
(get-op 'sub '(int int))
(get-op 'add 'int)

;; test co-table
(get-co 'int 'rat)
(get-co 'int 'int)

;; test tag
(define datum (attach-tag 'int 1))
(get-tag datum)
(get-content datum)

;; test make
(install-int)
(make-int 7)
(install-rat)
(make-rat 3 4)

(define n (make-int 15))
(define m (make-int 22))
(add n m)

(define rn (make-rat 3 11))
(define rm (make-rat 5 11))
(add rn rm)

;; test coercion
(int->rat n)
(coerce-to (list rn n m))
(apply-coercion 'add (list rm rn n m m))
;(apply-coercion 'add (list n m rn m))
;(apply-coercion 'add (list n m rn))
(apply-generic 'add n m)
(apply-generic 'add rn n)
(apply-generic 'add n rn)
;; 使用这个策略的问题是，参数类型必须从左到右由“复杂”到“简单”