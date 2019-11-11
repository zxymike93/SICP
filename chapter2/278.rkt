#lang racket

;; operation-type table
(define table (make-hash))
(define (put op type value) (hash-set! table (list op type) value))
(define (get op type) (hash-ref table (list op type)))


;; type tag
(define (attach-tag type value)
  (cond [(number? value) value]
        [else (cons type value)]))

(define (type obj)
  (cond [(number? obj) 'org]
        [(pair? obj) (car obj)]
        [else (error "No tag for this data:" obj)]))

(define (value obj)
  (cond [(number? obj) obj]
        [(pair? obj) (cdr obj)]
        [else (error "No value for this data:" obj)]))


;; generic dispatch
(define (apply-generic op . args)
  (let ([types (map type args)])
    (let ([proc (get op types)])
      (if proc
          (apply proc (map value args))
          (error "No method for these types:" (list op types))))))


;; number's api
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;; original number
(define (install-original-package)
  
  (define (tag x) (attach-tag 'org x))
  ;; original number's api table
  (put 'make 'org
       (lambda [x] (tag x)))
  ;; number's api
  (put 'add '(org org)
       (lambda [x y] (+ x y)))
  (put 'sub '(org org)
       (lambda [x y] (- x y)))
  (put 'mul '(org org)
       (lambda [x y] (* x y)))
  (put 'div '(org org)
       (lambda [x y] (/ x y)))
  
  '(ORG-NUM INSTALLED))


;; tests
(install-original-package)
(add 1 2)