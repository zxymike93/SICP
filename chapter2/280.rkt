#lang racket

;; auxiliary
(define (square x) (* x x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; operation-type table
(define table (make-hash))
(define (put op type value) (hash-set! table (list op type) value))
(define (get op type) (hash-ref table (list op type)))


;; type tag
(define (attach-tag type value) (cons type value))

(define (type obj)
  (if (pair? obj)
      (car obj)
      (error "No tag for this data:" obj)))

(define (value obj)
  (if (pair? obj)
      (cdr obj)
      (error "No tag for this data:" obj)))


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

(define (equ? x y)
  (if (eq? (type x) (type y))
      (apply-generic 'equ? x y)
      (error "Comparing different types" (cons (type x) (type y)))))

(define (=zero? x) (apply-generic '=zero? x))


;; original number's api
(define (make-org x) ((get 'make 'org) x))

;; original number
(define (install-original-package)
  
  (define (tag x) (attach-tag 'org x))
  ;; original number's api table
  (put 'make 'org
       (lambda [x] (tag x)))
  ;; number's api
  (put 'add '(org org)
       (lambda [x y] (tag (+ x y))))
  (put 'sub '(org org)
       (lambda [x y] (tag (- x y))))
  (put 'mul '(org org)
       (lambda [x y] (tag (* x y))))
  (put 'div '(org org)
       (lambda [x y] (tag (/ x y))))
  (put 'equ? '(org org)
       (lambda [x y] (= x y)))
  (put '=zero? '(org)
       (lambda [x] (= x 0)))
  
  '(ORG-NUM INSTALLED))


;; rational number's api
(define (make-rat x y) ((get 'make 'rat) x y))

;; rational number
(define (install-rational-package)
  
  (define (tag x) (attach-tag 'rat x))
  ;; constructors / selectors
  (define (rat x y) (let ([g (gcd x y)])
                      (cons (/ x g) (/ y g))))
  (define (numer z) (car z))
  (define (denom z) (cdr z))
  ;; rational number's api table
  (put 'make 'rat
       (lambda [x y] (tag (rat x y))))
  ;; number's api table
  (put 'add '(rat rat)
       (lambda [x y] (rat (+ (* (numer x) (denom y))
                             (* (numer y) (denom x)))
                          (* (denom x) (denom y)))))
  (put 'sub '(rat rat)
       (lambda [x y] (rat (- (* (numer x) (denom y))
                             (* (numer y) (denom x)))
                          (* (denom x) (denom y)))))
  (put 'mul '(rat rat)
       (lambda [x y] (rat (* (numer x) (numer y))
                          (* (denom x) (denom y)))))
  (put 'div '(rat rat)
       (lambda [x y] (rat (* (numer x) (denom y))
                          (* (denom x) (numer y)))))
  (put 'equ? '(rat rat)
       (lambda [x y] (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put '=zero? '(rat)
       (lambda [x] (= (numer x) 0)))

  '(RAT-NUM INSTALLED))


;; complex number's api
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'cpx) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'cpx) r a))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; complex number
(define (install-complex-package)
  
  (define (tag x) (attach-tag 'cpx x))
  ;; constructors / selectors
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
  (define (equ? x y) (apply-generic 'equ? x y))
  (define (=zero? x) (apply-generic '=zero? x))
  ;; complex number's api table
  (put 'make-from-real-imag 'cpx
       (lambda [x y] (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'cpx
       (lambda [r a] (tag (make-from-mag-ang r a))))
  (put 'real-part '(cpx) real-part)
  (put 'imag-part '(cpx) imag-part)
  (put 'magnitude '(cpx) magnitude)
  (put 'angle '(cpx) angle)
  ;; number's api table
  (put 'add '(cpx cpx)
       (lambda [x y] (tag (make-from-real-imag
                           (+ (real-part x) (real-part y))
                           (+ (imag-part x) (imag-part y))))))
  (put 'sub '(cpx cpx)
       (lambda [x y] (tag (make-from-real-imag
                           (- (real-part x) (real-part y))
                           (- (imag-part x) (imag-part y))))))
  (put 'mul '(cpx cpx)
       (lambda [x y] (tag (make-from-mag-ang
                           (* (magnitude x) (magnitude y))
                           (+ (angle x) (angle y))))))
  (put 'div '(cpx cpx)
       (lambda [x y] (tag (make-from-mag-ang 
                           (/ (magnitude x) (magnitude y))
                           (- (angle x) (angle y))))))
  (put 'equ? '(cpx cpx) equ?)
  (put '=zero? '(cpx) =zero?)

  '(CPX-NUM INSTALLED))


;; complex number - rectangular form
(define (install-rectangular-package)

  (define (tag x) (attach-tag 'rectangular x))
  ;; constructors / selectors
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (sqrt (+ (square (real-part z))
                                 (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  ;; rectangular form's api talbe
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda [x y] (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda [r a] (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular)
       (lambda [x y] (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put '=zero? '(rectangular)
       (lambda [x] (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  
  '(RECT-FORM INSTALLED))

;; complex number - polar form
(define (install-polar-package)
  
  (define (tag x) (attach-tag 'polar x))
  ;; constructors / selectors
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons
                                     (sqrt (+ (square x) (square y)))
                                     (atan y x)))
  ;; polar form's api table
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda [x y] (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda [r a] (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar)
       (lambda [x y] (and (= (magnitude x) (magnitude y))
                          (= (magnitude x) (magnitude y)))))
  (put '=zero? '(polar)
       (lambda [x] (and (= (magnitude x) 0)
                        (= (angle x) 0))))
  
  '(POLAR-FORM INSTALLED))

;; tests
(install-original-package)
(add (make-org 10) (make-org 22))
(sub (make-org 22) (make-org 10))
(mul (make-org 3) (make-org 4))
(div (make-org 2) (make-org 5))
(equ? (make-org 2) (make-org 3))
(=zero? (make-org 1))

(install-rational-package)
(add (make-rat 1 2) (make-rat 3 4))
(sub (make-rat 1 2) (make-rat 3 4))
(mul (make-rat 1 2) (make-rat 3 4))
(div (make-rat 1 2) (make-rat 3 4))
(equ? (make-rat 7 11) (make-rat 7 11))
(=zero? (make-rat 0 2))

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(add (make-from-real-imag 1 2) (make-from-real-imag 3 4))
(mul (make-from-mag-ang 11 2) (make-from-real-imag 2 2))
(equ? (make-from-mag-ang 1 45) (make-from-mag-ang 1 45))
(=zero? (make-from-real-imag 0 0))
(=zero? (make-from-mag-ang 0 0))