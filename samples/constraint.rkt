#lang racket

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond [(null? items)
           'done]
          [(eq? (car items) exception)
           (loop (cdr items))]
          [else
           (procedure (car items))
           (loop (cdr items))]))
  (loop list))


;; (has-value? c) -> bool
(define (has-value? c)
  (c 'has-value?))

;; (get-value c) -> num
(define (get-value c)
  (c 'get-value))

;; (set-value! c value source) 
;; set new value from source
(define (set-value! c val src)
  ((c 'set-value!) val src))

;; (forget-value! c source)
;; reset value to none from source
(define (forget-value! c src)
  ((c 'forget-value!) src))

;; (connect c constraint)
(define (connect c cst)
  ((c 'connect) cst))


;; connector
(define (make-connector)
  (let ([val #f]
        [src #f]
        [cst '()])
    ;; 利用是否有消息源判断是否设置新的值，如果已经设置了就不能重设（除非 forget）
    (define (set-value! new-val setter)
      (cond [(not (has-value? me))
             (set! val new-val)
             (set! src setter)
             (for-each-except setter
                              inform-about-value
                              cst)]
            [(not (= val new-val))
             (begin
               (display "Contradiction: Please reset first. ")
               (list val new-val))]
            [else
             'ignore]))
    ;; 同一个消息源才会 forget
    (define (forget-value! refactor)
      (cond [(eq? refactor src)
             (begin (set! src #f)
                    (for-each-except refactor
                                     inform-no-value
                                     cst))]
            [else
             'ignore]))

    (define (connect new-cst)
      (cond [(not (memq new-cst cst))
             (set! cst (cons new-cst cst))])
      (cond [(has-value? me)
             (inform-about-value new-cst)])
      'done)

    (define (me request)
      (cond [(eq? request 'has-value?)
             (if src #t #f)]
            [(eq? request 'get-value)
             val]
            [(eq? request 'set-value!)
             set-value!]
            [(eq? request 'forget-value!)
             forget-value!]
            [(eq? request 'connect)
             connect]
            [else
             (error "connector: Unknown request" request)]))
    me))


;; request apis of constraints
(define (inform-about-value cst)
  (cst 'have-value))

(define (inform-no-value cst)
  (cst 'lost-value))

;; constraint: adder, multiplier, constant

(define (adder a1 a2 sum)
  
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       add-me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       add-me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       add-me)]))

  (define (process-forget-value)
    (forget-value! sum add-me)
    (forget-value! a1 add-me)
    (forget-value! a2 add-me)
    (process-new-value))

  (define (add-me request)
    (cond [(eq? request 'have-value)
           (process-new-value)]
          [(eq? request 'lost-value)
           (process-forget-value)]
          [else
           (error "adder: Unknown request" request)]))

  (connect a1 add-me)
  (connect a2 add-me)
  (connect sum add-me)
  add-me)


(define (multiplier m1 m2 product)

  (define (process-new-value)
    (cond [(and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       mul-me)]
          [(and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       mul-me)]
          [(and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       mul-me)]))

  (define (process-forget-value)
    (forget-value! product mul-me)
    (forget-value! m1 mul-me)
    (forget-value! m2 mul-me)
    (process-new-value))

  (define (mul-me request)
    (cond [(eq? request 'have-value)
           (process-new-value)]
          [(eq? request 'lost-value)
           (process-forget-value)]
          [else
           (error "multiplier: Unknown request" request)]))

  (connect m1 mul-me)
  (connect m2 mul-me)
  (connect product mul-me)
  mul-me)


(define (constant value connector)

  (define (constant-me request)
    (error "constant: Unknown request" request))

  (connect connector constant-me)
  (set-value! connector value constant-me)
  constant-me)

;; 监视器
(define (probe name connector)

  (define (print-probe value)
    (newline)
    (display "=> ")
    (display name)
    (display " = ")
    (display value))

  (define (process-new-value)
    (print-probe (get-value connector)))

  (define (process-forget-value)
    (print-probe "?"))

  (define (probe-me request)
    (cond [(eq? request 'have-value)
           (process-new-value)]
          [(eq? request 'lost-value)
           (process-forget-value)]
          [else
           (error "Unknown request: PROBE" request)]))

  (connect connector probe-me)
  probe-me)


;;
(define c (make-connector))
(define f (make-connector))

(define (cel-fah-converter c f)
  (let ([u (make-connector)]
        [v (make-connector)]
        [w (make-connector)]
        [x (make-connector)]
        [y (make-connector)])
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)))


;; test
(probe "C" c)
(probe "F" f)

(cel-fah-converter c f)

(set-value! c 25 'user)

(forget-value! c 'user)
;(set-value! f 212 'man)
;
;(forget-value! c 'man)
;(forget-value! f 'user)
;
;(set-value! c 12 'user)