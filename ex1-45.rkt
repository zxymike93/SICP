#lang sicp


(define (average-damp f)
  [lambda (x) (/ (+ x (f x)) 2)])


(define (fixed-point f guess)
  (let ([close? [lambda (a b) (< (abs (- a b)) 0.0000001)]]
       [next (f guess)])
    (if (close? guess next)
        next
        (fixed-point f next))))


(define (fixed-point-by-transform f trans guess)
  (fixed-point (trans f) guess))


(define (sqrt x)
  (fixed-point-by-transform [lambda (y) (/ x y)]
                            average-damp
                            1.0))


;(sqrt 4)
;Value 2.000000000000002

(define (compose f g)
  [lambda (x) (f (g x))])


(define (repeat f n)
  (if (= n 1)
      f
      (compose (repeat f (- n 1))
               f)))


(define (fourth-root x)
  (fixed-point-by-transform [lambda (y) (/ x (* y y y))]
                            (repeat average-damp 2)
                            1.0))


;(fourth-root 4)
;Value 1.4142135650961398


(define (8th-root x)
  (fixed-point-by-transform [lambda (y) (/ x (* y y y y y y y y))]
                            (repeat average-damp 3)
                            1.0))


;(8th-root 256)
;Value 1.8517494204049794

(define (expt x n)

  (define (iter n result)
    (if (= n 0)
        result
        (iter (- n 1) (* x result))))

  (iter n 1)
)


(define (nth x)

  (define (iter n result)
    (if (> result x)
        n
        (iter (+ n 1) (expt result 2))))

  (iter 1 2)
)


(define (nth-root x n)
  (let ([repeated-times (nth n)])
    (display repeated-times)
    (newline)
    (fixed-point-by-transform [lambda (y) (/ x (expt y (- n 1)))]
                              (repeat average-damp repeated-times)
                              1.0)))


(nth-root 32 5)