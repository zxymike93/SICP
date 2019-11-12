#lang sicp

(define (square x) (* x x))

;; int, P -> rat
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1))]
          [else
           (iter (- trials-remaining 1)
                 trials-passed)]))
  (iter trials 0))

;; dec, dec -> dec
(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (random range))))

;; P(x, y): if (x, y) in circle, true; else false
(define (P x1 x2 y1 y2)
  (let ([r (/ (- x2 x1) 2)]
        [x0 (/ (+ x2 x1) 2)]
        [y0 (/ (+ y2 y1) 2)])
    (lambda []
      (>= (square r)
          (+ (square (- (random-in-range x1 x2) x0))
             (square (- (random-in-range y1 y2) y0)))))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (P x1 x2 y1 y2)))

;; test
(define x1 0.0)
(define y1 0.0)
(define x2 6.0)
(define y2 6.0)
(define r (/ (- x1 x2) 2.0))

(define area-of-rectangle
  (* (- x2 x1) (- y2 y1)))

(define area-of-circle
  (* (estimate-integral P x1 x2 y1 y2 10000)
     area-of-rectangle))

;; r^2 * pi = area-of-circle
(/ area-of-circle (square r))