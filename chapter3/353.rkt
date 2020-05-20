#lang sicp

(define (map-stream proc . argstreams)
  (if (null-stream? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map car-stream argstreams))
       (apply map-stream
              (cons proc (map cdr-stream argstreams))))))

(define (add-stream s1 s2)
  (map-stream + s1 s2))

(define s (cons-stream 1 (add-stream s s)))

;; 因为 (+ s s) => (+ (1 (delay (+ ...)))
;;                   (1 (delay (+ ...))))
;; s => 1, 2, 4, ..., 2^n