#lang sicp

(define zero [lambda (f) [lambda (x) x]])

(define (add-1 n)
  [lambda (f) [lambda (x) (f ((n f) x))]])

#|
; Let's define `one` as `(add-1 zero)`,
;   but not directly `(define one (add-1 zero))`.
; So, `one` can be evaluate through the process below
(add-1 zero)

; First, evaluate argument: n -> zero
(add-1 [lambda (f) [lambda (x) x]])

; Second, extract the "precedure"
[lambda (f) [lambda (x) (f (
                            ([lambda (f) [lambda (x) x]]  ;; zero
                             f)                           ;; reduce here
                            x))]]

[lambda (f) [lambda (x) (f
                           ([lambda (x) x]  ;; reduce
                            x)              ;; again
                          )]]

; Finally, we'll get
[lambda (f) [lambda (x) (f x)]]

; To define `two`, we use `one` defined above
; two = (add-1 one) = [lambda (f) [lambda (x) (f x)]]

(add-1 [lambda (f) [lambda (x) (f x)]])

[lambda (f) [lambda (x) (f (
                            ([lambda (f) [lambda (x) (f x)]]
                             f)
                            x))]]

[lambda (f) [lambda (x) (f
                         ([lambda (x) (f x)]
                          x))]]

[lambda (f) [lambda (x) (f (f x))]]
|#

; Direct definition of `one` and `two` (not in terms of `zero` and `add-1`)
(define one
  [lambda (f) [lambda (x) (f x)]])

(define two
  [lambda (f) [lambda (x) (f (f x))]])

; By observing that,
;   there are `(f x)` and `(f (f x))`
;   inside `one` and `two` respectively,
; (+ one two) should be calling (f) once plus twice.
; Thus, `+` means calling nth (f)
;   while nth is the sum of elements in `+` operation...

(define add
  [lambda (a)
    [lambda (b)
      [lambda (f) [lambda (x) (a f (b f x))]]]])

#| Let's try (add one two)
(
 [lambda (a)
   [lambda (b)
     [lambda (f) [lambda (x) (a f (b f x))]]]]
 one
 two
 )

(
 [lambda (b)
   [lambda (f) [lambda (x) (one f (b f x))]]]
 two
 )

[lambda (f) [lambda (x) (one f (two f x))]]

[lambda (f) [lambda (x) (one f
                             ([lambda (f) [lambda (x) (f (f x))]]
                              f
                              x))
              ]]
             
[lambda (f) [lambda (x) (one f (f (f x)))]]

[lambda (f) [lambda (x)
              ([lambda (f) [lambda (x) (f x)]]
               f
               (f (f x)))
              ]]

[lambda (f) [lambda (x) (f (f (f x)))]]
|#

#|
`add` `one` `two` ... work the same as + 1 2 ...
If we rename them, they perfectly serve as "real" data.
That is, procedural representations of data...
|#