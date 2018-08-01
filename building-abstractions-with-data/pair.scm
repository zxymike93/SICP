(define (cons x y)
  (define (dispatch i)
    (cond ((= i 0) x)
          ((= i 1) y)
          (else
           (error "Argument is not 0 or 1."))))

  dispatch)


(define (car z) (z 0))

(define (cdr z) (z 1))
