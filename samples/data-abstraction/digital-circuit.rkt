#lang sicp


(define (call-each procs)
  (if (not (null? procs))
      (begin ((car procs))
             (call-each (cdr procs)))))


;; QUEUE
;; (make-queue) -> queue
;; (empty-queue? queue) -> bool
;; (front-queue queue) -> obj
;; (insert-queue! queue obj)
;; (delete-queue! queue)

(define (front-ptr queue)
  (car queue))


(define (rear-ptr queue)
  (cdr queue))


(define (set-front-ptr! queue item)
  (set-car! queue item))


(define (set-rear-ptr! queue item)
  (set-cdr! queue item))


(define (make-queue)
  (cons '() '()))


(define (empty-queue? queue)
  (null? (front-ptr queue)))


(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT-QUEUE: empty queue")
      (car (front-ptr queue))))

;; Add new item at the end of the queue.
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)))))

;; Delete front item of the queue.
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE-QUEUE: empty queue"))
        (else
         (set-front-ptr! queue 
                         (cdr (front-ptr queue))))))


;; WIRE
;;   signal: 0/1
;;   actions: list of procs
;; (make-wire) -> wire
;; (get-signal wire) -> signal
;; (set-signal! wire signal)
;; (add-action! wire procs)

;; Returns an wire object
(define (make-wire)
  (let ((signal 0)
        (actions '()))
    ;; Set local signal to new-value
    (define (set-signal! new-value)
      (if (not (= signal new-value))
          (begin (set! signal new-value)
                 (call-each actions))))
    ;; Add new-action to local actions list
    (define (add-action! new-action)
      (set! actions (cons new-action actions))
      (new-action))

    (define (dispatch msg)
      (cond ((eq? msg 'get-signal) signal)
            ((eq? msg 'set-signal!) set-signal!)
            ((eq? msg 'add-action!) add-action!)
            (else (error "WIRE: unknown operation " msg))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire value) ((wire 'set-signal!) value))
(define (add-action! wire action) ((wire 'add-action!) action))


;; SEGMENT: (time, queue)
;; (make-segment time queue) -> segment
;; (create-segment time proc) -> segment
;; (segment-time segment) -> time
;; (segment-queue segment) -> queue

(define (make-segment time queue)
  (cons time queue))


(define (create-segment time proc)
  (let ((q (make-queue)))
    (insert-queue! q proc)
    (make-segment time q)))


(define (segment-time s)
  (car s))


(define (segment-queue s)
  (cdr s))


;; AGENDA: list of segments (starts with a header 0)
;; (make-agenda) -> agenda
;; (current-time agenda) -> time
;; (empty-agenda? agenda) -> boolean
;; (first-agenda agenda) ->
;; (remove-first-agenda! agenda)
;; (add-to-agenda! time proc agenda)

(define (make-agenda)
  (list 0))


(define (current-time agenda)
  (car agenda))


(define (segments agenda)
  (cdr agenda))


(define (set-current-time! agenda time)
  (set-car! agenda time))


(define (set-segments! agenda segments)
  (set-cdr! agenda segments))


(define (first-segment agenda)
  (car (segments agenda)))


(define (rest-segments agenda)
  (cdr (segments agenda)))


(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (first-agenda agenda)
  (if (empty-agenda? agenda)
      (error "FIRST-AGENDA: agenda is empty")
      (let ((first (first-segment agenda)))
        (set-current-time! agenda (segment-time first))
        (front-queue (segment-queue first)))))


(define (remove-first-agenda! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))


(define (add-to-agenda! time proc agenda)
  ;;
  (define (belongs-before? ss)
    (or (null? ss)
        (< time
           (segment-time (car ss)))))
  ;;
  (define (add-to-segments! ss)
    (if (= (segment-time (car ss)) time)
        (insert-queue! (segment-queue (car ss)) proc)
        (let ((rest (cdr ss)))
          (if (belongs-before? rest)
              (set-cdr! ss
                        (cons (create-segment time proc) rest))
              (add-to-segments! rest)))))
  ;;
  (let ((segs (segments agenda)))
    (if (belongs-before? segs)
        (set-segments! agenda
                       (cons (create-segment time proc) segs))
        (add-to-segments! segs))))


;;

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (not (empty-agenda? the-agenda))
      (let ((first-item (first-agenda the-agenda)))
        (first-item)
        (remove-first-agenda! the-agenda)
        (propagate))))


;; GATE: procs
;; (inverter wire wire)
;; (and-gate wire wire wire)
;; (or-gate wire wire wire)

;; Convert 0 to 1, 1 to 0.
(define (inverter input output)

  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal " s))))

  (define (invert-proc)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))

  (add-action! input invert-proc))

;; Takes two inputs, if both 1 ouputs 1, else 0.
(define (and-gate a1 a2 output)

  (define (logical-and s1 s2)
    (cond ((and (= s1 1) (= s2 1)) 1)
          ((or (= s1 0) (= s2 0)) 0)
          (else (error "Invalid signal " s1 s2))))

  (define (and-proc)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))

  (add-action! a1 and-proc)
  (add-action! a2 and-proc))

;; Takes two inputs, if either 1 ouputs 1, else 0.
(define (or-gate o1 o2 output)

  (define (logical-or s1 s2)
    (cond ((or (= s1 1) (= s2 1)) 1)
          ((and (= s1 0) (= s2 0)) 0)
          (else (error "Invalid signal " s1 s2))))

  (define (or-proc)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))

  (add-action! o1 or-proc)
  (add-action! o2 or-proc))


(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))


;; sample
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ") (display (current-time the-agenda))
                 (display " New value = ") (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define a (make-wire))
(define b (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(half-adder a b sum carry)

;; 0 0 -> 0 0
;(set-signal! a 0)
;(set-signal! b 0)

;; 1 0 -> 0 1
;(set-signal! a 1)
;(set-signal! b 0)

;; 0 1 -> 0 1
;(set-signal! a 0)
;(set-signal! b 1)

;; 1 1 -> 1 0
;(set-signal! a 1)
;(set-signal! b 1)

(propagate)