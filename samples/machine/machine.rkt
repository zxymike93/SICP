#lang sicp

;;; REGISTER <--> lambda ;;;
;; register 是一个有两个接口的数据类型，它有一个局部变量 contents
;; get-contents 获取 contents
;; set-contents! 为 contents 赋值

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch msg)
      (cond ((eq? msg 'get) contents)
            ((eq? msg 'set) (lambda (val) (set! contents val)))
            (else (error "Unknown request -- REGISTER" msg))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register val) ((register 'set) val))


;;; STACK <--> lambda ;;;

(define (make-stack)
  (let ((s '()))
    
    (define (push x)
      (set! s (cons x s)))

    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))

    (define (init)
      (set! s '()))

    (define (dispatch msg)
      (cond ((eq? msg 'push) push)
            ((eq? msg 'pop) pop)
            ((eq? msg 'init) init)
            (else (error "Unknown request -- STACK" msg))))
    dispatch))

(define (push stack x) ((stack 'push) x))
(define (pop stack) ((stack 'pop)))


;;; PROCEDURE

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (constant-exp? exp) (tagged-list? exp 'const))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (operation-exp? exp) (and (pair? exp) (tagged-list? (car exp) 'op)))

;; (const 2)
;; (label after)
;; (reg n)
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (lambda ()
           (cadr exp)))
        ((label-exp? exp)
         (lambda ()
           (lookup-label labels (cadr exp))))
        ((register-exp? exp)
         (lambda ()
           (get-contents (get-register machine (cadr exp)))))
        (else
         (error "Unkown expression type -- ASSEMBLE" exp))))

(define (lookup-prim symbol ops)
  (let ((val (assoc symbol ops)))
    (if val
        (cadr val)
        (error "Unkown operation -- ASSEMBLE" symbol))))

;; ((op *) (reg n) (reg p))
(define (make-operation-exp exp machine labels ops)
  (let ((op (lookup-prim (cadr (car exp)) ops))
        (procs (map (lambda (e) (make-primitive-exp e machine labels))
                    (cdr exp))))
    (lambda ()
      (apply op
             (map (lambda (p) (p))
                  procs)))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; (assign n (const 1)) => #procedure
(define (make-assign inst machine labels ops pc)
  (let ((target (get-register machine (cadr inst)))
        (expr (cddr inst)))
    (let ((proc (if (operation-exp? expr)
                    (make-operation-exp expr machine labels ops)
                    (make-primitive-exp (car expr) machine labels))))
      (lambda ()
        (set-contents! target (proc))
        (advance-pc pc)))))

;; (test (op =) (reg n) (reg p)) => #procedure
(define (make-test inst machine labels ops flag pc)
  (let ((condition (cdr inst)))
    (if (operation-exp? condition)
        (let ((proc (make-operation-exp condition machine labels ops)))
          (lambda ()
            (set-contents! flag (proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

;; (branch (label done))
(define (make-branch inst machine labels flag pc)
  (let ((dest (cadr inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (cadr dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

;; (goto (label loop))
;; (goto (reg continue))
(define (make-goto inst machine labels pc)
  (let ((dest (cadr inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (cadr dest))))
             (lambda ()
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (cadr dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else
           (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

;; (save val)
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (cadr inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

;; (restore continue)
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (cadr inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

;; (perform (op print) (reg a))
(define (make-perform inst machine labels ops pc)
  (let ((action (cdr inst)))
    (if (operation-exp? action)
        (let ((proc (make-operation-exp action machine labels ops)))
          (lambda ()
            (proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (make-procedure instruction labels machine pc flag stack ops)
  (let ((inst (car instruction)))
    (cond ((eq? inst 'assign)
           (make-assign instruction machine labels ops pc))
          ((eq? inst 'test)
           (make-test instruction machine labels ops flag pc))
          ((eq? inst 'branch)
           (make-branch instruction machine labels flag pc))
          ((eq? inst 'goto)
           (make-goto instruction machine labels pc))
          ((eq? inst 'save)
           (make-save instruction machine stack pc))
          ((eq? inst 'restore)
           (make-restore instruction machine stack pc))
          ((eq? inst 'perform)
           (make-perform instruction machine labels ops pc))
          (else
           (error "Unknown instruction type -- ASSEMBLE " instruction)))))


;;; INSTRUCTION
;; ('(assign n (const 1)) . #procedure)
(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-procedure inst) (cdr inst))
(define (set-instruction-procedure! inst proc) (set-cdr! inst proc))

;;; LABEL
;; '(test-fact
;;   ((test ....) . #procedure)
;;   ((branche ...) . #procedure))
(define (make-label-entry label insts) (cons label insts))
(define (lookup-label labels name)
  (let ((label (assoc name labels)))
    (if label
        (cdr label)
        (error "Undefined label -- ASSEMBLE" name))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each (lambda (inst)
                (set-instruction-procedure!
                 inst
                 (make-procedure (instruction-text inst)
                                 labels machine
                                 pc flag stack ops)))
              insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              ;; label
                              (receive insts
                                       (cons (make-label-entry next-inst insts)
                                             labels))
                              ;; instruction
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

(define (assemble controller machine)
  (extract-labels controller
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))


;;; MACHINE <--> lambda

(define (make-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'init-stack
                       (lambda () (stack 'init)))))
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))

      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table))))

      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))

      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ;(newline) (newline) (display insts) (newline)
                ((instruction-procedure (car insts)))
                (execute)))))

      (define (dispatch msg)
        (cond ((eq? msg 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? msg 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? msg 'allocate-register) allocate-register)
              ((eq? msg 'get-register) lookup-register)
              ((eq? msg 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? msg 'stack) stack)
              ((eq? msg 'operations) the-ops)
              ((eq? msg 'controller) the-instruction-sequence)
              (else (error "Unknown request -- MACHINE" msg))))
      dispatch)))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (init-machine reg-names ops controller)
  (let ((machine (make-machine)))
    (for-each (lambda (name) ((machine 'allocate-register) name))
              reg-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller machine))
    machine))

(define (get-register-contents machine reg-name)
  (get-contents (get-register machine reg-name)))

(define (set-register-contents! machine reg-name value)
  (set-contents! (get-register machine reg-name) value))

(define (start machine) (machine 'start))
