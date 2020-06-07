;;; Base on the interfaces:
;;; (can-do-job ...)
;;; (rule (same ?x ?x))

(rule (replace ?person1 ?person2)
      (and (or
            ;; person1 does the same job as person2
            (and (job ?person1 ?work)
                 (job ?person2 ?work))
            ;; someone who does person1's job
            (and (job ?person1 ?work1)
                 (job ?someone ?work1)
                 ;; can also do person2's job
                 (job ?person2 ?work2)
                 (can-do-job ?someone ?work2)))
           ;; person1 and person2 are not the same
           (not (same ?person1 ?person2))))

;; replace Cy D. Fect
(replace ?who (Cy D. Fect))

;; replace someone who is being paid more than they are,
;; with the two salaries.
(and (replace ?person ?someone)
     (salary ?person ?s1)
     (salary ?someone ?s2)
     (lisp-value < ?s1 ?s2))