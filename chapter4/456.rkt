;; the names of all people who are supervised by Ben Bitdiddle,
;; together with their addresses
(and (supervisor ?name (Ben Bitdiddle))
     (address ?name ?where))

;; all people whose salary is less than Ben Bitdiddle’s,
;; together with their salary and Ben Bitdiddle’s salary
(and (salary ?who ?amount1)
     (salary (Ben Bitdiddle) ?amount2)
     (lisp-value < ?amount1 ?amount2))

;; all people who are supervised by someone who is not in the computer division,
;; together with the supervisor’s name and job
(and (supervisor ?person ?someone)
     (not (job ?someone (computer . ?type)))
     (job ?someone ?what))