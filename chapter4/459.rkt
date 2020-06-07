;;; 1. query meeting on friday morning

(and (meeting ?division (Friday ?time))
     (lisp-value assoc 'am ?time))

;;; 2.

(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?work))
           (or (meeting ?division ?day-and-time)
               (meeting whole-company ?day-and-time))))

;;; 3.

(meeting-time (Alyssa P. Hacker) (Wednesday . ?time))