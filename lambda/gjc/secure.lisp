;;; -*- Mode:LISP; Package:CHAOS; Base:10; Readtable:ZL -*-

(DEFVAR *SECURE-HOSTS* '("RAVEL"
                         "LARRY"
                         "CURLEY"
                         "EXP1"
                         "LAM2"
                         "LAM2B"
                         "LAM9"
                         ))

(DEFUN SYMBOLICS-CONNECTION-P (CONN &OPTIONAL REJECT-UNKNOWN-HOSTS)
  (LET ((HOST (CATCH-ERROR (SI:GET-HOST-FROM-ADDRESS (FOREIGN-ADDRESS CONN) :CHAOS) NIL)))
    (COND ((EQ *SECURE-HOSTS* T)
           NIL)
          ((AND (NOT HOST) REJECT-UNKNOWN-HOSTS)
           T)
          ((NOT HOST)
           NIL)
          ((MEMQ #'(LAMBDA (H H?)
                     (EQ H (SI:PARSE-HOST H?)))
                 *SECURE-HOSTS*)
           NIL)
          ('ELSE
           T))))

(DEFUN SYMBOLICS-REJECT (CONN)
  (REJECT CONN (FORMAT NIL "Sorry, security enabled~%service available to only:~%~{~S~^ ~}"
                       *SECURE-HOSTS*)))

