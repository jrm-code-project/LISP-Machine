;;; -*- Mode:LISP; Package:CHAOS; Base:8; Readtable:ZL -*-
;;; SHOUT to all Lisp Machines.
(DEFUN SHOUT (&AUX MSG-TEXT HOST PERSON)
  "Send a message to all Lisp machines.  The message is read from the terminal."
  (FS:FORCE-USER-TO-LOGIN)
  (FORMAT T "~%Message: (terminate with ~:@C)~%" #/END)
  (SETQ MSG-TEXT
        (STRING-APPEND "Attention everyone -- " ;formerly just "Everybody: "
                       (ZWEI:QSEND-GET-MESSAGE *QUERY-IO*
                                               (FORMAT NIL
"You are typing a message which will be sent to everyone who is using a Lisp Machine.
If you want to quit, hit the ~:@C key.  To end your message, hit the ~:@C key."
#/ABORT #/END))))                             ; PERSON "anyone")) ;PERSON never bound,
                                                ; could never have been NON-NIL
  (DO ((MACHINE SI:MACHINE-LOCATION-ALIST (CDR MACHINE)))
      ((NULL MACHINE))
    (SETQ HOST (CAAR MACHINE))
    (NET:SEND-TERMINAL-MESSAGE (SI:PARSE-HOST HOST) ""
                               #'(LAMBDA (STREAM)
                                   (FORMAT STREAM "~A@~A ~\DATIME\~%" USER-ID SI:LOCAL-HOST)
                                   (SEND STREAM :STRING-OUT MSG-TEXT)))))
