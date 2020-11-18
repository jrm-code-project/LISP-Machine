;-*- Mode:LISP; Package:MAIL; Base:8 -*-
;;; Mail server for the local file system

;;; For now only allow mail from one place at a time.
(DEFVAR *MAIL-SERVER-LOCK* NIL)

(DEFUN MAIL-SERVER (&AUX LOCK CONN STREAM (USER-ID USER-ID))
  (SETQ LOCK (LOCF *MAIL-SERVER-LOCK*))
  (CATCH-ERROR
    (UNWIND-PROTECT
      (PROG TOP ()
            (AND (EQUAL USER-ID "") (SETQ USER-ID "Mail-server"))
            (PROCESS-LOCK LOCK)
            (SETQ CONN (CHAOS:LISTEN "MAIL"))
            (CHAOS:ACCEPT CONN)
            (FUNCALL TV:WHO-LINE-FILE-STATE-SHEET ':ADD-SERVER CONN "MAIL")
            (SETQ STREAM (CHAOS:STREAM CONN))
            (LET* ((RECIPIENTS (GET-MAIL-RECIPIENTS STREAM))
                   (TEXT (GET-MAIL-TEXT STREAM)))
              (DOLIST (RECIPIENT RECIPIENTS)
                (WITH-OPEN-FILE (OUTFILE (FS:MAKE-PATHNAME ':HOST "LM"
                                                           ':DIRECTORY RECIPIENT
                                                           ':NAME RECIPIENT
                                                           ':TYPE "MAIL"
                                                           ':VERSION 0)
                                         '(:OUT :NOERROR))
                  (COND ((STRINGP OUTFILE)
                         (FORMAT STREAM "-Unexpected error for ~A: ~A~%" RECIPIENT OUTFILE)
                         (FUNCALL STREAM ':FORCE-OUTPUT)
                         (RETURN-FROM TOP)))
                  ;; This always appends new mail.  ZMail knows how to reverse it after all.
                  (WITH-OPEN-FILE (INFILE (FS:MAKE-PATHNAME ':HOST "LM"
                                                            ':DIRECTORY RECIPIENT
                                                            ':NAME RECIPIENT
                                                            ':TYPE "MAIL"
                                                            ':VERSION ':NEWEST)
                                          '(:IN :NOERROR))
                    (OR (STRINGP INFILE)
                        (STREAM-COPY-UNTIL-EOF INFILE OUTFILE)))
                  (FUNCALL OUTFILE ':STRING-OUT TEXT)
                  (FUNCALL OUTFILE ':LINE-OUT ""))))
            (FORMAT STREAM "+Message sent successfully.~%")
            (FUNCALL STREAM ':FORCE-OUTPUT)
            (FUNCALL STREAM ':FINISH)
            (FUNCALL STREAM ':CLOSE))
      (AND CONN (CHAOS:REMOVE-CONN CONN))
      (PROCESS-UNLOCK LOCK))))

(DEFUN GET-MAIL-RECIPIENTS (STREAM)
  (DO ((LINE)
       (RECIPIENTS NIL))
      (NIL)
    (SETQ LINE (FUNCALL STREAM ':LINE-IN))
    (AND (EQUAL LINE "") (RETURN (NREVERSE RECIPIENTS)))
    (IF (NOT (FS:LM-DIRECTORY-EXISTS-P LINE))
        (FORMAT STREAM "-Unknown user ~A.~%" LINE)
        (PUSH LINE RECIPIENTS)
        (FORMAT STREAM "+Recipient name ~A ok.~%" LINE))
    (FUNCALL STREAM ':FORCE-OUTPUT)))

(DEFUN GET-MAIL-TEXT (STREAM)
  (WITH-OUTPUT-TO-STRING (SSTREAM)
    (STREAM-COPY-UNTIL-EOF STREAM SSTREAM)
    (FUNCALL SSTREAM ':FRESH-LINE)))

(ADD-INITIALIZATION "MAIL"
                    '(PROCESS-RUN-TEMPORARY-FUNCTION "MAIL Server" 'MAIL-SERVER)
                    NIL
                    'CHAOS:SERVER-ALIST)
