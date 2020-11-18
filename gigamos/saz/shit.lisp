
;;; This gets called when starting to get new mail or after one file of new mail
;;; has been read in.  It should return T if it has started something loading.
(DEFMETHOD (INBOX-BUFFER :START-NEXT-FILE) ()
  (DO ((FILE) (RENAME) (DELETE-P) (STR)
       (LOADING-NAME NIL NIL)
       (LOADING-TRUENAME NIL NIL))
      ((NULL

         ;(progn
         ;  (with-open-file (foo "DJ:SAZ;test.lisp" :direction :output :if-exists :append)
         ;    (format foo "~A" PENDING-FILE-LIST))

               PENDING-FILE-LIST))
       NIL)
    (SETF `(,FILE ,RENAME ,DELETE-P) (CAR PENDING-FILE-LIST))
    ;; If the next pending file is on a different host,
    ;; and needs a different flavor of INBOX-BUFFER,
    ;; replace this one with a suitable one and try again with that one.
    (UNLESS (EQ (TYPE-OF SELF) (SEND FILE :INBOX-BUFFER-FLAVOR))
      (RETURN (SEND (REPLACE-INBOX-BUFFER (SEND FILE :INBOX-BUFFER-FLAVOR))
                    :START-NEXT-FILE)))
    (COND ((NOT RENAME)
           ;;No file to rename to, see if new file exists
           (CONDITION-CASE ()
               (SETQ STREAM (OPEN FILE :DIRECTION :INPUT))
             (FS:FILE-NOT-FOUND (POP PENDING-FILE-LIST) NIL)
             (:NO-ERROR (POP PENDING-FILE-LIST) NIL)
             (ERROR)))
          ((IGNORE-ERRORS
             (SETQ STREAM (OPEN RENAME :DIRECTION :INPUT)))
           (LET ((TEM (CAR PENDING-FILE-LIST)))
             (POP PENDING-FILE-LIST)
             ;;If file to rename to already exists,
             ;;arrange for real file to get read in after saving done next time
             (SETQ NEXT-PENDING-FILE-LIST
                   (NCONC NEXT-PENDING-FILE-LIST (NCONS TEM)))))
          ((CONDITION-CASE ()
               (SETQ STR (OPEN FILE :DIRECTION NIL))
             (FS:FILE-NOT-FOUND (POP PENDING-FILE-LIST) NIL)
             (ERROR))
           (POP PENDING-FILE-LIST)
           ;;Rename to new file.  This does not use the :RENAME stream operation since that
           ;;doesn't work correctly on Tops-20 and does not return the TRUENAME.
           (SETQ LOADING-NAME (SEND STR :PATHNAME)
                 LOADING-TRUENAME (SEND STR :TRUENAME))
           (CONDITION-CASE ()
               (SEND FILE :RENAME RENAME)
             (FS:FILE-LOCKED
              (FORMAT *QUERY-IO* "~&File ~A is being accessed so cannot be renamed.  Pausing."
                      LOADING-TRUENAME)
              (PROCESS-SLEEP (* 10. 60.))
              (DO ()
                  (())
                (CONDITION-CASE ()
                    (SEND FILE :RENAME RENAME)
                  (FS:FILE-LOCKED NIL)
                  (:NO-ERROR
                   (FORMAT *QUERY-IO* "~&File successfully renamed, continuing.")
                   (RETURN T)))
                (UNLESS (Y-OR-N-P "Try again?  You can also abort. ")
                  (SIGNAL-CONDITION EH:ABORT-OBJECT)))))
           ;;Get the renamed file
           (SETQ STREAM (OPEN RENAME '(:IN)))))
    (WHEN STREAM
      (SETQ PATHNAME (OR LOADING-NAME (SEND STREAM :PATHNAME))
            NAME (SEND PATHNAME :STRING-FOR-PRINTING)
            STATUS :LOADING-NEW-MAIL)
      (START-LOADING-ZMAIL-BUFFER SELF STREAM T LOADING-TRUENAME)
      (AND DELETE-P (PUSH (SEND STREAM :TRUENAME) PENDING-DELETION-LIST))
      (RETURN T))))
