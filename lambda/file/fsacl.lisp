;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10 -*-


;;; Copyright LISP Machine, Inc. 1986
;;;   See filename "Copyright.Text" for
;;; licensing and release information.

;; file access control mechanism:
;; Prevent accidental deletion of files and accidental writing to directories.
;;  As RG put it to me, "George, you know that a road to hell can be paved with good intentions,
;;                       but what you might not know is that the first step is usually
;;                       the easiest, certainly the most seductive, and by far the least expensive to avoid."
;;
;;  The point here is that while it is easy to add access control it is not easy to
;;  control the control in a flexible manner. Example: In the case of a loser (uninitiated person)
;;  writing a munged META-DOT file back to the system source you would like to signal an error
;;  that would cause a "Do you really want to mess up the system sources?" to which he can reply
;;  "yes damn it" which also sends mail to the system maintainer. The present QFILE access methods
;;  just dont provide the proper signalling and control channels for that kind of thing.
;;  On the other hand, you dont need to use the file access channels, you can create other connections
;;  at will. e.g. Say a file access is being made from a QFILE server. The access control can
;;  tell what host the request is coming from and what username (and possibly password) was given.
;;  The mechanism is free do something like (CHAOS:OPEN-STREAM <requesting-host> "VERIFY-ACCESS")
;;  and talk to that.
;;
;; So for now, here are some hooks, that we will at least be using to record the patterns
;; of file access.
;;
;;
;; * Need to handle operations defined in FSACC.
;;

;; Must hack:
;;
;; LOOKUP-FILE. looking at *CURRENT-FILE-OPERATION* to handle
;;              :CHANGE-PROPERTIES, :PROPERTIES, :DELETE, :OPEN,
;;              signal error if access denied.
;; LMFS-COMPLETE-PATH. to return nil if access denied.
;;
;; LMFS-CREATE-DIRECTORY, signal error of access denied.
;;
;; LOOKUP-FILES. looking at *CURRENT-FILE-OPERATION* to handle
;;                    :EXPUNGE, :DIRECTORY-LIST. signal error.
;;
;; TOP-LEVEL-DIRECTORIES. looking at *CURRENT-FILE-OPERATION* to handle
;;                        :ALL-DIRECTORIES. signal error.


(DEFVAR *ACCESS-CONTROL* NIL)

;;; *ACCESS-CONTROL* is a function of 5 arguments:
;;    (OPERATION PURPOSE DIRECTORY NAME TYPE VERSION)
;;
;; Operations.
;; :READ-FILE        read an existing file or look at its properties.
;; :WRITE-FILE       change data in an existing file.
;; :CREATE-FILE      create a new file.
;; :COMPLETE         find other file names that are like this one.
;; :CREATE-DIRECTORY create a directory.
;; :LIST             list files that match a pattern.
;;
;; Purposes:
;; :OPEN
;; :DELETE to delete something.
;;
;;
;; If the operation is allowed then return NIL, if denied then return
;;  :FILE if something is wrong due to the particular file.
;;  :DIRECTORY if something is wrong due to the directory.
;;  T otherwise.
;;
;; Note: The DIRECTORY NAME TYPE VERSION are that requested, e.g. ("L" "SYS") "FOO" "LISP" :NEWEST
;;       *not* that of the actual file that would be returned or acted upon by the operation.
;;       So to find the exact file you must do the operation yourself.

(DEFUN ACCESS-CONTROL (OPERATION DIRSPEC NAME TYPE VERSION)
  (SETQ DIRSPEC (COND ((TYPEP DIRSPEC 'FILE)
                       (OR (DIRECTORY-FULL-NAME DIRSPEC)
                           :ROOT))
                      ('ELSE
                       (OR DIRSPEC :ROOT))))
  (COND ((NOT *ACCESS-CONTROL*)
         T)
        ('ELSE
         (LET ((F *ACCESS-CONTROL*)
               (VALUE))
           (LET ((*ACCESS-CONTROL* NIL))
             (SETQ VALUE (FUNCALL F OPERATION
                                  *CURRENT-FILE-OPERATION*
                                  DIRSPEC
                                  NAME
                                  TYPE
                                  VERSION)))
           (COND ((EQ OPERATION :COMPLETE)
                  (NOT VALUE))
                 ((EQ VALUE NIL)
                  T)
                 ('ELSE
                  (LM-SIGNAL-ERROR (OR (CADR (ASSQ VALUE '((:FILE INCORRECT-ACCESS-TO-FILE)
                                                           (:DIRECTORY INCORRECT-ACCESS-TO-DIRECTORY))))
                                       'ACCESS-ERROR)
                                   (MAKE-PATHNAME :HOST "LM" :DEVICE "DSK"
                                                  :DIRECTORY DIRSPEC
                                                  :NAME NAME :TYPE TYPE :VERSION VERSION)
                                   NIL
                                   NIL *CURRENT-FILE-OPERATION*)))))))


(DEFUN LOOKUP-FILE-ACL (DIRSPEC NAME TYPE VERSION IF-DOES-NOT-EXIST IF-EXISTS REALLY-OPEN DELETED?)
  REALLY-OPEN DELETED?
  ;; IF-DOES-NOT-EXIST = NIL, :ERROR, :CREATE
  ;; IF-EXISTS = NIL, :NEW-VERSION, :SUPERSEDE, :OVERWRITE, :TRUNCATE, :APPEND, :ERROR, :RENAME, :RENAME-AND-DELETE
  ;; perhaps i have oversimplified the operations. the arguments to these function are
  ;; quite hairy however, so it is good to simplify some. What cases are meaningful
  ;; out of the 3*9 = 27 of them?
  ;; Trick: Handle multiple meanings with multiple calls.
  (COND ((MEMQ IF-EXISTS '(:SUPERSEDE :OVERWRITE :TRUNCATE :APPEND :RENAME :RENAME-AND-DELETE))
         (ACCESS-CONTROL :WRITE-FILE DIRSPEC NAME TYPE VERSION)))
  (COND ((EQ IF-DOES-NOT-EXIST :CREATE)
         (ACCESS-CONTROL :CREATE-FILE DIRSPEC NAME TYPE VERSION))
        ('ELSE
         (ACCESS-CONTROL :READ-FILE  DIRSPEC NAME TYPE VERSION))))


(DEFUN COMPLETE-PATH-ACLP (DIR NAME TYPE)
  (ACCESS-CONTROL :COMPLETE DIR NAME TYPE NIL))

(DEFUN CREATE-DIRECTORY-ACL (NAME)
  (ACCESS-CONTROL :CREATE-DIRECTORY NAME NIL NIL NIL))

(DEFUN LOOKUP-FILES-ACL (DIRECTORY NAME TYPE VERSION)
  (ACCESS-CONTROL :LIST DIRECTORY NAME TYPE VERSION))

(DEFUN TOP-LEVEL-DIRECTORIES-ACL ()
  (ACCESS-CONTROL :LIST :ROOT :WILD :WILD :WILD))


;; SOME EXAMPLE ACCESS CONTROL FUNCTIONS.

(DEFUN HACK-PEOPLE-ACCESS (OPERATION PURPOSE DIRECTORY NAME TYPE VERSION)
  OPERATION
  PURPOSE
  (COND ((OR (EQ DIRECTORY :WILD)
             (EQ NAME :WILD)
             (EQ TYPE :WILD)
             (EQ VERSION :WILD))
         :DIRECTORY)
        ('ELSE
         :FILE)))

(DEFVAR *ACCESS-RECORD* NIL)

(DEFUN SAVE-ACCESS-RECORD (&OPTIONAL FILENAME)
  (LET ((*ACCESS-CONTROL* NIL)
        (S (TIME:PRINT-CURRENT-TIME NIL :YY-MMM-DD))
        (CL  (SI:FIND-READTABLE-NAMED "CL"))
        (UP (FIND-PACKAGE "USER")))
    (WITH-OPEN-FILE (STREAM (OR FILENAME
                                (FORMAT NIL "LM:ACCESS-RECORD;~A.LISP#>"
                                        (SUBSTRING S 0 9)))
                            :DIRECTION :OUTPUT)
      (FORMAT STREAM ";;; -*-MODE:LISP;PACKAGE:USER;READTABLE:CL;BASE:10-*-~%")
      (FORMAT STREAM ";;; ACCESS RECORD FOR ~S ON ~A~%" SI:LOCAL-HOST S)
      (DOLIST (R (NREVERSE (PROG1 *ACCESS-RECORD* (SETQ *ACCESS-RECORD* NIL))))
        (LET ((BASE 10.)
              (*READTABLE* CL)
              (*PACKAGE* UP))
          (PRIN1 R STREAM))
        (TERPRI STREAM)))))

(DEFUN ACCESS-USER-DESCRIPTION ()
  (COND ((AND (BOUNDP 'CONN) (TYPEP CONN 'CHAOS:CONN))
         ;; SPECIAL VARIABLE BOUND BIND THE FILE SERVER!
         (LIST USER-ID
               :CHAOS
               (OR (SI:GET-HOST-FROM-ADDRESS (CHAOS:FOREIGN-ADDRESS CONN) :CHAOS)
                   (CHAOS:FOREIGN-ADDRESS CONN))))
        ('ELSE
         USER-ID)))


(DEFUN RECORD-ACCESS (OPERATION PURPOSE DIRECTORY NAME TYPE VERSION)
  (LET ((RECORD (LIST OPERATION PURPOSE DIRECTORY NAME TYPE VERSION
                      (ACCESS-USER-DESCRIPTION)
                      (TIME:GET-UNIVERSAL-TIME))))
    (WITHOUT-INTERRUPTS
      (PUSH RECORD *ACCESS-RECORD*)))
  NIL)
