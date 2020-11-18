;;; -*- Mode:LISP; Package:SITE-DATA-EDIT; Base:10; Readtable: CL -*-
;;; Copyright (c) Lisp Machine Inc., 1986.

;;; NOW. SPECIAL BOOTSTRAP CONSIDERATIONS ARE DEALT WITH.
;;; 3-May-86 12:34:23 -GJC

(DEFUN BOOTSTRAP-SITE-INFORMATION (&OPTIONAL DISK)
  (LET ((INFO (BOOTSTRAP-READ-INFORMATION (ECASE DISK
                                            (NIL NIL)
                                            (:SAVE T)))))
    (LET ((DATA (BOOTSTRAP-READ-SITE-DATA (GETF INFO 'SITE-DIR))))
      (LET ((TRANS (BOOTSTRAP-CHECK-SYS-TRANSLATIONS (GETF INFO 'SITE-DIR)
                                                     (SEND (BOOTSTRAP-LOOKUP-HOST (GETF INFO 'NAME)
                                                                                  (GETF DATA 'SI:HOST-ALIST))
                                                           :NAME))))
        (BOOTSTRAP-SITE-INFORMATION-DOIT DATA INFO TRANS)))))

(DEFVAR *BOOTSTRAP-SITE-VARS* '(SI:HOST-ALIST SI:SITE-NAME SI:SITE-OPTION-ALIST SI:MACHINE-LOCATION-ALIST))

(DEFVAR *BOOTSTRAP-OLD* NIL)

(DEFUN BOOTSTRAP-SITE-INFORMATION-DOIT (DATA INFO TRANS)
  (DOLIST (VAR *BOOTSTRAP-SITE-VARS*)
    (SETF (GETF *BOOTSTRAP-OLD* VAR) (SYMBOL-VALUE VAR)))
  (SETF (GETF *BOOTSTRAP-OLD* 'PACK-NAME) (FORMAT NIL "~{~A~^ ~}" (MULTIPLE-VALUE-LIST (SI:GET-PACK-NAME))))
  (DOLIST (VAR *BOOTSTRAP-SITE-VARS*)
    (SET VAR (GETF DATA VAR)))
  (SI:SET-PACK-NAME (GETF INFO 'NAME))
  (BOOTSTRAP-SITE-INITIALIZATIONS)
  (EVAL TRANS)
  (COND ((GETF INFO 'LOD)
         (SETQ *BOOTSTRAP-OLD* NIL)
         (SI:DISK-SAVE (GETF INFO 'LOD)))))

(DEFUN BOOTSTRAP-SITE-INITIALIZATIONS ()
  (FORMAT T "~&Running bootstrap site initializations...")
  (CHAOS:SETUP-MY-ADDRESS) ;; AN INIT, BUT IN WRONG ORDER IN INIT LIST.
  (LET ((SI:HOST-TABLE-FILE-ALIST NIL))
    ;; must bind above to keep reset-non-site-hosts from reading site files again.
    (INITIALIZATIONS 'SI:SITE-INITIALIZATION-LIST T))
  (CHAOS:RESET T)
  (FORMAT T " done.~%"))

(DEFUN BOOTSTRAP-SITE-REVERT ()
  (DOLIST (VAR *BOOTSTRAP-SITE-VARS*)
    (SET VAR (GETF *BOOTSTRAP-OLD* VAR)))
  (SI:SET-PACK-NAME (GETF *BOOTSTRAP-OLD* 'PACK-NAME))
  (BOOTSTRAP-SITE-INITIALIZATIONS))


(DEFUN BOOTSTRAP-READ-INFORMATION (DISK-SAVEP)
  (LET (NAME SITE-DIR LOD)
    (DO ()
        ((SETQ NAME (PROMPT-AND-READ :STRING-OR-NIL
                                     "~& Short name of this machine: "))))
    (DO ()
        ((WHEN (AND (SETQ SITE-DIR (CATCH-ERROR (PROMPT-AND-READ :PATHNAME-OR-NIL "~& Site file directory       : ")))
                    (SETQ SITE-DIR (SEND SITE-DIR :NEW-PATHNAME :HOST "LM"
                                         :NAME "SITE" :TYPE "LISP" :VERSION :NEWEST)))
           (LET ((TEMP (OPEN SITE-DIR :ERROR NIL :direction nil)))
           (COND ((ERRORP TEMP)
                  (FORMAT T "Incorrect Site File directory: ~S~%" site-dir)
                  (send temp :report standard-output)
                  nil)
                 ('else
                  (setq site-dir (send temp :truename))))))))
    (when disk-savep
      (LET ((L (SUBSET #'(LAMBDA (X)
                           (STRING-EQUAL "LOD" (CAR X) :END2 3))
                       (SI:PARTITION-LIST)))
            (CB (SI:CURRENT-BAND)))
        (DO ((x))
            ((PROGN (FORMAT T "~&Available LOD Partitions:~%")
                    (DOLIST (P L)
                      (FORMAT T "~4A ~14A ~25S ~5D blocks.~%"
                              (NTH 0 P)
                              (COND ((STRING-EQUAL CB (NTH 0 P))
                                     "(current band)")
                                    ((STRING-EQUAL (NTH 3 P) "")
                                     "(unused band)")
                                    ('ELSE
                                     "(a lisp band)"))
                              (NTH 3 P)
                              (NTH 2 P)))
                    (SETQ LOD (PROMPT-AND-READ :STRING-OR-NIL "~& LOD band to save to :"))
                    (setq x lod)
                    (WHEN (STRING-EQUAL "LOD" x :END2 3)
                      (SETQ x (SUBSTRING x 3)))
                    (COND ((FIXP (CATCH-ERROR (PARSE-INTEGER x)))
                           (SETQ x (PARSE-INTEGER x))
                           (LET ((FL (FORMAT NIL "LOD~D" x)))
                             (DO ((P L (CDR P)))
                                 ((NULL P)
                                  (FORMAT T "Not an available load partition: ~S" LOD)
                                  NIL)
                               (IF (string-equal (caar p) fl)
                                   (return (setq lod x))))))))))))
    (list 'NAME name 'SITE-DIR site-dir 'LOD lod)))

(DEFUN BOOTSTRAP-READ-SITE-DATA (DIR)
  (LET ((FS (LIST (SEND DIR :NEW-PATHNAME :NAME "SITE" :TYPE "QFASL" :VERSION :NEWEST)
                  (SEND DIR :NEW-PATHNAME :NAME "HSTTBL" :TYPE "QFASL" :VERSION :NEWEST)
                  (SEND DIR :NEW-PATHNAME :NAME "LMLOCS" :TYPE "QFASL" :VERSION :NEWEST))))
    (remprop 'si:machine-location-alist :source-file)
    (PROGV *BOOTSTRAP-SITE-VARS*
           (MAKE-LIST (LENGTH *BOOTSTRAP-SITE-VARS*))
      (DOLIST (F FS)
        (LOAD F :SET-DEFAULT-PATHNAME NIL))
      (DOLIST (ELEM SI:HOST-ALIST)
        (SI:GET-ALIST-ELEM-HOST ELEM))
      (MAPCAN #'(LAMBDA (VAR)
                  (LIST VAR (SYMBOL-VALUE VAR)))
              *BOOTSTRAP-SITE-VARS*))))

(DEFUN BOOTSTRAP-LOOKUP-HOST (NAME ALIST)
  (OR (DOLIST (A ALIST)
        (WHEN (MEM #'STRING-EQUAL NAME (SI:HOST-NAME-LIST A))
          (RETURN (SI:HOST-INSTANCE A))))
      (FERROR NIL "Host ~S not found in bootstrap data" NAME)))

(DEFVAR *RELEASE-ROOT-NAME* "RELEASE-3")
(DEFVAR *RELEASE-ROOT-DEPTH* 5)

(DEFUN BOOTSTRAP-CHECK-SYS-TRANSLATIONS (DIR SYS-HOST-NAME)
  (LET* ((NP (SEND DIR :NEW-PATHNAME :NAME NIL :TYPE NIL :VERSION NIL))
         (SP (SEND DIR :NEW-PATHNAME :NAME "SYS" :TYPE "TRANSLATIONS" :VERSION :NEWEST))
         (FORM `(FS:SET-LOGICAL-PATHNAME-HOST "SYS"
                  :PHYSICAL-HOST ,SYS-HOST-NAME
                  :TRANSLATIONS '(("CHAOS;" ,(SEND NP :STRING-FOR-HOST))
                                  ("SITE;" ,(SEND NP :STRING-FOR-HOST))
                                  ,@(DO ((L NIL
                                            (CONS (LIST (FORMAT NIL "~{~A;~}"
                                                                (MAKE-LIST DEPTH :INITIAL-ELEMENT '*))
                                                        (SEND (SEND NP :NEW-PATHNAME
                                                                    :DIRECTORY (CONS *RELEASE-ROOT-NAME*
                                                                                     (MAKE-LIST DEPTH
                                                                                                :INITIAL-ELEMENT :WILD)))
                                                              :STRING-FOR-HOST))
                                                  L))
                                         (DEPTH *RELEASE-ROOT-DEPTH* (1- DEPTH)))
                                        ((ZEROP DEPTH)
                                         L))))))
    (FLET ((WRITE-FORM ()
                       (WITH-OPEN-FILE (S SP :DIRECTION :OUTPUT)
                         (FORMAT S ";; SYS.TRANSLATIONS WRITTEN BY BOOTSTRAP SITE GENERATOR~%")
                         (PPRINT FORM S)
                         (TERPRI S))))
      (COND ((NOT (PROBE-FILE SP))
             (WRITE-FORM))
            ((WITH-OPEN-FILE (S SP)
               (NOT (EQUALP FORM (LET ((*PACKAGE* (FIND-PACKAGE "USER"))
                                       (*READTABLE* (SI:FIND-READTABLE-NAMED "ZL")))
                                   (READ S)))))
             (WRITE-FORM)))
      FORM)))
