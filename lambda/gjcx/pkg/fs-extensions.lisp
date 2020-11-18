;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:T; Base:10 -*-

;;; things that should be in local file.


;;; (C) Copyright 1987, LISP MACHINE INC
;;; See filename "Copyright.Text" for more information.
;;; *********************************************************
;;; *********************************************************
;;; *** NOTE: This is an EXAMPLE, not LMI supported code. ***
;;; *** information contained in this example is subject  ***
;;; *** to change without notice. The ways of doing       ***
;;; *** the things contained in the example may change    ***
;;; *** between system releases. Some techniques which    ***
;;; *** are mere examples in one release may become built ***
;;; *** in system features in the next release. Use good  ***
;;; *** judgement when copying these techniques. Most     ***
;;; *** examples have been motivated by specific customer ***
;;; *** requests, and may not be the best engineered      ***
;;; *** or most efficient solution for someone else.      ***
;;; *********************************************************
;;; *********************************************************



;;; speed ups.
#|| SLOW WAY IS
SLOW WAY IS (DOLIST (E LIST)
              (APPLY 'fs:change-file-properties (car E) t (CDR E)))
||#

(defun multiple-change-file-properties (error-p list)
  ;; list of (file . plist-items)
  ;; This should an operation to the pathname.
  (IDENTIFY-FILE-OPERATION :CHANGE-PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (LET ((FILES)
            (PLISTS))
        (UNWIND-PROTECT
            (PROGN
              (DOLIST (E LIST)
                (LET ((PATHNAME (CAR E)))
                  (PUSH (LOOKUP-FILE
                          (PATHNAME-RAW-DIRECTORY PATHNAME)
                          (PATHNAME-RAW-NAME PATHNAME)
                          (PATHNAME-RAW-TYPE PATHNAME)
                          (PATHNAME-RAW-VERSION PATHNAME)
                          :ERROR NIL)
                        FILES)
                  (PUSH (CDR E) PLISTS)))
              (LMFS-MULTIPLE-CHANGE-FILE-PROPERTIES FILES PLISTS))
          (DOLIST (FILE FILES)
            (LMFS-CLOSE-FILE FILE)))))))

(DEFUN LMFS-MULTIPLE-CHANGE-FILE-PROPERTIES (FILES PLISTS)
  (DO ((DIRS)
       (L1 FILES (CDR L1))
       (L2 PLISTS (CDR L2)))
      ((NULL L1)
       (DOLIST (DIR DIRS)
         (WRITE-DIRECTORY-FILES DIR)))
    (LET ((FILE (CAR L1))
          (PLIST (CAR L2)))
      (LOCKING (FILE-LOCK FILE)
        (DO ((P PLIST (CDDR P)))
            ((NULL P)
             (IF (FILE-CLOSED? FILE)
                 (PUSHNEW (FILE-DIRECTORY FILE) DIRS)))
          (SELECTQ (CAR P)
            (:DELETED
             (IF (NULL (CADR P))
                 (LMFS-UNDELETE-FILE FILE)
               (LMFS-DELETE-FILE FILE)))
            ((:DONT-DELETE :DONT-REAP :CHARACTERS)
             (SETF (FILE-ATTRIBUTE FILE (CAR P)) (CADR P)))
            (:NOT-BACKED-UP
             (SETF (FILE-ATTRIBUTE FILE ':DUMPED) (NOT (CADR P))))
            (:BYTE-SIZE
             (SETF (FILE-DEFAULT-BYTE-SIZE FILE) (CADR P)))
            (:AUTHOR
             (SETF (FILE-AUTHOR-INTERNAL FILE) (CADR P)))
            (:CREATION-DATE
             (SETF (FILE-CREATION-DATE-INTERNAL FILE) (CADR P)))
            (OTHERWISE
             (COND ((MEMQ (CAR P) LM-UNSETTABLE-PROPERTIES)
                    (LM-SIGNAL-ERROR 'UNSETTABLE-PROPERTY NIL NIL (CAR P)))
                   ((NOT (SYMBOLP (CAR P)))
                    (LM-SIGNAL-ERROR 'INVALID-PROPERTY-NAME NIL NIL (CAR P)))
                   ((GET (CAR P) 'ATTRIBUTE)
                    (FERROR NIL "CHANGE-FILE-PROPERTIES hasn't been updated to match the defined attributes."))
                   (T (PUTPROP (LOCF (FILE-PLIST FILE)) (CADR P) (CAR P)))))))))))
