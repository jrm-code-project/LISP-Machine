;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10; Patch-File:T -*-

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


(DEFMETHOD (LOCAL-FILE-ACCESS :CHANGE-PROPERTIES) (PATHNAME ERROR-P &REST PLIST)
  (IDENTIFY-FILE-OPERATION :CHANGE-PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (OPEN-INPUT-FILE-OR-DIRECTORY (FILE PATHNAME)
        (LMFS-CHANGE-FILE-PROPERTIES FILE PLIST)))))
