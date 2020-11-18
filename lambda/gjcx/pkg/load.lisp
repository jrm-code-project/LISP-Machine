;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

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


(LET* ((P (SEND FS:FDEFINE-FILE-PATHNAME :TRANSLATED-PATHNAME))
       (H (SEND P :HOST))
       (W (SEND P :NEW-PATHNAME
                :NAME :WILD :TYPE :WILD :VERSION :WILD))
       (D (IF (ATOM (SEND P :DIRECTORY)) (LIST (SEND P :DIRECTORY)) (SEND P :DIRECTORY))))
  (FORMAT T "~&;; Logical host NETPKG on physical host ~S rooted at ~S~%"
          H D)
  (FS:SET-LOGICAL-PATHNAME-HOST "NETPKG"
                                :PHYSICAL-HOST H
                                :TRANSLATIONS `(("SOURCE;" ,W)
                                                ("*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD))))
                                                ("*;*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD :WILD)))))))



(SI:SET-SYSTEM-SOURCE-FILE "NETPKG"
                           "NETPKG:SOURCE;SYSDEF")

(LOAD "NETPKG:SOURCE;PKG" :SET-DEFAULT-PATHNAME NIL)
