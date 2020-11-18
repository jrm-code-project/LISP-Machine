;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

;;; *****************************************
;;; ** (C) COPYRIGHT 1986 LISP MACHINE INC **
;;; **  Also see file named Copyright.Text **
;;; *****************************************



(OR (FBOUNDP'MAP-ALL-FILES) (LOAD "SYS:EXAMPLES;FILE-UTILS" :SET-DEFAULT-PATHNAME NIL))

(putprop (si:parse-host "DJ")
         '(

           BOBP
           BUGS
           DG
           GJC GJCX GLD
           JRM
           KHS
           L
           MFILE
           MZWEI
           MUSIC
           PACE
           RELEASE
           RG
           RPK)
         'DIRECTORIES-TO-BACKUP)


(DEFUN BACKUP-DJ ()
  (BACKUP-HOST-TO-HERE 'DJ))


(DEFUN COPY-RELEASE-3 ()
  (COPY-TO-NEW-ROOT "DJ:L;" "LAM3:RELEASE-3;" :STRIP-ROOT 1
                    :FILTER-DIRECTORIES
                    '(;; DIRECTORIES THAT ARE ETHER
                      ;; AS YET UNRELEASED OR "DEAD"
                      ("L" "CDI-SITE") ; RANDOM
                      ("L" "CUSTOMER-SITE") ; WANT TO REWORK THIS FOR RELEASE
                      ("L" "DOC") ; RANDOM
                      ("L" "FILE2") ; MAYBE EMPTY EVEN.
                      ("L" "GATEWAY") ; UNSTABLE
                      ("L" "IMICRO")  ; MAYBE LATER
                      ("L" "LM-PROLOG") ; NOT COMPLETED YET.
                      ("L" "NEW-TAPE")  ; OBSOLETE
                      ("L" "OBJECTLISP") ; WHICH ONE?
                      ("L" "OBLISP")     ; WHICH ONE?
                      ("L" "VISTA")      ; STATUS?
                      ("L" "TAPE" "OLD") ; obsolete
                      )))



(DEFUN COPY-FROM-DJ-TO-LAM3 (NAME &REST DIR)
  (FS:COPY-DIRECTORY (FORMAT NIL "DJ:L.~{~A~^.~};~A.LISP#>" DIR NAME)
                     (FORMAT NIL "LAM3:RELEASE-3.~{~A~^.~};~A.LISP#*" DIR NAME)
                     :COPY-ONLY :NEWEST))


