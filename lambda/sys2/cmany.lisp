-*- Mode:LISP; Package:COMPILER; Base:8 -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; Compile many driven off a LOAD-FILE-LIST assoc-list

;Compile all the files in the alist LST which have been edited since last compiled.
(DEFUN COMPILE-FILE-ALIST (&OPTIONAL (ALIST SYSTEM-FILE-ALIST)
                                    (DONT-ASK-P 0)
                                    (DONT-CARE-IF-UNCHANGED-P 0)
                                    DONT-ASK-FOR-CONFIRMATION
                                    PACKAGE-SPEC)
  (COMPILE-FILE-ALIST-MAP (FUNCTION FUNCALL)
                          ALIST
                          DONT-ASK-P
                          DONT-CARE-IF-UNCHANGED-P
                          DONT-ASK-FOR-CONFIRMATION
                          PACKAGE-SPEC))

;Like COMPILE-FILE-ALIST, but takes a functional arg which is called
; instead of compiling or loading anything.  Functional arg is called with
; args of function and args that would have been called.
;  ie QC-FILE FN NIL NIL NIL PACKAGE-SPEC.
(DEFUN COMPILE-FILE-ALIST-MAP (FCTN ALIST &OPTIONAL (DONT-ASK-P 0)
                                    (DONT-CARE-IF-UNCHANGED-P 0)
                                    DONT-ASK-FOR-CONFIRMATION
                                    PACKAGE-SPEC
                               &AUX COMPILE-LIST QFASL-DATE SOURCE-DATE
                                    FILE-NAME SOURCE-NAME)
  (COND ((NUMBERP DONT-ASK-P)                           ;If not specified,
         (SETQ DONT-ASK-P (NOT (Y-OR-N-P "Should I ask you about each file? ")))))
  (COND ((NUMBERP DONT-CARE-IF-UNCHANGED-P)
         (SETQ DONT-CARE-IF-UNCHANGED-P
               (Y-OR-N-P "Should I compile even if the file is unchanged? "))))
  (DO L ALIST (CDR L) (NULL L)
      ;; Look at each source file's date and compare it with the QFASL file's date.
      ;; If the source is newer, or if DONT-CARE-IF-UNCHANGED-P is T,
      ;; the file should be compiled.  If DONT-ASK-P is NIL, we ask
      ;; about each of those files.
      (SETQ FILE-NAME (FS:MERGE-PATHNAME-DEFAULTS (CAAR L) FS:LOAD-PATHNAME-DEFAULTS)
            SOURCE-NAME NIL)
      (COND ((CMANY-QFASL-P FILE-NAME)                  ;If it's compiled
             (COND ((NOT DONT-CARE-IF-UNCHANGED-P)
                    (SETQ SOURCE-DATE
                          (FS:FILE-GET-CREATION-DATE
                           (SETQ SOURCE-NAME (FUNCALL FILE-NAME ':COPY-WITH-TYPE "LISP"))
                           T))
                    (SETQ QFASL-DATE (FS:FILE-GET-CREATION-DATE FILE-NAME NIL))))
             (COND ((OR DONT-CARE-IF-UNCHANGED-P        ;If either OK if already loaded,
                        (NULL QFASL-DATE)               ;or no QFASL file exists now,
                        (ALPHALESSP QFASL-DATE SOURCE-DATE))    ;or if QFASL is old,
                    (OR SOURCE-NAME
                        (SETQ SOURCE-NAME (FUNCALL FILE-NAME ':COPY-WITH-TYPE "LISP")))
                    (AND (COND ((NOT DONT-ASK-P)        ;then ask if necessary
                                (Y-OR-N-P (FORMAT NIL "Compile file ~A? " SOURCE-NAME)))
                               (T T))
                         ;; This file to be compiled
                         (PUSH SOURCE-NAME COMPILE-LIST)))))))
  ;; Now we know which files to compile.  Get confirmation and then load them.
  (COND ((NOT (NULL COMPILE-LIST))
         (SETQ COMPILE-LIST (NREVERSE COMPILE-LIST))
         (FORMAT *QUERY-IO* "~2%Files to be compiled:~%")
         (DOLIST (L COMPILE-LIST)
           (FORMAT *QUERY-IO* "~A~%" L))
         (COND ((OR DONT-ASK-FOR-CONFIRMATION
                    (Y-OR-N-P (IF (= (LENGTH COMPILE-LIST) 1) "Compile it?"
                                  "Compile them?")))
                (TERPRI)
                (DOLIST (L COMPILE-LIST)
                  (FORMAT *STANDARD-OUTPUT* "~%Compiling ~A" L)
                  (FUNCALL FCTN 'QC-FILE L NIL NIL NIL PACKAGE-SPEC))
                T))))
  NIL)

(DEFUN CMANY-QFASL-P (FILE)
  (STRING-EQUAL (FUNCALL FILE ':TYPE) "QFASL"))
