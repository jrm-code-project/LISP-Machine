;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10; Patch-File:T -*-

;;; FOR A LITTLE WHILE WRITE-PROPERTY-LIST WAS BROKEN.
;;; THIS KLUDGE TRIES TO READ THE PROPERTY LIST ANYWAY.
;;; EVEN GIVEN POSSIBLY INCORRECT LENGTH.

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


(DEFUN FIX-UP-AFTER-3-112-BUG ()
  (LABELS ((RECURSE (PATH)
                    (PRINT PATH)
                    (DO ((L (FS:DIRECTORY-LIST PATH) (CDR L))
                         (DIRS))
                        ((NULL L)
                         (MAPC #'RECURSE DIRS))
                      (COND ((NULL (CAAR L)))
                            ((GET (CAR L) :DIRECTORY)
                             (PUSH (SEND (SEND (CAAR L) :PATHNAME-AS-DIRECTORY) :NEW-PATHNAME
                                         :NAME :WILD :TYPE :WILD :VERSION :WILD)
                                   DIRS))))))
    (RECURSE (FS:PARSE-PATHNAME "LM:~;*.*#*")))
  (WRITE-CORE-FILE-SYSTEM-ONTO-DISK))

;;; USE DJ

(DEFUN GET-STRING (STREAM)
  (LET* ((LEN (SEND STREAM :TYI))
         (ARR (MAKE-ARRAY LEN :TYPE 'ART-STRING)))
    (SEND STREAM :STRING-IN NIL ARR 0 LEN)
    ARR))


(DEFUN READ-PROPERTY-LIST (STREAM &AUX LIST (PAK SI:PKG-KEYWORD-PACKAGE))
  (SETQ LIST (MAKE-LIST (* (PROGN (SEND STREAM :TYI)) 2)))
  (DO ((L LIST (CDDR L))
       (LEN-PEEK))
      ((NULL L))
    (SETQ LEN-PEEK (SEND STREAM :TYIPEEK))
    (WHEN (OR (NOT LEN-PEEK) (>= LEN-PEEK #\SP))
      ;; LENGTHS OF PROPERTIES ARE ALMOST ALWAYS LESS THAN 32.
      ;; OTHERWISE WE ARE PROBABLY UP AGAINST A NEW DIRECTORY ENTRY.
      (FORMAT T "~&FOUND A LOSING PLIST: ~S~%" LEN-PEEK)
      (RETURN (RPLACD L NIL)))
    (RPLACA L (INTERN (GET-STRING STREAM) PAK))
    (CASE (SEND STREAM :TYI)
      (0)
      (1 (SETF (CADR L) T))
      (2 (SETF (CADR L) (INTERN (GET-STRING STREAM) PAK)))
      (3 (SETF (CADR L) (INTERN (GET-STRING STREAM)
                                (PKG-FIND-PACKAGE (GET-STRING STREAM) :ASK))))
      (4 (SETF (CADR L) (GET-BYTES STREAM 3)))
      (5 (SETF (CADR L) (GET-STRING STREAM)))
      (6 (SETF (CADR L)
               (LET ((*READ-BASE* 10.)
                     (*PACKAGE* SI:PKG-USER-PACKAGE)
                     (*READTABLE* SI:INITIAL-READTABLE))
                   ;; This can lose pretty badly with #<'s, etc.  -- DLA
                   (CLI:READ STREAM))))
      (7 (LET* ((*READ-BASE* 10.)
                (*PACKAGE* SI:PKG-USER-PACKAGE)
                (*READTABLE* SI:INITIAL-READTABLE)
                (FORM (CLI:READ STREAM))
                (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))       ;<-*
           (SETF (CADR L)
                 (IF (= (LENGTH FORM) 6)
                     (APPLY #'FS:MAKE-FASLOAD-PATHNAME FORM)
                   (EVAL FORM)))))              ;Obsolete form for pathnames to be written.
      (OTHERWISE (FERROR NIL "Invalid Plist property designator."))))
  (LET ((N (LENGTH LIST)))
    (COND ((NOT (ODDP N)))
          ((= N 1)
           (SETQ LIST NIL))
          ('ELSE
           (SETF (NTHCDR (1- N) LIST) NIL))))
  LIST)
