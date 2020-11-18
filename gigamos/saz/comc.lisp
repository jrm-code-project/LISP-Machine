;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:ZL -*-

(DEFUN COMPILE-DEFUN-INTERNAL (COMPILE-P MODE-NAME ECHO-NAME
                               &OPTIONAL USE-TYPEOUT DEFVAR-HACK
                               (COMPILER-PROCESSING-MODE
                                 '(:MODE COMPILER:MACRO-COMPILE))
                               (*target-computer* 'compiler:lambda-interface)
                               &AUX BP1 BP2 DEFUN-NAME)
  "Compile or evaluate a part of the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
If there is a region, it is used; otherwise the current or following defun is used.
USE-TYPEOUT is passed to COMPILE-PRINT-INTERVAL and controls where information is printed.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowercase past participle and period (/"compiled./")."
  (COND ((WINDOW-MARK-P *WINDOW*)
         (SETQ BP1 (MARK) BP2 (POINT))
         (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
         (SETQ DEFUN-NAME "region"))
        ((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
         (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
         ;the following setq fixes bug in which compile-print-interval
         ;was making up random strings for printing in *query-io*...
         (setq defun-name
               (string
                 (bp-read-object
                   (if (char-equal (bp-char bp1)        ;first char of defun-interval a "(" ?
                                   #/()
                       (forward-word bp1 1)
                     (forward-word (beg-line bp1 1) 1)))))      ;if not, assume it's an introductory package prefix.
         (SETQ DEFVAR-HACK T))
        (T
         (BARF "Cannot find a defun near point.")))
  (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P
                          DEFUN-NAME MODE-NAME ECHO-NAME USE-TYPEOUT DEFVAR-HACK
                          COMPILER-PROCESSING-MODE
                          nil           ;already-resectionized-flag
                          *target-computer*))
