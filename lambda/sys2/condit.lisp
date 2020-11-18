;;; This stuff copied from SYS2;LMMAC.  -*-Mode:LISP; Base:8 -*-

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file is pointed to by the Lisp Machine manual.  The idea is that
;;; you load this file into a Maclisp compiler to get these macros in
;;; place so that you can write conditional code.

;The IF-IN-MACLISP/IF-IN-LISPM conditionals have to do with not breaking
;the Maclisp environment when compiling.  The optimizers in COMPAT take
;over these functions when compiling in Maclisp.

(DECLARE (SETQ INHIBIT-STYLE-WARNINGS-SWITCH T)
         (SPECIAL COMPILING-FOR-LISPM))

;THESE ARE CONDITIONAL ON WHICH SYSTEM IS EXECUTING THEM.
(DEFMACRO IF-IN-MACLISP (&REST FORMS)
    (COND ((NOT (STATUS FEATURE LISPM))
           `(PROGN 'COMPILE . ,FORMS))))

(DEFMACRO IF-IN-LISPM (&REST FORMS)
    (COND ((STATUS FEATURE LISPM)
           `(PROGN 'COMPILE . ,FORMS))))

;THESE ARE CONDITIONAL ON WHICH SYSTEM RESULT IS INTENDED "FOR ".
; THIS IS THE SAME AS WHICH SYSTEM IS "IN" EXCEPT IN THE CASE
; COMPILING IN MACLISP FOR LISPM (IE QCMP, AFTER COMPILER ITSELF HAS
; BEEN LOADED).  THE COMPILING-FOR-LISPM SWITCH IS SET BY .LISP. (INIT)
; AFTER QCMP HAS BEEN LOADED.

(DEFMACRO IF-FOR-MACLISP (&REST FORMS)
    (COND ((AND (NOT (STATUS FEATURE LISPM))            ;IN MACLISP
                (OR (NOT (BOUNDP 'COMPILING-FOR-LISPM))
                    (NULL COMPILING-FOR-LISPM)))
           `(PROGN 'COMPILE . ,FORMS))))

(DEFMACRO IF-FOR-LISPM (&REST FORMS)
    (COND ((OR (STATUS FEATURE LISPM)
               (AND (BOUNDP 'COMPILING-FOR-LISPM)
                    COMPILING-FOR-LISPM))
           `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL))
                          (PROGN 'COMPILE . ,FORMS)))))

(DEFMACRO IF-FOR-MACLISP-ELSE-LISPM (MACLISP-FORM LISPM-FORM)
    (COND ((NOT (STATUS FEATURE LISPM))
           (COND ((OR (NOT (BOUNDP 'COMPILING-FOR-LISPM))       ;QCMP DEFINES THIS TO T
                      (NULL COMPILING-FOR-LISPM))
                  MACLISP-FORM)
                 (T `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL)) ,LISPM-FORM))))
    ;COMPLR DOESNT KNOW (OR CARE) ABOUT COMPILER-LET.
          (T LISPM-FORM)))
