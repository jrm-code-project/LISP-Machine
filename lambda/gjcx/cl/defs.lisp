;;; -*- Mode:LISP; Package:CLC; Readtable:CL; Base:10 -*-

(DEFVAR *WARN-ON-ERRORS* T
  "T to enable the WARN-ON-ERRORS feature while compiling. NIL means get an error.")

(DEFVAR *COMPILER-ENV* NIL)


(DEFUN SYMBOLCONC (&REST L)
  (INTERN (APPLY #'STRING-APPEND L)))

(DEFMACRO DEF-CL (PROPNAME DOC)
  (LET ((DN (SYMBOLCONC "DEF-" PROPNAME)))
    `(DEFMACRO ,DN (SYMBOL ARGLIST &BODY BODY)
       ,DOC
       (*def-cl ',dn symbol ',propname arglist body))))

(defun *def-cl (dn symbol key arglist body)
  (LET ((S (SYMBOLCONC SYMBOL "_" KEY "_CL")))
    `(PROGN (DEFUN ,S ,ARGLIST ,@BODY)
            (*DEFCL ',dn ',SYMBOL ',KEY ',S))))


(DEFUN *DEFCL (dn SYMBOL KEY P)
  (WHEN (SI:RECORD-SOURCE-FILE-NAME SYMBOL dn)
    (putprop symbol p key)
    symbol))



(DEF-CL CL-TOPLEVEL-FORM "define a special handler for form a toplevel in a file")
