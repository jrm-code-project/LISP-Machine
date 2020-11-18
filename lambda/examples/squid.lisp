#| -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

 Copyright LISP Machine, Inc. 1985, 1986
   See filename "Copyright" for
 licensing and release information.

SQUID - Self QUoting Internal Datum (Name from PDP-10 Maclisp) -GJC

In the interpreter, this form will eval itself and the result will
remain quoted forever after.

In the compiler, this says that the form is to be evaluated at load
time and that its result is to be used in all computations.

A few people have asked how to get load-time evaluations to happen, so
here it is. (This is used in Object-Lisp(TM) for example).

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

|#

#+MACLISP
(DEFMACRO SQUID (&WHOLE FORM)
  (DECLARE (SPECIAL SQUID COMPILER-STATE))
  (COND ((MEMQ COMPILER-STATE '(NIL TOPLEVEL))
         (DISPLACE FORM `',(EVAL (CADR FORM))))
        (T
         `(,SQUID ,(CADR FORM)))))


#+(OR LMI TI)
(PROGN 'COMPILE

(DEFVAR *SQUID-TABLE* ())

(DEFUN SQUID (&QUOTE X)
  (LET ((CELL (ASSQ X *SQUID-TABLE*)))
    (IF (NULL CELL)
        (CDAR (SETQ *SQUID-TABLE* (CONS (CONS X (EVAL X)) *SQUID-TABLE*)))
      (CDR CELL))))

(DEFUN SQUID-OPTIMIZER (FORM)
  (IF (EQ compiler:qc-tf-output-mode 'COMPILER:COMPILE-TO-CORE)
      (LIST 'QUOTE (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
                     (EVAL (CADR FORM))))
    ;; COMPILER:EVAL-AT-LOAD-TIME-MARKER
    ;; is used in the implementation of "#,"
    (LIST 'QUOTE (CONS COMPILER:EVAL-AT-LOAD-TIME-MARKER (CADR FORM)))))

(DEFPROP SQUID (SQUID-OPTIMIZER) COMPILER:OPTIMIZERS)

)
