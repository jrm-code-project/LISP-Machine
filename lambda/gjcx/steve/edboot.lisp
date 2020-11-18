; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;Functions to compile and load the editor.
;
;Editor top level.
;

(defparameter *editor-files*
  '("nil$disk:[nil.steve]xedboot"
    ;;Definition of variables.
    "nil$disk:[nil.steve]vars"
    ;;The "kernel"
    "nil$disk:[nil.steve]em"
    "nil$disk:[nil.steve]newmacros" ; Recently added macros used by edkeys
    "nil$disk:[nil.steve]aux"
    ;;Overwrite streams. These supercede the overwrite-FOO functions in AUX.LSP
    "nil$disk:[nil.steve]over"
    ;;The top-level.
    "nil$disk:[nil.steve]ed"
    "nil$disk:[nil.steve]prll"
    ;;The redisplay.
    "nil$disk:[nil.steve]redis"
    ;;Key binding mechanism.
    "nil$disk:[nil.steve]hashish"
    ;;Major modes mechanism.
    "nil$disk:[nil.steve]mode"
    "nil$disk:[nil.steve]hash"
    ;; To avoid problems with ordering, all computation
    ;; to set up the binding tables is done in INITBINDS.
    "nil$disk:[nil.steve]initbinds"
    ;;Meta-x command reader and cruft.
    "nil$disk:[nil.steve]xscan" ;Older/alternate versions are OSCAN, SCAN.
    "nil$disk:[nil.steve]metax"
    ;;Major mode definitions.
    "nil$disk:[nil.steve]modes"
    ;;Self-insert and echoin.
    "nil$disk:[nil.steve]echo"
    ;;File i/o.
    "nil$disk:[nil.steve]edio"
    ;;
    ;;The editor might work at this point in loading.
    ;;
    ;;Recursive editing support.
    "nil$disk:[nil.steve]recurse"
    ;;Parsing of S-expressions.
    "nil$disk:[nil.steve]sexp"
    ;;Argument reading commands.
    "nil$disk:[nil.steve]args"
    ;;Most of the vanilla key bindings.
    "nil$disk:[nil.steve]edkeys"
    ;;Keyboard macros.
    "nil$disk:[nil.steve]keymac"
    ;;Comment manipulation. (And paragraphs?)
    "nil$disk:[nil.steve]comment"
    ;;Killing and unkilling.
    "nil$disk:[nil.steve]kills"
    ;;Indent for lisp (tab)
    "nil$disk:[nil.steve]indent"
    "nil$disk:[nil.steve]indents"
    ;;Definitions of Meta-X commands.
    "nil$disk:[nil.steve]mxcom"
    ;; M-X EDIT-OPTIONS
    "nil$disk:[nil.steve]editopt"
    ;;The start of some fancy support for writing fancy editor extentions.
    ;;Used by SYN.LSP below.
    "nil$disk:[nil.steve]hashini"
    ;;M-X syntax-modification. An easy way to change the syntax of characters.
    "nil$disk:[nil.steve]syn"
    ;; Autofill minor mode.
    "nil$disk:[nil.steve]autofill"
    ;;Window streams for the mode area.
    "nil$disk:[nil.steve]dow"
    ;;Search primitives and I-Search.
    "nil$disk:[nil.steve]search"
    ;;search all buffers for string => Meta-,
    "nil$disk:[nil.steve]xsearch"
    ;;Meta-.
    "nil$disk:[nil.steve]defsrch"
    ;;Help functions and info commands.
    "nil$disk:[nil.steve]help"
    ;;Directory editor.
    "nil$disk:[nil.steve]dired"
    ;;Send mail.
    "nil$disk:[nil.steve]sm"))


(defun make-inp-file (&optional
                      (name "NIL$DISK:[NIL.DUMP]EDITOR.INP"))
 (with-open-file (s name 'out)
  (loop for file in (cons "nil$disk:[nil.steve]steve" *editor-files*)
        do (format s "~a~%" file))
  (format s "nil$disk:[nil.vas]pkgusr~%")
  s))
