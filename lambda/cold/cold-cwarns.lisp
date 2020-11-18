
;System COLD made by keith at 4-Nov-88 20:05:25  -*-Mode: Lisp; Package: User; Base: 10-*-

;;; Written 4-Nov-88 20:05:25 by keith at site Gigamos Cambridge
;;; while running on Djinn from band 1
;;; with Experimental ZWEI 126.28, Experimental ZMail 74.14, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Experimental System 129.0, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, 11/04.

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: COLD; COLDUT LISP >"
  '((:COMPILE NIL
     (COLD::VMEM-FIND-PAGE NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "LDB called with the numeric constant 2020 as a BYTE specifier (this is not portable)"))
     (COLD::VREAD NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with the numeric constant 2020 as a BYTE specifier (this is not portable)"))
     (COLD::VWRITE NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "LDB called with the numeric constant 20 as a BYTE specifier (this is not portable)"))
     (COLD::VSTORE-CONTENTS NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "LDB called with the numeric constant 20 as a BYTE specifier (this is not portable)")
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DEPOSIT-FIELD called with a call to - as a BYTE specifier (this may not be portable)"))
     (COLD::VSTORE-CDR-CODE NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with a call to - as a BYTE specifier (this may not be portable)"))
     ((:SPECIAL-FORM COLD::DEFMIC) NIL NIL
      (COMPILER::FUNCTION-NOT-VALID :OBSOLETE NIL "(DEFUN ~S ...) has &QUOTE in argument list. New special forms should be macros" COLD::DEFMIC)
      (SI::PREMATURE-WARNING-MARKER NIL NIL "The problems described above were in data preceding the definition of ~S." (:SPECIAL-FORM COLD::DEFMIC)))
     (COLD::STORE-HALFWORD NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with the numeric constant 2020 as a BYTE specifier (this is not portable)"))
     (COLD::STORE-EXTENDED-NUMBER NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "%P-LDB-OFFSET called with the numeric constant 20 as a BYTE specifier (this is not portable)"))
     (COLD::STORE-COMPLEX-OR-RATIONAL NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "%P-LDB called with the numeric constant 20 as a BYTE specifier (this is not portable)"))
     (COLD::INIT-ADDRESS-SPACE-MAP NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with the numeric constant 2020 as a BYTE specifier (this is not portable)"))
     ((:INTERNAL COLD::MAKE-COLD 0) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." SI::COLD-LOAD-FILE-LIST))
     ((:INTERNAL COLD::MAKE-COLD-1 2) NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" COLD::COLD-FASLOAD)))))
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: COLD; COLDLD LISP >"
  '((:COMPILE NIL
     (COLD::M-FASL-OP-FIXED NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with a call to + as a BYTE specifier (this may not be portable)"))
     (COLD::M-FASL-OP-CHARACTER NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with a call to + as a BYTE specifier (this may not be portable)"))
     (COLD::M-FASL-OP-FLOAT-FLOAT NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "%P-DPB-OFFSET called with the numeric constant 1013 as a BYTE specifier (this is not portable)")
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "LDB called with the numeric constant 1010 as a BYTE specifier (this is not portable)")
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "%LOGDPB called with the numeric constant 2010 as a BYTE specifier (this is not portable)"))
     (COLD::Q-FASL-OP-SMALL-FLOAT NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "%LOGDPB called with the numeric constant 2010 as a BYTE specifier (this is not portable)"))
     (COLD::Q-FASL-OP-INITIALIZE-NUMERIC-ARRAY NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with the numeric constant 2020 as a BYTE specifier (this is not portable)"))
     (COLD::Q-FASL-CONVERT-FEF-TO-FAST-FEF-IF-POSSIBLE NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with a call to LOGAND as a BYTE specifier (this may not be portable)"))
     (COLD::VSETQ-IF-UNBOUND NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "LDB called with a call to - as a BYTE specifier (this may not be portable)")))))
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: COLD; MINI-SERVER LISP >"
  '((:COMPILE NIL)))
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; SYSDCL LISP >"
  '((:COMPILE NIL)))
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; INNER-SYSTEM-FILE-ALIST LISP >"
  '((:COMPILE NIL)))
