
;System COLD-LOAD made by keith at 29-Oct-88 01:29:12  -*-Mode: Lisp; Package: User; Base: 10-*-

;;; Written 29-Oct-88 01:29:12 by keith (Keith Corbett) at site Gigamos Cambridge
;;; while running on Djinn from band 4
;;; with Experimental ZWEI 126.27, Experimental ZMail 74.13, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Experimental System 128.0, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 102, 10/27.

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QRAND LISP >"
  '((:COMPILE NIL
     (SI::MAYBE-CHANGE-FEF-TYPE NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with a call to LOGAND as a BYTE specifier (this may not be portable)"))
     (SI::FUNCTION-SPEC-DEFAULT-HANDLER NIL NIL
      (COMPILER::OBSOLETE :OBSOLETE NIL "~S ~A." WITH-STACK-LIST "is an obsolete form; most processor-dependent functions will go away in a future release")))))
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: SYS; QFASL LISP >"
  '((:COMPILE NIL
     (SI::ALLOCATE-FASL-TABLE NIL NIL
      (COMPILER::ART-Q-LIST-WARNING :NOT-PORTABLE NIL "~S called with :TYPE ART-Q-LIST (this may not be portable)" MAKE-ARRAY))
     (SYSTEM:FASL-OP-FIXED NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with a call to + as a BYTE specifier (this may not be portable)"))
     (SYSTEM:FASL-OP-CHARACTER NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "DPB called with a call to + as a BYTE specifier (this may not be portable)"))
     (SYSTEM:FEF-INSTRUCTION NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "%P-LDB-OFFSET called with a call to IF as a BYTE specifier (this may not be portable)"))
     (SYSTEM:FEF-LIMIT-PC NIL NIL
      (COMPILER::INVALID-BYTE-SPEC :NOT-PORTABLE NIL "%P-LDB-OFFSET called with a call to IF as a BYTE specifier (this may not be portable)")))))
