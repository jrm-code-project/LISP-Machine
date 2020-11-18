;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-
;;;
;;; WARM-BOOT.LISP


(defconstant lisp-symbols
             '(
               VINC::ATOM
               VINC::COMPLEXP
               VINC::ARRAYP
               VINC::COMPILED-FUNCTION-P
               VINC::CONSP
               VINC::INTEGERP
               VINC::RATIONALP
               VINC::FLOATP
               VINC::NUMBERP
               VINC::LISTP
               VINC::SYMBOLP
               VINC::COMMONP

               CONS::CAAAAR
               CONS::CAAADR
               CONS::CAAAR
               CONS::CAADAR
               CONS::CAADDR
               CONS::CAADR
               CONS::CAAR
               CONS::CADAAR
               CONS::CADADR
               CONS::CADAR
               CONS::CADDAR
               CONS::CADDDR
               CONS::CADDR
               CONS::CADR
               CONS::CAR
               CONS::CDAAAR
               CONS::CDAADR
               CONS::CDAAR
               CONS::CDADAR
               CONS::CDADDR
               CONS::CDADR
               CONS::CDAR
               CONS::CDDAAR
               CONS::CDDADR
               CONS::CDDAR
               CONS::CDDDAR
               CONS::CDDDDR
               CONS::CDDDR
               CONS::CDDR
               CONS::CDR
               CONS::CONS
               CONS::ENDP
               CONS::RPLACA
               CONS::RPLACD

               ARRAY::ADJUSTABLE-ARRAY-P
               ARRAY::AREF
               ARRAY::ARRAY-DIMENSION
               ARRAY::ARRAY-DIMENSION-LIMIT
               ARRAY::ARRAY-DIMENSIONS
               ARRAY::ARRAY-ELEMENT-TYPE
               ARRAY::ARRAY-HAS-FILL-POINTER-P
               ARRAY::ARRAY-IN-BOUNDS-P
               ARRAY::ARRAY-RANK
               ARRAY::ARRAY-RANK-LIMIT
               ARRAY::ARRAY-ROW-MAJOR-INDEX
               ARRAY::ARRAY-TOTAL-SIZE
               ARRAY::ARRAY-TOTAL-SIZE-LIMIT
               ARRAY::ASET
               ARRAY::BIT-VECTOR-P
               ARRAY::FILL-POINTER
               ARRAY::LENGTH
               ARRAY::MAKE-ARRAY
               ARRAY::MAKE-STRING
               ARRAY::SIMPLE-BIT-VECTOR-P
               ARRAY::SIMPLE-STRING-P
               ARRAY::SIMPLE-VECTOR-P
               ARRAY::STRING=
               ARRAY::STRINGP
               ARRAY::SVREF
               ARRAY::VECTOR-POP
               ARRAY::VECTOR-PUSH
               ARRAY::VECTORP
               ARRAY::VECTOR

               SYMBOL::BOUNDP
               SYMBOL::FBOUNDP
               SYMBOL::GET
               SYMBOL::GET-PROPERTIES
               SYMBOL::GETF
               SYMBOL::REMF
               SYMBOL::REMPROP
               SYMBOL::SET
               SYMBOL::SYMBOL-FUNCTION
               SYMBOL::SYMBOL-NAME
               SYMBOL::SYMBOL-PACKAGE
               SYMBOL::SYMBOL-PLIST
               SYMBOL::SYMBOL-VALUE

               LISP-INTERNALS::ZEROP
               LISP-INTERNALS::PLUSP
               LISP-INTERNALS::MINUSP
               LISP-INTERNALS::EVENP
               LISP-INTERNALS::ODDP
               LISP-INTERNALS::=
               LISP-INTERNALS::<
               LISP-INTERNALS::>
               LISP-INTERNALS::<=
               LISP-INTERNALS::>=
               LISP-INTERNALS::/=
               LISP-INTERNALS::MAX
               LISP-INTERNALS::MIN
               LISP-INTERNALS::ABS
               LISP-INTERNALS::+

               LISP-INTERNALS::*
               LISP-INTERNALS::-
               LISP-INTERNALS::/
               LISP-INTERNALS::1+
               LISP-INTERNALS::1-
               LISP-INTERNALS::LOGIOR
               LISP-INTERNALS::LOGXOR
               LISP-INTERNALS::LOGAND
               LISP-INTERNALS::LOGEQV
               LISP-INTERNALS::LOGNAND
               LISP-INTERNALS::LOGNOR
               LISP-INTERNALS::LOGANDC1
               LISP-INTERNALS::LOGANDC2
               LISP-INTERNALS::LOGORC1
               LISP-INTERNALS::LOGORC2
               LISP-INTERNALS::BOOLE-CLR
               LISP-INTERNALS::BOOLE-SET
               LISP-INTERNALS::BOOLE-1
               LISP-INTERNALS::BOOLE-2
               LISP-INTERNALS::BOOLE-C1
               LISP-INTERNALS::BOOLE-C2
               LISP-INTERNALS::BOOLE-AND
               LISP-INTERNALS::BOOLE-IOR
               LISP-INTERNALS::BOOLE-XOREQV
               LISP-INTERNALS::BOOLE-NAND
               LISP-INTERNALS::BOOLE-NOR
               LISP-INTERNALS::BOOLE-ANDC1
               LISP-INTERNALS::BOOLE-ANDC2
               LISP-INTERNALS::BOOLE-ORC1
               LISP-INTERNALS::BOOLE-ORC2
               LISP-INTERNALS::BOOLE
               LISP-INTERNALS::DPB
               LISP-INTERNALS::LDB
               LISP-INTERNALS::ASH
               LISP-INTERNALS::INTEGER-LENGTH
               LISP-INTERNALS::LOGCOUNT
               LISP-INTERNALS::LOGBITP
               LISP-INTERNALS::BYTE
               LISP-INTERNALS::BYTE-SIZE
               LISP-INTERNALS::BYTE-POSITION
               LISP-INTERNALS::LDB-TEST
               LISP-INTERNALS::MASK-FIELD
               LISP-INTERNALS::DEPOSIT-FIELD
               LISP-INTERNALS::MOST-POSITIVE-FIXNUM
               LISP-INTERNALS::MOST-NEGATIVE-FIXNUM

               LISP-INTERNALS::DEFSTRUCT

               LISP-INTERNALS::CHAR-CODE-LIMIT
               LISP-INTERNALS::CHAR-FONT-LIMIT
               LISP-INTERNALS::CHAR-BITS-LIMIT
               LISP-INTERNALS::CHAR-CONTROL-BIT
               LISP-INTERNALS::CHAR-META-BIT
               LISP-INTERNALS::CHAR-SUPER-BIT
               LISP-INTERNALS::CHAR-HYPER-BIT
               LISP-INTERNALS::CHAR-CODE
               LISP-INTERNALS::CHAR-BITS
               LISP-INTERNALS::CHAR-FONT
               LISP-INTERNALS::CHAR-INT
               LISP-INTERNALS::INT-CHAR
               LISP-INTERNALS::CHARACTERP
               LISP-INTERNALS::STRING-CHAR-P
               LISP-INTERNALS::GRAPHIC-CHAR-P
               LISP-INTERNALS::STANDARD-CHAR-P
               LISP-INTERNALS::UPPER-CASE-P
               LISP-INTERNALS::LOWER-CASE-P
               LISP-INTERNALS::ALPHA-CHAR-P
               LISP-INTERNALS::BOTH-CASE-P
               LISP-INTERNALS::DIGIT-CHAR-P
               LISP-INTERNALS::DIGIT-CHAR
               LISP-INTERNALS::ALPHANUMERICP
               LISP-INTERNALS::CHAR-UPCASE
               LISP-INTERNALS::CHAR-DOWNCASE
               LISP-INTERNALS::CHAR-BIT
               LISP-INTERNALS::SET-CHAR-BIT
               LISP-INTERNALS::CHAR<
               LISP-INTERNALS::CHAR>
               LISP-INTERNALS::CHAR=
               LISP-INTERNALS::CHAR/=
               LISP-INTERNALS::CHAR>=
               LISP-INTERNALS::CHAR<=
               LISP-INTERNALS::CHAR-EQUAL
               LISP-INTERNALS::CHAR-LESSP
               LISP-INTERNALS::CHAR-GREATERP
               LISP-INTERNALS::CHAR-NOT-LESSP
               LISP-INTERNALS::CHAR-NOT-GREATERP
               LISP-INTERNALS::CHAR-NOT-EQUAL
               LISP-INTERNALS::CODE-CHAR
               LISP-INTERNALS::CHARACTER
               LISP-INTERNALS::MAKE-CHAR
               LISP-INTERNALS::CHAR-NAME

               LISP-INTERNALS::MAKE-STRING
               LISP-INTERNALS::CHAR
               LISP-INTERNALS::SCHAR
               LISP-INTERNALS::STRING=
               LISP-INTERNALS::STRING/=
               LISP-INTERNALS::STRING<
               LISP-INTERNALS::STRING<=
               LISP-INTERNALS::STRING>
               LISP-INTERNALS::STRING>=
               LISP-INTERNALS::STRING-EQUAL
               LISP-INTERNALS::STRING-LESSP
               LISP-INTERNALS::STRING-NOT-EQUAL
               LISP-INTERNALS::STRING-NOT-LESSP
               LISP-INTERNALS::STRING-NOT-GREATERP
               LISP-INTERNALS::STRING-GREATERP
               LISP-INTERNALS::STRING-UPCASE
               LISP-INTERNALS::STRING-DOWNCASE
               LISP-INTERNALS::NSTRING-UPCASE
               LISP-INTERNALS::NSTRING-DOWNCASE
               LISP-INTERNALS::STRING-TRIM
               LISP-INTERNALS::STRING-TRIM-LEFT
               LISP-INTERNALS::STRING-TRIM-RIGHT
               LISP-INTERNALS::STRING-CAPITALIZE
               LISP-INTERNALS::NSTRING-CAPITALIZE

               LISP-INTERNALS::LIST-LENGTH
               LISP-INTERNALS::FIRST
               LISP-INTERNALS::SECOND
               LISP-INTERNALS::THIRD
               LISP-INTERNALS::FOURTH
               LISP-INTERNALS::FIFTH
               LISP-INTERNALS::SIXTH
               LISP-INTERNALS::SEVENTH
               LISP-INTERNALS::EIGHTH
               LISP-INTERNALS::NINTH
               LISP-INTERNALS::REST
               LISP-INTERNALS::LAST
               LISP-INTERNALS::NTH
               LISP-INTERNALS::NTHCDR
               LISP-INTERNALS::MAKE-LIST
               LISP-INTERNALS::LIST
               LISP-INTERNALS::LIST*
               LISP-INTERNALS::ACONS
               LISP-INTERNALS::COPY-LIST
               LISP-INTERNALS::COPY-ALIST
               LISP-INTERNALS::COPY-TREE
               LISP-INTERNALS::LIST-REVERSE
               LISP-INTERNALS::LIST-NREVERSE
               LISP-INTERNALS::APPEND
               LISP-INTERNALS::REVAPPEND
               LISP-INTERNALS::NCONC
               LISP-INTERNALS::NRECONC
               LISP-INTERNALS::BUTLAST
               LISP-INTERNALS::NBUTLAST
               LISP-INTERNALS::LDIFF
               LISP-INTERNALS::TAILP
               LISP-INTERNALS::PAIRLIS

               LISP-INTERNALS::TREE-EQUAL
               LISP-INTERNALS::SUBST
               LISP-INTERNALS::SUBST-IF
               LISP-INTERNALS::SUBST-IF-NOT
               LISP-INTERNALS::NSUBST
               LISP-INTERNALS::NSUBST-IF
               LISP-INTERNALS::NSUBST-IF-NOT
               LISP-INTERNALS::SUBLIS
               LISP-INTERNALS::NSUBLIS
               LISP-INTERNALS::MEMBER
               LISP-INTERNALS::MEMBER-IF
               LISP-INTERNALS::MEMBER-IF-NOT
               LISP-INTERNALS::ASSOC
               LISP-INTERNALS::ASSOC-IF
               LISP-INTERNALS::ASSOC-IF-NOT
               LISP-INTERNALS::RASSOC
               LISP-INTERNALS::RASSOC-IF
               LISP-INTERNALS::RASSOC-IF-NOT
               LISP-INTERNALS::ADJOIN
               LISP-INTERNALS::PUSHNEW
               LISP-INTERNALS::UNION
               LISP-INTERNALS::NUNION
               LISP-INTERNALS::SET-INTERSECTION
               LISP-INTERNALS::NSET-INTERSECTION
               LISP-INTERNALS::SUBSETP
               LISP-INTERNALS::SET-DIFFERENCE
               LISP-INTERNALS::NSET-DIFFERENCE
               LISP-INTERNALS::SET-EXCLUSIVE-OR
               LISP-INTERNALS::NSET-EXCLUSIVE-OR

               LISP-INTERNALS::CONCATENATE
               LISP-INTERNALS::COPY-SEQ
               LISP-INTERNALS::COUNT
               LISP-INTERNALS::COUNT-IF
               LISP-INTERNALS::COUNT-IF-NOT
               LISP-INTERNALS::DELETE
               LISP-INTERNALS::DELETE-DUPLICATES
               LISP-INTERNALS::DELETE-IF
               LISP-INTERNALS::DELETE-IF-NOT
               LISP-INTERNALS::ELT
               LISP-INTERNALS::EVERY
               LISP-INTERNALS::FILL
               LISP-INTERNALS::FIND
               LISP-INTERNALS::FIND-IF
               LISP-INTERNALS::FIND-IF-NOT
               LISP-INTERNALS::LENGTH
               LISP-INTERNALS::MAKE-SEQUENCE
               LISP-INTERNALS::MAP
               LISP-INTERNALS::MERGE
               LISP-INTERNALS::MISMATCH
               LISP-INTERNALS::NOTANY
               LISP-INTERNALS::NOTEVERY
               LISP-INTERNALS::NREVERSE
               LISP-INTERNALS::NSUBSTITUTE
               LISP-INTERNALS::NSUBSTITUTE-IF
               LISP-INTERNALS::NSUBSTITUTE-IF-NOT
               LISP-INTERNALS::POSITION
               LISP-INTERNALS::POSITION-IF
               LISP-INTERNALS::POSITION-IF-NOT
               LISP-INTERNALS::REDUCE
               LISP-INTERNALS::REMOVE
               LISP-INTERNALS::REMOVE-DUPLICATES
               LISP-INTERNALS::REMOVE-IF
               LISP-INTERNALS::REMOVE-IF-NOT
               LISP-INTERNALS::REPLACE
               LISP-INTERNALS::REVERSE
               LISP-INTERNALS::SEARCH
               LISP-INTERNALS::SOME
               LISP-INTERNALS::SORT
               LISP-INTERNALS::STABLE-SORT
               LISP-INTERNALS::SUBSEQ
               LISP-INTERNALS::SUBSTITUTE
               LISP-INTERNALS::SUBSTITUTE-IF
               LISP-INTERNALS::SUBSTITUTE-IF-NOT

               LISP-INTERNALS::EQ
               LISP-INTERNALS::EQL
               LISP-INTERNALS::EQUAL
               LISP-INTERNALS::EQUALP

               LISP-INTERNALS::CLRHASH
               LISP-INTERNALS::GETHASH
               LISP-INTERNALS::HASH-TABLE-P
               LISP-INTERNALS::MAKE-HASH-TABLE
               LISP-INTERNALS::MAPHASH
               LISP-INTERNALS::REMHASH
               LISP-INTERNALS::SXHASH

               LISP-INTERNALS::*PACKAGE*
               LISP-INTERNALS::MAKE-PACKAGE
               LISP-INTERNALS::IN-PAACKAGE
               LISP-INTERNALS::FIND-PACKAGE
               LISP-INTERNALS::PACKAGE-NAME
               LISP-INTERNALS::RENAME-PACKAGE
               LISP-INTERNALS::PACKAGE-USE-LIST
               LISP-INTERNALS::PACKAGE-USED-BY-LIST
               LISP-INTERNALS::PACKAGE-SHADOWING-SYMBOLS
               LISP-INTERNALS::LIST-ALL-PACAKGES
               LISP-INTERNALS::INTERN
               LISP-INTERNALS::FIND-SYMBOL
               LISP-INTERNALS::UNINTERN
               LISP-INTERNALS::EXPORT
               LISP-INTERNALS::UNEXPORT
               LISP-INTERNALS::IMPORT
               LISP-INTERNALS::SHADOWING-IMPORT
               LISP-INTERNALS::SHADOW
               LISP-INTERNALS::USE-PACKAGE
               LISP-INTERNALS::UNUSE-PACKAGE
               LISP-INTERNALS::FIND-ALL-SYMBOLS
               LISP-INTERNALS::DO-SYMBOLS
               LISP-INTERNALS::DO-EXTERNAL-SYMBOLS
               LISP-INTERNALS::DO-ALL-SYMBOLS

               LISP-INTERNALS::*MODULES*
               LISP-INTERNALS::PROVIDE
               LISP-INTERNALS::REQUIRE

               LISP-INTERNALS::*READ-BASE*
               LISP-INTERNALS::*READ-SUPPRESS*
               LISP-INTERNALS::*READTABLE*
               LISP-INTERNALS::COPY-READTABLE
               LISP-INTERNALS::READTABLEP
               LISP-INTERNALS::SET-SYNTAX-FROM-CHAR
               LISP-INTERNALS::SET-MACRO-CHARACTER
               LISP-INTERNALS::GET-MACRO-CHARACTER
               LISP-INTERNALS::MAKE-DISPATCH-MACRO-CHARACTER
               LISP-INTERNALS::SET-DISPATCH-MACRO-CHARACTER
               LISP-INTERNALS::GET-DISPATCH-MACRO-CHARACTER

               LISP-INTERNALS::*STANDARD-OUTPUT*
               LISP-INTERNALS::*STANDARD-INPUT*
               LISP-INTERNALS::*TERMINAL-IO*
               LISP-INTERNALS::*ERROR-OUTPUT*
               LISP-INTERNALS::*QUERY-IO*
               LISP-INTERNALS::*DEBUG-IO*
               LISP-INTERNALS::*TRACE-OUTPUT*
               LISP-INTERNALS::MAKE-SYNONYM-STREAM
               LISP-INTERNALS::MAKE-BROADCAST-STREAM
               LISP-INTERNALS::MAKE-CONCATENATED-STREAM
               LISP-INTERNALS::MAKE-TWO-WAY-STREAM
               LISP-INTERNALS::MAKE-ECHO-STREAM
               LISP-INTERNALS::MAKE-STRING-INPUT-STREAM
               LISP-INTERNALS::MAKE-STRING-OUTPUT-STREAM
               LISP-INTERNALS::GET-OUTPUT-STREAM-STRING
               LISP-INTERNALS::WITH-OPEN-STREAM
               LISP-INTERNALS::WITH-INPUT-FROM-STREAM
               LISP-INTERNALS::WITH-OUTPUT-TO-STREAM
               LISP-INTERNALS::STREAMP
               LISP-INTERNALS::INPUT-STREAM-P
               LISP-INTERNALS::OUTPUT-STREAM-P
               LISP-INTERNALS::STREAM-ELEMENT-TYPE
               LISP-INTERNALS::CLOSE

               LISP-INTERNALS::*PRINT-ESCAPE*
               LISP-INTERNALS::*PRINT-PRETTY*
               LISP-INTERNALS::*PRINT-CIRCLE*
               LISP-INTERNALS::*PRINT-BASE*
               LISP-INTERNALS::*PRINT-RADIX*
               LISP-INTERNALS::*PRINT-CASE*
               LISP-INTERNALS::*PRINT-GENSYM*
               LISP-INTERNALS::*PRINT-LEVEL*
               LISP-INTERNALS::*PRINT-LENGTH*
               LISP-INTERNALS::*PRINT-ARRAY*
               LISP-INTERNALS::READ
               LISP-INTERNALS::*READ-DEFAULT-FLOAT-FORMAT*
               LISP-INTERNALS::READ-PRESERVING-WHITESPACE
               LISP-INTERNALS::READ-DELIMITED-LIST
               LISP-INTERNALS::READ-LINE
               LISP-INTERNALS::READ-CHAR
               LISP-INTERNALS::UNREAD-CHAR
               LISP-INTERNALS::PEEK-CHAR
               LISP-INTERNALS::LISTEN
               LISP-INTERNALS::READ-CHAR-NO-HANG
               LISP-INTERNALS::CLEAR-INPUT
               LISP-INTERNALS::READ-FROM-STRING
               LISP-INTERNALS::PARSE-INTEGER
               LISP-INTERNALS::READ-BYTE
               LISP-INTERNALS::PRIN1
               LISP-INTERNALS::PRINT
               LISP-INTERNALS::PPRINT
               LISP-INTERNALS::PRINC
               LISP-INTERNALS::WRITE-TO-STRING
               LISP-INTERNALS::PRIN1-TO-STRING
               LISP-INTERNALS::PRINC-TO-STRING
               LISP-INTERNALS::WRITE-CHAR
               LISP-INTERNALS::WRITE-STRING
               LISP-INTERNALS::TERPRI
               LISP-INTERNALS::FRESH-LINE
               LISP-INTERNALS::FINISH-OUTPUT
               LISP-INTERNALS::FORCE-OUTPUT
               LISP-INTERNALS::CLEAR-OUTPUT
               LISP-INTERNALS::WRITE-BYTE
               LISP-INTERNALS::FORMAT
               LISP-INTERNALS::Y-OR-N-P
               LISP-INTERNALS::YES-OR-NO-P

               LISP-INTERNALS::QUOTE
               LISP-INTERNALS::FUNCTION
               LISP-INTERNALS::SPECIAL-FORM-P
               LISP-INTERNALS::SETQ
               LISP-INTERNALS::PSETQ
               LISP-INTERNALS::SETF
               LISP-INTERNALS::PSETF
               LISP-INTERNALS::SHIFTF
               LISP-INTERNALS::ROTATEF
               LISP-INTERNALS::INCF
               LISP-INTERNALS::DECF
               LISP-INTERNALS::DEFINE-MODIFY-MACRO
               LISP-INTERNALS::DEFSETF
               LISP-INTERNALS::DEFINE-SETF-METHOD
               LISP-INTERNALS::GET-SETF-METHOD
               LISP-INTERNALS::GET-SETF-METHOD-MULTIPLE-VALUE
               LISP-INTERNALS::APPLY
               LISP-INTERNALS::FUNCALL
               LISP-INTERNALS::CALL-ARGUMENTS-LIMIT
               LISP-INTERNALS::PROGN
               LISP-INTERNALS::PROG1
               LISP-INTERNALS::PROG2
               LISP-INTERNALS::LET
               LISP-INTERNALS::LET*
               LISP-INTERNALS::COMPILER-LET
               LISP-INTERNALS::PROGV
               LISP-INTERNALS::FLET
               LISP-INTERNALS::LABELS
               LISP-INTERNALS::MACROLET
               LISP-INTERNALS::IF
               LISP-INTERNALS::WHEN
               LISP-INTERNALS::UNLESS
               LISP-INTERNALS::COND
               LISP-INTERNALS::CASE
               LISP-INTERNALS::TYPECASE
               LISP-INTERNALS::BLOCK
               LISP-INTERNALS::RETURN-FROM
               LISP-INTERNALS::RETURN
               LISP-INTERNALS::LOOP
               LISP-INTERNALS::DO
               LISP-INTERNALS::DO*
               LISP-INTERNALS::DOLIST
               LISP-INTERNALS::DOTIMES
               LISP-INTERNALS::MAPCAR
               LISP-INTERNALS::MAPLIST
               LISP-INTERNALS::MAPC
               LISP-INTERNALS::MAPL
               LISP-INTERNALS::MAPCAN
               LISP-INTERNALS::MAPCON
               LISP-INTERNALS::TAGBODY
               LISP-INTERNALS::PROG
               LISP-INTERNALS::PROG*
               LISP-INTERNALS::GO
               LISP-INTERNALS::VALUES
               LISP-INTERNALS::MULTIPLE-VALUES-LIMIT
               LISP-INTERNALS::VALUES-LIST
               LISP-INTERNALS::MULTIPLE-VALUE-LIMIT
               LISP-INTERNALS::MULTIPLE-VALUE-CALL
               LISP-INTERNALS::MULTIPLE-VALUE-PROG1
               LISP-INTERNALS::MULTIPLE-VALUE-BIND
               LISP-INTERNALS::MULTIPLE-VALUE-SETQ
               LISP-INTERNALS::CATCH
               LISP-INTERNALS::UNWIND-PROTECT
               LISP-INTERNALS::THROW

               LISP-INTERNALS::DEFMACRO
               LISP-INTERNALS::MACRO-FUNCTION
               LISP-INTERNALS::MACROEXPAND
               LISP-INTERNALS::MACROEXPAND-1
               LISP-INTERNALS::*MACROEXPAND-HOOK*

               LISP-INTERNALS::DECLARE
               LISP-INTERNALS::LOCALLY
               LISP-INTERNALS::PROCLAIM
               LISP-INTERNALS::SPECIAL
               LISP-INTERNALS::TYPE
               LISP-INTERNALS::FTYPE
               LISP-INTERNALS::INLINE
               LISP-INTERNALS::NOTINLINE
               LISP-INTERNALS::IGNORE
               LISP-INTERNALS::OPTIMIZE
               LISP-INTERNALS::DECLARATION
               LISP-INTERNALS::THE

               LISP-INTERNALS::EVAL
               LISP-INTERNALS::*EVALHOOK*
               LISP-INTERNALS::*APPLYHOOK*
               LISP-INTERNALS::CONSTANTP
               LISP-INTERNALS::+
               LISP-INTERNALS::++
               LISP-INTERNALS::+++
;              LISP-INTERNALS::*
               LISP-INTERNALS::**
               LISP-INTERNALS::***
;              LISP-INTERNALS::/
               LISP-INTERNALS:://
               LISP-INTERNALS::///

               LISP-INTERNALS::LAMBDA
               LISP-INTERNALS::&REST
               LISP-INTERNALS::&OPTIONAL
               LISP-INTERNALS::&KEY
               LISP-INTERNALS::&AUX
               LISP-INTERNALS::&ALLOW-OTHER-KEYS
               LISP-INTERNALS::&BODY
               LISP-INTERNALS::&WHOLE
               LISP-INTERNALS::&ENVIRONMENT
               LISP-INTERNALS::LAMBDA-LIST-KEYWORDS
               LISP-INTERNALS::LAMBDA-PARAMETERS-LIMIT
               LISP-INTERNALS::DEFUN
               LISP-INTERNALS::DEFVAR
               LISP-INTERNALS::DEFPARAMETER
               LISP-INTERNALS::DEFCONSTANT
               LISP-INTERNALS::EVAL-WHEN

               NEW-MATH::TRUNCATE
               NEW-MATH::FLOOR
               NEW-MATH::CEILING
               NEW-MATH::ROUND
               NEW-MATH::REM
               NEW-MATH::MOD
               NEW-MATH::GCD
               NEW-MATH::LCM
               NEW-MATH::NUMERATOR
               NEW-MATH::DENOMINATOR
               NEW-MATH::DECODE-FLOAT
               NEW-MATH::INTEGER-DECODE-FLOAT
               NEW-MATH::REALPART
               NEW-MATH::IMAGPART
               NEW-MATH::CONJGATE

;              NEW-MATH::SIGNUM
;              NEW-MATH::FLOAT
;              NEW-MATH::RATIONAL
;              NEW-MATH::RATIONALIZE
;              NEW-MATH::NUMERATOR
;              NEW-MATH::DENOMINATOR
;              NEW-MATH::FFLOOR
;              NEW-MATH::FCEILING
;              NEW-MATH::FTRUNCATE
;              NEW-MATH::FROUND
;              NEW-MATH::SCALE-FLOAT
;              NEW-MATH::FLOAT-RADIX
;              NEW-MATH::FLOAT-SIGN
;              NEW-MATH::FLOAT-DIGITS
;              NEW-MATH::FLOAT-PRECISION


               ))

(defun warm-boot ()

  (boot-stack-groups)

  (setq *all-packages* nil)
  (setq *package*      nil)

  (make-package "KEYWORD"    :use '())
  (make-package "PRIMITIVES" :use '() :nicknames '("PRIMS"))

  (make-package "K"          :use '())
  (make-package "HARDWARE"   :use '() :nicknames '("HW"))
  (make-package "VINCULUM"   :use '("PRIMS") :nicknames '("VINC"))
  (make-package "TIMERS"     :use '("VINCULUM" "PRIMS"))
  (make-package "MAP"        :use '("VINCULUM" "PRIMS"))
  (make-package "GC-RAM"           :use '("VINCULUM" "PRIMS"))
  (make-package "DATATYPE-RAM" :use '("VINCULUM" "PRIMS" "K") :nicknames '("DT-RAM"))
  (make-package "PAGING-DEVICES" :use '("VINCULUM" "PRIMS"))
  (make-package "VIRTUAL-MEMORY" :use '("VINCULUM" "PRIMS") :nicknames '("VMEM"))
  (make-package "PHYSICAL-CLUSTER-DATA" :use '("VIRTUAL-MEMORY" "VINCULUM" "PRIMS") :nicknames '("PCD"))
  (make-package "QUANTUM-MAP" :use '("VIRTUAL-MEMORY" "VINCULUM" "PRIMS"))
  (make-package "MEMORY-MANAGEMENT" :use '("VINCULUM" "PRIMS") :nicknames '("MEMLOW"))
  (make-package "REGION-BITS" :use '("MEMORY-MANAGEMENT" "VIRTUAL-MEMORY" "VINCULUM" "PRIMS"))
  (make-package "MAP-FAULT"   :use '("VIRTUAL-MEMORY" "PHYSICAL-CLUSTER-DATA" "MAP" "VINCULUM" "PRIMS"))
  (make-package "GC-FAULT"    :use '("MAP" "VINCULUM" "PRIMS"))
  (make-package "REGION-DATA" :use '("MEMORY-MANAGEMENT" "VINCULUM" "PRIMS"))
  (make-package "AREA-DATA"   :use '("REGION-DATA" "MEMORY-MANAGEMENT" "VINCULUM" "PRIMS"))
  (make-package "MEMORY-MANAGEMENT-INTERFACE" :use '("VINCULUM" "PRIMS") :nicknames '("MEM"))
  (make-package "BOOT"        :use '("VINCULUM" "VIRTUAL-MEMORY" "PRIMS" "K"))
  (make-package "TRANSPORTER-RAM" :use '("VINCULUM" "VIRTUAL-MEMORY" "PRIMS"))
  (make-package "CONS"        :use '("MEMORY-MANAGEMENT-INTERFACE" "VINCULUM" "PRIMS" "K"))
  (make-package "SYMBOL"      :use '("CONS" "VINCULUM" "PRIMS"))
  (make-package "NEW-MATH"    :use '("VINCULUM" "PRIMS" "K"))
  (make-package "ARRAY"       :use '("VINCULUM" "PRIMS" "K"))
  (make-package "TRAP"        :use '("VINCULUM" "PRIMS" "K"))
  (make-package "NUBUS-STUFF" :use '("VINCULUM" "PRIMS" "K"))
  (make-package "K2"          :use '("VINCULUM" "PRIMS" "K"))
  (make-package "LISP-INTERNALS" :use '("PRIMS" "K") :nicknames '("LI"))
  (make-package "LISP"        :use '())
  (make-package "SETF")
  ;(make-package "SYSTEM")

  (make-package "NC"          :use '("LISP"))
  (make-package "GLOBAL-REGISTERS" :use '("LISP") :nicknames '("GR"))

  ;; some stuff in si appears in KFASL files
  ;; notably XR-BQ-LIST
  (make-package "GLOBAL" :use () :nicknames '("ZETALISP" "ZL"))
  (make-package "SYSTEM" :use '("GLOBAL") :nicknames '("SYS"))
  (make-package "SYSTEM-INTERNALS" :use '("GLOBAL" "SYS") :nicknames '("SI"))
  (make-package "COMPILER" :use '("GLOBAL" "SYS"))
  (make-package "USER" :use '("LISP"))
  ;; put gensyms here until fasl table stuff works
  (make-package "NIL" :use ())

  (setq pkg-keyword-package (find-package "KEYWORD"))
  (setq pkg-user-package (find-package "USER"))
  (setq pkg-global-package (find-package "GLOBAL"))
  (setq pkg-lisp-package (find-package "LISP"))
  (setq pkg-system-package (find-package "SYSTEM"))
  (setq pkg-system-internals-package (find-package "SYSTEM-INTERNALS"))

  (setq *package* (find-package "USER"))

  (let ((pack (find-package "LISP")))
    (intern-symbol NIL pack)
    (intern-symbol T pack)
    (export '(T NIL) pack))

  (dolist (sym gr:*warm-symbols*)
    (let ((pack (find-package (symbol-package sym)))
          (name (symbol-name sym)))
      (unless (or (string= name "NIL")
                  (string= name "T"))
        (intern-symbol sym pack)
        (setf (symbol-package sym) pack))))

  (setq gr:*warm-symbols* gr:*t*)

  (let ((pack (find-package "LISP")))
    (dolist (sym lisp-symbols)
      (import sym pack)
      (export sym pack)))

  (unintern nil *package*)    ;;; No I don't know why, but it works!!!!
  (let ((pack (find-package "LISP")))
    (intern-symbol nil pack)
    (intern-symbol t   pack)
    (setf (symbol-package t) pack)
    (setf (symbol-package nil) pack)
    (export '(t nil) pack))

;  (setq *read-base* 10.)
;  (setq *read-suppress* nil)
;  (setq *unread-char* nil)
;  (setq *echo?* t)
;  (setq *initial-common-lisp-readtable* (make-common-lisp-readtable))
;  (setq *readtable* *initial-common-lisp-readtable*)
;  (setq *standard-input* k2:kbug-k-input-character-stream)
;  (instantiate-standard-streams)

;  (make-control-pdl-area)

  (install-top-level-macros)
  (eval '(DEFVAR SETF::*GRODY-SETF-MACRO-TABLE* NIL))
;  (map-eval-onto-list (censor-and-reverse-list k2::*warm-eval-list*))
  (eval '(DEFVAR *EVALUATOR-AVAILABLE?* T))

  (warm-boot-complete)
  )

(defun warm-boot-complete ()
  (warm-boot-complete))

(defun unwarm-boot-if-not-complete ()
  (dolist (s gr:*warm-symbols*)
    (when (package-p (symbol-package s))
      (setf (symbol-package s) (package-name (symbol-package s)))))
  (loop))



(defun hot-boot ()
  (install-top-level-macros)
  (eval '(DEFVAR SETF::*GRODY-SETF-MACRO-TABLE* NIL))
  (map-eval-onto-list (censor-and-reverse-list k2::*warm-eval-list*))
  (eval '(DEFVAR *EVALUATOR-AVAILABLE?* T))
  )

(defun install-top-level-macros ()
  (setf (symbol-function 'DEFUN)
        (symbol-function 'DEFUN-K))
  (setf (symbol-function 'DEFVAR)
        (symbol-function 'DEFVAR-K))
  (setf (symbol-function 'DEFPARAMETER)
        (symbol-function 'DEFPARAMETER-K))
  (setf (symbol-function 'DEFCONSTANT)
        (symbol-function 'DEFCONSTANT-K)))

(defun map-eval-onto-list (list)
  (dolist (form list)
    (eval-special-ok form)))

(defun censor-and-reverse-list (list)
  "Remove anything from LIST that the evaluator will barf on, and
also reverse it."
  (let ((censored-list nil))
    (dolist (form list)
      (unless (or (bogus-compiler-form? form)
                  (export-form? form)
                  (defmacro-form? form)
                  (contains-big-number? form))
        (push form censored-list)))
    censored-list))

;;; These predicates detect forms which can cause evaluator barfage.
;;;
(defun bogus-compiler-form? (form)
  (and (consp form)
       (symbolp (car form))
       (string= (package-name (symbol-package (car form)))
                "NC")))

(defun export-form? (form)
  (and (consp form)
       (eq (car form) 'EXPORT)))

(defun defmacro-form? (form)
  (and (consp form)
       (eq (car form) 'DEFMACRO)))

(defun contains-big-number? (form)
  (and (consp form)
       (eq (car form) 'DEFCONSTANT)
       (eq (cadr form) 'MOST-NEGATIVE-FIXNUM)))
