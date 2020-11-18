;;; -*- Mode:LISP; Package:GLOBAL; Base:10; Readtable:COMMON-LISP -*-
(import '(
  &ALLOW-OTHER-KEYS                             ;common
  &AUX                                          ;common
  &BODY                                         ;common
  &ENVIRONMENT                                  ;common
  &EVAL
  &FUNCTIONAL
  &KEY                                          ;common
  &LIST-OF
  &LOCAL
  &OPTIONAL                                     ;common
  &QUOTE
  &REST                                         ;common
  &SPECIAL
  &WHOLE

 *package*
 *PRINT-RADIX*                                  ;common
 *READ-BASE*


 
 
 
 ***                                            ;common
 **                                             ;common
 *                                              ;common
 +++                                            ;common
 ++                                             ;common
 +                                              ;common

 -                                              ;common

 ///                                            ;common
 //                                             ;common
 /=                                             ;common
 /                                              ;common -- incompatible
 1+                                             ;common
 1-                                             ;common
;<-                                             ;So long, losers
;<-AS
;<<--
 <=                                             ;common
 <                                              ;common
 =                                              ;common
 >=                                             ;common
 >                                              ;common
 \\
 \\\\
 ^
 @DEFINE
 ABS                                            ;common
 ACONS                                          ;common
 ACOSH                                          ;common
 ACOS                                           ;common
 ADD-INITIALIZATION
 ADD1
 ADJOIN                                         ;common
 ADJUST-ARRAY-SIZE
 ADJUST-ARRAY                                   ;common
 ADJUSTABLE-ARRAY-P                             ;common
 ADVISE
 ADVISE-WITHIN
 ALL-SPECIAL-SWITCH
 ALLOCATE-RESOURCE
;ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
 ALOC
 ALPHA-CHAR-P                                   ;common
 ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON
 ALPHALESSP
 ALPHANUMERICP                                  ;common
 AND                                            ;common
 AP-1
 AP-1-FORCE
 AP-2
 AP-3
 AP-LEADER
 APPEND-TO-ARRAY
 APPEND                                         ;common
 APPLYHOOK                                      ;common
 APPLY                                          ;common
 APROPOS-LIST                                   ;common
 APROPOS                                        ;common
 AR-1
 AR-1-FORCE
 AR-2
 AR-2-REVERSE
 AR-3
 AREA-LIST
 AREA-NAME
 AREA-NUMBER
 AREF                                   ;common -- incompatible
 ARG
 ARGLIST
 ARGS
 ARGS-INFO
 AROUND-METHOD-CONTINUE
 ARRAY-\#-DIMS                                  ;obsolete
 ARRAY-ACTIVE-LENGTH
 ARRAY-BITS-PER-ELEMENT
 ARRAY-DIMENSION-LIMIT                          ;common
 ARRAY-DIMENSION-N
 ARRAY-DIMENSIONS                               ;common
 ARRAY-DIMENSION                                ;common
 ARRAY-DISPLACED-P                              ;common
 ARRAY-ELEMENT-SIZE
 ARRAY-ELEMENT-TYPE                             ;common
 ARRAY-ELEMENTS-PER-Q
 ARRAY-GROW
 ARRAY-HAS-FILL-POINTER-P                       ;common
 ARRAY-HAS-LEADER-P
 ARRAY-IN-BOUNDS-P                              ;common
 ARRAY-INDEX-ORDER
 ARRAY-INDEXED-P
 ARRAY-INDIRECT-P
 ARRAY-INITIALIZE
 ARRAY-LEADER
 ARRAY-LEADER-LENGTH
 ARRAY-LENGTH
 ARRAY-POP
 ARRAY-PUSH
 ARRAY-PUSH-EXTEND
 array-push-portion-extend
 ARRAY-RANK-LIMIT                               ;common
 ARRAY-RANK                                     ;common
 ARRAY-ROW-MAJOR-INDEX                          ;common
 ARRAY-TOTAL-SIZE-LIMIT
 ARRAY-TOTAL-SIZE                               ;common
 ARRAY-TYPE
 ARRAY-TYPES
 ARRAYCALL                                      ;maclisp
 ARRAYDIMS
 ARRAYP                                         ;common
 ARRAY                                          ;common
 AS-1
 AS-1-FORCE
 AS-2
 AS-2-REVERSE
 AS-3
 ASCII
 ASET
 ASH                                            ;common
 ASINH                                          ;common
 ASIN                                           ;common
 ASS
 ASSERT                                         ;common
 ASSIGN-ALTERNATE                               ;glorp. vestigal crocks from cold-load
 ASSIGN-VALUES                                  ; ditto
 ASSIGN-VALUES-INIT-DELTA                       ; ditto. sigh.
 ASSOC-IF-NOT                                   ;common
 ASSOC-IF                                       ;common
 ASSOC                                  ;common -- incompatible
 ASSQ
 ATAN2
 ATANH                                          ;common
 ATAN                                   ;common -- incompatible
 ATOM
 BACK-TRANSLATED-PATHNAME                       ;>>???
 BASE
 BEEP
 BIGNUM                                         ;common
 BIGP
 BIND
 BIT-ANDC1                                      ;common
 BIT-ANDC2                                      ;common
 BIT-AND                                        ;common
 BIT-EQV                                        ;common
 BIT-IOR                                        ;common
 BIT-NAND                                       ;common
 BIT-NOR                                        ;common
 BIT-NOT                                        ;common
 BIT-ORC1                                       ;common
 BIT-ORC2                                       ;common
 BIT-TEST
 BIT-VECTOR-P                                   ;common
 BIT-VECTOR                                     ;common
 BIT-XOR                                        ;common
 BITBLT
 BIT                                            ;common
 BLOCK                                          ;common
 BOOLE-1                                        ;common
 BOOLE-2                                        ;common
 BOOLE-ANDC1                                    ;common
 BOOLE-ANDC2                                    ;common
 BOOLE-AND                                      ;common
 BOOLE-C1                                       ;common
 BOOLE-C2                                       ;common
 BOOLE-CLR                                      ;common
 BOOLE-EQV                                      ;common
 BOOLE-IOR                                      ;common
 BOOLE-NAND                                     ;common
 BOOLE-NOR                                      ;common
 BOOLE-ORC1                                     ;common
 BOOLE-ORC2                                     ;common
 BOOLE-OR                                       ;common
 BOOLE-SET                                      ;common
 BOOLE-XOR                                      ;common
 BOOLE                                          ;common
 BOTH-CASE-P                                    ;common
 BOUNDP-GLOBALLY
 BOUNDP-IN-CLOSURE
 BOUNDP                                         ;common
 BREAKON
 BREAK                                          ;common
 BUG
 BUTLAST                                        ;common
 BYTE-POSITION                                  ;common
 BYTE-SIZE                                      ;common
 BYTE                                           ;common

 CALL-ARGUMENTS-LIMIT                           ;common
                                                ;common
 CASEQ
 CASE                                           ;common
 CATCH-ALL
 CATCH-CONTINUATION
 CATCH-CONTINUATION-IF
 CATCH-ERROR
 CATCH-ERROR-RESTART
 CATCH-ERROR-RESTART-EXPLICIT-IF
 CATCH-ERROR-RESTART-IF
 CATCH                                          ;common
 CCASE                                          ;common
                                                ;common
 CEILING                                        ;common
 CERROR                                         ;common
 CHAR
 CHAR
 CHAR
 CHAR-BITS-LIMIT                                ;common
 CHAR-BITS                                      ;common
 CHAR-BIT                                       ;common
 CHAR-CODE-LIMIT                                ;common
 CHAR-CODE                                      ;common
 CHAR-CONTROL-BIT                               ;common
 CHAR-DOWNCASE                                  ;common
 CHAR-EQUAL                                     ;common
 CHAR-FLIPCASE
 CHAR-FONT-LIMIT                                ;common
 CHAR-FONT                                      ;common
 CHAR-GREATERP                                  ;common
 CHAR-HYPER-BIT                                 ;common
 CHAR-INT                                       ;common
 CHAR-LESSP                                     ;common
 CHAR-META-BIT                                  ;common
 CHAR-NAME                                      ;common
 CHAR-NOT-EQUAL                                 ;common
 CHAR-NOT-GREATERP                              ;common
 CHAR-NOT-LESSP                                 ;common
 CHAR-STANDARD
 CHAR-SUPER-BIT                                 ;common
 CHAR-UPCASE                                    ;common
 CHAR/=                                         ;common
 CHAR<=                                         ;common
 CHAR<                                          ;common
 CHAR=                                          ;common
 CHAR>=                                         ;common
 CHAR>                                          ;common
 CHARACTERP                                     ;common
 CHARACTER                                      ;common -- incompatible
 CHAR                                           ;common
 CHECK-ARG
 CHECK-ARG-TYPE
 CHECK-TYPE                                     ;common
 CHOOSE-USER-OPTIONS
 CIRCULAR-LIST
 CIS                                            ;common
 CLEAR-INPUT                                    ;common
 CLEAR-MAR
 CLEAR-OUTPUT                                   ;common
 CLEAR-RESOURCE
 CLOSE                                  ;common -- incompatible
 CLOSURE
 CLOSURE-ALIST
 CLOSURE-BINDINGS
 CLOSURE-FUNCTION
 CLOSURE-VARIABLES
 CLOSUREP
 CLRHASH-EQUAL
 CLRHASH                                        ;common
 CODE-CHAR                                      ;common
 COERCE                                         ;common
 COMMENT
 COMMONP                                        ;common
 COMMON                                         ;common
 COMMON-LISP
 COMPILATION-SPEED                              ;common
 COMPILE-ENCAPSULATIONS
 COMPILE-ENCAPSULATIONS-FLAG
 COMPILE-FILE-ALIST
 COMPILE-FILE                                   ;common
 COMPILE-FLAVOR-METHODS
 COMPILE-LAMBDA
 COMPILED-FUNCTION-P                            ;common
 COMPILED-FUNCTION                              ;common
 COMPILER-LET                                   ;common
 COMPILE                                        ;common
 COMPLEXP                                       ;common
 COMPLEX                                        ;common
 CONCATENATE                                    ;common
 COND-EVERY
 CONDITION
 CONDITION-BIND
 CONDITION-BIND-DEFAULT
 CONDITION-BIND-DEFAULT-IF
 CONDITION-BIND-IF
 CONDITION-CALL
 CONDITION-CALL-IF
 CONDITION-CASE
 CONDITION-CASE-IF
 CONDITION-PSETQ
 CONDITION-RESUME
 CONDITION-RESUME-IF
 CONDITION-TYPEP
 COND                                           ;common
 CONJUGATE                                      ;common
 CONS-IN-AREA
 CONSP                                          ;common
 CONSTANTP                                      ;common
 CONTENTS
 CONTINUE-WHOPPER
 COPY-ALIST                                     ;common
 COPY-ARRAY-CONTENTS
 COPY-ARRAY-CONTENTS-AND-LEADER
 COPY-ARRAY-PORTION
 COPY-CLOSURE
 COPY-FILE
 COPY-LIST                                      ;common
 COPY-READTABLE                                 ;common
 COPY-SEQ                                       ;common
 COPY-SYMBOL                                    ;common
 COPY-SYNTAX
 COPY-TREE                                      ;common
 COPYALIST
 COPYLIST
 COPYLIST*
 COPYSYMBOL
 COPYTREE
 COPYTREE-SHARE
 COSD
 COSH                                           ;common
 COS                                            ;common
 COUNT-IF-NOT                                   ;common
 COUNT-IF                                       ;common
 COUNT                                          ;common
 CTYPECASE                                      ;common
 CURRENT-PROCESS
 CURRENT-STACK-GROUP
 CURRENT-STACK-GROUP-RESUMER
 CURSORPOS
 DATA-TYPE
 DBG
 DEALLOCATE-RESOURCE
 DEALLOCATE-WHOLE-RESOURCE
 DEBUG-IO
 DEBUGGING-INFO
 DECF                                           ;common
 DECLARE-FLAVOR-INSTANCE-VARIABLES
 DECLARATION                                    ;common?
 DECLARE                                        ;common -- needs hacking
 DECODE-FLOAT                                   ;common
 DECODE-UNIVERSAL-TIME                          ;common
 DEF
 DEFAULT-CONS-AREA
 DEFCONST
 DEFCONSTANT                                    ;common
 DEFDECL
 DEFF
 DEFF-MACRO
 DEFFLAVOR
 DEFFUNCTION
 DEFGENERIC
 DEFINE-LOOP-MACRO
 DEFINE-LOOP-PATH
 DEFINE-LOOP-SEQUENCE-PATH
 DEFINE-MODIFY-MACRO                            ;common
 DEFINE-PROMPT-AND-READ-TYPE
 DEFINE-SETF-METHOD                             ;common
 DEFINE-SITE-ALIST-USER-OPTION
 DEFINE-SITE-HOST-LIST
 DEFINE-SITE-USER-OPTION
 DEFINE-SITE-VARIABLE
 DEFINE-USER-OPTION
 DEFINE-USER-OPTION-ALIST
 DEFLAMBDA-MACRO
 DEFLAMBDA-MACRO-DISPLACE
 DEFLOCF
 DEFMACRO-DISPLACE
 DEFMACRO                                       ;common
 DEFMETHOD
 DEFMETHOD-INSTANCE
 DEFPACKAGE
 DEFPARAMETER                                   ;common
 DEFPROP
 DEFRESOURCE
 DEFSELECT
 DEFSELECT-INCREMENTAL
 DEFSETF                                        ;common
 DEFSIGNAL
 DEFSIGNAL-EXPLICIT
 DEFSTRUCT-DEFINE-TYPE
 DEFSTRUCT                                      ;common -- incompatible
 DEFSUBST
 DEFSYSTEM
 DEFTYPE                                        ;common
 DEFUN-METHOD
 DEFUNP
 DEFUN                                          ;common
 DEFVAR                                         ;common
 DEFVAR-SITE-ALIST-USER-OPTION
 DEFVAR-SITE-USER-OPTION
 defvar-resettable
 defvar-standard
 DEFVAR-USER-OPTION
 DEFWHOPPER
 DEFWHOPPER-SUBST
 DEFWINDOW-RESOURCE
 DEFWRAPPER
 DEL
 DEL-IF
 DEL-IF-NOT
 DELETE-DUPLICATES
 DELETE-FILE                                    ;common
 DELETE-IF-NOT                                  ;common
 DELETE-IF                                      ;common
 DELETE-INITIALIZATION
 DELETEF
 DELETE                                 ;common -- incompatible
 DELQ
 DENOMINATOR                                    ;common
 DEPOSIT-BYTE
 DEPOSIT-FIELD                                  ;common
 DESCRIBE-AREA
 DESCRIBE-DEFSTRUCT
 DESCRIBE-FLAVOR
 DESCRIBE-PACKAGE
 DESCRIBE-PARTITION
 DESCRIBE-SYSTEM
 DESCRIBE                                       ;common
 DESTRUCTURING-BIND
 DIFFERENCE
 DIGIT-CHAR-P                                   ;common
 DIGIT-CHAR                                     ;common
 DIRECTORY-NAMESTRING                           ;common
 DIRECTORY                                      ;common
 DIRED
 DISASSEMBLE                                    ;common
 DISK-RESTORE
 DISK-SAVE
 DISPATCH
 DISPLACE
 DO*-NAMED
 DO*                                            ;common
 DO-ALL-SYMBOLS                                 ;common
 DO-EXTERNAL-SYMBOLS                            ;common
 DO-FOREVER
 DO-LOCAL-EXTERNAL-SYMBOLS
 DO-LOCAL-SYMBOLS
 DO-NAMED
 DO-SYMBOLS                                     ;common
 DOCUMENTATION                                  ;common
 DOLIST                                         ;common
 DONT-OPTIMIZE
 DOTIMES                                        ;common
 DOUBLE-FLOAT-EPSILON                           ;common
 DOUBLE-FLOAT-NEGATIVE-EPSILON                  ;common
 DOUBLE-FLOAT                                   ;common
 DO                                             ;common
 DPB                                            ;common
 DRIBBLE-ALL
;DRIBBLE-END
;DRIBBLE-START
 DRIBBLE                                        ;common
 DUMP-FORMS-TO-FILE
 ECASE                                          ;common
 ED                                             ;common
 EH
 EH-ARG
 EH-ERROR
 EH-FRAME
 EH-FUN
 EH-LOC
 EH-SG
 EH-VAL
 EIGHTH                                         ;common
 ELT                                            ;common
 ENCODE-UNIVERSAL-TIME                          ;common
 ENDP                                           ;common
 ENOUGH-NAMESTRING                              ;common
 ENTITY
 ENTITYP
 EQL                                            ;common
 EQUALP                                         ;common
 EQUAL                                          ;common
 EQ                                             ;common
 ERR
 ERROR-OUTPUT
 ERROR-RESTART
 ERROR-RESTART-LOOP
 ERRORP
 ERROR                                          ;common
 ERRSET
 ETYPECASE                                      ;common
 EVAL-WHEN                                      ;common
 EVALHOOK                                       ;common
 EVAL                                           ;common
 EVENP                                          ;common
 EVERY                                  ;common -- incompatible
 EXPLODE
 EXPLODEC
 EXPLODEN
 EXPORT                                         ;common
 EXPR
 EXPT                                           ;common
 EXP                                            ;common
 FALSE
 FASD-UPDATE-FILE
 FASL-APPEND
 FASLOAD
 FBOUNDP                                        ;common
 FCEILING                                       ;common
 FDEFINE
 FDEFINEDP
 FDEFINITION
 FED
 FERROR
 FEXPR
 FFLOOR                                         ;common
 FIFTH                                          ;common
 FILE-AUTHOR                                    ;common
 FILE-LENGTH                                    ;common
 FILE-NAMESTRING                                ;common
 FILE-POSITION                                  ;common
 FILE-RETRY-NEW-PATHNAME
 FILE-RETRY-NEW-PATHNAME-IF
 FILE-WRITE-DATE                                ;common
 FILL-POINTER                                   ;common
 FILLARRAY
 FILL                                           ;common
 FIND-ALL-SYMBOLS
 FIND-EXTERNAL-SYMBOL
 FIND-IF-NOT                                    ;common
 FIND-IF                                        ;common
 FIND-PACKAGE                                   ;common
 FIND-POSITION-IN-LIST
;FIND-POSITION-IN-LIST-EQUAL
 FIND-SYMBOL                                    ;common
 FIND                                           ;common
 FINGER
 FINISH-OUTPUT                                  ;common
 FIRSTN
 FIRST                                          ;common
 FIX
 FIXNUMP
 FIXNUM                                         ;common
 FIXP
 FIXR
 FLATC
 FLATSIZE
 FLAVOR-ALLOWS-INIT-KEYWORD-P
 FLET                                           ;common
 FLOAT-DIGITS                                   ;common
 FLOAT-PRECISION                                ;common
 FLOAT-RADIX                                    ;common
 FLOAT-SIGN                                     ;common
 FLOATP                                         ;common
 FLOAT                                          ;common
 FLONUM
 FLONUMP
 FLOOR                                          ;common
 FMAKUNBOUND                                    ;common
 FOLLOW-CELL-FORWARDING
 FOLLOW-STRUCTURE-FORWARDING
 FONT                                           ;sigh... why are these global?
 FONT-BASELINE
 FONT-BLINKER-HEIGHT
 FONT-BLINKER-WIDTH
 FONT-CHAR-HEIGHT
 FONT-CHAR-WIDTH
 FONT-CHAR-WIDTH-TABLE
 FONT-CHARS-EXIST-TABLE
 FONT-FILL-POINTER
 FONT-INDEXING-TABLE
 FONT-LEFT-KERN-TABLE
 FONT-NAME
 FONT-NEXT-PLANE
 FONT-RASTER-HEIGHT
 FONT-RASTER-WIDTH
 FONT-RASTERS-PER-WORD
 FONT-WORDS-PER-CHAR
 FORCE-OUTPUT                                   ;common
 FORMAT-MACRO
 FORMAT                                         ;common
 FORWARD-VALUE-CELL
 FOURTH                                         ;common
 FQUERY
 FRESH-LINE                                     ;common
 FROUND                                         ;common
 FSET
 FSET-CAREFULLY
 FSIGNAL
 FSYMEVAL
 FTRUNCATE                                      ;common
 FTYPE                                          ;common
 FUNCALL-SELF
 FUNCALL-WITH-MAPPING-TABLE
 FUNCALL                                        ;common
 FUNCTION-CELL-LOCATION
 FUNCTION-DOCUMENTATION
 FUNCTION-NAME
 FUNCTIONP                                      ;incompatible.
 FUNCTION                                       ;common
 FUNDEFINE
 G-L-P
;GC-IMMEDIATELY
;GC-OFF
;GC-ON
;GC-STATUS
 GCD                                            ;common
 GENSYM                                         ;common
 GENTEMP                                        ;common
 GET-DECODED-TIME                               ;common
 GET-DISPATCH-MACRO-CHARACTER                   ;common
 GET-FROM-ALTERNATING-LIST                      ;obsolete
 GET-HANDLER-FOR
 GET-INTERNAL-REAL-TIME                         ;common
 GET-INTERNAL-RUN-TIME                          ;common
 GET-LIST-POINTER-INTO-ARRAY                    ;obsolete
 GET-LOCATIVE-POINTER-INTO-ARRAY                ;obsolete
 GET-MACRO-CHARACTER                            ;common
 GET-OUTPUT-STREAM-STRING                       ;common
 GET-PNAME
 GET-PROPERTIES                                 ;common
 GET-SETF-METHOD-MULTIPLE-VALUE                 ;common
 GET-SETF-METHOD                                ;common
 GET-SITE-OPTION
 GET-UNIVERSAL-TIME                             ;common
 GETCHARN                                       ;maclisp
 GETCHAR                                        ;maclisp
 GETDECL
 GETF                                           ;common
 GETF-from-area
 GETHASH-EQUAL
 gethash
 GETHASH                                        ;common
 GETL
 GET                                            ;common
 get-from-area
 GLOBALIZE
 GO                                             ;common
 GRAPHIC-CHAR-P                                 ;common
 GREATERP                                       ;common
 GRIND-TOP-LEVEL
 GRINDEF
 HAIPART
 HARDCOPY-BIT-ARRAY
 HARDCOPY-FILE
 HARDCOPY-STATUS
 HARDCOPY-STREAM
 HASH-TABLE-COUNT                               ;common
 HASH-TABLE-P                                   ;common
 HASH-TABLE                                     ;common
 HAULONG
 HOST-NAMESTRING                                ;common
 HOSTAT
 IBASE
 IDENTITY
 if
 IF-FOR-LISPM                                   ;obsolete
 IF-FOR-MACLISP                                 ;obsolete
 IF-FOR-MACLISP-ELSE-LISPM                      ;obsolete
 IF-IN-CADR                                     ;obsolete
 IF-IN-CADR-ELSE-LAMBDA                         ;obsolete
 IF-IN-LAMBDA                                   ;obsolete
 IF-IN-LAMBDA-ELSE-CADR                         ;obsolete
 IF-IN-LISPM                                    ;obsolete
 IF-IN-MACLISP                                  ;obsolete
 IF                                             ;incompatible
 IGNORE-ERRORS
 IGNORE                                         ;common
 IMAGPART                                       ;common
 IMPLODE                                        ;maclisp
 IMPORT                                         ;common
 IN-PACKAGE                                     ;common
 INCF                                           ;common
 INHIBIT-FDEFINE-WARNINGS
 INHIBIT-IDLE-SCAVENGING-FLAG
 INHIBIT-SCAVENGING-FLAG
 INHIBIT-SCHEDULING-FLAG
 INHIBIT-STYLE-WARNINGS
 INHIBIT-STYLE-WARNINGS-SWITCH
;INIT-FILE-PATHNAME
 INITIALIZATIONS
 INLINE
 INPUT-STREAM-P                                 ;common
 INSPECT                                        ;common
 INSTANCE
 INSTANCEP
;INSTANCE-VARIABLE-BOUNDP
 INSTANTIATE-FLAVOR
 INT-CHAR                                       ;common
 INTEGER-DECODE-FLOAT                           ;common
 INTEGER-LENGTH                                 ;common
 INTEGERP                                       ;common
 INTEGER                                        ;common
 INTERN-LOCAL
 INTERN-LOCAL-SOFT
 INTERN-SOFT
 INTERNAL-TIME-UNITS-PER-SECOND                 ;common
 INTERN                                         ;common
 INTERSECTION                           ;common -- incompatible
 ISQRT                                          ;common
 KBD-CHAR-AVAILABLE
 KBD-TYI
 KBD-TYI-NO-HANG
 KEYWORD-EXTRACT
 KEYWORDP                                       ;common
 KEYWORD                                        ;common
 KILL-PACKAGE
 LABELS                                         ;common
 LAMBDA-LIST-KEYWORDS                           ;common
 LAMBDA-MACRO
 LAMBDA-PARAMETERS-LIMIT                        ;common
 LAMBDA                                         ;common
 LAST                                           ;common
 LCM                                            ;common
 LDB-TEST                                       ;common
 LDB                                            ;common
 LDIFF                                          ;common
 LEAST-NEGATIVE-DOUBLE-FLOAT                    ;common
 LEAST-NEGATIVE-LONG-FLOAT                      ;common
 LEAST-NEGATIVE-SHORT-FLOAT                     ;common
 LEAST-NEGATIVE-SINGLE-FLOAT                    ;common
 LEAST-POSITIVE-DOUBLE-FLOAT                    ;common
 LEAST-POSITIVE-LONG-FLOAT                      ;common
 LEAST-POSITIVE-SHORT-FLOAT                     ;common
 LEAST-POSITIVE-SINGLE-FLOAT                    ;common
 LENGTH                                         ;common
 LESSP
 LET*                                           ;common
 LET-CLOSED
 LET-GLOBALLY
 LET-GLOBALLY-IF
 LET-IF
 LET                                            ;common
 LETF
 LETF*
 LETF-IF
 LEXPR-CONTINUE-WHOPPER
 LEXPR-FUNCALL
 LEXPR-FUNCALL-SELF
 LEXPR-FUNCALL-WITH-MAPPING-TABLE
 LEXPR-SEND
 LEXPR-SEND-IF-HANDLES
;LISP-CRASH-LIST
 LISP-IMPLEMENTATION-TYPE                       ;common
 LISP-IMPLEMENTATION-VERSION                    ;common
;LISP-REINITIALIZE
 LIST*-IN-AREA
 LIST*                                          ;common
 LIST-ALL-PACKAGES                              ;common
 LIST-ARRAY-LEADER
 LIST-IN-AREA
 LIST-LENGTH                                    ;common
 LIST-MATCH-P
 LISTARRAY
 LISTEN                                         ;common
 LISTF
 LISTF
 LISTIFY
 LISTP                                  ;common -- incompatible
 LIST                                           ;common
 LOAD-AND-SAVE-PATCHES
 LOAD-BYTE
 LOAD-FILE-ALIST
 LOAD-FILE-LIST
 LOAD-PATCHES
 LOAD                                           ;common
 LOCAL-DECLARATIONS
 LOCAL-DECLARE
 LOCALLY                                        ;common
 LOCATE-IN-CLOSURE
 LOCATE-IN-INSTANCE
 LOCATION-BOUNDP
 LOCATION-CONTENTS
 LOCATION-MAKUNBOUND
 LOCATIVE
 LOCATIVEP
 LOCF
 LOG1
 LOGANDC1                                       ;common
 LOGANDC2                                       ;common
 LOGAND                                         ;common
 LOGBITP                                        ;common
 LOGCOUNT                                       ;common
 LOGEQV                                         ;common
 LOGIN
 LOGIN-EVAL
 LOGIN-FDEFINE
 LOGIN-FORMS
 LOGIN-SETQ
 LOGIOR                                         ;common
 LOGNAND                                        ;common
 LOGNOR                                         ;common
 LOGNOT                                         ;common
 LOGORC1                                        ;common
 LOGORC2                                        ;common
 LOGOUT
 LOGOUT-LIST
 LOGTEST                                        ;common
 LOGXOR                                         ;common
 LOG                                            ;common
 LONG-FLOAT-EPSILON                             ;common
 LONG-FLOAT-NEGATIVE-EPSILON                    ;common
 LONG-FLOAT                                     ;common
 LONG-SITE-NAME                                 ;common
 LOOP-FINISH
 LOOP                                           ;common
 LOWER-CASE-P                                   ;common
 LSH
 MACHINE-INSTANCE                               ;common
 MACHINE-TYPE                                   ;common
 MACHINE-VERSION                                ;common
 MACRO
 MACRO-COMPILED-PROGRAM
 MACRO-FUNCTION                                 ;common
 MACROEXPAND-1                          ;incompatible
 MACROEXPAND-ALL
 MACROEXPAND                                    ;incompatible
 MACROLET                                       ;common
 MAIL
 MAKE-AREA
 MAKE-ARRAY-INTO-NAMED-STRUCTURE
 MAKE-ARRAY                                     ;incompatible
 MAKE-BROADCAST-STREAM                          ;common
 MAKE-CHAR                                      ;common
 MAKE-CONCATENATED-STREAM                       ;common
 MAKE-CONDITION
 MAKE-DISPATCH-MACRO-CHARACTER                  ;common
 MAKE-ECHO-STREAM                               ;common
 MAKE-EQUAL-HASH-TABLE
 MAKE-HARDCOPY-STREAM
 MAKE-HASH-TABLE                                ;common
 make-heap
 MAKE-INSTANCE
 MAKE-LIST                                      ;common
 MAKE-PACKAGE                                   ;common
 MAKE-PATHNAME                                  ;common
 MAKE-PIXEL-ARRAY
 MAKE-PLANE
 MAKE-PROCESS
 MAKE-RANDOM-STATE                              ;common
 MAKE-SEQUENCE                                  ;common
 MAKE-STACK-GROUP
 MAKE-STRING-INPUT-STREAM                       ;common
 MAKE-STRING-OUTPUT-STREAM                      ;common
 MAKE-STRING                                    ;common
 MAKE-SYMBOL                                    ;common
 MAKE-SYN-STREAM
 MAKE-SYNONYM-STREAM                            ;common
 MAKE-SYSTEM
 MAKE-TWO-WAY-STREAM                            ;common
 MAKNAM
 MAKUNBOUND-GLOBALLY
 MAKUNBOUND-IN-CLOSURE
 MAKUNBOUND                                     ;common
 MAP-RESOURCE
 MAPATOMS-ALL                                   ;obsolete
 MAPATOMS                                       ;obsolete
 MAPCAN                                         ;common
 MAPCAR                                         ;common
 MAPCON                                         ;common
 MAPC                                           ;common
 MAPHASH-EQUAL
 MAPHASH-RETURN
 MAPHASH                                        ;common
 MAPLIST                                        ;common
 MAPL                                           ;common
 MAP                                            ;common -- incompatible
 MAR-BREAK
 MAR-MODE
 MASK-FIELD                                     ;common
 MAXPREFIX
 MAXSUFFIX
 MAX                                            ;common
 MEM
 MEMASS
 MEMBER-IF-NOT                                  ;common
 MEMBER-IF                                      ;common
 MEMBER                                 ;common -- incompatible
 MEMQ
 MERGE-PATHNAMES                                ;common
 MERGE                                          ;common
 MEXP
 MICROCODE-FUNCTION
 MINUS
 MINUSP                                         ;common
 MIN                                            ;common
 MISMATCH                                       ;common
 MOD                                            ;common
;MONITOR-VARIABLE
 MOST-NEGATIVE-DOUBLE-FLOAT                     ;common
 MOST-NEGATIVE-FIXNUM                           ;common
 MOST-NEGATIVE-LONG-FLOAT                       ;common
 MOST-NEGATIVE-SHORT-FLOAT                      ;common
 MOST-NEGATIVE-SINGLE-FLOAT                     ;common
 MOST-POSITIVE-DOUBLE-FLOAT                     ;common
 MOST-POSITIVE-FIXNUM                           ;common
 MOST-POSITIVE-LONG-FLOAT                       ;common
 MOST-POSITIVE-SHORT-FLOAT                      ;common
 MOST-POSITIVE-SINGLE-FLOAT                     ;common
 MULTIPLE-VALUE
 MULTIPLE-VALUE-BIND                            ;common
 MULTIPLE-VALUE-CALL                            ;common
 MULTIPLE-CERROR
 MULTIPLE-VALUE-LIST                            ;common
 MULTIPLE-VALUE-PROG1                           ;common
;MULTIPLE-VALUE-RETURN
 MULTIPLE-VALUE-SETQ                            ;common
 MULTIPLE-VALUES-LIMIT                          ;common
 NAME-CHAR                                      ;common
 NAMED-LAMBDA
 NAMED-STRUCTURE
 NAMED-STRUCTURE-INVOKE
 NAMED-STRUCTURE-P
 NAMED-STRUCTURE-SYMBOL
 NAMED-SUBST
 NAMESTRING                                     ;common
 NBUTLAST                                       ;common
 NCONC                                          ;common
 NCONS
 NCONS-IN-AREA
 NEQ
 NIL                                            ;common
 NINTERSECTION                          ;common -- incompatible
 NINTH                                          ;common
 NLEFT
 NLISTP
 NON-COMPLEX-NUMBER
 NOTANY                                         ;common
 NOTEVERY                                       ;common
 NOTINLINE
 NOT                                            ;common
 NRECONC                                        ;common
 NREVERSE                                       ;common
 NSET-DIFFERENCE                                ;common
 NSET-EXCLUSIVE-OR                              ;common
 NSTRING-CAPITALIZE                             ;common
 NSTRING-DOWNCASE                               ;common
 NSTRING-UPCASE                                 ;common
 NSUBLIS                                        ;common
 NSUBST-IF-NOT                                  ;common
 NSUBST-IF                                      ;common
 NSUBSTITUTE-IF-NOT                             ;common
 NSUBSTITUTE-IF                                 ;common
 NSUBSTITUTE                                    ;common
 NSUBSTRING
 NSUBST                                         ;common
 NSYMBOLP
 NTH-SAFE
 NTH-VALUE
 NTHCDR-SAFE
 NTHCDR                                         ;common
 NTH                                            ;common
 NULL-MACRO
 NULL                                           ;common
 NUMBER-INTO-ARRAY
 NUMBERP                                        ;common
 NUMBER                                         ;common
 NUMERATOR                                      ;common
 NUNION                                 ;common  -- incompatible
;OBSOLETE-FUNCTION-WARNING-SWITCH
 ODDP                                           ;common
 ONCE-ONLY
 OPEN-CODE-MAP-SWITCH
 OPEN                                           ;common
 OPERATION-HANDLED-P
 OPTIMIZE
 OR                                             ;common
 OTHERWISE                                      ;common
 OUTPUT-STREAM-P                                ;common
 PACKAGE-CELL-LOCATION
 PACKAGE-DECLARE                                ;obsolete
 PACKAGE-EXTERNAL-SYMBOLS
 PACKAGE-NAME                                   ;common
 PACKAGE-NICKNAMES                              ;common
 PACKAGE-PREFIX-PRINT-NAME
 PACKAGE-SHADOWING-SYMBOLS                      ;common
 PACKAGE-USE-LIST                               ;common
 PACKAGE-USED-BY-LIST                           ;common
 PACKAGEP                                       ;common
 PACKAGE                                        ;common
 PAIRLIS                                        ;common
 PARSE-INTEGER                                  ;common
 PARSE-NAMESTRING                               ;common
 PARSE-NUMBER
 PATHNAME-DEVICE                                ;common
 PATHNAME-DIRECTORY                             ;common
 PATHNAME-HOST                                  ;common
 PATHNAME-NAME                                  ;common
 PATHNAME-PLIST
 PATHNAME-TYPE                                  ;common
 PATHNAME-VERSION                               ;common
 PATHNAMEP                                      ;common
 PATHNAME                                       ;common
 PEEK
 PEEK-CHAR                                      ;common
 PERMANENT-STORAGE-AREA
 PHASE                                          ;common
 PIXEL-ARRAY-HEIGHT
 PIXEL-ARRAY-WIDTH
 PI                                             ;common
 PKG-BIND
 PKG-CREATE-PACKAGE
 PKG-FIND-PACKAGE
 PKG-GLOBAL-PACKAGE
 PKG-GOTO
 PKG-GOTO-GLOBALLY
 PKG-KEYWORD-PACKAGE
 PKG-KILL                                       ;obsolete: use kill-package
 PKG-SYSTEM-PACKAGE
 PLANE-AREF
 PLANE-ASET
 PLANE-DEFAULT
 PLANE-EXTENSION
 PLANE-ORIGIN
 PLANE-REF                                      ;obsolete: use plane-aref
 PLANE-STORE                                    ;obsolete: use plane-aset
 PLIST
 PLUS
 PLUSP                                          ;common
 POP                                            ;common
 POSITION-IF-NOT                                ;common
 POSITION-IF                                    ;common
 POSITION                                       ;common
 PPRINT                                         ;common
 PRIN1-THEN-SPACE
 PRIN1-TO-STRING                                ;common
 PRIN1                                          ;common
 PRINC-TO-STRING                                ;common
 PRINC                                          ;common
 PRINLENGTH
 PRINLEVEL
 PRINT-DISK-LABEL
 PRINT-ERROR-MODE
 PRINT-HERALD
 PRINT-LOADED-BAND
 PRINT-LOGIN-HISTORY
 PRINT-NOTIFICATIONS
 PRINT-SENDS
 PRINT-SYSTEM-MODIFICATIONS
 PRINT                                          ;common
 PROBE-FILE                                     ;common
 PROBEF
 PROCESS-ALLOW-SCHEDULE
 PROCESS-CREATE
 PROCESS-DISABLE
 PROCESS-ENABLE
 PROCESS-ERROR-STOP-PROCESSES
 PROCESS-INITIAL-FORM
 PROCESS-INITIAL-STACK-GROUP
 PROCESS-LOCK
 PROCESS-NAME
 PROCESS-PLIST
 PROCESS-PRESET
 PROCESS-RESET
 PROCESS-RESET-AND-ENABLE
 PROCESS-RUN-FUNCTION
 PROCESS-RUN-RESTARTABLE-FUNCTION
 PROCESS-RUN-TEMPORARY-FUNCTION
 PROCESS-SLEEP
 PROCESS-STACK-GROUP
 PROCESS-UNLOCK
 PROCESS-WAIT
 PROCESS-WAIT-ARGUMENT-LIST
 PROCESS-WAIT-FUNCTION
 PROCESS-WAIT-WITH-TIMEOUT
 PROCESS-WHOSTATE                               ;obsolete
 PROCLAIM                                       ;common
 PROG*                                          ;common
 PROG1                                          ;common
 PROG2                                          ;common
 PROGN                                          ;common
 PROGV                                          ;common
 PROGW
 PROG                                           ;common
 PROMPT-AND-READ
 PROPERTY-CELL-LOCATION
 PROVIDE                                        ;common
 PSETF                                          ;common
 PSETQ                                          ;common
 PUSHNEW                                        ;common
 PUSH                                           ;common
 PUTDECL
 PUTHASH
 PUTHASH-EQUAL
 PUTPROP
 PUTPROP-IN-AREA
 Q-DATA-TYPES
 QC-FILE
 QC-FILE-LOAD
 QREPLY
 QSEND
 QSENDS-OFF
 QSENDS-ON
 QUERY-IO
 QUOTE                                          ;common
 QUOTIENT
 RANDOM-STATE-P                                 ;common
 RANDOM-STATE                                   ;common
 RANDOM                                         ;common
 RASS
 RASSOC-IF-NOT                                  ;common
 RASSOC-IF                                      ;common
 RASSOC                                 ;common -- incompatible
 RASSQ
 RATIONALIZE                                    ;common
 RATIONALP                                      ;common
 RATIONAL                                       ;common
 RATIO                                          ;common
 READ-BYTE                                      ;common
 READ-CHAR-NO-HANG                              ;common
 READ-CHAR                                      ;common
 READ-CHECK-INDENTATION
 READ-DELIMITED-LIST                            ;common
 READ-DELIMITED-STRING
 READ-FOR-TOP-LEVEL
 READ-FROM-STRING                               ;common -- incompatible
 READ-LINE                                      ;common
 READ-METER
 READ-OR-END
 READ-PRESERVE-DELIMITERS
 READ-PRESERVING-WHITESPACE                     ;common
 READCH                                         ;maclisp
 READFILE
 READLINE
 READLINE-OR-NIL
 READLINE-TRIM
 READLIST
 READTABLEP                                     ;common
 READTABLE                                      ;common
 READ                                   ;common -- incompatible
 REAL
 REALP
 REALPART                                       ;common
 RECOMPILE-FLAVOR
 RECORD-SOURCE-FILE-NAME
 REDUCE                                         ;common
 REM-IF-NOT                                     ;common
 REM-IF                                         ;common
 REMAINDER
 REMF                                           ;common
 REMHASH-EQUAL
 REMHASH                                        ;common
 REMOB
 REMOVE-DUPLICATES                              ;common
 REMOVE-IF-NOT                                  ;common
 REMOVE-IF                                      ;common
 REMOVE                                 ;common -- incompatible
 REMPROP                                        ;common
 REMQ
 REM                                            ;common -- incompatible
 RENAME-FILE                                    ;common
 RENAME-PACKAGE                                 ;common
 RENAMEF
 REPLACE                                        ;common
 REPLY
 REQUIRE                                        ;common
 RESET-FILL-POINTER
 RESET-INITIALIZATIONS
 RESET-USER-OPTIONS
 REST1
 REST2
 REST3
 REST4
 REST                                           ;common
 RETURN-ARRAY
 RETURN-FROM                                    ;common
 RETURN-LIST
 RETURN-NEXT-VALUE
 RETURN-STORAGE
 RETURN                                         ;common
 REVAPPEND                                      ;common
 REVERSE                                        ;common
 ROOM
 ROT
 ROTATEF                                        ;common
 ROUND                                          ;common
 RPLACA                                         ;common
 RPLACD
 RUBOUT-HANDLER
 RUBOUT-HANDLER-OPTIONS
 SAFETY                                         ;common
 SAMEPNAMEP
 SASSOC
 SASSQ
 SATISFIES                                      ;common
 SBIT                                           ;common
 SCALE-FLOAT                                    ;common
 SCHAR                                          ;common
 SEARCH                                         ;common
 SECOND                                         ;common
 SELECT
 SELECT-MATCH
 SELECT-PROCESSOR
 SELECTOR
 SELECTQ
 SELECTQ-EVERY
 SELF
 SELF-EVALUATING-P
 SEND
 SEND-IF-HANDLES
 SEQUENCE                                       ;common
 SET-CHAR-BIT                                   ;common
 SET-CHARACTER-TRANSLATION
 SET-CURRENT-BAND
 SET-CURRENT-MICROLOAD
 SET-DIFFERENCE                                 ;common
 SET-DISPATCH-MACRO-CHARACTER                   ;common
 SET-EXCLUSIVE-OR                               ;common
 SET-GLOBALLY
 SET-IN-CLOSURE
 SET-IN-INSTANCE
 SET-MACRO-CHARACTER                            ;common
 SET-MAR
 SET-MEMORY-SIZE
 SET-PRINTER-DEFAULT-OPTION
 SET-SYNTAX-\#-MACRO-CHAR
 SET-SYNTAX-FROM-CHAR                           ;common
 SET-SYNTAX-FROM-DESCRIPTION
 SET-SYNTAX-MACRO-CHAR
 SETARG
 SETF                                           ;common
 SETPLIST
 SETQ                                           ;common
 SETQ-GLOBALLY
 setq-standard-value
 SETSYNTAX
 SETSYNTAX-SHARP-MACRO
 SET                                            ;common
 SEVENTH                                        ;common
 SG-AREA
 SG-RETURN-UNSAFE
 SGVREF
 SHADOWING-IMPORT                               ;common
 SHADOW                                         ;common
 SHIFTF                                         ;common
 SHORT-FLOAT-EPSILON                            ;common
 SHORT-FLOAT-NEGATIVE-EPSILON                   ;common
 SHORT-FLOAT                                    ;common
 SHORT-SITE-NAME                                ;common
 SIGNAL
 SIGNAL-CONDITION
 SIGNAL-PROCEED-CASE
 SIGNED-BYTE
 SIGNP
 SIGNUM                                         ;common
 SIMPLE-ARRAY                                   ;common
 SIMPLE-BIT-VECTOR-P                            ;common
 SIMPLE-BIT-VECTOR                              ;common
 SIMPLE-STRING-P                                ;common
 SIMPLE-STRING                                  ;common
 SIMPLE-VECTOR-P                                ;common
 SIMPLE-VECTOR                                  ;common
 SIND
 SINGLE-FLOAT-EPSILON
 SINGLE-FLOAT-NEGATIVE-EPSILON
 SINGLE-FLOAT                                   ;common
 SINH                                           ;common
 SIN                                            ;common
 SITE-NAME
 SIXTH                                          ;common
 SLEEP                                          ;common
 SMALL-FLOAT
 SMALL-FLOATP
 SMALL-FLONUM
 SOFTWARE-TYPE                                  ;common
 SOFTWARE-VERSION                               ;common
 SOME                                   ;common -- incompatible
 SORT-GROUPED-ARRAY
 SORT-GROUPED-ARRAY-GROUP-KEY
 SORTCAR
 SORT                                           ;common
 SPACE                                          ;common
 SPECIAL-FORM-P                                 ;common
 SPECIAL                                        ;common -- needs hacking
 SPEED                                          ;common
 SQRT                                           ;common
 SSTATUS
 STABLE-SORTCAR
 STABLE-SORT                                    ;common
 STACK-GROUP
 STACK-GROUP-PRESET
 STACK-GROUP-RESUME
 STACK-GROUP-RETURN
 STANDARD-CHAR-P                                ;common
 STANDARD-CHAR                                  ;common
 STANDARD-INPUT
 STANDARD-OUTPUT
 standard-value-let
 standard-value-let*
 standard-value-progv
 STATUS
 STEP-FORM
 STEP-VALUE
 STEP-VALUES
 STEP                                           ;common
 STORE
 STORE-ARRAY-LEADER
 STORE-CONDITIONAL
 STREAM-COPY-UNTIL-EOF
 STREAM-DEFAULT-HANDLER
 STREAM-ELEMENT-TYPE                            ;common
 STREAMP                                        ;common
 STREAM                                         ;common
 STRING
 STRING
 STRING
 STRING-APPEND
 STRING-APPEND-A-OR-AN
 STRING-CAPITALIZE-WORDS
 STRING-CAPITALIZE                              ;common
 STRING-CHAR-P                                  ;common
 STRING-CHAR                                    ;common
 STRING-COMPARE
 STRING-DOWNCASE                                ;common
 STRING-EQUAL                                   ;common
 STRING-GREATERP                                ;common
 STRING-LEFT-TRIM                               ;common
 STRING-LENGTH
 STRING-LESSP                                   ;common
 STRING-NCONC
 string-nconc-portion
 STRING-NOT-EQUAL                               ;common
 STRING-NOT-GREATERP                            ;common
 STRING-NOT-LESSP                               ;common
 STRING-NREVERSE
 STRING-PLURALIZE
 STRING-REMOVE-FONTS
 STRING-REMOVE-FONTS
 STRING-REVERSE
 STRING-REVERSE-SEARCH
 STRING-REVERSE-SEARCH-CHAR
 STRING-REVERSE-SEARCH-NOT-CHAR
 STRING-REVERSE-SEARCH-NOT-SET
 STRING-REVERSE-SEARCH-SET
 STRING-RIGHT-TRIM                              ;common
 STRING-SEARCH
 STRING-SEARCH-CHAR
 STRING-SEARCH-NOT-CHAR
 STRING-SEARCH-NOT-SET
 STRING-SEARCH-SET
 STRING-SELECT-A-OR-AN
 STRING-SUBST-CHAR
 STRING-TRIM                                    ;common
 STRING-UPCASE                                  ;common
 STRING/=
 STRING<=                                       ;common
 STRING<                                        ;common
 STRING=
 STRING=                                        ;common
 STRING>=                                       ;common
 STRING>                                        ;common
 STRINGP                                        ;common
 STRING                                         ;common
 STRUCTURE
 STRUCTURE-FORWARD
 SUB-APROPOS
 SUB1
 SUBLIS                                         ;common
 SUBRP
 SUBSEQ                                         ;common
 SUBSET
 SUBSET-NOT
 SUBSETP                                        ;common
 SUBST-IF-NOT                                   ;common
 SUBST-IF                                       ;common
 SUBSTITUTE-IF-NOT                              ;common
 SUBSTITUTE-IF                                  ;common
 SUBSTITUTE                                     ;common
 SUBSTRING
 SUBSTRING-AFTER-CHAR
 SUBST                                  ;common -- incompatible
 SUBTYPEP                                       ;common
 SUPDUP
 SVREF                                          ;common
 SWAP-SV-OF-SG-THAT-CALLS-ME
 SWAP-SV-ON-CALL-OUT
 SWAPF
 SWAPHASH
 SWAPHASH-EQUAL
 SXHASH                                         ;common
 SYMBOL-FUNCTION                                ;common
 SYMBOL-NAME                                    ;common
 SYMBOL-PACKAGE                                 ;common
 SYMBOL-PLIST                                   ;common
 SYMBOL-VALUE                                   ;common
 SYMBOLP                                        ;common
 SYMBOL                                         ;common
 SYMEVAL
 SYMEVAL-GLOBALLY
 SYMEVAL-IN-CLOSURE
 SYMEVAL-IN-INSTANCE
 SYMEVAL-IN-STACK-GROUP
 TAGBODY                                        ;common
 TAIL-RECURSION-FLAG
 TAILP                                          ;common
 TAND
 TANH                                           ;common
 TAN                                            ;common
 TELNET
 TENTH                                          ;common
 TERMINAL-IO
 TERPRI
 THE                                            ;common
 THIRD                                          ;common
 THROW                                          ;common
 TIME-DIFFERENCE
 TIME-INCREMENT
 TIME-LESSP
 TIMES
 TIME                                   ;common
 TRACE-COMPILE-FLAG
 TRACE-OUTPUT
 TRACE                                          ;common
 TRANSLATED-PATHNAME
 TREE-EQUAL                                     ;common
 TRUE
 TRUENAME                                       ;common
 TRUNCATE                                       ;common
 TYI
 TYIPEEK
 TYO
 TYPE
 TYPE-OF                                        ;common
 TYPECASE                                       ;common
 TYPEP                                          ;common
 T                                              ;common
 UNADVISE
 UNADVISE-WITHIN
 UNBIND
 UNBOUND-FUNCTION                               ;for WHO-CALLS
 UNBREAKON
 UNCOMPILE
 UNDEFFLAVOR
 UNDEFMETHOD
 UNDEFUN
 UNDELETE-FILE
 UNDELETEF
 UNEXPORT                                       ;common
 UNINTERN                                       ;common
 UNION                                  ;common
 UNLESS                                         ;common
;UNMONITOR-VARIABLE
 UNREAD-CHAR                                    ;common
 UNSIGNED-BYTE                                  ;common
 UNSPECIAL                                      ;common -- needs hacking
 UNTRACE                                        ;common
 UNUSE-PACKAGE                                  ;common
 UNWIND-ALL
 UNWIND-PROTECT                                 ;common
 UNWIND-PROTECT-CASE
 UPDATE-SITE-CONFIGURATION-INFO
 UPPER-CASE-P                                   ;common
 USE-PACKAGE                                    ;common
 USER-HOMEDIR-PATHNAME                          ;common
 USER-ID
 USING-RESOURCE
 VALUE-CELL-LOCATION
 VALUES-LIST
 VALUES                                         ;common
 VARIABLE-BOUNDP
 VARIABLE-LOCATION
 VARIABLE-MAKUNBOUND
 VARIABLE                                       ;common
 VECTOR-POP                                     ;common
 VECTOR-PUSH-EXTEND                             ;common
 VECTOR-PUSH                                    ;common
 VECTORP                                        ;common
 VECTOR                                         ;common
 VIEWF
 WARN                                           ;common
 WHAT-FILES-CALL
 WHEN                                           ;common
 WHERE-IS
 WHO-CALLS
;WHO-USES                                       ;obsolete: use who-calls
 WHOIS
 WITH-INPUT-EDITING
 WITH-INPUT-FROM-STRING                         ;common
 WITH-LIST
 WITH-LIST*
 WITH-LOCK
 WITH-OPEN-FILE-CASE
 WITH-OPEN-FILE-RETRY
 WITH-OPEN-FILE-SEARCH
 WITH-OPEN-FILE                                 ;common
 WITH-OPEN-STREAM
 WITH-OPEN-STREAM-CASE
 WITH-OPEN-STRING
 WITH-OUTPUT-TO-STRING                          ;common
 WITH-SELF-VARIABLES-BOUND                      ;bletcherous: use anything else.
 WITH-STACK-LIST
 WITH-STACK-LIST*
 WITH-TIMEOUT
 WITHOUT-FLOATING-UNDERFLOW-TRAPS
 WITHOUT-INTERRUPTS
 WORKING-STORAGE-AREA
 WRITE-BYTE                                     ;common
 WRITE-CHAR                                     ;common
 WRITE-LINE                                     ;common
 WRITE-METER
 WRITE-STRING                                   ;common
 WRITE-TO-STRING                                ;common
 WRITE-USER-OPTIONS
 WRITE                                          ;common
 XCONS
 XCONS-IN-AREA
 XSTORE
 Y-OR-N-P                                       ;common
 YES-OR-NO-P                                    ;common
 ZEROP                                          ;common
 ZMAIL
 ZUNDERFLOW

 ) 'user:k-global)

(export '(
  &ALLOW-OTHER-KEYS                             ;common
  &AUX                                          ;common
  &BODY                                         ;common
  &ENVIRONMENT                                  ;common
  &EVAL
  &FUNCTIONAL
  &KEY                                          ;common
  &LIST-OF
  &LOCAL
  &OPTIONAL                                     ;common
  &QUOTE
  &REST                                         ;common
  &SPECIAL
  &WHOLE

 *package*
 *PRINT-RADIX*                                  ;common
 *READ-BASE*


 
 
 
 ***                                            ;common
 **                                             ;common
 *                                              ;common
 +++                                            ;common
 ++                                             ;common
 +                                              ;common

 -                                              ;common

 ///                                            ;common
 //                                             ;common
 /=                                             ;common
 /                                              ;common -- incompatible
 1+                                             ;common
 1-                                             ;common
;<-                                             ;So long, losers
;<-AS
;<<--
 <=                                             ;common
 <                                              ;common
 =                                              ;common
 >=                                             ;common
 >                                              ;common
 \\
 \\\\
 ^
 @DEFINE
 ABS                                            ;common
 ACONS                                          ;common
 ACOSH                                          ;common
 ACOS                                           ;common
 ADD-INITIALIZATION
 ADD1
 ADJOIN                                         ;common
 ADJUST-ARRAY-SIZE
 ADJUST-ARRAY                                   ;common
 ADJUSTABLE-ARRAY-P                             ;common
 ADVISE
 ADVISE-WITHIN
 ALL-SPECIAL-SWITCH
 ALLOCATE-RESOURCE
;ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
 ALOC
 ALPHA-CHAR-P                                   ;common
 ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON
 ALPHALESSP
 ALPHANUMERICP                                  ;common
 AND                                            ;common
 AP-1
 AP-1-FORCE
 AP-2
 AP-3
 AP-LEADER
 APPEND-TO-ARRAY
 APPEND                                         ;common
 APPLYHOOK                                      ;common
 APPLY                                          ;common
 APROPOS-LIST                                   ;common
 APROPOS                                        ;common
 AR-1
 AR-1-FORCE
 AR-2
 AR-2-REVERSE
 AR-3
 AREA-LIST
 AREA-NAME
 AREA-NUMBER
 AREF                                   ;common -- incompatible
 ARG
 ARGLIST
 ARGS
 ARGS-INFO
 AROUND-METHOD-CONTINUE
 ARRAY-\#-DIMS                                  ;obsolete
 ARRAY-ACTIVE-LENGTH
 ARRAY-BITS-PER-ELEMENT
 ARRAY-DIMENSION-LIMIT                          ;common
 ARRAY-DIMENSION-N
 ARRAY-DIMENSIONS                               ;common
 ARRAY-DIMENSION                                ;common
 ARRAY-DISPLACED-P                              ;common
 ARRAY-ELEMENT-SIZE
 ARRAY-ELEMENT-TYPE                             ;common
 ARRAY-ELEMENTS-PER-Q
 ARRAY-GROW
 ARRAY-HAS-FILL-POINTER-P                       ;common
 ARRAY-HAS-LEADER-P
 ARRAY-IN-BOUNDS-P                              ;common
 ARRAY-INDEX-ORDER
 ARRAY-INDEXED-P
 ARRAY-INDIRECT-P
 ARRAY-INITIALIZE
 ARRAY-LEADER
 ARRAY-LEADER-LENGTH
 ARRAY-LENGTH
 ARRAY-POP
 ARRAY-PUSH
 ARRAY-PUSH-EXTEND
 array-push-portion-extend
 ARRAY-RANK-LIMIT                               ;common
 ARRAY-RANK                                     ;common
 ARRAY-ROW-MAJOR-INDEX                          ;common
 ARRAY-TOTAL-SIZE-LIMIT
 ARRAY-TOTAL-SIZE                               ;common
 ARRAY-TYPE
 ARRAY-TYPES
 ARRAYCALL                                      ;maclisp
 ARRAYDIMS
 ARRAYP                                         ;common
 ARRAY                                          ;common
 AS-1
 AS-1-FORCE
 AS-2
 AS-2-REVERSE
 AS-3
 ASCII
 ASET
 ASH                                            ;common
 ASINH                                          ;common
 ASIN                                           ;common
 ASS
 ASSERT                                         ;common
 ASSIGN-ALTERNATE                               ;glorp. vestigal crocks from cold-load
 ASSIGN-VALUES                                  ; ditto
 ASSIGN-VALUES-INIT-DELTA                       ; ditto. sigh.
 ASSOC-IF-NOT                                   ;common
 ASSOC-IF                                       ;common
 ASSOC                                  ;common -- incompatible
 ASSQ
 ATAN2
 ATANH                                          ;common
 ATAN                                   ;common -- incompatible
 ATOM
 BACK-TRANSLATED-PATHNAME                       ;>>???
 BASE
 BEEP
 BIGNUM                                         ;common
 BIGP
 BIND
 BIT-ANDC1                                      ;common
 BIT-ANDC2                                      ;common
 BIT-AND                                        ;common
 BIT-EQV                                        ;common
 BIT-IOR                                        ;common
 BIT-NAND                                       ;common
 BIT-NOR                                        ;common
 BIT-NOT                                        ;common
 BIT-ORC1                                       ;common
 BIT-ORC2                                       ;common
 BIT-TEST
 BIT-VECTOR-P                                   ;common
 BIT-VECTOR                                     ;common
 BIT-XOR                                        ;common
 BITBLT
 BIT                                            ;common
 BLOCK                                          ;common
 BOOLE-1                                        ;common
 BOOLE-2                                        ;common
 BOOLE-ANDC1                                    ;common
 BOOLE-ANDC2                                    ;common
 BOOLE-AND                                      ;common
 BOOLE-C1                                       ;common
 BOOLE-C2                                       ;common
 BOOLE-CLR                                      ;common
 BOOLE-EQV                                      ;common
 BOOLE-IOR                                      ;common
 BOOLE-NAND                                     ;common
 BOOLE-NOR                                      ;common
 BOOLE-ORC1                                     ;common
 BOOLE-ORC2                                     ;common
 BOOLE-OR                                       ;common
 BOOLE-SET                                      ;common
 BOOLE-XOR                                      ;common
 BOOLE                                          ;common
 BOTH-CASE-P                                    ;common
 BOUNDP-GLOBALLY
 BOUNDP-IN-CLOSURE
 BOUNDP                                         ;common
 BREAKON
 BREAK                                          ;common
 BUG
 BUTLAST                                        ;common
 BYTE-POSITION                                  ;common
 BYTE-SIZE                                      ;common
 BYTE                                           ;common

 CALL-ARGUMENTS-LIMIT                           ;common
                                                ;common
 CASEQ
 CASE                                           ;common
 CATCH-ALL
 CATCH-CONTINUATION
 CATCH-CONTINUATION-IF
 CATCH-ERROR
 CATCH-ERROR-RESTART
 CATCH-ERROR-RESTART-EXPLICIT-IF
 CATCH-ERROR-RESTART-IF
 CATCH                                          ;common
 CCASE                                          ;common
                                                ;common
 CEILING                                        ;common
 CERROR                                         ;common
 CHAR
 CHAR
 CHAR
 CHAR-BITS-LIMIT                                ;common
 CHAR-BITS                                      ;common
 CHAR-BIT                                       ;common
 CHAR-CODE-LIMIT                                ;common
 CHAR-CODE                                      ;common
 CHAR-CONTROL-BIT                               ;common
 CHAR-DOWNCASE                                  ;common
 CHAR-EQUAL                                     ;common
 CHAR-FLIPCASE
 CHAR-FONT-LIMIT                                ;common
 CHAR-FONT                                      ;common
 CHAR-GREATERP                                  ;common
 CHAR-HYPER-BIT                                 ;common
 CHAR-INT                                       ;common
 CHAR-LESSP                                     ;common
 CHAR-META-BIT                                  ;common
 CHAR-NAME                                      ;common
 CHAR-NOT-EQUAL                                 ;common
 CHAR-NOT-GREATERP                              ;common
 CHAR-NOT-LESSP                                 ;common
 CHAR-STANDARD
 CHAR-SUPER-BIT                                 ;common
 CHAR-UPCASE                                    ;common
 CHAR/=                                         ;common
 CHAR<=                                         ;common
 CHAR<                                          ;common
 CHAR=                                          ;common
 CHAR>=                                         ;common
 CHAR>                                          ;common
 CHARACTERP                                     ;common
 CHARACTER                                      ;common -- incompatible
 CHAR                                           ;common
 CHECK-ARG
 CHECK-ARG-TYPE
 CHECK-TYPE                                     ;common
 CHOOSE-USER-OPTIONS
 CIRCULAR-LIST
 CIS                                            ;common
 CLEAR-INPUT                                    ;common
 CLEAR-MAR
 CLEAR-OUTPUT                                   ;common
 CLEAR-RESOURCE
 CLOSE                                  ;common -- incompatible
 CLOSURE
 CLOSURE-ALIST
 CLOSURE-BINDINGS
 CLOSURE-FUNCTION
 CLOSURE-VARIABLES
 CLOSUREP
 CLRHASH-EQUAL
 CLRHASH                                        ;common
 CODE-CHAR                                      ;common
 COERCE                                         ;common
 COMMENT
 COMMONP                                        ;common
 COMMON                                         ;common
 COMMON-LISP
 COMPILATION-SPEED                              ;common
 COMPILE-ENCAPSULATIONS
 COMPILE-ENCAPSULATIONS-FLAG
 COMPILE-FILE-ALIST
 COMPILE-FILE                                   ;common
 COMPILE-FLAVOR-METHODS
 COMPILE-LAMBDA
 COMPILED-FUNCTION-P                            ;common
 COMPILED-FUNCTION                              ;common
 COMPILER-LET                                   ;common
 COMPILE                                        ;common
 COMPLEXP                                       ;common
 COMPLEX                                        ;common
 CONCATENATE                                    ;common
 COND-EVERY
 CONDITION
 CONDITION-BIND
 CONDITION-BIND-DEFAULT
 CONDITION-BIND-DEFAULT-IF
 CONDITION-BIND-IF
 CONDITION-CALL
 CONDITION-CALL-IF
 CONDITION-CASE
 CONDITION-CASE-IF
 CONDITION-PSETQ
 CONDITION-RESUME
 CONDITION-RESUME-IF
 CONDITION-TYPEP
 COND                                           ;common
 CONJUGATE                                      ;common
 CONS-IN-AREA
 CONSP                                          ;common
 CONSTANTP                                      ;common
 CONTENTS
 CONTINUE-WHOPPER
 COPY-ALIST                                     ;common
 COPY-ARRAY-CONTENTS
 COPY-ARRAY-CONTENTS-AND-LEADER
 COPY-ARRAY-PORTION
 COPY-CLOSURE
 COPY-FILE
 COPY-LIST                                      ;common
 COPY-READTABLE                                 ;common
 COPY-SEQ                                       ;common
 COPY-SYMBOL                                    ;common
 COPY-SYNTAX
 COPY-TREE                                      ;common
 COPYALIST
 COPYLIST
 COPYLIST*
 COPYSYMBOL
 COPYTREE
 COPYTREE-SHARE
 COSD
 COSH                                           ;common
 COS                                            ;common
 COUNT-IF-NOT                                   ;common
 COUNT-IF                                       ;common
 COUNT                                          ;common
 CTYPECASE                                      ;common
 CURRENT-PROCESS
 CURRENT-STACK-GROUP
 CURRENT-STACK-GROUP-RESUMER
 CURSORPOS
 DATA-TYPE
 DBG
 DEALLOCATE-RESOURCE
 DEALLOCATE-WHOLE-RESOURCE
 DEBUG-IO
 DEBUGGING-INFO
 DECF                                           ;common
 DECLARE-FLAVOR-INSTANCE-VARIABLES
 DECLARATION                                    ;common?
 DECLARE                                        ;common -- needs hacking
 DECODE-FLOAT                                   ;common
 DECODE-UNIVERSAL-TIME                          ;common
 DEF
 DEFAULT-CONS-AREA
 DEFCONST
 DEFCONSTANT                                    ;common
 DEFDECL
 DEFF
 DEFF-MACRO
 DEFFLAVOR
 DEFFUNCTION
 DEFGENERIC
 DEFINE-LOOP-MACRO
 DEFINE-LOOP-PATH
 DEFINE-LOOP-SEQUENCE-PATH
 DEFINE-MODIFY-MACRO                            ;common
 DEFINE-PROMPT-AND-READ-TYPE
 DEFINE-SETF-METHOD                             ;common
 DEFINE-SITE-ALIST-USER-OPTION
 DEFINE-SITE-HOST-LIST
 DEFINE-SITE-USER-OPTION
 DEFINE-SITE-VARIABLE
 DEFINE-USER-OPTION
 DEFINE-USER-OPTION-ALIST
 DEFLAMBDA-MACRO
 DEFLAMBDA-MACRO-DISPLACE
 DEFLOCF
 DEFMACRO-DISPLACE
 DEFMACRO                                       ;common
 DEFMETHOD
 DEFMETHOD-INSTANCE
 DEFPACKAGE
 DEFPARAMETER                                   ;common
 DEFPROP
 DEFRESOURCE
 DEFSELECT
 DEFSELECT-INCREMENTAL
 DEFSETF                                        ;common
 DEFSIGNAL
 DEFSIGNAL-EXPLICIT
 DEFSTRUCT-DEFINE-TYPE
 DEFSTRUCT                                      ;common -- incompatible
 DEFSUBST
 DEFSYSTEM
 DEFTYPE                                        ;common
 DEFUN-METHOD
 DEFUNP
 DEFUN                                          ;common
 DEFVAR                                         ;common
 DEFVAR-SITE-ALIST-USER-OPTION
 DEFVAR-SITE-USER-OPTION
 defvar-resettable
 defvar-standard
 DEFVAR-USER-OPTION
 DEFWHOPPER
 DEFWHOPPER-SUBST
 DEFWINDOW-RESOURCE
 DEFWRAPPER
 DEL
 DEL-IF
 DEL-IF-NOT
 DELETE-DUPLICATES
 DELETE-FILE                                    ;common
 DELETE-IF-NOT                                  ;common
 DELETE-IF                                      ;common
 DELETE-INITIALIZATION
 DELETEF
 DELETE                                 ;common -- incompatible
 DELQ
 DENOMINATOR                                    ;common
 DEPOSIT-BYTE
 DEPOSIT-FIELD                                  ;common
 DESCRIBE-AREA
 DESCRIBE-DEFSTRUCT
 DESCRIBE-FLAVOR
 DESCRIBE-PACKAGE
 DESCRIBE-PARTITION
 DESCRIBE-SYSTEM
 DESCRIBE                                       ;common
 DESTRUCTURING-BIND
 DIFFERENCE
 DIGIT-CHAR-P                                   ;common
 DIGIT-CHAR                                     ;common
 DIRECTORY-NAMESTRING                           ;common
 DIRECTORY                                      ;common
 DIRED
 DISASSEMBLE                                    ;common
 DISK-RESTORE
 DISK-SAVE
 DISPATCH
 DISPLACE
 DO*-NAMED
 DO*                                            ;common
 DO-ALL-SYMBOLS                                 ;common
 DO-EXTERNAL-SYMBOLS                            ;common
 DO-FOREVER
 DO-LOCAL-EXTERNAL-SYMBOLS
 DO-LOCAL-SYMBOLS
 DO-NAMED
 DO-SYMBOLS                                     ;common
 DOCUMENTATION                                  ;common
 DOLIST                                         ;common
 DONT-OPTIMIZE
 DOTIMES                                        ;common
 DOUBLE-FLOAT-EPSILON                           ;common
 DOUBLE-FLOAT-NEGATIVE-EPSILON                  ;common
 DOUBLE-FLOAT                                   ;common
 DO                                             ;common
 DPB                                            ;common
 DRIBBLE-ALL
;DRIBBLE-END
;DRIBBLE-START
 DRIBBLE                                        ;common
 DUMP-FORMS-TO-FILE
 ECASE                                          ;common
 ED                                             ;common
 EH
 EH-ARG
 EH-ERROR
 EH-FRAME
 EH-FUN
 EH-LOC
 EH-SG
 EH-VAL
 EIGHTH                                         ;common
 ELT                                            ;common
 ENCODE-UNIVERSAL-TIME                          ;common
 ENDP                                           ;common
 ENOUGH-NAMESTRING                              ;common
 ENTITY
 ENTITYP
 EQL                                            ;common
 EQUALP                                         ;common
 EQUAL                                          ;common
 EQ                                             ;common
 ERR
 ERROR-OUTPUT
 ERROR-RESTART
 ERROR-RESTART-LOOP
 ERRORP
 ERROR                                          ;common
 ERRSET
 ETYPECASE                                      ;common
 EVAL-WHEN                                      ;common
 EVALHOOK                                       ;common
 EVAL                                           ;common
 EVENP                                          ;common
 EVERY                                  ;common -- incompatible
 EXPLODE
 EXPLODEC
 EXPLODEN
 EXPORT                                         ;common
 EXPR
 EXPT                                           ;common
 EXP                                            ;common
 FALSE
 FASD-UPDATE-FILE
 FASL-APPEND
 FASLOAD
 FBOUNDP                                        ;common
 FCEILING                                       ;common
 FDEFINE
 FDEFINEDP
 FDEFINITION
 FED
 FERROR
 FEXPR
 FFLOOR                                         ;common
 FIFTH                                          ;common
 FILE-AUTHOR                                    ;common
 FILE-LENGTH                                    ;common
 FILE-NAMESTRING                                ;common
 FILE-POSITION                                  ;common
 FILE-RETRY-NEW-PATHNAME
 FILE-RETRY-NEW-PATHNAME-IF
 FILE-WRITE-DATE                                ;common
 FILL-POINTER                                   ;common
 FILLARRAY
 FILL                                           ;common
 FIND-ALL-SYMBOLS
 FIND-EXTERNAL-SYMBOL
 FIND-IF-NOT                                    ;common
 FIND-IF                                        ;common
 FIND-PACKAGE                                   ;common
 FIND-POSITION-IN-LIST
;FIND-POSITION-IN-LIST-EQUAL
 FIND-SYMBOL                                    ;common
 FIND                                           ;common
 FINGER
 FINISH-OUTPUT                                  ;common
 FIRSTN
 FIRST                                          ;common
 FIX
 FIXNUMP
 FIXNUM                                         ;common
 FIXP
 FIXR
 FLATC
 FLATSIZE
 FLAVOR-ALLOWS-INIT-KEYWORD-P
 FLET                                           ;common
 FLOAT-DIGITS                                   ;common
 FLOAT-PRECISION                                ;common
 FLOAT-RADIX                                    ;common
 FLOAT-SIGN                                     ;common
 FLOATP                                         ;common
 FLOAT                                          ;common
 FLONUM
 FLONUMP
 FLOOR                                          ;common
 FMAKUNBOUND                                    ;common
 FOLLOW-CELL-FORWARDING
 FOLLOW-STRUCTURE-FORWARDING
 FONT                                           ;sigh... why are these global?
 FONT-BASELINE
 FONT-BLINKER-HEIGHT
 FONT-BLINKER-WIDTH
 FONT-CHAR-HEIGHT
 FONT-CHAR-WIDTH
 FONT-CHAR-WIDTH-TABLE
 FONT-CHARS-EXIST-TABLE
 FONT-FILL-POINTER
 FONT-INDEXING-TABLE
 FONT-LEFT-KERN-TABLE
 FONT-NAME
 FONT-NEXT-PLANE
 FONT-RASTER-HEIGHT
 FONT-RASTER-WIDTH
 FONT-RASTERS-PER-WORD
 FONT-WORDS-PER-CHAR
 FORCE-OUTPUT                                   ;common
 FORMAT-MACRO
 FORMAT                                         ;common
 FORWARD-VALUE-CELL
 FOURTH                                         ;common
 FQUERY
 FRESH-LINE                                     ;common
 FROUND                                         ;common
 FSET
 FSET-CAREFULLY
 FSIGNAL
 FSYMEVAL
 FTRUNCATE                                      ;common
 FTYPE                                          ;common
 FUNCALL-SELF
 FUNCALL-WITH-MAPPING-TABLE
 FUNCALL                                        ;common
 FUNCTION-CELL-LOCATION
 FUNCTION-DOCUMENTATION
 FUNCTION-NAME
 FUNCTIONP                                      ;incompatible.
 FUNCTION                                       ;common
 FUNDEFINE
 G-L-P
;GC-IMMEDIATELY
;GC-OFF
;GC-ON
;GC-STATUS
 GCD                                            ;common
 GENSYM                                         ;common
 GENTEMP                                        ;common
 GET-DECODED-TIME                               ;common
 GET-DISPATCH-MACRO-CHARACTER                   ;common
 GET-FROM-ALTERNATING-LIST                      ;obsolete
 GET-HANDLER-FOR
 GET-INTERNAL-REAL-TIME                         ;common
 GET-INTERNAL-RUN-TIME                          ;common
 GET-LIST-POINTER-INTO-ARRAY                    ;obsolete
 GET-LOCATIVE-POINTER-INTO-ARRAY                ;obsolete
 GET-MACRO-CHARACTER                            ;common
 GET-OUTPUT-STREAM-STRING                       ;common
 GET-PNAME
 GET-PROPERTIES                                 ;common
 GET-SETF-METHOD-MULTIPLE-VALUE                 ;common
 GET-SETF-METHOD                                ;common
 GET-SITE-OPTION
 GET-UNIVERSAL-TIME                             ;common
 GETCHARN                                       ;maclisp
 GETCHAR                                        ;maclisp
 GETDECL
 GETF                                           ;common
 GETF-from-area
 GETHASH-EQUAL
 gethash
 GETHASH                                        ;common
 GETL
 GET                                            ;common
 get-from-area
 GLOBALIZE
 GO                                             ;common
 GRAPHIC-CHAR-P                                 ;common
 GREATERP                                       ;common
 GRIND-TOP-LEVEL
 GRINDEF
 HAIPART
 HARDCOPY-BIT-ARRAY
 HARDCOPY-FILE
 HARDCOPY-STATUS
 HARDCOPY-STREAM
 HASH-TABLE-COUNT                               ;common
 HASH-TABLE-P                                   ;common
 HASH-TABLE                                     ;common
 HAULONG
 HOST-NAMESTRING                                ;common
 HOSTAT
 IBASE
 IDENTITY
 if
 IF-FOR-LISPM                                   ;obsolete
 IF-FOR-MACLISP                                 ;obsolete
 IF-FOR-MACLISP-ELSE-LISPM                      ;obsolete
 IF-IN-CADR                                     ;obsolete
 IF-IN-CADR-ELSE-LAMBDA                         ;obsolete
 IF-IN-LAMBDA                                   ;obsolete
 IF-IN-LAMBDA-ELSE-CADR                         ;obsolete
 IF-IN-LISPM                                    ;obsolete
 IF-IN-MACLISP                                  ;obsolete
 IF                                             ;incompatible
 IGNORE-ERRORS
 IGNORE                                         ;common
 IMAGPART                                       ;common
 IMPLODE                                        ;maclisp
 IMPORT                                         ;common
 IN-PACKAGE                                     ;common
 INCF                                           ;common
 INHIBIT-FDEFINE-WARNINGS
 INHIBIT-IDLE-SCAVENGING-FLAG
 INHIBIT-SCAVENGING-FLAG
 INHIBIT-SCHEDULING-FLAG
 INHIBIT-STYLE-WARNINGS
 INHIBIT-STYLE-WARNINGS-SWITCH
;INIT-FILE-PATHNAME
 INITIALIZATIONS
 INLINE
 INPUT-STREAM-P                                 ;common
 INSPECT                                        ;common
 INSTANCE
 INSTANCEP
;INSTANCE-VARIABLE-BOUNDP
 INSTANTIATE-FLAVOR
 INT-CHAR                                       ;common
 INTEGER-DECODE-FLOAT                           ;common
 INTEGER-LENGTH                                 ;common
 INTEGERP                                       ;common
 INTEGER                                        ;common
 INTERN-LOCAL
 INTERN-LOCAL-SOFT
 INTERN-SOFT
 INTERNAL-TIME-UNITS-PER-SECOND                 ;common
 INTERN                                         ;common
 INTERSECTION                           ;common -- incompatible
 ISQRT                                          ;common
 KBD-CHAR-AVAILABLE
 KBD-TYI
 KBD-TYI-NO-HANG
 KEYWORD-EXTRACT
 KEYWORDP                                       ;common
 KEYWORD                                        ;common
 KILL-PACKAGE
 LABELS                                         ;common
 LAMBDA-LIST-KEYWORDS                           ;common
 LAMBDA-MACRO
 LAMBDA-PARAMETERS-LIMIT                        ;common
 LAMBDA                                         ;common
 LAST                                           ;common
 LCM                                            ;common
 LDB-TEST                                       ;common
 LDB                                            ;common
 LDIFF                                          ;common
 LEAST-NEGATIVE-DOUBLE-FLOAT                    ;common
 LEAST-NEGATIVE-LONG-FLOAT                      ;common
 LEAST-NEGATIVE-SHORT-FLOAT                     ;common
 LEAST-NEGATIVE-SINGLE-FLOAT                    ;common
 LEAST-POSITIVE-DOUBLE-FLOAT                    ;common
 LEAST-POSITIVE-LONG-FLOAT                      ;common
 LEAST-POSITIVE-SHORT-FLOAT                     ;common
 LEAST-POSITIVE-SINGLE-FLOAT                    ;common
 LENGTH                                         ;common
 LESSP
 LET*                                           ;common
 LET-CLOSED
 LET-GLOBALLY
 LET-GLOBALLY-IF
 LET-IF
 LET                                            ;common
 LETF
 LETF*
 LETF-IF
 LEXPR-CONTINUE-WHOPPER
 LEXPR-FUNCALL
 LEXPR-FUNCALL-SELF
 LEXPR-FUNCALL-WITH-MAPPING-TABLE
 LEXPR-SEND
 LEXPR-SEND-IF-HANDLES
;LISP-CRASH-LIST
 LISP-IMPLEMENTATION-TYPE                       ;common
 LISP-IMPLEMENTATION-VERSION                    ;common
;LISP-REINITIALIZE
 LIST*-IN-AREA
 LIST*                                          ;common
 LIST-ALL-PACKAGES                              ;common
 LIST-ARRAY-LEADER
 LIST-IN-AREA
 LIST-LENGTH                                    ;common
 LIST-MATCH-P
 LISTARRAY
 LISTEN                                         ;common
 LISTF
 LISTF
 LISTIFY
 LISTP                                  ;common -- incompatible
 LIST                                           ;common
 LOAD-AND-SAVE-PATCHES
 LOAD-BYTE
 LOAD-FILE-ALIST
 LOAD-FILE-LIST
 LOAD-PATCHES
 LOAD                                           ;common
 LOCAL-DECLARATIONS
 LOCAL-DECLARE
 LOCALLY                                        ;common
 LOCATE-IN-CLOSURE
 LOCATE-IN-INSTANCE
 LOCATION-BOUNDP
 LOCATION-CONTENTS
 LOCATION-MAKUNBOUND
 LOCATIVE
 LOCATIVEP
 LOCF
 LOG1
 LOGANDC1                                       ;common
 LOGANDC2                                       ;common
 LOGAND                                         ;common
 LOGBITP                                        ;common
 LOGCOUNT                                       ;common
 LOGEQV                                         ;common
 LOGIN
 LOGIN-EVAL
 LOGIN-FDEFINE
 LOGIN-FORMS
 LOGIN-SETQ
 LOGIOR                                         ;common
 LOGNAND                                        ;common
 LOGNOR                                         ;common
 LOGNOT                                         ;common
 LOGORC1                                        ;common
 LOGORC2                                        ;common
 LOGOUT
 LOGOUT-LIST
 LOGTEST                                        ;common
 LOGXOR                                         ;common
 LOG                                            ;common
 LONG-FLOAT-EPSILON                             ;common
 LONG-FLOAT-NEGATIVE-EPSILON                    ;common
 LONG-FLOAT                                     ;common
 LONG-SITE-NAME                                 ;common
 LOOP-FINISH
 LOOP                                           ;common
 LOWER-CASE-P                                   ;common
 LSH
 MACHINE-INSTANCE                               ;common
 MACHINE-TYPE                                   ;common
 MACHINE-VERSION                                ;common
 MACRO
 MACRO-COMPILED-PROGRAM
 MACRO-FUNCTION                                 ;common
 MACROEXPAND-1                          ;incompatible
 MACROEXPAND-ALL
 MACROEXPAND                                    ;incompatible
 MACROLET                                       ;common
 MAIL
 MAKE-AREA
 MAKE-ARRAY-INTO-NAMED-STRUCTURE
 MAKE-ARRAY                                     ;incompatible
 MAKE-BROADCAST-STREAM                          ;common
 MAKE-CHAR                                      ;common
 MAKE-CONCATENATED-STREAM                       ;common
 MAKE-CONDITION
 MAKE-DISPATCH-MACRO-CHARACTER                  ;common
 MAKE-ECHO-STREAM                               ;common
 MAKE-EQUAL-HASH-TABLE
 MAKE-HARDCOPY-STREAM
 MAKE-HASH-TABLE                                ;common
 make-heap
 MAKE-INSTANCE
 MAKE-LIST                                      ;common
 MAKE-PACKAGE                                   ;common
 MAKE-PATHNAME                                  ;common
 MAKE-PIXEL-ARRAY
 MAKE-PLANE
 MAKE-PROCESS
 MAKE-RANDOM-STATE                              ;common
 MAKE-SEQUENCE                                  ;common
 MAKE-STACK-GROUP
 MAKE-STRING-INPUT-STREAM                       ;common
 MAKE-STRING-OUTPUT-STREAM                      ;common
 MAKE-STRING                                    ;common
 MAKE-SYMBOL                                    ;common
 MAKE-SYN-STREAM
 MAKE-SYNONYM-STREAM                            ;common
 MAKE-SYSTEM
 MAKE-TWO-WAY-STREAM                            ;common
 MAKNAM
 MAKUNBOUND-GLOBALLY
 MAKUNBOUND-IN-CLOSURE
 MAKUNBOUND                                     ;common
 MAP-RESOURCE
 MAPATOMS-ALL                                   ;obsolete
 MAPATOMS                                       ;obsolete
 MAPCAN                                         ;common
 MAPCAR                                         ;common
 MAPCON                                         ;common
 MAPC                                           ;common
 MAPHASH-EQUAL
 MAPHASH-RETURN
 MAPHASH                                        ;common
 MAPLIST                                        ;common
 MAPL                                           ;common
 MAP                                            ;common -- incompatible
 MAR-BREAK
 MAR-MODE
 MASK-FIELD                                     ;common
 MAXPREFIX
 MAXSUFFIX
 MAX                                            ;common
 MEM
 MEMASS
 MEMBER-IF-NOT                                  ;common
 MEMBER-IF                                      ;common
 MEMBER                                 ;common -- incompatible
 MEMQ
 MERGE-PATHNAMES                                ;common
 MERGE                                          ;common
 MEXP
 MICROCODE-FUNCTION
 MINUS
 MINUSP                                         ;common
 MIN                                            ;common
 MISMATCH                                       ;common
 MOD                                            ;common
;MONITOR-VARIABLE
 MOST-NEGATIVE-DOUBLE-FLOAT                     ;common
 MOST-NEGATIVE-FIXNUM                           ;common
 MOST-NEGATIVE-LONG-FLOAT                       ;common
 MOST-NEGATIVE-SHORT-FLOAT                      ;common
 MOST-NEGATIVE-SINGLE-FLOAT                     ;common
 MOST-POSITIVE-DOUBLE-FLOAT                     ;common
 MOST-POSITIVE-FIXNUM                           ;common
 MOST-POSITIVE-LONG-FLOAT                       ;common
 MOST-POSITIVE-SHORT-FLOAT                      ;common
 MOST-POSITIVE-SINGLE-FLOAT                     ;common
 MULTIPLE-VALUE
 MULTIPLE-VALUE-BIND                            ;common
 MULTIPLE-VALUE-CALL                            ;common
 MULTIPLE-CERROR
 MULTIPLE-VALUE-LIST                            ;common
 MULTIPLE-VALUE-PROG1                           ;common
;MULTIPLE-VALUE-RETURN
 MULTIPLE-VALUE-SETQ                            ;common
 MULTIPLE-VALUES-LIMIT                          ;common
 NAME-CHAR                                      ;common
 NAMED-LAMBDA
 NAMED-STRUCTURE
 NAMED-STRUCTURE-INVOKE
 NAMED-STRUCTURE-P
 NAMED-STRUCTURE-SYMBOL
 NAMED-SUBST
 NAMESTRING                                     ;common
 NBUTLAST                                       ;common
 NCONC                                          ;common
 NCONS
 NCONS-IN-AREA
 NEQ
 NIL                                            ;common
 NINTERSECTION                          ;common -- incompatible
 NINTH                                          ;common
 NLEFT
 NLISTP
 NON-COMPLEX-NUMBER
 NOTANY                                         ;common
 NOTEVERY                                       ;common
 NOTINLINE
 NOT                                            ;common
 NRECONC                                        ;common
 NREVERSE                                       ;common
 NSET-DIFFERENCE                                ;common
 NSET-EXCLUSIVE-OR                              ;common
 NSTRING-CAPITALIZE                             ;common
 NSTRING-DOWNCASE                               ;common
 NSTRING-UPCASE                                 ;common
 NSUBLIS                                        ;common
 NSUBST-IF-NOT                                  ;common
 NSUBST-IF                                      ;common
 NSUBSTITUTE-IF-NOT                             ;common
 NSUBSTITUTE-IF                                 ;common
 NSUBSTITUTE                                    ;common
 NSUBSTRING
 NSUBST                                         ;common
 NSYMBOLP
 NTH-SAFE
 NTH-VALUE
 NTHCDR-SAFE
 NTHCDR                                         ;common
 NTH                                            ;common
 NULL-MACRO
 NULL                                           ;common
 NUMBER-INTO-ARRAY
 NUMBERP                                        ;common
 NUMBER                                         ;common
 NUMERATOR                                      ;common
 NUNION                                 ;common  -- incompatible
;OBSOLETE-FUNCTION-WARNING-SWITCH
 ODDP                                           ;common
 ONCE-ONLY
 OPEN-CODE-MAP-SWITCH
 OPEN                                           ;common
 OPERATION-HANDLED-P
 OPTIMIZE
 OR                                             ;common
 OTHERWISE                                      ;common
 OUTPUT-STREAM-P                                ;common
 PACKAGE-CELL-LOCATION
 PACKAGE-DECLARE                                ;obsolete
 PACKAGE-EXTERNAL-SYMBOLS
 PACKAGE-NAME                                   ;common
 PACKAGE-NICKNAMES                              ;common
 PACKAGE-PREFIX-PRINT-NAME
 PACKAGE-SHADOWING-SYMBOLS                      ;common
 PACKAGE-USE-LIST                               ;common
 PACKAGE-USED-BY-LIST                           ;common
 PACKAGEP                                       ;common
 PACKAGE                                        ;common
 PAIRLIS                                        ;common
 PARSE-INTEGER                                  ;common
 PARSE-NAMESTRING                               ;common
 PARSE-NUMBER
 PATHNAME-DEVICE                                ;common
 PATHNAME-DIRECTORY                             ;common
 PATHNAME-HOST                                  ;common
 PATHNAME-NAME                                  ;common
 PATHNAME-PLIST
 PATHNAME-TYPE                                  ;common
 PATHNAME-VERSION                               ;common
 PATHNAMEP                                      ;common
 PATHNAME                                       ;common
 PEEK
 PEEK-CHAR                                      ;common
 PERMANENT-STORAGE-AREA
 PHASE                                          ;common
 PIXEL-ARRAY-HEIGHT
 PIXEL-ARRAY-WIDTH
 PI                                             ;common
 PKG-BIND
 PKG-CREATE-PACKAGE
 PKG-FIND-PACKAGE
 PKG-GLOBAL-PACKAGE
 PKG-GOTO
 PKG-GOTO-GLOBALLY
 PKG-KEYWORD-PACKAGE
 PKG-KILL                                       ;obsolete: use kill-package
 PKG-SYSTEM-PACKAGE
 PLANE-AREF
 PLANE-ASET
 PLANE-DEFAULT
 PLANE-EXTENSION
 PLANE-ORIGIN
 PLANE-REF                                      ;obsolete: use plane-aref
 PLANE-STORE                                    ;obsolete: use plane-aset
 PLIST
 PLUS
 PLUSP                                          ;common
 POP                                            ;common
 POSITION-IF-NOT                                ;common
 POSITION-IF                                    ;common
 POSITION                                       ;common
 PPRINT                                         ;common
 PRIN1-THEN-SPACE
 PRIN1-TO-STRING                                ;common
 PRIN1                                          ;common
 PRINC-TO-STRING                                ;common
 PRINC                                          ;common
 PRINLENGTH
 PRINLEVEL
 PRINT-DISK-LABEL
 PRINT-ERROR-MODE
 PRINT-HERALD
 PRINT-LOADED-BAND
 PRINT-LOGIN-HISTORY
 PRINT-NOTIFICATIONS
 PRINT-SENDS
 PRINT-SYSTEM-MODIFICATIONS
 PRINT                                          ;common
 PROBE-FILE                                     ;common
 PROBEF
 PROCESS-ALLOW-SCHEDULE
 PROCESS-CREATE
 PROCESS-DISABLE
 PROCESS-ENABLE
 PROCESS-ERROR-STOP-PROCESSES
 PROCESS-INITIAL-FORM
 PROCESS-INITIAL-STACK-GROUP
 PROCESS-LOCK
 PROCESS-NAME
 PROCESS-PLIST
 PROCESS-PRESET
 PROCESS-RESET
 PROCESS-RESET-AND-ENABLE
 PROCESS-RUN-FUNCTION
 PROCESS-RUN-RESTARTABLE-FUNCTION
 PROCESS-RUN-TEMPORARY-FUNCTION
 PROCESS-SLEEP
 PROCESS-STACK-GROUP
 PROCESS-UNLOCK
 PROCESS-WAIT
 PROCESS-WAIT-ARGUMENT-LIST
 PROCESS-WAIT-FUNCTION
 PROCESS-WAIT-WITH-TIMEOUT
 PROCESS-WHOSTATE                               ;obsolete
 PROCLAIM                                       ;common
 PROG*                                          ;common
 PROG1                                          ;common
 PROG2                                          ;common
 PROGN                                          ;common
 PROGV                                          ;common
 PROGW
 PROG                                           ;common
 PROMPT-AND-READ
 PROPERTY-CELL-LOCATION
 PROVIDE                                        ;common
 PSETF                                          ;common
 PSETQ                                          ;common
 PUSHNEW                                        ;common
 PUSH                                           ;common
 PUTDECL
 PUTHASH
 PUTHASH-EQUAL
 PUTPROP
 PUTPROP-IN-AREA
 Q-DATA-TYPES
 QC-FILE
 QC-FILE-LOAD
 QREPLY
 QSEND
 QSENDS-OFF
 QSENDS-ON
 QUERY-IO
 QUOTE                                          ;common
 QUOTIENT
 RANDOM-STATE-P                                 ;common
 RANDOM-STATE                                   ;common
 RANDOM                                         ;common
 RASS
 RASSOC-IF-NOT                                  ;common
 RASSOC-IF                                      ;common
 RASSOC                                 ;common -- incompatible
 RASSQ
 RATIONALIZE                                    ;common
 RATIONALP                                      ;common
 RATIONAL                                       ;common
 RATIO                                          ;common
 READ-BYTE                                      ;common
 READ-CHAR-NO-HANG                              ;common
 READ-CHAR                                      ;common
 READ-CHECK-INDENTATION
 READ-DELIMITED-LIST                            ;common
 READ-DELIMITED-STRING
 READ-FOR-TOP-LEVEL
 READ-FROM-STRING                               ;common -- incompatible
 READ-LINE                                      ;common
 READ-METER
 READ-OR-END
 READ-PRESERVE-DELIMITERS
 READ-PRESERVING-WHITESPACE                     ;common
 READCH                                         ;maclisp
 READFILE
 READLINE
 READLINE-OR-NIL
 READLINE-TRIM
 READLIST
 READTABLEP                                     ;common
 READTABLE                                      ;common
 READ                                   ;common -- incompatible
 REAL
 REALP
 REALPART                                       ;common
 RECOMPILE-FLAVOR
 RECORD-SOURCE-FILE-NAME
 REDUCE                                         ;common
 REM-IF-NOT                                     ;common
 REM-IF                                         ;common
 REMAINDER
 REMF                                           ;common
 REMHASH-EQUAL
 REMHASH                                        ;common
 REMOB
 REMOVE-DUPLICATES                              ;common
 REMOVE-IF-NOT                                  ;common
 REMOVE-IF                                      ;common
 REMOVE                                 ;common -- incompatible
 REMPROP                                        ;common
 REMQ
 REM                                            ;common -- incompatible
 RENAME-FILE                                    ;common
 RENAME-PACKAGE                                 ;common
 RENAMEF
 REPLACE                                        ;common
 REPLY
 REQUIRE                                        ;common
 RESET-FILL-POINTER
 RESET-INITIALIZATIONS
 RESET-USER-OPTIONS
 REST1
 REST2
 REST3
 REST4
 REST                                           ;common
 RETURN-ARRAY
 RETURN-FROM                                    ;common
 RETURN-LIST
 RETURN-NEXT-VALUE
 RETURN-STORAGE
 RETURN                                         ;common
 REVAPPEND                                      ;common
 REVERSE                                        ;common
 ROOM
 ROT
 ROTATEF                                        ;common
 ROUND                                          ;common
 RPLACA                                         ;common
 RPLACD
 RUBOUT-HANDLER
 RUBOUT-HANDLER-OPTIONS
 SAFETY                                         ;common
 SAMEPNAMEP
 SASSOC
 SASSQ
 SATISFIES                                      ;common
 SBIT                                           ;common
 SCALE-FLOAT                                    ;common
 SCHAR                                          ;common
 SEARCH                                         ;common
 SECOND                                         ;common
 SELECT
 SELECT-MATCH
 SELECT-PROCESSOR
 SELECTOR
 SELECTQ
 SELECTQ-EVERY
 SELF
 SELF-EVALUATING-P
 SEND
 SEND-IF-HANDLES
 SEQUENCE                                       ;common
 SET-CHAR-BIT                                   ;common
 SET-CHARACTER-TRANSLATION
 SET-CURRENT-BAND
 SET-CURRENT-MICROLOAD
 SET-DIFFERENCE                                 ;common
 SET-DISPATCH-MACRO-CHARACTER                   ;common
 SET-EXCLUSIVE-OR                               ;common
 SET-GLOBALLY
 SET-IN-CLOSURE
 SET-IN-INSTANCE
 SET-MACRO-CHARACTER                            ;common
 SET-MAR
 SET-MEMORY-SIZE
 SET-PRINTER-DEFAULT-OPTION
 SET-SYNTAX-\#-MACRO-CHAR
 SET-SYNTAX-FROM-CHAR                           ;common
 SET-SYNTAX-FROM-DESCRIPTION
 SET-SYNTAX-MACRO-CHAR
 SETARG
 SETF                                           ;common
 SETPLIST
 SETQ                                           ;common
 SETQ-GLOBALLY
 setq-standard-value
 SETSYNTAX
 SETSYNTAX-SHARP-MACRO
 SET                                            ;common
 SEVENTH                                        ;common
 SG-AREA
 SG-RETURN-UNSAFE
 SGVREF
 SHADOWING-IMPORT                               ;common
 SHADOW                                         ;common
 SHIFTF                                         ;common
 SHORT-FLOAT-EPSILON                            ;common
 SHORT-FLOAT-NEGATIVE-EPSILON                   ;common
 SHORT-FLOAT                                    ;common
 SHORT-SITE-NAME                                ;common
 SIGNAL
 SIGNAL-CONDITION
 SIGNAL-PROCEED-CASE
 SIGNED-BYTE
 SIGNP
 SIGNUM                                         ;common
 SIMPLE-ARRAY                                   ;common
 SIMPLE-BIT-VECTOR-P                            ;common
 SIMPLE-BIT-VECTOR                              ;common
 SIMPLE-STRING-P                                ;common
 SIMPLE-STRING                                  ;common
 SIMPLE-VECTOR-P                                ;common
 SIMPLE-VECTOR                                  ;common
 SIND
 SINGLE-FLOAT-EPSILON
 SINGLE-FLOAT-NEGATIVE-EPSILON
 SINGLE-FLOAT                                   ;common
 SINH                                           ;common
 SIN                                            ;common
 SITE-NAME
 SIXTH                                          ;common
 SLEEP                                          ;common
 SMALL-FLOAT
 SMALL-FLOATP
 SMALL-FLONUM
 SOFTWARE-TYPE                                  ;common
 SOFTWARE-VERSION                               ;common
 SOME                                   ;common -- incompatible
 SORT-GROUPED-ARRAY
 SORT-GROUPED-ARRAY-GROUP-KEY
 SORTCAR
 SORT                                           ;common
 SPACE                                          ;common
 SPECIAL-FORM-P                                 ;common
 SPECIAL                                        ;common -- needs hacking
 SPEED                                          ;common
 SQRT                                           ;common
 SSTATUS
 STABLE-SORTCAR
 STABLE-SORT                                    ;common
 STACK-GROUP
 STACK-GROUP-PRESET
 STACK-GROUP-RESUME
 STACK-GROUP-RETURN
 STANDARD-CHAR-P                                ;common
 STANDARD-CHAR                                  ;common
 STANDARD-INPUT
 STANDARD-OUTPUT
 standard-value-let
 standard-value-let*
 standard-value-progv
 STATUS
 STEP-FORM
 STEP-VALUE
 STEP-VALUES
 STEP                                           ;common
 STORE
 STORE-ARRAY-LEADER
 STORE-CONDITIONAL
 STREAM-COPY-UNTIL-EOF
 STREAM-DEFAULT-HANDLER
 STREAM-ELEMENT-TYPE                            ;common
 STREAMP                                        ;common
 STREAM                                         ;common
 STRING
 STRING
 STRING
 STRING-APPEND
 STRING-APPEND-A-OR-AN
 STRING-CAPITALIZE-WORDS
 STRING-CAPITALIZE                              ;common
 STRING-CHAR-P                                  ;common
 STRING-CHAR                                    ;common
 STRING-COMPARE
 STRING-DOWNCASE                                ;common
 STRING-EQUAL                                   ;common
 STRING-GREATERP                                ;common
 STRING-LEFT-TRIM                               ;common
 STRING-LENGTH
 STRING-LESSP                                   ;common
 STRING-NCONC
 string-nconc-portion
 STRING-NOT-EQUAL                               ;common
 STRING-NOT-GREATERP                            ;common
 STRING-NOT-LESSP                               ;common
 STRING-NREVERSE
 STRING-PLURALIZE
 STRING-REMOVE-FONTS
 STRING-REMOVE-FONTS
 STRING-REVERSE
 STRING-REVERSE-SEARCH
 STRING-REVERSE-SEARCH-CHAR
 STRING-REVERSE-SEARCH-NOT-CHAR
 STRING-REVERSE-SEARCH-NOT-SET
 STRING-REVERSE-SEARCH-SET
 STRING-RIGHT-TRIM                              ;common
 STRING-SEARCH
 STRING-SEARCH-CHAR
 STRING-SEARCH-NOT-CHAR
 STRING-SEARCH-NOT-SET
 STRING-SEARCH-SET
 STRING-SELECT-A-OR-AN
 STRING-SUBST-CHAR
 STRING-TRIM                                    ;common
 STRING-UPCASE                                  ;common
 STRING/=
 STRING<=                                       ;common
 STRING<                                        ;common
 STRING=
 STRING=                                        ;common
 STRING>=                                       ;common
 STRING>                                        ;common
 STRINGP                                        ;common
 STRING                                         ;common
 STRUCTURE
 STRUCTURE-FORWARD
 SUB-APROPOS
 SUB1
 SUBLIS                                         ;common
 SUBRP
 SUBSEQ                                         ;common
 SUBSET
 SUBSET-NOT
 SUBSETP                                        ;common
 SUBST-IF-NOT                                   ;common
 SUBST-IF                                       ;common
 SUBSTITUTE-IF-NOT                              ;common
 SUBSTITUTE-IF                                  ;common
 SUBSTITUTE                                     ;common
 SUBSTRING
 SUBSTRING-AFTER-CHAR
 SUBST                                  ;common -- incompatible
 SUBTYPEP                                       ;common
 SUPDUP
 SVREF                                          ;common
 SWAP-SV-OF-SG-THAT-CALLS-ME
 SWAP-SV-ON-CALL-OUT
 SWAPF
 SWAPHASH
 SWAPHASH-EQUAL
 SXHASH                                         ;common
 SYMBOL-FUNCTION                                ;common
 SYMBOL-NAME                                    ;common
 SYMBOL-PACKAGE                                 ;common
 SYMBOL-PLIST                                   ;common
 SYMBOL-VALUE                                   ;common
 SYMBOLP                                        ;common
 SYMBOL                                         ;common
 SYMEVAL
 SYMEVAL-GLOBALLY
 SYMEVAL-IN-CLOSURE
 SYMEVAL-IN-INSTANCE
 SYMEVAL-IN-STACK-GROUP
 TAGBODY                                        ;common
 TAIL-RECURSION-FLAG
 TAILP                                          ;common
 TAND
 TANH                                           ;common
 TAN                                            ;common
 TELNET
 TENTH                                          ;common
 TERMINAL-IO
 TERPRI
 THE                                            ;common
 THIRD                                          ;common
 THROW                                          ;common
 TIME-DIFFERENCE
 TIME-INCREMENT
 TIME-LESSP
 TIMES
 TIME                                   ;common
 TRACE-COMPILE-FLAG
 TRACE-OUTPUT
 TRACE                                          ;common
 TRANSLATED-PATHNAME
 TREE-EQUAL                                     ;common
 TRUE
 TRUENAME                                       ;common
 TRUNCATE                                       ;common
 TYI
 TYIPEEK
 TYO
 TYPE
 TYPE-OF                                        ;common
 TYPECASE                                       ;common
 TYPEP                                          ;common
 T                                              ;common
 UNADVISE
 UNADVISE-WITHIN
 UNBIND
 UNBOUND-FUNCTION                               ;for WHO-CALLS
 UNBREAKON
 UNCOMPILE
 UNDEFFLAVOR
 UNDEFMETHOD
 UNDEFUN
 UNDELETE-FILE
 UNDELETEF
 UNEXPORT                                       ;common
 UNINTERN                                       ;common
 UNION                                  ;common
 UNLESS                                         ;common
;UNMONITOR-VARIABLE
 UNREAD-CHAR                                    ;common
 UNSIGNED-BYTE                                  ;common
 UNSPECIAL                                      ;common -- needs hacking
 UNTRACE                                        ;common
 UNUSE-PACKAGE                                  ;common
 UNWIND-ALL
 UNWIND-PROTECT                                 ;common
 UNWIND-PROTECT-CASE
 UPDATE-SITE-CONFIGURATION-INFO
 UPPER-CASE-P                                   ;common
 USE-PACKAGE                                    ;common
 USER-HOMEDIR-PATHNAME                          ;common
 USER-ID
 USING-RESOURCE
 VALUE-CELL-LOCATION
 VALUES-LIST
 VALUES                                         ;common
 VARIABLE-BOUNDP
 VARIABLE-LOCATION
 VARIABLE-MAKUNBOUND
 VARIABLE                                       ;common
 VECTOR-POP                                     ;common
 VECTOR-PUSH-EXTEND                             ;common
 VECTOR-PUSH                                    ;common
 VECTORP                                        ;common
 VECTOR                                         ;common
 VIEWF
 WARN                                           ;common
 WHAT-FILES-CALL
 WHEN                                           ;common
 WHERE-IS
 WHO-CALLS
;WHO-USES                                       ;obsolete: use who-calls
 WHOIS
 WITH-INPUT-EDITING
 WITH-INPUT-FROM-STRING                         ;common
 WITH-LIST
 WITH-LIST*
 WITH-LOCK
 WITH-OPEN-FILE-CASE
 WITH-OPEN-FILE-RETRY
 WITH-OPEN-FILE-SEARCH
 WITH-OPEN-FILE                                 ;common
 WITH-OPEN-STREAM
 WITH-OPEN-STREAM-CASE
 WITH-OPEN-STRING
 WITH-OUTPUT-TO-STRING                          ;common
 WITH-SELF-VARIABLES-BOUND                      ;bletcherous: use anything else.
 WITH-STACK-LIST
 WITH-STACK-LIST*
 WITH-TIMEOUT
 WITHOUT-FLOATING-UNDERFLOW-TRAPS
 WITHOUT-INTERRUPTS
 WORKING-STORAGE-AREA
 WRITE-BYTE                                     ;common
 WRITE-CHAR                                     ;common
 WRITE-LINE                                     ;common
 WRITE-METER
 WRITE-STRING                                   ;common
 WRITE-TO-STRING                                ;common
 WRITE-USER-OPTIONS
 WRITE                                          ;common
 XCONS
 XCONS-IN-AREA
 XSTORE
 Y-OR-N-P                                       ;common
 YES-OR-NO-P                                    ;common
 ZEROP                                          ;common
 ZMAIL
 ZUNDERFLOW

 ) 'user:k-global) ;;eof
