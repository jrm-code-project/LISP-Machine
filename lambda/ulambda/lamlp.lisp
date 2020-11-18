;; -*- Mode:LISP; Package:LAMBDA; Base:8; Readtable:ZL -*-
;       ** (c) Copyright 1984, 1985 Lisp Machine Inc **
;;;     Micro Assembler for Lambda or Explorer

(DEFVAR FOR-LAMBDA NIL)         ;t during actual lambda u-assembly
(DEFVAR *TARGET-PROCESSOR-TYPE* ':LAMBDA) ;or :EXPLORER.  Some old CADR stuff may say
                                          ; CADR but that doesnt really claim to work.

(DEFVAR *SYMBOL-PROPERTY-FLAGS*
        '(LAM-LAP-SYM LAM-LAP-USER-SYMBOL LAM-LAP-ADDITIVE-CONSTANT))  ;Or corresponding
                        ;for EXPLORER (IN SAME ORDER!)

(DEFVAR *COMMENT-DEPTH* 0)      ;non-zero if in word-wise comment section.
  ;incremented by (begin-comment) decremented by (end-comment)

(DEFVAR *PAGABLE-UCODE-MODE* NIL)  ;T MEANS AVOID NEXT UINST DEPENDANCIES ACROSS MICRO PAGE
                                   ; BOUNDARIES.  Can be set only on LAMBDA.
(DEFVAR *MACRO-IR-DISPATCH-ALIST* NIL)  ;(<i-mem-loc> . <macro-ir-pattern>)
(DEFVAR *MACRO-IR-DISPATCH-SPEC* NIL)   ;used to interpret macro-ir-patterns. see below.
(DEFVAR *MACRO-IR-MISC-DISPATCH-ALIST* NIL)  ;(<i-mem-loc> . <macro-ir-misc-decode arg>)

(defvar *bypass-errors* nil)    ;if T, continue assembly after (error)

(defvar *georges-quick-file-read-is-loaded* nil) ;if nil, load in all sources fast.

(DECLARE        ;in DEBUG-UINST def file for LAM-EXECUTE
  (SPECIAL COM-IR-M-SRC COM-IR-A-SRC COM-IR-RPN COM-IR-JUMP-ADDR COM-DISP-RPN-BITS
           COM-IR-M-MEM-DEST COM-IR-A-MEM-DEST COM-IR-FUNC-DEST COM-IR-A-MEM-DEST-FLAG
           COM-IR-OP COM-IR-BYTE-SR COM-IR-BYTE-FUNC COM-IR-ILONG ))

;TO COMPILE OR RUN ON THE LISP MACHINE, USE THE PACKAGE DEFINITION IN LCADR;UA PKG

;(IF-FOR-LISPM          ;These not used here anymore, but needed to read in QCOM.
; (DEFMACRO LOGLDB (PTR VAL) `(LDB ,PTR ,VAL)))
;(IF-FOR-LISPM
; (DEFMACRO LOGDPB (NEWVAL PTR VAL) `(DPB ,NEWVAL ,PTR ,VAL)))

;SYMBOLS IN LAM-LAP:
; A SYMBOL IN LAM-LAP HAS AS ITS VALUE A PROGRAM!
;  THE PROGRAM IS EVALUATED BY RECURSIVE CALLS TO LAM-LAP-EVAL.
;  IF THE ARGUMENT TO LAM-LAP-EVAL IS NUMERIC, IT IS RETURNED AS THE VALUE.
;  IF NIL, THIS SPECIFIES THE NULL VALUE.
;  IF A SYMBOL, ITS VALUE IS RUN AS A PROGRAM AND RETURNED.
;  IF A LIST, CAR OF THE LISP IS THE FUNCTION AND THE REST OF THE LIST
;   ARGUMENTS, LISP STYLE.  UNLESS OTHERWISE NOTED BELOW, ALL FUNCTIONS
;   EVALUATE THEIR ARGS (LISP STYLE) AND ACTUALLY DO SOMETHING ONLY
;   AFTER THE EVALUATION OF THEIR ARGUMENTS HAS FINISHED.

;AVAILABLE FUNCTIONS:
; FUNCTIONS OF ONE ARGUMENT
;  SPECIFERS OF LOCALITY: A-MEM, M-MEM, I-MEM, D-MEM.
;       RETURN VALUE INDICATING THAT THEIR ARGUMENT CORRESPONDS TO AN
;       ADDRESS IN THE SPECIFIED MEMORY.
;  CONDITIONALS:  DESTINATION-P, SOURCE-P, DISPATCH-INSTRUCTION-P, JUMP-INSTRUCTION-P
;       ALU-INSTRUCTION-P, BYTE-INSTRUCTION-P. EVALUATE AND RETURN ARGUMENT
;       ONLY IF SPECIFIED CONDITION TRUE (NAMELY: ASSEMBLING A DESTINATION FIELD,
;       A SOURCE FIELD, OR THE TYPE OF INSTRUCTION INDICATED). RETURN NIL
;       IF CONDITION FALSE.
;  NEGATION: NOT. MUST BE NESTED WITH ONE OF THE CONDITIONALS ABOVE AS IS
;       (NOT (DESTINATION (...))).
;  OR. RETURNS FIRST NON-NIL VALUE LIKE LISP OR.
;  PLUS. COMBINES THE VALUES / PROPERTIES REPRESENTED BY ALL ITS ARGUMENTS.
;       USED TO BE TWO ARGS ONLY, NOW TAKES ANY NUMBER OF ARGS.
;  DIFFERENCE.  LIKEWISE.
;  INSTRUCTION-TYPE FORCE: FORCE-DISPATCH, FORCE-JUMP, FORCE-ALU, FORCE-BYTE.
;       FORCE-DISPATCH-OR-BYTE, FORCE-ALU-OR-BYTE.
;  DEFAULT-CONDITION.  DEFAULT-BTYE. IF DISPATCH IS FORCED, RETURN NIL.
;       OTHERWISE FORCE BYTE.
;  BYTE-FIELD <BITS BITS-OVER>. DEFAULTS BYTE-INSTRUCTION.  ERROR IF OTHER THAN
;       BYTE INSTRUCTION OR DISPATCH INSTRUCTION (OR IF A ONE BIT FIELD,
;       JUMP INSTRUCTION).  ASSEMBLES THE RIGHT THING
;       TO REFERENCE BYTE, AS PER WHAT INSTRUCTION TYPE IS.
;  LISP-BYTE <%% FORM BYTE SPECIFIER>.  SIMILIAR TO BYTE-FIELD, BUT BYTE DESCRIPTION IS
;       OBTAINED BY EVAL ING ARGUMENT AND INTERPRETING IT AS A BYTE SPECIFIER.
;       I.E. PPSS WHERE PP GIVES POSITION AND SS GIVES SIZE A LA PDP-10
;       BYTE INSTRUCTION.
;  ALL-BUT-LISP-BYTE <%% FORM BYTE SPECIFIER>.  SIMILAR, BUT ADDRESSES BITS NOT IN
;       <BYTE>.  <BYTE> MUST BE EITHER LEFT OR RIGHT ADJUSTED IN 32. BITS.
;  BYTE-MASK <SYMBOLIC BYTE SPECIFIER>.  ARG CAN BE SYMBOL OR COMPOSITION OF
;       OPS AND SYMBOLS SPECIFYING A BYTE (IE CONTAINING SOMEWHERE IN THERE
;       A BYTE-FIELD OR LISP-BYTE OPERATION).  THIS IS DUG OUT BY BYTE-MASK
;       AND IS RETURNS THE VALUE OF ALL 1'S IN THE SPECIFIED BYTE.
;  BYTE-VALUE <SYMBOLIC BYTE SPECIFIER> <VALUE TO STORE IN BYTE>
;       RETURNS A VALUE OF THE SPECIFIED NUMBER IN THE SPECIFIED BYTE.
;       FOR CONVENIENCE, THE VALUE MAY BE EITHER A LAM-LAP SYMBOL OR A LISP SYMBOL.
;  FIELDS: (FIELD <FIELD NAME> <VALUE>).  NOTATION IS MADE THAT <FIELD NAME>
;       HAS BEEN SPECIFIED.  THE VALUE IS OBTAINED AS FOLLOWS:  THE PROGRAM
;       ASSOCIATED WITH <FIELD NAME> AS A SYMBOL IS RUN AND ITS VALUE MULTIPLIED
;       BY <VALUE> (THIS IS DONE RATHER THAN SHIFTING SO BIGNUMS WORK CONVIENTLY).
;       ADDITIONALLY, IF A LAM-LAP-ADDITIVE-CONSTANT
;       PROPERTY IS PRESENT ON <FIELD NAME> IT WILL BE ADDED IN AFTER MULTIPLING.
;       ANY PROPERTIES SPECIFIED IN THE RUNNING OF <FIELD NAME> STICK.
;  I-ARG.  ASSEMBLES ITS ARGUMENT INTO THE IMMEDIATE ARGUMENT FIELD OF A DISPATCH
;       INSTRUCTION.
;  ((ARG-CALL ADR) .. ) OR ((ARG-JUMP ADR) .. ).  ASSEMBLES A DISPATCH INSTRUCTION
;       WHICH DISPATCHES ON ZERO BITS TO A D-MEM ENTRY WHICH DOES A CALL (OR JUMP)
;       TO ADR.  USE IF IT IS DESIRED TO SUPPLY AN I-ARG ON AN UNCONDITIONAL
;       CALL (OR JUMP).  ((ARG-CALL-XCT-NEXT ADR) .. ) AND ((ARG-JUMP-XCT-NEXT ADR) ..)
;       ARE ALSO AVAILABLE.
;  EVAL <ARG>.  CALLS LISP EVAL ON ARG AND RETURNS (NUMERIC HOPEFULLY) VALUE.
;  LOC <ARG> SETS LOCATION COUNTER TO <ARG>.
;  MODULO <ARG> SETS LOCATION COUNTER TO BE ON A MOD <ARG> BOUNDARY.
; The following group provide communication between an assembly and microcompiled
;    code or other assemblies which may be added to it.
;  MC-LINKAGE <list of symbols>.  The values of these symbols are made available
;       to the micro-compiled-code loader and to the incremental mode of the assembler.
;       A and M memory symbols with values less than 40 (100 on ) are automatically
;       MC-LINKAGEifyed.
;  MC-LINKAGE sym.  Useful primarily in incrmental assemblies.  Expands to value
;       given sym in either current or previous
;      assembly.  Includes appropriate memory.
;  MC-ENTRY-ADR <microcoded-function>  allowable only in incremental assembly.
;       evaluates to I-MEM address of entry to <function> in JUMP-ADDRESS field.
;  MISC-ENTRY-ADR <misc-instruction>   allowable only in incremental assembly.
;       evaluates to I-MEM address of entry to <misc-instruction> in JUMP-ADDRESS field.
;  MC-LINKAGE-VALUE <memory> <symbol>  useful primarily in incremental assemblies.
;       <memory> must be one of NUMBER, I-MEM, D-MEM, A-MEM, M-MEM.  <symbol> must
;       have been assigned a value with the MC-LINKAGE operation (either in the
;       current assembly, or a previous one to which this assembly is being added).
;       Evaluates to the value in the appropriate memory.

;  INSTRUCTIONS FOR ASSEMBLING VALUES FOR USE WITH OA REGISTER.  (RECALL? THAT
;       THE OA "REGISTER" IS THE HACK WHEREBY THE NEXT MICRO-INSTRUCTION GETS
;       IOR-ED WITH DATA PRODUCED BY THIS ONE).
;    OA-LOW-CONTEXT OA-HIGH-CONTEXT <I-MEM STORAGE-WORD>.  ASSEMBLES <I-MEM STORAGE
;       WORD> AND RETURN EITHER HI OR LOW PART AS NUMBER FOR USE WITH DESTINATIONS
;       OA-REG-HI OR OA-REG-LOW.
; SYMBOLS MAY BE EITHER ON THE SYMTAB OR ON THE PROPERTY LIST UNDER THE INDICATOR
;  LAM-LAP-SYM (OR EXP-LAP-SYM).

;THE TYPE OF INSTRUCTION THAT GETS ASSEMBLED IN A GIVEN STORAGE WORD IS DETERMINED
;AS FOLLOWS:
;  FIRST THERE IS A DEFAULT, ALU-INSTRUCTION.  IT IS OVERRIDDEN BY ANY OTHER SPECIFIER.
;       THIS IS THE ONLY SPECIFIER THAT
;       CAN BE "OUT-OF-HARMONY" WITH ANY OTHER PRESENT SPECIFIER WITHOUT CAUSING AN
;       ERROR.
;  IF A DESTINATION IS PRESENT, INSTRUCTION MUST BE ALU-INSTRUCTION OR BYTE-INSTRUCTION.
;  IF AN I-MEM CONTEXT SYMBOL IS PRESENT, INSTRUCTION MUST BE JUMP-INSTRUCTION.
;  IF A D-MEM CONTEXT SYMBOL IS PRESENT, INSTRUCTION MUST BE DISPATCH-INSTRUCTION.
;  IF BOTH A M-MEM AND A A-MEM SYMBOL ARE PRESENT, INSTRUCTION MUST BE ALU-INSTRUCTION
;       OR BYTE-INSTRUCTION.
;  INSTRUCTION CAN BE FORCED BY A FORCE-INSTRUCTION PROPERTY ON ANY SYMBOL IN THE
;       WORD.
;  TWO A-MEM OR TWO M-MEM SYMBOLS IN ONE INSTRUCTION IS AN ERROR.

;ONCE INSTRUCTION TYPE IS DETERMINED, A CHECK IS MADE TO SEE THAT ALL NECESSARY
; FIELDS IN IT HAVE BEEN SPECIFIED, AND DEFAULTS SUPPLIED FOR VARIOUS OPTIONAL
; FIELDS AND MODES IF THEY WERE NOT SPECIFIED.

;RANDOM CONVENTIONS --
; LOCATION TAGS ARE DEFINED AS FIELDS. IE (FIELD JUMP-ADDRESS-MULTIPLIER NNN)
; FOR SYMBOLS IN I-MEM. (A-SOURCE-MULTIPLIER, M-SOURCE-MULTIPLIER, AND
; DISPATCH-ADDRESS-MULTIPLIER ARE THE CORRESPONDING FIELDS FOR A-MEM, M-MEM,
; AND D-MEM RESPECTIVELY).  THUS, WHEN NORMALLY EVALUATED, THEY HAVE
; THEIR VALUES IN THESE "PLACES".  THIS IS THE RIGHT THING EXCEPT FOR THESE
; CASES: 1)  DESTINATIONS.  CONVERT-VALUE-TO-DESTINATION COMPUTES AN APPROPRIATE
;               "SHIFT"
;        2)  LOCALITY D-MEM.  LAM-LAP-PASS2 DOES THE RIGHT THING.  THIS INVOLVES
;               SHIFTING THE I-MEM ADR BACK TO THE LOW PART AND MOVING THE RPN
;               BITS UP (FROM THEIR NORMAL POSITION IN A JUMP INSTRUCTION).
; OTHER FEATURES/CROCKS
;   WHEN A BYTE-FIELD OPERATION IS ENCOUNTERED BY LAM-LAP-EVAL,
;       THE INSTRUCTION CONTEXT IS FORCED TO BYTE IF IT HAS NOT ALREADY
;       BEEN COMPLETELY SPECIFIED.  THEN THE BYTE REFERENCE IS ASSEMBLED
;       IN THE MANNER APPROPRIATE TO THE INSTRUCTION CONTEXT.
;   THE SR-BIT IS STORED INVERTED (SO THAT IT WILL OFF FOR NORMAL LDB).
;       LAM-LAP-DEFAULT-AND-BUGGER REVERSES SR-BIT IF IT'S A BYTE INSTRUCTION
;   THE HARDWARE IMPLEMENTS A LEFT ROTATE FOR THE M-ROTATE FIELD.  The is the
;       "right thing" for DPB and SELECTIVE-DEPOSIT, but LDB, DISPATCH, and
;       JUMP-IF-BIT-SET need to be 32-reflected (IE ( 32. - M-ROTATE) MOD 32.)
;       This is done by LAM-LAP-DEFAULT-AND-BUGGER.
;       CODE USING THE OA-REGISTER FEATURE TO MODIFY BYTE TYPE INSTRUCTIONS
;       MUST BE AWARE OF THIS.
;  TO PUT THE ADDRESS OF A MICRO CODE LOCATION INTO A CONSTANT IN A OR M
;       MEMORY, USE THE KLUDGEY CONSTRUCTION (I-MEM-LOC <TAG>).
;  SIMILARLY, A-MEM-LOC, M-MEM-LOC, D-MEM-LOC PSEUDO-OPS EXIST.

;   OPERATION OF THE ARG-CALL, ETC, FEATURE IN DISPATCH INSTRUCTIONS.
;       SOMETIMES IT IS DESIRABLE TO USE A DISPATCH INSTRUCTION WHEN
;       REALLY ONLY AN UNCONDITIONAL TRANSFER (CALL, ETC) IS DESIRED
;       IN ORDER TO BE ABLE TO LOAD THE DISPATCH-CONSTANT REGISTER IN THE
;       SAME INSTRUCTION.  IT WOULD BE A PAIN TO HAVE TO DEFINE A ONE REGISTER
;       DISPATCH TABLE, ETC IN THIS CASE.  SO THE ASSEMBLER PROVIDES A FEATURE
;       WHEREBY ARG-CALL, ARG-JUMP, ARG-CALL-XCT-NEXT, AND ARG-JUMP-XCT-NEXT
;       ARE SPECIALLY RECOGNIZED.  USING THESE PSEUDO-OPS, THE INSTRUCTION
;       MAY BE WRITTEN AS "NORMAL" AND THE ASSEMBLER WILL TAKE CARE OF
;       ALLOCATING A D-MEM LOCATION AND MOVING THE RPN BITS AND I-MEM JUMP ADDRESS
;       BITS THERE.  THIS D-MEM LOCATION IS AUTOMATICALLY PLUGGED INTO THE
;       DISPATCH OFFSET.
;   ON A NORMAL PDP-10 STYLE LOAD BYTE, THE A-MEM ADDRESS MUST CONTAIN 0
;       FOR CORRECT OPERATION.  A-MEM
;       LOCATION 2 IS CHOSEN TO CONTAIN ZERO, AND LOCATION 3 TO CONTAIN -1,
;       MAKING A CONVENIENT PAIR FOR DOING SIGN-EXTENSION.  THE A-MEM ADDRESS
;       OF A LOAD-BYTE INSTRUCTION WILL BE DEFAULTED TO 2 IF NOT SPECIFIED.

;ENTRY POINTS INTO MICRO-CODE FROM MACRO-CODE, ETC:
;   THE MICRO-CODE-SYMBOL AREA CONTAINS ALL (INITIAL) ENTRY POINTS INTO
;  MICRO-CODE.  THE FIRST 600 Q'S OF MICRO-CODE-SYMBOL AREA GIVE THE CONTROL-MEMORY
;  TRANSFER ADDRESSES FOR MACRO-CODE MISC-INSTRUCTIONS 200-777.  FOLLOWING THAT
;  ARE OTHER ENTRY POINTS, MOSTLY FOR MICRO-COMPILED RUNTIME ROUTINES, ETC.
;  THESE LAST ARE NOT REFERENCED DYNAMICALLY, BUT JUST BY LOADERS, ETC.
;   THE MICRO-CODE-SYMBOL AREA IS COMPLETELY DETERMINED BY CONSLP UNDER CONTROL
;  OF THE (MISC-INST-ENTRY <NAME>) PSEUDO-OPERATION.
;     (MISC-INST-ENTRY <NAME>) DECLARES THAT THE CURRENT LOCATION IS THE ENTRY POINT
;       WHEN <NAME> IS EXECUTED AS A MACRO-INSTRUCTION. CONSLP LOOKS ON THE PROPERTY
;       LIST OF <NAME> TO FIND THE QLVAL PROPERTY (WHICH HAD BETTER BE THERE OR ERROR).
;       THESE QLVAL COME FROM LISPM;DEFMIC. CONSLP THEN ARRANGES FOR . TO APPEAR
;       IN THE APPROPRIATE LOCATION OF MICRO-CODE-SYMBOL AREA.
; IN ADDITION, (MICRO-CODE-ILLEGAL-ENTRY-HERE), ENCOUNTERED AT ANY TIME, FILLS
;       ALL UNUSED ENTRIES OF MICRO-CODE-SYMBOL AREA WITH THE CURRENT LOCATION.
;       (IT IS OK IF SOME OF THEM LATER GET STORED OVER WITH OTHER STUFF...)
;THE MC-LINKAGE PSEUDO-OP IS THE OTHER MECHANISM (BESIDE MISC-INST-ENTRY)
;  BY WHICH LINKAGE INFO CAN BE "COUPLED OUT" AND USED BY MICROCOMPILED ROUTINES.
;  USAGE IS (MC-LINKAGE <SYM> ..)  THE LOCATION WITHIN MEMORY OF SYM IS ADDED TO
;  MC-LINKAGE-ALIST, AND THAT IS WRITTEN AS PART OF THE ASSEMBLER STATE.  IF
;  SYM IS A LIST, CAR IS THE MICROCOMPILED NAME, CADR THE CONSLP NAME.

;THE ERROR TABLE:
; THE PSEUDO-OP (ERROR-TABLE FOO BAR BAZ...)
; WILL ADD THE LINE (LOC FOO BAR BAZ...) TO THE ERROR TABLE, WHERE LOC IS
; THE ADDRESS OF THE PRECEEDING I-MEM INSTRUCTION.  THE ERROR TABLE IS
; AN OUTPUT FILE, UCONS TABLE, WHICH CAN BE READ IN TO LISP.  IT CONTAINS
; A SETQ OF MICROCODE-ERROR-TABLE TO A LIST OF ERROR TABLE ENTRIES,
; AND A SETQ OF MICROCODE-ERROR-TABLE-VERSION TO THE SOURCE FILE VERSION
; NUMBER, WHICH CAN BE COMPARED AGAINST %MICROCODE-VERSION-NUMBER.

;THE DECLARATION TABLE:
; THE PSEUDO-OP (DECLARE FOO BAR BAZ ..)
; WILL ADD THE LINE (OC FOO BAR BAZ .. ) TO THE DECLARATION TABLE, SIMILAR TO
; ERROR TABLE ABOVE.  THIS FILE CONTAINS INFORMATION FOR THE MICRO-TRACE FLOW
; TRACER.  SEE THE FLOW TRACER FOR FURTHER INFO.

(DECLARE (SPECIAL DESTINATION-CONTEXT LOCALITY
           I-MEM-LOC PAGABLE-I-MEM-LOC D-MEM-LOC
           A-MEM-CREVICE-LIST A-CONSTANT-LOC M-CONSTANT-LOC
           *m-constant-limit* *a-constant-limit*
           CONSLP-INPUT CONSLP-OUTPUT CONSLP-OUTPUT-PATHNAME
           VERSION-NUMBER      ;Numeric value of FN2 for this file
           BASE-VERSION-NUMBER ;NIL or, if incremental assembly, version this to augment.
           A-MEM-LOC M-MEM-LOC D-MEM-FREE-BLOCKS FIELD-INDICATORS COMBINED-VALUE
           COMBINED-INDICATORS INSTRUCTION-CONTEXT IN-DISPATCH-BLOCK
           DISPATCH-BLOCK-LIMIT DISPATCH-ARM DISPATCH-CONSTANT M-CONSTANT-LIST
           A-CONSTANT-LIST A-CONSTANT-BASE M-CONSTANT-BASE LAM-LAP-LAST-SYM
           A-MEMORY-RANGE-LIST M-MEMORY-RANGE-LIST
           I-MEMORY-RANGE-LIST D-MEMORY-RANGE-LIST
           LAM-LAP-WDS-SINCE-LAST-SYM LAM-LAP-SAVED-SYMTAB SR-BIT

           *DIRECT-BITS*

           *NOOPS-INSERTED-DUE-TO-PAGING* *previous-uinst-linked-to-next*
           ARG-CALL-LIST CURRENT-WORD
           MC-LINKAGE-ALIST
           COLD-LOAD-AREA-SIZES PAGE-SIZE LAM-LAP-PASS2 MICRO-CODE-SYMBOL-TABLE-FILL-VALUE
           LAM-LAP-INIT-STATE   ;If this non-null, current assembly is incremental
                                 ; from this saved state.
           CURRENT-ASSEMBLY-MICRO-ENTRIES   ;List, ea element, (<type> <name> <adr>),
                                            ; in incremental assembly
           CURRENT-ASSEMBLY-TABLE           ;Error table
           CURRENT-ASSEMBLY-DECLARATIONS    ;Info for flow tracer.
           CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY
           CURRENT-ASSEMBLY-DEFMICS
))

(DEFVAR FILE-TRUENAMES-LISTIFIED NIL)

;THE ARG CALL LIST IS AN ASSOCIATION LIST WHERE THE KEY IS THE I-MEM LOCATION
;AT WHICH AN ((ARG-CALL) ..) TYPE INSTRUCTION HAS APPEARED, AND THE VALUE
;IS THE D-MEM LOCATION THAT HAS BEEN ALLOCATED TO IT.


;ARRAYS WHICH RECEIVE THE OUTPUT OF THE ASSEMBLY
(DEFVAR I-MEM)
(DEFVAR A-MEM)
(DEFVAR D-MEM)
(DEFVAR MICRO-CODE-SYMBOL-IMAGE)        ;misc instruction vector, starting with misc 200
(DEFVAR MACRO-INSTRUCTION-DECODE)       ;low quarter addressed by ten top bits of uinst.
                                        ;next quarter unused.
                                        ;next quarter unused.
                                        ;next eighth decodes MISC  inst.
                                        ;next eighth decodes MISC2 inst.
(defvar d-mem-free-blocks)
(defvar initial-d-mem-free-blocks)

(DEFUN LAM-LAP-BARF (A B C)
  (when (not (memq a '(pgf-r pgf-r-i
                             pgf-w pgf-w-i
                             pgf-w-unboxed
                             pgf-w-map-reload-only)))
    (TERPRI)
    (PRIN1 (LIST LAM-LAP-LAST-SYM LAM-LAP-WDS-SINCE-LAST-SYM))
    (PRIN1 (LIST A B C))
    (COND ((NOT (or (EQ C 'WARN)
                    (and *bypass-errors*
                         (equal a '(error)))))
           (BREAK "FOO"))))
  0)

;; utility to improve performance ~jrm
(defun measuring-elapsed-time (thunk-to-time receiver)
  (let ((begin-time (time)))
    (multiple-value-prog1
      (funcall thunk-to-time)
      (funcall receiver (// (time-difference (time) begin-time) 60.0)))))

(DEFUN INITIALIZE-FOR-PROCESSOR-TYPE ()
  (SELECTQ *TARGET-PROCESSOR-TYPE*
    (:LAMBDA
     (SETQ *SYMBOL-PROPERTY-FLAGS*
           '(LAM-LAP-SYM LAM-LAP-USER-SYMBOL LAM-LAP-ADDITIVE-CONSTANT))
      (lambda-mode))    ;setup common symbols
    (:EXPLORER
     (SETQ *SYMBOL-PROPERTY-FLAGS*
           '(EXP-LAP-SYM EXP-LAP-USER-SYMBOL EXP-LAP-ADDITIVE-CONSTANT))
     (explorer-mode))))

(DEFUN LAM-LAP-INITIALIZE (INIT-STATE)
  (PROG (TEM)
        (LAM-LAP-INIT-LOCS-FROM-STATE INIT-STATE)
        (SETQ BASE-VERSION-NUMBER (GETF INIT-STATE 'VERSION-NUMBER))
        (SETQ A-MEM-CREVICE-LIST NIL)
        (SETQ D-MEM-FREE-BLOCKS
              (copytree (or (getf init-state 'd-mem-free-blocks)
                            (SELECTQ *TARGET-PROCESSOR-TYPE*
                              (:LAMBDA
                               '(nil (4400 . 3400)  ; (length . start)
                                     ;(100 . 2300)  had to use this for mouse cursor...
                                     ))
                              (:EXPLORER
                               '(NIL (4000 . 0)))))))
        (ALLREMPROP (CADR *SYMBOL-PROPERTY-FLAGS*))
        (SETQ M-CONSTANT-LIST                   ;DUMMY UP SLOTS FOR USAGE COUNT AND LAST
              (COND ((SETQ TEM (GETF INIT-STATE 'M-CONSTANT-LIST))  ;USE
                     (MAPCAR (FUNCTION (LAMBDA (X)
                                         (APPEND X '(100000 NIL) NIL)))
                             TEM))
                    (T NIL)))
        (SETQ A-CONSTANT-LIST
              (COND ((SETQ TEM (GETF INIT-STATE 'A-CONSTANT-LIST))
                     (MAPCAR (FUNCTION (LAMBDA (X)
                                         (APPEND X '(100000 NIL) NIL)))
                             TEM))
                    (T NIL)))
        (SETQ A-CONSTANT-BASE NIL)              ;SEE LAM-LAP-LOC-MODULO
        (SETQ M-CONSTANT-BASE NIL)
        (SETQ A-MEMORY-RANGE-LIST NIL)
        (SETQ M-MEMORY-RANGE-LIST NIL)
        (SETQ I-MEMORY-RANGE-LIST NIL)
        (SETQ D-MEMORY-RANGE-LIST NIL)
        (setq *m-constant-limit* 100
              *a-constant-limit* 4000)
        (SETQ CURRENT-ASSEMBLY-MICRO-ENTRIES NIL)
        (SETQ CURRENT-ASSEMBLY-TABLE NIL)
        (SETQ CURRENT-ASSEMBLY-DECLARATIONS NIL)
  ;do not initialize current-assembly-defmics here computed during readin phase
        (SETQ CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY
              (COND ((GETF INIT-STATE 'HIGHEST-MISC-ENTRY))
                    (T 0)))
        (SETQ MC-LINKAGE-ALIST (GETF INIT-STATE 'MC-LINKAGE-ALIST))
        (DOLIST (E MC-LINKAGE-ALIST)
          (COND ((AND (MEMQ (CADR E) '(A M))
                      (< (CADDR E) 100))                ;*CADR
                 (LAM-LAP-DEFINE-LINKAGE-SYMBOL (CAR E)))))
        (LAM-LAP-ALLOCATE-ARRAYS)
        (ALLREMPROP 'LAM-LAP-B-PTR)
))

(DEFUN MAKE-ASSEMBLER-STATE-LIST NIL
  (LIST 'I-MEM-LOC I-MEM-LOC 'D-MEM-LOC D-MEM-LOC 'A-MEM-LOC A-MEM-LOC
        'M-MEM-LOC M-MEM-LOC 'PAGABLE-I-MEM-LOC PAGABLE-I-MEM-LOC
        'A-CONSTANT-LOC A-CONSTANT-LOC 'A-CONSTANT-BASE A-CONSTANT-BASE
        'M-CONSTANT-LOC M-CONSTANT-LOC 'M-CONSTANT-BASE M-CONSTANT-BASE
        'D-MEM-FREE-BLOCKS D-MEM-FREE-BLOCKS
        'M-CONSTANT-LIST (MAKE-CONSTANT-LIST M-CONSTANT-LIST)
        'A-CONSTANT-LIST (MAKE-CONSTANT-LIST A-CONSTANT-LIST)
        'MICRO-CODE-SYMBOL-TABLE-FILL-VALUE
        (COND ((BOUNDP 'MICRO-CODE-SYMBOL-TABLE-FILL-VALUE)
               MICRO-CODE-SYMBOL-TABLE-FILL-VALUE)
              (T NIL))
        'A-MEMORY-RANGE-LIST A-MEMORY-RANGE-LIST
        'M-MEMORY-RANGE-LIST M-MEMORY-RANGE-LIST
        'I-MEMORY-RANGE-LIST I-MEMORY-RANGE-LIST
        'D-MEMORY-RANGE-LIST D-MEMORY-RANGE-LIST
        'm-constant-limit *m-constant-limit*
        'a-constant-limit *a-constant-limit*
        'MC-LINKAGE-ALIST MC-LINKAGE-ALIST
        'MICRO-ENTRIES CURRENT-ASSEMBLY-MICRO-ENTRIES
        'HIGHEST-MISC-ENTRY CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY
        'VERSION-NUMBER VERSION-NUMBER
        'BASE-VERSION-NUMBER BASE-VERSION-NUMBER        ;nil or version number this
                                                        ; loads into.
        'FILE-TRUENAMES-LISTIFIED FILE-TRUENAMES-LISTIFIED
        ))

(DEFUN MAKE-CONSTANT-LIST (LST)   ;FLUSH USAGE COUNT, LAST LOCN REF'ED AT.
   (MAPCAR (FUNCTION (LAMBDA (X)
                         (LIST (CAR X) (CADR X))))
           LST))

;; Used by micro assembler.
;(DEFVAR MICRO-CODE-SYMBOL-AREA-SIZE 2000)      ;will live in QCOM

(DEFUN LAM-LAP-ALLOCATE-ARRAYS NIL
  (SETQ I-MEM (MAKE-ARRAY 60000)   ;allow 8K for hand coded pagable.
        A-MEM (MAKE-ARRAY 2000)   ;TOP THREE QUARTERS IS REALLY D-MEM ON LAMBDA.
        D-MEM (MAKE-ARRAY 10000)  ;BOTTOM QUARTER IS REALLY A-MEM ON LAMBDA.
        MACRO-INSTRUCTION-DECODE (MAKE-ARRAY 10000))
  (SETQ MICRO-CODE-SYMBOL-IMAGE (MAKE-ARRAY MICRO-CODE-SYMBOL-AREA-SIZE)))

(DEFUN LAM-LAP-INIT-LOCS-FROM-STATE (INIT-STATE)
  (PROG (TEM)
        (SETQ I-MEM-LOC (COND ((GETF INIT-STATE 'I-MEM-LOC)) (T 0)))
        (SETQ PAGABLE-I-MEM-LOC
              (COND ((GETF INIT-STATE 'PAGABLE-I-MEM-LOC))
                    (T 40000)))
        (SETQ D-MEM-LOC (COND ((GETF INIT-STATE 'D-MEM-LOC)) (T 0)))
        (SETQ A-MEM-LOC (COND ((SETQ TEM (GETF INIT-STATE 'A-MEM-LOC))
                               (MAX TEM (COND ((GETF
                                                 INIT-STATE 'A-CONSTANT-LOC))
                                              (T 0))))
                              (T 0)))
        (SETQ M-MEM-LOC (COND ((SETQ TEM (GETF INIT-STATE 'M-MEM-LOC))
                               (MAX TEM (COND ((GETF
                                                  INIT-STATE 'M-CONSTANT-LOC))
                                              (T 0))))
                              (T 0)))
        (setq *m-constant-limit* (cond ((getf init-state 'm-constant-limit))
                                       (t 100)))
        (setq *a-constant-limit* (cond ((getf init-state 'a-constant-limit))
                                       (t 4000)))
))

(DEFVAR PATHNAME-DEFAULTS)

;IF INIT-STATE NON-NIL, ITS REPRESENTS A PREVIOUS ASSEMBLY
; IS TO BE AUGMENTED BY THE CURRENT ASSEMBLY.
;--- see ASSEMBLE-SYSTEM below which is the new interface to this stuff..
(DEFUN ASSEMBLE (&OPTIONAL FN INIT-STATE DONT-RE-READ &AUX INPUT-FILE INPUT-TRUENAME)
 (LET ((FOR-LAMBDA T))

  (PKG-BIND 'LAMBDA                     ;Put user typein into our package during assembly
    (COND ((NOT (BOUNDP 'PATHNAME-DEFAULTS))
           (SETQ PATHNAME-DEFAULTS (FS:MAKE-PATHNAME-DEFAULTS))
           (FS:SET-DEFAULT-PATHNAME "SYS: ULAMBDA; BOOTSTRAP LISP >" PATHNAME-DEFAULTS)))
    (COND ((NULL FN)
           (FORMAT T "~&Enter input file name (default ~A): "
                   (FS:DEFAULT-PATHNAME PATHNAME-DEFAULTS))
           (SETQ FN (READLINE))))
    (SETQ INPUT-FILE (FS:MERGE-AND-SET-PATHNAME-DEFAULTS FN PATHNAME-DEFAULTS))
    (SETQ CONSLP-INPUT
          (SETQ CONSLP-OUTPUT (INTERN (STRING-UPCASE (FUNCALL INPUT-FILE ':NAME)))))
    (SETQ INPUT-TRUENAME (FUNCALL INPUT-FILE ':TRUENAME)
          VERSION-NUMBER (FUNCALL INPUT-TRUENAME ':VERSION))
    (LET* ((TIME (TIME))
           (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
           (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES))
           (si:*features* (copylist si:*features*)))
      (dolist (x '(:CADR :LAMBDA :EXPLORER :EXP))
        (setq si:*features* (delq x si:*features*)))
      (dolist (x (selectq *target-processor-type*
                   (:lambda '(:lambda))
                   (:explorer '(:explorer :exp))))
        (push x si:*features*))
      (COND ((AND DONT-RE-READ (BOUNDP CONSLP-INPUT))
             (FORMAT T "~&Ucode already read in.~%"))
            ((OR INIT-STATE             ;Use regular reader for incremental assembly
                 (NOT (FBOUNDP 'READ-UCODE)))
             (FORMAT T "Reading ~A~%" INPUT-TRUENAME)
             (SETQ CURRENT-ASSEMBLY-DEFMICS NIL)
             (READFILE INPUT-FILE "UA"))
            (T
             (FORMAT T "Reading ~A with fast reader~%" INPUT-TRUENAME)
             (SETQ CURRENT-ASSEMBLY-DEFMICS NIL)
             (READ-UCODE INPUT-FILE)))
      (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
      (FORMAT T "~&Read-in time ~D:~D, ~D disk reads, ~D disk writes~%"
              (TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW)))
    (DOLIST (X CURRENT-ASSEMBLY-DEFMICS)        ;process UA-DEFMICs read
      (APPLY (FUNCTION UA-DO-DEFMIC) X))
    (LET ((TIME (TIME))
          (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
          (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES)))
      (FORMAT T "~&Begin Assembly~%")
      (LAM-LAP-INITIALIZE INIT-STATE)
      (LAM-LAP NIL (SYMEVAL CONSLP-INPUT) INIT-STATE)
      (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
      (FILL-IN-MACRO-INSTRUCTION-DECODE)
      (COND ((NULL INIT-STATE)          ;dont write on incremental assembly
             (WRITE-VARIOUS-OUTPUTS INPUT-FILE)))
      (FORMAT T "~&Assembly time ~D:~D, ~D disk reads, ~D disk writes~%"
              (TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW))))))


(DEFUN WRITE-VARIOUS-OUTPUTS-SYSTEM (OUTPUT-GENERIC-PATHNAME)
  (LET ((OUTPUT-FILE-PREFIX
          (SELECTQ *TARGET-PROCESSOR-TYPE*
            (:LAMBDA "LMC")
            (:EXPLORER "EMC")
            (OTHERWISE (FERROR NIL "")))))
    (COND ((Y-OR-N-P (string-append "WRITE- " output-file-prefix "? "))
           (WRITE-LMC-FILE (FUNCALL OUTPUT-GENERIC-PATHNAME
                                    ':NEW-TYPE
                                    OUTPUT-FILE-PREFIX)
                           BASE-VERSION-NUMBER)
           (WRITE-TBL-FILE (FUNCALL OUTPUT-GENERIC-PATHNAME
                                    ':NEW-TYPE
                                    (STRING-APPEND OUTPUT-FILE-PREFIX "-LOCS")))
           (WRITE-ERROR-TABLE (FUNCALL OUTPUT-GENERIC-PATHNAME
                                       ':NEW-TYPE
                                       (STRING-APPEND OUTPUT-FILE-PREFIX "-TBL")))
           (WRITE-DECLARATION-TABLE (FUNCALL OUTPUT-GENERIC-PATHNAME
                                             ':NEW-TYPE
                                             (STRING-APPEND OUTPUT-FILE-PREFIX "-DCL")))
           (update-comment-file (send output-generic-pathname :new-pathname
                                      :type "COMMENTS"
                                      :version :NEWEST))
           ))))


;obsolete now.  see write-various-outputs-system
(DEFUN WRITE-VARIOUS-OUTPUTS (INPUT-FILE)
  (if (neq *target-processor-type* ':LAMBDA)
      (ferror nil "this old funciton works only for LAMBDA"))
  ;; Binary for the main microcode lives on another directory.
  ;; Allow the user to type the name of the translated file explicitly.
  (LET ((INPUT-FILE-1 INPUT-FILE))
    (OR (EQUAL (FUNCALL INPUT-FILE-1 ':HOST) "SYS")
        (SETQ INPUT-FILE-1 (FUNCALL (FS:DEFAULT-PATHNAME PATHNAME-DEFAULTS "SYS")
                                    ':BACK-TRANSLATED-PATHNAME INPUT-FILE-1)))
    (AND (EQUAL (FUNCALL INPUT-FILE-1 ':DIRECTORY) "ULAMBDA")
         (SETQ INPUT-FILE (FUNCALL INPUT-FILE-1 ':NEW-DIRECTORY "UBIN"))))
  (SETQ CONSLP-OUTPUT-PATHNAME (FUNCALL INPUT-FILE ':NEW-PATHNAME
                                        ':NAME (STRING CONSLP-OUTPUT)
                                        ':TYPE ':UNSPECIFIC ':VERSION ':UNSPECIFIC))
  (COND ((Y-OR-N-P "WRITE-LMC? ")
         (WRITE-LMC BASE-VERSION-NUMBER)
         (WRITE-TBL-FILE (FUNCALL CONSLP-OUTPUT-PATHNAME
                                  ':NEW-PATHNAME ':TYPE "LMC-LOCS"
                                  ':VERSION VERSION-NUMBER))
         (WRITE-ERROR-TABLE (FUNCALL CONSLP-OUTPUT-PATHNAME
                                     ':NEW-TYPE-AND-VERSION
                                     "LMC-TBL" VERSION-NUMBER))
         (WRITE-DECLARATION-TABLE (FUNCALL CONSLP-OUTPUT-PATHNAME
                                           ':NEW-TYPE-AND-VERSION
                                           "LMC-DCL" VERSION-NUMBER)))))


(defun make-ucode-for-lambda (&optional (system 'lambda-ucode))
  (setq *target-processor-type* ':LAMBDA)
  (make-system system :noconfirm))

(defun make-ucode-for-explorer (&optional (system 'ucode))
  (setq *target-processor-type* ':EXPLORER)
  (make-system system :noconfirm))

;somewhat fake interface to make-system.  Main advantage is it allows UCADR file to be split.
;sample DEFSYSTEM looks like:
(COMMENT

  (SI:DEFINE-SIMPLE-TRANSFORMATION :MICRO-ASSEMBLE lambda:MICRO-ASSEMBLE-SYSTEM-TOP-LEVEL
     lambda:FILE-TEST-ALWAYS ("LISP") ("LMC")
     NIL NIL T) ;LOAD like

  (SI:DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *FILES-TO-MICRO-ASSEMBLE* NIL)

  (DEFSYSTEM UCODE
    (:NAME "Ucode")
    (:PACKAGE "UA")
    (:MICRO-ASSEMBLE ("sys:ulambda;test1" "sys:ulambda;test2")))
  )  ;end comment
 ;note the transformation definition must be in effect when the DEFSYSTEM is evaluated.

;moved to SYS2;MAKSYS
;(DEFMACRO (:USE-FAST-READER SI:DEFSYSTEM-MACRO) (T-OR-NIL)
;  (PUTPROP (LOCF (SI:SYSTEM-PLIST SI:*SYSTEM-BEING-DEFINED*)) T-OR-NIL ':FAST-READ-SWITCH)
;  NIL)

;(DEFMACRO (:OUTPUT-PATHNAME SI:DEFSYSTEM-MACRO) (PATHNAME)
;  (PUTPROP (LOCF (SI:SYSTEM-PLIST SI:*SYSTEM-BEING-DEFINED*)) PATHNAME ':OUTPUT-PATHSTRING)
;  NIL)

(DECLARE (SPECIAL SI:*FILES-TO-MICRO-ASSEMBLE*))

(si:define-make-system-special-variable comment-for-this-microassembly nil)

(DEFUN FILE-TEST-ALWAYS (F1 &OPTIONAL (F2 nil))
  F1 F2
  (unless comment-for-this-microassembly
    (format t "~2&Comment to put into -LOCS file to describe changes in this version.")
    (format t "  (Type END to finish.)~&")
    (setq comment-for-this-microassembly
          (with-timeout ((* 60. 60. 60.)        ;1 hour.  You really SHOULD type a comment...
                         (format t "~2&***NO COMMENT***")
                         "***NO COMMENT***")
            (read-delimited-string))))
  T)


(DEFUN MICRO-ASSEMBLE-SYSTEM-TOP-LEVEL (INFILE &OPTIONAL (OUTFILE nil))
 ;  (FORMAT T "~% infile : ~s outfile: ~s" INFILE OUTFILE)
  OUTFILE  ;We don't have an output file for each input file anyway.
  (COND ((NULL SI:*FILES-TO-MICRO-ASSEMBLE*)
         (PUSH `(MICRO-ASSEMBLE-SYSTEM-DO-IT)
               SI:*MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*)))
  (SETQ SI:*FILES-TO-MICRO-ASSEMBLE*
        (NCONC SI:*FILES-TO-MICRO-ASSEMBLE* (LIST INFILE))))

(DEFUN MICRO-ASSEMBLE-SYSTEM-TOP-LEVEL-experimental (INFILE &OPTIONAL (OUTFILE nil))
 ;  (FORMAT T "~% infile : ~s outfile: ~s" INFILE OUTFILE)
  OUTFILE  ;We don't have an output file for each input file anyway.
  (COND ((NULL SI:*FILES-TO-MICRO-ASSEMBLE*)
         (PUSH `(MICRO-ASSEMBLE-SYSTEM-DO-IT t)
               SI:*MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*)))
  (SETQ SI:*FILES-TO-MICRO-ASSEMBLE*
        (NCONC SI:*FILES-TO-MICRO-ASSEMBLE* (LIST INFILE))))

(DEFUN MICRO-ASSEMBLE-SYSTEM-DO-IT (&rest ignore)
  ;(FORMAT T "~%micro-assemble-do-it ~s" SI:*FILES-TO-MICRO-ASSEMBLE*)
  (LET* ((FOR-LAMBDA T)
         (si:*features* (copylist si:*features*))
         (FILE-TRUENAMES (MAPCAR #'(LAMBDA (X) (FUNCALL X ':TRUENAME))
                                 SI:*FILES-TO-MICRO-ASSEMBLE*))
         (FILE-TRUENAMES-LISTIFIED (MAPCAR #'(LAMBDA (X) (LISTIFY-PATHNAME X))
                                           FILE-TRUENAMES))
         (OUTPUT-PATHSTRING (GET (LOCF (SI:SYSTEM-PLIST SI:*SYSTEM-BEING-MADE*))
                                 ':OUTPUT-PATHSTRING))
         (OUTPUT-GENERIC-PATHNAME (FS:PARSE-PATHNAME OUTPUT-PATHSTRING))
         (emc-version)
         (lmc-version))
    (let ((probe (probef (send output-generic-pathname :new-type "LMC"))))
      (setq lmc-version (if probe (send (send probe :truename) :version) 0)))
    (let ((probe (probef (send output-generic-pathname :new-type "EMC"))))
      (setq emc-version (if probe (send (send probe :truename) :version) 0)))
    (dolist (x '(:CADR :LAMBDA :EXPLORER :EXP))
      (setq si:*features* (delq x si:*features*)))
    (dolist (x (selectq *target-processor-type*
                 (:lambda '(:lambda))
                 (:explorer '(:explorer :exp))))
      (push x si:*features*))
    (pushnew :cdr-next-is-0 si:*features*)
    (SETQ VERSION-NUMBER (1+ (max lmc-version emc-version)))
    (SETQ OUTPUT-GENERIC-PATHNAME (FUNCALL OUTPUT-GENERIC-PATHNAME
                                           ':NEW-VERSION VERSION-NUMBER))
    (FORMAT T "~%Output will be version ~D" VERSION-NUMBER)
    (ASSEMBLE-SYSTEM OUTPUT-GENERIC-PATHNAME
                     FILE-TRUENAMES NIL NIL
                     (GET (LOCF (SI:SYSTEM-PLIST SI:*SYSTEM-BEING-MADE*))
                          ':FAST-READ-SWITCH))))

(DEFUN LISTIFY-PATHNAME (PATHNAME)
  (LIST (FUNCALL (FUNCALL PATHNAME ':HOST) ':NAME)
        (FUNCALL PATHNAME ':DEVICE)
        (FUNCALL PATHNAME ':NAME)
        (FUNCALL PATHNAME ':TYPE)
        (FUNCALL PATHNAME ':VERSION)))

;; Remember the files that comprise this microcode, for later output into the
;; LMC-LOCS file.
(defvar *files-comprising-this-microcode*)

(defun dump-ass (&optional &key filename (processor :lambda))
  (or filename
      (setq filename (format nil "SYS:ULAMBDA;~A-MICROCODE QFASL >" processor)))
  (or (= (length *files-comprising-this-microcode*)
         (length (subset #'(lambda (x) (not (string-equal "UCODE" (send x :name))))
                         (si:system-source-files :lambda-ucode))))
      (ferror nil "inconsistent assembly state"))
  (let ((l *files-comprising-this-microcode*)
        (forms)
        (sexp-key (ecase processor
                    (:LAMBDA 'UA-LAMBDA-SEXP)
                    (:EXPLORER 'UA-EXPLORER-SEXP)))
        (defmic-key (ecase processor
                      (:LAMBDA 'LAMBDA-DEFMICS)
                      (:EXPLORER 'EXPLORER-DEFMICS))))
    (dolist (f l)
      (let ((sexp (send f :get sexp-key))
            (defmic (send f :get defmic-key)))
        (or sexp (ferror nil "File ~A has no ~S property" f sexp-key))
        (push `(funcall ',f :putprop ',sexp ',sexp-key) forms)
        (push `(funcall ',f :putprop ',defmic ',defmic-key) forms)))
    (let ((time (time)))
      (format t "~&Dumping data to ~A" filename)
      (COMPILER:DUMP-FORMS-TO-FILE filename
                                   forms
                                   '(:package :lam))
      (setq time (quotient (time-difference (time) time) 60.0))
      (format t " took ~\scientific\seconds.~%" time)
      filename)))

;IF INIT-STATE NON-NIL, ITS REPRESENTS A PREVIOUS ASSEMBLY
; IS TO BE AUGMENTED BY THE CURRENT ASSEMBLY.
(DEFUN ASSEMBLE-SYSTEM (OUTPUT-GENERIC-PATHNAME
                        FILE-TRUENAMES INIT-STATE RE-READ USE-FAST-READER)
  (setq *files-comprising-this-microcode* file-truenames)
  (INITIALIZE-FOR-PROCESSOR-TYPE)
  (unless (memq *target-processor-type* *georges-quick-file-read-is-loaded*)
    (let ((fn (format nil "SYS:ULAMBDA;~A-MICROCODE QFASL >"
                  (case *target-processor-type*
                    (:lambda 'lambda)
                    (:explorer 'explorer)))))
      (cond ((probef fn)
             (load fn)
             (push *target-processor-type* *georges-quick-file-read-is-loaded*)))))
  (PKG-BIND 'LAMBDA                     ;Put user typein into our package during assembly
    (LET ((TIME (TIME))
          (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
          (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES)))
      (DOLIST (FILE-TRUENAME FILE-TRUENAMES)
          ;read in S-exp if necessary.  Also save DEFMICSs on property list of TRUENAME.
        (ASSEMBLE-READ-FILE FILE-TRUENAME USE-FAST-READER RE-READ))
      (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
      (FORMAT T "~&Read-in time ~D:~D, ~D disk reads, ~D disk writes~%"
              (TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW)))
    (DOLIST (FILE-TRUENAME FILE-TRUENAMES)
      (DOLIST (X (FUNCALL FILE-TRUENAME ':GET
                          (SELECTQ *TARGET-PROCESSOR-TYPE*
                            (:LAMBDA 'LAMBDA-DEFMICS)
                            (:EXPLORER 'EXPLORER-DEFMICS))))    ;process UA-DEFMICs
        (APPLY (FUNCTION UA-DO-DEFMIC) X)))
    (LET ((TIME (TIME))
          (DR (READ-METER 'SI:%COUNT-DISK-PAGE-READS))
          (DW (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES)))
      (FORMAT T "~&Begin Assembly~%")
      (measuring-elapsed-time
        #'(lambda ()  (LAM-LAP-INITIALIZE INIT-STATE))
        #'(lambda (time-interval) (format t "Initialization took ~d seconds." time-interval)))
      (LAM-LAP T FILE-TRUENAMES INIT-STATE)
      (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
      (measuring-elapsed-time
        #'(lambda () (FILL-IN-MACRO-INSTRUCTION-DECODE))
        #'(lambda (time-interval) (format t "~%FILL-IN-MACRO-INSTRUCTION-DECODE took ~D seconds." time-interval)))
      (COND ((NULL INIT-STATE)                  ;dont write on incremental assembly
             (WRITE-VARIOUS-OUTPUTS-SYSTEM OUTPUT-GENERIC-PATHNAME)))
      (FORMAT T "~&Assembly time ~D:~D, ~D disk reads, ~D disk writes~%"
              (TRUNCATE TIME 3600.) (\ (TRUNCATE TIME 60.) 60.)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-READS) DR)
              (- (READ-METER 'SI:%COUNT-DISK-PAGE-WRITES) DW)))))

(DEFUN ASSEMBLE-READ-FILE (FILE-TRUENAME USE-FAST-READER RE-READ)
  (COND ((OR RE-READ
             (NULL (FUNCALL FILE-TRUENAME ':GET
                            (SELECTQ *TARGET-PROCESSOR-TYPE*
                              (:LAMBDA 'UA-LAMBDA-SEXP)
                              (:EXPLORER 'UA-EXPLORER-SEXP)))))
         (LET ((CURRENT-ASSEMBLY-DEFMICS NIL)
               (NAME (INTERN (STRING-UPCASE (FUNCALL FILE-TRUENAME ':NAME)))))
           (MAKUNBOUND NAME)
           (COND (USE-FAST-READER
                  (FORMAT T "~%Reading ~A with fast reader" FILE-TRUENAME)
                  (READ-UCODE FILE-TRUENAME))
                 (T
                  (FORMAT T "~%Reading ~A" FILE-TRUENAME)
                  (READFILE FILE-TRUENAME PACKAGE)))
           (IF (NOT (BOUNDP NAME))
               (FERROR NIL "~%Reading ~s failed to set the symbol ~s" FILE-TRUENAME NAME))
           (FUNCALL FILE-TRUENAME ':PUTPROP (SYMEVAL NAME)
                    (SELECTQ *TARGET-PROCESSOR-TYPE*
                      (:LAMBDA 'UA-LAMBDA-SEXP)
                      (:EXPLORER 'UA-EXPLORER-SEXP)))
           (FUNCALL FILE-TRUENAME ':PUTPROP CURRENT-ASSEMBLY-DEFMICS
                    (SELECTQ *TARGET-PROCESSOR-TYPE*
                      (:LAMBDA 'LAMBDA-DEFMICS)
                      (:EXPLORER 'EXPLORER-DEFMICS)))))
        (T (FORMAT T "~%Already read ~S" FILE-TRUENAME))))

(DEFUN LAM-LAP (ARG-IS-LIST-OF-TRUENAMES ARG LAM-LAP-INIT-STATE)
  (PROG (;I-MEM-LOC  PAGABLE-I-MEM-LOC
         ;D-MEM-LOC A-MEM-LOC M-MEM-LOC M-CONSTANT-LOC A-CONSTANT-LOC ;USE TOP LEVEL
         ;M-CONSTANT-LIST A-CONSTANT-LIST M-CONSTANT-BASE A-CONSTANT-BASE  ;BINDINGS FOR THESE
         ;D-MEM-FREE-BLOCKS MICRO-CODE-SYMBOL-TABLE-FILL-VALUE
         ;A-MEMORY-RANGE-LIST M-MEMORY-RANGE-LIST I-MEMORY-RANGE-LIST D-MEMORY-RANGE-LIST
         ;CURRENT-ASSEMBLY-MICRO-ENTRIES CURRENT-ASSEMBLY-TABLE CURRENT-ASSEMBLY-DEFMICS
         ;CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY CURRENT-ASSEMBLY-DECLARATIONS
         ;MC-LINKAGE-ALIST *MACRO-IR-DISPATCH-ALIST* *MACRO-IR-DISPATCH-SPEC*
         ;*MACRO-IR-DISPATCH-ALIST*
         *COMMENT-DEPTH*

         *PAGABLE-UCODE-MODE*   ;automatically avoid next uinst dependancies across pg boundaries
         ;*NOOPS-INSERTED-DUE-TO-PAGING* *PREVIOUS-UINST-LINKED-TO-NEXT*
         INIT-PAGABLE-I-MEM-LOC
         INITIAL-A-MEM-LOC INITIAL-M-MEM-LOC INITIAL-I-MEM-LOC ;INITIAL-D-MEM-FREE-BLOCKS
         LOCALITY
         IN-DISPATCH-BLOCK LAM-LAP-LAST-SYM LAM-LAP-WDS-SINCE-LAST-SYM
         DISPATCH-BLOCK-LIMIT DISPATCH-ARM LAM-LAP-PASS2
         DISPATCH-CONSTANT ARG-CALL-LIST)
        (SETQ LAM-LAP-WDS-SINCE-LAST-SYM 0)

        (SETQ *DIRECT-BITS*
              (SELECTQ *TARGET-PROCESSOR-TYPE*
                (:LAMBDA
                 (LAM-IR-DIRECT-BITS))
                (:EXPLORER
                 (EXP-IR-DIRECT-BITS))))
        (SETQ *NOOPS-INSERTED-DUE-TO-PAGING* 0)
        (SETQ INITIAL-A-MEM-LOC A-MEM-LOC INITIAL-M-MEM-LOC M-MEM-LOC
              INITIAL-I-MEM-LOC I-MEM-LOC)
        (SETQ INITIAL-D-MEM-FREE-BLOCKS (COPYTREE D-MEM-FREE-BLOCKS))
        (SETQ *MACRO-IR-DISPATCH-ALIST* NIL
              *MACRO-IR-DISPATCH-SPEC* NIL
              *MACRO-IR-MISC-DISPATCH-ALIST* NIL)
        (SETQ *COMMENT-DEPTH* 0)
        (SETQ *PREVIOUS-UINST-LINKED-TO-NEXT* NIL)
        (setq init-pagable-i-mem-loc pagable-i-mem-loc)
        (IF ARG-IS-LIST-OF-TRUENAMES
            (DOLIST (*UA-TRUENAME* ARG)
              (LET ((BEG-I-MEM-LOC I-MEM-LOC)
                    (BEG-PAGABLE-I-MEM-LOC PAGABLE-I-MEM-LOC))
                (DOLIST (S (FUNCALL *UA-TRUENAME* ':GET
                                    (SELECTQ *TARGET-PROCESSOR-TYPE*
                                      (:LAMBDA 'UA-LAMBDA-SEXP)
                                      (:EXPLORER 'UA-EXPLORER-SEXP))))
                  (LAM-LAP-PASS1 S))
                (FORMAT T "~%File ~A assembled into ~D. I-MEM locs plus ~D pagable"
                        (send *UA-TRUENAME* :name)
                        (- I-MEM-LOC BEG-I-MEM-LOC)
                        (- PAGABLE-I-MEM-LOC BEG-PAGABLE-I-MEM-LOC))
                (IF (not (zerop *COMMENT-DEPTH*))
                    (FORMAT T "~%Warning! file ~s ended in a comment section!" *UA-TRUENAME*))
                (IF *PAGABLE-UCODE-MODE*
                    (FORMAT T "~%Warning! file ~s ended in a pagable ucode section!"
                            *UA-TRUENAME*))
                ))
          (DOLIST (S ARG)
            (LAM-LAP-PASS1 S))
          (IF (not (zerop *COMMENT-DEPTH*)) (FORMAT T "~%Sexp ended in a comment section!"))
          )
        (SETQ M-CONSTANT-LOC (SETQ M-CONSTANT-BASE M-MEM-LOC))
        (SETQ A-CONSTANT-LOC (SETQ A-CONSTANT-BASE A-MEM-LOC))
        (SETQ LAM-LAP-LAST-SYM NIL)
        (SETQ LAM-LAP-WDS-SINCE-LAST-SYM 0)
        (SETQ LAM-LAP-PASS2 T)
        (LAM-LAP-INIT-LOCS-FROM-STATE LAM-LAP-INIT-STATE)
        (SETQ PAGABLE-I-MEM-LOC INIT-PAGABLE-I-MEM-LOC)
        (IF ARG-IS-LIST-OF-TRUENAMES
            (DOLIST (*UA-TRUENAME* ARG)
              (DOLIST (S (FUNCALL *UA-TRUENAME* ':GET
                                  (SELECTQ *TARGET-PROCESSOR-TYPE*
                                    (:LAMBDA 'UA-LAMBDA-SEXP)
                                    (:EXPLORER 'UA-EXPLORER-SEXP))))
                (LAM-LAP-PASS2 S)))
          (DOLIST (S ARG)
            (LAM-LAP-PASS2 S)))
        (COND ((NOT (= M-MEM-LOC M-CONSTANT-BASE))
               (LAM-LAP-BARF (LIST M-MEM-LOC M-CONSTANT-BASE) 'CLD-M-MEM 'BARF)))
        (COND ((NOT (= A-MEM-LOC A-CONSTANT-BASE))
               (LAM-LAP-BARF (LIST A-MEM-LOC A-CONSTANT-BASE) 'CLD-A-MEM 'BARF)))
        (cond ((> m-constant-loc *m-constant-limit*)
               (lam-lap-barf m-constant-loc 'm-mem-overflow 'barf)))
        (cond ((> a-constant-loc *a-constant-limit*)
               (lam-lap-barf a-constant-loc 'a-mem-overflow 'barf)))
        (SETQ LOCALITY 'M-MEM)
        (LAM-LAP-STORE-CONSTANT-LIST A-MEM
                                      M-CONSTANT-LIST)  ;THIS STORES
                        ;THE COMPLETE LIST (INCLUDING THOSE FROM PREVIOUS ASSEMBLY)
                        ;BUT I GUESS THATS OK.
        (SETQ LOCALITY 'A-MEM)
        (LAM-LAP-STORE-CONSTANT-LIST A-MEM
                                      A-CONSTANT-LIST)
        (SETQ A-MEMORY-RANGE-LIST (CONS (LIST INITIAL-A-MEM-LOC
                                              (- (MAX A-MEM-LOC A-CONSTANT-LOC)
                                                 INITIAL-A-MEM-LOC))
                                        A-MEMORY-RANGE-LIST))
        (SETQ M-MEMORY-RANGE-LIST (CONS (LIST INITIAL-M-MEM-LOC
                                              (- (MAX M-MEM-LOC M-CONSTANT-LOC)
                                                 INITIAL-M-MEM-LOC))
                                        M-MEMORY-RANGE-LIST))
        (SETQ I-MEMORY-RANGE-LIST (CONS (LIST INITIAL-I-MEM-LOC
                                              (- I-MEM-LOC INITIAL-I-MEM-LOC))
                                        I-MEMORY-RANGE-LIST))
        (LET ((TEM (FIND-D-MEM-RANGES-USED
                      (CDR INITIAL-D-MEM-FREE-BLOCKS)
                      (CDR D-MEM-FREE-BLOCKS))))
          (COND (TEM (SETQ D-MEMORY-RANGE-LIST (APPEND TEM D-MEMORY-RANGE-LIST)))))
        (RETURN "Now do (WRITE-VARIOUS-OUTPUTS) and//or (LAM-DUMP-MEMORIES)")))

;Taken from LISPM;UTIL.  This is used in reading in the DEFMIC file.
;Only sets up the QLVAL property, not the QINTCMP property and not the function lists.
(defmacro defmic (&whole x)
  `(internal-defmic-function (quote ,(cdr x))))

(DEFUN internal-DEFMIC-function (x) ;x is quoted
  (PROG (NAME OPCODE ARGLIST LISP-FUNCTION-P NO-QINTCMP
         FUNCTION-NAME INSTRUCTION-NAME)
    (SETQ NAME (CAR X)
          OPCODE (CADR X)
          ARGLIST (CADDR X)
          LISP-FUNCTION-P (CADDDR X))
    (AND (CDDDDR X) (SETQ NO-QINTCMP (CAR (CDDDDR X))))
    (COND ((ATOM NAME)
           (SETQ FUNCTION-NAME NAME INSTRUCTION-NAME NAME))
          ((SETQ FUNCTION-NAME (CAR NAME) INSTRUCTION-NAME (CDR NAME))))
    (PUTPROP INSTRUCTION-NAME OPCODE 'QLVAL)))

(defvar error-table-string)
(defun make-error-table-string ()
  (setq error-table-string (make-array 30000. :type :art-string :leader-list '(0)))
  (with-output-to-string (s error-table-string)
    (let ((base 8))
      (print `(SETQ MICROCODE-ERROR-TABLE-VERSION-NUMBER
                    ,VERSION-NUMBER)
             s)
      (princ "(setq microcode-error-table '(" s)
      (dolist (i current-assembly-table)
        (print i s))
      (princ "))" s)
      )))

(DEFUN WRITE-ERROR-TABLE (FN &AUX (BASE 8.))    ;Defend against base screw, KHS 9/20/84.
  (WITH-OPEN-FILE (OUTPUT-FILE FN '(:OUT :BLOCK))
    (PRINT `(SETQ MICROCODE-ERROR-TABLE-VERSION-NUMBER
                  ,VERSION-NUMBER)
           OUTPUT-FILE)
    (TERPRI OUTPUT-FILE)
    (PRINC "(SETQ MICROCODE-ERROR-TABLE '(" OUTPUT-FILE)
    (DOLIST (I CURRENT-ASSEMBLY-TABLE)
      (PRINT I OUTPUT-FILE))
    (PRINC "))" OUTPUT-FILE)
    (TERPRI OUTPUT-FILE)))

(DEFUN WRITE-DECLARATION-TABLE (FN &AUX (BASE 8.))      ;Defend against base screw, KHS 9/20/84.
  (WITH-OPEN-FILE (OUTPUT-FILE FN '(:OUT :BLOCK))
    (PRINC "(SETQ MICROCODE-DECLARATION-TABLE '(" OUTPUT-FILE)
    (DOLIST (I CURRENT-ASSEMBLY-DECLARATIONS)
      (PRINT I OUTPUT-FILE))
    (PRINC "))" OUTPUT-FILE)
    (TERPRI OUTPUT-FILE)))

(defun update-comment-file (filename &aux info)
  (cond ((not (null (probef filename)))
         (with-open-file (f filename)
           (setq info (read f)))))
  (setq info (nconc info
                    (list (list (format nil "~d." version-number)
                                (format nil "~\datime\")
                                (string *target-processor-type*)
                                user-id
                                comment-for-this-microassembly))))
  (with-open-file (f filename :direction :output)
    (format f "(~&")
    (dolist (entry info)
      (format f "~& ~s" entry))
    (format f "~&)~&")))

(defun write-tbl-file (fn)
  (with-open-file (output fn '(:out))
    (format output "~%LMI Lambda microcode version ~D" version-number)
    (format output "~%Produced by /"~A/", ~\datime\" user-id)
    (format output "~%~%~A" comment-for-this-microassembly)
    (format output "~%~%Locations used:")
    (let ((a-top (max a-mem-loc a-constant-loc)))
      (format output "~%  A-memory: ~D." a-top)
      (cond ((> a-top *a-constant-limit*)
             (format t "~%**WARNING: you have a A-MEMORY overflow!!!")
             (format output "~%**WARNING: A-MEMORY has overflowwed!!"))))
    (format output "~%  M-memory: ~D." (max m-mem-loc m-constant-loc))
    (format output "~%  I-memory: ~D." i-mem-loc)
    (when (>= i-mem-loc (ecase *TARGET-PROCESSOR-TYPE*
                          (:lambda #o36000)
                          (:explorer #o40000)))
      (format t "~%**WARNING: I-MEM overflow")
      (format output "~%**WARNING: I-MEM overflow"))
    (format output "~%  Pagable-I-memory: ~D." (- pagable-i-mem-loc 40000))
    (format output "~%  D-memory: ~D." (- (get-d-mem-free-locs (cdr initial-d-mem-free-blocks))
                                          (get-d-mem-free-locs (cdr d-mem-free-blocks))))
    (format output "~%~%Component files:")
    (dolist (pathname *files-comprising-this-microcode*)
      (format output "~%  ~A" (send pathname :string-for-printing)))
    (format output "~%~%"))
  fn)

;For each old free block, determine what part of it has been used and
; make a list of those ranges.
(DEFUN FIND-D-MEM-RANGES-USED (OLD-FREE-BLOCKS NEW-FREE-BLOCKS)
  (PROG (ANS SA LEN NEW-SA NEW-LEN)
     L  (COND ((NULL OLD-FREE-BLOCKS) (RETURN ANS)))
        (SETQ SA (CDAR OLD-FREE-BLOCKS) LEN (CAAR OLD-FREE-BLOCKS))
     L1 (MULTIPLE-VALUE (NEW-SA NEW-LEN)
          (FIND-NEXT-FREE-BLOCK-HIGHER-OR-EQUAL SA NEW-FREE-BLOCKS))
        (COND ((NULL NEW-SA)
               (SETQ ANS (CONS (LIST SA LEN) ANS))    ;EVIDENTLY, BLOCK MUST BE USED NOW
               (GO X1))
              ((NOT (= SA NEW-SA))
               (SETQ ANS (CONS (LIST SA (MIN LEN (- NEW-SA SA)))  ;PART (OR ALL) BLOCK USED
                               ANS))))
        (SETQ LEN (- LEN (- (+ NEW-SA NEW-LEN) SA)))    ;ADVANCE TO ABOVE THAT ONE
        (COND ((<= LEN 0) (GO X1))
              (T (SETQ SA (+ NEW-SA NEW-LEN))
                 (GO L1)))
     X1 (SETQ OLD-FREE-BLOCKS (CDR OLD-FREE-BLOCKS))
        (GO L)))

(DEFUN FIND-NEXT-FREE-BLOCK-HIGHER-OR-EQUAL (SA FREE-BLOCKS)
  (PROG (ANS)
     L  (COND ((NULL FREE-BLOCKS)
               (COND ((NULL ANS) (RETURN NIL))
                     (T (RETURN (CDR ANS) (CAR ANS)))))
              ((AND (>= (CDAR FREE-BLOCKS) SA)
                    (OR (NULL ANS)
                        (< (CDAR FREE-BLOCKS) (CDR ANS))))
               (SETQ ANS (CAR FREE-BLOCKS))))
        (SETQ FREE-BLOCKS (CDR FREE-BLOCKS))
        (GO L)))

(DEFUN GET-D-MEM-FREE-LOCS (X)
  (COND ((NULL X) 0)
        (T (+ (CAAR X) (GET-D-MEM-FREE-LOCS (CDR X))))))

(DEFUN LAM-LAP-STORE-CONSTANT-LIST (MEM L)
  (PROG NIL
 L      (COND ((NULL L) (RETURN NIL)))
        (STORE (ARRAYCALL NIL MEM (CADAR L)) (CAAR L))
        (SETQ L (CDR L))
        (GO L)))

;CONSTANT LISTS.
;A LIST OF LISTS.  CAR IS VALUE OF CONSTANT, CADR IS ADDRESS, CADDR IS #USERS, CADDDR IS
;       LAST PC TO USE IT.

; ARG IS A-CONSTANT-LIST OR M-CONSTANT-LIST
(DEFUN LAM-LAP-REPORT-CONSTANTS-USAGE (L)
  (SETQ L (SORT (copylist L) (FUNCTION (LAMBDA (X Y) (< (CADDR X) (CADDR Y))))))
  (TERPRI)
  (PRINC "#USES VALUE   USEPC")
  (DO L L (CDR L) (NULL L)
    (PRINT (CADDR (CAR L)))
    (TYO 11)
    (PRIN1 (CAAR L))
    (TYO 11)
    (PRIN1 (CADDDR (CAR L))))
  (TERPRI))

(DEFUN LAM-LAP-PASS1 (WD)
  (PROG (CURRENT-WORD V)
        (SETQ CURRENT-WORD WD)                  ;FOR DEBUGGING
        (COND ((not (zerop *COMMENT-DEPTH*))
               (cond ((listp wd)
                      (cond ((eq (car wd) 'begin-comment)
                             (incf *comment-depth*))
                            ((eq (car wd) 'end-comment)
                             (decf *comment-depth*)))))
                (RETURN NIL))
              ((ATOM WD)
               (SETQ LAM-LAP-LAST-SYM WD)
               (SETQ LAM-LAP-WDS-SINCE-LAST-SYM 0)
               (LAM-LAP-DEFSYM
                 WD
                 (LIST LOCALITY
                       (CONS 'FIELD
                             (COND ((EQ LOCALITY 'I-MEM)
                                    (LIST 'JUMP-ADDRESS-MULTIPLIER I-MEM-LOC))
                                   ((EQ LOCALITY 'A-MEM)
                                    (LIST 'A-SOURCE-MULTIPLIER A-MEM-LOC))
                                   ((EQ LOCALITY 'M-MEM)
                                    (LIST 'M-SOURCE-MULTIPLIER M-MEM-LOC))
                                   ((EQ LOCALITY 'D-MEM)
                                    (LIST 'DISPATCH-ADDRESS-MULTIPLIER D-MEM-LOC))
                                   (T (LAM-LAP-BARF LOCALITY
                                                     'BAD-LOCALITY
                                                     'BARF))) )) )
               (COND ((OR (EQ LOCALITY 'M-MEM)          ;automatically MC-LINKAGEify
                          (AND (EQ LOCALITY 'A-MEM)     ; accumulator type frobs.
                               (< A-MEM-LOC 100)))      ;*CADR
                      (LAM-LAP-MC-LINKAGE-STORE WD))))
              ((EQ (CAR WD) 'DEF-DATA-FIELD)
                (DEF-DATA-FIELD (CADR WD)
                                (LAM-LAP-ARG-EVAL (CADDR WD))
                                (LAM-LAP-ARG-EVAL (CADDDR WD))))
              ((EQ (CAR WD) 'DEF-BIT-FIELD-IN-REG)
                (DEF-BIT-FIELD-IN-REG (CADR WD)
                                      (LAM-LAP-ARG-EVAL (CADDR WD))
                                      (LAM-LAP-ARG-EVAL (CADDDR WD))
                                      (CAR (CDDDDR WD))))
              ((EQ (CAR WD) 'ASSIGN)
                (LAM-LAP-DEFSYM (CADR WD)
                                 (CADDR WD)))
              ((EQ (CAR WD) 'ASSIGN-EVAL)
                (LAM-LAP-DEFSYM (CADR WD)
                                 (LAM-LAP-ARG-EVAL (CADDR WD))))
              ((EQ (CAR WD) 'DEF-NEXT-BIT)
                (DEF-NEXT-FIELD (CADR WD) 1 (CADDR WD)))
              ((EQ (CAR WD) 'RESET-BIT-POINTER)
                (RESET-BIT-POINTER (CADR WD)))
              ((EQ (CAR WD) 'DEF-NEXT-FIELD)
                (DEF-NEXT-FIELD (CADR WD)
                                (LAM-LAP-ARG-EVAL (CADDR WD))
                                (CADDDR WD)))
              ((EQ (CAR WD) 'LOCALITY)
                (SETQ LOCALITY (CADR WD))
                (COND ((NOT (MEMQ LOCALITY '(M-MEM A-MEM D-MEM I-MEM)))
                        (LAM-LAP-BARF LOCALITY 'BAD-LOCALITY 'BARF))))
              ((EQ (CAR WD) 'START-DISPATCH)
                (COND ((NOT (EQ LOCALITY 'D-MEM))
                        (LAM-LAP-BARF LOCALITY 'BAD-START-DISPATCH 'BARF)))
                (COND (IN-DISPATCH-BLOCK
                        (LAM-LAP-BARF WD 'ALREADY-IN-DISPATCH 'DATA)))
                (SETQ D-MEM-LOC (FIND-D-MEM-SPACE (EXPT 2 (CADR WD))))
                (SETQ IN-DISPATCH-BLOCK T))
              ((EQ (CAR WD) 'END-DISPATCH)
                (COND ((NULL IN-DISPATCH-BLOCK)
                        (LAM-LAP-BARF WD 'NOT-IN-DISPATCH-BLOCK 'DATA)))
                (COND ((> D-MEM-LOC DISPATCH-BLOCK-LIMIT)
                        (LAM-LAP-BARF D-MEM-LOC
                                       'DISPATCH-BLOCK-OVERFLOW
                                       'DATA))
                      ((NOT (= D-MEM-LOC DISPATCH-BLOCK-LIMIT))
                        (LAM-LAP-BARF (LIST D-MEM-LOC DISPATCH-BLOCK-LIMIT)
                              'DISPATCH-BLOCK-UNDERFLOW
                              'WARN)))
                (SETQ IN-DISPATCH-BLOCK NIL))
              ((MEMQ (CAR WD) '(LOC MODULO))
                (LAM-LAP-LOC-MODULO WD))
              ((EQ (CAR WD) 'REPEAT)
                (LAM-LAP-REPEAT-1 (LAM-LAP-ARG-EVAL (CADR WD))
                                   (CDDR WD)))
              ((MEMQ (CAR WD) '(MISC-INST-ENTRY MC-LINKAGE MC-LINKAGE-VALUE
                                MICRO-CODE-ILLEGAL-ENTRY-HERE ERROR-TABLE ERROR-TABLE-AFTER-NEXT
                                DECLARE
                                MC-ENTRY-ADR MISC-ENTRY-ADR MACRO-IR-DECODE
                                MACRO-IR-MISC-DECODE))
                (GO X))
              ((EQ (CAR WD) 'COMMENT))
              ((EQ (CAR WD) 'IF)
               (COND ((si:EVAL-special-ok (CADR WD))
                      (LAM-LAP-PASS1 (CADDR WD)))
                     (T (MAPC (FUNCTION LAM-LAP-PASS1) (CDDDR WD)))))
              ((EQ (CAR WD) 'BEGIN-COMMENT)
               (incf *COMMENT-DEPTH*)
               (GO X))
              ((eq (car wd) 'end-comment)
               (lam-lap-barf wd 'mismatched-comments 'warn))
              ((EQ (CAR WD) 'MACRO-IR-DISPATCH-SPEC)
               (SETQ *MACRO-IR-DISPATCH-SPEC* (CADR WD))
               (GO X))
              ((eq (car wd) 'begin-pagable-ucode)
               (cond ((eq *target-processor-type* ':LAMBDA)
                      (if *pagable-ucode-mode*
                          (ferror nil "Pagable ucode begun when already pagable"))
                      (setq *pagable-ucode-mode* t)
                      (setq i-mem-loc (prog1 pagable-i-mem-loc
                                             (setq pagable-i-mem-loc i-mem-loc)))))
               (go x))
              ((eq (car wd) 'end-pagable-ucode)
               (cond ((eq *target-processor-type* ':LAMBDA)
                      (if (null *pagable-ucode-mode*)
                          (ferror nil "Pagable ucode ended when not pagable"))
                      (setq *pagable-ucode-mode* nil)
                      (setq i-mem-loc (prog1 pagable-i-mem-loc
                                             (setq pagable-i-mem-loc i-mem-loc)))))
               (go x))
        ;insert this in front of a uinst which is not really linked to the previous uinst,
        ; altho LAMLP thinks it might be.  Notably after dispatch instructions which never
        ; XCT-NEXT. Avoids error warning.
              ((eq (car wd) 'micro-fault-ok-here)
               (setq *previous-uinst-linked-to-next* nil)
               (go x))
              ((get (car wd) 'lam-lap-macro)
               (lam-lap-call-macro 'lam-lap-pass1 wd))
              ((eq (car wd) 'm-constant-limit)
               (setq *m-constant-limit* (cadr wd)))
              ((eq (car wd) 'A-CONSTANT-LIMIT)
               (setq *a-constant-limit* (cadr wd)))
              (T (GO W1)))
X       (RETURN NIL)
W1      (COND ((NULL *PAGABLE-UCODE-MODE*)
               (LAM-LAP-PASS1-WD WD))
              (T (SETQ V (LAM-LAP-PASS1-WD-FOR-VALUE WD))))
        (SETQ LAM-LAP-WDS-SINCE-LAST-SYM (1+ LAM-LAP-WDS-SINCE-LAST-SYM))
        (COND ((EQ LOCALITY 'A-MEM)
               (SETQ A-MEM-LOC (1+ A-MEM-LOC)))
              ((EQ LOCALITY 'M-MEM)
               (SETQ M-MEM-LOC (1+ M-MEM-LOC)))
              ((EQ LOCALITY 'D-MEM)
               (COND ((NOT IN-DISPATCH-BLOCK)
                      (LAM-LAP-BARF WD 'STORAGE-WD-NOT-IN-DISPATCH-BLOCK 'DATA)))
               (SETQ D-MEM-LOC (1+ D-MEM-LOC)))
              ((EQ LOCALITY 'I-MEM)
               (COND ((AND *PAGABLE-UCODE-MODE*
                           (LAM-INSERT-NOOP-FOR-PAGING V))
                      (SETQ *NOOPS-INSERTED-DUE-TO-PAGING* (1+ *NOOPS-INSERTED-DUE-TO-PAGING*))
                      (SETQ I-MEM-LOC (1+ I-MEM-LOC))))         ;INSERT NOOP.
               (SETQ I-MEM-LOC (1+ I-MEM-LOC)))
              (T (LAM-LAP-BARF WD 'STORAGE-WD-IN-BAD-LOCALITY 'DATA)))
        (RETURN NIL)))

;XCT-NEXT and OA-MOD sequences cannot straddle page boundaries in pagable UCODE.
;(If the second page weren't there, taking a micro-fault to get it would not work.)
;This predicate tries to avoid such lossage, however, it is only able to deal with
;sequences of two "interlocked" uinsts.  Unfortunately, three can be useful, such as where
;an OA-MOD is used on a uinst which is an XCT-NEXT.  This has to be outlawwed in
;pagable ucode for now, altho we could easily add a manual frob which would insert two
;no-ops if necessary, etc.  VMA-START-READ, etc, lose because information needed by the
;following CALL-CONDITIONAL-IF-PAGE-FAULT, etc, could be lost in case of micro-fault.
(DEFUN LAM-INSERT-NOOP-FOR-PAGING (V)
  (LET* ((OP (LDB LAM-IR-OP V))         ;can get here only on LAMBDA
         (i-link-to-next
           (and (zerop (ldb (byte 1 65.) v))    ;i-dont-chain
                (OR (= 1 (ldb (byte 1 64.) v))  ;i-chain
                    (= 1 (LDB LAM-IR-POPJ-AFTER-NEXT V))
                    (= OP LAM-OP-DISPATCH)             ;FOR NOW, ANY DISPATCH MIGHT XCT-NEXT
                    (AND (= OP LAM-OP-JUMP)
                         (= 0 (LDB LAM-IR-N V)))
                    (AND (OR (= OP LAM-OP-ALU) (= OP LAM-OP-BYTE))
                         (= 0 (LDB LAM-IR-A-MEM-DEST-FLAG V))
                         (OR (= (LDB LAM-IR-FUNC-DEST V) LAM-FUNC-DEST-IMOD-LOW)
                             (= (LDB LAM-IR-FUNC-DEST V) LAM-FUNC-DEST-IMOD-HIGH)
                             (= (LDB LAM-IR-FUNC-DEST V) LAM-FUNC-DEST-VMA-START-READ)
                             (= (LDB LAM-IR-FUNC-DEST V) LAM-FUNC-DEST-VMA-START-WRITE)
                             (= (LDB LAM-IR-FUNC-DEST V) LAM-FUNC-DEST-MD-START-WRITE)
                             ))))))
    (cond ((and *previous-uinst-linked-to-next*
                (zerop (logand 17 i-mem-loc)))
           (lam-lap-barf i-mem-loc 'first-word-in-ucode-pageable-linked-to-previous 'foobar)))
    (cond ((and *previous-uinst-linked-to-next*
                i-link-to-next)
           (cond ((= 17 (logand 17 i-mem-loc))
                  (lam-lap-barf i-mem-loc
                                'more-than-two-linked-uinst-straddling-a-u-page-boundary
                                'warn))
                 (t
                  (lam-lap-barf i-mem-loc
                                'more-than-two-linked-uinst-luckily-aligned-this-time
                                'warn)))))
    (setq *previous-uinst-linked-to-next*
          i-link-to-next)
    (let ((val (and (= 17 (logand 17 i-mem-loc))        ;last word in page
                    i-link-to-next)))
      (if val (setq *previous-uinst-linked-to-next* nil))
      val)))

(defun lam-lap-call-macro (fctn-ea-word macro)
  (mapcar fctn-ea-word (apply (get (car macro) 'lam-lap-macro) (cdr macro))))

(DEFUN LAM-LAP-LOC-MODULO (WD)
   ((LAMBDA (POINT ITEM)
        (AND (EQ (CAR WD) 'MODULO)
             (SETQ ITEM (* ITEM (CEILING (SYMEVAL POINT) ITEM))))
        (AND (< ITEM (SYMEVAL POINT))
             (not (eq locality 'i-mem))
             (LAM-LAP-BARF WD 'BACKWARDS 'DATA))
        (AND (EQ LOCALITY 'D-MEM)
             (LAM-LAP-D-MEM-LOC ITEM))
        (AND (NULL A-CONSTANT-BASE)     ;ON PASS 1
             (EQ LOCALITY 'A-MEM)       ;KLUDGE TO USE SKIPPED AREA FOR CONSTANTS
             (DO I A-MEM-LOC (1+ I) (= I ITEM)
                (OR (< I 100)           ;*CADR
                    (SETQ A-MEM-CREVICE-LIST (CONS I A-MEM-CREVICE-LIST)))))
        (SET POINT ITEM))
     (CDR (ASSQ LOCALITY '((A-MEM . A-MEM-LOC)
                           (M-MEM . M-MEM-LOC)
                           (D-MEM . D-MEM-LOC)
                           (I-MEM . I-MEM-LOC))))
     (CADR WD)))

;ALLOCATE ONE D-MEM WORD AT A SPECIFIC ADDRESS
(DEFUN LAM-LAP-D-MEM-LOC (L)
  (OR LAM-LAP-PASS2
      (DO ((BL D-MEM-FREE-BLOCKS (CDR BL))
           (TEM))
          ((NULL (CDR BL)) (BREAK "LAM-LAP-D-MEM-LOC"))
        (SETQ TEM (CADR BL))                            ;A BLOCK
        (COND ((AND (NOT (< L (CDR TEM)))               ;IF LOC IS IN THIS BLOCK
                    (< L (+ (CDR TEM) (CAR TEM))))
               (RPLACD BL (CDDR BL))                    ;PATCH OUT THIS BLOCK
               (LAM-LAP-D-MEM-LOC-SPLITUP BL (CDR TEM) L)       ;INSTALL BLOCKS BEFORE LOC
               (LAM-LAP-D-MEM-LOC-SPLITUP BL (1+ L)     ;INSTALL BLOCKS AFTER LOC
                                          (+ (CAR TEM) (CDR TEM)))
               (RETURN NIL)))))
  (SETQ D-MEM-LOC L
        IN-DISPATCH-BLOCK T
        DISPATCH-CONSTANT 0     ;DONT ADD ANYTHING TO THIS ONE.
        DISPATCH-BLOCK-LIMIT (1+ L)))

;SPLIT UP INTO POWER OF 2 BLOCKS
;******* KNOWS THAT D MEM IS 4000 LOCATIONS *******
(DEFUN LAM-LAP-D-MEM-LOC-SPLITUP (BL LOW HIGH)
  (DECLARE (FIXNUM LOW HIGH))
  (PROG (BLOCKSIZE)
    (DECLARE (FIXNUM BLOCKSIZE))
RCR (COND ((= LOW HIGH) (RETURN NIL)))
                 ;COMPUTE LARGEST POWER OF 2 BLOCK STARTING AT LOW
;   (SETQ BLOCKSIZE (BOOLE 1 (+ 10000 LOW) (- 10000 LOW)))
    (SETQ BLOCKSIZE (logand (+ 20000 LOW) (- 20000 LOW)))
A   (COND ((> (+ LOW BLOCKSIZE) HIGH)
           (SETQ BLOCKSIZE (TRUNCATE BLOCKSIZE 2))
           (GO A)))
    (RPLACD BL (CONS (CONS BLOCKSIZE LOW) (CDR BL)))    ;PUT IN THIS BLOCK
    (SETQ BL (CDR BL)   ;DO THE REMAINDER
          LOW (+ LOW BLOCKSIZE))
    (GO RCR)))

(DEFUN LAM-LAP-REPEAT-1 (COUNT LST)
 (PROG (ORPCNT RPCNT)
        (SETQ ORPCNT (LAM-LAP-SYMEVAL 'REPEAT-COUNT))
        (SETQ RPCNT 0)

    L   (COND ((ZEROP COUNT)
               (LAM-LAP-SET 'REPEAT-COUNT ORPCNT)
               (RETURN NIL)))
        (LAM-LAP-SET 'REPEAT-COUNT RPCNT)
        (MAPC (FUNCTION (LAMBDA (X) (LAM-LAP-PASS1 (COND ((ATOM X) (LIST X))
                                                          (T X)))))
              LST)
        (SETQ COUNT (1- COUNT))
        (SETQ RPCNT (1+ RPCNT))
        (GO L)))


;USED IF NOT IN PAGABLE UCODE MODE
(DEFUN LAM-LAP-PASS1-WD (WD)
  (PROG ()
    L   (COND ((ATOM WD) (RETURN NIL))
              ((ATOM (CAR WD)))                 ;FLUSH
              ((MEMQ (CAAR WD)
                     '(ARG-CALL ARG-JUMP ARG-CALL-XCT-NEXT ARG-JUMP-XCT-NEXT))
               (SETQ ARG-CALL-LIST
                     (CONS (CONS I-MEM-LOC (FIND-D-MEM-SPACE 1))
                           ARG-CALL-LIST)))
              ((MEMQ (CAAR WD) '(OA-LOW-CONTEXT OA-HI-CONTEXT))
               (LAM-LAP-PASS1-WD (CDAR WD))))
        (SETQ WD (CDR WD))
        (GO L)))

;USED IF IN PAGABLE UCODE MODE.  RETURN VALUE WHICH HOPEFULLY HAS RIGHT VALUE IN
; OPCODE AND DESTINATION FIELDS (AT LEAST IF DESTINATION IS OA- REG).
(DEFUN LAM-LAP-PASS1-WD-FOR-VALUE (WD)
  (PROG (COMBINED-VALUE COMBINED-INDICATORS DESTINATION-CONTEXT
                        INSTRUCTION-CONTEXT FIELD-INDICATORS FIELD-VALUE
                        DESTINATION-INDICATORS CURRENT-WORD)
        (SETQ COMBINED-VALUE 0)         ;CAUTION! COMBINED-VALUE CAN BE A BIGNUM
        (SETQ CURRENT-WORD WD)          ;SO CAN SEE IT WHEN STUFF COMPILED
        (SETQ INSTRUCTION-CONTEXT 'INSTRUCTION)
    L   (setq field-value nil)
        (COND ((ATOM WD)
               (RETURN
                 (LAM-LAP-DEFAULT-AND-BUGGER
                   INSTRUCTION-CONTEXT
                   COMBINED-VALUE
                   COMBINED-INDICATORS
                   DESTINATION-INDICATORS)))
              ((NUMBERP (CAR WD))
               (SETQ FIELD-VALUE (CAR WD)))
              ((AND (ATOM (CAR WD))
                    (GETL (CAR WD) *SYMBOL-PROPERTY-FLAGS*))
               (SETQ FIELD-VALUE (LAM-LAP-SYM-RUN (CAR WD))))
              ((ATOM (CAR WD)))                 ;FLUSH
              ((MEMQ (CAAR WD)
                     '(ARG-CALL ARG-JUMP ARG-CALL-XCT-NEXT ARG-JUMP-XCT-NEXT))
               (SETQ ARG-CALL-LIST
                     (CONS (CONS I-MEM-LOC (FIND-D-MEM-SPACE 1))
                           ARG-CALL-LIST))
               (ADD-FIELD-INDICATORS 'D-MEM))
              ((MEMQ (CAAR WD) '(OA-LOW-CONTEXT OA-HI-CONTEXT))
               (LAM-LAP-PASS1-WD (CDAR WD)))    ;NOT FOR VALUE...
              ((eq (caar wd) 'i-arg)            ;***???
               )
              (T (LET* ((FIELD-INDICATORS NIL)
                        (V (LAM-DESTINATION-PASS1 (CAR WD))))
                   (COND ((NOT (ZEROP V))
                          (SETQ V (CONVERT-VALUE-TO-DESTINATION V FIELD-INDICATORS))))
                   (SETQ FIELD-VALUE V))))
        (IF FIELD-VALUE
            (SETQ COMBINED-VALUE (FUNNY-PLUS COMBINED-VALUE FIELD-VALUE)))
        (SETQ WD (CDR WD))
        (GO L)))

(DEFUN FIND-D-MEM-SPACE (L)
  (PROG (B P S)
  L0    (SETQ S 20000)  ;SIZE OF BEST BLOCK TO SPLIT SO FAR
        (SETQ P D-MEM-FREE-BLOCKS)
  L     (COND ((NULL (CDR P)) (GO S))
              ((= L (CAADR P))
                (GO X))
              ((AND (> (CAADR P) L)
                    (< (CAADR P) S))
                (SETQ B P)
                (SETQ S (CAADR P))))
        (SETQ P (CDR P))
        (GO L)
  X     (SETQ B (CADR P))
        (RPLACD P (CDDR P))
        (SETQ DISPATCH-BLOCK-LIMIT (+ (CAR B) (CDR B)))
        (RETURN (CDR B))
  S     (COND ((NULL B)
                (LAM-LAP-BARF L 'OUT-OF-D-MEM 'BARF)))
        (RPLACA (CADR B) (LSH S -1))
        (RPLACD D-MEM-FREE-BLOCKS
                (CONS (CONS (LSH S -1)
                            (+ (LSH S -1) (CDADR B)))
                      (CDR D-MEM-FREE-BLOCKS)))
        (SETQ B NIL)
        (GO L0) ))

(DEFUN LAM-LAP-DEFSYM (SYM VAL)
  (PROG (TM)
        (COND ((SETQ TM (LAM-LAP-SYMEVAL SYM))
                (COND ((NOT (EQUAL VAL TM))
                        (LAM-LAP-BARF (LIST VAL TM) 'MULT-DEF-SYM 'DATA))))
              (T (PUTPROP SYM VAL (CADR *SYMBOL-PROPERTY-FLAGS*))))
        (RETURN NIL)))

(DEFUN LAM-LAP-SET (SYM VAL)
  (PUTPROP SYM VAL (CADR *SYMBOL-PROPERTY-FLAGS*)))

(DEFUN LAM-LAP-SYMEVAL (SYM)
  (OR (GET SYM (CAR *SYMBOL-PROPERTY-FLAGS*))
      (GET SYM (CADR *SYMBOL-PROPERTY-FLAGS*))))

(DEFUN LAM-LAP-LISP-SYMEVAL (SYM)
  (OR (BOUNDP SYM) (FERROR NIL "Unbound Lisp Variable ~s" SYM))
  (SYMEVAL SYM))

(DEFUN DEF-DATA-FIELD (SYM BITS BITS-OVER)
  (PROG ()
        (LAM-LAP-DEFSYM SYM
          (LIST 'M-MEM (LIST 'BYTE-FIELD BITS BITS-OVER)))
        (RETURN NIL)))

(DEFUN DEF-BIT-FIELD-IN-REG (SYM BITS BITS-OVER REG)
  (PROG ()
        (LAM-LAP-DEFSYM SYM
          (LIST 'PLUS
                (LIST 'BYTE-FIELD BITS BITS-OVER)
                REG))
        (RETURN NIL)))


(DEFUN RESET-BIT-POINTER (SYM)
  (PROG ()
        (PUTPROP SYM 0 'LAM-LAP-B-PTR)))

(DEFUN DEF-NEXT-FIELD (SYM BITS IN-SYM)
  (PROG (B-PTR IN-SYM-V N-B-PTR)
        (COND ((NOT (ATOM IN-SYM))
                (LAM-LAP-BARF IN-SYM 'BAD-NEXT-FIELD 'DATA)
                (RETURN NIL)))
        (SETQ B-PTR (COND ((GET IN-SYM 'LAM-LAP-B-PTR))
                          (T '0)))
        (COND ((NULL (SETQ IN-SYM-V (LAM-LAP-SYMEVAL IN-SYM)))
                (LAM-LAP-BARF IN-SYM 'UNDEF-IN-DEF-NEXT-FIELD 'DATA)
                (RETURN NIL)))
        (COND ((> (SETQ N-B-PTR (+ BITS B-PTR)) 32.)
                (LAM-LAP-BARF IN-SYM 'OUT-OF-BITS 'DATA)
                (RETURN NIL)))
        (LAM-LAP-DEFSYM SYM (LIST 'PLUS (LIST 'BYTE-FIELD BITS B-PTR)
                                   IN-SYM-V))
        (PUTPROP IN-SYM N-B-PTR 'LAM-LAP-B-PTR)
))

(DEFUN LAM-LAP-PASS2 (WD)
  (PROG (V)
        (COND ((not (zerop *COMMENT-DEPTH*))
               (cond ((listp wd)
                      (cond ((eq (car wd) 'begin-comment)
                             (incf *comment-depth*))
                            ((eq (car wd) 'end-comment)
                             (decf *comment-depth*)))))
                (RETURN NIL))
              ((ATOM WD)
               (SETQ LAM-LAP-LAST-SYM WD)
               (SETQ LAM-LAP-WDS-SINCE-LAST-SYM 0)
               (COND ((AND DISPATCH-ARM
                           (EQ LOCALITY 'D-MEM))
                      (SETQ D-MEM-LOC (LDB COM-IR-DISP-ADDR (LAM-LAP-ARG-EVAL WD)))
                      (SETQ DISPATCH-ARM NIL))
                     ((NOT (EQUAL
                            (LAM-LAP-SYMEVAL WD)
                            (LIST LOCALITY
                                  (CONS 'FIELD
                                        (COND ((EQ LOCALITY 'I-MEM)
                                               (LIST 'JUMP-ADDRESS-MULTIPLIER I-MEM-LOC))
                                              ((EQ LOCALITY 'A-MEM)
                                               (LIST 'A-SOURCE-MULTIPLIER A-MEM-LOC))
                                              ((EQ LOCALITY 'M-MEM)
                                               (LIST 'M-SOURCE-MULTIPLIER M-MEM-LOC))
                                              ((EQ LOCALITY 'D-MEM)
                                               (LIST 'DISPATCH-ADDRESS-MULTIPLIER D-MEM-LOC))
                                              (T (LAM-LAP-BARF LOCALITY
                                                                'BAD-LOCALITY
                                                                'BARF))) )) ))
                      (LAM-LAP-BARF WD 'DEF-DFRS-ON-PASS2 'BARF))))
              ((MEMQ (CAR WD) '(DEF-DATA-FIELD ASSIGN ASSIGN-EVAL DEF-NEXT-BIT
                                               RESET-BIT-POINTER
                                               DEF-NEXT-FIELD END-DISPATCH
                                               DEF-BIT-FIELD-IN-REG
                                               M-CONSTANT-LIMIT A-CONSTANT-LIMIT)))
              ((EQ (CAR WD) 'LOCALITY)
               (SETQ LOCALITY (CADR WD)))
              ((EQ (CAR WD) 'START-DISPATCH)
               (SETQ DISPATCH-CONSTANT (COND ((LAM-LAP-ARG-EVAL (CADDR WD)))
                                             (T 0)))
               (SETQ DISPATCH-ARM T))   ;SET D-MEM-LOC TO NEXT D-MEM SYMBOL ENCOUNTERED
                                        ;ERROR IF STORAGE WORD BEFORE THAT.
              ((MEMQ (CAR WD) '(LOC MODULO))
               (LAM-LAP-LOC-MODULO WD))
              ((EQ (CAR WD) 'REPEAT)
               (LAM-LAP-REPEAT-2 (LAM-LAP-ARG-EVAL (CADR WD))
                                  (CDDR WD)))
              ((EQ (CAR WD) 'MISC-INST-ENTRY)
               (LET ((OPCODE (GET (CADR WD) 'QLVAL)))
                 (COND ((NULL OPCODE)
                        (LAM-LAP-BARF (CADR WD) 'NO-UCODE-ENTRY-INDEX 'WARN))
                       (T
                         (SETQ CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY
                               (MAX OPCODE CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY))
                         (COND ((NULL LAM-LAP-INIT-STATE)
                                (SETF (AREF MICRO-CODE-SYMBOL-IMAGE (- OPCODE 200))
                                      I-MEM-LOC))
                               (T (SETQ CURRENT-ASSEMBLY-MICRO-ENTRIES;in incremental assembly
                                        (CONS (LIST 'MISC-INST-ENTRY (CADR WD) I-MEM-LOC)
                                              CURRENT-ASSEMBLY-MICRO-ENTRIES))))))))
              ((EQ (CAR WD) 'MICRO-CODE-ILLEGAL-ENTRY-HERE)
               (SETQ MICRO-CODE-SYMBOL-TABLE-FILL-VALUE I-MEM-LOC)
               (LAM-LAP-WIPE-SYMBOL-VECTOR I-MEM-LOC))
              ((AND (EQ (CAR WD) 'MC-LINKAGE)
                    (LISTP (CADR WD)))
               (MAPC (FUNCTION LAM-LAP-MC-LINKAGE-STORE) (CADR WD)))
              ((EQ (CAR WD) 'ERROR-TABLE)
               (SETQ CURRENT-ASSEMBLY-TABLE
                     (append CURRENT-ASSEMBLY-TABLE
                            (LIST (CONS (1- I-MEM-LOC) (CDR WD))))))
              ((EQ (CAR WD) 'ERROR-TABLE-AFTER-NEXT)
               (SETQ CURRENT-ASSEMBLY-TABLE
                     (append CURRENT-ASSEMBLY-TABLE
                            (LIST (CONS I-MEM-LOC (CDR WD))))))
              ((EQ (CAR WD) 'DECLARE)
               (SETQ CURRENT-ASSEMBLY-DECLARATIONS
                     (append CURRENT-ASSEMBLY-DECLARATIONS
                            (LIST (CONS I-MEM-LOC (CDR WD))))))
              ((EQ (CAR WD) 'COMMENT))
              ((EQ (CAR WD) 'IF)
               (COND ((si:eval-special-ok (CADR WD))
                      (LAM-LAP-PASS2 (CADDR WD)))
                     (T (MAPC (FUNCTION LAM-LAP-PASS2) (CDDDR WD)))))
              ((EQ (CAR WD) 'BEGIN-COMMENT)
               (incf *COMMENT-DEPTH*)
               (GO X))
              ((eq (car wd) 'end-comment)
               (lam-lap-barf wd 'mismatched-comments 'warn))
              ((EQ (CAR WD) 'MACRO-IR-DISPATCH-SPEC)
               (GO X))
              ((EQ (CAR WD) 'MACRO-IR-DECODE)
               (PUSH (CONS I-MEM-LOC (CADR WD))
                     *MACRO-IR-DISPATCH-ALIST*)
               (GO X))
              ((EQ (CAR WD) 'MACRO-IR-MISC-DECODE)
               (PUSH (CONS I-MEM-LOC (CADR WD))
                     *MACRO-IR-MISC-DISPATCH-ALIST*))
              ((eq (car wd) 'begin-pagable-ucode)
               (cond ((eq *target-processor-type* ':LAMBDA)
                      (if *pagable-ucode-mode*
                          (ferror nil "Pagable ucode begun when already pagable"))
                      (setq *pagable-ucode-mode* t)
                      (setq i-mem-loc (prog1 pagable-i-mem-loc
                                             (setq pagable-i-mem-loc i-mem-loc)))))
               (go x))
              ((eq (car wd) 'end-pagable-ucode)
               (cond ((eq *target-processor-type* ':LAMBDA)
                      (if (null *pagable-ucode-mode*)
                          (ferror nil "Pagable ucode region ended when not pagable"))
                      (setq *pagable-ucode-mode* nil)
                      (setq i-mem-loc (prog1 pagable-i-mem-loc
                                             (setq pagable-i-mem-loc i-mem-loc)))))
               (go x))
        ;insert this in front of a uinst which is not really linked to the previous uinst,
        ; altho LAMLP thinks it might be.  Notably after dispatch instructions which never
        ; XCT-NEXT. Avoids error warning.
              ((eq (car wd) 'micro-fault-ok-here)
               (setq *previous-uinst-linked-to-next* nil)
               (go x))
              ((get (car wd) 'lam-lap-macro)
               (lam-lap-call-macro 'lam-lap-pass2 wd))
              (T (GO W1)))
     X  (RETURN NIL)

     W1 (COND (DISPATCH-ARM
               (LAM-LAP-BARF WD 'STORAGE-WD-IN-UNLOCATED-DISPATCH-BLOCK 'DATA)))
        (SETQ V (LAM-WORD-EVAL WD))
        (SETQ LAM-LAP-WDS-SINCE-LAST-SYM (1+ LAM-LAP-WDS-SINCE-LAST-SYM))
        (COND ((EQ LOCALITY 'A-MEM)
               (COND ((>= A-MEM-LOC (ARRAY-ACTIVE-LENGTH A-MEM))
                      (LAM-LAP-BARF A-MEM-LOC 'A-MEM-OVERFLOW 'DATA))
                     ((>= A-MEM-LOC
                          100)          ;The rest is really m-memory. *CADR
                      (SETF (AREF A-MEM A-MEM-LOC) V)))
               (SETQ A-MEM-LOC (1+ A-MEM-LOC)))
              ((EQ LOCALITY 'M-MEM)
               (COND ((< M-MEM-LOC 100)                 ;*CADR
                      (SETF (AREF A-MEM M-MEM-LOC) V))
                     (T (LAM-LAP-BARF M-MEM-LOC 'M-MEM-OVERFLOW 'DATA)))
               (SETQ M-MEM-LOC (1+ M-MEM-LOC)))
              ((EQ LOCALITY 'D-MEM)
               (SETQ V (+ V DISPATCH-CONSTANT)) ;CONSTANT FOR ENTIRE BLOCK
               (SETQ V (SELECTQ *TARGET-PROCESSOR-TYPE*
                         (:LAMBDA
                          (+ (LSH (LDB LAM-IR-RPN V) 16.)       ;RPN BITS FROM JUMP
                             (LDB LAM-IR-JUMP-ADDR V)           ;PC FROM JUMP
                             (DPB (LDB 0001 V) LAM-DISP-START-MEM-READ-BIT 0))) ;SPECIAL CROCK
                         (:EXPLORER
                          (+ (LSH (LDB COM-IR-RPN V) 14.)
                             (LDB COM-IR-JUMP-ADDR V)))))
               (SETF (AREF D-MEM D-MEM-LOC) V)
               (SETQ D-MEM-LOC (1+ D-MEM-LOC)))
              ((EQ LOCALITY 'I-MEM)
               (COND ((AND *PAGABLE-UCODE-MODE*
                           (LAM-INSERT-NOOP-FOR-PAGING V))
                      (SETF (AREF I-MEM I-MEM-LOC) 0)   ;INSERT NOOP.
                      (SETQ I-MEM-LOC (1+ I-MEM-LOC))))
               (IF ( I-MEM-LOC (ARRAY-ACTIVE-LENGTH I-MEM))
                   (LAM-LAP-BARF I-MEM-LOC 'I-MEM-OVERFLOW 'DATA)
                 (SETF (AREF I-MEM I-MEM-LOC) V))
               (SETQ I-MEM-LOC (1+ I-MEM-LOC)))
              (T (LAM-LAP-BARF WD 'STORAGE-WD-IN-BAD-LOCALITY 'DATA)))
        (RETURN NIL)
        ))

;add symbol to MC-LINKAGE-ALIST
(DEFUN LAM-LAP-MC-LINKAGE-STORE (ELEM)
  (PROG (MC-SYM CONSLP-SYM VAL TEM TYPE)
        (COND ((ATOM ELEM)
               (SETQ MC-SYM ELEM CONSLP-SYM ELEM))
              (T (SETQ MC-SYM (CAR ELEM) CONSLP-SYM (CADR ELEM))))
        (SETQ VAL (GET CONSLP-SYM (CADR *SYMBOL-PROPERTY-FLAGS*)))
    L   (COND ((NULL VAL) (RETURN NIL))
              ((NUMBERP VAL))
              ((ATOM VAL)
                (SETQ VAL (LAM-LAP-SYMEVAL VAL))
                (SETQ TYPE 'N)
                (GO L))
             ((AND (SETQ TEM (ASSQ (CAR VAL)
                        '( (I-MEM JUMP-ADDRESS-MULTIPLIER I)
                           (D-MEM DISPATCH-ADDRESS-MULTIPLIER D)
                           (A-MEM A-SOURCE-MULTIPLIER A)
                           (M-MEM M-SOURCE-MULTIPLIER M))))
                   (EQ (CAADR VAL) 'FIELD)
                   (EQ (CADADR VAL) (CADR TEM)))
              (SETQ VAL (CADDR (CADR VAL)))
              (SETQ TYPE (CADDR TEM)))
             (T (RETURN NIL)))
        (SETQ MC-LINKAGE-ALIST (CONS (LIST MC-SYM TYPE VAL) MC-LINKAGE-ALIST))
        ))



;define MC-LINKAGE symbol as regular symbol
(DEFUN LAM-LAP-DEFINE-LINKAGE-SYMBOL (SYMBOL)
  (LAM-LAP-DEFSYM SYMBOL (LAM-LAP-MC-LINKAGE SYMBOL)))

;(MC-LINKAGE <SYMBOL>)
(DEFUN LAM-LAP-MC-LINKAGE (SYMBOL)
  (PROG (TEM V MULT MEM)
        (COND ((NULL (SETQ TEM (ASS (FUNCTION STRING-EQUAL) SYMBOL MC-LINKAGE-ALIST)))
               (FERROR NIL "~%Undefined MC-LINKAGE symbol ~S" SYMBOL)))
        (SETQ MEM (STRING (CADR TEM)) V (CADDR TEM))
        (COND ((STRING-EQUAL MEM "N") (GO X))
              ((SETQ TEM (ASS (FUNCTION STRING-EQUAL) MEM
                              '(("I" JUMP-ADDRESS-MULTIPLIER I-MEM)
                                ("D" DISPATCH-ADDRESS-MULTIPLIER D-MEM)
                                ("A" A-SOURCE-MULTIPLIER A-MEM)
                                ("M" M-SOURCE-MULTIPLIER M-MEM))))
               (SETQ MULT (CADR TEM) MEM (CADDR TEM)))
              (T (FERROR NIL "~%Unknown memory name ~S" MEM)))
        (SETQ V `(,MEM (FIELD ,MULT ,V)))
    X   (RETURN V)
))

;(MC-LINKAGE-VALUE <MEMORY> <SYMBOL>)
(DEFUN LAM-LAP-MC-LINKAGE-VALUE (MEMORY SYMBOL)
  (PROG (V MULT)
        (COND ((NULL (SETQ V (ASS (FUNCTION STRING-EQUAL) SYMBOL MC-LINKAGE-ALIST)))
               (FERROR NIL "~%Undefined MC-LINKAGE symbol ~S" SYMBOL)))
        (SETQ V (CADDR V))
        (COND ((STRING-EQUAL MEMORY "NUMBER") (GO X))
              ((SETQ MULT (ASS (FUNCTION STRING-EQUAL) MEMORY
                               '( ("I-MEM" . JUMP-ADDRESS-MULTIPLIER)
                                  ("D-MEM" . DISPATCH-ADDRESS-MULTIPLIER)
                                  ("A-MEM" . A-SOURCE-MULTIPLIER)
                                  ("M-MEM" . M-SOURCE-MULTIPLIER))))
               (SETQ MULT (CDR MULT)))
              (T (FERROR NIL "~%Unknown memory name ~S" MEMORY)))
        (SETQ V `(FIELD ,MULT ,V))
    X   (RETURN V)
))

(DEFUN LAM-LAP-WIPE-SYMBOL-VECTOR (QUAN)
  (PROG (IDX END-TEST)
        (SETQ IDX 0)
        (SETQ END-TEST (ARRAY-LENGTH MICRO-CODE-SYMBOL-IMAGE))
     L  (COND ((NOT (< IDX END-TEST))
               (RETURN T))
              ((NULL (AREF MICRO-CODE-SYMBOL-IMAGE IDX))
               (SETF (AREF MICRO-CODE-SYMBOL-IMAGE IDX) QUAN)))
        (SETQ IDX (1+ IDX))
        (GO L)))

(DEFUN LAM-LAP-REPEAT-2 (COUNT LST)
  (PROG (ORPCNT RPCNT)
        (SETQ ORPCNT (LAM-LAP-SYMEVAL 'REPEAT-COUNT))
        (SETQ RPCNT 0)
     L  (COND ((ZEROP COUNT)
               (LAM-LAP-SET 'REPEAT-COUNT ORPCNT)
               (RETURN NIL)))
        (LAM-LAP-SET 'REPEAT-COUNT RPCNT)
        (MAPC (FUNCTION (LAMBDA (X) (LAM-LAP-PASS2 (COND ((ATOM X) (LIST X))
                                                          (T X)))))
              LST)
        (SETQ COUNT (1- COUNT))
        (SETQ RPCNT (1+ RPCNT))
        (GO L)))

(DEFUN LAM-WORD-EVAL (WD)
  (PROG (COMBINED-VALUE COMBINED-INDICATORS DESTINATION-CONTEXT
                        INSTRUCTION-CONTEXT FIELD-INDICATORS FIELD-VALUE TEM TEM1 TEM2
                        DESTINATION-INDICATORS CURRENT-WORD)
        (SETQ COMBINED-VALUE 0)         ;CAUTION! COMBINED-VALUE CAN BE A BIGNUM
        (SETQ CURRENT-WORD WD)          ;SO CAN SEE IT WHEN STUFF COMPILED
        (SETQ INSTRUCTION-CONTEXT 'INSTRUCTION)
     L  (SETQ FIELD-INDICATORS NIL)

        (COND ((NULL WD)
               (RETURN
                 (LAM-LAP-DEFAULT-AND-BUGGER
                   INSTRUCTION-CONTEXT
                   COMBINED-VALUE
                   COMBINED-INDICATORS
                   DESTINATION-INDICATORS)))
              ((NUMBERP (CAR WD))
               (SETQ FIELD-VALUE (CAR WD)))
              ((ATOM (CAR WD))
               (SETQ FIELD-VALUE (LAM-LAP-SYM-RUN (CAR WD))))
              ((EQ (CAAR WD) 'M-CONSTANT)
               (SETQ FIELD-VALUE (LAM-M-CONSTANT (CADAR WD))))
              ((EQ (CAAR WD) 'A-CONSTANT)
               (SETQ FIELD-VALUE (LAM-A-CONSTANT (CADAR WD))))
              ((SETQ TEM
                     (ASSQ (CAAR WD)
                           '((ARG-CALL . 3)                  ;P-BIT N-BIT
                             (ARG-JUMP . 1)                  ;N-BIT
                             (ARG-CALL-XCT-NEXT . 2)         ;P-BIT
                             (ARG-JUMP-XCT-NEXT . 0) )))     ; NONE
               (SETQ TEM1 (LAM-LAP-ARG-EVAL (CADAR WD))) ;TAG
               (SETQ TEM2 (ASSOC I-MEM-LOC ARG-CALL-LIST))
               (COND ((NULL TEM2)
                      (LAM-LAP-BARF I-MEM-LOC
                                     'NO-D-MEM-RESERVED-FOR-ARG-CALL
                                     'BARF)))
               (SETF (AREF D-MEM (CDR TEM2))
                     (+ (DPB (CDR TEM) COM-DISP-RPN-BITS 0)
                        (LDB COM-IR-JUMP-ADDR TEM1)))
               (LAM-GET-NEW-CONTEXT 'FORCE-DISPATCH)
               (ADD-FIELD-INDICATORS 'D-MEM)
               (SETQ FIELD-VALUE (* (CDR TEM2)
                                    (GET 'DISPATCH-ADDRESS-MULTIPLIER
                                         (CAR *SYMBOL-PROPERTY-FLAGS*)))))
              ((MEMQ (CAAR WD) '(byte BYTE-FIELD LISP-BYTE ALL-BUT-LISP-BYTE
                                            FIELD BYTE-MASK BYTE-VALUE PLUS DIFFERENCE
                                            LOGXOR LOGIOR LOGAND CEILING FLOOR
                                            OA-HIGH-CONTEXT OA-LOW-CONTEXT EVAL I-ARG
                                            I-MEM-LOC D-MEM-LOC A-MEM-LOC M-MEM-LOC
                                            MC-LINKAGE MC-LINKAGE-VALUE
                                            MC-ENTRY-ADR MISC-ENTRY-ADR))
               (SETQ FIELD-VALUE (LAM-LAP-EVAL (CAR WD))))
              (T
               (LAM-GET-NEW-CONTEXT 'FORCE-ALU-OR-BYTE)
               (SETQ FIELD-VALUE (LAM-DESTINATION (CAR WD)))

               (SETQ FIELD-VALUE
                     (CONVERT-VALUE-TO-DESTINATION FIELD-VALUE FIELD-INDICATORS))
               (SETQ DESTINATION-INDICATORS FIELD-INDICATORS)
               (SETQ FIELD-INDICATORS NIL)) )

        (SETQ COMBINED-VALUE (FUNNY-PLUS COMBINED-VALUE FIELD-VALUE))
;       (PRINT (LIST (CAR WD) FIELD-VALUE FIELD-INDICATORS))
        (SETQ COMBINED-INDICATORS (MERGE-INDICATORS
                                   FIELD-INDICATORS COMBINED-INDICATORS))
        (SETQ WD (CDR WD))
        (GO L)
        ))


;Do LOGIOR on high bits part, ADD rest.
(defun funny-plus (a b)
  (selectq *target-processor-type*
    (:lambda
     (let ((sum (+ a b))
           (high-bits (logior (ldb com-ir-high-individual-bits a)
                              (ldb com-ir-high-individual-bits b))))
       (dpb high-bits com-ir-high-individual-bits sum)))
    (:explorer
     (+ a b))))


(DEFUN LAM-LAP-DEFAULT-AND-BUGGER
         (INSTRUCTION-CONTEXT COMBINED-VALUE COMBINED-INDICATORS DESTINATION-INDICATORS)
  (PROG (T1 T2 INST)
;       (PRINT (LIST INSTRUCTION-CONTEXT
;                    COMBINED-VALUE
;                    COMBINED-INDICATORS
;                    DESTINATION-INDICATORS))
        (COND ((NOT (EQ LOCALITY 'I-MEM))
               (GO X))
              ((MEMQ INSTRUCTION-CONTEXT '(FORCE-ALU FORCE-ALU-OR-BYTE INSTRUCTION))
               (GO ALU))
              ((EQ INSTRUCTION-CONTEXT 'FORCE-DISPATCH)
               (GO DISPATCH))
              ((EQ INSTRUCTION-CONTEXT 'FORCE-BYTE)
               (GO BYTE))
              ((EQ INSTRUCTION-CONTEXT 'FORCE-JUMP)
               (GO JUMP))
              (T (LAM-LAP-BARF (LIST INSTRUCTION-CONTEXT
                                      COMBINED-VALUE COMBINED-INDICATORS
                                      DESTINATION-INDICATORS)
                                'BAD-INSTRUCTION-TYPE
                                'WARN)
                 (GO X)))
    ALU (COND ((NULL (MEMQ 'ALU-OUTPUT-BUS-SELECTOR-MULTIPLIER  ;DEFAULT OUTPUT BUS
                           COMBINED-INDICATORS))                ;SELECTOR IF NOT SPECD
               (SETQ COMBINED-VALUE (PLUS COMBINED-VALUE
                                          (LAM-LAP-SYM-RUN 'OUTPUT-SELECTOR-NORMAL)))))
        (COND ((MEMQ 'ALU-OP COMBINED-INDICATORS)
               (GO ALU-1)))
        (SETQ T1 (MEMQ 'A-MEM COMBINED-INDICATORS))             ;DEFAULT ALU OP IF NOT
        (SETQ T2 (MEMQL '(M-MEM FUNCTION-SOURCE) COMBINED-INDICATORS))  ;SPECD
        (COND ((AND T1 T2)                      ;(ALU MUST BE ACTING AS A SELECTOR)
               (LAM-LAP-BARF COMBINED-INDICATORS
                              'ALU-INST-ADRS-A-AND-M-WITHOUT-ALU-OP
                              'WARN))
              (T1 (SETQ COMBINED-VALUE
                        (PLUS COMBINED-VALUE (DPB COM-ALU-SETA COM-IR-ALUF 0))))        ;SETA
              (T2 (SETQ COMBINED-VALUE
                        (PLUS COMBINED-VALUE (DPB COM-ALU-SETM COM-IR-ALUF 0))))        ;SETM
              (T  (SETQ COMBINED-VALUE
                        (PLUS COMBINED-VALUE 0_3))))    ;NEITHER SPECD? SETZ I GUESS
     ALU-1
        (cond ((eq *target-processor-type* :explorer)
               (setq combined-value (dpb rav-jump-cond-unc rav-ir-jump-cond combined-value))))
        (GO X)
     BYTE
       (COND ((NULL (MEMQ 'A-MEM COMBINED-INDICATORS))  ;DEFAULT A-MEM ADR TO
              (SETQ COMBINED-VALUE                      ;A-ZERO IF NOT SUPPLIED,
                    (PLUS COMBINED-VALUE
                          (DPB 2 COM-IR-A-SRC 0))))) ;THIS RIGHT FOR BOTH LDB AND DPB
       (SETQ INST (DPB COM-OP-BYTE COM-IR-OP
                       (if (eq *target-processor-type* :explorer)
                           (dpb rav-jump-cond-unc rav-ir-jump-cond 0)
                         0)))                   ;BYTE INST
       (SETQ T1 (LDB COM-IR-BYTE-SR COMBINED-VALUE))    ;GET SR-BIT
       (SETQ COMBINED-VALUE (DPB (- 1 T1) ;STORE IT BACK COMPLEMENTED
                                    COM-IR-BYTE-SR COMBINED-VALUE))
       (COND ((> (LDB COM-IR-BYTE-FUNC COMBINED-VALUE) 1)
              (GO X1))) ;DONT BUGGER DPB OR SEL DEPOS
     M-ROTATE-BUGGER                            ;32. REFLECT M-ROTATE FIELD
       (SETQ T1 (LDB COM-IR-M-ROTATE COMBINED-VALUE))     ;M-ROTATE
       (SETQ T1 (LOGAND 37 T1))
       (SETQ T2 (LOGAND 37 (- 40 T1)))
       (SETQ COMBINED-VALUE (DPB T2 COM-IR-M-ROTATE COMBINED-VALUE))
     X1 (SETQ COMBINED-VALUE (PLUS COMBINED-VALUE INST))
     X  (RETURN COMBINED-VALUE)
     DISPATCH
       (SETQ INST (DPB COM-OP-DISPATCH COM-IR-OP 0))  ;DISPATCH INSTRUCTION
       (GO M-ROTATE-BUGGER)
     JUMP
       (SETQ INST (DPB COM-OP-JUMP COM-IR-OP 0))
       (selectq *target-processor-type*
         (:lambda
          (if (ZEROP (LDB COM-IR-JUMP-TEST-CONDITION COMBINED-VALUE))
              (go m-rotate-bugger)))
         (:explorer
          (if (not (zerop (ldb com-ir-jump-test-condition combined-value)))
              (go m-rotate-bugger))))
       (GO x1)))

;CONSTANT LISTS.
;A LIST OF LISTS.  CAR IS VALUE OF CONSTANT, CADR IS ADDRESS, CADDR IS #USERS, CADDDR IS
;       LAST PC TO USE IT.

(DEFUN LAM-M-CONSTANT (C)
  (PROG (TEM V)
        (SETQ V (LAM-LAP-ARG-EVAL C))
        (COND ((= V 0)
               (SETQ TEM 2))    ;M LOCN 2 ALWAYS HAS 0
              ((OR (= V 37777777777) (= V -1))
               (SETQ TEM 3))    ;M LOCN 3 ALWAYS HAS -1 (TO 32 BITS)
              ((SETQ TEM (ASSOC V M-CONSTANT-LIST))
                (RPLACA (CDDR TEM) (1+ (CADDR TEM)))
                (RPLACA (CDDDR TEM) LAM-LAP-LAST-SYM)
                (SETQ TEM (CADR TEM)))
              (T
                (SETQ TEM M-CONSTANT-LOC M-CONSTANT-LOC (1+ M-CONSTANT-LOC))
                (SETQ M-CONSTANT-LIST (CONS (LIST V TEM 1 LAM-LAP-LAST-SYM) M-CONSTANT-LIST))))
        (OR (< TEM 100)         ;*CADR
            (LAM-LAP-BARF (LIST TEM C) 'M-CONST-ADDR-OOB 'BARF))
        (ADD-FIELD-INDICATORS 'M-MEM)
        (RETURN (DPB TEM COM-IR-M-SRC 0)) ))

(DEFUN LAM-A-CONSTANT (C)
  (PROG (TEM V)
        (SETQ V (LAM-LAP-ARG-EVAL C))
        (COND ((= V 0)
               (SETQ TEM 2))    ;A LOCN 2 ALWAYS HAS 0
              ((OR (= V 37777777777) (= V -1))
               (SETQ TEM 3))    ;A LOCN 3 ALWAYS HAS -1 (TO 32 BITS)
              ((SETQ TEM (ASSOC V A-CONSTANT-LIST))
                (RPLACA (CDDR TEM) (1+ (CADDR TEM)))
                (RPLACA (CDDDR TEM) LAM-LAP-LAST-SYM)
                (SETQ TEM (CADR TEM)))
              ((SETQ TEM (ASSOC V M-CONSTANT-LIST))     ;A=M!!
                (RPLACA (CDDR TEM) (1+ (CADDR TEM)))
                (RPLACA (CDDDR TEM) LAM-LAP-LAST-SYM)
                (SETQ TEM (CADR TEM)))
              ((NOT (NULL A-MEM-CREVICE-LIST))  ;TRY TO FILL IN CREVICES IN MEMORY
                (SETQ TEM (CAR A-MEM-CREVICE-LIST))
                (SETQ A-MEM-CREVICE-LIST (CDR A-MEM-CREVICE-LIST))
                (SETQ A-CONSTANT-LIST (CONS (LIST V TEM 1 LAM-LAP-LAST-SYM) A-CONSTANT-LIST)))
              (T
                (SETQ TEM A-CONSTANT-LOC A-CONSTANT-LOC (1+ A-CONSTANT-LOC))
                (SETQ A-CONSTANT-LIST (CONS (LIST V TEM 1 LAM-LAP-LAST-SYM)
                                            A-CONSTANT-LIST))))
        (OR (< TEM 2000) (LAM-LAP-BARF (LIST TEM C) 'A-CONST-ADDR-OOB 'BARF))
        (ADD-FIELD-INDICATORS 'A-MEM)
        (RETURN (DPB TEM COM-IR-A-SRC 0)) ))

(DEFUN CONVERT-VALUE-TO-DESTINATION (VALUE INDICATORS)
  (PROG (V)
        (SETQ V (+ (LDB COM-BYTE-SPEC VALUE)    ;GOBBLE BYTE INFO, IF ANY (HOPE HOPE)
                   (LOGAND *DIRECT-BITS* VALUE)))       ;crock LAM-IR-SLOW-DEST, etc
        (COND ((MEMQL '(A-MEM D-MEM) INDICATORS)
               (COND ((MEMQL '(M-MEM FUNCTION-DESTINATION) INDICATORS)
                      (LAM-LAP-BARF (LIST VALUE INDICATORS) 'BAD-DESTINATION 'DATA)))
               (SETQ V (+ V (DPB (LDB COM-IR-A-SRC VALUE) COM-IR-A-MEM-DEST 0))))
              ((MEMQ 'M-MEM INDICATORS)
               (SETQ V (+ V (DPB (LDB COM-IR-M-SRC VALUE) COM-IR-M-MEM-DEST 0)))))
        (COND ((MEMQ 'FUNCTION-DESTINATION INDICATORS)
               (SETQ V (+ V (LOGAND (DPB -1 COM-IR-FUNC-DEST 0) VALUE)))))
        (COND ((MEMQL '(A-MEM D-MEM) INDICATORS)
               (SETQ V (+ V (DPB 1 COM-IR-A-MEM-DEST-FLAG 0)))))
        (RETURN V)
))

(DEFUN MERGE-INDICATORS (A B) (LAMLP-MERGE A B))

(DEFUN LAMLP-MERGE (A B)
  (PROG NIL
        (COND ((NULL B) (RETURN A)))
  L     (COND ((NULL A) (RETURN B))
              ((NOT (MEMQ (CAR A) B))
                (SETQ B (CONS (CAR A) B))))
        (SETQ A (CDR A))
        (GO L)))

(DEFUN LAM-DESTINATION (X)
  (PROG (DESTINATION-CONTEXT V)
        (SETQ V 0)
        (SETQ DESTINATION-CONTEXT 'DESTINATION)
        (COND ((NULL (CDR X))   ;SAVE A PLUS IN COMMON CASE..
                (RETURN (LAM-LAP-SYM-RUN (CAR X)))))
   L    (COND ((NULL X) (RETURN V)))
        (SETQ V (PLUS V (LAM-LAP-SYM-RUN (CAR X))))
        (SETQ X (CDR X))
        (GO L)
))

(DEFUN LAM-DESTINATION-PASS1 (X)  ;RETURN A GOOD ENUF APPROXIMATION TO THE VALUE ON PASS1
        ;FOR PURPOSES OF SEEING IF UINST IS LINKED.  THIS MEANS FUNCTIONAL-DESTINATION FIELD
        ;MUST BE RIGHT, OTHER STUFF DOESNT MATTER.
  (PROG (DESTINATION-CONTEXT V)
        (SETQ V 0)
        (SETQ DESTINATION-CONTEXT 'DESTINATION)
        (COND ((NULL (CDR X))   ;SAVE A PLUS IN COMMON CASE..
               (RETURN (COND ((AND (SYMBOLP (CAR X))
                                   (GETL (CAR X) *SYMBOL-PROPERTY-FLAGS*))
                              (LAM-LAP-SYM-RUN (CAR X)))
                             (T 0)))))
   L    (COND ((NULL X) (RETURN V)))
        (SETQ V (PLUS V
                      (COND ((AND (SYMBOLP (CAR X))
                                  (GETL (CAR X) *SYMBOL-PROPERTY-FLAGS*))
                             (LAM-LAP-SYM-RUN (CAR X)))
                            (T 0))))
        (SETQ X (CDR X))
        (GO L)
))

(DEFUN LAM-LAP-SYM-RUN (SYM)
  (PROG (TEM)
        (COND ((NULL (SETQ TEM (LAM-LAP-SYMEVAL SYM)))
                (LAM-LAP-BARF SYM 'UNDEFINED-SYM 'WARN)
                (RETURN 0))
              (T (RETURN (LAM-LAP-EVAL TEM))))))

(DEFUN LAM-LAP-ARG-EVAL (ARG)
  (PROG (COMBINED-VALUE COMBINED-INDICATORS DESTINATION-CONTEXT
         INSTRUCTION-CONTEXT FIELD-INDICATORS)
        (SETQ INSTRUCTION-CONTEXT 'INSTRUCTION)
        (RETURN (LAM-LAP-EVAL ARG))))

(DEFUN LAM-LAP-EVAL (EXP)      ;EXP A SYMBOL "PROGRAM".
                                ;RETURNS EITHER A NUMBERIC VALUE OR NIL, AND
                                ;MAY HAVE THE SIDE EFFECT OF MODIFING
                                ;INSTRUCTION-CONTEXT AND/OR FIELD-INDICATORS
  (PROG (VAL V V1 V2 TEM)
    L   (COND ((NULL EXP) (GO X))
              ((NUMBERP EXP)
                (SETQ V EXP)
                (GO C-V))
              ((ATOM EXP)
                (SETQ V (LAM-LAP-SYM-RUN EXP))
                (GO C-V))
              ((MEMQ (CAR EXP) '(A-MEM M-MEM I-MEM D-MEM))
                (GO L2))
              ((EQ (CAR EXP) 'SOURCE-P) (GO S-P))
              ((EQ (CAR EXP) 'DESTINATION-P) (GO D-P))
              ((MEMQ (CAR EXP) '(FORCE-DISPATCH FORCE-JUMP FORCE-ALU FORCE-BYTE
                        FORCE-DISPATCH-OR-BYTE FORCE-ALU-OR-BYTE))
                (LAM-GET-NEW-CONTEXT (CAR EXP))
                (GO L2))
              ((SETQ TEM (ASSQ (CAR EXP) '( (DISPATCH-INSTRUCTION-P . FORCE-DISPATCH)
                (BYTE-INSTRUCTION-P . FORCE-BYTE) (JUMP-INSTRUCTION-P . FORCE-JUMP)
                (ALU-INSTRUCTION-P . FORCE-ALU))))
                (GO I-P))
              ((EQ (CAR EXP) 'NOT)
                (GO N1))
              ((EQ (CAR EXP) 'OR)
                (GO OR-1))
              ((SETQ V (ASSQ (CAR EXP)
                             '((I-MEM-LOC . I-MEM) (D-MEM-LOC . D-MEM)
                               (A-MEM-LOC . A-MEM) (M-MEM-LOC . M-MEM))))
                (SETQ TEM (LAM-LAP-SYMEVAL (CADR EXP)))
                (OR (EQ (CAR TEM) (CDR V))
                    (LAM-LAP-BARF EXP 'LOSES 'DATA))
                (SETQ V (CADDR (CADR TEM)))
                (GO C-V))
              ((EQ (CAR EXP) 'FIELD)
                (SETQ TEM (LAM-LAP-SYM-RUN (CADR EXP)))
                (SETQ V (TIMES (LAM-LAP-EVAL (CADDR EXP)) TEM))
                (COND ((SETQ TEM (GET (CADR EXP) (CADDR *SYMBOL-PROPERTY-FLAGS*)))
                        (SETQ V (PLUS V TEM))))
                (ADD-FIELD-INDICATORS (CADR EXP))
                (GO C-V))
              ((EQ (CAR EXP) 'PLUS)
                (SETQ V (LAM-LAP-EVAL (CADR EXP)))
                (DO L (CDDR EXP) (CDR L) (NULL L)
                  (SETQ V (PLUS V (LAM-LAP-EVAL (CAR L)))))
                (GO C-V))
              ((EQ (CAR EXP) 'DIFFERENCE)
                (SETQ V (DIFFERENCE (LAM-LAP-EVAL (CADR EXP))
                                    (LAM-LAP-EVAL (CADDR EXP))))
                (GO C-V))
              ((eq (car exp) 'logxor)
               (setq v (logxor (lam-lap-eval (cadr exp))
                               (lam-lap-eval (caddr exp))))
               (go c-v))
              ((eq (car exp) 'logand)
               (setq v (logand (lam-lap-eval (cadr exp))
                               (lam-lap-eval (caddr exp))))
               (go c-v))
              ((eq (car exp) 'logior)
               (setq v (logior (lam-lap-eval (cadr exp))
                               (lam-lap-eval (caddr exp))))
               (go c-v))
              ((eq (car exp) 'ceiling)
               (setq v (ceiling (lam-lap-eval (cadr exp))
                               (lam-lap-eval (caddr exp))))
               (go c-v))
              ((eq (car exp) 'floor)
               (setq v (floor (lam-lap-eval (cadr exp))
                               (lam-lap-eval (caddr exp))))
               (go c-v))
              ((memq (CAR EXP) '(byte BYTE-FIELD))
                (COND ((MEMQ INSTRUCTION-CONTEXT '(INSTRUCTION FORCE-DISPATCH-OR-BYTE
                                                        FORCE-ALU-OR-BYTE))
                        (LAM-GET-NEW-CONTEXT 'FORCE-BYTE)))
                (SETQ V1 (LAM-LAP-EVAL (CADR EXP)) V2 (LAM-LAP-EVAL (CADDR EXP)))
                (COND ((EQ INSTRUCTION-CONTEXT 'FORCE-BYTE)
                       (AND (> V1 32.) (LAM-LAP-BARF (CADR EXP)
                                                      'BYTE-SIZE-GREATER-THAN-32
                                                      'DATA))
                       (selectq *target-processor-type*
                         (:lambda
                          (AND (ZEROP V1) (SETQ V1 1))  ;BYTE SIZE 0, DOING OA HACKERY, USE 1-1
                           (SETQ V (+ (DPB (1- V1) COM-IR-BYTE-LENGTH-SPEC 0)
                                      V2))) ;1- BYTE SIZE, MROT NOT BUGGERED YET
                         (:explorer
                          (setq v (+ (dpb v1 com-ir-byte-length-spec 0)
                                     (ldb com-ir-m-rotate v2)))))) ;make sure V2 not
                                                ;bigger than field
                      ((EQ INSTRUCTION-CONTEXT 'FORCE-DISPATCH)
                        (AND (> V1 7) (LAM-LAP-BARF (CADR EXP)
                                                     'DISPATCH-BYTE-SIZE-GREATER-THAN-7
                                                     'DATA))
                        (SETQ V (+ (DPB V1 COM-IR-DISP-BYTL 0) V2)))
                      ((EQ INSTRUCTION-CONTEXT 'FORCE-JUMP)
                        (COND ((NOT (= 1 V1))
                                (LAM-LAP-BARF (CADR EXP)
                                                'CAN-ONLY-TEST-ONE-BIT-FIELD-WITH-JUMP
                                                 'DATA)))
                        (SETQ V V2))
                      (T (LAM-LAP-BARF INSTRUCTION-CONTEXT
                                        'BYTE-FIELD-IN-BAD-CONTEXT
                                        'DATA)))
                (GO C-V))
              ((EQ (CAR EXP) 'LISP-BYTE)
                (SETQ V (LAM-LAP-EVAL (CONVERT-LISP-BYTE (CADR EXP))))
                (GO C-V))
              ((EQ (CAR EXP) 'ALL-BUT-LISP-BYTE)
                (SETQ V (LAM-LAP-EVAL (CONVERT-ALL-BUT-LISP-BYTE (CADR EXP))))
                (GO C-V))
              ((EQ (CAR EXP) 'BYTE-MASK)
                (SETQ V (LAM-LAP-GET-BYTE-VALUE (CADR EXP) -1))
                (GO C-V))
              ((EQ (CAR EXP) 'BYTE-VALUE)
                (SETQ V (LAM-LAP-GET-BYTE-VALUE (CADR EXP) (CADDR EXP)))
                (GO C-V))
              ((EQ (CAR EXP) 'EVAL)
                (SETQ V (si:EVAL-special-ok (CADR EXP)))
                (GO C-V))
              ((EQ (CAR EXP) 'I-ARG)
                (SETQ V (DPB (LAM-LAP-EVAL (CADR EXP))
                                COM-IR-DISP-DISPATCH-CONSTANT
                                0))
                (GO C-V))
              ((EQ (CAR EXP) 'OA-HIGH-CONTEXT)
                (SETQ V (LOGAND 37777777777 (ASH (LAM-WORD-EVAL (CADR EXP)) -32.)))
                                        ;ALL ABOVE 32. BITS
                (GO C-V))
              ((EQ (CAR EXP) 'OA-LOW-CONTEXT)
                ;  (SETQ V (LDB 0040 (LAM-WORD-EVAL (CADR EXP)))) ;LOW 32. BITS
                   (SETQ V (LET ((TEM-V (LAM-WORD-EVAL (CADR EXP))))  ;RESULT OF LDB CANT BE
                             (DPB (LDB 2020 TEM-V) 2020 (LDB 0020 TEM-V)))) ;BIGNUM FOR NOW.
                (GO C-V))
              ((AND (EQ (CAR EXP) 'MC-LINKAGE)
                    (SYMBOLP (CADR EXP)))
               (SETQ V (LAM-LAP-EVAL (LAM-LAP-MC-LINKAGE (CADR EXP))))
               (GO C-V))
              ((EQ (CAR EXP) 'MC-LINKAGE-VALUE)
               (SETQ V (LAM-LAP-EVAL (LAM-LAP-MC-LINKAGE-VALUE (CADR EXP) (CADDR EXP))))
               (GO C-V))
              ((AND LAM-LAP-INIT-STATE          ;incremental assembly
                    (EQ (CAR EXP) 'MC-ENTRY-ADR))
               (COND ((NOT (= (%DATA-TYPE
                                (SETQ TEM (CAR (FUNCTION-CELL-LOCATION (CADR EXP)))))
                              DTP-U-ENTRY))
                (FERROR NIL "mc-entry-adr not DTP-U-ENTRY")))
               (SETQ V (LAM-LAP-EVAL
                         `(I-MEM (FIELD JUMP-ADDRESS-MULTIPLIER
                                        ,(AR-1 (FUNCTION SYS:MICRO-CODE-SYMBOL-AREA)
                                               (AR-1 (FUNCTION SYS:MICRO-CODE-ENTRY-AREA)
                                                     (%POINTER TEM)))))))
               (GO C-V))
              ((AND LAM-LAP-INIT-STATE          ;incremental assembly
                    (EQ (CAR EXP) 'MISC-ENTRY-ADR))
               (SETQ V (LAM-LAP-EVAL
                         `(I-MEM (FIELD JUMP-ADDRESS-MULTIPLIER
                                        ,(AR-1 (FUNCTION SYS:MICRO-CODE-SYMBOL-AREA)
                                               (- (GET (CADR EXP) 'QLVAL) 200))))))
               (GO C-V))
              (T (LAM-LAP-BARF EXP 'UNRECGONIZED-OP 'DATA)
                 (setq val 0)
                 (GO X)))
OR-2    (COND ((NULL (CDR (SETQ EXP (CDR EXP))))
                (GO X)))                                ;ALL NIL
OR-1    (SETQ TEM (LAM-LAP-EVAL (CADR EXP)))
        (COND ((NULL TEM) (GO OR-2)))   ;THAT ONE EVALUATED TO NIL
MERGE-V (COND ((NULL VAL) (SETQ VAL TEM))
              (T (SETQ VAL (PLUS VAL TEM))))
        (GO X)
N1      (SETQ TEM (LAM-LAP-EVAL (LIST (CAADR EXP) 1)))
        (COND ((= TEM 1) (GO X))   ;THAT CONDITION TRUE, THIS FALSE
              (T (SETQ EXP (CADR EXP))  ;THAT CONDITION FALSE, THIS TRUE
                 (GO L1)))
D-P     (COND (DESTINATION-CONTEXT (GO L1)))
        (GO X)
S-P     (COND (DESTINATION-CONTEXT (GO X)))
        (GO L1)

L2      (ADD-FIELD-INDICATORS (CAR EXP))
L1      (SETQ EXP (CADR EXP))
        (GO L)
I-P     (COND ((EQ (CDR TEM) INSTRUCTION-CONTEXT)
                (GO L1))                ;CONDITION TRUE
              ((EQ INSTRUCTION-CONTEXT 'INSTRUCTION)
                (LAM-LAP-BARF EXP 'UNDETERMINED-CONDITION 'WARN)))
        (GO X)          ;CONDITION FALSE
C-V     (COND ((NULL VAL) (SETQ VAL 0)))
        (COND ((NULL V)
               (LAM-LAP-BARF EXP 'EVALUATED-TO-NIL 'DATA))
              (T (SETQ VAL (PLUS VAL V))))
X       (RETURN VAL) ))

(DEFUN CONVERT-LISP-BYTE (X)  ;CONVERT LISP-BYTE TO CORRESPONDING BYTE-FIELD
  (PROG (TEM)
        (SETQ TEM (si:EVAL-special-ok X))
        (RETURN (LIST 'BYTE-FIELD (LOGAND TEM 77)
                                  (LDB 0606 TEM)
))))

(DEFUN CONVERT-ALL-BUT-LISP-BYTE (X)    ;ADDRESS ALL BITS NOT IN BYTE. BYTE MUST BE
  (PROG (TEM BITS OVER)                 ;LEFT OR RIGHT ADJUSTED IN 32. BITS
        (SETQ TEM (si:EVAL-special-ok X))
        (SETQ BITS (LOGAND TEM 77) OVER (LDB 0606 TEM))
        (COND ((= 0 OVER)
                (SETQ OVER BITS)
                (SETQ BITS (- 32. BITS)))
              ((= 32. (+ BITS OVER))
                (SETQ BITS (- 32. BITS))
                (SETQ OVER 0))
              (T (LAM-LAP-BARF X 'ALL-BUT-BYTE-NOT-LEFT-OR-RIGHT-ADJUSTED 'DATA)))
        (RETURN (LIST 'BYTE-FIELD BITS OVER))))

(DEFUN LAM-LAP-GET-BYTE-VALUE (EXP VAL &AUX TEM) ;"EVALUATE" EXP SIMILIAR TO LAM-LAP-EVAL
                                ;BUT RETURN NIL FOR ANYTHING BUT BYTE-FIELD,
  (COND ((NUMBERP VAL))         ;FOR WHICH RETURN VAL IN FIELD OF BYTE
        ((NOT (ATOM VAL))
         (SETQ VAL (LAM-LAP-ARG-EVAL VAL)))
        ((SETQ TEM (LAM-LAP-SYMEVAL VAL))
         (SETQ VAL TEM))
        ((SETQ VAL (LAM-LAP-LISP-SYMEVAL VAL))))
  (LET ((BS (LAM-LAP-GET-BYTE-SPEC EXP)))
    (COND (BS (DPB-BIG VAL BS 0)))))

(DEFUN LAM-LAP-GET-BYTE-SPEC (EXP &AUX TEM)
  (COND ((NULL EXP) NIL)
        ((NUMBERP EXP)
         EXP)
        ((ATOM EXP)
         (LAM-LAP-GET-BYTE-SPEC
           (OR (LAM-LAP-SYMEVAL EXP) (LAM-LAP-LISP-SYMEVAL EXP))))
        ((MEMQ (CAR EXP) '(M-MEM FORCE-DISPATCH FORCE-BYTE FORCE-DISPATCH-OR-BYTE
                           FORCE-ALU-OR-BYTE))
         (LAM-LAP-GET-BYTE-SPEC (CADR EXP)))
        ((MEMQ (CAR EXP) '(A-MEM I-MEM D-MEM SOURCE-P DESTINATION-P FORCE-JUMP
                           FORCE-ALU NOT OR FIELD EVAL))
         NIL)
        ((EQ (CAR EXP) 'PLUS)
         (DO L (CDR EXP) (CDR L) (NULL L)
             (AND (SETQ TEM (LAM-LAP-GET-BYTE-SPEC (CAR L)))
                  (RETURN TEM))))
        ((EQ (CAR EXP) 'LISP-BYTE)
         (LAM-LAP-GET-BYTE-SPEC (CADR EXP)))
        ((memq (CAR EXP) '(byte BYTE-FIELD))
         (+ (LSH (CADDR EXP) 6) (CADR EXP)))    ;Turns into lisp type byte spec.
        (T (LAM-LAP-BARF EXP 'LAM-LAP-GET-BYTE-SPEC 'WARN))))

(DEFUN ADD-FIELD-INDICATORS (X)
  (PROG NIL
        (COND ((AND DESTINATION-CONTEXT   ;BETTER NOT PUT IN MORE THAN ONE OF THESE
                    (MEMQ X '(A-MEM M-MEM I-MEM D-MEM))  ;SINCE GOING TO DIVIDE IT OUT.
                    (MEMQL '(A-MEM M-MEM I-MEM D-MEM) FIELD-INDICATORS))
                (GO E1)))
        (COND ((EQ X 'A-MEM)
                (GO X))
              ((EQ X 'M-MEM)
                (GO X))
              ((EQ X 'I-MEM)
                (GO ADD-I))
              ((EQ X 'D-MEM)
                (GO ADD-D))
              ((EQ X 'FORCE-DISPATCH)
                (GO F-D))
              ((EQ X 'FORCE-BYTE)
                (GO F-B))
              ((EQ X 'FORCE-ALU)
                (GO F-A))
              ((EQ X 'FORCE-JUMP)
                (GO F-J)))
   X    (COND ((NOT (MEMQ X FIELD-INDICATORS))
                (SETQ FIELD-INDICATORS (CONS X FIELD-INDICATORS))))
        (RETURN NIL)
 F-B    (COND ((MEMQL '(I-MEM D-MEM) COMBINED-INDICATORS)
                (GO E1)))
        (GO X)
 F-A    (COND ((OR (MEMQ INSTRUCTION-CONTEXT '(FORCE-DISPATCH FORCE-JUMP))
                   (MEMQL '(I-MEM D-MEM) COMBINED-INDICATORS))
                (GO E1)))
        (GO X)
 F-J
 ADD-I  (COND ((MEMQ INSTRUCTION-CONTEXT '(FORCE-DISPATCH FORCE-BYTE FORCE-ALU))
                (GO E1)))
        (GO X)
 F-D
 ADD-D  (COND ((OR (MEMQ INSTRUCTION-CONTEXT '(FORCE-JUMP FORCE-BYTE FORCE-ALU))
                   (MEMQL '(I-MEM) COMBINED-INDICATORS))  ;A-MEM OK NOW IF WRITING DRAM
                (GO E1)))
        (GO X)
  E1    (LAM-LAP-BARF (LIST X FIELD-INDICATORS COMBINED-INDICATORS)
              'INDICATOR-CONFLICT
              'DATA)
        (RETURN NIL)
))

(DEFUN MEMQL (A B)
  (PROG NIL
L       (COND ((NULL A) (RETURN NIL))
              ((MEMQ (CAR A) B) (RETURN A)))
        (SETQ A (CDR A))
        (GO L)))

(DEFUN LAM-GET-NEW-CONTEXT (NEW-CONTEXT)
  (PROG NIL
        (COND ((ATOM NEW-CONTEXT)
                (RETURN (LAM-GET-NEW-CONTEXT-1 NEW-CONTEXT))))
L       (COND ((NULL NEW-CONTEXT) (RETURN T))
              (T (LAM-GET-NEW-CONTEXT-1 (CAR NEW-CONTEXT))))
        (SETQ NEW-CONTEXT (CDR NEW-CONTEXT))
        (GO L)))

(DEFUN LAM-GET-NEW-CONTEXT-1 (NEW)
  (PROG NIL
        (COND ((OR (EQ INSTRUCTION-CONTEXT NEW)
                   (NOT (MEMQ NEW '(FORCE-DISPATCH FORCE-JUMP FORCE-ALU FORCE-BYTE
                        FORCE-DISPATCH-OR-BYTE FORCE-ALU-OR-BYTE))))
                (RETURN NIL))
              ((EQ INSTRUCTION-CONTEXT 'INSTRUCTION)
                (GO N1))
              ((AND (EQ INSTRUCTION-CONTEXT 'FORCE-BYTE)
                    (MEMQ NEW '(FORCE-DISPATCH-OR-BYTE FORCE-ALU-OR-BYTE)))
                (RETURN NIL))
              ((AND (EQ INSTRUCTION-CONTEXT 'FORCE-ALU)
                    (EQ NEW 'FORCE-ALU-OR-BYTE))
                (RETURN NIL))
              ((AND (EQ NEW 'FORCE-BYTE)
                    (MEMQ INSTRUCTION-CONTEXT
                          '(FORCE-DISPATCH-OR-BYTE FORCE-ALU-OR-BYTE)))
                (GO N1))
              ((AND (EQ NEW 'FORCE-ALU)
                    (EQ INSTRUCTION-CONTEXT 'FORCE-ALU-OR-BYTE))
                (GO N1))
              ((OR (AND (EQ INSTRUCTION-CONTEXT 'FORCE-DISPATCH-OR-BYTE)
                        (EQ NEW 'FORCE-ALU-OR-BYTE))
                   (AND (EQ NEW 'FORCE-ALU-OR-BYTE)
                        (EQ INSTRUCTION-CONTEXT 'FORCE-DISPATCH-OR-BYTE)))
                (SETQ NEW 'FORCE-BYTE)
                (GO N1)))
        (LAM-LAP-BARF (LIST INSTRUCTION-CONTEXT NEW) 'CONFLICTING-CONTEXT 'DATA)
        (RETURN NIL)
  N1    (SETQ INSTRUCTION-CONTEXT NEW)
        (RETURN T)
))


(DEFUN LIST-ASSQ (ITEM IN-LIST)
  (PROG NIL
    L   (COND ((NULL IN-LIST) (RETURN NIL))
              ((EQ ITEM (CAR IN-LIST))
                (RETURN (CADR IN-LIST))))
        (SETQ IN-LIST (CDDR IN-LIST))
        (GO L)))


(DEFUN ALLREMPROP (INDICATOR)
 (MAPATOMS (FUNCTION (LAMBDA (X) (REMPROP X INDICATOR)))))

;This called on buffered stuff from lambda:ASSEMBLE just before assembly actually done.
;ASSEMBLER-STATE environment has been established.

(comment
(DEFUN UA-DO-DEFMIC (NAME OPCODE ARGLIST LISP-FUNCTION-P NO-QINTCMP
                  &AUX FUNCTION-NAME INSTRUCTION-NAME MICRO-CODE-ENTRY-INDEX NARGS)
  (COND ((ATOM NAME)
         (SETQ FUNCTION-NAME NAME INSTRUCTION-NAME NAME))
        ((SETQ FUNCTION-NAME (CAR NAME) INSTRUCTION-NAME (CDR NAME))))
  (COND ((NULL OPCODE)
         (SETQ OPCODE (COND ((GET INSTRUCTION-NAME 'QLVAL))
                            (T (UA-ASSIGN-MICRO-ENTRY NAME))))))
  (PUTPROP INSTRUCTION-NAME OPCODE 'QLVAL)
  (SETQ NARGS (SI:ARGS-INFO-FROM-LAMBDA-LIST ARGLIST))
  (COND ((OR (BIT-TEST NARGS %ARG-DESC-QUOTED-REST)
             (BIT-TEST NARGS %ARG-DESC-EVALED-REST)
             (BIT-TEST NARGS %ARG-DESC-INTERPRETED)
             (BIT-TEST NARGS %ARG-DESC-FEF-QUOTE-HAIR)
             (AND (NOT NO-QINTCMP)
                  (NOT (= (LDB %%ARG-DESC-MAX-ARGS NARGS)
                          (LDB %%ARG-DESC-MIN-ARGS NARGS)))))
         (FERROR NIL "~%The arglist of the function ~s, ~s, is too hairy to microcompile.
ARGS-INFO = ~O~%"
                 NAME ARGLIST NARGS)))
  (COND (LISP-FUNCTION-P
         (SETQ MICRO-CODE-ENTRY-INDEX (ALLOCATE-MICRO-CODE-ENTRY-SLOT FUNCTION-NAME))
         (STORE (SYSTEM:MICRO-CODE-ENTRY-ARGLIST-AREA MICRO-CODE-ENTRY-INDEX) ARGLIST)
         (STORE (SYSTEM:MICRO-CODE-ENTRY-ARGS-INFO-AREA MICRO-CODE-ENTRY-INDEX) NARGS)
         ))
  (COND ((NOT NO-QINTCMP)
         (PUTPROP INSTRUCTION-NAME (LDB %%ARG-DESC-MAX-ARGS NARGS) 'QINTCMP)
         (OR (EQ FUNCTION-NAME INSTRUCTION-NAME)
             (PUTPROP FUNCTION-NAME (LDB %%ARG-DESC-MAX-ARGS NARGS) 'QINTCMP))))
)
)

;(DEFVAR PAGE-SIZE UA:PAGE-SIZE)
;(DEFVAR COLD-LOAD-AREA-SIZES UA:COLD-LOAD-AREA-SIZES)

(DECLARE (SPECIAL *OUT-FILE*))

(DECLARE (SPECIAL
           AREA-LIST COLD-LOAD-AREA-SIZES PAGE-SIZE CONSLP-INPUT CONSLP-OUTPUT))


(DEFUN DUMP-MEM-ARRAY (ARRAYP MEM OUT-FILE)
  (PROG (IDX LIM TEM)
        (SETQ IDX 0)
        (SETQ LIM (CADR (ARRAYDIMS ARRAYP)))
  L     (COND ((NOT (< IDX LIM))
                (RETURN T))
              ((SETQ TEM (ARRAYCALL T ARRAYP IDX))
               (PRIN1-THEN-SPACE MEM OUT-FILE)
               (PRIN1-THEN-SPACE IDX OUT-FILE)
               (PRIN-16 TEM OUT-FILE)
               (TERPRI OUT-FILE)))
        (SETQ IDX (1+ IDX))
        (GO L)))

(DEFUN DUMP-MEM-ARRAY-WITH-PARITY-64 (ARRAYP MEM OUT-FILE)
  (PROG (IDX LIM TEM)
        (SETQ IDX 0)
        (SETQ LIM (CADR (ARRAYDIMS ARRAYP)))
  L     (COND ((NOT (< IDX LIM))
                (RETURN T))
              ((SETQ TEM (ARRAYCALL T ARRAYP IDX))
               (SETQ TEM (COMPUTE-PARITY-64 TEM))
               (PRIN1-THEN-SPACE MEM OUT-FILE)
               (PRIN1-THEN-SPACE IDX OUT-FILE)
               (PRIN-16 TEM OUT-FILE)
               (TERPRI OUT-FILE)))
        (SETQ IDX (1+ IDX))
        (GO L)))

(DEFUN LAM-DUMP-ARRAY (ARRAYP OUT-FILE)
  (PROG (IDX LIM)
        (SETQ IDX 0)
        (SETQ LIM (CADR (ARRAYDIMS ARRAYP)))
  L     (COND ((NOT (< IDX LIM))
                (TERPRI OUT-FILE)
                (RETURN T)))
        (PRINT (ARRAYCALL T ARRAYP IDX) OUT-FILE)
        (SETQ IDX (1+ IDX))
        (GO L)))

(DEFUN PRIN-16 (NUM OUT-FILE)
       (COND ((MINUSP NUM) (SETQ NUM (PLUS NUM 40000000000))))
                ;TURN IT INTO A 32 BIT POS NUMBER
       (PRIN1-THEN-SPACE NUM OUT-FILE))

(DEFUN LAM-DUMP-MEMORIES (&OPTIONAL (DUMP-MACRO-INSTRUCTION-DECODE NIL))
  (PROG (OUT-FILE)
        (IF (NOT (EQ *TARGET-PROCESSOR-TYPE* ':LAMBDA))
            (FERROR NIL "THIS ONLY WORKS FOR LAMBDA"))
        (PKG-BIND "LAMBDA" ;REDUCE INCIDENCE OF :'S IN OUTPUT, CAUSE MAPATOMS TO WIN MORE.
          (SETQ OUT-FILE (OPEN (format nil "SYS:UBIN;~A.LAM-ULOAD" (string conslp-output))
                               'OUT))
          (DUMP-MEM-ARRAY-WITH-PARITY-64 I-MEM 'I OUT-FILE)
          (IF DUMP-MACRO-INSTRUCTION-DECODE
              (DUMP-MEM-ARRAY MACRO-INSTRUCTION-DECODE 'M OUT-FILE))
          (DUMP-MEM-ARRAY D-MEM 'D OUT-FILE)
          (DUMP-MEM-ARRAY A-MEM 'A OUT-FILE)
          (TERPRI OUT-FILE)
          (COND ((NOT (NULL (AREF MICRO-CODE-SYMBOL-IMAGE 0)))  ;IF HAVE WIPED SYMBOL VECTOR
                 (PRINT -3 OUT-FILE)            ;DUMP MICRO-CODE-SYMBOL AREA
                 (PRINT (LAM-DUMP-FIND-AREA-ORIGIN 'MICRO-CODE-SYMBOL-AREA) OUT-FILE)
                 (LAM-DUMP-ARRAY MICRO-CODE-SYMBOL-IMAGE
                                  OUT-FILE)))
          (PRINT -2 OUT-FILE)           ;NOW DUMP SYMBOLS
          (TERPRI OUT-FILE)
          (LAM-DUMP-SYMBOLS OUT-FILE)
          (PRINT -1 OUT-FILE)           ;EOF
          (CLOSE OUT-FILE)
          (RETURN T))))

(defun lam-disassemble-uload-file (&optional file-name &aux (eof-marker (ncons nil)))
  (cond ((null file-name)
         (setq file-name (format nil "SYS:UBIN;~A.LAM-ULOAD" (string conslp-output)))))
  (pkg-bind 'lambda
    (with-open-file (f file-name)
      (do ((sym (read f eof-marker) (read f eof-marker)))
          ((eq eof-marker sym))
        (cond ((eq sym 'I)
               (let ((addr (read f))
                     (instr (read f)))
                 (format t "~&~6o: " addr)
                 (lam-print-uinst instr)))
              (t
               (return nil)))))))

(DEFUN LAM-DUMP-FIND-AREA-ORIGIN (AREA)
  (PROG (ADR LST TEM)
        (SETQ ADR 0)
        (SETQ LST INITIAL-AREA-LIST)
   L    (COND ((NULL LST)(BREAK "CANT-FIND-AREA-ORIGIN"))
              ((EQ (CAR LST) AREA) (RETURN ADR))
              (T (OR (SETQ TEM (LIST-ASSQ (CAR LST) COLD-LOAD-AREA-SIZES))
                     (SETQ TEM 1))))
        (SETQ ADR (+ ADR (* TEM PAGE-SIZE)))
        (SETQ LST (CDR LST))
        (GO L)))

(DEFUN LAM-DUMP-SYMBOLS (*OUT-FILE*)
  (setq lam-file-symbols-loaded-from nil)
  (lam-initialize-symbol-table t lam-initial-syms)
  (MAPATOMS #'LAM-LAP-DUMP-SYMTAB-ELEMENT)
  (lam-end-adding-symbols)
  (let ((truename (send *out-file* :truename)))
    (setq lam-file-symbols-loaded-from truename)
    (LAM-RECORD-SYMBOL-TABLE truename)))

(DEFUN LAM-LAP-DUMP-SYMTAB-ELEMENT (SYM)
  (let ((val (get sym (cadr *symbol-property-flags*))))
    (loop while (and val (symbolp val))
          do (setq val (lam-lap-symeval val)))
    (cond ((null val))
          ((numberp val)
           (prin1-then-space sym *out-file*)
           (prin1-then-space 'number *out-file*)
           (prin1-then-space val *out-file*)
           (terpri *out-file*))
          ((and (listp val)
                (listp (cdr val))
                (listp (cadr val))
                (eq (car (cadr val)) 'field))
           (let (dump-type)
             (case (car val)
               (i-mem
                (when (eq (cadr (cadr val)) 'jump-address-multiplier)
                  (setq dump-type 'i-mem)))
               (d-mem
                (when (eq (cadr (cadr val)) 'dispatch-address-multiplier)
                  (setq dump-type 'd-mem)))
               (a-mem
                (when (eq (cadr (cadr val)) 'a-source-multiplier)
                  (setq dump-type 'a-mem)))
               (m-mem
                (when (eq (cadr (cadr val)) 'm-source-multiplier)
                  (setq dump-type 'm-mem))))
             (when dump-type
               (lam-add-typed-symbol sym dump-type (caddr (cadr val)))
               (prin1-then-space sym *out-file*)
               (prin1-then-space dump-type *out-file*)
               (prin1-then-space (caddr (cadr val)) *out-file*)
               (terpri *out-file*)))))))

;(DEFUN LAM-LAP-DUMP-SYMTAB-ELEMENT (SYM)
;  (PROG (VAL DMP-TYPE TEM)
;       (SETQ VAL (GET SYM (CADR *SYMBOL-PROPERTY-FLAGS*)))
;    L  (COND ((NULL VAL) (RETURN NIL))
;             ((NUMBERP VAL)
;               (SETQ DMP-TYPE 'NUMBER))
;             ((ATOM VAL)
;               (SETQ VAL (LAM-LAP-SYMEVAL VAL))
;               (GO L))
;             ((AND (SETQ TEM (ASSQ (CAR VAL)
;                       '( (I-MEM JUMP-ADDRESS-MULTIPLIER)
;                           (D-MEM DISPATCH-ADDRESS-MULTIPLIER)
;                           (A-MEM A-SOURCE-MULTIPLIER)
;                           (M-MEM M-SOURCE-MULTIPLIER))))
;                   (EQ (CAADR VAL) 'FIELD)
;                   (EQ (CADADR VAL) (CADR TEM)))
;              (SETQ DMP-TYPE (CAR VAL) VAL (CADDR (CADR VAL))))
;            (T (RETURN NIL)))
;       (PRIN1-THEN-SPACE SYM *OUT-FILE*)
;        (PRIN1-THEN-SPACE DMP-TYPE *OUT-FILE*)
;       (PRIN1-THEN-SPACE VAL *OUT-FILE*)
;       (TERPRI *OUT-FILE*)
;       (RETURN T)))

(DEFUN FILL-IN-MACRO-INSTRUCTION-DECODE ()
  (fill macro-instruction-decode
        (if (boundp 'micro-code-symbol-table-fill-value)
            micro-code-symbol-table-fill-value
            0))
  (match-dispatch-specs)
  ;fill in the high part used for misc instructions.  This is a copy of micro code symbol area.
  (do ((rel-adr 0 (1+ rel-adr)))
      ((= rel-adr 2000))
    (let ((misc-entry (find-macro-instruction-misc-decode-entry rel-adr)))
      (when misc-entry
        (setf (aref macro-instruction-decode (+ 6000 rel-adr)) misc-entry)))))

(defun find-macro-instruction-misc-decode-entry (adr)
  (let ((misc-number (ldb 0012 adr)))    ;9 bits plus misc-misc1 bit.
    (cond ((find-misc-on-misc-dispatch-alist misc-number))
          ((< misc-number 200) nil)
          (t (aref micro-code-symbol-image (- misc-number 200))))))

(defun find-misc-on-misc-dispatch-alist (misc-number)
  (dolist (e *macro-ir-misc-dispatch-alist*)
    (cond ((and (numberp (cdr e))
                (= misc-number (cdr e)))
           (return (car e)))
          ((symbolp (cdr e))
           (cond ((eq misc-number (get (cdr e) ':compiler:qlval))
                  (return (car e)))))
          ((and (listp (cdr e))
                (numberp (car (cdr e)))
                (numberp (cadr (cdr e)))
                (>= misc-number (car (cdr e)))
                (<= misc-number (cadr (cdr e))))
           (return (car e))))))

(DEFUN FIND-MACRO-INSTRUCTION-DECODE-ENTRY (ADR)
  (LET ((ANS))
    (DOLIST (E *MACRO-IR-DISPATCH-ALIST*)
      (WHEN (MATCH-DISPATCH-SPEC *MACRO-IR-DISPATCH-SPEC* (LSH ADR 6) (CDR E))
        (IF (NULL ANS)
            (SETQ ANS E)
          (FORMAT T "~% dual match for mir adr ~s, ~s and ~s" ADR ANS E))))
    (CAR ANS)))

; The macro-ir-dispatch-spec creates a mapping between 16 bit macro-instructions
; and a unique pattern describing the instruction
; The macro-ir-decode specifies a pattern or patterns which should have entry points
; at that location in the microcode.

;SPEC is what was specified by the MACRO-IR-DISPATCH-SPEC pseudo-op, or a sublist of that.
;ADR is proposed address in the main section of MACRO-IR-DISPATCH memory.
;PATTERN was argument to to MACRO-IR-DECODE pseudo-op.
; if PATTERN, interpreted according to SPEC, matches ADR then return NON-NIL.

; in more detail:  PATTERN is a list, most significant field first.
;                       sub-lists, if any, specify alternative matching values.
;                       numbers, stand for themselves, either at the top level or in sublists.
;                       the distinguished symbol * always matches.
;                  SPEC is 3-list (<name> <byte-field> <option-list>)
;                       <name> is for memonic value only
;                       <byte-field> is a symbol (w/ value) or number for LDB
;                       <option-list> is a paired list
;                          (<list of consecutive values> <sub-spec> ... )
;                   idea is the "next" (length <list-of-consec-values>) values for
;                   <byte-field> are described.  If byte-field has a value higher than that,
;                   then discard this pair and try again with the next, etc.
(DEFUN MATCH-DISPATCH-SPEC (SPEC ADR PATTERN)
  (IF (NULL PATTERN) T
    (LET ((SELECTED-SUB-SPEC (MATCH-DISPATCH-SPEC-TERM SPEC ADR (CAR PATTERN))))
      (COND ((NULL SELECTED-SUB-SPEC) NIL)
            ((ATOM SELECTED-SUB-SPEC) SELECTED-SUB-SPEC)
            (T (MATCH-DISPATCH-SPEC SELECTED-SUB-SPEC ADR (CDR PATTERN)))))))

(DEFUN MATCH-DISPATCH-SPEC-TERM (SPEC ADR TERM)
  (LET* (;(FIELD-NAME (CAR SPEC))
         (BYTE-FIELD (LAM-LAP-GET-BYTE-SPEC (CADR SPEC)))
         (OPTION-LIST (CADDR SPEC))
         (FIELD-VALUE (LDB BYTE-FIELD ADR)))
   (COND ((NUMBERP TERM)
          (= TERM FIELD-VALUE))
         ((AND (LISTP TERM)             ;this makes numbers "stand for themselves"
               (LIST-OF-NUMBERS-P TERM))
          (MEMBER FIELD-VALUE TERM))
         ((EQ TERM '*)          ;this matches.  figure out what sub-desc goes with actual
          (PROG (V TEM)         ; value and return it.
                (SETQ TEM OPTION-LIST V FIELD-VALUE)
             L  (COND ((NULL TEM) (RETURN T))
                      ((>= V (LENGTH (CAR TEM)))
                       (SETQ V (- V (LENGTH (CAR TEM))))
                       (SETQ TEM (CDDR TEM))
                       (GO L))
                      ((NULL (CADR TEM)) (RETURN T))  ;no further sublists, it matched.
                      (T (RETURN (CADR TEM))))))
         (T
          (PROG (TEM COUNT MATCH-SPEC)
                (SETQ TEM OPTION-LIST COUNT 0)
             L  (COND ((NULL TEM)
                       (FERROR NIL "unable to evaluate pattern element ~s vs spec ~s"
                               TERM SPEC))
                      ((SETQ MATCH-SPEC (NTH (- FIELD-VALUE COUNT) (CAR TEM)))
                       (COND ((NULL (MATCH-DISPATCH-SPEC-ITEM TERM MATCH-SPEC))
                              (RETURN NIL))
                             ((NULL (CADR TEM)) (RETURN T))
                             (T (RETURN (CADR TEM))))))
                (SETQ COUNT (+ COUNT (LENGTH (CAR TEM)))
                      TEM (CDDR TEM))
                (GO L))))))


(DEFUN MATCH-DISPATCH-SPEC-ITEM (TERM MATCH-SPEC)
  (COND ((SYMBOLP TERM)
         (EQ TERM MATCH-SPEC))
        (T (MEMBER MATCH-SPEC TERM))))

(DEFUN LIST-OF-NUMBERS-P (LIST)
  (DO ((P LIST (CDR P)))
      ((NULL P) (RETURN T))
    (COND ((NOT (NUMBERP (CAR P)))
           (RETURN NIL)))))

(defun parse-macro-ir-dispatch-spec (macro-ir-dispatch-spec)
  "Turn macro-ir-dispatch-spec into something the pattern matcher can easily digest."
  (if (null macro-ir-dispatch-spec)
      nil
      (labels
        ((parse-all-options-and-subspecs (byte-field options-spec-list)
           (let ((answer '())
                 (starting-point 0))
             (if options-spec-list
                 (do ((os-sublist options-spec-list (rest (rest os-sublist))))
                     ((null os-sublist) answer)
                   (multiple-value-bind
                     (next-starting-point some-sublists)
                       (assign-consecutive-byte-fields byte-field starting-point (first os-sublist))
                     (let ((subparse (parse-macro-ir-dispatch-spec (second os-sublist))))
                       (setq answer (append (merge-parse-with-subparse subparse some-sublists) answer))
                       (setq starting-point next-starting-point))))
                 (multiple-value-bind
                   (ignore answer)
                     (assign-consecutive-byte-fields-numerically byte-field starting-point)
                   answer))))

         (assign-consecutive-byte-fields-numerically (byte-field starting-value)
           (let ((ending-value (expt 2 (byte-size byte-field))))
             (do ((v starting-value (1+ v))
                  (answer '() (cons (list (ash (dpb v byte-field 0) -6) (list (list v byte-field))) answer)))
                 ((= v ending-value) (values v answer)))))

         (assign-consecutive-byte-fields (byte-field starting-value option-list)
           (do ((option-lists option-list (rest option-lists))
                (value starting-value (1+ value))
                (answer '() (cons (list (ash (dpb value byte-field 0) -6) ;shifts value to correct slot.
                                        (list (list (first option-lists) byte-field))) answer)))
               ((null option-lists) (values value answer))))

         (merge-parse-with-subparse (subparse parse)
           (if subparse
               (let ((answer '()))
                 (dolist (s subparse)
                   (dolist (p parse)
                     (let ((byte-pattern (logior (first s) (first p)))
                           (field (append (second p) (second s))))
                       (setq answer (cons (list byte-pattern field) answer)))))
                 answer)
               parse))
         )
        (parse-all-options-and-subspecs
          (lam-lap-get-byte-spec (second macro-ir-dispatch-spec))
          (third macro-ir-dispatch-spec)))))

(defconstant *wildcard* '*)

;;; Next three accomplish the same thing.
;;; This turns out to be a big time sink when recompiling the microcode.
;;; I came up with these three different mechanisms in order to speed
;;; things up.  ~jrm

;;; The fastest way.
;;; Reparses the pattern each match, but does it rather quickly.
(defun compile-pattern (pattern value)
  #'(lambda (name address)
      (block match
;       (format t "~%Testing ~S" pattern)
        (do ((pattern-tail pattern (cdr pattern-tail))
             (pattern-to-match  name (cdr pattern-to-match)))
            ((null pattern-tail) value)
          (let ((sub-pattern (first pattern-tail))
                (sub-match   (first pattern-to-match)))
            (let ((sub-match-name (first sub-match))
                  (sub-match-byte (second sub-match)))
;             (format t "~%  Subpattern ~S" sub-pattern)
              (or
                (cond ((symbolp sub-pattern)
                       (or (eq sub-match-name sub-pattern)
                           (eq sub-pattern *wildcard*)))
                      ((numberp sub-pattern)
                       (or (and (numberp sub-match-name)
                                (= sub-pattern sub-match-name))
                           (= sub-pattern (ldb sub-match-byte address))))
                      ((consp sub-pattern)
                       (if (list-of-numbers-p sub-pattern)
                           (memq (ldb sub-match-byte address) sub-pattern)
                           (memq sub-match-name sub-pattern))))
                (return-from match nil))))))))

;;; The "right" way.  Loses on speed because of lexical references, but only
;;; parses the pattern once.
;(defun compile-pattern (pattern value)
;  (labels ((generate-pattern-matcher (sub-pattern)
;            (cond ((symbolp sub-pattern)
;                   #'(lambda (element ignore ignore) (eq element sub-pattern)))
;                  ((numberp sub-pattern)
;                   #'(lambda (element ignore ignore)
;                       (and (numberp element) (= element sub-pattern))))
;                  ((eq sub-pattern *wildcard*) #'(lambda (ignore ignore) t))
;                  ((consp sub-pattern)
;                   (if (list-of-numbers-p sub-pattern)
;                       #'(lambda (ignore byte address)
;                           (memq (ldb byte address) sub-pattern))
;                       #'(lambda (element ignore ignore)
;                           (memq element sub-pattern)))))))
;    (let ((matchers-list
;           (mapcar #'generate-pattern-matcher pattern)))
;      #'(lambda (name address)
;         (block match
;           (do ((p name (cdr p))
;                (m matchers-list (cdr m)))
;               ((null p) value)
;             (unless (funcall (first m) (first p) (second p) address)
;               (return-from match nil))))))))


;;; Really fast matcher, but too much time is lost generating it.
;;; A cool hack:  Generate a fast pattern matcher and compile it.
;;; The resulting code is blazingly fast, but it takes so much time
;;; to generate it that we don't get a win.  ~jrm
;(defun compile-pattern (pattern value)
;  (let ((name 'pattern)
;       (adr  'address))
;    (labels
;      ((generate-pattern-match-code (sub-pattern element byte)
;        (cond ((numberp sub-pattern) `(and (numberp ,element) (= ,sub-pattern ,element)))
;              ((eq sub-pattern *wildcard*) 't)
;              ((symbolp sub-pattern) `(eq ',sub-pattern ,element))
;              ((consp sub-pattern)
;               (if (list-of-numbers-p sub-pattern)
;                   `(memq (ldb ,byte ,adr) ',sub-pattern)
;                   `(memq ,element ',sub-pattern)))))

;       (generate-element-selector (n)
;        `(car (,(elt '(car cadr caddr cadddr) n) ,name)))

;       (generate-byte-field-selector (n)
;        `(cadr (,(elt '(car cadr caddr cadddr) n) ,name)))
;       )

;      (do ((pattern-tail pattern (cdr pattern-tail))
;          (index 0 (1+ index))
;          (code '(and)
;                (cons (generate-pattern-match-code
;                        (car pattern-tail)
;                        (generate-element-selector index)
;                        (generate-byte-field-selector index)) code)))
;         ((null pattern-tail)
;            `(lambda (,name ,adr)
;               ,adr
;               ,(reverse (cons value code))))))))

(defun match-dispatch-specs ()
  (let ((matchers (mapcar
                    #'(lambda (pattern) (compile-pattern (rest pattern) (first pattern)))
                    *macro-ir-dispatch-alist*)))
    (dolist (pattern-to-match (parse-macro-ir-dispatch-spec *macro-ir-dispatch-spec*))
      (let ((match '())
            (pattern (second pattern-to-match))
            (address (first pattern-to-match)))
        (dolist (matcher matchers)
          (let ((result (funcall matcher pattern (ash address 6.))))
            (when result
              (when match (ferror nil "Dual matches for macro instruction pattern ~S" pattern-to-match))
              (setq match result))))
        (when match
          (setf (aref macro-instruction-decode address) match))))))


;Write out the output in LMC form.

;An LMC file looks a lot like a microcode partition.
; Its a binary file consisting of 32 bit words.
; From the Lisp machine, we write this as 2 16-bit pieces
;Differences between a MCR file and a LMC file:
;  halfwords in standard order.  (this lossage was carried over from PDP-10 days).

(DECLARE (SPECIAL CONSLP-OUTPUT-SYMBOL-PREDICTED-FILEPOS
                  CONSLP-OUTPUT-CURRENT-FILEPOS
                  CONSLP-OUTPUT VERSION-NUMBER
                  CONSLP-OUTPUT-PATHNAME))

(DECLARE (SPECIAL ASSEMBLER-SAVED-STATE))

(DEFUN OUT16 (FILE BYTE)
  (SETQ CONSLP-OUTPUT-CURRENT-FILEPOS (1+ CONSLP-OUTPUT-CURRENT-FILEPOS))
  (FUNCALL FILE ':TYO BYTE))

(DEFUN OUT32 (FILE WORD)
  (selectq *target-processor-type*
    (:lambda
     ;Note standard order of 16-bit bytes now.
     (OUT16 FILE (LDB 0020 WORD))
     (OUT16 FILE (LDB 2020 WORD)))
    (:explorer
     (OUT16 FILE (LDB 2020 WORD))
     (OUT16 FILE (LDB 0020 WORD)))))

;obsolete entry function
(DEFUN WRITE-LMC (BASE-VERSION-NUMBER)
  (WRITE-LMC-FILE (FUNCALL CONSLP-OUTPUT-PATHNAME ':NEW-TYPE-AND-VERSION
                                   "LMC" VERSION-NUMBER)
                  BASE-VERSION-NUMBER))

(defun rewrite-lmc (file)
  (let* ((p (fs:parse-pathname file))
         (v (send p :version)))
    (write-lmc-file p (if (numberp v) v 1))))

(DEFUN WRITE-LMC-FILE (PATHNAME &optional (BASE-VERSION-NUMBER 1))

  (PKG-BIND 'LAMBDA                     ;Try to reduces :s in symtab, etc.
    (WITH-OPEN-FILE (FILE PATHNAME
                          '(:OUT :FIXNUM))
      (LET ((CONSLP-OUTPUT-CURRENT-FILEPOS 0))
        (selectq *TARGET-PROCESSOR-TYPE*
          (:lambda
           (COND (BASE-VERSION-NUMBER
                  (OUT32 FILE 3)                ;a fake main memory block
                  (OUT32 FILE 0)                ; blocks to xfer
                  (OUT32 FILE 0)                ; normally relative disk block,
                                                ; 0 says base version follows
                  (OUT32 FILE BASE-VERSION-NUMBER))))
          (:explorer
           (out32 file 3) ;fake main memory block
           (out32 file 0)
           (out32 file 0)
           (out32 file version-number)
           (out32 file 3)                       ;starting address
           ))
        (WRITE-OUT-I-MEM I-MEM 1 FILE)
        ;; For explorer write out D-MEM which includes macro instruction decode
        ;; For lambda write out macro instruction decode, D-MEM is done with A-MEM
        (COND ((EQ *TARGET-PROCESSOR-TYPE* ':EXPLORER)
               (WRITE-OUT-D-MEM D-MEM MACRO-INSTRUCTION-DECODE 2 FILE))
              ((EQ *TARGET-PROCESSOR-TYPE* ':LAMBDA)
               (WRITE-OUT-MACRO-INSTRUCTION-DECODE MACRO-INSTRUCTION-DECODE 5 FILE)))

        (make-error-table-string)

        (WRITE-MICRO-CODE-SYMBOL-AREA-PART-1 FILE)
        (WRITE-OUT-A-MEM A-MEM 4 FILE)
        (write-error-table-string-to-lmc-file 6 file)
        (WRITE-MICRO-CODE-SYMBOL-AREA-PART-2 FILE)))
    (WRITE-SYMBOL-TABLE-FILE (FUNCALL PATHNAME ':NEW-TYPE
                                      (STRING-APPEND (SELECTQ *TARGET-PROCESSOR-TYPE*
                                                       (:LAMBDA "LMC")
                                                       (:EXPLORER "EMC"))
                                                     "-SYM")))))

;;; WRITE-OUT-D-MEM is for Explorer only - it combines the D-MEM array with part of the
;;; macro instruction decode array.
(DEFUN WRITE-OUT-D-MEM (ARRAY INST-DECODE-ARRAY CODE FILE)
    (OUT32 FILE CODE)           ;Code for this kind of section.
    (OUT32 FILE 0)              ;Start address.
    (LET* ((ARRAY-SIZE 4000)
           (SIZE (+ ARRAY-SIZE 4000)))          ;4000 is how much d-mem is left for
                                                ;macro instruction decode
      (OUT32 FILE SIZE)
      ;; regular dispatch table entries
      (DO I 0 (1+ I) (= I ARRAY-SIZE)
        (LET ((VAL (OR (AREF ARRAY I) 0)))
          (out32 file (ldb (byte 17. 0) val))
          ))
      ;; misc group 1
      (Do ((i 7000 (1+ i)))
          ((= i 10000))
        (Let ((val (or (aref inst-decode-array i) 0)))
          (out32 file val)))
      ;; misc group 0
      (Do ((i 6000 (1+ i)))
          ((= i 7000))
        (Let ((val (or (aref inst-decode-array i) 0)))
          (out32 file val)))
      ;; macro instructions (except miscops)
      (do ((i 0 (1+ i)))
          ((= i 2000))
        (Let ((val (or (aref inst-decode-array i) 0)))
          (out32 file val)))
      ))


(DEFUN WRITE-OUT-MACRO-INSTRUCTION-DECODE (ARRAY CODE FILE)
    (OUT32 FILE CODE)           ;Code for this kind of section.
    (OUT32 FILE 0)              ;Start address.
    (LET ((SIZE (ARRAY-LENGTH ARRAY)))
      (OUT32 FILE SIZE)
      (DO I 0 (1+ I) (= I SIZE)
        (OUT32 FILE (OR (AREF ARRAY I) 0)))))

;WRITE OUT RELAVENT PARTS OF BOTH A AND D MEMORIES, FOR LAMBDA.  FOR EXPLORER, WRITE
; REAL A-MEM ONLY
(DEFUN WRITE-OUT-A-MEM (A-ARRAY CODE FILE)
    (OUT32 FILE CODE)           ;Code for this kind of section.
    (OUT32 FILE 0)              ;Start address.
    (LET ((SIZE (SELECTQ *TARGET-PROCESSOR-TYPE*
                  (:LAMBDA 10000)       ;COMBINED A AND D MEMORY.
                  (:EXPLORER 2000))))   ;A ONLY
      (OUT32 FILE SIZE)
      (DO I 0 (1+ I) (= I SIZE)
        (OUT32 FILE (COND ((< I 2000)
                           (OR (AREF A-ARRAY I) 0))
                          (T (OR (AREF D-MEM I) 0)))))))     ;CROCK.

(defun write-error-table-string-to-lmc-file (code file)
  (out32 file code)
  (out32 file 0)                                ;start address
  (out32 file (ceiling (string-length error-table-string) 4))   ;size in 32's
  (dotimes (i (ceiling (string-length error-table-string) 4))
    (out32 file (+ (ash (aref error-table-string (+ (* i 4) 3)) 24.)
                   (ash (aref error-table-string (+ (* i 4) 2)) 16.)
                   (ash (aref error-table-string (+ (* i 4) 1)) 8)
                   (aref error-table-string (* i 4))))))

(defun compute-raven-cram-parity-for-lamlp (data)
  (setq data (dpb 0 rav-ir-parity data))
  (let ((parity 1))
    (dotimes (bit 56.)
      (setq parity (logxor parity (ldb (byte 1 bit) data))))
    (dpb parity rav-ir-parity data)))

(DEFUN WRITE-OUT-I-MEM (ARRAY CODE FILE)
    (OUT32 FILE CODE)           ;Code for this kind of section.
    (OUT32 FILE 0)              ;Start address.
    (LET ((SIZE (ARRAY-LENGTH ARRAY)) (TEM))
      (DO () ((NOT (NULL (AREF ARRAY (1- SIZE)))))
        (SETQ SIZE (1- SIZE)))
      (OUT32 FILE SIZE)
      (DO I 0 (1+ I) (= I SIZE)
        (SETQ TEM (OR (AREF ARRAY I) 0))
        (selectq *target-processor-type*
          (:lambda
           (setq tem (compute-parity-64 tem))
            (OUT16 FILE (LDB 0020 TEM))
            (OUT16 FILE (LDB 2020 TEM))
            (OUT16 FILE (LDB 4020 TEM))
            (OUT16 FILE (LDB 6020 TEM)))
          (:explorer
           (setq tem (compute-raven-cram-parity-for-lamlp tem))
            (OUT16 FILE (LDB 6020 TEM))
            (OUT16 FILE (LDB 4020 TEM))
            (OUT16 FILE (LDB 2020 TEM))
            (OUT16 FILE (LDB 0020 TEM))
           ))
        )))

(DEFUN WRITE-MICRO-CODE-SYMBOL-AREA-PART-1 (FILE)
  (OUT32 FILE 3)                ;Code for main mem section.
  (OUT32 FILE (TRUNCATE (ARRAY-LENGTH MICRO-CODE-SYMBOL-IMAGE) 400)) ;# of blocks
  (SETQ CONSLP-OUTPUT-SYMBOL-PREDICTED-FILEPOS
        (+ CONSLP-OUTPUT-CURRENT-FILEPOS
           4 ;rest of this block
           6 ;A/M header
           (selectq *target-processor-type*
             (:LAMBDA 20000) ;A/M data
             (:EXPLORER 4000))
           6 ;error table header
           (* 2 (ceiling (string-length error-table-string) 4))
           ))
  (OUT32 FILE (TRUNCATE (+ CONSLP-OUTPUT-SYMBOL-PREDICTED-FILEPOS 777) 1000)) ;Rel disk block #
  (OUT32 FILE (LAM-DUMP-FIND-AREA-ORIGIN 'MICRO-CODE-SYMBOL-AREA))) ;Phys mem address

;Call this after everything else, to put the micro code symbol area at the end
(DEFUN WRITE-MICRO-CODE-SYMBOL-AREA-PART-2 (FILE)
  (OR (= CONSLP-OUTPUT-CURRENT-FILEPOS CONSLP-OUTPUT-SYMBOL-PREDICTED-FILEPOS)
      (BREAK "LOSSAGE"))
  (DO N (\ CONSLP-OUTPUT-CURRENT-FILEPOS 1000)
        (1+ N) (OR (ZEROP N) (= N 1000))  ;Pad to page boundary
    (OUT16 FILE 0))
  (LET ((ARRAY MICRO-CODE-SYMBOL-IMAGE))
    (DO ((I 0 (1+ I))
         (N (ARRAY-LENGTH ARRAY))
         (FIXNUM-DATA-TYPE (DPB DTP-FIX %%Q-DATA-TYPE 0)))
        ((NOT (< I N)))
      (OUT32 FILE (+ FIXNUM-DATA-TYPE (COND ((AREF ARRAY I)) (T 0)))))))

;This writes an ascii file containing the symbol table
(DEFUN WRITE-SYMBOL-TABLE-FILE (PATHNAME &AUX (BASE 8) (IBASE 8))
  (WITH-OPEN-FILE (OUT-FILE PATHNAME
                            '(:OUT :BLOCK :ASCII))
    (PRINT -4 OUT-FILE) ;ASSEMBLER STATE INFO
    (PRINT (MAKE-ASSEMBLER-STATE-LIST) OUT-FILE)
    (PRINT -2 OUT-FILE)
    (LAM-DUMP-SYMBOLS OUT-FILE)
    (PRINT -1 OUT-FILE)         ;EOF
    ))

(DEFUN MAKE-CONSTANT-LIST (LST)   ;FLUSH USAGE COUNT, LAST LOCN REF'ED AT.
   (MAPCAR (FUNCTION (LAMBDA (X)
                         (LIST (CAR X) (CADR X))))
           LST))


;build-constant macro

; ones-loc, zero-loc, a-or-m-dest are symbols, value a number, returns a list of instructions
(defun (build-constant lam-lap-macro) (ones-loc zero-loc a-or-m-dest value &aux code)
  (cond ((not (char-equal #/m (aref (string ones-loc) 0)))
         (ferror nil "build-constant must be called with an m memory address as the first arg")))
  (cond ((not (char-equal #/a (aref (string zero-loc) 0)))
         (ferror nil "build-constant must be called with an a memory addresses as the 2nd arg")))
  (cond ((not (or (char-equal #/m (aref (string a-or-m-dest) 0))
                  (char-equal #/a (aref (string a-or-m-dest) 0))))
         (ferror nil "the third arg to build-constant should be an a or m memory loc")))
  (let ((real-value (lam-lap-arg-eval value))
        (a-dest (cond ((char-equal #/m (aref (string a-or-m-dest) 0))
                       (intern (let ((new-string (string-append a-or-m-dest)))
                                 (aset #/A new-string 0)
                                 new-string)"LAMBDA"))
                      (t
                       a-or-m-dest))))
    (cond ((zerop real-value)
           `(((,a-or-m-dest) setz)))
          (t
           (let ((bit-list (make-bit-list real-value)))
             (setq code
                   (dolist (x (cdr bit-list) code)
                     (setq code (cons `((,a-or-m-dest) dpb ,ones-loc (byte-field ,@x) ,a-dest)
                                      code))))
             (cons `((,a-or-m-dest) dpb ,ones-loc (byte-field ,@(car bit-list)) ,zero-loc)
                   code))))))

(defun make-bit-list (n)
  (do ((ppss 0001 (+ ppss 100))
       (bitnum 0 (1+ bitnum))
       (lst nil)
       (start-of-range nil)                     ;nil if not in range now
       )
      ((= bitnum 33.) lst)
    (cond ((or (= bitnum 32.)
               (zerop (ldb ppss n)))
           (cond ((null start-of-range))
                 (t
                  (setq lst (cons (list (- bitnum start-of-range)
                                        start-of-range)
                                  lst))))
           (setq start-of-range nil))
          (t
           (cond ((null start-of-range)
                  (setq start-of-range bitnum)))))))


(defmacro defun-lam-lap-macro (name arglist &body body)
  `(progn (defun ,name ,arglist ,@body)
          (putprop ',name (function ,name) 'lam-lap-macro)))

;  `(progn 'compile
;         (defun (:property ,name lam-lap-macro) ,arglist
;           (,name ,@arglist))
;         (defun ,name ,arglist
;           ,@body))

;this macro is intended to replace sequences like
;
;#+CADR ((M-K) Q-DATA-TYPE M-A)
;#+CADR (CALL-NOT-EQUAL M-K (A-CONSTANT (EVAL DTP-FEF-POINTER)) ILLOP)
;#+LAMBDA(CALL-DATA-TYPE-NOT-EQUAL M-A (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FEF-POINTER))
;               ILLOP)
;
;(check-data-type-equal m-a m-k dtp-fef-pointer illop)

(defvar cadr-jumps '(call-equal call-equal-xct-next call-not-equal call-not-equal-xct-next
                     jump-equal jump-equal-xct-next jump-not-equal jump-not-equal-xct-next))

(defvar lambda-jumps '(call-data-type-equal call-data-type-equal-xct-next
                       call-data-type-not-equal call-data-type-not-equal-xct-next
                       jump-data-type-equal jump-data-type-equal-xct-next
                       jump-data-type-not-equal jump-data-type-not-equal-xct-next))



(defun output-data-type-instructions (instruction-code register temp-register data-type jump-loc)
  "instruction-code
  0  call-equal
  1  call-not-equal
  2  jump-equal
  3  jump-not-equal
"
  (selectq *target-processor-type*
    (:CADR
     `(((,temp-register) q-data-type ,register)
       (,(nth instruction-code cadr-jumps)
        ,temp-register (a-constant (si:eval-special-ok ,data-type)) ,jump-loc)))
    (:LAMBDA
     `((,(nth instruction-code lambda-jumps)
        ,register (a-constant (byte-value q-data-type ,data-type)) ,jump-loc)))
    (:EXPLORER
     `((,(nth instruction-code lambda-jumps)
        ,register (a-constant (byte-value q-data-type ,data-type)) ,jump-loc)))))

(defun-lam-lap-macro check-data-type-call-equal (reg temp-reg dtp adr)
  (output-data-type-instructions 0 reg temp-reg dtp adr))

(defun-lam-lap-macro check-data-type-call-equal-xct-next (reg temp-reg dtp adr)
  (output-data-type-instructions 1 reg temp-reg dtp adr))

(defun-lam-lap-macro check-data-type-call-not-equal (reg temp-reg dtp adr)
  (output-data-type-instructions 2 reg temp-reg dtp adr))

(defun-lam-lap-macro check-data-type-call-not-equal-xct-next (reg temp-reg dtp adr)
  (output-data-type-instructions 3 reg temp-reg dtp adr))

(defun-lam-lap-macro check-data-type-jump-equal (reg temp-reg dtp adr)
  (output-data-type-instructions 4 reg temp-reg dtp adr))

(defun-lam-lap-macro check-data-type-jump-equal-xct-next (reg temp-reg dtp adr)
  (output-data-type-instructions 5 reg temp-reg dtp adr))

(defun-lam-lap-macro check-data-type-jump-not-equal (reg temp-reg dtp adr)
  (output-data-type-instructions 6 reg temp-reg dtp adr))

(defun-lam-lap-macro check-data-type-jump-not-equal-xct-next (reg temp-reg dtp adr)
  (output-data-type-instructions 7 reg temp-reg dtp adr))

;;; (open-qcar-xct-next foo)
;;; If in cadr, just (call-xct-next qcar)
;;; In lambda, do the dispatch immediately using FOO, and remember an ETE
;;; Note that you have to put the arg in M-T in the xct-next, if it is not
;;; already there
;;;
;;;  (open-qcar-xct-next m-a)
;;; ((m-t) m-a) ;or whatever      ==>
;;;
;;; #+CADR   (CALL-XCT-NEXT QCAR)
;;; #+LAMBDA (DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;;;                 (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE M-A CAR-PRE-DISPATCH-DIRECT)
;;; #+LAMBDA (error-table-after-next argtyp cons m-t t car car)
;;;         ((m-t) m-a) ;or whatever
;;;

(defun output-direct-dispatch (cadr-function xct-next-p dispatch-args error-table)
  (selectq *target-processor-type*
    (:CADR
     `((,(if xct-next-p 'call-xct-next 'call) ,cadr-function)))
    (:LAMBDA
     `((dispatch-xct-next ,@dispatch-args)
       ,@(if error-table (list error-table) nil)
       ,@(if (null xct-next-p) '((no-op)) nil)))
    (:EXPLORER
     `((,(if xct-next-p 'call-xct-next 'call) ,cadr-function)))))

(defun-lam-lap-macro open-qcar-xct-next (arg)
  (output-direct-dispatch
    'qcar t
    `(dispatch-write-vma (i-arg instance-invoke-car) q-data-type ,arg car-pre-dispatch-direct)
    '(error-table-after-next argtyp cons m-t t car car)))

(defun-lam-lap-macro open-qcar (arg)
  (output-direct-dispatch
    'qcar nil
    `(dispatch-write-vma (i-arg instance-invoke-car) q-data-type ,arg car-pre-dispatch-direct)
    '(error-table-after-next argtyp cons m-t t car car)))

(defun-lam-lap-macro open-qcdr-xct-next (arg)
  (output-direct-dispatch
    'qcdr t
    `(dispatch-write-vma (i-arg instance-invoke-cdr) q-data-type ,arg cdr-pre-dispatch-direct)
    '(error-table-after-next argtyp cons m-t t cdr cdr)))

(defun-lam-lap-macro open-qcdr (arg)
  (output-direct-dispatch
    'qcdr nil
    `(dispatch-write-vma (i-arg instance-invoke-cdr) q-data-type ,arg cdr-pre-dispatch-direct)
    '(error-table-after-next argtyp cons m-t t cdr cdr)))

(defun-lam-lap-macro open-qcdr-sb-xct-next (arg)
  (output-direct-dispatch
    'qcdr-sb t
    `(dispatch-write-vma (i-arg instance-invoke-cdr) q-data-type ,arg cdr-pre-dispatch-sb-direct)
    '(error-table-after-next argtyp cons m-t t cdr cdr)))

(defun-lam-lap-macro open-qcdr-sb (arg)
  (output-direct-dispatch
    'qcdr-sb nil
    `(dispatch-write-vma (i-arg instance-invoke-cdr) q-data-type ,arg cdr-pre-dispatch-sb-direct)
    '(error-table-after-next argtyp cons m-t t cdr cdr)))

(defun-lam-lap-macro open-carcdr-xct-next (arg)
  (output-direct-dispatch
    'carcdr t
    `(dispatch-write-vma q-data-type ,arg carcdr-dispatch-direct)
    nil))

(defun-lam-lap-macro open-carcdr (arg)
  (output-direct-dispatch
    'carcdr nil
    `(dispatch-write-vma q-data-type ,arg carcdr-dispatch-direct)
    nil))

(defun-lam-lap-macro call-return (subroutine return-to)
; This definition consumes A-memory locations, which are scarce.
; `((jump-xct-next ,subroutine)
;   ((micro-stack-data-push) (a-constant (i-mem-loc ,return-to)))))
  `((jump-xct-next ,return-to)
    (call ,subroutine)))

(defun-lam-lap-macro trap-unless-fixnum (m-location &key (argument :none) (restart nil))
  (cond ((and (eq argument :none) (eq restart nil))
         `((dispatch (i-arg data-type-invoke-op) q-data-type ,m-location trap-unless-fixnum)))
        (t
         (if (eq argument :none) (setq argument nil))
         `((dispatch (i-arg data-type-invoke-op) q-data-type ,m-location trap-unless-fixnum)
           (error-table argtyp fixnum ,m-location ,argument ,(or restart 'fall-through))))))

(defun-lam-lap-macro array-trap-unless-fixnum (m-location &key (argument :none) (restart nil))
  (cond ((and (eq argument :none) (eq restart nil))
         `((dispatch (i-arg data-type-invoke-op) q-data-type ,m-location array-trap-unless-fixnum)))
        (t
         (if (eq argument :none) (setq argument nil))
         `((dispatch (i-arg data-type-invoke-op) q-data-type ,m-location array-trap-unless-fixnum)
           (error-table argtyp fixnum ,m-location ,argument ,(or restart '(nil restore-array-registers)))))))

(defun-lam-lap-macro array-subscript-range-check ()
  `((call-greater-or-equal m-q a-array-length array-subscript-error)))


(defun-lam-lap-macro array-header-setup (arg)
  (output-direct-dispatch
    'gahdr nil
    `(dispatch-write-vma (i-arg data-type-invoke-op) q-data-type ,arg array-header-dispatch-setup)
    nil))

(defun-lam-lap-macro array-header-setup-xct-next (arg)
 (output-direct-dispatch
   'gahdr t
   `(dispatch-write-vma (i-arg data-type-invoke-op) q-data-type ,arg array-header-dispatch-setup)
   nil))

;;;make jump-if-not-list (using skip-if-not-list on cadr)
;;;trap-unless-sym


(defun load-symbols-from-assembler-state ()
  (setq lam-file-symbols-loaded-from nil)
  (lam-initialize-symbol-table t lam-initial-syms)
  (mapatoms #'(lambda (sym)
                (let ((val (get sym (cadr *symbol-property-flags*))))
                  (loop while (and val (symbolp val))
                           do (setq val (lam-lap-symeval val)))
                  (when (and (listp val)
                             (listp (cdr val))
                             (listp (cadr val))
                             (eq (car (cadr val)) 'field))
                    (case (car val)
                      (i-mem
                       (when (eq (cadr (cadr val)) 'jump-address-multiplier)
                         (lam-add-typed-symbol sym 'i-mem (caddr (cadr val)))))
                      (d-mem
                       (when (eq (cadr (cadr val)) 'dispatch-address-multiplier)
                         (lam-add-typed-symbol sym 'd-mem (caddr (cadr val)))))
                      (a-mem
                       (when (eq (cadr (cadr val)) 'a-source-multiplier)
                         (lam-add-typed-symbol sym 'a-mem (caddr (cadr val)))))
                      (m-mem
                       (when (eq (cadr (cadr val)) 'm-source-multiplier)
                         (lam-add-typed-symbol sym 'm-mem (caddr (cadr val))))))))))
  (lam-end-adding-symbols)
  (let ((filename (send (fs:parse-pathname (format nil "sys:ubin;ulambda.~a.~d"
                                                   (ecase *target-processor-type*
                                                     (:lambda "LMC")
                                                     (:explorer "EMC"))
                                                   version-number))
                        :truename)))
    (setq lam-file-symbols-loaded-from filename)
    (LAM-RECORD-SYMBOL-TABLE filename)
    )
  )
