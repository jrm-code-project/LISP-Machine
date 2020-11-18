;;; FEF Disassembler -*- Mode:LISP; Package:COMPILER; Readtable:CL; Base:8 -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This stuff is used by SYS: DEBUGGER; EH >.  If you change things around,
;;; make sure not to break that.

;;;     "We could always add another MISC instruction (another misconstruction)."
;;;                                                  -- Heard outside RG's area

;; Aesthetic note:
;;  do not change (princ 'foo) or (format t "~A" 'foo) into (format t "FOO")
;;  as that loses when *print-case* neq :upcase
(DEFVAR DISASSEMBLE-OBJECT-OUTPUT-FUN NIL)
(DEFUN DISASSEMBLE (FUNCTION &AUX FEF LIM-PC ILEN (DISASSEMBLE-OBJECT-OUTPUT-FUN NIL))
  "Print a disassembly of FUNCTION on *STANDARD-OUTPUT*.
FUNCTION can be a compiled function, an LAMBDA-expression (which will be compiled),
or a function spec (whose definition will be used)."
  (DO ((FUNCTION FUNCTION)) (())
    (COND ((TYPEP FUNCTION 'COMPILED-FUNCTION)
           (SETQ FEF FUNCTION)
           (RETURN))
          ((MEMQ (CAR-SAFE FUNCTION) '(LAMBDA NAMED-LAMBDA ZL:SUBST CL:SUBST NAMED-SUBST))
           (SETQ FEF (COMPILE-LAMBDA FUNCTION (GENSYM)))
           (FORMAT T "~%Definition of compilation of function")
           (RETURN))
          ((EQ (CAR-SAFE FUNCTION) 'MACRO)
           (FORMAT T "~%Definition as macro")
           (SETQ FUNCTION (CDR FUNCTION)))
          ((TYPEP FUNCTION 'CLOSURE)
           (FORMAT T "~%Definition of closed-over function")
           (SETQ FUNCTION (CLOSURE-FUNCTION FUNCTION)))
          (T
           ;>> Should mention if function is encapsulated.
           (SETQ FUNCTION (SI:DWIMIFY-PACKAGE FUNCTION))
           (SETQ FUNCTION (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION))))))
  ;>> Print arglist?
  (WHEN (GET-MACRO-ARG-DESC-POINTER FEF)
    (SI::DESCRIBE-FEF-ADL FEF)
    (TERPRI))
  (SETQ LIM-PC (DISASSEMBLE-LIM-PC FEF))
  (DO ((PC (FEF-INITIAL-PC FEF) (+ PC ILEN))) (( PC LIM-PC))
    (TERPRI)
    (SETQ ILEN (DISASSEMBLE-INSTRUCTION FEF PC)))
  (TERPRI)
  FUNCTION)

(DEFF DISASSEMBLE-INSTRUCTION-LENGTH 'FEF-INSTRUCTION-LENGTH)
(DEFF DISASSEMBLE-FETCH 'FEF-INSTRUCTION)
(DEFF DISASSEMBLE-LIM-PC 'FEF-LIMIT-PC)


(DEFUN DISASSEMBLE-LEXICAL-REFERENCE-INSTRUCTION-P (FEF PC)
  (IF ( PC (DISASSEMBLE-LIM-PC FEF))
      NIL
    (LET* ((WD (DISASSEMBLE-FETCH FEF PC))
           (OP (LDB (BYTE 4 #o11) WD))
           (SUBOP (LDB (BYTE 3 #o15) WD))
           (DISP (LDB (BYTE #o11 0) WD)))
      (cond (( OP #o15)
             NIL)
            ((BIT-TEST 1 SUBOP)
             (SETQ DISP (+ DISP #o1000)))
            ((< DISP #o200)
             NIL)
            (t (MEMQ (AREF (SYMBOL-FUNCTION 'MICRO-CODE-SYMBOL-NAME-AREA) (- DISP #o200))
                     '(%LOAD-FROM-HIGHER-CONTEXT %STORE-IN-HIGHER-CONTEXT
                       %LOCATE-IN-HIGHER-CONTEXT)))))))

(DEFUN DISASSEMBLE-INSTRUCTION (FEF PC &AUX ILEN)
  "Print on *STANDARD-OUTPUT* the disassembly of the instruction at PC in FEF.
Returns the length of that instruction."
  (SETQ ILEN (DISASSEMBLE-INSTRUCTION-LENGTH FEF PC))
  (BLOCK NIL
    (FORMAT T "~3D " PC)
    (LET* ((WD (DISASSEMBLE-FETCH FEF PC))
           (OP (LDB (BYTE 4 #o11) WD))
           (SUBOP (LDB (BYTE 3 #o15) WD))
           (DEST (LDB (BYTE 2 #o16) WD))
           (DISP (LDB (BYTE #o11 0) WD))
           (REG (LDB (BYTE 3 6) WD))
           SECOND-WORD)
      (WHEN (= ILEN 2)
        (INCF PC)
        (SETQ SECOND-WORD (DISASSEMBLE-FETCH FEF PC))
        ;; If a two-word insn has a source address, it must be an extended address,
        ;; so set up REG and DISP to be right for that.
        (UNLESS (= OP #o14)
          (SETQ REG (LDB (BYTE 3 6) SECOND-WORD)
                DISP (DPB (LDB (BYTE 4 #o11) SECOND-WORD)
                          (BYTE 4 6)
                          (LDB (BYTE 6 0) SECOND-WORD)))))
      (IF (< OP #o11) (SETQ OP (LDB (BYTE 5 #o11) WD)))
      (COND ((ZEROP WD)
             (PRINC 0))
            ((< OP #o11)                        ;DEST/ADDR
             (cond ((and (eq op 2) (eq dest 1)) ;move d-pdl
                    (let (n name)
                      (princ 'push)
                      (cond ((not (and (disassemble-lexical-reference-instruction-p fef
                                                                                    (+ pc ilen))
                                       (fixnump (setq n (disassemble-address-contents
                                                          fef reg disp second-word)))
                                       (setq name (disassemble-lexical-ref-name fef n))))
                             (disassemble-address fef reg disp second-word))
                            (t
                             (format t " (lexical ref ~D,~D)~30,4T;"
                                     (ldb (byte 12. 12.) n) (ldb (byte 12. 0) n))
                             (if (eq (car-safe name) 'function)
                                 (format t "#'~S" (cadr name)) (prin1 name))))))
                   (t
                    (FORMAT T "~A ~A" (NTH OP '(CALL CALL0 MOVE CAR CDR CADR CDDR CDAR CAAR))
                            (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST)))
                    (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))))
            ((= OP #o11)                        ;ND1
             (cond ((zerop subop)
                    (FORMAT T "~A local slot ~D" 'MAKE-LEXICAL-CLOSURE-TOP-LEVEL DISP))
                   (t
                    (PRINC (NTH SUBOP '(<ND1-BAD> + - * ZL-/ LOGAND LOGXOR LOGIOR)))
                    (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))))
            ((= OP #o12)                        ;ND2
             (PRINC (NTH SUBOP '(= > < EQ SETE-CDR SETE-CDDR INCF DECF
                                          #|SETE-CDR SETE-CDDR SETE-1+ SETE-1-|#)))
             (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
            ((= OP #o13)                        ;ND3
             (PRINC (NTH SUBOP '(<ND3-UNUSED> BIND-NIL BIND-POP
                                 SET-NIL SET-ZERO PUSH-ADDRESS MOVEM POP)))
             (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
            ((= OP #o14)                        ;BRANCH
             (PRINC (NTH SUBOP '(BR BR-NIL BR-NOT-NIL BR-NIL-POP
                                 BR-NOT-NIL-POP BR-ATOM BR-NOT-ATOM <BR-ILL-7>)))
             (IF (> DISP #o400) (SETQ DISP (LOGIOR #o-400 DISP)))       ;Sign-extend
             (cond ((NEQ DISP -1)
                    ;; One word
                    (FORMAT T " ~D" (+ PC DISP 1)))
                   (t
                    ;; Long branch
                    (SETQ DISP SECOND-WORD)
                    (IF ( DISP #o100000) (SETQ DISP (LOGIOR #o-100000 DISP)))
                    (FORMAT T " ~A ~D" 'LONG (+ PC DISP 1))
                    (RETURN nil))))
            ((= OP #o15)                        ;MISC
             (FORMAT T "(~A) " 'MISC)           ;Moon likes to see this
             (IF (BIT-TEST 1 SUBOP)
                 (SETQ DISP (+ DISP #o1000)))
             (COND ((< DISP #o200)
                    (FORMAT T "~A (~D) "
                            (NTH (LDB (BYTE 4 4) DISP)
                                 '(ZL:AR-1 ARRAY-LEADER %INSTANCE-REF UNUSED-AREFI-3
                                   AS-1 STORE-ARRAY-LEADER %INSTANCE-SET <UNUSED-AREFI-7>))
                            (+ (LDB (BYTE 4 0) DISP)
                               ;; bagbiting %instance-ref is 1-based.  Struth.
                               (IF (MEMQ (LDB (BYTE 2 4) DISP) '(2 6)) 1 0))))
                   ((< DISP #o220)
                    (FORMAT T "~A ~D binding~:P " 'UNBIND (- DISP #o177));200 does 1 unbind.
                    (AND (ZEROP DEST) (RETURN)))
                   ((< DISP #o240)
                    (FORMAT T "~A ~D time~:P " 'PDL-POP (- DISP #o220)) ;220 does 0 pops.
                    (AND (ZEROP DEST) (RETURN)))
                   ((= DISP #o460)              ;(GET 'INTERNAL-FLOOR-1 'QLVAL)
                    (PRINC (NTH DEST '(FLOOR CEILING TRUNCATE ROUND)))
                    (PRINC " one value to stack")
                    (SETQ DEST NIL))
                   ((= DISP #o510)              ;(GET 'INTERNAL-FLOOR-2 'QLVAL)
                    (PRINC (NTH DEST '(FLOOR CEILING TRUNCATE ROUND)))
                    (PRINC " two values to stack")
                    (SETQ DEST NIL))
                   (T
                    (LET ((OP (AREF (SYMBOL-FUNCTION 'MICRO-CODE-SYMBOL-NAME-AREA)
                                    (- DISP #o200))))
                      (IF (NULL OP) (FORMAT T "#~O " DISP) (FORMAT T "~A " OP)))))
             (WHEN DEST (PRINC (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST)))))
            ((= OP #o16)                        ;ND4
             (CASE SUBOP
               (0 (FORMAT T "~A local slot ~D" 'LEXICAL-CLOSURE-DISCONNECT DISP))
               (1 (LET ((LOCALNUM (LDB (BYTE #o12 0)
                                       (NTH DISP  ;; This is in reverse order in the FEF
                                            (reverse
                                              (%P-CONTENTS-OFFSET
                                                FEF (- (%P-LDB %%FEFH-PC-IN-WORDS FEF) 2)))))))
                    (FORMAT T "~A ~A|~D" 'LEXICAL-CLOSURE-UNSHARE 'LOCAL LOCALNUM)
                    (LET ((TEM (DISASSEMBLE-LOCAL-NAME FEF LOCALNUM)))
                      (AND TEM (FORMAT T "~30,4T;~A" TEM)))))
               (2 (FORMAT T "~A local slot ~D" 'MAKE-LEXICAL-CLOSURE DISP))
               (3
                (let (name)
                  (cond ((not (and (disassemble-lexical-reference-instruction-p fef (+ pc ilen))
                                   (setq name (disassemble-lexical-ref-name fef disp))))
                         (FORMAT T "~A ~S" 'PUSH-NUMBER DISP))
                        (t
                         (format t "~A (lexical ref ~D,~D)~30,4T;"
                                 'push (ldb (byte 12. 12.) disp) (ldb (byte 12. 0) disp) name)
                         (if (eq (car-safe name) 'function)
                             (format t "#'~S" (cadr name)) (prin1 name))))))
               (4 (FORMAT T "~A local slot ~D" 'LEXICAL-CLOSURE-DISCONNECT-FIRST DISP))
               (5 (PRINC 'PUSH-CDR-IF-CAR-EQUAL)
                  (WRITE-CHAR #\SPACE)
                  (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD PC))
               (6 (PRINC 'PUSH-CDR-STORE-CAR-IF-CONS)
                  (WRITE-CHAR #\SPACE)
                  (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD PC))
               (T (FORMAT T "<~A-~O> ~D" 'UNDEF-ND4 SUBOP DISP))))
            ((= OP #o20)
             (FORMAT T "~A (~D) "
                     (NTH REG
                          '(ZL:AR-1 ARRAY-LEADER %INSTANCE-REF COMMON-LISP-AR-1
                            SET-AR-1 SET-ARRAY-LEADER SET-%INSTANCE-REF <UNUSED-AREFI>))
                     (+ (LDB (BYTE 6 0) DISP)
                        ;; bagbiting %instance-ref is 1-based.  Struth.
                        (IF (MEMQ REG '(2 6)) 1 0)))
             (PRINC (NTH DEST '(D-IGNORE D-PDL D-RETURN D-LAST))))
            ((= op #o21)
             (IF (zerop (ldb (byte 1 #o15) wd))
                 (FORMAT T "<~A-21-~O>" 'UNDEF SUBOP)
               (CASE (ldb (byte 2 #o16) wd)
                 (0 (format t "~A 1+" 'push))
                 (1 (format t "~A 1-" 'push))
                 (2 (princ 'ZEROP))
                 (t (princ '<UNUSED-QIND5>))))
             (DISASSEMBLE-ADDRESS FEF REG DISP SECOND-WORD))
            (T                                  ;UNDEF
             (FORMAT T "<~A-~O>" 'UNDEF OP)))))
  ILEN)

;;; This ought to figure out which flavor's mapping table is going to be current
;;; at a certain PC, assuming that the compiled code explicitly sets it up.
(DEFUN DISASSEMBLE-CURRENT-FLAVOR (FEF PC)
  (DECLARE (IGNORE FEF PC))
  NIL)

(DEFUN DISASSEMBLE-ADDRESS (FEF REG DISP &OPTIONAL SECOND-WORD PC &AUX TEM)
  "Print out the disassembly of an instruction source address.
REG is the register number of the address, and DISP is the displacement.
SECOND-WORD should be the instruction's second word if it has two.
PC should be where the instruction was found in the FEF."
  ;; In a one-word instruction, the displacement for types 4 through 7 is only 6 bits,
  ;; so ignore the rest.  In a two word insn, we have been fed the full disp from word 2.
  (IF (AND ( REG 4) (NOT SECOND-WORD))
      (SETQ DISP (LOGAND #o77 DISP)))
  (COND ((< REG 4)
         (FORMAT T " ~A|~D~30,4T;" 'FEF DISP)
         (DISASSEMBLE-POINTER FEF DISP PC))
        ((= REG 4)
         (FORMAT T " '~S" (AREF (SYMBOL-FUNCTION 'CONSTANTS-AREA) DISP)))
        ((= REG 5)
         (FORMAT T " ~A|~D" 'LOCAL DISP)
         (SETQ TEM (DISASSEMBLE-LOCAL-NAME FEF DISP))
         (AND TEM (FORMAT T "~30,4T;~A" TEM)))
        ((= REG 6)
         (FORMAT T " ~A|~D" 'ARG DISP)
         (SETQ TEM (DISASSEMBLE-ARG-NAME FEF DISP))
         (AND TEM (FORMAT T "~30,4T;~A" TEM)))
        ((AND (NOT SECOND-WORD) (= DISP #o77))
         (FORMAT T " ~A" 'PDL-POP))
        ((< DISP #o40)
         (FORMAT T " ~A|~D" 'SELF DISP)
         (SETQ TEM (DISASSEMBLE-INSTANCE-VAR-NAME FEF DISP))
         (AND TEM (FORMAT T "~30,4T;~A" TEM)))
        ((< DISP #o70)
         (FORMAT T " ~A|~D" 'SELF-MAP (- DISP #o40))
         (SETQ TEM (DISASSEMBLE-MAPPED-INSTANCE-VAR-NAME FEF (- DISP #o40)))
         (AND TEM (FORMAT T "~30,4T;~A" TEM)))
        (T
         (FORMAT T " ~A|~D (undefined)" 'PDL DISP))))

(defun disassemble-address-contents (fef reg disp &optional second-word)
  (if (and ( reg 4) (not second-word))
      (setq disp (logand #o77 disp)))
  (cond ((< reg 4)
         (cond ((= (%p-ldb-offset %%q-data-type fef disp) dtp-self-ref-pointer)
                (values nil nil))
               ((= (%p-ldb-offset %%q-data-type fef disp) dtp-one-q-forward)
                (values (%p-contents-as-locative-offset fef disp) nil))
               (t
                (values (%p-contents-offset fef disp) t))))
        ((= reg 4)
         (values (aref (symbol-function 'constants-area) disp) t))
        (t
         (values nil nil))))

(DEFUN DISASSEMBLE-POINTER (FEF DISP PC &AUX CELL LOC PTR OFFSET TEM)
  (SETQ LOC (%MAKE-POINTER-OFFSET DTP-LOCATIVE FEF DISP))
  (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF DISP) DTP-SELF-REF-POINTER)
         (MULTIPLE-VALUE-BIND (PTR COMPONENT-FLAVOR-FLAG)
             (SI::FLAVOR-DECODE-SELF-REF-POINTER
               (OR (DISASSEMBLE-CURRENT-FLAVOR FEF PC)
                   (SI::FEF-FLAVOR-NAME FEF))
               (%P-LDB-OFFSET %%Q-POINTER FEF DISP))
           (cond ((NULL PTR)
                  (SETQ CELL "self-ref-pointer " PTR (%P-LDB-OFFSET %%Q-POINTER FEF DISP)))
                 (t (SETQ CELL (IF COMPONENT-FLAVOR-FLAG "mapping table for " ""))
                    (cond (DISASSEMBLE-OBJECT-OUTPUT-FUN
                           (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN PTR CELL LOC T))
                          (t
                           (FORMAT T "~A~S" CELL PTR)
                           (IF (EQUAL CELL "")
                               (PRINC " in SELF"))))))))
        ((= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF DISP) DTP-ONE-Q-FORWARD)
         (SETQ PTR (%FIND-STRUCTURE-HEADER
                     (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF DISP)))
               OFFSET (%POINTER-DIFFERENCE TEM PTR))
         (COND ((SYMBOLP PTR)
                (SETQ CELL (NTH OFFSET '("@+0?? "
                                         ""
                                         "#'"
                                         "@PLIST-HEAD-CELL "
                                         "@PACKAGE-CELL "))))
               ((CONSP PTR)
                (SETQ PTR (CAR PTR) CELL "#'"))
               (T (SETQ CELL "")))
         (IF DISASSEMBLE-OBJECT-OUTPUT-FUN
             (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN PTR CELL LOC T)
           (FORMAT T "~A~S" CELL PTR)))
        ((= (%p-ldb-offset %%q-data-type fef disp) dtp-indexed-forward)
         (let ((cell-name (aref si:*index-name-table* (%p-ldb-offset %%q-pointer fef disp))))
           (setq ptr (cadr cell-name))
           (setq cell (cdr (assq (car cell-name) '((:function . "#'") (:value . "")))))
           (IF DISASSEMBLE-OBJECT-OUTPUT-FUN
               (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN PTR CELL LOC T)
             (FORMAT T "~A~S" CELL PTR))))
        (T
         (IF DISASSEMBLE-OBJECT-OUTPUT-FUN
             (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN (CAR LOC) "'" LOC NIL)
           (FORMAT T "'~S" (%P-CONTENTS-OFFSET FEF DISP))))))

;;; Given a fef and an instance variable slot number,
;;; find the name of the instance variable,
;;; if the fef knows which flavor is involved.
(DEFUN DISASSEMBLE-INSTANCE-VAR-NAME (FEF SLOTNUM)
  (LET ((FLAVOR (GET (CADR (ASSQ ':FLAVOR (FEF-DEBUGGING-INFO FEF))) 'SI:FLAVOR)))
    (AND FLAVOR (NTH SLOTNUM (SI:FLAVOR-ALL-INSTANCE-VARIABLES FLAVOR)))))

(DEFUN DISASSEMBLE-MAPPED-INSTANCE-VAR-NAME (FEF MAPSLOTNUM)
  (LET ((FLAVOR (GET (CADR (ASSQ ':FLAVOR (FEF-DEBUGGING-INFO FEF))) 'SI:FLAVOR)))
    (AND FLAVOR (NTH MAPSLOTNUM (SI:FLAVOR-MAPPED-INSTANCE-VARIABLES FLAVOR)))))

;;; Given a fef and the number of a slot in the local block,
;;; return the name of that local (or NIL if unknown).
;;; If it has more than one name due to slot-sharing, we return a list of
;;; the names, but if there is only one name we return it.
(DEFUN DISASSEMBLE-LOCAL-NAME (FEF LOCALNUM)
  (LET ((NAMES (NTH LOCALNUM (CADR (ASSQ 'COMPILER::LOCAL-MAP (FEF-DEBUGGING-INFO FEF))))))
    (COND ((NULL NAMES) NIL)
          ((NULL (CDR NAMES)) (CAR NAMES))
          (T NAMES))))

;; Given a fef and the number of a slot in the argument block,
;; return the name of that argument (or NIL if unknown).
;; First we look for an arg map, then we look for a name in the ADL.
(DEFUN DISASSEMBLE-ARG-NAME (FEF ARGNUM)
  (LET* ((FDI (FEF-DEBUGGING-INFO FEF))
         (ARGMAP (CADR (ASSQ 'ARG-MAP FDI))))
    (IF ARGMAP
        (CAR (NTH ARGNUM ARGMAP))
      (DO ((ADL (GET-MACRO-ARG-DESC-POINTER FEF) (CDR ADL))
           (IDX 0 (1+ IDX))
           (ADLWORD))
          ((NULL ADL))
        (SETQ ADLWORD (CAR ADL))
        (SELECT (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
          ((FEF-ARG-REQ FEF-ARG-OPT))
          (OTHERWISE (RETURN NIL)))
        (IF (= 1 (LDB %%FEF-NAME-PRESENT ADLWORD))
            (SETQ ADL (CDR ADL)))
        (WHEN (= IDX ARGNUM)
          (RETURN (AND (= 1 (LDB %%FEF-NAME-PRESENT ADLWORD)) (CAR ADL))))
        (SELECT (MASK-FIELD %%FEF-INIT-OPTION ADLWORD)
          ((FEF-INI-PNTR FEF-INI-C-PNTR FEF-INI-OPT-SA FEF-INI-EFF-ADR)
           (SETQ ADL (CDR ADL))))))))

(DEFUN DISASSEMBLE-LEXICAL-REF-NAME (FEF REF)
  (LET ((NAMES (ASSQ REF (CDR (ASSQ 'LEXICAL-REF-MAP (FEF-DEBUGGING-INFO FEF))))))
    (COND (NAMES
           (CADR NAMES))
          (T
           NIL))))
