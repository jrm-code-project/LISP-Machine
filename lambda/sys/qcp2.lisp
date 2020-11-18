;;;   -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-
;;; This is pass 2 of the Lisp machine Lisp compiler

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

;;; PDLLVL, on pass 2, is the current level of the PDL above the last local
;;; (number of temporary slots).  It isn't always updated by things which
;;; push and pop on a very local basis, but function calls, etc. update it.
;;; MAXPDLLVL is the largest value ever attained by PDLLVL.
;;; It goes into the FEF to say how large a stack frame is needed.
;;; The function MKPDLLVL sets PDLLVL and updates MXPDLLVL if necessary.
;;; INCPDLLVL increments PDLLVL by one, updating MXPDLLVL.
(PROCLAIM '(SPECIAL PDLLVL MAXPDLLVL))
;;; NEEDPDL just says we need <n> more words of room on the pdl beyond what is there now.
(DEFMACRO NEEDPDL (N) `(SETQ MAXPDLLVL (MAX MAXPDLLVL (+ PDLLVL ,N))))

;;; *CALL-BLOCK-PDL-LEVELS* is a list of the PDLLVL's corresponding to the open
;;; call blocks.  PDLLVL is pushed on this stack before a call block is pushed
;;; and popped when one is popped (ie, the D-LAST is compiled).
;;; This is used so that we can see how many call blocks lie above
;;; a given old PDLLVL, so that we can compile instructions to pop call blocks
;;; rather than just pdl words (though this isn't implemented now).
;;; The reason for that is that if CALL is open-compiled someday then %SPREAD
;;; will push an unknown number of args on the pdl.  Each %SPREAD will just increment
;;; the stack by one.  Popping a fixed number of words loses when popping these,
;;; but it turns out that you never want to pop one of them without also popping
;;; the call block that contains it.
;;; So if we compile using popping call blocks, it will work!

;;; Each element actually is either just a number or
;;; a list (pdllvl flag tag).  Flag can be either NIL or UNWIND-PROTECT.
(DEFVAR *CALL-BLOCK-PDL-LEVELS*)                ;used only in LAMBDA mode.

(DEFVAR *WITHIN-CATCH* :UNBOUND
  "T on pass 2 if within a CATCH. UNWIND-PROTECTs are counted also.")

(DEFVAR *WITHIN-POSSIBLE-LOOP* :UNBOUND
  "T on pass 2 if within a TAGBODY and a tag has been seen in it already.
If this is NIL while compiling some code, that code can only be executed once.")

(DEFVAR *DROPTHRU* :UNBOUND
  "*DROPTHRU* on pass 2 is T if the code now being output can be reached.
Code which cannot be reached is discarded at a low level.")

(DEFVAR *TAGOUT* :UNBOUND
  "*TAGOUT* on pass 2 is NIL until a tag has been output.
While *TAGOUT* is NIL, setting a local variable to NIL can be flushed.")

(DEFVAR *P2FN* :UNBOUND
  "*P2FN* on pass 2 is the function we are compiling a call to.
Pass 2 handler functions are normally passed the arglist and destination
as arguments, since that makes most of them simpler.
Those that handle more than one function find the function name in *P2FN*.")

(DEFVAR *BDEST* :UNBOUND
  "*BDEST* on pass 2 is the branch destination of the current form, or a tag destination.
See P2BRANCH.")

;;; *M-V-TARGET* on pass 2 says whether and how the function call now being compiled
;;; is supposed to return multiple values.  It is NIL for an ordinary call
;;; from which only one value is expected.  Other things it can be are
;;; MULTIPLE-VALUE-LIST, or a number of values to just leave on the stack on return,
;;; or THROW meaning the values (except for the last one) should be thrown to a tag
;;; (which is at the top of the stack before execution of this expression)
;;; or RETURN meaning return the values (except for the last one) from the active frame,
;;; but do not return control, and leave the last value on the pdl instead.

;;; In the THROW or RETURN case, the caller still gets one value back on the stack
;;; just as if he were not asking for multiple values.  However, additional
;;; values may have been returned via the ADI of some frame, as a side effect.

(DEFVAR *M-V-TARGET*)

;;; Compile code to compute FORM and leave the result on the PDL.
(DEFUN P2PUSH (FORM) (P2 FORM 'D-PDL))

;;; Compile a form for multiple values (maybe).
;;; If our value is non-nil, it means that the code compiled
;;; failed to produce the multiple values as it was asked to.
;;; Normally, the destination should be D-PDL.
;;; If you use another destination, then, if the value returned is non-NIL
;;; then the single value has been compiled to the given destination,
;;; but if the value is NIL, then the destination has been ignored.
;;; This happens because forms that know how to generate the multiple
;;; values setq *M-V-TARGET* to NIL.

;;; Note: It is assumed that D-RETURN never has an *M-V-TARGET*,
;;; and that an *M-V-TARGET* of MULTIPLE-VALUE-LIST implies D-PDL.

(DEFUN P2MV (FORM DEST *M-V-TARGET*)
  (when (eql *m-v-target* 0)
    (fsignal "Nothing?"))
  (IF (NULL *M-V-TARGET*)
      (P2 FORM DEST)
    ;; In macrocode, d-next is the same as d-pdl, but d-pdl causes lots of optimizations.
    (OR GENERATING-MICRO-COMPILER-INPUT-P
        (AND (EQ DEST 'D-NEXT)
             (SETQ DEST 'D-PDL)))
    (COND ((ADRREFP FORM)
           (P2 FORM DEST))
          ((EQ (CAR FORM) 'LEXICAL-REF)
           (P2 FORM DEST))
          ((memq (car form) '(%POP %pop-for-with-stack-list))
           (P2 FORM DEST))
          (T
           (P2F FORM DEST))))
  *M-V-TARGET*)

;;; Compile code to compute FORM and put the result in destination DEST.
;;; If DEST is D-IGNORE, we may not actually bother to compute the value
;;; if we can tell that there would be no side-effects.
;in cross compiler mode, DEST can be a symbol in the K package such as K:O0, K:A1, K:R2, etc.
(DEFUN P2 (FORM DEST)
  (AND (MEMQ DEST '(D-PDL D-NEXT))
       (NEEDPDL 1))
  ;; In macrocode, d-next is the same as d-pdl, but d-pdl causes lots of optimizations.
  (OR GENERATING-MICRO-COMPILER-INPUT-P
      (AND (EQ DEST 'D-NEXT)
           (SETQ DEST 'D-PDL)))
  (COND ((ADRREFP FORM)
         (IF (AND (NOT GENERATING-MICRO-COMPILER-INPUT-P)
                  (EQ DEST 'D-PDL)
                  (QUOTEP FORM)
                  (INTEGERP (SECOND FORM))
                  ( 0 (SECOND FORM) #o777))
             (OUTI `(PUSH-NUMBER ,(SECOND FORM)))
           (OR (EQ DEST 'D-IGNORE)
               (OUTI `(MOVE ,DEST ,(P2-SOURCE FORM DEST))))))
        ((EQ (CAR FORM) 'LEXICAL-REF)
         (OR (EQ DEST 'D-IGNORE)
             (PROGN (P2PUSH-CONSTANT (CADR FORM))
                    (OUTI `(MISC ,DEST %LOAD-FROM-HIGHER-CONTEXT)))))
        ((EQ (CAR FORM) '%POP)
;        (IF (ZEROP PDLLVL)
;            (FERROR "%POP with nothing on the stack."))
         (DECF PDLLVL)
         (MOVE-RESULT-FROM-PDL DEST))
        ;; like %pop, but doesn't affect pdllvl, see with-stack-list in p1
        ((EQ (CAR FORM) '%POP-for-with-stack-list)
         (IF (< PDLLVL 0)
             (FERROR "%POP with nothing on the stack."))
         (MOVE-RESULT-FROM-PDL DEST))
        ((EQ (CAR FORM) 'CHANGE-PDLLVL)
         (LET ((*BDEST* ())
               (*M-V-TARGET* ()))
           (PROG1 (P2F (CADDR FORM) DEST)
                  (MKPDLLVL (+ PDLLVL (CADR FORM))))))
        (T
         (LET ((*BDEST* ())
               (*M-V-TARGET* ()))
           (P2F FORM DEST)))))

(DEFUN P2F (FORM DEST)
  (LET ((PDLLVL PDLLVL)
        (*P2FN* (CAR FORM))
        (ARGL (CDR FORM))
        TEM)
    (COND ((SETQ TEM (GET *P2FN* 'P2))
           (FUNCALL TEM (CDR FORM) DEST))
          ((SETQ TEM (GET *P2FN* 'QINTCMP))
           (P2MISC *P2FN* ARGL DEST TEM))
          (T
           (P2ARGC *P2FN* ARGL (GETARGDESC *P2FN*) DEST *P2FN*)))))
(defun p2f-argc (fn argl dest)
  (p2argc fn argl (getargdesc fn) dest fn))


;;;; Compile functions which have their own special instructions.

;;; Here for a "miscellaneous" instruction (no source address field; args always on PDL).
;;; Such functions have no P2 properties.  We recognize them by their QINTCMP
;;; properties, which hold the number of args which the function takes.
(DEFUN P2MISC (INSN ARGL DEST NARGS)
  (WHEN ( NARGS (LENGTH ARGL))                 ;Too few args
    (BARF INSN "P2MISC WNA"))
  (COND (*M-V-TARGET*
         (P2ARGC INSN ARGL (LIST (CONS NARGS '((FEF-ARG-REQ FEF-QT-EVAL))))
                 DEST *P2FN*))
        (t
         (ARGLOAD (FIRSTN NARGS ARGL) 'D-PDL)
         (ARGLOAD (NTHCDR NARGS ARGL) 'D-IGNORE)
         (LET ((REAL-INSN (OR (GET INSN 'MISC-INSN) INSN)))
           (IF ( (GET REAL-INSN 'QLVAL) #o1000)
               (OUTI (LIST 'MISC1 DEST REAL-INSN))
             (OUTI (LIST 'MISC DEST REAL-INSN)))))))

;;; Compile functions which have special non-destination instructions
;;; which expect one operand on the pdl and one specified as a source.
(DEFPROP *PLUS P2NODEST P2)
(DEFPROP *DIF P2NODEST P2)
(DEFPROP *TIMES P2NODEST P2)
(DEFPROP *QUO P2NODEST P2)
(DEFPROP *LOGAND P2NODEST P2)
(DEFPROP *LOGXOR P2NODEST P2)
(DEFPROP *LOGIOR P2NODEST P2)
(DEFPROP *PLUS M-+ MISC-INSN)
(DEFPROP *DIF M-- MISC-INSN)
(DEFPROP *TIMES M-* MISC-INSN)
(DEFPROP *QUO M-// MISC-INSN)
(DEFPROP *LOGAND M-LOGAND MISC-INSN)
(DEFPROP *LOGXOR M-LOGXOR MISC-INSN)
(DEFPROP *LOGIOR M-LOGIOR MISC-INSN)

;;; Note that DEST should be 0 if we're compiling an instruction
;;; that doesn't produce a result on the PDL (just sets indicators).
;;; If microcompiling, the special instruction complicates the microcompiler,
;;; so use corresponding miscellaneous instruction.
(DEFUN P2NODEST (ARGL DEST)
  (COND ((NULL GENERATING-MICRO-COMPILER-INPUT-P)
         ;; Get first argument onto the pdl unless not being used anyway
         (COND ((EQ DEST 'D-IGNORE)
                (P2 (CAR ARGL) 'D-IGNORE))
               (T (P2 (CAR ARGL) 'D-PDL)))
         (LET ((SOURCE (P2-SOURCE (CADR ARGL) DEST)))   ;Make second argument addressable
           (COND ((EQ DEST 'D-IGNORE))                  ;Generate no code if not being used
                 (T (OUTI `(,*P2FN* 0 ,SOURCE))
                    ;; If the instrn generated a result on the pdl, put it where it belongs.
                    (OR (SI:MEMBER-EQUAL DEST '(0 D-PDL))
                        (MOVE-RESULT-FROM-PDL DEST))))))
        (T (P2MISC (GET *P2FN* 'MISC-INSN) ARGL DEST 2))))

;;; Compile functions which have special instructions with destination fields.
;;; These take only one argument.
;;; The result can go directly to any destination, not just to the PDL.

(DEFPROP CAR P2DEST P2)
(DEFPROP CDR P2DEST P2)
(DEFPROP CAAR P2DEST P2)
(DEFPROP CADR P2DEST P2)
(DEFPROP CDAR P2DEST P2)
(DEFPROP CDDR P2DEST P2)

(DEFUN P2DEST (ARGL DEST)
  (LET ((SOURCE (P2-SOURCE (CAR ARGL) DEST)))
    (OR (EQ DEST 'D-IGNORE)
        (OUTI `(,*P2FN* ,DEST ,SOURCE)))))

;;; Compile comparison functions.  They have special non-destination instructions
;;; which just set the indicators, but do not push T or NIL on the pdl.
;;; These instructions can be used only when calling the function "for predicate"
;;; (destination D-INDS).  Otherwise, the functions have corresponding
;;; miscellaneous instructions which are used instead.

(DEFPROP EQ P2COMPAR P2)
(DEFPROP INTERNAL-= P2COMPAR P2)
(DEFPROP INTERNAL-> P2COMPAR P2)
(DEFPROP INTERNAL-< P2COMPAR P2)
(DEFPROP EQ M-EQ MISC-INSN)
(DEFPROP INTERNAL-= M-= MISC-INSN)
(DEFPROP INTERNAL-> M-> MISC-INSN)
(DEFPROP INTERNAL-< M-< MISC-INSN)

(DEFUN P2COMPAR (ARGL DEST)
  (unless (= (length argl) 2)
    (barf *p2fn* "P2 WNA"))
  (COND ((EQ DEST 'D-IGNORE)
         (ARGLOAD ARGL DEST))
        ((EQ DEST 'D-INDS)
         (P2NODEST ARGL 0))             ;The 0 here means the instruction produces no result
        (T (P2MISC (GET *P2FN* 'MISC-INSN) ARGL DEST 2))))

(DEFCONST AREFI-MAX #o77 "Max index for the new AREFI and AREFI-SET series of instructions")
(DEFCONST ASETI-MAX #o17 "Max index for the old misc insns still used for AS-1, etc.")

(DEFUN (:PROPERTY AR-1 P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 2)
           (QUOTEP (SECOND ARGL))
           (FIXNUMP (CADR (SECOND ARGL)))
           ( 0 (CADR (SECOND ARGL)) AREFI-MAX))
      (PROGN (P2PUSH (CAR ARGL))
             (OUTI `(AREFI ,DEST ,(CADR (SECOND ARGL)))))
    (P2MISC 'AR-1 ARGL DEST 2)))

(DEFUN (:PROPERTY COMMON-LISP-AR-1 P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 2)
           (QUOTEP (SECOND ARGL))
           (FIXNUMP (CADR (SECOND ARGL)))
           ( 0 (CADR (SECOND ARGL)) AREFI-MAX))
      (PROGN (P2PUSH (CAR ARGL))
             (OUTI `(AREFI-COMMON-LISP ,DEST ,(CADR (SECOND ARGL)))))
    (P2MISC 'COMMON-LISP-AR-1 ARGL DEST 2)))

(DEFUN (:PROPERTY SET-AR-1 P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 3)
           (QUOTEP (SECOND ARGL))
           (FIXNUMP (CADR (SECOND ARGL)))
           ( 0 (CADR (SECOND ARGL)) AREFI-MAX))
      (PROGN (P2PUSH (FIRST ARGL))
             (P2PUSH (THIRD ARGL))
             (OUTI `(AREFI-SET ,DEST ,(CADR (SECOND ARGL)))))
    (P2MISC 'SET-AR-1 ARGL DEST 3)))

(DEFUN (:PROPERTY AS-1 P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 3)
           (QUOTEP (THIRD ARGL))
           (FIXNUMP (CADR (THIRD ARGL)))
           ( 0 (CADR (THIRD ARGL)) ASETI-MAX))
      (PROGN (P2PUSH (CAR ARGL))
             (P2PUSH (CADR ARGL))
             (OUTI `(MISC ,DEST ASETI ,(CADR (THIRD ARGL)))))
    (P2MISC 'AS-1 ARGL DEST 3)))

(DEFUN (:PROPERTY ARRAY-LEADER P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 2)
           (QUOTEP (SECOND ARGL))
           (FIXNUMP (CADR (SECOND ARGL)))
           ( 0 (CADR (SECOND ARGL)) AREFI-MAX))
      (PROGN (P2PUSH (CAR ARGL))
             (OUTI `(AREFI-LEADER ,DEST ,(CADR (SECOND ARGL)))))
    (P2MISC 'ARRAY-LEADER ARGL DEST 2)))

(DEFUN (:PROPERTY SET-ARRAY-LEADER P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 3)
           (QUOTEP (SECOND ARGL))
           (FIXNUMP (CADR (SECOND ARGL)))
           ( 0 (CADR (SECOND ARGL)) AREFI-MAX))
      (PROGN (P2PUSH (FIRST ARGL))
             (P2PUSH (THIRD ARGL))
             (OUTI `(AREFI-SET-LEADER ,DEST ,(CADR (SECOND ARGL)))))
    (P2MISC 'SET-ARRAY-LEADER ARGL DEST 3)))

(DEFUN (:PROPERTY STORE-ARRAY-LEADER P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 3)
           (QUOTEP (THIRD ARGL))
           (FIXNUMP (CADR (THIRD ARGL)))
           ( 0 (CADR (THIRD ARGL)) ASETI-MAX))
      (PROGN (P2PUSH (CAR ARGL))
             (P2PUSH (CADR ARGL))
             (OUTI `(MISC ,DEST ASETI-LEADER ,(CADR (THIRD ARGL)))))
    (P2MISC 'STORE-ARRAY-LEADER ARGL DEST 3)))

(DEFUN (:PROPERTY %INSTANCE-REF P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 2)
           (QUOTEP (SECOND ARGL))
           (FIXNUMP (CADR (SECOND ARGL)))
           ( 1 (CADR (SECOND ARGL)) (1+ AREFI-MAX)))
      (PROGN (P2PUSH (CAR ARGL))
             ;; %INSTANCE-REF is cretinously origin-1, but we are always origin-0.
             (OUTI `(AREFI-INSTANCE ,DEST ,(1- (CADR (SECOND ARGL))))))
    (P2MISC '%INSTANCE-REF ARGL DEST 2)))

(DEFUN (:PROPERTY SET-%INSTANCE-REF P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 3)
           (QUOTEP (SECOND ARGL))
           (FIXNUMP (CADR (SECOND ARGL)))
           ( 1 (CADR (SECOND ARGL)) (1+ AREFI-MAX)))
      (PROGN (P2PUSH (FIRST ARGL))
             (P2PUSH (THIRD ARGL))
             ;; %INSTANCE-REF is cretinously origin-1, but we are always origin-0.
             (OUTI `(AREFI-SET-INSTANCE ,DEST ,(1- (CADR (SECOND ARGL))))))
    (P2MISC 'SET-%INSTANCE-REF ARGL DEST 3)))

(DEFUN (:PROPERTY %INSTANCE-SET P2) (ARGL DEST)
  (IF (AND (NULL *M-V-TARGET*)
           (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (= (LENGTH ARGL) 3)
           (QUOTEP (THIRD ARGL))
           (FIXNUMP (CADR (THIRD ARGL)))
           ( 1 (CADR (THIRD ARGL)) (1+ ASETI-MAX)))
      (PROGN (P2PUSH (CAR ARGL))
             (P2PUSH (CADR ARGL))
             ;; %INSTANCE-SET is cretinously origin-1, but we are always origin-0.
             (OUTI `(MISC ,DEST ASETI-INSTANCE ,(1- (CADR (THIRD ARGL))))))
    (P2MISC '%INSTANCE-SET ARGL DEST 3)))

(DEFUN (:PROPERTY FUNCTION P2) (ARGL DEST)
  (OUTI `(MOVE ,DEST (QUOTE-VECTOR (FUNCTION ,(CAR ARGL))))))

(DEFUN (:PROPERTY BREAKOFF-FUNCTION P2) (ARGL DEST)
  (OUTI `(MOVE ,DEST (QUOTE-VECTOR (BREAKOFF-FUNCTION ,(CAR ARGL))))))

(DEFUN (:PROPERTY LEXICAL-CLOSURE P2) (ARGL DEST)
  (P2PUSH (CAR ARGL))
  (OUTI `(,(if *compiling-breakoffs-p*
               'MAKE-CLOSURE
             'make-closure-top-level)
          ,(+ (LENGTH *LOCAL-MAP*) *LEXICAL-CLOSURE-COUNT*)))
  (INCF *LEXICAL-CLOSURE-COUNT*)
  (MOVE-RESULT-FROM-PDL DEST))

(DEFUN (:PROPERTY APPLY P2) (ARGL DEST)
  (COND ((null argl)
         ;; avoid blowout
         (p2f-argc 'apply argl dest))
        ((AND (= (LENGTH ARGL) 2) (NULL *M-V-TARGET*))
         (P2MISC 'INTERNAL-APPLY ARGL DEST 2))
        (T
         (LET ((SOURCE (P2-SOURCE (CAR ARGL) 'D-PDL)))
           ;; Don't hack PDLLVL here since going to pop 1 and push 4
           (P2ARGC SOURCE                       ;Non-symbolic first arg indicates it is
                   (CDR ARGL)                   ; address for CALL instruction
                   '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL LEXPR-FUNCALL)))
                   DEST
                   NIL)))))

(DEFUN (:PROPERTY FUNCALL P2) (ARGL DEST)
  (IF (null argl)
      ;; avoid blowout
      (p2f-argc 'funcall argl dest)
    (LET ((SOURCE (P2-SOURCE (CAR ARGL) 'D-PDL)))
      ;; Don't hack PDLLVL here since going to pop 1 and push 4
      (P2ARGC SOURCE    ;Non-symbolic first arg indicates it is address for CALL instruction
              (CDR ARGL)
              '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL)))
              DEST
              NIL))))

(DEFUN (:PROPERTY SI::FUNCALL-WITH-MAPPING-TABLE-INTERNAL P2) (ARGL DEST)
  (LET ((SOURCE (P2-SOURCE (CAR ARGL) 'D-PDL)))
    ;; Don't hack PDLLVL here since going to pop 1 and push 4
    (P2ARGC SOURCE      ;Non-symbolic first arg indicates it is address for CALL instruction
            (CDDR ARGL)
            '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL)))
            DEST
            NIL
            (CADR ARGL))))

(DEFUN (:PROPERTY SI::LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL P2) (ARGL DEST)
  (LET ((SOURCE (P2-SOURCE (CAR ARGL) 'D-PDL)))
    ;; Don't hack PDLLVL here since going to pop 1 and push 4
    (P2ARGC SOURCE      ;Non-symbolic first arg indicates it is address for CALL instruction
            (CDDR ARGL)
            '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL LEXPR-FUNCALL)))
            DEST
            NIL
            (CADR ARGL))))

(DEFUN (:PROPERTY VARIABLE-LOCATION P2) (ARGL DEST)
  (CASE (CAAR ARGL)
    (LOCAL-REF (OUTI `(PUSH-E 0 ,(VAR-LAP-ADDRESS (CADR (CAR ARGL)))))
               (NEEDPDL 1)
               (MOVE-RESULT-FROM-PDL DEST))
    (SELF-REF (OUTI `(PUSH-E 0 (QUOTE-VECTOR ,(CAR ARGL))))
              (NEEDPDL 1)
              (MOVE-RESULT-FROM-PDL DEST))
    (LEXICAL-REF (P2PUSH-CONSTANT (CADR (CAR ARGL)))
                 (NEEDPDL 1)
                 (OUTI `(MISC ,DEST %LOCATE-IN-HIGHER-CONTEXT)))))

(DEFUN (:PROPERTY COMPILER-LET-INTERNAL P2) (ARGL DEST)
  (IF (NULL (CAR ARGL))
      (P2F (CONS 'PROGN (CDR ARGL)) DEST)
    (PROGV (LIST (CAAAR ARGL)) (LIST (EVAL (CADAAR ARGL)))
      (P2F `(COMPILER-LET-INTERNAL ,(CDAR ARGL) . ,(CDR ARGL))
           DEST))))

;;; %ACTIVATE-OPEN-CALL-BLOCK must ignore its apparent destination and
;;; instead compile to D-IGNORE (microcode depends on this).
;;; This fails to let the compiler know that the pdl is popped and a delayed
;;; transfer may be taken, but then it didn't know the pdl was pushed either.
(DEFUN (:PROPERTY %ACTIVATE-OPEN-CALL-BLOCK P2) (IGNORE IGNORE)
  (OUTI '(MISC D-IGNORE %ACTIVATE-OPEN-CALL-BLOCK)))

;;; Don't actually call %PUSH, just push its argument
(DEFUN (:PROPERTY %PUSH P2) (ARGL DEST)
  (IF (EQ DEST 'D-IGNORE)
      (P2 (CAR ARGL) 'D-PDL)
    (WARN 'CALLED-FOR-VALUE :IMPOSSIBLE "~S may not be called for value." '%PUSH)
    (P2F-ARGC 'IGNORE () DEST)))


(DEFUN (:PROPERTY FLOOR P2) (ARGL DEST)
  (P2FLOOR 0 ARGL DEST))

(DEFUN (:PROPERTY CEILING P2) (ARGL DEST)
  (P2FLOOR 1 ARGL DEST))

(DEFUN (:PROPERTY TRUNCATE P2) (ARGL DEST)
  (P2FLOOR 2 ARGL DEST))

(DEFUN (:PROPERTY ROUND P2) (ARGL DEST)
  (P2FLOOR 3 ARGL DEST))

;;; There are two forms of them: INTERNAL-FLOOR-2 returns two values,
;;; and INTERNAL-FLOOR-1 returns only the first value.
;;; The value or values are left on the stack.
;;; We produce code to request one or two values and move them to
;;; the appropriate place.
;;; FLOOR, CEILING, TRUNCATE and ROUND are distinguished by ROUNDING-TYPE,
;;; which will be put into the destination field of the instruction.
(DEFUN P2FLOOR (ROUNDING-TYPE ARGL DEST)
  (P2PUSH (CAR ARGL))
  (IF (CDR ARGL)
      (P2PUSH (CADR ARGL))
    (P2PUSH-CONSTANT 1))
  (ARGLOAD (CDDR ARGL) 'D-IGNORE)
  (LET ((DESTFIELD (DPB ROUNDING-TYPE %%MACRO-DEST-FIELD 0)))
    (COND ((EQ DEST 'D-RETURN)
           (OUTI `(MISC ,DESTFIELD INTERNAL-FLOOR-2))
           (OUTI '(MISC D-IGNORE %RETURN-2)))
          ((NULL *M-V-TARGET*)
           (OUTI `(MISC ,DESTFIELD INTERNAL-FLOOR-1))
           (MOVE-RESULT-FROM-PDL DEST))
          ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
           (OUTI `(MISC ,DESTFIELD INTERNAL-FLOOR-2))
           (OUTI '(MISC D-PDL NCONS))
           (OUTI '(MISC D-PDL CONS)))
          ((EQ *M-V-TARGET* 'THROW)
           (OUTI `(MISC ,DESTFIELD INTERNAL-FLOOR-2))
           (P2PUSH-CONSTANT 2)
           (OUTI '(MISC D-IGNORE THROW-N)))
          ((EQ *M-V-TARGET* 'RETURN)
           (OUTI `(MISC ,DESTFIELD INTERNAL-FLOOR-2))
           (P2PUSH-CONSTANT 2)
           (OUTI '(MISC D-IGNORE RETURN-N-KEEP-CONTROL)))
          ((= *M-V-TARGET* 1)
           (OUTI `(MISC ,DESTFIELD INTERNAL-FLOOR-1)))
          (T
           (OUTI `(MISC ,DESTFIELD INTERNAL-FLOOR-2))
           (DOTIMES (I (- *M-V-TARGET* 2))
             (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))))
  (SETQ *M-V-TARGET* NIL))

(DEFUN (:PROPERTY MOD P2) (ARGL DEST)
  (UNLESS (MEMQ DEST '(D-IGNORE 0))
    (LET ((*M-V-TARGET* 2))
      (P2FLOOR 0 ARGL 'D-PDL))
    (UNLESS (EQ DEST 'D-RETURN)
      (P2PUSH-CONSTANT 1)
      (OUTI '(MISC D-PDL SHRINK-PDL-SAVE-TOP)))
    (MOVE-RESULT-FROM-PDL DEST)))

(DEFUN (:PROPERTY GET P2) (ARGL DEST &AUX (LEN (LENGTH ARGL)))
  (COND ((= LEN 2)
         (P2MISC 'INTERNAL-GET-2 ARGL DEST 2))
        ((EQUAL (CDDR ARGL) '('NIL))
         (P2MISC 'INTERNAL-GET-2 (LIST (CAR ARGL) (CADR ARGL)) DEST 2))
        (T (P2MISC 'INTERNAL-GET-3 ARGL DEST 3))))

(defun (:property delq p2) (argl dest)
  (if (= (length argl) 3)
      (p2misc '%internal-delq argl dest 3)
      (p2misc '%internal-delq (append argl '('-1)) dest 3)))

(DEFUN (:PROPERTY SETQ P2) (ARGL DEST)
  (PROG ()
        (OR ARGL (RETURN (OUTI `(MOVE ,DEST (QUOTE-VECTOR 'NIL)))))
     LOOP
        (P2SETQ-1 (CAR ARGL) (CADR ARGL)
                  (COND ((NULL (CDDR ARGL)) DEST)
                        (T 'D-IGNORE)))
        (SETQ ARGL (CDDR ARGL))
        (AND ARGL (GO LOOP))))

;;; Compile code to set VAR to the result of computing VALUE,
;;; and also move that value to DEST.
(DEFUN P2SETQ-1 (VAR VALUE DEST)
  (COND ((MEMQ VAR '(NIL T))
         NIL)
        ((AND (CONSP VAR) (EQ (CAR VAR) 'LEXICAL-REF))
         (P2PUSH VALUE)
         (MOVEM-AND-MOVE-TO-DEST VAR DEST))
        ((SI:MEMBER-EQUAL VALUE '('0 'NIL))
         (OUTI `(,(CDR (ASSQ (CADR VALUE)
                             '((0 . SETZERO) (NIL . SETNIL))))
                 0
                 ,(P2-SOURCE VAR 'D-PDL)))
         (OR (MEMQ DEST '(D-IGNORE D-INDS))
             (P2 VALUE DEST)))
        ((AND (NOT (ATOM VALUE))
              (CDR VALUE)
              (EQUAL (CADR VALUE) VAR)
              (MEMQ (CAR VALUE) '(CDR CDDR 1+ 1-))
              (MEMQ DEST '(D-IGNORE D-INDS)))
         (OUTI `(SETE ,(CAR VALUE) ,(P2-SOURCE VAR 'D-PDL))))
        (T
         (P2PUSH VALUE)
         (MOVEM-AND-MOVE-TO-DEST VAR DEST)))
  NIL)

;;; Move the quantity on the top of the stack to the value of a variable
;;; and also move it to the specified destination.
(DEFUN MOVEM-AND-MOVE-TO-DEST (VAR DEST)
  (COND ((ATOM VAR)
         (IF (MEMQ DEST '(D-IGNORE D-INDS))
             (OUTI `(POP 0 (SPECIAL ,VAR)))
           (OUTI `(MOVEM 0 (SPECIAL ,VAR)))
           (MOVE-RESULT-FROM-PDL DEST)))
        ((EQ (CAR VAR) 'LOCAL-REF)
         (IF (MEMQ DEST '(D-IGNORE D-INDS))
             (OUTI `(POP 0 ,(VAR-LAP-ADDRESS (CADR VAR))))
           (OUTI `(MOVEM 0 ,(VAR-LAP-ADDRESS (CADR VAR))))
           (MOVE-RESULT-FROM-PDL DEST)))
        ((EQ (CAR VAR) 'SELF-REF)
         (IF (MEMQ DEST '(D-IGNORE D-INDS))
             (OUTI `(POP 0 (QUOTE-VECTOR ,VAR)))
           (OUTI `(MOVEM 0 (QUOTE-VECTOR ,VAR)))
           (MOVE-RESULT-FROM-PDL DEST)))
        ((EQ (CAR VAR) 'LEXICAL-REF)
         (P2PUSH-CONSTANT (CADR VAR))
         (NEEDPDL 1)
         (OUTI `(MISC ,DEST %STORE-IN-HIGHER-CONTEXT)))))

(DEFUN MOVE-RESULT-FROM-PDL (DEST)
  (IF (NEQ DEST 'D-PDL)
      (OUTI `(MOVE ,DEST PDL-POP))))

(DEFUN (:PROPERTY PUSH-CDR-STORE-CAR-IF-CONS P2) (ARGL DEST)
  (P2PUSH (CAR ARGL))
  (IF (ADRREFP (CADR ARGL))
      (PROGN (OUTI `(PUSH-CDR-STORE-CAR-IF-CONS
                      ,(LET ((VAR (CADR ARGL)))
                         (COND ((ATOM VAR)
                                `(SPECIAL ,VAR))
                               ((EQ (CAR VAR) 'LOCAL-REF)
                                (VAR-LAP-ADDRESS (CADR VAR)))
                               ((EQ (CAR VAR) 'SELF-REF)
                                `(QUOTE-VECTOR ,VAR))))))
             (UNLESS (EQ DEST 'D-INDS)
               (OUTI `(MISC ,DEST INDICATORS-VALUE))))
    (LET ((TAG (GENSYM)))
      (OUTI '(MISC D-INDS CONSP-OR-POP))
      (OUTB `(BRANCH NILIND TRUE NIL ,TAG))
      (OUTI '(MISC D-PDL CARCDR))
      (MOVEM-AND-MOVE-TO-DEST (CADR ARGL) 'D-IGNORE)
      (OUTTAG TAG)
      (UNLESS (EQ DEST 'D-INDS)
        (OUTI `(MISC ,DEST INDICATORS-VALUE))))))

(DEFUN (:PROPERTY PUSH-CDR-IF-CAR-EQUAL P2) (ARGL DEST)
  (P2NODEST ARGL 0)
  (UNLESS (EQ DEST 'D-INDS)
    (OUTI `(MISC ,DEST INDICATORS-VALUE))))

(DEFUN (:PROPERTY PROGN-WITH-DECLARATIONS P2) (ARGL DEST)
  (LET ((*VARS* (CAR ARGL)))
    (P2PROG12N (LENGTH (CDR ARGL)) DEST (CDR ARGL))))

(DEFUN (:PROPERTY THE P2) (ARGL DEST)
  (P2 (CADR ARGL) DEST)
  (IF GENERATING-MICRO-COMPILER-INPUT-P
      (OUTF `(((TYPE-SPECIFIER ,(CAR ARGL)))))))

(DEFUN (:PROPERTY PROGN P2) (ARGL DEST)
  (P2PROG12N (LENGTH ARGL) DEST ARGL))

(DEFUN (:PROPERTY PROG2 P2) (ARGL DEST)
  (P2PROG12N 2 DEST ARGL))

;;; Compile a PROGN or PROG2, etc.  ARGL is the list of argument expressions.
;;; N says which arg is to be returned as the value of the PROGN or PROG2
;;; (equals the length of ARGL for PROGN, or 2 for PROG2, etc.).
(DEFUN P2PROG12N (N DEST ARGL)
  (PROG ((IDEST DEST))
        (COND ((AND (NOT (EQ DEST 'D-IGNORE))
                    (< N (LENGTH ARGL)))
               (SETQ IDEST 'D-PDL)))            ;MIGHT COMPILE TEST ON RESULT INDICATORS
        (DECF N)                                ;Convert to origin 0.
        ;; Compile the args before the one whose value we want.
        (DOTIMES (I N)
          (P2 (OR (CAR ARGL) ''NIL) 'D-IGNORE)
          (POP ARGL))
        ;; Compile the arg whose value we want.
        ;; If it's the last arg (this is PROGN),
        ;; make sure to pass along any multiple value target that the PROGN has,
        ;; and to report back how many args were actually pushed.
        (COND ((AND (NULL (CDR ARGL)) *M-V-TARGET*)
               (COND ((P2MV (OR (CAR ARGL) ''NIL) IDEST *M-V-TARGET*)
                      (INCPDLLVL))
                     ((NUMBERP *M-V-TARGET*)
                      (MKPDLLVL (+ PDLLVL *M-V-TARGET*))
                      (SETQ *M-V-TARGET* NIL))
                     (T (INCPDLLVL)     ;target was THROW, RETURN or MULTIPLE-VALUE-LIST
                        (SETQ *M-V-TARGET* NIL))))
              ((AND (NULL (CDR ARGL)) *BDEST*)
               (P2BRANCH (OR (CAR ARGL) ''NIL) IDEST *BDEST*)
               (SETQ *BDEST* NIL)
               (COND ((EQ IDEST 'D-PDL) (INCPDLLVL))))
              (T (P2 (OR (CAR ARGL) ''NIL) IDEST)
                 (COND ((EQ IDEST 'D-PDL) (INCPDLLVL)))))
        (OR (CDR ARGL) (RETURN NIL))
        ;; Compile the remaining args.
        (DOLIST (ARG (CDR ARGL))
          (P2 ARG 'D-IGNORE))
        (COND ((NOT (EQ IDEST DEST))
               (MOVE-RESULT-FROM-PDL DEST))
              ((NOT (EQ DEST 'D-IGNORE))
               (OUTF '(MOVE D-PDL PDL-POP)))))) ;Make sure it's really in indicators
                             ; if IDEST and DEST both D-PDL

;;;; Functions to gobble multiple values.

(DEFUN (:PROPERTY MULTIPLE-VALUE-BIND P2) (TAIL DEST)
  ;; The first "argument" is the multiple-value producing form.
  (LET ((MVFORM (CAR TAIL))
        (TAIL (CDR TAIL))   ; Remove that, and we have what looks like
                            ; the arguments that a LET would have in pass 2.
        NBINDS)
    (LET* ((VLIST (CAR TAIL))
           (MVTARGET (LENGTH VLIST))
           (*VARS* (SECOND TAIL)))
      ;; Compile the form to leave N things on the stack.
      ;; If it fails to do so, then it left only one, so push the other N-1.
      (AND (P2MV MVFORM 'D-PDL MVTARGET)
           (DO ((I 1 (1+ I))) ((= I MVTARGET))
             (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
      (MKPDLLVL (+ PDLLVL MVTARGET))            ;say that they are there.
      ;; Now pop them off, binding the variables to them.
      ;; Note that the vlist contains the variables
      ;; in the original order,
      ;; each with an initialization of (%POP).
      (SETQ NBINDS (P2PBIND VLIST (THIRD TAIL)))
      ;; Record that they were popped. -- 9/8/84
      ;; This is needed because P2PBIND binds PDLLVL.
      (MKPDLLVL (- PDLLVL MVTARGET)))
    (P2LET-INTERNAL (SECOND TAIL) NBINDS TAIL DEST)))

(DEFUN (:PROPERTY NTH-VALUE P2) (TAIL DEST)
  (IF (TYPEP (CAR TAIL) '(INTEGER 0 63.))
      (IF (ZEROP (CAR TAIL))
          (P2 `(VALUES ,(CADR TAIL)) DEST)
        (P2MV (CADR TAIL) 'D-PDL (1+ (CAR TAIL)))
        (POPPDL 1 (CAR TAIL))
        (MOVE-RESULT-FROM-PDL DEST))
    (barf tail "~S p2 lost" 'nth-value)))

(DEFUN (:PROPERTY MULTIPLE-VALUE P2) (TAIL DEST)
  (LET* ((VARIABLES (CAR TAIL))
         (DEST1 (COND ((AND (EQ DEST 'D-IGNORE) (NULL (CAR VARIABLES))) 'D-IGNORE)
                      (T 'D-PDL))))
    (PROG ()
      (COND ((P2MV (CADR TAIL) DEST1 (LENGTH VARIABLES)) ; NIL if it actually pushes N values.
             ;; It didn't push them.  Set the other variables to NIL.
             (DOLIST (VAR (CDR VARIABLES))
                 (AND VAR (P2SETQ-1 VAR ''NIL 'D-IGNORE)))
             ;; If the single value was discarded, nothing remains to be done.
             (AND (EQ DEST1 'D-IGNORE) (RETURN NIL)))
            (T ;; It really did push N values on the stack.  Pop all but the first off.
             (DOLIST (VAR (REVERSE (CDR VARIABLES)))
               (COND (VAR (MOVEM-AND-MOVE-TO-DEST VAR 'D-IGNORE))
                     (T (OUTF '(MOVE D-IGNORE PDL-POP)))))))
      ;; Now there is only one thing on the stack, which is the value
      ;; of the first variable, and the value to be returned by
      ;; the call to MULTIPLE-VALUE.
      (COND ((CAR VARIABLES) (MOVEM-AND-MOVE-TO-DEST (CAR VARIABLES) DEST))
            (T (MOVE-RESULT-FROM-PDL DEST))))))

(DEFUN (:PROPERTY MULTIPLE-VALUE-PROG1 P2) (TAIL DEST)
  (COND (*M-V-TARGET*
         (UNLESS (P2MV (CAR TAIL) DEST *M-V-TARGET*)
           (SETQ *M-V-TARGET* NIL)))
        ((EQ DEST 'D-RETURN)
         (P2MV (CAR TAIL) 'D-PDL 'RETURN))
        (T (P2 (CAR TAIL)
               (IF (EQ DEST 'D-LAST) 'D-PDL DEST))))
  (DOLIST (FORM (CDR TAIL))
    (P2 FORM 'D-IGNORE))
  (IF (MEMQ DEST '(D-RETURN D-LAST))
      (MOVE-RESULT-FROM-PDL DEST)))

;;; Note that we make no provision for the possibility
;;; than anything might want to optimize being compiled
;;; for multiple-value-list by storing the list directly
;;; to a destination other than D-PDL.
(DEFUN (:PROPERTY MULTIPLE-VALUE-LIST P2) (TAIL DEST)
  (COND ((P2MV (CAR TAIL) 'D-PDL 'MULTIPLE-VALUE-LIST)
         (OUTF `(MISC ,DEST NCONS)))
        (T (MOVE-RESULT-FROM-PDL DEST))))

(DEFUN (:PROPERTY *THROW P2) (TAIL IGNORE)
  (P2PUSH (CAR TAIL))                           ;Compute and push the tag.
  (P2MV (CADR TAIL) 'D-PDL 'THROW)
  (OUTI '(MISC D-IGNORE *THROW))
  (SETQ *DROPTHRU* NIL))

(DEFUN (:PROPERTY MULTIPLE-VALUE-PUSH P2) (TAIL DEST)
  (DECLARE (IGNORE DEST))
  (WHEN (P2MV (CADR TAIL) 'D-PDL (CAR TAIL)) ; NIL if it actually pushes N values.
    ;; It didn't push them.  Push extra NILs.
    (DOTIMES (I (1- (CAR TAIL)))
      (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
  (MKPDLLVL (+ PDLLVL (CAR TAIL))))

;;;; Functions to generate multiple values.

(DEFPROP VALUES P2VALUES P2)
(DEFUN P2VALUES (ARGL DEST &AUX (NARGS (LENGTH ARGL)))
  ;; Handle returning from the top level of a function.
  (COND ((EQ DEST 'D-RETURN)
         (COND ((= NARGS 1)
                ;; DON'T change this to (P2 ... 'D-RETURN)
                ;; because we want to make sure to pass only one value.
                (P2 (CAR ARGL) 'D-PDL)
                (MOVE-RESULT-FROM-PDL 'D-RETURN)
                NIL)
               (T
                (ARGLOAD ARGL 'D-PDL)
                (CASE NARGS
                  (0 (P2VALUES-LIST '('NIL) DEST))
                  (2 (OUTI '(MISC D-IGNORE %RETURN-2)))
                  (3 (OUTI '(MISC D-IGNORE %RETURN-3)))
                  (T (P2PUSH-CONSTANT NARGS)
                     (OUTI '(MISC D-IGNORE %RETURN-N))))
                (SETQ *DROPTHRU* NIL)   ;Above MISC RETURN instructions return
                NIL)))
        ((NUMBERP *M-V-TARGET*)
         ;; If we want N values on the stack,
         ;; then eval all the args to return
         ;; and save exactly N things on the stack.
         (DO ((VALS ARGL (CDR VALS)) (I 0 (1+ I)))
             ((AND (NULL VALS) ( I *M-V-TARGET*)))
           (P2 (OR (CAR VALS) ''NIL)
               (IF ( I *M-V-TARGET*) 'D-IGNORE 'D-PDL)))
         (SETQ *M-V-TARGET* NIL))
        ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
         (P2 `(LIST . ,ARGL) DEST)
         (SETQ *M-V-TARGET* NIL))
        ((EQ *M-V-TARGET* 'THROW)
         (DOLIST (ELT ARGL)
           (P2PUSH ELT))
         (UNLESS (= NARGS 1)
           (P2PUSH-CONSTANT NARGS)
           (OUTI '(MISC D-IGNORE THROW-N)))
         (SETQ *M-V-TARGET* NIL))
        ((EQ *M-V-TARGET* 'RETURN)
         (DOLIST (ELT ARGL)
           (P2PUSH ELT))
         (UNLESS (= NARGS 1)
           (P2PUSH-CONSTANT NARGS)
           (OUTI '(MISC D-IGNORE RETURN-N-KEEP-CONTROL)))
         (SETQ *M-V-TARGET* NIL))
        ((NULL *M-V-TARGET*)
         (LET ((PDLLVL PDLLVL))
           (P2PROG12N 1 DEST ARGL))
         (SETQ *M-V-TARGET* NIL))
        (T ;??
         (SETQ *M-V-TARGET* NIL))))

(DEFPROP VALUES-LIST P2VALUES-LIST P2)
(DEFUN P2VALUES-LIST (ARGL DEST &AUX (ARG (CAR ARGL)))
  (COND ((AND (CONSP ARG) (EQ (CAR ARG) 'MULTIPLE-VALUE-LIST))
         (SETQ *M-V-TARGET* (P2MV (CADR ARG) DEST *M-V-TARGET*)))
        ((AND (CONSP ARG) (EQ (CAR ARG) 'LIST))
         (P2VALUES (CDR ARG) DEST))
        (T (COND ((EQ DEST 'D-RETURN)
                  (P2MISC 'RETURN-LIST ARGL 'D-RETURN 1))
                 ((NULL *M-V-TARGET*)
                  (P2 `(CAR ,ARG) DEST))
                 ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
                  (P2 ARG DEST))
                 ((EQ *M-V-TARGET* 'THROW)
                  (P2PUSH ARG)
                  (OUTI '(MISC D-IGNORE THROW-SPREAD))
                  ;; This does not really throw!!  It CAN drop thru after this!!.
                  )
                 ((EQ *M-V-TARGET* 'RETURN)
                  (P2PUSH ARG)
                  (OUTI '(MISC D-IGNORE RETURN-SPREAD-KEEP-CONTROL)))
                 ((NUMBERP *M-V-TARGET*)
                  (NEEDPDL 2)
                  (P2PUSH ARG)
                  (OUTF `(MOVE D-PDL (QUOTE-VECTOR ',*M-V-TARGET*)))
                  (OUTF '(MISC D-PDL %SPREAD-N))))
           (SETQ *M-V-TARGET* NIL))))

(DEFUN (:PROPERTY UNWIND-PROTECT P2) (FORMS DEST)
  (LET ((RESTART-TAG (GENSYM))
        (EXIT-TAG (IF *M-V-TARGET* (GENSYM)))
        (PDLLVL0 PDLLVL))
    (LET ((*CALL-BLOCK-PDL-LEVELS* *CALL-BLOCK-PDL-LEVELS*)
          (*WITHIN-CATCH* T))
      ;; Open a multiple-value call to *CATCH with four values expected.
      ;; This is relevant only if we do catch an unwind,
      ;; and in this case the value we get is the last one.
      (OUTI1 `(ADI-CALL CALL D-IGNORE (QUOTE-VECTOR #'*CATCH)
                        (RESTART-PC (QUOTE-VECTOR (TAG ,RESTART-TAG))
                                    BIND-STACK-LEVEL NIL
                                    MULTIPLE-VALUE (QUOTE-VECTOR (QUOTE 4)))))
      (MKPDLLVL (+ PDLLVL 10.))                 ;4 multiple value words, 6 ADI words
      (PUSH (LIST PDLLVL 'UNWIND-PROTECT RESTART-TAG)
            *CALL-BLOCK-PDL-LEVELS*)
      (MKPDLLVL (+ 4 PDLLVL))                   ;4 words of call block
      (OUTI `(MOVE D-PDL (QUOTE-VECTOR (QUOTE T))))     ;Catch tag is T
      (INCPDLLVL)
      (COND ((NUMBERP *M-V-TARGET*)
             (WHEN (P2MV (CAR FORMS) 'D-PDL *M-V-TARGET*)
               (DOTIMES (I (1- *M-V-TARGET*))
                 (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
             (POPPDL *M-V-TARGET* (- PDLLVL PDLLVL0))
             (OUTB `(BRANCH ALWAYS NIL NIL ,EXIT-TAG)))
            ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
             (IF (P2MV (CAR FORMS) 'D-PDL *M-V-TARGET*)
                 (OUTI '(MISC D-LAST NCONS))
               (OUTI '(MOVE D-LAST PDL-POP))))
            ((OR (EQ *M-V-TARGET* 'THROW) (EQ *M-V-TARGET* 'RETURN))
             (P2MV (CAR FORMS) 'D-PDL *M-V-TARGET*)
             (OUTI '(MOVE D-LAST PDL-POP)))
            (T
             (P2 (CAR FORMS) (IF (EQ DEST 'D-RETURN) DEST 'D-LAST))))
      (SETQ *M-V-TARGET* NIL)
      (SETQ PDLLVL (+ PDLLVL0 4))               ;Now have just 4 multiple values on stack
      (SETQ *DROPTHRU* T)
      (OUTF `(RESTART-TAG ,RESTART-TAG)))
    (DOLIST (FORM (CDR FORMS))                  ;Cleanup forms
      (P2 FORM 'D-IGNORE))
    (SETQ PDLLVL PDLLVL0)
    (OUTI `(MISC ,DEST %UNWIND-PROTECT-CONTINUE))
    (IF EXIT-TAG (OUTF EXIT-TAG))))             ;Continue according to stuff on stack

(DEFUN (:PROPERTY %MAKE-EXPLICIT-STACK-LIST P2) (FORMS DEST)
  (ARGLOAD FORMS 'D-PDL)
  (OUTI `(MOVE D-PDL (QUOTE-VECTOR ',(LENGTH FORMS))))
  (OUTI `(MISC ,DEST %MAKE-EXPLICIT-STACK-LIST)))

(DEFUN (:PROPERTY %MAKE-EXPLICIT-STACK-LIST* P2) (FORMS DEST)
  (ARGLOAD FORMS 'D-PDL)
  (OUTI `(MOVE D-PDL (QUOTE-VECTOR ',(LENGTH FORMS))))
  (OUTI `(MISC ,DEST %MAKE-EXPLICIT-STACK-LIST*)))

(DEFUN (:PROPERTY LET* P2) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (P2LET-INTERNAL *VARS*
                    (P2SBIND (CAR ARGL) (CADDR ARGL) *VARS*)
                    ARGL DEST)))

(DEFUN (:PROPERTY LET P2) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (P2LET-INTERNAL *VARS* (P2PBIND (CAR ARGL) (CADDR ARGL)) ARGL DEST)))

(DEFUN (:PROPERTY LET-FOR-LAMBDA P2) (ARGL DEST)
  (LET ((OVARS *VARS*)
        (*VARS* *VARS*)
        (NBINDS (P2PBIND (CAR ARGL) (CADDR ARGL))))
    (PROCESS-SPECIAL-DECLARATIONS (CADR ARGL))
    (P2LET-INTERNAL OVARS NBINDS ARGL DEST)))

;;; LET-HACK is generated by LET-INTERNAL in case of lexical closures and *WITHIN-CATCH*.
(DEFUN (:PROPERTY LET-HACK P2) (ARGL DEST)
  (LET ((*VARS* (CAR ARGL)))
    (P2LET-INTERNAL *VARS* (CADR ARGL) (CADDR ARGL) DEST T)))

;;; Compile the body of a LET.  The variable binding has already been done
;;; by P1PBIND or P1SBIND, which returned the number of special bindings made
;;; which is our argument NBINDS.
(DEFUN P2LET-INTERNAL (OVARS NBINDS ARGL DEST &OPTIONAL IGNORE-LEXICAL-CLOSURES)
  (IF (AND *WITHIN-CATCH*
           (NOT IGNORE-LEXICAL-CLOSURES)
           (NEQ (FIFTH ARGL) (SIXTH ARGL)))
      (P2F `(UNWIND-PROTECT
                (LET-HACK ,OVARS ,NBINDS ,ARGL)
              (DISCONNECT-CLOSURES ,(FIFTH ARGL) ,(SIXTH ARGL))
              (UNSHARE-CLOSURE-VARS ,*VARS* ,OVARS))
           DEST)
    (LET* ((*VARS* (THIRD ARGL))
           (IBINDP (FOURTH ARGL))
           (ENTRY-LEXICAL-CLOSURE-COUNT (FIFTH ARGL))
           (EXIT-LEXICAL-CLOSURE-COUNT (SIXTH ARGL))
           (BDY (NTHCDR 6 ARGL))
           (IDEST 'D-PDL)
           NVALUES
           M-V-DONE
           (*PROGDESC-ENVIRONMENT* *PROGDESC-ENVIRONMENT*))
      ;; Determine the immediate destination of returns in this prog.
      (AND (MEMQ DEST '(D-IGNORE D-INDS D-RETURN))
           (NULL *M-V-TARGET*)
           (SETQ IDEST DEST))
      ;; If %BIND is used within this LET, and it's an internal LET,
      ;; we must push the specpdl index at entry so we can unbind to it later.
      ;; This is not needed for D-RETURN since function exit pops all bindings.
      (WHEN (AND IBINDP (NOT (EQ DEST 'D-RETURN)))
        (OUTI '(MISC D-PDL SPECIAL-PDL-INDEX))
        (INCPDLLVL))
      ;; Push a dummy progdesc so that GOs exiting this LET can unbind our specials.
      (PUSH (MAKE-PROGDESC :NAME '(LET)
                           :PDL-LEVEL PDLLVL
                           :NBINDS (IF IBINDP (LIST NBINDS) NBINDS))
            *PROGDESC-ENVIRONMENT*)
      (WHEN (AND (EQ *M-V-TARGET* 'THROW) IBINDP)
        (P2PUSH-CONSTANT 1)
        (OUTI '(MISC D-PDL PDL-WORD)))
      ;; How many words are we supposed to leave on the stack?
      (SETQ NVALUES
            (COND ((NUMBERP *M-V-TARGET*) *M-V-TARGET*)
                  ((EQ IDEST 'D-PDL) 1)
                  (T 0)))
      (UNLESS BDY (SETQ BDY '('NIL)))
      (DO ((TAIL BDY (CDR TAIL)))
          ((NULL (CDR TAIL))
           (UNLESS (P2MV (CAR TAIL) IDEST *M-V-TARGET*)
             (SETQ M-V-DONE T)))
        (P2 (CAR TAIL) 'D-IGNORE))
      (UNLESS M-V-DONE
        (SETQ NVALUES 1))
      ;; If this is a top-level PROG, we just went to D-RETURN, so we are done.
      (UNLESS (EQ DEST 'D-RETURN)
        ;; Unbind any locals that need to be unbound.
        (WHEN (AND (NOT IGNORE-LEXICAL-CLOSURES)
                   ( ENTRY-LEXICAL-CLOSURE-COUNT EXIT-LEXICAL-CLOSURE-COUNT)
                   ;; If this code can only be executed once per function call
                   ;; and the variables we would consider unsharing
                   ;; are not overlapped with any other variables,
                   ;; then we do not need to unbind them explicitly.
                   (OR *WITHIN-POSSIBLE-LOOP*
                       (DO ((VS *VARS* (CDR VS)))
                           ((EQ VS OVARS))
                         (LET ((V (CAR VS)))
                           (WHEN (OR (MEMQ 'FEF-ARG-OVERLAPPED (VAR-MISC V))
                                     (VAR-OVERLAP-VAR V))
                             (RETURN T))))))
          (P2 `(DISCONNECT-CLOSURES ,ENTRY-LEXICAL-CLOSURE-COUNT
                                    ,EXIT-LEXICAL-CLOSURE-COUNT)
              'D-IGNORE)
          (P2 `(UNSHARE-CLOSURE-VARS ,*VARS* ,OVARS) 'D-IGNORE))
        (WHEN (AND (EQ *M-V-TARGET* 'THROW) IBINDP)
          (POPPDL NVALUES 1))
        ;; Unbind any specials
        (WHEN IBINDP
          (OUTPUT-UNBIND-TO-INDEX NVALUES))
        (UNBIND IDEST NBINDS)
        ;; Dispose of our value.
        (AND (NEQ DEST IDEST)
             (NULL *M-V-TARGET*)
             (MOVE-RESULT-FROM-PDL DEST))
        ;; If we produced multiple values, say we did.
        (IF M-V-DONE (SETQ *M-V-TARGET* NIL))))))

;;; These two do not occur in code except as generated by P2PROG-INTERNAL.
;;; They are almost a kind of macro for use in pass 2.
(DEFUN (:PROPERTY DISCONNECT-CLOSURES P2) (ARGL IGNORE)
  (LET ((ENTRY-LEXICAL-CLOSURE-COUNT (CAR ARGL))
        (EXIT-LEXICAL-CLOSURE-COUNT (CADR ARGL)))
    (DO ((I (max ENTRY-LEXICAL-CLOSURE-COUNT *highest-lexical-closure-disconnected*) (1+ I)))
        ((= I EXIT-LEXICAL-CLOSURE-COUNT)
         (setq *highest-lexical-closure-disconnected* exit-lexical-closure-count))
      (OUTI `(,(IF (= I ENTRY-LEXICAL-CLOSURE-COUNT)
                   'CLOSURE-DISCONNECT-FIRST
                   'CLOSURE-DISCONNECT)
              ,(+ (LENGTH *LOCAL-MAP*) I))))))

(DEFUN (:PROPERTY UNSHARE-CLOSURE-VARS P2) (ARGL IGNORE)
  (LET ((*VARS* (CAR ARGL)) (OVARS (CADR ARGL)))
    (DO ((VS *VARS* (CDR VS)))
        ((EQ VS OVARS))
      (LET ((V (CAR VS)))
        (WHEN (MEMQ 'FEF-ARG-USED-IN-LEXICAL-CLOSURES (VAR-MISC V))
          (OUTI `(CLOSURE-UNSHARE
                   ,(FIND-POSITION-IN-LIST V *VARIABLES-USED-IN-LEXICAL-CLOSURES*))))))))

;;;; Compile a BLOCK.

;;; A BLOCK has no user-defined GOTAGS, but it does have one tag at this level: its rettag.
(DEFUN (:PROPERTY BLOCK P2) (ARGL DEST)
  (P2BLOCK ARGL DEST NIL NIL))

;;; This differs from block only when DEST is D-INDS.
;;; In that case, this one compiles the value to the PDL,
;;; then moves it to D-INDS after popping off any excess pdl words
;;; underneath it.  BLOCK would compile the value direct to D-INDS,
;;; which loses if words must be popped off the stack on falling thru.
;;; However, that is something that cannot happen for user BLOCKs.
;;; It can happen only for the weird BLOCK body that WITH-STACK-LIST generates.
(DEFUN (:PROPERTY BLOCK-FOR-WITH-STACK-LIST P2) (ARGL DEST)
  (P2BLOCK ARGL DEST NIL T))

(DEFUN (:PROPERTY BLOCK-FOR-PROG P2) (ARGL DEST)
  (P2BLOCK ARGL DEST T NIL))

(DEFUN P2BLOCK (ARGL DEST &OPTIONAL ALSO-BLOCK-NAMED-NIL D-INDS-LOSES)
  (LET* ((OLDGOTAGS *GOTAG-ENVIRONMENT*)
         (*GOTAG-ENVIRONMENT* (CAR ARGL)) (MYPROGDESC (CADR ARGL)) (BDY (CDDR ARGL))
         (*PROGDESC-ENVIRONMENT* *progdesc-environment*)
         (PROGNAME (PROGDESC-NAME MYPROGDESC))
         (RETTAG (PROGDESC-RETTAG MYPROGDESC))
         (IDEST 'D-PDL)
         NVALUES)
    ;; Determine the immediate destination of returns in this prog.
    (AND (MEMQ DEST '(D-IGNORE D-INDS D-RETURN))
         (NOT (AND (EQ DEST 'D-INDS) D-INDS-LOSES))
         (NULL *M-V-TARGET*)
         (SETQ IDEST DEST))
    ;; Add this block to the stack of entered ones.
    (SETF (PROGDESC-IDEST MYPROGDESC) IDEST)
    (SETF (PROGDESC-M-V-TARGET MYPROGDESC) *M-V-TARGET*)
    (SETF (PROGDESC-PDL-LEVEL MYPROGDESC) PDLLVL)
    (SETF (PROGDESC-NBINDS MYPROGDESC) 0)
    (PUSH MYPROGDESC *PROGDESC-ENVIRONMENT*)
    ;; For PROG, add a block named NIL also.
    (WHEN (AND ALSO-BLOCK-NAMED-NIL (NEQ PROGNAME 'T))
      (PUSH (COPY-PROGDESC MYPROGDESC) *PROGDESC-ENVIRONMENT*)
      (SETF (PROGDESC-NAME (CAR *PROGDESC-ENVIRONMENT*)) 'NIL))
    ;; How many words are we supposed to leave on the stack?
    (SETQ NVALUES
          (COND ((NUMBERP *M-V-TARGET*) *M-V-TARGET*)
                ((EQ IDEST 'D-PDL) 1)
                (T 0)))
    ;; Set the GOTAG-PDL-LEVEL of the rettag.
    ;; *GOTAG-ENVIRONMENT* at this moment contains the RETTAG and nothing else.
    (SETF (GOTAG-PROGDESC (CAR *GOTAG-ENVIRONMENT*))
          (CAR *PROGDESC-ENVIRONMENT*))
    (SETF (GOTAG-PDL-LEVEL (CAR *GOTAG-ENVIRONMENT*))
          (+ PDLLVL NVALUES))
    (SETF *GOTAG-ENVIRONMENT*
          (APPEND *GOTAG-ENVIRONMENT* OLDGOTAGS))
    ;; Generate code for the body.
    (IF (NULL BDY)
        (P2RETURN1 '('NIL) PROGNAME)
      (DO ((TAIL BDY (CDR TAIL)))
          ((NULL (CDR TAIL))
           (P2RETURN1 (LIST (CAR TAIL)) PROGNAME))
        (P2 (CAR TAIL) 'D-IGNORE)))
    ;; If this is a top-level BLOCK, we just went to D-RETURN,
    ;; and nobody will use the RETTAG, so we are done.
    (IF (EQ DEST 'D-RETURN)
        NIL
      ;; Otherwise, this is where RETURNs jump to.
      (MKPDLLVL (GOTAG-PDL-LEVEL (CAR *GOTAG-ENVIRONMENT*)))
      (OUTTAG RETTAG)
      ;; Store away the value if
      ;; it is not supposed to be left on the stack.
      (AND (NEQ DEST IDEST)
           (NULL *M-V-TARGET*)
           (MOVE-RESULT-FROM-PDL DEST))
      ;; If we were supposed to produce multiple values, we did.
      (SETQ *M-V-TARGET* NIL))))

;;;; Various types of RETURN.

;(DEFUN (:PROPERTY RETURN-LIST P2) (ARGL IGNORE)
;  (P2RETURN1 `((VALUES-LIST ,(CAR ARGL))) NIL))

(DEFUN (:PROPERTY RETURN-FROM P2) (ARGL IGNORE)
  (P2RETURN1 (CDR ARGL) (CAR ARGL)))

;;; (RETURN-FROM-T <value>) is like (RETURN-FROM T <value>).
(DEFUN (:PROPERTY RETURN-FROM-T P2) (ARGL IGNORE)
  (P2RETURN1 ARGL T))

(DEFUN P2RETURN1 (ARGL PROGNAME)
  (LET ((RPDESC (find progname *PROGDESC-ENVIRONMENT* :key #'progdesc-name))
        IPROGDEST
        MVTARGET
        ARG
        LOSE
        NVALUES)
    (OR RPDESC
      (FERROR "Internal compiler error: BLOCK environments randomized."))
    (COND ((= (LENGTH ARGL) 1)
           (SETQ ARG (CAR ARGL)))
          (T (SETQ ARG `(VALUES . ,ARGL))))
    (SETQ IPROGDEST (PROGDESC-IDEST RPDESC))
    (SETQ MVTARGET (PROGDESC-M-V-TARGET RPDESC))
    ;; If going to throw values, things expect tag on top of stack.  So copy it to there.
    (WHEN (EQ MVTARGET 'THROW)
      (UNLESS (= PDLLVL (PROGDESC-PDL-LEVEL RPDESC))
        (P2PUSH-CONSTANT (- PDLLVL (PROGDESC-PDL-LEVEL RPDESC)))
        (OUTI '(MISC D-PDL PDL-WORD))
        (INCPDLLVL)))
    ;; Compile the arg with same destination and *m-v-target*
    ;; that the PROG we are returning from had.
    (SETQ LOSE (P2MV ARG IPROGDEST MVTARGET))
    ;; But, since a PROG has multiple returns, we can't simply
    ;; pass on to the PROG's caller whether this function did or did not
    ;; generate those multiple values if desired.
    ;; If the function failed to, we just have to compensate here.
    (AND LOSE
         (COND ((NUMBERP MVTARGET)
                ;; If we wanted N things on the stack, we have only 1, so push N-1 NILs.
                (DO ((I 1 (1+ I))) ((= I MVTARGET))
                  (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
               ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
                (OUTF '(MISC D-PDL NCONS)))))
    (SETQ NVALUES (COND ((NUMBERP MVTARGET) MVTARGET)
                        ((EQ IPROGDEST 'D-PDL) 1)
                        (T 0)))
    ;; Note how many things we have pushed.
    (AND (EQ IPROGDEST 'D-PDL)
         (MKPDLLVL (+ PDLLVL NVALUES)))
    ;; Jump to the prog's rettag, unless the prog is top-level (to d-return)
    ;; since in that case the code just compiled will not ever drop through.
    (OR (EQ IPROGDEST 'D-RETURN)
        (OUTBRET (PROGDESC-RETTAG RPDESC) RPDESC NVALUES))))

(DEFUN (:PROPERTY TAGBODY P2) (ARGL PROGDEST)
  (LET* ((MYGOTAGS (CAR ARGL))
         (*GOTAG-ENVIRONMENT* *GOTAG-ENVIRONMENT*)
         (*WITHIN-POSSIBLE-LOOP* *WITHIN-POSSIBLE-LOOP*)
         (MYPROGDESC (cadr argl))
         (BODY (CDDR ARGL))
         ;(MYPROGDESC (GOTAG-PROGDESC (CAR MYGOTAGS)))  ;can lose if no gotags
         (*PROGDESC-ENVIRONMENT* *PROGDESC-ENVIRONMENT*))
    ;; Remember this TAGBODY's general environment.
    ;; We supply as the supposed block name
    ;; a list that will not appear as the block name in any RETURN-FROM.
    ;; So we can have an entry on the *PROGDESC-ENVIRONMENT* list to record our tags' pdllvl
    ;; without interfering with RETURN-FROM.
    (WHEN MYGOTAGS
      (SETF (PROGDESC-PDL-LEVEL MYPROGDESC) PDLLVL)
      (PUSH MYPROGDESC *PROGDESC-ENVIRONMENT*)
      ;; Set the GOTAG-PDL-LEVEL of each of the tags.
      (DOLIST (GOTAG MYGOTAGS)
        (SETF (GOTAG-PDL-LEVEL GOTAG) PDLLVL))
      (SETQ *GOTAG-ENVIRONMENT* (APPEND MYGOTAGS *GOTAG-ENVIRONMENT*)))
    (DOLIST (STMT BODY)
      (COND ((ATOM STMT)
             (OR *DROPTHRU* (OUTF '(NO-DROP-THROUGH)))
             (SETQ *TAGOUT* (SETQ *DROPTHRU* T))
             (SETQ *WITHIN-POSSIBLE-LOOP* T)
             (OUTF (GTAG STMT)))
            (T (P2 STMT 'D-IGNORE))))
    (P2 ''NIL PROGDEST)))

(DEFUN (:PROPERTY GO P2) (ARGL IGNORE)
  (COND ((NULL *PROGDESC-ENVIRONMENT*)
         (WARN 'BAD-PROG ':IMPOSSIBLE
               "There is a ~S to ~S not within any ~S."
               'GO (CAR ARGL) 'TAGBODY))
        ((OR (SYMBOLP (CAR ARGL))
             (NOT (%POINTERP (CAR ARGL))))
         (OUTBRET (CAR ARGL) NIL 0))
        (T
         (WARN 'BAD-PROG ':IMPOSSIBLE
               "The argument of ~S was ~S, not a symbol."
               'GO (CAR ARGL)))))

(DEFUN (:PROPERTY GO-HACK P2) (ARGL IGNORE)
  (OUTB `(BRANCH ALWAYS NIL NIL ,(GOTAG-LAP-TAG (CAR ARGL)))))

;;; Unbind NBINDS special variables, unless IDEST is D-RETURN.
(DEFUN UNBIND (IDEST NBINDS)
  (OR (EQ IDEST 'D-RETURN)                      ;returning unbinds for us
      (DO ((N 16. (+ N 16.)))
          ;; N is number of unbinds we would have done if we now
          ;; unbind another 16.  N-16 is number unbound so far.
          ;; Note that an UNBIND X instruction unbinds X+1 vars.
          ((> N NBINDS)
           (OR (= NBINDS (- N 16.))
               (OUTI `(MISC D-IGNORE UNBIND ,(- NBINDS (- N 16.) 1)))))
        (OUTI '(MISC D-IGNORE UNBIND 15.)))))

;;; Compile something to be addressed by an instruction.
;;; Return the address which the instruction can address it by.
;;; Can push the value on the stack and return PDL-POP,
;;; or for a variable or constant can just return its address.
;;; DEST is significant only if it is D-IGNORE, in which case
;;; we compile code to compute and ignore the value.  What we return then is irrelevant.
(DEFUN P2-SOURCE (FORM DEST)
  (COND ((ATOM FORM)
         `(SPECIAL ,FORM))
        ((EQ (CAR FORM) 'LOCAL-REF)
         (VAR-LAP-ADDRESS (CADR FORM)))
        ((EQ (CAR FORM) 'LEXICAL-REF)
         (COND ((NEQ DEST 'D-IGNORE)
                (P2PUSH-CONSTANT (CADR FORM))
                (OUTI '(MISC D-PDL %LOAD-FROM-HIGHER-CONTEXT))))
         '(PDL-POP))
        ((MEMQ (CAR FORM) '(FUNCTION QUOTE BREAKOFF-FUNCTION SELF-REF))
         `(QUOTE-VECTOR ,FORM))
        (T (LET ((*BDEST* NIL)
                 (*M-V-TARGET* NIL))
             (P2F FORM (COND ((EQ DEST 'D-IGNORE) 'D-IGNORE) (T 'D-PDL)))
             '(PDL-POP)))))

(DEFUN P2PUSH-CONSTANT (CONSTANT)
  (IF (AND (NOT GENERATING-MICRO-COMPILER-INPUT-P)
           (TYPEP CONSTANT '(INTEGER 0 #o777)))
      (OUTI `(PUSH-NUMBER ,CONSTANT))
      (OUTI `(MOVE D-PDL (QUOTE-VECTOR (QUOTE ,CONSTANT))))))

(DEFUN ADRREFP (EXP)                            ;Predicate T if can be ref by adr only
  (OR (ATOM EXP)
      (MEMQ (CAR EXP) '(LOCAL-REF QUOTE FUNCTION BREAKOFF-FUNCTION SELF-REF))))

(DEFUN MKPDLLVL (X)
;This happens legitimately; for example, exiting from a WITH-STACK-LIST.
;  (IF (< X PDLLVL)
;      (FERROR "MKPDLLVL made the pdl level smaller."))
  (IF (> (SETQ PDLLVL X) MAXPDLLVL)
      (SETQ MAXPDLLVL PDLLVL)))

;;; Equivalent to (MKPDLLVL (1+ PDLLVL)) but call is just one word.
(DEFUN INCPDLLVL ()
  (SETQ MAXPDLLVL (MAX MAXPDLLVL (INCF PDLLVL))))

(DEFUN ARGLOAD (ARGL DEST &AUX (IDEST (IF (EQ DEST 'D-IGNORE) 'D-IGNORE 'D-PDL)))
  (DOLIST (ARG ARGL)
    (P2 ARG IDEST)
    (UNLESS (EQ IDEST 'D-IGNORE)
      (INCPDLLVL))))

;;; FCTN is either a symbol which is the name of a function
;;; or it is a list which can be used as a source address in an instruction.
;;; MAPPING-TABLE, if not NIL, is an expression whose value is a flavor mapping table;
;;; we compile code to compute that table and put it in SELF-MAPPING-TABLE.
(DEFUN P2ARGC (FCTN ARGL DESC DEST TARGET &OPTIONAL MAPPING-TABLE)   ;see also P2ARGC-FOR-K
  (LET (COUNT TOKEN-LIST AG1 DSC1 IDEST CALLI TM TDEST LDEST
        RESTART-PC ADI-LIST
        (MVTARGET *M-V-TARGET*)
        (*CALL-BLOCK-PDL-LEVELS* *CALL-BLOCK-PDL-LEVELS*))
    (PROG ()
          ;; Whatever our caller wants in the way of multiple values,
          ;; we will do it for him.  Say so.
          (SETQ *M-V-TARGET* NIL)
          (SETQ IDEST 'D-NEXT)
          (SETQ CALLI (COND ((NULL ARGL) 'CALL0)
                            (T 'CALL)))
          (SETQ LDEST (SETQ TDEST DEST))        ;May get changed to D-PDL below
;;TDEST is destination actually to be compiled into CALL instruction.
;;LDEST is "logical" destination.  This is usually the same except in case of multiple values.
;; Then TDEST is assembled D-IGNORE (it is actually ignored by the microcode, but doing
;; this confuses the micro-compiler least), while LDEST is D-PDL, reflecting the fact that the
;; values actually show up on the PDL.
          (COND ((NULL MVTARGET))
                ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
                 (SETQ ADI-LIST
                       (CONS MVTARGET (CONS NIL ADI-LIST)))
                 (SETQ TDEST 'D-IGNORE LDEST 'D-PDL))
                ((EQ MVTARGET 'THROW)
                 (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR 'NIL) . ,ADI-LIST)
                       TDEST 'D-PDL LDEST 'D-PDL))
                ((EQ MVTARGET 'RETURN)
                 (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR 'T) . ,ADI-LIST)
                       TDEST 'D-PDL LDEST 'D-PDL))
                ((NUMBERP MVTARGET)
                 ;; MVTARGET is a number => it is number of values,
                 ;; just leave them on the stack.
                 (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR ',MVTARGET) . ,ADI-LIST)
                       TDEST 'D-IGNORE LDEST 'D-PDL)))
          (SETQ AG1 ARGL)
          (SETQ DSC1 DESC)
          (COND ;; Use of FEXPR-CALL turned on 11/16/82.
                ((AND (MEMQ 'FEF-ARG-REST (SETQ TM (CADAR (LAST DSC1))))
                      (MEMQ 'FEF-QT-QT TM))
                 (SETQ CALLI 'CALL)
                 (SETQ ADI-LIST (CONS 'FEXPR-CALL
                                      (CONS NIL ADI-LIST)))))
          (COND ((NOT (SYMBOLP FCTN))
                 (SETQ TM FCTN))                ;Non-symbolic fctn; it's address for CALL
                (T (SETQ TM `(QUOTE-VECTOR (FUNCTION ,TARGET)))))
          (COND ((NULL ADI-LIST)
                 (OUTI (LIST CALLI TDEST TM)))
                (T (OUTI1 (LIST 'ADI-CALL CALLI TDEST TM ADI-LIST))
                   (MKPDLLVL (+ PDLLVL (LENGTH ADI-LIST)))))
          (COND ((NULL MVTARGET))
                ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
                 (INCPDLLVL))
                ((NUMBERP MVTARGET)
                 (MKPDLLVL (+ PDLLVL MVTARGET))))
          (PUSH PDLLVL *CALL-BLOCK-PDL-LEVELS*)
          (MKPDLLVL (+ 4 PDLLVL))
       L4
          (COND ((NULL DSC1) (GO X2)))
          (SETQ COUNT (CAAR DSC1))
          (SETQ TOKEN-LIST (CADAR DSC1))
          (COND ((MEMQ 'FEF-ARG-REST TOKEN-LIST)
                 (SETQ COUNT #o1005)))
       L3
          (AND (NULL (CDR AG1)) (NULL MAPPING-TABLE)
               (SETQ IDEST 'D-LAST))
          (COND ((= 0 COUNT) (SETQ DSC1 (CDR DSC1)) (GO L4))
                ;; The following clause works with the FEXPR-CALL ADI
                ;; and was turned on 11/16/82.
                ((AND (MEMQ 'FEF-ARG-REST TOKEN-LIST)
                      (MEMQ 'FEF-QT-QT TOKEN-LIST))
                 (GO OFEXPR))   ;DO THIS EVEN IF ARG LIST IS NULL
                ((NULL AG1) (GO RET))   ;OUT OF ARG LIST
                ((MEMQ 'FEF-QT-QT TOKEN-LIST)
                 (OUTI `(MOVE ,IDEST (QUOTE-VECTOR (QUOTE ,(CAR AG1))))))
                ((MEMQL '(FEF-QT-EVAL FEF-QT-DONTCARE) TOKEN-LIST)
                 (COND ((AND (NULL (CDR AG1))
                             (MEMQ 'LEXPR-FUNCALL TOKEN-LIST))
                        (P2 (CAR AG1) 'D-PDL)   ;Arg to %SPREAD
                        (OUTI (LIST 'MISC IDEST '%SPREAD)))
                       (T (P2 (CAR AG1) IDEST))))
                (T (BARF TOKEN-LIST
                         'TOKEN-LIST-LOSES-P2
                         'BARF)))
          (INCPDLLVL)
          (SETQ AG1 (CDR AG1))
          (DECF COUNT)
          (GO L3)
       X2
          (COND (AG1 (SETQ DSC1 '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL))))  ;Compile the rest
                     (GO L4)))                  ;of them; he may possibly know what he's doing
       RET
          (COND (MAPPING-TABLE
                 (P2PUSH MAPPING-TABLE)
                 (OUTI '(MISC D-LAST %SET-SELF-MAPPING-TABLE))))
          (COND (RESTART-PC
                 (SETQ *DROPTHRU* T)
                 (OUTF (LIST 'RESTART-TAG RESTART-PC))))
          (COND ((NULL MVTARGET))
                ((EQ MVTARGET 'MULTIPLE-VALUE-LIST))
                ((EQ MVTARGET 'THROW)
                 (RETURN NIL))
                ((EQ MVTARGET 'RETURN)
                 (RETURN NIL))
                ((NUMBERP MVTARGET) (RETURN NIL)))
          (COND ((NOT (EQ LDEST DEST))          ;Interested in where value is, not what was
                 (MOVE-RESULT-FROM-PDL DEST)))  ;assembled into call
          (COND ((AND (EQ DEST 'D-RETURN)
                      (NULL RESTART-PC))
                 (TAKE-DELAYED-TRANSFER)))
          (RETURN NIL)

       OFEXPR
          (OUTI `(MOVE D-LAST (QUOTE-VECTOR (QUOTE ,AG1))))
          (GO RET)
          )))

(DEFUN (:PROPERTY *CATCH P2) (ARGL DEST)
  (LET (TDEST   ;TDEST is destination actually to be compiled into call instruction.
        (INITIAL-PDLLVL PDLLVL)
        RESTART-PC ADI-LIST
        (*CALL-BLOCK-PDL-LEVELS* *CALL-BLOCK-PDL-LEVELS*)
        (*WITHIN-CATCH* T))
    (COND ((NULL *M-V-TARGET*)
           (IF (EQ DEST 'D-IGNORE)
               (SETQ TDEST 'D-IGNORE)
             (IF (AND (EQ DEST 'D-RETURN)
                      (NOT GENERATING-MICRO-COMPILER-INPUT-P))
                 (SETQ TDEST 'D-RETURN)
               (SETQ TDEST 'D-PDL))))
          ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
           (SETQ ADI-LIST
                 (CONS *M-V-TARGET* (CONS NIL ADI-LIST)))
           (SETQ TDEST 'D-IGNORE))
          ((EQ *M-V-TARGET* 'THROW)
           (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR 'NIL) . ,ADI-LIST)
                 TDEST 'D-PDL))
          ((EQ *M-V-TARGET* 'RETURN)
           (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR 'T) . ,ADI-LIST)
                 TDEST 'D-PDL))
          ((NUMBERP *M-V-TARGET*)
           ;; *M-V-TARGET* is a number => it is number of values,
           ;; just leave them on the stack.
           (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR (QUOTE ,*M-V-TARGET*)) . ,ADI-LIST)
                 TDEST 'D-IGNORE)))
    (SETQ ADI-LIST `(RESTART-PC (QUOTE-VECTOR (TAG ,(SETQ RESTART-PC (GENSYM))))
                     BIND-STACK-LEVEL ()
                     . ,ADI-LIST))
    (OUTI1 `(ADI-CALL CALL ,TDEST (QUOTE-VECTOR (FUNCTION *CATCH)) ,ADI-LIST))
    (MKPDLLVL (+ PDLLVL (LENGTH ADI-LIST)))
    (COND ((NULL *M-V-TARGET*))
          ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
           (INCPDLLVL))
          ((NUMBERP *M-V-TARGET*)
           (MKPDLLVL (+ PDLLVL *M-V-TARGET*))))
    (PUSH PDLLVL *CALL-BLOCK-PDL-LEVELS*)
    (MKPDLLVL (+ 4 PDLLVL))
    (P2 (CAR ARGL) 'D-NEXT)
    (INCPDLLVL)
    (COND ((EQ DEST 'D-RETURN)
           (P2 (CADR ARGL) 'D-RETURN))
          ((NULL *M-V-TARGET*)
           (P2 (CADR ARGL) 'D-LAST))
          ((EQ *M-V-TARGET* 'THROW)
           (P2PUSH-CONSTANT (- PDLLVL INITIAL-PDLLVL))
           (OUTI `(MISC D-PDL PDL-WORD))
           (UNLESS (P2MV (CADR ARGL) 'D-PDL *M-V-TARGET*)
             (POPPDL 1 (- PDLLVL INITIAL-PDLLVL -1))
             (SETQ *M-V-TARGET* NIL)))
          ((eq *m-v-target* 'multiple-value-list)
           ;; Have to deal with normal forms that return multiple values
           ;; (pop extra stuff, note values obtained) and with forms
           ;; that don't return multiple values (pop extra stuff, make
           ;; value on stack into one element list, not values obtained).
           ;; Example: (MULTIPLE-VALUE-LIST (CATCH 'FOO (+ X Y)))
           (let ((flag (p2mv (cadr argl) 'd-pdl *m-v-target*)))
             (poppdl 1 (- pdllvl initial-pdllvl))
             (when (not (null flag))
               (outf '(misc d-pdl ncons)))
             (setq *m-v-target* ())))
          (T
           (UNLESS (P2MV (CADR ARGL) 'D-PDL *M-V-TARGET*)
             (POPPDL (IF (NUMBERP *M-V-TARGET*) *M-V-TARGET* 1)
                     (- PDLLVL INITIAL-PDLLVL))
             (SETQ *M-V-TARGET* NIL))))
    (UNLESS (EQ DEST 'D-RETURN)
      (SETQ *DROPTHRU* T))
    (OUTF (LIST 'RESTART-TAG RESTART-PC))
    (WHEN (null *m-v-target*)
      (UNLESS (OR (MEMQ DEST '(D-PDL D-IGNORE))
                  (AND (EQ DEST 'D-RETURN)
                       (EQ TDEST 'D-RETURN)))
        (MOVE-RESULT-FROM-PDL DEST)))))

;;; Bind a list of variables, computing initializations and binding sequentially.
;;; *VARS* are the *VARS* outside of this binding environment.
;;; NEWVARS are the *VARS* inside of it, starting with the variables in X in reverse order,
;;; except there may be additional entries for optional-specified-flags; each one
;;; will be on NEWVARS just before its corresponding main variable.
;;; We have to install these variables one at a time as we go, using successive tails.
(DEFUN P2SBIND (X NEWVARS *VARS*)
  (LET ((NBINDS 0)                              ;Number of (internal-aux) special bindings
        (NNEWVARS (LOOP FOR L ON NEWVARS UNTIL (EQ L *VARS*) COUNT T)))
    (DO ((X X (CDR X)) (HOME))
        ((NULL X))
      (SETQ HOME (NTH (1- NNEWVARS) NEWVARS))
      (WHEN (P2LMB (CAR X) HOME)
        (INCF NBINDS))
      ;; Set *VARS* to the tail of NEWVARS starting at the variable we just handled
      ;; or its optional-specified-flag.
      (SETQ NNEWVARS (1- NNEWVARS))
      (AND (CDDR (VAR-INIT HOME)) (SETQ NNEWVARS (1- NNEWVARS)))
      (SETQ *VARS* (NTHCDR NNEWVARS NEWVARS)))
    (OR (ZEROP NNEWVARS) (BARF X "VARS screwed up by this binding" 'BARF))
    NBINDS))

;;; Output code for binding the var VARNAME as specified in its HOME.
;;; Return T if a BINDPOP or BINDNIL instruction was output.
(DEFUN P2LMB (VARNAME HOME &AUX INTCODE INITFORM)
  (BLOCK NIL
    (WHEN (NOT (ATOM VARNAME))
      (SETQ INITFORM (CADR VARNAME))
      (SETQ VARNAME (CAR VARNAME)))
    (UNLESS (EQ (VAR-NAME HOME) VARNAME)
      (BARF VARNAME "wrong home in P2LMB" 'BARF))
    (SETQ INTCODE (VAR-INIT HOME))
    ;; If this variable's binding is fully taken care of by function entry,
    ;; we have nothing to do here.
    (COND ((NOT (MEMQ (CAR INTCODE) '(FEF-INI-OPT-SA FEF-INI-COMP-C)))
           (RETURN NIL)))
    ;; Detect and handle internal special bound variables.
    (COND ((AND (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX)
                (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE)))
           ;; Output BINDNIL, or push value and BINDPOP.
           (COND ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
                  (OUTIV 'BINDNIL HOME))
                 (T (P2PUSH INITFORM)
                    (OUTIV 'BINDPOP HOME)))
           (RETURN T)))
    ;; Otherwise, it's an internal local variable,
    ;; or else a special variable already bound by entering the function.
    ;; Don't bind, just init.
    (COND ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
           ;; if initting to NIL, then if no tags output so far (*TAGOUT* is NIL)
           ;; we can assume it is still NIL from function entry time.
           (COND ((OR *TAGOUT*
                      (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE))
                      (VAR-OVERLAP-VAR HOME))
                  (OUTIV 'SETNIL HOME))))
          ;; If explicitly says value does not matter, do nothing to initialize.
          ((EQUAL INITFORM '(UNDEFINED-VALUE))
           NIL)
          ;; Initting var to itself;  do nothing.
          ((AND (EQ (VAR-TYPE HOME) 'FEF-REMOTE)
                (EQ INITFORM VARNAME)))
          ((EQUAL INITFORM ''0)
           (OUTIV 'SETZERO HOME))
          (T (P2PUSH INITFORM)
             ;;If &OPTIONAL and for micro-compiler, just leave variable on stack.
             (COND ((AND GENERATING-MICRO-COMPILER-INPUT-P
                         (EQ (CAR INTCODE) 'FEF-INI-OPT-SA)))
                   (T (OUTIV 'POP HOME)))))
    ;; If there is a specified-flag variable, it was bound to T at entry.
    ;; Set it to NIL here (ie, if the arg was NOT specified).
    (COND ((CDDR INTCODE)
           (OUTIV 'SETNIL (CDDR INTCODE))))
    (COND ((EQ (CAR INTCODE) 'FEF-INI-OPT-SA)
           (PUTPROP (CADR INTCODE) T 'PEEP-KEEP)
           (OUTF (CADR INTCODE))))
    (RETURN NIL)))

(DEFUN OUTIV (INST VARAB)
  (OUTI (LIST INST 0 (VAR-LAP-ADDRESS VARAB))))

;;; Bind a list of variables "in parallel":  compute all values, then bind them all.
;;; Return the number of special bindings made (BINDPOP and BINDNIL instructions).
;;; Note: an attempt to bind NIL is ignored at this level.
;;; Note: if several variables have init forms of (%pop),
;;; they are popped off the pdl LAST ONE FIRST!
;;; The "correct" thing would be to pop the first one first,
;;; but this would require another stack to keep them on to reverse them.
(DEFUN P2PBIND (VARNAMES NEWVARS)
  (LET ((PDLLVL PDLLVL)
        VARNAME HOME INTCODE INITFORM NBINDS)
    (BLOCK NIL
      (OR VARNAMES (RETURN 0))
      (SETQ VARNAME (CAR VARNAMES)
            VARNAMES (CDR VARNAMES))
      (WHEN (NOT (ATOM VARNAME))
        (SETQ INITFORM (CADR VARNAME))
        (SETQ VARNAME (CAR VARNAME)))
      ;; If trying to bind NIL, just discard the value to bind it to.
      (WHEN (NULL VARNAME)
        (P2 INITFORM 'D-PDL)
        (RETURN (PROG1 (P2PBIND VARNAMES NEWVARS)
                       (OUTF '(MOVE D-IGNORE PDL-POP)))))
      (WHEN (NULL (SETQ HOME (find VARNAME NEWVARS :key #'var-name)))
        (BARF VARNAME 'NOT-ON-VARS 'BARF))
      (SETQ INTCODE (VAR-INIT HOME))
      ;; If this variable's binding is fully taken care of by function entry,
      ;; we have nothing to do here.
      (IF (AND (NOT (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX))
               (NOT (MEMQ (CAR INTCODE) '(FEF-INI-OPT-SA FEF-INI-COMP-C))))
          (RETURN (P2PBIND VARNAMES NEWVARS)))
      ;; Detect and handle internal special bound variables.
      (WHEN (AND (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX)
                 (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE)))
        ;; Output a BIND, or BINDNIL, or push value and BINDPOP.
        (COND ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
               (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
               (OUTIV 'BINDNIL HOME))
              (T
               (P2PUSH INITFORM)
               (INCPDLLVL)
               (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
               (OUTIV 'BINDPOP HOME)))
        (RETURN (1+ NBINDS)))
      (COND ((EQUAL INITFORM '(UNDEFINED-VALUE))
             (SETQ NBINDS (P2PBIND VARNAMES NEWVARS)))
            ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
             (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
             (COND ((OR *TAGOUT*
                        (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE))
                        (VAR-OVERLAP-VAR HOME))
                    (OUTIV 'SETNIL HOME))))
            ;; Special vars bound at function entry and wanting to be
            ;; initted to themselves, need not be set at all.
            ((AND (EQ (VAR-TYPE HOME) 'FEF-SPECIAL)
                  (EQ INITFORM VARNAME))
             (SETQ NBINDS (P2PBIND VARNAMES NEWVARS)))
            (T (P2PUSH INITFORM)
               (INCPDLLVL)
               (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
               ;;If for micro-compiler and is optional arg, just leave variable on stack.
               (UNLESS (AND GENERATING-MICRO-COMPILER-INPUT-P
                            (MEMQ 'FEF-INI-OPT-SA INTCODE))
                 (OUTIV 'POP HOME))))
      (COND ((EQ (CAR INTCODE) 'FEF-INI-OPT-SA)
             (PUTPROP (CADR INTCODE) T 'PEEP-KEEP)
             (OUTF (CADR INTCODE))))
      (RETURN NBINDS))))

;;; Compile code to test CONDITION and jump to tag if it is NIL
;;; (for SENSE = TRUE) or if it is non-NIL (for SENSE = FALSE).
(DEFUN BOOL1 (CONDITION SENSE TAG)
  (P2BRANCH CONDITION 'D-INDS
            `(BRANCH NILIND ,SENSE NIL ,TAG)))

;;; Like P2, but also supply a "branch destination".
;;; The branch destination (*BDEST*) is just a branch instruction which
;;; could simple-mindedly be compiled right after (P2 FORM DEST),
;;; but some forms can optimize the code produced by incorporating
;;; the branch destination into their code.  Such forms can say that
;;; outputting the branch at the end is superfluous by setting *BDEST* to NIL.
;;; Forms which perform unconditional transfers need not worry about *BDEST*
;;; since it will be output and then discarded as unreachable.

;;; An unconditional branch destination can accompany any value of DEST.
;;; A conditional branch should only be used with DEST = D-INDS.
;;; This is taken to imply that the indicators are used by the branch,
;;; not that the indicators will be correctly set up after the optimized
;;; code is finished branching or not.  If you wish to compile something
;;; and want the indicators correctly set up according to its value,
;;; you should use D-INDS with no *BDEST*, and do your branching yourself.

;;; Branches which pop the pdl may not be used as branch destinations.
;;; Most people who look at *BDEST* don't check for them,
;;; and the optimizations that *BDEST* is used for wouldn't work for them anyway.

;;; A funny kind of branch that can be used as a destination is
;;; (BRANCH ALWAYS NO-OP NIL tag).  It is a sort of unconditional branch,
;;; used when the tag to be branched to is known to be right after
;;; this expression, so that one might think that no branch is needed at all.
;;; When OUTB is called on such a branch, it does nothing.
;;; But some functions (such as AND and OR) can optimize these no-op branches
;;; like any other unconditional branches.

;;; An even funnier kind of branch destination is the return branch:
;;; (BRANCH ALWAYS RETURN NIL tag).  This is given as the branch destination
;;; to the last statement in a PROG, so that if the statement is a RETURN
;;; then the implicit (RETURN NIL) at the end of the PROG can be omitted
;;; and the RETURN at the end can just drop through to the PROG's rettag.
;;; Return branch destinations may not be passed along to subexpressions
;;; by AND, OR and COND.

(DEFUN P2BRANCH (FORM DEST *BDEST*)
  (AND (MEMQ DEST '(D-PDL D-NEXT))
       (NEEDPDL 1))
  (COND ((AND *BDEST* (NEQ (CADR *BDEST*) 'ALWAYS)
              (NEQ DEST 'D-INDS))
         (BARF `(,DEST . ,*BDEST*) "*BDEST* is conditional and DEST is not D-INDS" 'BARF))
        ;; We can optimize things like (AND 'T (GO FOO)) and (AND 'NIL (GO FOO))
        ;; into an unconditional jump or into nothing at all.
        ((AND (EQ (CADR *BDEST*) 'NILIND)
              (NULL (CADDDR *BDEST*))
              (NOT (ATOM FORM))
              (EQ (CAR FORM) 'QUOTE))
         (AND (EQ (NULL (CADR FORM))
                  (EQ (CADDR *BDEST*) 'TRUE))
              (OUTB `(BRANCH ALWAYS NIL . ,(COPY-LIST (CDDDR *BDEST*)))))
         (SETQ *BDEST* NIL))
        ((ADRREFP FORM)
         (OR (EQ DEST 'D-IGNORE)
             (OUTI `(MOVE ,DEST ,(P2-SOURCE FORM DEST)))))
        ((EQ (CAR FORM) 'LEXICAL-REF)
         (P2 FORM DEST))
        ((memq (CAR FORM) '(%POP %pop-for-with-stack-list))
         (P2 FORM DEST))
        (T (LET ((*M-V-TARGET* NIL))
             (P2F FORM DEST))))
  (AND *BDEST* (OUTB (COPY-LIST *BDEST*))))

;;; A call to ATOM which is then tested by a branch-if-non-nil, etc.,
;;; can be turned into just a branch-if-atom, etc.
(DEFUN (:PROPERTY ATOM P2) (ARGL DEST)
  (IF (EQ (CADR *BDEST*) 'NILIND)
      (LET ((SENSE (OTHER (CADDR *BDEST*))))
        (P2BRANCH (CAR ARGL) DEST `(BRANCH ATOMIND ,SENSE . ,(CDDDR *BDEST*)))
        (SETQ *BDEST* NIL))
    (P2MISC 'ATOM ARGL DEST 1)))

;;; NOT compiles into a misc insn normally,
;;; but with a branch destination, it optimizes away by inverting the condition.
(DEFUN (:PROPERTY NOT P2) (ARGL DEST)
  (IF (OR (EQ (CADR *BDEST*) 'NILIND)
          (EQ (CADR *BDEST*) 'ATOMIND))
      (LET ((SENSE (OTHER (CADDR *BDEST*))))
        (P2BRANCH (CAR ARGL) DEST `(BRANCH ,(CADR *BDEST*) ,SENSE . ,(CDDDR *BDEST*)))
        (SETQ *BDEST* NIL))
    (P2MISC 'NOT ARGL DEST 1)))

(DEFUN OTHER (SENSE)
  (COND ((EQ SENSE 'TRUE) 'FALSE)
        ((EQ SENSE 'FALSE) 'TRUE)
        (T (BARF SENSE 'BAD-ARG-OTHER 'BARF))))

(DEFPROP AND P2ANDOR P2)
(DEFPROP OR P2ANDOR P2)
(DEFUN P2ANDOR (ARGL DEST)
  (LET ((SENSE (IF (EQ *P2FN* 'AND) 'TRUE 'FALSE)))
    (when (and (null *m-v-target*)
               (memq dest '(0 d-inds d-ignore)))
      ;; compiling for predicate or effect
      (DO ()
          ((NOT (EQUAL (CAR (LAST ARGL))
                       (IF (EQ SENSE 'TRUE) ''T ''NIL))))
        (SETQ ARGL (BUTLAST ARGL))))
    ;; RETURN branches can't be passed in to the last thing in an AND.
    (AND (EQ (CADR *BDEST*) 'ALWAYS)
         (EQ (CADDR *BDEST*) 'RETURN)
         (SETQ *BDEST* NIL))
    ;; Any non-null constant as arg in an AND is ignorable unless it is last.
    ;; NIL as arg in an OR is always ignorable.
    (COND ((NULL ARGL))
          ((EQ SENSE 'FALSE)
           (SETQ ARGL (ZL:DELETE ''NIL ARGL)))
          (T
           (SETQ ARGL (NREVERSE (CONS (CAR (LAST ARGL))
                                      (DEL (LAMBDA (IGNORE X)
                                             (AND (EQ (CAR-SAFE X) 'QUOTE)
                                                  (CADR X)))
                                           NIL
                                           (CDR (NREVERSE ARGL))))))))
    (WHEN (NULL ARGL)
      (RETURN-FROM P2ANDOR (PROG1 (P2BRANCH `',(EQ SENSE 'TRUE) DEST *BDEST*)
                                  (SETQ *BDEST* NIL))))
    ;; If we are going to jump somewhere unconditionally after the AND,
    ;; things which are NIL might as well jump conditionally straight there.
    ;; But this only works if the value of the AND will be in the right place then.
    (MULTIPLE-VALUE-BIND (TAG UNCONDITIONAL)
        (IF (AND (EQ (CADR *BDEST*) 'ALWAYS)
                 (NULL *M-V-TARGET*)
                 (MEMQ DEST '(D-PDL D-INDS D-IGNORE 0)))
            (VALUES (CAR (CDDDDR *BDEST*)) T)
            (VALUES (GENSYM) NIL))
      (LET (TAG1)
        (COND ((AND (NULL *M-V-TARGET*) (EQ DEST 'D-IGNORE))
               ;; Compilation strategy for AND for effect:
               ;; compute each arg, using it only to jump to end if it's NIL.
               ;; The last one we just ignore, but we feed it our *BDEST* for
               ;; branch tensioning.  However, (AND form (GO tag)) can be optimized
               ;; by making it a conditional jump to tag rather than a jump around a jump.
               (DO ((ARGL ARGL (CDR ARGL)))
                   ((NULL (CDR ARGL))
                    (P2BRANCH (CAR ARGL) DEST *BDEST*))
                 (AND (SIMPLEGOP (CADR ARGL))
                      (RETURN (BOOL1 (CAR ARGL) (OTHER SENSE) (GTAG (CADADR ARGL)))))
                 ;; If the next arg of this AND is NIL, this arg is effectively last.
                 ;; However, if AND has a branch destination, it must compute
                 ;; whether to branch based on the NIL, not on this arg.
                 (AND (EQ (CAR-SAFE (CADR ARGL)) 'QUOTE)
                      (EQ (NULL (CADADR ARGL))
                          (EQ SENSE 'TRUE))
                      (RETURN (P2BRANCH (CAR ARGL) DEST *BDEST*)))
                 (BOOL1 (CAR ARGL) SENSE TAG)))
              ((AND (NULL *M-V-TARGET*) (EQ (CADR *BDEST*) 'NILIND))
               ;; Compilation strategy for AND followed by jump if NIL:
               ;; jump compute each value and jump THERE rather than to end if NIL.
               ;; Compilation strategy for AND followed by jump if not NIL:
               ;; put that jump if not NIL after the last thing in the AND
               ;; and go to after that if anything else fails to be non-NIL.
               (IF (EQ SENSE (CADDR *BDEST*))
                   (DO ((ARGL ARGL (CDR ARGL)))
                       ((NULL ARGL))
                     (P2BRANCH (CAR ARGL) DEST *BDEST*))
                   (DO ((ARGL ARGL (CDR ARGL)))
                       ((NULL (CDR ARGL))
                        (P2BRANCH (CAR ARGL) DEST *BDEST*))
                     ;; If the next arg of this AND is NIL, this arg is effectively last.
                     ;; Also, *BDEST* can be flushed since it says branch if
                     ;; not NIL and we now know the value of the AND is always NIL.
                     (AND (NOT (ATOM (CADR ARGL)))
                          (EQ (CAADR ARGL) 'QUOTE)
                          (EQ (NULL (CADADR ARGL))
                              (EQ SENSE 'TRUE))
                          (RETURN (P2 (CAR ARGL) DEST)))
                     (BOOL1 (CAR ARGL) SENSE TAG)))
               (SETQ *BDEST* NIL))
              (T
               ;; Compilation strategy for AND for value
               ;; (correct indicators required counts as for value):
               ;; compile each arg, jumping to end if NIL.
               ;; Compile them to indicators, or to pdl and pop if NIL.
               ;; If compiling to indicators (no pushing), we can optimize
               ;; (AND form (GO tag)) just as when we are ignoring the value.
               (let ((idest (if (eq dest 'd-inds) 'd-inds 'd-pdl)))
                 ;; AND for multiple values is like AND for value on the stack,
                 ;; except that we can pass the *M-V-TARGET* along to the last form.
                 ;; Also, after the "end" where the failure branches branch to
                 ;; we put code to push N-1 extra NILs, or whatever.
                 ;; The code for the last form jumps around that, to the tag TAG1.
                 (IF *M-V-TARGET* (SETQ IDEST 'D-PDL))
                 (DO ((ARGL ARGL (CDR ARGL))
                      (BRANCH `(BRANCH NILIND ,SENSE ,(NEQ DEST 'D-INDS) ,TAG)))
                     ((NULL (CDR ARGL))
                      ;; Compile the last form.  If we want multiple values
                      ;; and it handles them, then say the AND is handling them.
                      (COND (*M-V-TARGET*
                             (IF (NULL (P2MV (CAR ARGL) IDEST *M-V-TARGET*))
                                 (SETQ TAG1 (GENSYM))))
                            (UNCONDITIONAL
                             (P2BRANCH (CAR ARGL) DEST *BDEST*)
                             (SETQ *BDEST* NIL))
                            (T
                             (P2 (CAR ARGL)
                                 (IF (AND (EQ DEST 'D-RETURN)
                                          (NOT GENERATING-MICRO-COMPILER-INPUT-P))
                                     ;;Ok to distribute down a D-RETURN, since
                                     ;; it is an implict transfer
                                     DEST
                                     ;;Compile to IDEST, since going to fall into common point
                                     ;; which expects result there
                                     IDEST)))))
                   (P2 (CAR ARGL) IDEST)
                   (AND (EQ IDEST 'D-INDS)
                        (SIMPLEGOP (CADR ARGL))
                        (RETURN (OUTB `(BRANCH NILIND ,(OTHER SENSE) NIL ,(GTAG (CADADR ARGL))))))
                   (OUTB (COPY-LIST BRANCH))))))
        (COND (TAG1
               ;; If we want multiple values, and the last form provides them,
               ;; say that the AND provides them,
               ;; and arrange to produce some in every other path.
               (OUTB `(BRANCH ALWAYS NIL NIL ,TAG1))    ;Last form jumps around.
               (OUTTAG TAG)                     ;Other paths come here.
               (COND ((NUMBERP *M-V-TARGET*)    ;Turn single value into N values,
                      (DOTIMES (I (1- *M-V-TARGET*))
                        (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
                     ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)    ;or into a list of values.
                      (OUTF '(MISC D-PDL NCONS))))
               (SETQ *M-V-TARGET* NIL)
               (OUTTAG TAG1))                   ;Last form jumps here.
              ((NOT UNCONDITIONAL)
               (OUTTAG TAG)
               (OR (EQ DEST 'D-IGNORE)
                   (EQ DEST 'D-INDS)
                   (MOVE-RESULT-FROM-PDL DEST))))
        NIL))))

;;; Return T if given a (GO tag) which could be done with just a branch
;;; (doesn't require popping anything off the pdl).
(DEFUN SIMPLEGOP (FORM)
  (AND (EQ (CAR-SAFE FORM) 'GO)
       (not (unbinds-needed-p (second form)))
       (= PDLLVL (GPDLLVL (second FORM)))))

(defun unbinds-needed-p (tag)
  (let ((gotag (find tag *gotag-environment* :key #'gotag-prog-tag)))
    (when gotag
      (let ((progdesc (gotag-progdesc gotag)))
        (loop for desc in *progdesc-environment*
              thereis (not (zerop (progdesc-nbinds desc)))
              until (eq progdesc desc))))))

(defun (:property casen p2) (argl dest)
  (let ((limit (1- (length argl))))
    ;;Push the Limit minus one
    (p2push-constant (1- limit))
    ;;Push the arg.
    (p2 (car argl) 'd-pdl)
    (outf '(misc d-ignore %nway-branch))
    (let ((tags '())
          (endtag (gensym)))
      (dotimes (n limit)
        (let ((tag (gensym)))
          (push tag tags)))
      (dolist (tg tags)
        ;; Kludge:
        ;; output a random restart-tag to keep the peephole
        ;; optimizer from deleting these jumps. - GJC
        ;; Kludge 2:
        ;; Don't use outb on the branch because two consecutive
        ;; branch instructions are on the fly optimized. -JRM
        (outf `(restart-tag ,(gensym)))
        (outf `(branch always nil nil ,tg)))
      (do ((tg tags (cdr tg))
           (form (cdr argl) (cdr form)))
          ((null form) nil)
        (outf (car tg))
        (p2 (car form) 'd-pdl)
        (outf `(branch always nil nil ,endtag)))
      (outf endtag)
      (move-result-from-pdl dest))))


(DEFUN (:PROPERTY COND P2) (ARGL DEST)  ;see also P2CROSS
  (PROG (CLAUSE TAG TAG1 TAG2 VALF CLAUSE-LENGTH TM IDEST PRED NOFALLTHRU
         LAST-CLAUSE-FLAG IDEST-USED)
        (SETQ TAG2 (GENSYM))                    ;Tag to go to with value of COND in DEST
        (SETQ TAG (GENSYM))                     ;Tag to go to with value of COND in IDEST
        ;; Choose an intermediate destination, depending on ultimate destination.
        ;; The intermediate destination can match the ultimate one
        ;; if they are D-IGNORE, D-INDS or D-PDL.
        ;; Each COND clause can compile its value to IDEST and go to TAG
        ;; or compile its value to DEST and go to TAG2.

        ;; Use of TAG and IDEST assumes that multiple values were NOT generated
        ;; whereas TAG2 and DEST assumes that they were if they are supposed to be.

        ;; For microcompiler input, we always use TAG and IDEST unless IDEST=DEST.
        ;; Otherwise, we usually use DEST except for clauses that are just predicates.

        ;; IDEST-USED is T if a clause has compiled its result to IDEST.
        ;; The code to move the value is only generated if IDEST/TAG has been used.
        (AND *M-V-TARGET* (SETQ DEST 'D-PDL))
        (SETQ IDEST 'D-IGNORE)
        (COND ((NOT (EQ DEST 'D-IGNORE))
               (SETQ VALF T)
               (SETQ IDEST 'D-PDL)))
        (IF (EQ DEST 'D-INDS) (SETQ IDEST 'D-INDS))

        ;; Compile next clause.
     L1
        (IF (NULL (CDR ARGL)) (SETQ LAST-CLAUSE-FLAG T))
        (SETQ CLAUSE (CAR ARGL))
        (AND LAST-CLAUSE-FLAG (NULL (CDR CLAUSE))
             (SETQ CLAUSE (CONS ''T CLAUSE)))
        (SETQ TAG1 (GENSYM))
        (SETQ PRED (CAR CLAUSE))
        (WHEN (EQ (CAR-SAFE PRED) 'QUOTE)
          (COND ((NULL (CADR PRED))     ;Is the null condition?
                 (AND (NOT LAST-CLAUSE-FLAG)
                      (GO L5)))         ;Yep. Can happen as result of DO expansion.
                ((CDR ARGL)             ;condition always true?
                 (SETQ LAST-CLAUSE-FLAG T)      ;If so, discard any remaining clauses
                 (SETQ NOFALLTHRU T)    ;after a warning about them.
;These can come from expanding DEFSUBSTs that contain CONDs, with constant arguments.
;                (WARN 'UNREACHABLE-CODE ':IMPLAUSIBLE
;                      "Some COND clauses are unreachable;
; the first starts with ~S."
;                (CAADR ARGL))
                 (SETQ ARGL (LIST CLAUSE)))
                (T
                 (SETQ NOFALLTHRU T))))
        (SETQ CLAUSE-LENGTH (LENGTH CLAUSE))
        ;; Handle certain special cases of clauses.
        (COND ((AND VALF (= 1 CLAUSE-LENGTH))
               ;; Clause containing only one element, compiled for value.
               ;; value of condition is also value of clause.
               (P2 PRED IDEST)
               (SETQ IDEST-USED T)
               ;; If something pushed, pop if the branch is not taken
               (OUTB `(BRANCH NILIND FALSE ,(EQ IDEST 'D-PDL) ,TAG))
               (GO L5))
              ;; Clause of one element, if value is not wanted.
              ((= 1 CLAUSE-LENGTH) (BOOL1 PRED 'FALSE TAG) (GO L5))
              ;; Clause is just condition followed by a GO.
              ((AND (= 2 CLAUSE-LENGTH)
                    (SIMPLEGOP (CADR CLAUSE)))
               (BOOL1 PRED 'FALSE (GTAG (CADADR CLAUSE)))
               (GO L5))
              ;; Clause after this one is (T (GO ...)).
              ;; Can get special handling only if the GO
              ;; requires no pdl adjustment.
              ((AND (NOT NOFALLTHRU)            ;Isolate case of ((P1 A1) (T (GO X)))
                    (NOT LAST-CLAUSE-FLAG)
                    (NOT (ATOM (CAR (SETQ TM (CADR ARGL)))))
                    (EQ (CAAR TM) 'QUOTE)
                    (CADAR TM)
                    (= 2 (LENGTH TM))
                    (SIMPLEGOP (CADR TM)))
               ;; In effect, we turn this into (COND ((NOT P1) (GO X)) (T A1))
               (BOOL1 PRED 'TRUE (GTAG (CADADR TM)))    ;Go X directly if P1 false
               (SETQ ARGL (CONS (CONS ''T (CDR CLAUSE)) (CDDR ARGL)))
               (GO L1))
              ((NOT NOFALLTHRU)                 ;Normal COND clause.
               (BOOL1 PRED 'TRUE TAG1)))        ;Jump around clause if predicate fails.

        ;; If the COND will have to return NIL if this clause's
        ;; condition is false, then generate a clause to return the nil.
        (WHEN (AND VALF LAST-CLAUSE-FLAG (NOT NOFALLTHRU))
          (SETQ ARGL (LIST CLAUSE '('T 'NIL)))
          (SETQ LAST-CLAUSE-FLAG NIL))

        ;; Compile the actions of the cond clause, except for the last.
        (DO ((ACTIONS (CDR CLAUSE) (CDR ACTIONS)))
            ((NULL (CDR ACTIONS))
             (SETQ CLAUSE ACTIONS))
          (P2 (CAR ACTIONS) 'D-IGNORE))

        ;; Compile last action of cond clause (the value).
        (LET ((TO-IDEST-P
                ;; Send value of last clause to IDEST rather than DEST
                ;; if that means we can avoid a branch to TAG2
                ;; that would otherwise be necessary.
                ;; Send values of all clauses to IDEST for microcompiler input.
                (OR (AND LAST-CLAUSE-FLAG
                         IDEST-USED
                         (NEQ DEST IDEST)
                         ;; Don't do this optimization if mult values wanted
                         ;; because only compilation to DEST can accept them.
                         (NULL *M-V-TARGET*)
                         ;; If D-RETURN, don't optimize, so it can propagate
                         ;; multiple values if there are any.
                         (NEQ DEST 'D-RETURN))
                    (AND GENERATING-MICRO-COMPILER-INPUT-P
                         (NOT (EQ DEST IDEST))))))
          (COND (TO-IDEST-P (P2 (CAR CLAUSE) IDEST))
                ((EQUAL (CAR CLAUSE) ''NIL)
                 ;; Avoid "Doesn't really produce multiple values"
                 ;; for internally generated 'NIL.
                 (OUTI `(MOVE ,DEST (QUOTE-VECTOR 'NIL)))
                 (AND *M-V-TARGET* (SETQ TO-IDEST-P T)))
                ((P2MV (CAR CLAUSE) DEST *M-V-TARGET*)
                 ;; If value fails to generate mult vals,
                 ;; we must make TAG generate them and go there.
                 (SETQ TO-IDEST-P T)))
          (IF (NULL TO-IDEST-P)
              (IF (OR (NULL LAST-CLAUSE-FLAG)
                      ;; If last clause, and TAG isn't the same as TAG2,
                      ;; we must still branch to TAG2.
                      (AND IDEST-USED (OR *M-V-TARGET* (NEQ DEST IDEST))))
                  (OUTB `(BRANCH ALWAYS NIL NIL ,TAG2)))
            (SETQ IDEST-USED T)
            (IF (NULL LAST-CLAUSE-FLAG)
                (OUTB `(BRANCH ALWAYS NIL NIL ,TAG)))))

        ;; Here at end of cond-clause.
     L5
        (OUTTAG TAG1)                           ;Output tag for jumps from failing predicate.
        (IF (SETQ ARGL (CDR ARGL))              ;If there are more clauses, process them.
            (GO L1))

        ;; There are no more cond clauses!
        (OUTTAG TAG)
        (AND IDEST-USED
             (COND ((NUMBERP *M-V-TARGET*)
                    (DOTIMES (I (1- *M-V-TARGET*))
                      (OUTI '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
                   ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
                    (OUTI '(MISC D-PDL NCONS)))
                   ((NEQ DEST IDEST)
                    (MOVE-RESULT-FROM-PDL DEST))))
        ;; We have generated multiple values if necessary.
        (SETQ *M-V-TARGET* NIL)
        (OUTTAG TAG2)
        (RETURN NIL)))


(defprop 1+ p2nodest-1arg p2)
(defprop 1- p2nodest-1arg p2)
(defprop 1+ 1+-to-pdl pdl-dest-inst)
(defprop 1- 1--to-pdl pdl-dest-inst)
(defun p2nodest-1arg (argl dest)
  (cond ((and (null generating-micro-compiler-input-p)
              (eq dest 'd-pdl))
         (cond ((adrrefp (car argl))
                (outi `(,(get *p2fn* 'pdl-dest-inst) ,(p2-source (car argl) dest))))
               (t (p2 (car argl) 'd-pdl)
                  (outi `(,(get *p2fn* 'pdl-dest-inst) (pdl-pop))))))
        (t (p2misc *p2fn* argl dest 1))))

(defun (:property %reference-simple-q-vector p2) (argl dest)
  (if (and (null *m-v-target*)
           (not generating-micro-compiler-input-p)
           (= (length argl) 2)
           (quotep (second argl))
           (fixnump (cadr (second argl))))
      (progn (p2push (first argl))
             (outi `(%reference-simple-q-vector ,dest ,(cadr (second argl)))))
    (p2misc 'ar-1 argl dest 2)))

(defun (:property %set-simple-q-vector p2) (argl dest)
  (if (and (null *m-v-target*)
           (not generating-micro-compiler-input-p)
           (= (length argl) 3)
           (quotep (third argl))
           (fixnump (cadr (third argl))))
      (progn (p2push (first argl))
             (p2push (second argl))
             (outi `(%set-simple-q-vector ,dest ,(cadr (third argl)))))
    (p2misc 'set-ar-1 argl dest 3)))


(defun push-arguments (argl)
  (dolist (arg argl) (p2 arg 'd-pdl)))

(defprop list p2list p2)
(defprop list-in-area p2list p2)
(defprop list* p2list p2)
(defprop list*-in-area p2list p2)
(defun p2list (argl dest &aux area)
  (when (memq *p2fn* '(list-in-area list*-in-area))
    (setq area (pop argl)))
  (push-arguments argl)
  (cond ((null generating-micro-compiler-input-p)
         (p2push-constant (length argl))
         (if area (p2 area 'd-pdl)))
        (t
         (p2push-constant (length argl))  ;this stuff might want to get changed...
         (if area (p2 area 'd-pdl))))
  (outi1 `(misc1 ,dest ,(cdr (assq *p2fn*
                                   '((list . %internal-list)
                                     (list-in-area . %internal-list-in-area)
                                     (list* . %internal-list*)
                                     (list*-in-area . %internal-list*-in-area))))))
  (cond (generating-micro-compiler-input-p
         (outf `(((%internal-list-args ,(list (length argl) area))))))))

;(defun (:property mapc p2) (argl dest)
;  (cond ((= (length argl) 2)
;        (push-arguments argl)
;        (outi1 `(misc1 ,dest %internal-mapc)))
;       (t
;        (p2f-argc 'mapc argl dest))))

;The current microcode has a problem if the list is circular.
; Tests should be made to see how much faster this is since
; micro-macro function calls are currently pretty slow.
;(defun (:property mapcar p2) (argl dest)
;  (cond ((= (length argl) 2)
;        (push-arguments argl)
;        (outi1 `(misc1 ,dest %internal-mapcar)))
;       (t
;        (p2f-argc 'mapcar argl dest))))

(defun (:property append p2) (argl dest)
  (cond ((= (length argl) 2)
         (push-arguments argl)
         (outi1 `(misc1 ,dest %internal-append-2)))
        (t
         (p2f-argc 'append argl dest))))

(defun (:property nconc p2) (argl dest)
  (cond ((= (length argl) 2)
         (push-arguments argl)
         (outi1 `(misc1 ,dest %internal-nconc-2)))
        (t
         (p2f-argc 'nconc argl dest))))

(defun (:property zerop p2) (argl dest)
  (cond ((and (memq dest '(d-ignore d-inds)) (not generating-micro-compiler-input-p))
         (if (adrrefp (car argl))
             (outi `(single-address-zerop ,(p2-source (car argl) dest)))
           (p2 (car argl) 'd-pdl)
           (outi `(single-address-zerop (pdl-pop)))))
        (t
         (p2misc 'zerop argl dest 1))))


(DEFUN GOTAGS-SEARCH (TAG)
  (OR (find TAG *GOTAG-ENVIRONMENT* :key #'gotag-prog-tag)
      (PROGN (WARN 'BAD-GO-TAG ':IMPOSSIBLE
                   "There is a ~S to tag ~S but no such tag exists."
                   'GO TAG)
             NIL)))

(DEFUN GPDLLVL (TAG)
  ;; Don't use GOTAGS-SEARCH because we want the warning
  ;; to appear only once, from OUTBRET or GTAG.
  (LET ((gotag (find TAG *GOTAG-ENVIRONMENT* :key #'gotag-prog-tag)))
    (IF GOTAG (GOTAG-PDL-LEVEL GOTAG) 0)))

(DEFUN GTAG (X)
  (GOTAG-LAP-TAG (GOTAGS-SEARCH X)))

;;; Output an unconditional transfer to the specified prog tag,
;;; popping the pdl the appropriate number of times to adjust the
;;; pdl from its current level to the level required at that tag.

;;; For handling GO, PROGDESC should be NIL and NVALUES should be 0.
;;; When jumping to the return tag of a prog, PROGDESC should be
;;; the desc for the prog we are returning from, and NVALUES should be
;;; the number of things on the top of the stack which are being left
;;; there as values to return from the prog.
(DEFUN OUTBRET (TAG PROGDESC NVALUES &AUX TEM (EXITPROGDESC PROGDESC))
  (SETQ TEM (GOTAGS-SEARCH TAG))
  (IF (NOT TEM)
      NIL
    ;; If this is GO, set EXITPROGDESC to the progdesc of its containing PROG
    (OR PROGDESC (SETQ EXITPROGDESC (GOTAG-PROGDESC TEM)))
    (POP-FRAMES EXITPROGDESC NVALUES)
    ;; For a prog rettag, the pdl level should include
    ;; the number of values desired on the stack.
    (POPPDL NVALUES (- PDLLVL (GOTAG-PDL-LEVEL TEM)))
    (OUTB `(BRANCH ALWAYS NIL NIL ,(GOTAG-LAP-TAG TEM)))))

(DEFUN POP-FRAMES (EXITPROGDESC NVALUES)
  ;; If we are exiting any PROGs, unwind stacks to their levels.
  ;; Does not include the prog whose desc is EXITPROGDESC.
  (LET ((N-UNBINDS 0) LAST-VARIABLE-UNBIND-PDL-LEVEL)
    (DO ((L *PROGDESC-ENVIRONMENT* (CDR L)))
        ((EQ (CAR L) EXITPROGDESC))
      (COND ((CONSP (PROGDESC-NBINDS (CAR L)))
             (SETQ N-UNBINDS (CAR (PROGDESC-NBINDS (CAR L))))
             (SETQ LAST-VARIABLE-UNBIND-PDL-LEVEL (PROGDESC-PDL-LEVEL (CAR L))))
            (T (SETQ N-UNBINDS (+ N-UNBINDS (PROGDESC-NBINDS (CAR L)))))))
    ;; LAST-VARIABLE-UNBIND-PDL-LEVEL is the level at start of PROG body,
    ;; and does not include the values we want to return.
    ;; PDLLVL at all times includes those values
    ;; since they are already on the stack.
    (COND (LAST-VARIABLE-UNBIND-PDL-LEVEL
           (POPPDL NVALUES (- PDLLVL NVALUES LAST-VARIABLE-UNBIND-PDL-LEVEL))
           (OUTPUT-UNBIND-TO-INDEX NVALUES)
           (MKPDLLVL (+ LAST-VARIABLE-UNBIND-PDL-LEVEL NVALUES -1))))
    (UNBIND 'D-IGNORE N-UNBINDS)))


;;; Pop NPOPS words off the pdl, from underneath the top NVALUES words.
;;; We do not change PDLLVL.
(DEFUN POPPDL (NVALUES NPOPS)
  (COND ((MINUSP NPOPS)
         (BARF NPOPS "negative number of pops" 'BARF))
        ((NOT (ZEROP NPOPS))
         ;; Output enough POP-OPEN-CALL instructions to flush
         ;; any unwind protects inside the desired pdl level.
         (DO ((I 0 (1+ I))
              (N 0)
              (L *CALL-BLOCK-PDL-LEVELS* (CDR L)))
             ((OR (NULL L)
                  ( (IF (CONSP (CAR L)) (CAAR L) (CAR L))
                     (- PDLLVL NPOPS NVALUES)))
              ;; N is the number of frams we must flush
              ;; to take us past all the unwind-protects.
              (DOTIMES (J N)
                (IF (NUMBERP (CAR *CALL-BLOCK-PDL-LEVELS*))
                    (OUTI '(MOVE D-PDL (QUOTE-VECTOR (QUOTE 0))))
                  (OUTI `(MOVE D-PDL (QUOTE-VECTOR
                                       (TAG ,(CADDAR *CALL-BLOCK-PDL-LEVELS*))))))
                (OUTI '(MISC D-IGNORE POP-OPEN-CALL))
                (POP *CALL-BLOCK-PDL-LEVELS*)))
           (IF (AND (CONSP (CAR L))
                    (EQ (CADAR L) 'UNWIND-PROTECT))
               (SETQ N (1+ I))))
         (COND ((> NVALUES 1)
                (OUTI `(PUSH-NUMBER ,npops))
                (outi `(push-number ,nvalues))
                (OUTI '(MISC D-IGNORE POP-M-FROM-UNDER-N)))
               ((= NVALUES 1)
                (outi `(push-number ,npops))
                (OUTI '(MISC D-PDL SHRINK-PDL-SAVE-TOP)))
               (T
                 (DO ((N 17 (+ N 17)))
                     ;; N is number of pops we would have done if we now do
                     ;; another POPPDL 17.  N-17 is number of pops so far.
                     ((> N NPOPS)
                      (OR (= NPOPS (- N 17))
                          (OUTI `(MISC D-IGNORE POPPDL ,(- NPOPS (- N 17))))))
                   (OUTI '(MISC D-IGNORE POPPDL 17))))))))

;;; Output code to unbind to a specpdl index saved on the stack
;;; underneath N values.  The code pops that one word out of the stack
;;; but we do not change PDLLVL.
(DEFUN OUTPUT-UNBIND-TO-INDEX (NVALUES)
  (COND ((= NVALUES 0)
         (OUTI '(MISC D-IGNORE UNBIND-TO-INDEX)))
        ((= NVALUES 1)
         (OUTI '(MISC D-PDL UNBIND-TO-INDEX-MOVE)))
        (T (OUTI `(MOVE D-PDL (QUOTE-VECTOR ',NVALUES)))
           (OUTI '(MISC D-IGNORE UNBIND-TO-INDEX-UNDER-N)))))

(DEFUN OUTI (X)
  (IF (NOT *DROPTHRU*)
      NIL
    (IF (AND (EQ (CADR X) 'D-RETURN)
             (NOT (EQ (CAR X) 'CALL)))
        (SETQ *DROPTHRU* NIL))
    (IF (AND (EQ (CAR X) 'MISC)
             ( (GET (THIRD X) 'QLVAL) #o1000))
        (SETQ X (CONS 'MISC1 (CDR X))))
    (IF (MEMQ (CAR X) '(MISC MISC1))
        (OUTF X)
      (OUTS X))))

(DEFUN OUTI1 (X)                                ;Use this for outputing instructions
  (IF *DROPTHRU* (OUTS X)))                     ;known to take delayed transfers

(DEFUN TAKE-DELAYED-TRANSFER ()                 ;Call this when args to list or call completed
  (SETQ *DROPTHRU* NIL))

;;; Output a BRANCH instruction
(DEFUN OUTB (X)
  (COND ((EQ (CADDR X) 'NO-OP))
        ((EQ (CADDR X) 'RETURN))
        ((NULL *DROPTHRU*))
        (T (COND ((EQ (CADR X) 'ALWAYS)
                  (SETQ *DROPTHRU* NIL)))
           (PUTPROP (CAR (LAST X)) T 'USED)
           (OUTF X))))

;;; Branch indicator sense POPONNOJUMP tag branch
;;; occurs in C(IND) = SENSE

(DEFUN OUTTAG (X)
  (WHEN (GET X 'USED)
    (OR *DROPTHRU* (OUTF '(NO-DROP-THROUGH)))
    (SETQ *DROPTHRU* T)
    (OUTF X)))

(DEFCONST SOURCE-TYPE-INDEX-LIMIT-ALIST '((LOCAL #o77) (ARG #o77))
  "For various types of source address, this gives the maximum index that there is room for.
If an attempt is made to output a source address with a bigger index, it gets turned into
a two word instruction whose second word is an EXTENDED-ADDRESS instruction, and whose first
word has EXTEND as a source.")

;;; Output an instruction that might have a source address which might require an extra word.
(DEFUN OUTS (INSN)
  (LET ((SOURCELOC (LAST INSN))
        TEM)
    (IF (AND (CONSP (CAR SOURCELOC))
             (SETQ TEM (ASSQ (CAAR SOURCELOC) SOURCE-TYPE-INDEX-LIMIT-ALIST))
             (> (CADR (CAR SOURCELOC)) (CADR TEM)))
        (LET ((EXTENDED-ADDRESS
                `(EXTENDED-ADDRESS
                   ,(IF (MEMQ (CADR INSN) '(D-IGNORE D-INDS D-LAST D-NEXT D-PDL D-RETURN 0))
                        (CADR INSN)
                        0)
                   ,(CAR SOURCELOC))))
          (OUTF (APPEND (BUTLAST INSN) '(EXTEND)))
          (OUTF EXTENDED-ADDRESS))
      (OUTF INSN))))

(DEFUN OUTF (X)
  (COND ((NULL HOLDPROG)
         (PRINT X)
         (AND (NOT (ATOM X)) (CDR X) (CDDR X) (UNMADR (CADDR X))))
        ((VECTOR-PUSH X QCMP-OUTPUT))
        (T
         (ADJUST-ARRAY-SIZE QCMP-OUTPUT
                            (* 2 (ARRAY-DIMENSION QCMP-OUTPUT 0)))
         (VECTOR-PUSH X QCMP-OUTPUT))))

;;; Arg desc list -- a list of lists
;;; Each list (<repeat-count> <token-list>)
;;; Token list has things like FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST,
;;; and FEF-QT-EVAL FEF-QT-QT.

(DEFUN GETARGDESC (X &AUX TEM)                  ;second value T if this is a guess.
  (COND ((SETQ TEM (GET X 'ARGDESC))
         TEM)
        ((SETQ TEM (GET X 'QINTCMP))
         (LIST (CONS TEM '((FEF-ARG-REQ FEF-QT-EVAL)))))
        ((SETQ TEM (GET X 'Q-ARGS-PROP))
         (GET-ARGDESC-PROP-FROM-Q-ARGS-PROP TEM X))
        ((EQ X THIS-FUNCTION-ARGLIST-FUNCTION-NAME)
         (GET-ARGDESC-PROP-FROM-LAMBDA-LIST THIS-FUNCTION-ARGLIST))
        ((SETQ TEM (DECLARED-DEFINITION X))
         (COND ((SYMBOLP TEM)
                (GETARGDESC TEM))
               ((CONSP TEM)
                (SETQ TEM (LAMBDA-MACRO-EXPAND TEM))
                (GET-ARGDESC-PROP-FROM-LAMBDA-LIST
                  (CAR (SI::LAMBDA-EXP-ARGS-AND-BODY TEM))))
               ((AND (TYPEP TEM ':COMPILED-FUNCTION)
                     (SETQ TEM (GET-MACRO-ARG-DESC-POINTER TEM)))
                ;; Use ADL in preference to %ARGS-INFO so that we
                ;; find things like FEF-ARG-FUNCTIONAL.
                ;; The only reason we would have an ADL if the
                ;; %ARGS-INFO would otherwise be correct
                ;; is if things like FEF-ARG-FUNCTIONAL are present.
                (GET-ARGDESC-PROP-FROM-ADL TEM))
               (T
                (SETQ TEM (%ARGS-INFO X))
                (IF (BIT-TEST %ARG-DESC-INTERPRETED TEM)
                    '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL)))
                  (GET-ARGDESC-PROP-FROM-Q-ARGS-PROP TEM X)))))
        (T (VALUES '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL)))        ;make a guess
                   T))))

(DEFUN GET-ARGDESC-PROP-FROM-Q-ARGS-PROP (ARG-PROP FN-NAME &AUX ANS MIN-ARGS OPT-ARGS)
  (IF ( 0 (LOGAND %ARG-DESC-FEF-QUOTE-HAIR ARG-PROP))
      (GET-ARGDESC-PROP-FROM-ADL (GET-MACRO-ARG-DESC-POINTER (FSYMEVAL FN-NAME))))
  (IF ( 0 (SETQ MIN-ARGS (LDB %%ARG-DESC-MIN-ARGS ARG-PROP)))
      (SETQ ANS (NCONC ANS (LIST (CONS MIN-ARGS '((FEF-ARG-REQ FEF-QT-EVAL)))))))
  (IF ( 0 (SETQ OPT-ARGS (- (LDB %%ARG-DESC-MAX-ARGS ARG-PROP) MIN-ARGS)))
      (SETQ ANS (NCONC ANS (LIST (CONS OPT-ARGS '((FEF-ARG-OPT FEF-QT-EVAL)))))))
  (IF ( 0 (LOGAND %ARG-DESC-QUOTED-REST ARG-PROP))
      (SETQ ANS (NCONC ANS (LIST '(1 (FEF-ARG-REST FEF-QT-QT))))))
  (IF ( 0 (LOGAND %ARG-DESC-EVALED-REST ARG-PROP))
      (SETQ ANS (NCONC ANS (LIST '(1 (FEF-ARG-REST FEF-QT-EVAL))))))
  ANS)

(DEFUN GET-ARGDESC-PROP-FROM-LAMBDA-LIST (LL)
  (PROG (ANS QUOTE-STATUS REST-FLAG OPT-FLAG TOKEN-LIST NEXT-ELEMENT)
        (SETQ QUOTE-STATUS '&EVAL)
     L0
        (SETQ TOKEN-LIST NIL)
     L1
        (OR LL (RETURN ANS))
        (SETQ NEXT-ELEMENT (CAR LL) LL (CDR LL))
        (COND ((EQ NEXT-ELEMENT '&AUX)
               (RETURN ANS))
              ((EQ NEXT-ELEMENT '&OPTIONAL) (SETQ OPT-FLAG T) (GO L1))
              ((EQ NEXT-ELEMENT '&FUNCTIONAL)
               (SETQ TOKEN-LIST (CONS 'FEF-FUNCTIONAL-ARG TOKEN-LIST))
               (GO L1))
              ((MEMQ NEXT-ELEMENT '(&EVAL &QUOTE))      ; &QUOTE-DONTCARE
               (SETQ QUOTE-STATUS NEXT-ELEMENT)
               (GO L1))
              ((OR (EQ NEXT-ELEMENT '&REST) (EQ NEXT-ELEMENT '&KEY))
               (SETQ REST-FLAG T)
               (GO L1))
              ((MEMQ NEXT-ELEMENT LAMBDA-LIST-KEYWORDS)
               (GO L1)))
        (PUSH (CDR (ASSQ QUOTE-STATUS
                         '((&EVAL . FEF-QT-EVAL)
                           (&QUOTE . FEF-QT-QT))))
              TOKEN-LIST)
        (PUSH (COND (REST-FLAG 'FEF-ARG-REST)
                    ((NULL OPT-FLAG) 'FEF-ARG-REQ)
                    (T 'FEF-ARG-OPT))
              TOKEN-LIST)
        (SETQ ANS (NCONC ANS (LIST
                               (LIST 1 TOKEN-LIST))))
        (COND (REST-FLAG (RETURN ANS)))
        (GO L0)))

(DEFUN GET-ARGDESC-PROP-FROM-ADL (ADL)
  (LET (ARGDESC)
    (DO ((L ADL (CDR L))
         ITEM
         SYNTAX
         QUOTE)
        ((NULL L) (NREVERSE ARGDESC))
      (SETQ ITEM (CAR L))
      (AND (BIT-TEST %FEF-NAME-PRESENT ITEM) (SETQ L (CDR L)))
      (SETQ SYNTAX (MASK-FIELD %%FEF-INIT-OPTION ITEM)) ;Skip extra init Q
      (OR (= SYNTAX FEF-INI-NONE) (= SYNTAX FEF-INI-NIL) (= SYNTAX FEF-INI-SELF)
          (SETQ L (CDR L)))
      (SETQ SYNTAX (MASK-FIELD %%FEF-ARG-SYNTAX ITEM))
      (SETQ QUOTE
            (COND ((> (MASK-FIELD %%FEF-QUOTE-STATUS ITEM) FEF-QT-EVAL)
                   '(FEF-QT-QT))
                  (T '(FEF-QT-EVAL))))
      (AND (BIT-TEST FEF-FUNCTIONAL-ARG ITEM) (PUSH 'FEF-FUNCTIONAL-ARG QUOTE))
      (COND ((> SYNTAX FEF-ARG-REST)
             (RETURN (NREVERSE ARGDESC)))
            ((= SYNTAX FEF-ARG-REST)
             (RETURN (NRECONC ARGDESC `((1 (FEF-ARG-REST . ,QUOTE))))))
            ((= SYNTAX FEF-ARG-OPT)
             (PUSH `(1 (FEF-ARG-OPT . ,QUOTE)) ARGDESC))
            (T (PUSH `(1 (FEF-ARG-REQ . ,QUOTE)) ARGDESC))))))

;;;; Testing functions

;;; Given the lap address of a variable, print out the name of the variable in a comment.
;;; Used when compiling a function and printing the lap code on the terminal.
(DEFUN UNMADR (X)
  (COND ((AND (NOT (ATOM X)) (MEMQ (CAR X) '(ARG LOCAL)))
         (DO ((VS *ALLVARS* (CDR VS))) ((NULL VS) NIL)
            (AND (EQUAL X (VAR-LAP-ADDRESS (CAR VS)))
                 (PROGN (PRINC "  ;")
                        (PRIN1 (VAR-NAME (CAR VS)))
                        (RETURN (VAR-NAME (CAR VS)))))))))
