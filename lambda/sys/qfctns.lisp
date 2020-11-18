;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Base:8; Readtable:ZL -*-
;;; This is SYS: SYS; QFCTNS, a cold load file.
;;;
;;; ** (c) Copyright 1980, 1984 Massachusetts Institute of Technology **
;;; (C) Enhancements Copyright 1986, Lisp Machine Inc.

;;;Primitive and low-level public functions.

;;;6/28/88 - Fixed DEFF's to point to symbol value, not (FUNCTION ...).
;;   This is 1) to work in cold load for a few existing
;;   forward references (to un-fbound functions), and
;;   2) so synonyms pick up changed f-values. -Keith

;;Define symbols that can be used as declarations within functions to
;;provide information to the compiler and/or debugger.  Others defined
;;elsewhere include:
;; compiler::compiler-arglist
;; dbg:uninteresting-function

(DEFPROP ARGLIST T DEBUG-INFO)
(DEFPROP :ARGLIST ARGLIST DEBUG-INFO)
(DEFPROP VALUES T DEBUG-INFO)
(DEFPROP :VALUES VALUES DEBUG-INFO)
(DEFPROP RETURN-LIST VALUES DEBUG-INFO)
(DEFPROP :RETURN-LIST VALUES DEBUG-INFO)
(DEFPROP FUNCTION-PARENT T DEBUG-INFO)
(DEFPROP INTERPRETED-DEFINITION T DEBUG-INFO)
(DEFPROP DOCUMENTATION T DEBUG-INFO)
(DEFPROP ZWEI:INDENTATION T DEBUG-INFO)
;(defprop combined-method-derivation t debug-info)      --

;;; This is an A-memory location which contains the area number for most things to cons in.
(DEFVAR DEFAULT-CONS-AREA :UNBOUND              ;Initialized elsewhere.
  "The area used for consing by CONS, LIST, MAKE-ARRAY, etc. if nothing else is specified.")

;;; Some things cons in this area instead.  While the previous may be a temporary area,
;;; this should never be one.
(DEFVAR BACKGROUND-CONS-AREA :UNBOUND           ;Initialized elsewhere.
  "The area used for consing which is supposed to never be in a temporary area.
This area is used by functions which want to update permanent data structures
and may be called even when DEFAULT-CONS-AREA is a temporary area.")

;;;; Function and macro defining.

(DEFspecialk DEFF (&QUOTE FUNCTION-SPEC &EVAL DEFINITION)
  "Define FUNCTION-SPEC with its definition being the value of DEFINITION.
/(DEFF FOO '(LAMBDA (X) X)) is a useless but correct example;
the compiler would not compile the LAMBDA.
/(DEFF FOO 'BAR) makes FOO a synonym for BAR.
/(DEFF FOO #'BAR) /"works/" but you probably don't want to do this -
BAR must already be defined, and redefinitions of BAR will not affect FOO."
  (FSET-CAREFULLY FUNCTION-SPEC DEFINITION)
  FUNCTION-SPEC)

(DEFspecialk DEF (&QUOTE FUNCTION-SPEC &REST DEFINING-FORMS)
  "Define FUNCTION-SPEC by evaluating DEFINING-FORMS for effect.
This function does nothing, really.  It exists to identify to the editor where a
function is being defined, if the editor would not otherwise be able to figure it out."
  (MAPC #'EVAL1 DEFINING-FORMS)
  FUNCTION-SPEC)

(DEFspecialk DEFUN (&QUOTE &REST ARG)
  "Define FUNCTION-SPEC to take args according to LAMBDA-LIST and compute value using BODY.
BODY may start with declarations, which do affect variables bound by LAMBDA-LIST,
and//or a documentation string.  Example:
  (DEFUN IDENTITY (ANYTHING)
    /"Returns its argument./"
    ANYTHING)"
  (DECLARE (ARGLIST &QUOTE FUNCTION-SPEC LAMBDA-LIST &REST BODY))
  (PROG (SYM DEF)
    ;; Turn old Maclisp DEFUNs into standard ones, provided function to do it is loaded.
    (COND ((FBOUNDP 'DEFUN-COMPATIBILITY)
           (SETQ ARG (DEFUN-COMPATIBILITY ARG))
           (OR (EQ (CAR ARG) 'DEFUN)
               (RETURN (EVAL ARG)))
           (SETQ ARG (CDR ARG))))
    ;; Convert body into NAMED-LAMBDA, hacking declarations
    (SETQ SYM (CAR ARG))
    (SETQ DEF (PROCESS-DEFUN-BODY SYM (CDR ARG)))
    ;; Carefully store into function cell
    (FDEFINE SYM DEF T)
    (RETURN SYM)))

(DEFspecialk MACRO (&QUOTE FUNCTION-SPEC &REST DEF)
  "Define FUNCTION-SPEC as a macro; this is the most primitive way.
LAMBDA-LIST should specify one arg, which gets the whole form
 that is the macro call.
BODY is what is evaluated to produce the expansion of the macro call.
Example:
  (MACRO FIRST (FORM) `(CONS ,(CADR FORM)))
Note that (CAR FORM) would be the symbol FIRST, since FORM
is a call to the macro FIRST."
  (DECLARE (ARGLIST &QUOTE FUNCTION-SPEC LAMBDA-LIST &REST BODY))
  (OR (SYMBOLP FUNCTION-SPEC) (SETQ FUNCTION-SPEC (STANDARDIZE-FUNCTION-SPEC FUNCTION-SPEC)))
  (AND UNDO-DECLARATIONS-FLAG
       (COMPILER:FUNCTION-REFERENCED-P FUNCTION-SPEC)
       (COMPILER:WARN 'MACRO-USED-BEFORE-DEFINED ':IMPOSSIBLE
                      "The macro ~S was used before it was defined" FUNCTION-SPEC))
  (SETQ DEF (PROCESS-DEFUN-BODY FUNCTION-SPEC DEF))
  (SETQ DEF (CONS 'MACRO DEF))
  ;; Put macro definition where it belongs (don't really define it if compiling)
  (COND ((AND (BOUNDP 'UNDO-DECLARATIONS-FLAG) UNDO-DECLARATIONS-FLAG)
         (IF (EQ (CAR-SAFE FUNCTION-SPEC) ':PROPERTY)
             (PUTDECL (CADR FUNCTION-SPEC) (CADDR FUNCTION-SPEC) DEF))
         (setf (gethash function-spec
                        (compiler:compilation-environment-macro-hashtab compiler:*compilation-environment*))
               def)
         ;; (PUSH `(DEF ,FUNCTION-SPEC . ,DEF) FILE-LOCAL-DECLARATIONS)
         )
        (T
         (FDEFINE FUNCTION-SPEC DEF T)))
  FUNCTION-SPEC)

(DEFspecialk DEFF-MACRO (&QUOTE FUNCTION &EVAL DEFINITION)
  "Define FUNCTION with definition DEFINITION, which should be a subst or macro.
If found in a file being compiled, this definition will be in effect
during compilation as well as when the compiled file is loaded.
That is how DEFF-MACRO differs from DEFF."
  (AND (BOUNDP 'UNDO-DECLARATIONS-FLAG)
       UNDO-DECLARATIONS-FLAG
       (COMPILER:FUNCTION-REFERENCED-P FUNCTION)
       (COMPILER:WARN 'MACRO-USED-BEFORE-DEFINED ':IMPOSSIBLE
                      "The macro ~S was used before it was defined" FUNCTION))
  ;; Put macro definition where it belongs (don't really define it if compiling)
  (COND ((AND (BOUNDP 'UNDO-DECLARATIONS-FLAG) UNDO-DECLARATIONS-FLAG)
         (WHEN (EQ (CAR-SAFE FUNCTION) ':PROPERTY)
           (PUTDECL (CADR FUNCTION) (CADDR FUNCTION) DEFINITION))
         (setf (gethash function
                        (compiler:compilation-environment-macro-hashtab compiler:*compilation-environment*))
               definition)
         ;; (PUSH `(DEF ,FUNCTION . ,DEFINITION) FILE-LOCAL-DECLARATIONS)
         )
        (T
         (FDEFINE FUNCTION DEFINITION T)))
  FUNCTION)

;;; (DEFSUBST FOO (X) (AREF X 5)) is like a similar DEFUN
;;; except that the definition of FOO will be substituted in at compile time
;;; and FOO's argument variables eliminated by substitution.
;;; This now hacks multiple evaluation of ts args right (ie doesn't do it) Yow!
(DEFspecialk DEFSUBST (&QUOTE SYMBOL &REST DEF)
  "Define SYMBOL as a substitutable function.
DEFSUBST is used like DEFUN, and the resulting function may be called
and will work just as if it had been defined with DEFUN.
However, the compiler will open-code calls to this function
by substituting the arguments specified in the call
into the function's body."
  (DECLARE (ARGLIST &QUOTE SYMBOL LAMBDA-LIST &REST BODY))
  (DEFSUBST-1 SYMBOL DEF))

(defmacro (defsubst alternate-macro-definition) (symbol &rest definition)
  `(DEFSUBST-1 (QUOTE ,symbol) (QUOTE ,definition)))

(DEFUN DEFSUBST-1 (SYMBOL DEF)
  (OR (SYMBOLP SYMBOL) (SETQ SYMBOL (STANDARDIZE-FUNCTION-SPEC SYMBOL)))
  (AND UNDO-DECLARATIONS-FLAG
       (COMPILER:FUNCTION-REFERENCED-P SYMBOL)
       (COMPILER:WARN 'MACRO-USED-BEFORE-DEFINED ':IMPOSSIBLE
                      "The defsubst ~S was used before it was defined" SYMBOL))
  ;; Convert body into NAMED-SUBST, hacking declarations
  (SETQ DEF (CONS 'NAMED-SUBST (CDR (PROCESS-DEFUN-BODY SYMBOL DEF T))))
  (DO ((PTR (CADDR DEF) (CDR PTR)))
      ((NULL PTR))
    (LET ((ELT (CAR PTR)))
      (COND ((AND (MEMQ ELT LAMBDA-LIST-KEYWORDS)
                  (NOT (MEMQ ELT '(&REST &OPTIONAL))))
             (COND ((EQ ELT '&AUX)
                    (SETQ DEF (LIST* (CAR DEF) (CADR DEF)
                                     (LDIFF (CADDR DEF) PTR)
                                     (CDDDR DEF))))
                   ((EQ ELT '&KEY)
                    (SETQ DEF (LIST* (CAR DEF) (CADR DEF)
                                     (APPEND (LDIFF (CADDR DEF) PTR)
                                             '(&REST IGNORE))
                                     (CDDDR DEF))))
                   (T
                    (SETQ DEF (LIST* (CAR DEF) (CADR DEF)
                                     (REMQ ELT (CADDR DEF))
                                     (CDDDR DEF)))))
             (IF UNDO-DECLARATIONS-FLAG
                 (COMPILER:WARN 'BAD-DEFSUBST-KEYWORDS ':IMPOSSIBLE
                                "The defsubst ~S uses the lambda-list keyword ~S"
                                SYMBOL ELT)
               (FERROR "The defsubst ~S uses the lambda-list keyword ~S"
                       SYMBOL ELT))))))
  ;; Put macro definition where it belongs (don't really define it if compiling)
  (COND ((AND (BOUNDP 'UNDO-DECLARATIONS-FLAG) UNDO-DECLARATIONS-FLAG)
         (setf (gethash symbol (compiler:compilation-environment-macro-hashtab compiler:*compilation-environment*))
               def)
         ;; (PUSH `(DEF ,SYMBOL . ,DEF) FILE-LOCAL-DECLARATIONS)
         )
        (T
         (FDEFINE SYMBOL DEF T)))
  SYMBOL)

(DEFUN EXTRACT-DECLARATIONS (BODY &OPTIONAL DECLS DOC-STRING-VALID-P ENVIRONMENT &AUX DOC)
  "Extract declarations and documentation string from BODY and return them.
The first value is what is left of BODY after any doc string and decls are removed.
It is BODY missing some number of its initial elements.

The second value is the list of declarations found.
Each element of a DECLARE found in body is a declaration
and goes on this list.  The argument DECLS is the initial
value of this list, and all declarations in BODY are added to that.

The third value is the doc string found in BODY, if there was one.
However, doc strings are only processed if DOC-STRING-VALID-P is non-NIL."
  (DECLARE (VALUES BODY DECLARATIONS DOC-STRING))
  (DO-FOREVER
    (LET (FORM)
      ;; Macro-expand the form, but don't worry if we get an error.
      ;; In that case, we will not see it as a declaration,
      ;; it will get macroexpanded again, and generate a warning then.
      (SETQ FORM (CONDITION-CASE ()
                                 (MACROEXPAND (CAR BODY) ENVIRONMENT)
                   (ERROR (RETURN))))
      (COND ((AND DOC-STRING-VALID-P
                  (STRINGP FORM))
                  ;; We skip any number of strings, but use only the first.
                  (OR DOC (SETQ DOC FORM))
                  ;; If the string is the last thing in the body,
                  ;; don't flush it, since it needs to be the return value.
                  (OR (CDR BODY) (RETURN)))
            ((EQ (CAR-SAFE FORM) 'DECLARE)
             ;; hack the documentation declaration specially
             (COND ((EQ (CADR-SAFE FORM) 'DOCUMENTATION)
                    (SETQ DOC (AND DOC-STRING-VALID-P (OR DOC (CADR-SAFE (CADR FORM))))))
                   ((CDR FORM)
                    ;; We allow any number of DECLAREs, and process them all.
                    (SETQ DECLS (APPEND (CDR FORM) DECLS)))))
            ;also ignore comment forms scattered among the declarations
            ((eq (car-safe form) 'comment)
             (or (cdr body) (return)))
            (T (RETURN)))
      (POP BODY)))
  (VALUES BODY DECLS DOC))

(DEFUN PROCESS-DEFUN-BODY (NAME VARS+BODY &OPTIONAL NO-IMPLICIT-BLOCK)
  "Given the name, and the data, for a DEFUN, return a NAMED-LAMBDA.
NO-IMPLICIT-BLOCK inhibits creation of the automatic BLOCK around the BODY.
This is used for DEFSUBST."
  ;; In case DEFUN was called from compiled code,
  ;; and VARS+BODY is a stack list, copy it.
  (SETQ VARS+BODY (COPY-OBJECT VARS+BODY))
  (LET ((LOCAL-DCL LOCAL-DECLARATIONS)
        (BODY (CDR VARS+BODY))
        (VARS (CAR VARS+BODY))
        DOCUMENTATION TEM)
    ;; Extract any DECLARE from the front of the body and put it into
    ;; the local declarations that are in effect.  Remove the DECLARE from the body.
    (SETF (VALUES BODY LOCAL-DCL DOCUMENTATION)
          (EXTRACT-DECLARATIONS BODY LOCAL-DCL T))
    (UNLESS NO-IMPLICIT-BLOCK
      (IF (SYMBOLP NAME)
          (SETQ BODY `((BLOCK ,NAME . ,BODY)))))
    ;; Use some local declarations for making debug-info, if they are present.
    ;; Canonicalize synonyms too.
    (LET ((DEBUG-INFO (LOOP FOR DCL IN LOCAL-DCL
                            WITH TEM
                         WHEN (SETQ TEM (GET (CAR DCL) 'DEBUG-INFO))
                            COLLECT (IF (OR (EQ TEM T) (NOT (SYMBOLP TEM)))
                                        (COPY-TREE DCL)
                                        (CONS TEM (COPY-TREE (CDR DCL)))))))
      (AND DOCUMENTATION (PUSH `(DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
      ;; Put whatever other local declarations there are
      ;; into a DECLARE at the front of the transformed body.
      (WHEN LOCAL-DCL
        (SETQ TEM (SUBSET-NOT #'(LAMBDA (X) (GET (CAR X) 'DEBUG-INFO))
                              LOCAL-DCL))
        (PUSH `(DECLARE . ,TEM) BODY))
      ;; Make a NAMED-LAMBDA of the appropriate form
      `(NAMED-LAMBDA ,(IF (OR DEBUG-INFO (NOT (SYMBOLP NAME)))
                          (CONS NAME DEBUG-INFO)
                          NAME)
                     ,VARS . ,BODY))))

;;; Used as the value of *MACROEXPAND-HOOK* to make all macros displace.
(defun automatic-displace (expander-function original-form)
  (let ((expanded-form
          (if (> (ldb %%arg-desc-max-args (args-info expander-function)) 1)
              (funcall expander-function original-form *macroexpand-environment*)
              (funcall expander-function original-form))))
    (if (or (eq expanded-form original-form)
            (eq (car original-form) 'displaced)
            inhibit-displacing-flag
            (not (= (%area-number original-form) working-storage-area))
            (and (%pointerp expanded-form)
                 (not (= (%area-number expanded-form) working-storage-area))))
        expanded-form
      (displace original-form expanded-form))))

;;; non-NIL to prevent macros from displacing.
(DEFVAR INHIBIT-DISPLACING-FLAG NIL
  "Non-NIL makes displacing macros not actually displace.")

;;; Make the original form now look like
;;; (displaced (original-car . original-cdr) expanded-form)
;;; avoiding timing errors in case two people displace the same form the same way
;;; at the same time.
;;; Note that if the original form is not in working-storage-area, don't try
;;; to displace it.  It might be in the compiler temporary area, in which case
;;; there wouldn't be much point to displacing.  It can also be in INIT-LIST-AREA,
;;; in which case attempting to displace would crash the machine.
(defun displace (original-form expanded-form &aux area tem)
  "Modify ORIGINAL-FORM so that, when evaluated, it acts like EXPANDED-FORM.
The list structure of ORIGINAL-FORM is altered so that it becomes a
call to SI:DISPLACED, which contains the expanded form and
a copy of the original contents of the expanded form."
  (without-interrupts
    (cond (inhibit-displacing-flag)
          ((eq (car original-form) 'displaced)
           (setf (caddr original-form) expanded-form))
          ((and (= (setq area (%area-number original-form)) working-storage-area)
                (or (null (%area-number expanded-form))
                    (= (%area-number expanded-form) area)))
           ;; Above area tests are intended to avoid problems with the compiler
           ;; temporary area, by not displacing anything with something that was
           ;; consed in a temporary area.  Note that not only lists are in the
           ;; temporary area, so are gensyms, strings, and flonums.
           (let ((default-cons-area area))
             (setq tem `((,(car original-form) . ,(cdr original-form)) ,expanded-form)))
           (rplaca original-form 'displaced)
           (rplacd original-form tem)))
    expanded-form))

(macro displaced (form &optional macroenvironment)
       macroenvironment
       (caddr form))

;(LAMBDA-MACRO DISPLACED (FCTN) ; doesn't work in make-cold moved to lmmac.
;  (CADDR FCTN))

;;;; Functions for compatibility with MACLISP LEXPRs.
;;; DEFUN-COMPATIBILITY is used to convert MACLISP LEXPR and FEXPR DEFUNs to Lispm form.

(DEFVAR *LEXPR-ARGLIST* :UNBOUND
  "This variable holds the &REST-argument to a converted Maclisp LEXPR.
The Maclisp functions ARG, SETARG and LISTIFY find the arguments here.")

(DEFUN ARG (N)
  "In a Maclisp LEXPR, refer to an argument by its number (origin-1)."
  (COND ((NULL N) (LENGTH *LEXPR-ARGLIST*))
        (T (LET ((ARGPTR (NTHCDR (1- N) *LEXPR-ARGLIST*)))
             (IF (OR ( N 0) (NULL ARGPTR))
               (FERROR "~D is not between 1 and the number of args" N))
             (CAR ARGPTR)))))

(DEFUN SETARG (N X)
  "In a Maclisp LEXPR, refer to an argument by its number (origin-1) and set it to X."
  (LET ((ARGPTR (NTHCDR (1- N) *LEXPR-ARGLIST*)))
    (COND ((OR ( N 0) (NULL ARGPTR))
           (FERROR "~D is not between 1 and the number of args" N)))
    (RPLACA ARGPTR X)
    X))

(DEFUN LISTIFY (N)
  "In a Maclisp LEXPR, return a list of some or all the arguments.
If N is positive, the first N; otherwise, the last -N."
  (COND ((MINUSP N) (COPY-LIST (NLEFT (- N) *LEXPR-ARGLIST*)))
        ((ZEROP N) NIL)
        (T (FIRSTN N *LEXPR-ARGLIST*))))

(DEFSUBST IDENTITY (X)
  "Return the argument."
  X)

;;;; List manipulation functions.

(defun list (&rest elements)
  "Return a list whose elements are the arguments."
  (copy-list elements))

(defun list-in-area (area &rest elements)
  "Returns a list of ELEMENTS, constructed in area AREA."
  (copy-list elements area))

(defun list* (&rest elements)
  "Return a list whose elements are the arguments, and whose tail is the last argument.
/(LIST* 'A 'B '(C D)) returns a list (A B C D)."
  (cond ((null elements) ())
        ((null (cdr elements))
         (car elements))
        (t
         (let* ((l (copy-list elements))
                (last (last l)))
           (without-interrupts
             (%p-dpb-offset cdr-error %%q-cdr-code last 0)
             (%p-dpb-offset cdr-normal %%q-cdr-code last -1))
           l))))

(defun list*-in-area (area &rest elements)
  "Returns a LIST* of ELEMENTS, constructed in area AREA."
  (cond ((null elements) ())
        ((null (cdr elements))
         (car elements))
        (t
         (let* ((l (copy-list elements area))
                (last (last l)))
           (without-interrupts
             (%p-dpb-offset cdr-error %%q-cdr-code last 0)
             (%p-dpb-offset cdr-normal %%q-cdr-code last -1))
           l))))


(DEFUN COPYLIST (LIST &OPTIONAL AREA FORCE-DOTTED)
  "Copy top level of list structure.  Dotted pair termination of list will be copied"
  (IF (ATOM LIST) LIST
    (LET ((DOTTED (OR FORCE-DOTTED (CDR (LAST LIST)))))
      (LET ((NEWLIST (MAKE-LIST (IF DOTTED (1+ (LENGTH LIST)) (LENGTH LIST)) :AREA AREA)))
        (DO ((L1 LIST (CDR L1))
             (L2 NEWLIST (CDR L2)))
            ((ATOM L1)
             (WHEN DOTTED
               (SETF (CAR L2) L1)
               (WITHOUT-INTERRUPTS
                 (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE L2 0)
                 (%P-DPB-OFFSET CDR-NORMAL %%Q-CDR-CODE L2 -1))))
          (SETF (CAR L2) (CAR L1)))
        NEWLIST))))

(DEFF COPY-LIST 'COPYLIST)

(DEFSUBST COPYLIST* (LIST &OPTIONAL AREA)
  "Like COPY-LIST but never cdr-codes the last pair of the list."
  (COPY-LIST LIST AREA T))

(DEFUN COPYALIST (AL &OPTIONAL (DEFAULT-CONS-AREA DEFAULT-CONS-AREA))
  "Copy top two levels of list structure.  Dotted pair termination of list will be copied"
  (IF (ATOM AL) AL
    (SETQ AL (APPEND AL (CDR (LAST AL))))       ;Recopy the top level.
    (DO ((P AL (CDR P)))
        ((ATOM P) AL)
      (IF (CONSP (CAR P))               ;Then recopy the assoc cells.
          (SETF (CAR P) (CONS (CAAR P) (CDAR P)))))))

(DEFF COPY-ALIST 'COPYALIST)

;;; (SUBST NIL NIL ...) is such an ugly language idiom...

(DEFUN COPYTREE (TREE &OPTIONAL (DEFAULT-CONS-AREA DEFAULT-CONS-AREA))
  "Copy list structure to all levels, creating a maximally cdr-coded structure."
  (IF (ATOM TREE)
      TREE
    (LET ((NEWTREE (COPY-LIST TREE)))
      (DO ((L NEWTREE (CDR L)))
          ((ATOM L))
        (SETF (CAR L) (COPYTREE (CAR L))))
      NEWTREE)))

(DEFF COPY-TREE 'COPYTREE)

;;; Copy an object, and the objects it points to, and ...
(DEFUN COPY-OBJECT-TREE (OBJECT &OPTIONAL TEMPORARY-AREAS-ONLY DEPTH)
  "Copy lists, arrays and everything but symbols, to DEPTH levels.
If TEMPORARY-AREAS-ONLY is non-NIL, objects in non-temporary areas are left alone.
DEPTH defaults to infinity --- this may lose if OBJECT is circular as no checking is done."
  (IF (OR (NOT (%POINTERP OBJECT))
          (SYMBOLP OBJECT)
          (LOCATIVEP OBJECT)
          (AND TEMPORARY-AREAS-ONLY
               (NOT (AREA-TEMPORARY? (%AREA-NUMBER OBJECT)))))
      OBJECT
    (LET* ((NEW (COPY-OBJECT OBJECT))
           (LEADER (%FIND-STRUCTURE-LEADER NEW)))
      (DOTIMES (I (%STRUCTURE-BOXED-SIZE NEW))
        (LET ((DTP (%P-LDB-OFFSET %%Q-DATA-TYPE LEADER I)))
          (WHEN (AND (%POINTER-TYPE-P DTP)
                     (NOT (MEMQ DTP '(#.DTP-LOCATIVE
                                      #.DTP-SYMBOL
                                      #.DTP-EXTERNAL-VALUE-CELL-POINTER
                                      #.DTP-BODY-FORWARD))))
            (SETF (%P-CONTENTS-OFFSET LEADER I)
                  (IF (EQ DEPTH 0)
                      NIL
                    (COPY-OBJECT-TREE (%P-CONTENTS-OFFSET LEADER I) TEMPORARY-AREAS-ONLY
                                      (IF DEPTH (1- DEPTH))))))))
      NEW)))

(DEFUN COPY-SEQ (SEQUENCE)
  "Return a new sequence with the same elements as SEQUENCE, and of the same type.
SEQUENCE may be a list or an array."
  (ETYPECASE SEQUENCE
    (LIST (COPY-LIST SEQUENCE))
    (VECTOR
     (LET* ((LEN (LENGTH SEQUENCE))
            (NEW (SIMPLE-MAKE-ARRAY LEN (ARRAY-TYPE SEQUENCE) nil (array-leader-length sequence))))
       (COPY-ARRAY-CONTENTS-and-leader SEQUENCE NEW)
       NEW))))

(DEFUN COPY-OBJECT (OBJECT)
  "Copy any kind of object that occupies memory, except symbols and locatives.
The copy has the same contents as the original.
Fixnums, etc., are simply returned as supplied."
  (TYPECASE OBJECT
    ((OR FIXNUM CHARACTER SYMBOL) OBJECT)
    (CONS (COPYLIST OBJECT))
    ((OR CLOSURE ENTITY SELECT)
     (%MAKE-POINTER (%DATA-TYPE OBJECT)
                    (COPYLIST (%MAKE-POINTER DTP-LIST OBJECT))))
    (SMALL-FLONUM OBJECT)
    (MICROCODE-FUNCTION OBJECT)
    (LOCATIVE OBJECT)
    (STACK-GROUP (FERROR "It is not possible to copy a stack group"))
    (T
     (IF (AND (ARRAYP OBJECT) (= (%STRUCTURE-TOTAL-SIZE OBJECT) 1))
         ;; 0-diml arrays are such a pain (and typecase won't hack "(satisifies (lambda ...))")
         (MAKE-ARRAY 0 :TYPE (ARRAY-TYPE OBJECT))
       (WITHOUT-INTERRUPTS
         (LET* ((OBJECT (IF (= (%P-DATA-TYPE OBJECT) DTP-HEADER-FORWARD)
                            (FOLLOW-STRUCTURE-FORWARDING OBJECT)
                          OBJECT))
                (LEADER (%FIND-STRUCTURE-LEADER OBJECT))
                (TOTAL (%STRUCTURE-TOTAL-SIZE LEADER))
                (BOXED (%STRUCTURE-BOXED-SIZE LEADER))
                (NEW (%MAKE-STRUCTURE (%DATA-TYPE OBJECT)
                                      (%P-DATA-TYPE LEADER)
                                      (%P-POINTER LEADER)
                                      0
                                      DEFAULT-CONS-AREA
                                      TOTAL
                                      BOXED)))
           ;; %POINTERs ensure %BLT-TYPED isn't given array-pointers,
           ;; which it treats differently.
           (%BLT-TYPED (%POINTER LEADER) (%POINTER NEW) BOXED 1)
           (%BLT (%MAKE-POINTER-OFFSET DTP-LOCATIVE LEADER BOXED)
                 (%MAKE-POINTER-OFFSET DTP-LOCATIVE NEW BOXED)
                 (- TOTAL BOXED) 1)
           (%MAKE-POINTER-OFFSET (%DATA-TYPE OBJECT) NEW
                                 (%POINTER-DIFFERENCE OBJECT LEADER))))))))

(DEFUN REVERSE (SEQUENCE)
  "Return a sequence whose elements are those of SEQUENCE, in reverse order.
If SEQUENCE is a list, the value is a list.
If it is an array, the value is an array of the same type."
  (ETYPECASE SEQUENCE
    (VECTOR
     (LET* ((LEN (LENGTH SEQUENCE))
            (RESULT (MAKE-ARRAY LEN :TYPE (ARRAY-TYPE SEQUENCE))))
       (DOTIMES (I LEN)
         (SETF (AREF RESULT I)
               (AREF SEQUENCE (- LEN I 1))))
       RESULT))
    (LIST
     (DO (V (S SEQUENCE (CDR S)))
         ((ATOM S) V)
       (PUSH (CAR S) V)))))

(DEFUN NREVERSE (SEQUENCE)
  "Alter SEQUENCE destructively to contain its elements in reverse order.
If SEQUENCE is a list, this works by changing cdr pointers.
If SEQUENCE is an array, this works by shuffling the elements."
  (ETYPECASE SEQUENCE
    (VECTOR
     (LET* ((LEN (LENGTH SEQUENCE))
            (HALFLEN (TRUNCATE LEN 2)))
       (DOTIMES (I HALFLEN)
         (LET ((TEM (AREF SEQUENCE I)))
           (SETF (CL:AREF SEQUENCE I)
                 (CL:AREF SEQUENCE (- LEN I 1)))
           (SETF (CL:AREF SEQUENCE (- LEN I 1)) TEM)))
       SEQUENCE))
    (LIST
     (NRECONC SEQUENCE NIL))))

(DEFUN NRECONC (L TAIL)
  "Alter L destructively to contain its elements in reverse order,
with TAIL as the ultimate cdr."
  (DO () ((ATOM L) TAIL)
    (SETQ L (PROG1 (CDR L)
                   (SETF (CDR L) TAIL)
                   (SETQ TAIL L)))))

(DEFUN REVAPPEND (LIST STARTING-TAIL)
  "Return a list whose elements are those of LIST, in reverse order,
followed by STARTING-TAIL."
  (PROG ((V STARTING-TAIL))
     L  (COND ((ATOM LIST) (RETURN V)))
        (SETQ V (CONS (CAR LIST) V))
        (SETQ LIST (CDR LIST))
        (GO L)))

(DEFVAR QREVERSE-DUMMY-ARRAY-HEADER
        (MAKE-ARRAY #o100
                    :TYPE 'ART-Q-LIST
                    :AREA PERMANENT-STORAGE-AREA
                    :DISPLACED-TO (%MAKE-POINTER DTP-LOCATIVE NIL)))

(DEFUN QREVERSE (LIST)
  (WITHOUT-INTERRUPTS
    (LET* ((LENGTH (LENGTH LIST))
           (RETURN-LIST (MAKE-LIST LENGTH)))
      (%P-STORE-CONTENTS-OFFSET RETURN-LIST QREVERSE-DUMMY-ARRAY-HEADER 1)
      (%P-STORE-CONTENTS-OFFSET LENGTH QREVERSE-DUMMY-ARRAY-HEADER 2)
      (DO ((I (1- LENGTH) (1- I))
           (L LIST (CDR L)))
          ((NULL L))
        (SETF (AREF QREVERSE-DUMMY-ARRAY-HEADER I) (CAR L)))
      RETURN-LIST)))

(DEFUN APPEND (&REST LISTS)
  "Append any number of lists.
The value is a list whose elements are those of the argument lists, in order."
  (PROG (TOTAL-LENGTH ARGP VAL VALP)
        (COND ((ATOM LISTS) (RETURN NIL))
              ((ATOM (CDR LISTS))
                (RETURN (CAR LISTS))))
        (SETQ TOTAL-LENGTH 0)
        ;; Accumulate length of args we must copy
        (DO ((ARGP LISTS (CDR ARGP)))
            ((ATOM (CDR ARGP))
             ;; Plus one more if the last arg is not NIL.
             ;; But if all are NIL so far, leave it 0 as signal to COND that follows.
             (AND (CAR ARGP) (NOT (ZEROP TOTAL-LENGTH))
                  (INCF TOTAL-LENGTH)))
          ;; Verify that all args (except perhaps the last) are lists.
          (OR (CL:LISTP (CAR ARGP))
              (LET ((ARG (CAR ARGP)))
                (CHECK-TYPE ARG LIST)
                (SETF (CAR ARGP) ARG)))
          (SETQ TOTAL-LENGTH (+ TOTAL-LENGTH (LENGTH (CAR ARGP)))))
        (COND ((ZEROP TOTAL-LENGTH) (RETURN (CAR (LAST LISTS)))))
        (SETQ VALP (SETQ VAL (MAKE-LIST TOTAL-LENGTH)))
        (SETQ ARGP LISTS)
     L2 (COND ((NULL (CDR ARGP))
               ;; When we reach the last arg, if it's NIL, we are done.
               (OR (CAR ARGP) (RETURN VAL))
               ;; Otherwise, stick in a pointer to the last arg,
               ;; and then change it from an element to a cdr.
               (SETF (CAR VALP) (CAR ARGP))
               (WITHOUT-INTERRUPTS
                 (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE VALP 0)
                 (%P-DPB-OFFSET CDR-NORMAL %%Q-CDR-CODE VALP -1))
               (RETURN VAL)))
        (DO ((ARGLP (CAR ARGP) (CDR ARGLP)))
            ((ATOM ARGLP)
             (SETQ ARGP (CDR ARGP))
             (GO L2))
          (SETF (CAR VALP) (CAR ARGLP))
          (SETQ VALP (CDR VALP)))))

(DEFUN UNION-EQ (&REST LISTS)
  "Return the union of any number of lists, regarded as sets.
Each element of any of the arguments is also an element of the value.
If the first argument has no duplicate elements, neither does the value.
Elements are compared with EQ."
  (COND ((NULL LISTS) NIL)
        ((NULL (CDR LISTS)) (CAR LISTS))
        (T (APPLY #'NUNION-EQ (COPY-LIST (CAR LISTS)) (CDR LISTS)))))

(deff zl:union 'union-eq)

(DEFUN NUNION-EQ (&REST LISTS &AUX ACCUM)
  "Alter the first argument so that it becomes the union of all the arguments.
Compares elements with EQ."
  (SETQ ACCUM (CAR LISTS))
  (LET ((TAIL (OR (LAST ACCUM) (VARIABLE-LOCATION ACCUM))))
    (DO ((LS (CDR LISTS) (CDR LS))) ((NULL LS))
      (DO ((L (CAR LS) (CDR L))) ((NULL L))
        (OR (MEMQ (CAR L) ACCUM)
            (SETF (CDR TAIL) (SETQ TAIL (NCONS (CAR L))))))))
  ACCUM)

(deff zl:nunion 'nunion-eq)

(DEFUN NUNION-EQUAL (&REST LISTS &AUX ACCUM)
  "Alter the first argument so that it becomes the union of all the arguments.
Compares elements with EQUAL."
  (SETQ ACCUM (CAR LISTS))
  (LET ((TAIL (OR (LAST ACCUM) (VARIABLE-LOCATION ACCUM))))
    (DO ((LS (CDR LISTS) (CDR LS))) ((NULL LS))
      (DO ((L (CAR LS) (CDR L))) ((NULL L))
        (OR (IF (ATOM (CAR L))
                (MEMQ (CAR L) ACCUM)
              (MEMBER-EQUAL (CAR L) ACCUM))
            (SETF (CDR TAIL) (SETQ TAIL (NCONS (CAR L))))))))
  ACCUM)

(DEFUN INTERSECTION-EQ (&REST LISTS)
  "Return the intersection of any number of lists, regarded as sets.
If the first argument contains no duplicate elements, neither does the value.
Compares elements with EQ."
  (COND ((NULL LISTS) NIL)
        ((NULL (CDR LISTS)) (CAR LISTS))
        (T (APPLY #'NINTERSECTION-EQ (COPY-LIST (CAR LISTS)) (CDR LISTS)))))

(deff zl:intersection 'intersection-eq)

(DEFUN NINTERSECTION-EQ (&REST LISTS)
  "Alter the first argument to be the intersection of all the arguments.
The arguments are lists, regarded as sets.  All elements of the first
argument that do not belong in the intersection are deleted.
Compares elements with EQ."
  (DO ((LIST (CAR LISTS) (CDR LIST))
       (REST (CDR LISTS))
       (RESULT)
       (OLD))
      ((NULL LIST) RESULT)
    (COND ((DO ((X (CAR LIST))
                (REST REST (CDR REST)))
               ((NULL REST) T)
             (OR (MEMQ X (CAR REST))
                 (RETURN NIL)))
           (OR RESULT (SETQ RESULT LIST))
           (SETQ OLD LIST))
          (OLD
           (SETF (CDR OLD) (CDR LIST))))))

(deff zl:nintersection 'nintersection-eq)

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:23
(DEFUN NCONC (&REST ARGS)
  "Concatenate lists destructively.
All but the last argument are modified in their tails
to point to the following argument."
  (COND ((NULL ARGS) NIL)
        ((NULL (CDR ARGS))
         (let ((first-arg (CAR ARGS)))
           (check-type first-arg list)
           first-arg))
        (T (*NCONC1 ARGS))))

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:24
(DEFUN *NCONC1 (ARGS)
  (let ((first-arg (car args)))
    (check-type first-arg list)
    (IF (NULL (CDDR ARGS))
        (*NCONC first-arg (CADR ARGS))
      (*NCONC first-arg (*NCONC1 (CDR ARGS))))))

(DEFUN *NCONC (A B)
  (IF (ATOM A)
      B
    (SETF (CDR (LAST A)) B)
    A))

(DEFUN NBUTLAST (LIST &OPTIONAL (N 1))
  "Modify a list so that its last N elements are dropped."
  (LET ((NEWLEN (- (LENGTH LIST) N)))
    (IF ( NEWLEN 0)
        NIL
      (SETF (NTHCDR NEWLEN LIST) NIL)
      LIST)))

(DEFUN CIRCULAR-LIST (&REST ARGS &AUX TEM)
  "Return a circular list whose elements are ARGS (over and over again)."
  (WHEN ARGS
    (SETQ TEM (COPYLIST* ARGS))
    (SETF (CDR (LAST TEM)) TEM)
    TEM))

;;;; List searching functions.

;;; Subroutine used by EQUALP when args are arrays (of same rank).
(DEFUN EQUALP-ARRAY (ARRAY1 ARRAY2)
  (AND (LET ((RANK (ARRAY-RANK ARRAY1)))
         (DO ((I 1 (1+ I)))
             ((= I RANK) T)
           (UNLESS (= (%P-CONTENTS-OFFSET ARRAY1 I) (%P-CONTENTS-OFFSET ARRAY2 I))
             (RETURN NIL))))
       (LET ((LEN (LENGTH ARRAY1)))
         (AND (= LEN (LENGTH ARRAY2))
              (DOTIMES (I LEN T)
                (UNLESS (EQUALP (CLI:AR-1-FORCE ARRAY1 I) (CLI:AR-1-FORCE ARRAY2 I))
                  (RETURN NIL)))))))

(DEFUN TREE-EQUAL (X Y &KEY TEST TEST-NOT)
  "Compare two lists or trees recursively for matching structure and leaves.
TEST or TEST-NOT is a function to compare leaves (non-lists) with:
if TEST-NOT is specified, leaves match if that function returns NIL;
if TEST is specified, leaves match if that function returns T.
If no test is specified, EQL is used."
  (COND ((and test test-not)
         (ferror "Both ~S and ~S specified" :test :test-not))
        ((OR (EQ TEST 'EQ) (EQ TEST #'EQ))
         (OR (EQ X Y) (TREE-EQUAL-EQ X Y)))
        ((OR (EQ TEST 'EQL) (EQ TEST #'EQL)
             (AND (NULL TEST) (NULL TEST-NOT)))
         (TREE-EQUAL-EQL X Y))
        (T (TREE-EQUAL-1 X Y (OR TEST-NOT TEST) (NOT (NULL TEST-NOT))))))

(DEFUN TREE-EQUAL-1 (X Y PRED INVERTP)
  (DO ((XTAIL X (CDR XTAIL))
       (YTAIL Y (CDR YTAIL)))
      (())
    (if (null xtail)
        (return (null ytail)))
    (IF (ATOM XTAIL)
        (RETURN (AND (ATOM YTAIL)
                     (EQ INVERTP (NOT (FUNCALL PRED XTAIL YTAIL))))))
    (IF (ATOM YTAIL) (RETURN NIL))
    (IF (NOT (TREE-EQUAL-1 (CAR XTAIL) (CAR YTAIL) PRED INVERTP))
        (RETURN NIL))))

(DEFUN TREE-EQUAL-EQL (X Y)
  (DO ((XTAIL X (CDR XTAIL))
       (YTAIL Y (CDR YTAIL)))
      (())
    (IF (ATOM XTAIL)
        (RETURN (AND (ATOM YTAIL) (EQL XTAIL YTAIL))))
    (IF (ATOM YTAIL) (RETURN NIL))
    (IF (AND (NOT (EQL (CAR XTAIL) (CAR YTAIL)))
             (NOT (TREE-EQUAL-EQL (CAR XTAIL) (CAR YTAIL))))
        (RETURN NIL))))

(DEFUN TREE-EQUAL-EQ (X Y)
  (DO ((XTAIL X (CDR XTAIL))
       (YTAIL Y (CDR YTAIL)))
      (())
    (IF (ATOM XTAIL)
        (RETURN (AND (ATOM YTAIL) (EQ XTAIL YTAIL))))
    (IF (ATOM YTAIL) (RETURN NIL))
    (IF (AND (NEQ (CAR XTAIL) (CAR YTAIL))
             (NOT (TREE-EQUAL-EQ (CAR XTAIL) (CAR YTAIL))))
        (RETURN NIL))))

(DEFUN LIST-LENGTH (LIST)
  "Return the length of LIST, or NIL if LIST is circular."
  (DO ((N 0 (+ N 2))
       (Y LIST (CDDR Y))
       (X LIST (CDR X)))
      (())
    (WHEN (ATOM Y) (RETURN N))
    (WHEN (ATOM (CDR Y)) (RETURN (1+ N)))
    (WHEN (AND (EQ X Y) (PLUSP N)) (RETURN NIL))))

(DEFUN SASSOC (ITEM IN-LIST ELSE)
  "Is like ASSOC except that if ITEM is not found in IN-LIST, instead of
returning NIL, SASSOC calls the function ELSE with no arguments.
This is a remnant of Lisp 1.5."
  (OR (ASSOC-EQUAL ITEM IN-LIST)
      (APPLY ELSE NIL)))

(DEFUN SASSQ (ITEM IN-LIST ELSE)
  "Is like ASSQ except that if ITEM is not found in IN-LIST, instead of
returning NIL, SASSQ calls the function ELSE with no arguments.
This is a remnant of Lisp 1.5"
  (OR (ASSQ ITEM IN-LIST)
      (APPLY ELSE NIL)))

;;; Microcoded for System 99.  Don't delete, may need later.
;(DEFUN ASSOC (ITEM IN-LIST)
;  "Return the first element of IN-LIST whose CAR is EQUAL to ITEM."
;  (PROG ()
;       (IF (TYPEP ITEM '(OR SYMBOL FIXNUM SHORT-FLOAT))
;           (RETURN (ASSQ ITEM IN-LIST)))
;     L (COND ((NULL IN-LIST) (RETURN NIL))
;             ((NULL (CAR IN-LIST)))
;             ((EQUAL ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
;       (SETQ IN-LIST (CDR IN-LIST))
;       (GO L)))

;;; Microcoded for System 99.  Don't delete, may need later.
;(DEFUN ASSOC-EQUAL (ITEM IN-LIST)
;  "Return the first element of IN-LIST whose CAR is EQUAL to ITEM."
;  (PROG ()
;       (IF (TYPEP ITEM '(OR SYMBOL FIXNUM SHORT-FLOAT))
;           (RETURN (ASSQ ITEM IN-LIST)))
;     L (COND ((NULL IN-LIST) (RETURN NIL))
;             ((NULL (CAR IN-LIST)))
;             ((EQUAL ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
;       (SETQ IN-LIST (CDR IN-LIST))
;       (GO L)))

(DEFUN ASSOC-EQUALP (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CAR is EQUALP to ITEM."
  (PROG ()
        (IF (TYPEP ITEM 'SYMBOL)
            (RETURN (ASSQ ITEM IN-LIST)))
     L  (COND ((NULL IN-LIST) (RETURN NIL))
              ((NULL (CAR IN-LIST)))
              ((EQUALP ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
        (SETQ IN-LIST (CDR IN-LIST))
        (GO L)))

(DEFUN ASSOC-EQL (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CAR is EQUALP to ITEM."
  (PROG ()
        (IF (TYPEP ITEM '(OR (NOT NUMBER) FIXNUM SHORT-FLOAT))
            (RETURN (ASSQ ITEM IN-LIST)))
     L  (COND ((NULL IN-LIST) (RETURN NIL))
              ((NULL (CAR IN-LIST)))
              ((EQL ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
        (SETQ IN-LIST (CDR IN-LIST))
        (GO L)))

(DEFUN ASS (PRED ITEM LIST)
  "Return the first element of IN-LIST whose CAR matches ITEM using PRED.
The args passed to PRED are ITEM followed by the car from the list."
  (DO ((L LIST (CDR L))) ((NULL L))
    (AND (CAR L)
         (FUNCALL PRED ITEM (CAAR L))
         (RETURN (CAR L)))))

(DEFUN RASSOC (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CDR is EQUAL to ITEM."
  (DO ((L IN-LIST (CDR L))) ((NULL L))
    (AND (CAR L)
         (EQUAL ITEM (CDAR L))
         (RETURN (CAR L)))))

(DEFUN RASSOC-EQUAL (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CDR is EQUAL to ITEM."
  (DO ((L IN-LIST (CDR L))) ((NULL L))
    (AND (CAR L)
         (EQUAL ITEM (CDAR L))
         (RETURN (CAR L)))))

(DEFUN RASSOC-EQUALP (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CDR is EQUAL to ITEM."
  (DO ((L IN-LIST (CDR L))) ((NULL L))
    (AND (CAR L)
         (EQUALP ITEM (CDAR L))
         (RETURN (CAR L)))))

(DEFUN RASSOC-EQL (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CDR is EQUAL to ITEM."
  (DO ((L IN-LIST (CDR L))) ((NULL L))
    (AND (CAR L)
         (EQL ITEM (CDAR L))
         (RETURN (CAR L)))))

(DEFUN RASSQ (ITEM IN-LIST)
  "Return the first element of IN-LIST whose CDR is EQ to ITEM."
  (DO ((L IN-LIST (CDR L))) ((NULL L))
    (AND (CAR L)
         (EQ ITEM (CDAR L))
         (RETURN (CAR L)))))

(DEFUN RASS (PRED ITEM IN-LIST)
  "Return the first element of IN-LIST whose CDR matches ITEM using PRED.
The args passed to PRED are the ITEM followed by the cdr from the list."
  (DO ((L IN-LIST (CDR L))) ((NULL L))
    (AND (CAR L)
         (FUNCALL PRED ITEM (CDAR L))
         (RETURN (CAR L)))))

(DEFUN ASSQ-CAREFUL (KEY IN-LIST)
  "Like ASSQ, but elements of IN-LIST that are not lists are just ignored (no error)."
  (PROG ()
     L  (COND ((ATOM IN-LIST) (RETURN NIL))
              ((ATOM (CAR IN-LIST)))
              ((EQ KEY (CAAR IN-LIST))
               (RETURN (CAR IN-LIST))))
        (SETQ IN-LIST (CDR IN-LIST))
        (GO L)))

(DEFUN ASSOC-CAREFUL (KEY IN-LIST)
  "Like ASSOC-EQUAL, but elements of IN-LIST that are not lists are just ignored (no error)."
  (PROG ()
     L  (COND ((ATOM IN-LIST) (RETURN NIL))
              ((ATOM (CAR IN-LIST)))
              ((EQUAL KEY (CAAR IN-LIST))
               (RETURN (CAR IN-LIST))))
        (SETQ IN-LIST (CDR IN-LIST))
        (GO L)))

;;; Microcoded in System 98.  Don't delete, may need later.
;(DEFUN MEMBER-EQL (ITEM LIST)
;  (IF (TYPEP ITEM '(OR (NOT NUMBER) FIXNUM))
;      (MEMQ ITEM LIST)
;    (LOOP FOR X ON LIST DO (WHEN (EQL (CAR X) ITEM) (RETURN X)))))

;;; Microcoded in System 99.  Don't delete, may need later.
;(DEFUN MEMBER (ITEM IN-LIST)
;  "Return non-NIL if IN-LIST has an element EQUAL to ITEM.
;The value is actually the link of IN-LIST whose CAR is that element."
;  (COND ((OR (FIXNUMP ITEM)
;            (SYMBOLP ITEM))
;        (MEMQ ITEM IN-LIST))
;       (T
;        (DO ((X IN-LIST (CDR X)))
;            ((NULL X))
;          (IF (EQUAL (CAR X) ITEM) (RETURN X))))))

;;; Microcoded in System 99.  Don't delete, may need later.
;(DEFUN MEMBER-EQUAL (ITEM IN-LIST)
;  "Return non-NIL if IN-LIST has an element EQUAL to ITEM.
;The value is actually the link of IN-LIST whose CAR is that element."
;  (COND ((OR (FIXNUMP ITEM)
;            (SYMBOLP ITEM))
;        (MEMQ ITEM IN-LIST))
;       (T
;        (DO ((X IN-LIST (CDR X)))
;            ((NULL X))
;          (IF (EQUAL (CAR X) ITEM) (RETURN X)))))) )

(DEFUN MEMBER-EQUALP (ITEM IN-LIST)
  "Return non-NIL if IN-LIST has an element EQUALP to ITEM.
The value is actually the link of IN-LIST whose car is that element."
  (DO ((X IN-LIST (CDR X)))
      ((NULL X))
    (IF (EQUALP (CAR X) ITEM) (RETURN X))))

(DEFUN MEM (PRED ITEM LIST)
  "Return non-NIL if IN-LIST has an element which matches ITEM using PRED.
The value is actually the link of IN-LIST whose car is that element.
The args passed to PRED are the ITEM followed by the element of the list."
  (DO ((L LIST (CDR L)))
      ((NULL L))
    (AND (FUNCALL PRED ITEM (CAR L))
         (RETURN L))))

;;; (MEMASS PRED ITEM LIST)  (MEM PRED (ASS PRED ITEM LIST) LIST) but twice as fast.
(DEFUN MEMASS (PRED ITEM LIST)
  "Return non-NIL if IN-LIST has an element whose car matches ITEM using PRED.
The value is actually the link of IN-LIST whose car is that element.
The args passed to PRED are the ITEM followed by the element of the list."
  (DO ((L LIST (CDR L)))
      ((NULL L))
    (AND (FUNCALL PRED ITEM (CAAR L))
         (RETURN L))))

(DEFUN MEMASSQ (ITEM LIST)
  "Return non-NIL if LIST has an element whose car is EQ to ITEM.
The value is actually the link of IN-LIST whose car is that element."
  (DO ((L LIST (CDR L)))
      ((NULL L))
    (AND (EQ ITEM (CAAR L)) (RETURN L))))

(DEFUN TAILP (TAIL LIST)
  "Return non-NIL if TAIL can be reached from LIST by cdr'ing."
  (DO ((LIST LIST (CDR LIST))) ((NULL LIST))
    (AND (EQ TAIL LIST)
         (RETURN T))))

;;; MEM and ASS are special cases of this, which is to TAILP as MEM is to MEMQ.
(DEFUN PRED-TAILP (PRED TAIL LIST)
  "Cdr down LIST to the first link that matches TAIL using PRED.
The args passed to PRED are the ITEM followed by the link of the list."
  (DO ((LIST LIST (CDR LIST))) ((NULL LIST))
    (AND (FUNCALL PRED TAIL LIST)
         (RETURN TAIL))))

;; find-position-in-list (ie :test #'eq) is ucoded.
(DEFUN FIND-POSITION-IN-LIST-EQL (ITEM IN-LIST)
  "Return the numeric position of the first element of IN-LIST that is EQL to ITEM.
The first element is position 0.  Returns NIL if no match is found."
  (DO ((L IN-LIST (CDR L))
       (C 0 (1+ C)))
      ((NULL L))
    (AND (EQL ITEM (CAR L)) (RETURN C))))

(DEFUN FIND-POSITION-IN-LIST-EQUAL (ITEM IN-LIST)
  "Return the numeric position of the first element of IN-LIST that is EQUAL to ITEM.
The first element is position 0.  Returns NIL if no match is found."
  (DO ((L IN-LIST (CDR L))
       (C 0 (1+ C)))
      ((NULL L))
    (AND (EQUAL ITEM (CAR L)) (RETURN C))))

(DEFUN NLEFT (N L &OPTIONAL TAIL)
  "If TAIL is a link in the list L, back up N cdrs from TAIL.
The value is a link in the list L, which, if cdr'd N times, gives TAIL.
If TAIL is NIL, the values is the last N elements of L.
If TAIL is not a link in L, or L is too short, the value is NIL."
  (DO ((L1 L (CDR L1))
       (L2 (NTHCDR N L) (CDR L2)))
      ((EQ L2 TAIL) L1)
    (AND (NULL L2) (RETURN NIL))))

(DEFUN BUTLAST (LIST &OPTIONAL (N 1))
  "Return a list which has all the elements of LIST except the last one (or N)."
  (FIRSTN (MAX 0 (- (LENGTH LIST) N)) LIST))

;;; LDIFF as in Interlisp:  applied to (A B C D E) and (D E), it returns (A B C).
(DEFUN LDIFF (LIST TAIL &AUX VALUE-LENGTH)
  "Return a copy of the part of LIST that precedes TAIL.
If TAIL is not a link in LIST, a copy of all of LIST is returned."
  (UNLESS VALUE-LENGTH
    (DO ((LTAIL LIST (CDR LTAIL))
         (COUNT 0 (1+ COUNT)))
        ((OR (NULL LTAIL) (EQ LTAIL TAIL))
         (SETQ VALUE-LENGTH COUNT))))
  (FIRSTN VALUE-LENGTH LIST))

;;; FIRSTN of a number and a list returns the first that many elements of the list.
;;; If the list isn't that long, it is extended with NILs.  Like Take in APL.
(DEFUN FIRSTN (N LIST)
  "Return a list containing the first N elements of LIST."
  (LET ((NEW-LIST (MAKE-LIST N)))
    (DO ((LIST LIST (CDR LIST))
         (NEW-LIST NEW-LIST (CDR NEW-LIST)))
        ((OR (NULL LIST) (NULL NEW-LIST)))
      (RPLACA NEW-LIST (CAR LIST)))
    NEW-LIST))

(DEFUN ZL:DELETE (ITEM LIST &OPTIONAL (TIMES MOST-POSITIVE-FIXNUM) &AUX LL PL)
  "Alter LIST so that elements EQUAL to ITEM are no longer present.
If the third argument is a positive number, only the first that many
elements that are EQUAL to ITEM are eliminated.
The alteration is done by changing cdr pointers."
  (PROG ()
     A  (COND ((OR (ZEROP TIMES) (ATOM LIST))
               (GO R))
              ((EQUAL ITEM (CAR LIST))
               (POP LIST)
               (DECF TIMES)
               (GO A)))
        (SETQ LL LIST)
     B  (COND ((OR (ZEROP TIMES) (ATOM LL))
               (GO R))
              ((EQUAL ITEM (CAR LL))
               (RPLACD PL (CDR LL))
               (DECF TIMES))
              ((SETQ PL LL)))
        (POP LL)
        (GO B)
     R  (RETURN LIST)))

(DEFUN DEL (PRED ITEM LIST &OPTIONAL (TIMES MOST-POSITIVE-FIXNUM) &AUX LL PL)
  "Alter LIST so that elements matching ITEM using PRED are no longer present.
If the third argument is a positive number,
only the first that many elements that match are eliminated.
The alteration is done by changing cdr pointers.
The args passed to PRED are ITEM followed by the element of LIST."
  (PROG ()
     A  (COND ((OR (ZEROP TIMES) (ATOM LIST))
               (GO R))
              ((FUNCALL PRED ITEM (CAR LIST))
               (POP LIST)
               (DECF TIMES)
               (GO A)))
        (SETQ LL LIST)
     B  (COND ((OR (ZEROP TIMES) (ATOM LL))
               (GO R))
              ((FUNCALL PRED ITEM (CAR LL))
               (RPLACD PL (CDR LL))
               (DECF TIMES))
              ((SETQ PL LL)))
        (POP LL)
        (GO B)
     R  (RETURN LIST)))

(DEFUN DEL-IF-NOT (PRED LIST)
  "Destructively remove all elements of LIST that don't satisfy PRED."
  (PROG (LST OLST)
     A  (COND ((ATOM LIST) (RETURN LIST))
              ((FUNCALL PRED (CAR LIST)))
              (T
               (SETQ LIST (CDR LIST))
               (GO A)))
        (SETQ OLST (SETQ LST LIST))
     B  (SETQ LST (CDR LST))
        (COND ((ATOM LST) (RETURN LIST))
              ((FUNCALL PRED (CAR LST))
               (SETQ OLST LST))
              (T
               (RPLACD OLST (CDR LST))))
        (GO B)))

(DEFUN DEL-IF (PRED LIST)
  "Destructively remove all elements of LIST that satisfy PRED."
  (PROG (LST OLST)
     A  (COND ((ATOM LIST) (RETURN LIST))
              ((FUNCALL PRED (CAR LIST))
               (SETQ LIST (CDR LIST))
               (GO A)))
        (SETQ OLST (SETQ LST LIST))
     B  (SETQ LST (CDR LST))
        (COND ((ATOM LST) (RETURN LIST))
              ((FUNCALL PRED (CAR LST))
               (RPLACD OLST (CDR LST)))
              (T
               (SETQ OLST LST)))
        (GO B)))

(DEFUN DELETE-EQUALP (ITEM LIST &OPTIONAL (TIMES MOST-POSITIVE-FIXNUM) &AUX LL PL)
  "Alter LIST so that elements EQUALP to ITEM are no longer present.
If the third argument is a positive number, only the first that many
elements that are EQUALP to ITEM are eliminated.
The alteration is done by changing cdr pointers."
  (PROG ()
     A  (COND ((OR (ZEROP TIMES) (ATOM LIST))
               (GO R))
              ((EQUALP ITEM (CAR LIST))
               (POP LIST)
               (DECF TIMES)
               (GO A)))
        (SETQ LL LIST)
     B  (COND ((OR (ZEROP TIMES) (ATOM LL))
               (GO R))
              ((EQUALP ITEM (CAR LL))
               (RPLACD PL (CDR LL))
               (DECF TIMES))
              ((SETQ PL LL)))
        (POP LL)
        (GO B)
     R  (RETURN LIST)))

;;; This copies only as much as it needs to in order to avoid bashing the original list
(DEFUN ZL:REMOVE (ITEM LIST &OPTIONAL (TIMES MOST-POSITIVE-FIXNUM))
  "Return a list like LIST except that elements EQUAL to ITEM are missing.
TIMES controls how many such elements are missing;
after that many have been eliminated, the rest are left alone."
  (IF (PLUSP TIMES)
      (LOOP WITH HEAD = (VARIABLE-LOCATION LIST)
            AS TAIL = (LOOP FOR L ON (CDR HEAD)         ;(MEMBER-EQUAL ITEM (CDR HEAD)) faster
                         WHEN (EQUAL (CAR L) ITEM) RETURN L)
            UNTIL (NULL TAIL)
         DO (LOOP UNTIL (EQ (CDR HEAD) TAIL)
               DO (RPLACD HEAD (SETQ HEAD (CONS (CADR HEAD) (CDDR HEAD)))))
            (RPLACD HEAD (CDR TAIL))
         UNTIL (ZEROP (SETQ TIMES (1- TIMES)))))
  LIST)

;;; This copies only as much as it needs to in order to avoid bashing the original list
(DEFUN REMQ (ITEM LIST &OPTIONAL (TIMES MOST-POSITIVE-FIXNUM))
  "Return a list like LIST except that elements EQ to ITEM are missing.
TIMES controls how many such elements are missing;
after that many have been eliminated, the rest are left alone."
  (IF (PLUSP TIMES)
      (LOOP WITH HEAD = (VARIABLE-LOCATION LIST)
            AS TAIL = (MEMQ ITEM (CDR HEAD))
            UNTIL (NULL TAIL)
         DO (LOOP UNTIL (EQ (CDR HEAD) TAIL)
               DO (RPLACD HEAD (SETQ HEAD (CONS (CADR HEAD) (CDDR HEAD)))))
            (RPLACD HEAD (CDR TAIL))
         UNTIL (ZEROP (SETQ TIMES (1- TIMES)))))
  LIST)

;;; This copies only as much as it needs to in order to avoid bashing the original list
(DEFUN REM (PRED ITEM LIST &OPTIONAL (TIMES MOST-POSITIVE-FIXNUM))
  "Return a list like LIST except that elements matching ITEM using PRED are missing.
TIMES controls how many such elements are missing;
after that many have been eliminated, the rest are left alone.
The arguments passed to PRED are ITEM followed by the element of LIST."
  (IF (PLUSP TIMES)
      (LOOP WITH HEAD = (VARIABLE-LOCATION LIST)
            AS TAIL = (LOOP FOR L ON (CDR HEAD)
                         WHEN (FUNCALL PRED ITEM (CAR L)) RETURN L)
            UNTIL (NULL TAIL)
         DO (LOOP UNTIL (EQ (CDR HEAD) TAIL)
               DO (RPLACD HEAD (SETQ HEAD (CONS (CADR HEAD) (CDDR HEAD)))))
            (RPLACD HEAD (CDR TAIL))
         UNTIL (ZEROP (SETQ TIMES (1- TIMES)))))
  LIST)

(DEFF REM-IF-NOT 'SUBSET)
(DEFF REM-IF 'SUBSET-NOT)

(DEFUN ELIMINATE-DUPLICATES (L &OPTIONAL (PREDICATE #'EQ) &AUX (MARKER '(DUPLICATE)))
  "Destructively take out any duplicate elements in the list.
Leaves the first instance where it is and removes following instances."
  (DO ((L1 L (CDR L1)))
      ((NULL L1)
       (DELQ MARKER L))
    (OR (EQ (CAR L1) MARKER)
        (DO ((TEM (CDR L1) (CDR TEM)))
            ((NULL (SETQ TEM (MEM PREDICATE (CAR L1) TEM))))
          (SETF (CAR TEM) MARKER)))))

(DEFUN ZL:SUBST (NEW OLD S-EXP &AUX TEM)
  "Replace OLD with NEW in all occurrences within S-EXP, to all levels, copying as needed."
  (COND ((EQUAL OLD S-EXP) NEW)
        ((ATOM S-EXP) S-EXP)
        (T (SETQ S-EXP (COPYLIST S-EXP))
           (DO ((S S-EXP (CDR S))
                (PREV NIL S))
               ((ATOM S)
                (SETQ TEM (SUBST NEW OLD S))
                (OR (EQ TEM S) (RPLACD PREV TEM)))
             (RPLACA S (SUBST NEW OLD (CAR S))))
           S-EXP)))

(defun nsubst-eq-safe (new old sexp &optional previous-sexps &aux car cdr)
  "Like (nsubst new old sexp :test #'eq) except that it will always terminate
/(ie does not recurse down lists it has already looked at)"
  (cond ((eq sexp old) new)
        ((atom sexp) sexp)
        ((memq sexp previous-sexps) sexp)
        (t (with-stack-list* (previous-sexps sexp previous-sexps)
             (setq car (nsubst-eq-safe new old (car sexp) previous-sexps))
             (if (neq car (car sexp)) (setf (car sexp) car))
             (with-stack-list* (previous-sexps (car sexp) previous-sexps)
               (setq cdr (nsubst-eq-safe new old (cdr sexp) previous-sexps))
               (if (neq cdr (cdr sexp)) (setf (cdr sexp) cdr))))
           sexp)))

(DEFUN SUBLIS (ALIST TREE &KEY &OPTIONAL TEST TEST-NOT KEY)
  "Make multiple replacements in TREE, copying structure as needed.
ALIST specifies the replacements; each element's car is something to replace,
and the cdr is what to replace it with.
Each atom or subtree found anywhere in TREE is compared against each
object to be replaced.  If KEY is non-NIL, it is a function to apply
to that non-list from TREE to get the thing to actually compare.
TEST and TEST-NOT specify how to do comparison.
The value is a predicate which accepts two arguments.
If TEST-NOT is specified, an object is replaced if the predicate returns NIL.
If TEST is specified, an object is replaced if the predicate returns non-NIL."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (SUBLIS-1 ALIST TREE KEY (OR TEST-NOT TEST 'EQL) (NOT (NULL TEST-NOT)) NIL))

(DEFUN NSUBLIS (ALIST TREE &KEY &OPTIONAL TEST TEST-NOT KEY)
  "Structure-modifying version of SUBLIS.  Same as SUBLIS except modifies structure of TREE."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (SUBLIS-1 ALIST TREE KEY (OR TEST-NOT TEST 'EQL) (NOT (NULL TEST-NOT)) T))

(DEFUN SUBLIS-1 (ALIST TREE KEY PRED INVERTP DESTRUCTIVEP &AUX TEM)
  (IF (COND ((AND (NULL KEY) (OR (EQ PRED 'EQ) (EQ PRED #'EQ)))
             (SETQ TEM (ASSQ TREE ALIST)))
            (T
             (DOLIST (ELT ALIST)
               (IF (EQ INVERTP (NULL (FUNCALL PRED (CAR ELT)
                                              (IF KEY (FUNCALL KEY TREE) TREE))))
                   (RETURN (SETQ TEM ELT))))))
      (CDR TEM)
    (IF (ATOM TREE) TREE
      (LET ((NEWCAR (SUBLIS-1 ALIST (CAR TREE) KEY PRED INVERTP DESTRUCTIVEP))
            (NEWCDR (SUBLIS-1 ALIST (CDR TREE) KEY PRED INVERTP DESTRUCTIVEP)))
        (IF (NOT DESTRUCTIVEP)
            (IF (AND (EQL NEWCAR (CAR TREE))
                     (EQL NEWCDR (CDR TREE)))
                TREE
              (CONS NEWCAR NEWCDR))
          (SETF (CAR TREE) NEWCAR)
          (UNLESS (EQL (CDR TREE) NEWCDR)
            (SETF (CDR TREE) NEWCDR))
          TREE)))))

;;;; Full mapping functions

(DEFUN MAPCAR (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive elements, returns a list of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAAR LP))                               ;PASS CAR OF THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (RPLACD P (SETQ P (NCONS LP)))                  ;CONS IT ONTO LIST
        (GO L)))

(DEFUN MAPCAR-IN-AREA (&FUNCTIONAL FCN &EVAL AREA &REST LISTS)
  "Maps over successive elements, returns a list of the results.
 All consing done in AREA."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAAR LP))                               ;PASS CAR OF THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (RPLACD P (SETQ P (NCONS-in-area area LP)))     ;CONS IT ONTO LIST
        (GO L)))

(DEFUN MAPC (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive elements, returns second argument."
  (PROG (LP RES)
        (SETQ RES (CAR LISTS))                          ;RESULT WILL BE FIRST ARG
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 0)                      ;DESTINATION IGNORE
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN RES))              ;A LIST ENDS, RETURN SECOND ARG
        (%PUSH (CAAR LP))                               ;PASS CAR OF THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (GO L)))

(DEFUN MAPLIST (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive sublists, returns a list of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAR LP))                                ;PASS THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (RPLACD P (SETQ P (NCONS LP)))                  ;CONS IT ONTO LIST
        (GO L)))

(DEFUN MAPLIST-in-area (&FUNCTIONAL FCN &EVAL area &REST LISTS)
  "Maps over successive sublists, returns a list of the results.
  Cons in AREA."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAR LP))                                ;PASS THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (RPLACD P (SETQ P (NCONS-in-area area LP)))                     ;CONS IT ONTO LIST
        (GO L)))

(DEFUN MAPL (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive sublists, returns second argument."
  (PROG (LP RES)
        (SETQ RES (CAR LISTS))                          ;RESULT WILL BE FIRST ARG
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 0)                      ;DESTINATION IGNORE
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN RES))              ;A LIST ENDS, RETURN SECOND ARG
        (%PUSH (CAR LP))                                ;PASS THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (GO L)))

(DEFF MAP 'MAPL)

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:28
(DEFUN MAPCAN (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive elements, returns NCONC of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAAR LP))                               ;PASS CAR OF THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (and (null lp) (go l))                          ;if nil, ignore it
        (not (check-type lp list))                      ;if not a list error out...otherwise
        (RPLACD P LP)                                   ;CONC IT ONTO LIST
        (SETQ P (LAST LP))                              ;SAVE NEW CELL TO BE CONC'ED ONTO
        (GO L)))

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:29
(DEFUN MAPCON (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive sublists, returns NCONC of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAR LP))                                ;PASS THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (and (null lp) (go l))                          ;if nil, ignore it
        (not (check-type lp list))                      ;if not a list error out...otherwise
        (RPLACD P LP)                                   ;CONC IT ONTO LIST
        (SETQ P (LAST LP))                              ;SAVE NEW CELL TO BE CONC'ED ONTO
        (GO L)))

(DEFUN SUBSET (&FUNCTIONAL PRED LIST &EVAL &REST EXTRA-LISTS &AUX VALUE P LP)
  "Return a list of all elements of LIST for which PRED is true.
If extra args are supplied, their successive elements are passed
to PRED along with elements of LIST.  Unlike MAP, etc., we process
every element of LIST even if extra args are exhausted by cdr'ing."
  (SETQ P (LOCF VALUE))                         ;ACCUMULATE LIST IN P, VALUE
  (%ASSURE-PDL-ROOM (+ (LENGTH EXTRA-LISTS) 5)) ;Make sure %PUSH's don't lose.
  (DO () ((NULL LIST) VALUE)
    (SETQ LP EXTRA-LISTS)
    (%OPEN-CALL-BLOCK PRED 0 1)                 ;call with destination=stack.
    (%PUSH (CAR LIST))                          ;push next element of LIST.

    (DO () ((NULL LP))                          ;LP scans down the extra lists.
      (%PUSH (CAAR LP))                         ;Push car of each one.
      (POP (CAR LP))                            ;cdr this list.
      (POP LP))                                 ;advance to next list.

    (%ACTIVATE-OPEN-CALL-BLOCK)                 ;Make the call.
    (IF (%POP)                                  ;If value non-nil, put this one in the value.
        (RPLACD P (SETQ P (NCONS (CAR LIST)))))

    (POP LIST)))

(DEFUN SUBSET-NOT (&FUNCTIONAL PRED LIST &EVAL &REST EXTRA-LISTS &AUX VALUE P LP)
  "Return a list of all elements of LIST for which PRED is false.
If extra args are supplied, their successive elements are passed
to PRED along with elements of LIST.  Unlike MAP, etc., we process
every element of LIST even if extra args are exhausted by cdr'ing."
  (SETQ P (LOCF VALUE))                         ;ACCUMULATE LIST IN P, VALUE
  (%ASSURE-PDL-ROOM (+ (LENGTH EXTRA-LISTS) 5)) ;Make sure %PUSH's don't lose.
  (DO () ((NULL LIST) VALUE)
    (SETQ LP EXTRA-LISTS)
    (%OPEN-CALL-BLOCK PRED 0 1)                 ;call with destination=stack.
    (%PUSH (CAR LIST))                          ;push next element of LIST.

    (DO () ((NULL LP))                          ;LP scans down the extra lists.
      (%PUSH (CAAR LP))                         ;Push car of each one.
      (POP (CAR LP))                            ;cdr this list.
      (POP LP))                                 ;advance to next list.

    (%ACTIVATE-OPEN-CALL-BLOCK)                 ;Make the call.
    (IF (NOT (%POP))                            ;If value nil, put this one in the value.
        (RPLACD P (SETQ P (NCONS (CAR LIST)))))

    (POP LIST)))

(defparameter function-start-symbols
              '(lambda subst cli:subst named-lambda named-subst curry-before curry-after)
  "A list starting with one of these symbols can be a function.")

(DEFUN FUNCTIONP (X &OPTIONAL ALLOW-SPECIAL-FORMS)
  "T if X is a function.  ALLOW-SPECIAL-FORMS=T says count special forms as functions.
Closures and select-methods are considered functions,
but arrays, entities, instances and stack groups are not."
  (PROG ()
     LOOP
        (RETURN (CASE (%DATA-TYPE X)
                  ((#.DTP-FEF-POINTER #.DTP-U-ENTRY)
                   (IF (BIT-TEST (LOGIOR %ARG-DESC-QUOTED-REST %ARG-DESC-FEF-QUOTE-HAIR)
                                 (%ARGS-INFO X))
                       (FERROR NIL "Obsolete special form. Recompile the definition of ~S" X))
                   T)
                  ((#.DTP-LIST)
                   (COND ((EQ (CAR X) 'LAMBDA)
                          (IF (MEMQ '&QUOTE (CADR X)) (FERROR NIL "Obsolete special form: ~S" X))
                          T)
                         ((EQ (CAR X) 'NAMED-LAMBDA)
                          (IF (MEMQ '&QUOTE (CADDR X)) (FERROR NIL "Obsolete special form: ~S" X))
                          T)
                         ((MEMQ (CAR X) '(SUBST CLI:SUBST NAMED-SUBST
                                          CURRY-BEFORE CURRY-AFTER)) T)
                         ((EQ (CAR X) 'MACRO) ALLOW-SPECIAL-FORMS)
                         ((LAMBDA-MACRO-CALL-P X)
                          (SETQ X (LAMBDA-MACRO-EXPAND X))
                          (GO LOOP))))
                  ((#.DTP-SELECT-METHOD #.DTP-CLOSURE) T)
                  ((#.DTP-SYMBOL)
                   (COND ((NOT (FBOUNDP X))
                          (AND ALLOW-SPECIAL-FORMS
                               (INTERPRETER-SPECIAL-FORM X)
                               T))
                         ((ARRAYP (SETQ X (SYMBOL-FUNCTION X))))
                         (T (GO LOOP))))))))

(defun cli:functionp (object)
  (case (%data-type object)
           ((#.dtp-symbol #.dtp-closure #.dtp-select-method #.dtp-fef-pointer #.dtp-u-entry) t)
           ((#.dtp-list)
            (and (lisp:member (car object) function-start-symbols)
                 t))))

(DEFUN MACRO-FUNCTION (FSPEC &OPTIONAL ENVIRONMENT &AUX DEF)
  "If FSPEC has a function definition which is a macro, return the expander function; else NIL."
  (COND ((AND (SYMBOLP FSPEC) (SETQ DEF (FSYMEVAL-IN-ENVIRONMENT FSPEC ENVIRONMENT NIL)))
         (IF (SYMBOLP DEF) (MACRO-FUNCTION DEF ENVIRONMENT)
           (IF (EQ (CAR-SAFE DEF) 'MACRO)
               (CDR DEF))))
        ((FDEFINEDP FSPEC)
         (SETQ DEF (FDEFINITION FSPEC))
         (COND ((EQ (CAR-SAFE DEF) 'MACRO)
                (CDR DEF))
               ((AND (SYMBOLP FSPEC)
                     (CDR (GET FSPEC 'ALTERNATE-MACRO-DEFINITION))))
               ((SYMBOLP DEF)
                (MACRO-FUNCTION DEF))
               (T NIL)))
        ((SYMBOLP FSPEC)
         (CDR (GET FSPEC 'ALTERNATE-MACRO-DEFINITION)))
        (T NIL)))

;;; Used by SETF of MACRO-FUNCTION
(DEFUN SET-MACRO-FUNCTION (FSPEC DEFINITION)
  (FDEFINE (IF (SPECIAL-FORM-P FSPEC)
               `(:PROPERTY ,FSPEC ALTERNATE-MACRO-DEFINITION)
               FSPEC)
           (CONS 'MACRO DEFINITION) NIL)
  definition)

(DEFPARAMETER *COMMON-LISP-ONE-TRUE-AND-ONLY-OFFICIAL-SPECIAL-FORMS*
              'LISP:(BLOCK CATCH COMPILER-LET DECLARE EVAL-WHEN FLET FUNCTION GO
                           COND LABELS LET LET* MACROLET MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1 PROGN
                           PROGV QUOTE RETURN-FROM SETQ TAGBODY THE THROW UNWIND-PROTECT)
  "So decree the Gang of Five")

(defun common-lisp-special-form-p (symbol)
 (and (memq symbol *common-lisp-one-true-and-only-official-special-forms*)
      t))

(defun implementation-special-form-p (symbol)
  (and (interpreter-special-form symbol)
       t))

(defun special-form-p (symbol &optional environment)
  "T if SYMBOL has a function definition taking unevaluated arguments.
This does not include macros. To test for them, use MACRO-FUNCTION."
  (if (fsymeval-in-environment symbol environment nil)
      nil                                       ;we don't allow (flet ((foo (... &quote ...)
    (or (common-lisp-special-form-p symbol)
        (implementation-special-form-p symbol))))

(defun fsymeval-in-environment (symbol environment check-symbol-function &aux mumble)
  "Returns SYMBOL's function or macro definition within ENVIRONMENT,
If CHECK-SYMBOL-FUNCTION is T we take SYMBOL-FUNCTION of SYMBOL if the function is not
defined by ENVIRONMENT, otherwise we return NIL if the environment doesn't define it."
  (dolist (frame (car environment) (if check-symbol-function (symbol-function symbol) nil))
    (and (setq mumble (get-location-or-nil (locf frame) (locf (symbol-function symbol))))
         (return (car mumble)))))

;;; Note: this is different from (macro-function symbol environment),
;;;  as this doesn't look at alternate-macro-definitions
(defun macro-in-environment-p (symbol environment &aux tem)
  "Returns SYMBOL's macroexpansion function if it is defined as a macro (either
within ENVIRONMENT or gloablly), or NIL if it does not have a macro definition"
  (if (setq tem (fsymeval-in-environment symbol environment nil))
      (if (eq (car-safe tem) 'macro) (cadr tem))
    (and (fboundp symbol)
         (eq (car-safe (setq tem (symbol-function symbol))) 'macro)
         tem)))

(DEFUN FUNCTION-NAME (FUNCTION &OPTIONAL
                      RETURN-FLAVOR-NAMES-FLAG)
  "Return FUNCTION's name, if known.  Otherwise return FUNCTION.
RETURN-FLAVOR-NAMES-FLAG, if T, says that if FUNCTION is a flavor instance
then the flavor name should be returned.  Otherwise FUNCTION is returned.
The second value is T if a name was known."
  (TYPECASE FUNCTION
    (COMPILED-FUNCTION
     (VALUES (%P-CONTENTS-OFFSET FUNCTION %FEFHI-FCTN-NAME) T))
    (MICROCODE-FUNCTION
     (VALUES (MICRO-CODE-ENTRY-NAME-AREA (%POINTER FUNCTION)) T))
    (STACK-GROUP
     (VALUES (ARRAY-LEADER FUNCTION SG-NAME) T))
    (SELECT-METHOD
     ;; See if any function in the select method list
     ;; has a name which is (:SELECT-METHOD something).
     ;; If so, that something must be our name!
     (DO ((ALIST (%MAKE-POINTER DTP-LIST FUNCTION) (CDR ALIST)))
         ((NULL ALIST)
          FUNCTION)
       (LET ((ELEM (CAR ALIST)))
         (AND (CONSP ELEM)
              (LET ((SUBFUNCTION-NAME (FUNCTION-NAME (CDR ELEM))))
                (cond ((AND (CONSP SUBFUNCTION-NAME)
                            (EQ (CAR SUBFUNCTION-NAME) ':SELECT-METHOD))
                       (RETURN (VALUES (CADR SUBFUNCTION-NAME) T)))))))))
    (CONS
     (COND ((MEMQ (CAR FUNCTION) '(NAMED-LAMBDA NAMED-SUBST))
            (VALUES
              (IF (SYMBOLP (CADR FUNCTION))
                  (CADR FUNCTION)
                (CAADR FUNCTION))
              T))
           ((EQ (CAR FUNCTION) 'MACRO)
            (FUNCTION-NAME (CDR FUNCTION)
                           RETURN-FLAVOR-NAMES-FLAG))
           (T FUNCTION)))
    ((OR CLOSURE ENTITY)
     (FUNCTION-NAME (CLOSURE-FUNCTION FUNCTION)))
    (INSTANCE
     ;; Return the flavor name.  Best we can do.
     (IF RETURN-FLAVOR-NAMES-FLAG
         (VALUES (TYPE-OF FUNCTION) T)
       FUNCTION))
    (SYMBOL
     (COND ((NULL FUNCTION) NIL)
           ((AND (FDEFINEDP FUNCTION)
                 (SYMBOLP (FDEFINITION FUNCTION)))
            (FUNCTION-NAME (FDEFINITION FUNCTION)))
           (T (VALUES FUNCTION T))))
    (T FUNCTION)))

(DEFUN LAMBDA-MACRO-CALL-P (FCTN)
  "Return T if FCTN, a function, is a use of a lambda macro."
  (AND (CONSP FCTN) (SYMBOLP (CAR FCTN))
       (GETDECL (CAR FCTN) 'LAMBDA-MACRO)))

(DEFUN LAMBDA-MACRO-EXPAND (FCTN)
  "If FCTN is a function which is a use of a lambda-macro, return its expansion."
  (DO ()
      ((NOT (LAMBDA-MACRO-CALL-P FCTN))
       FCTN)
    (SETQ FCTN (FUNCALL (GETDECL (CAR FCTN) 'LAMBDA-MACRO) FCTN))))

;;; Function spec handler for :LAMBDA-MACRO
(DEFPROP :LAMBDA-MACRO LAMBDA-MACRO-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN LAMBDA-MACRO-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((SYMBOL (SECOND FUNCTION-SPEC)))
    (IF (NOT (AND (= (LENGTH FUNCTION-SPEC) 2) (SYMBOLP SYMBOL)))
        (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
          (FERROR 'SYS:INVALID-FUNCTION-SPEC "The function spec ~S is invalid."
                  FUNCTION-SPEC))
      (CASE FUNCTION
        (VALIDATE-FUNCTION-SPEC T)
        (FDEFINE (PUTPROP SYMBOL ARG1 'LAMBDA-MACRO))
        ((FDEFINITION FDEFINEDP) (GET SYMBOL 'LAMBDA-MACRO))
        (FDEFINITION-LOCATION (LOCF (GET SYMBOL 'LAMBDA-MACRO)))        ;Not perfect, but close
        (FUNDEFINE (REMPROP SYMBOL 'LAMBDA-MACRO))
        (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))


;;;; Number comparison functions.

;MAX and MIN must be written with a single REST arg, otherwise,
; the hack of (APPLY #'MAX xx) can lose because it will try to
; copy the arglist to the stack.
(DEFUN MAX (&REST NUMBERS)
  "Return the largest of the arguments."
  (LET ((ARG0 (CAR NUMBERS)))
    (CHECK-TYPE ARG0 NON-COMPLEX-NUMBER)
    (DO ((REST NUMBERS (CDR REST)))
        ((NULL REST) ARG0)
      (SETQ ARG0 (MAX ARG0 (CAR REST))))))

(DEFUN MIN (&REST NUMBERS)
  "Return the smallest of the arguments."
  (LET ((ARG0 (CAR NUMBERS)))
    (CHECK-TYPE ARG0 NON-COMPLEX-NUMBER)
    (DO ((REST (CDR NUMBERS) (CDR REST)))
        ((NULL REST) ARG0)
      (SETQ ARG0 (MIN ARG0 (CAR REST))))))

(defun min-ignoring-nils (&rest numbers)
  "Return the smallest of the arguments, ignoring any NILs.  Result NIL if all arguments are."
  (let ((ans nil))
    (do ((rest numbers (cdr rest)))
        ((null rest) ans)
      (cond ((null (car rest)))
            ((null ans) (setq ans (car rest)))
            (t (setq ans (min ans (car rest))))))))

;(DEFUN GREATERP (&REST NUMBERS)
;  "Return T if the arguments are in strictly decreasing numerical order."
;  (DO ((A NIL C)
;       (B NUMBERS (CDR B))
;       (C))
;      ((NULL B) T)
;    (SETQ C (CAR B))
;    (OR (NULL A) (> A C) (RETURN NIL))))

(DEFUN GREATERP (&REST NUMBERS)
  "Return T if the arguments are in strictly decreasing numerical order."
  (prog (a (b (cdr numbers)) c)
        (if (null b) (return t))                ;T if 0 or 1 arg
        (setq a (car numbers))
      again
        (setq c (car b))
        (if (<= a c) (return nil))
        (setq b (cdr b))
        (if (null b) (return t))
        (setq a c)
        (go again)))

(DEFF > 'GREATERP)

;(DEFUN LESSP (&REST NUMBERS)
;  "Return T if the arguments are in strictly increasing numerical order."
;  (DO ((A NIL C)
;       (B NUMBERS (CDR B))
;       (C))
;      ((NULL B) T)
;    (SETQ C (CAR B))
;    (OR (NULL A) (< A C) (RETURN NIL))))

(defun lessp (&rest numbers)
  "Return T if the arguments are in strictly increasing numerical order."
  (prog (a (b (cdr numbers)) c)
        (if (null b) (return t))
        (setq a (car numbers))
     again
        (setq c (car b))
        (if (>= a c) (return nil))
        (setq b (cdr b))
        (if (null b) (return t))
        (setq a c)
        (go again)))

(DEFF < 'LESSP)

(DEFUN = (&REST NUMBERS)
  "Return T if the arguments are all numerically equal."
  (DOLIST (N (CDR NUMBERS) T)
    (UNLESS (= N (CAR NUMBERS))
      (RETURN NIL))))

;(DEFUN <= (&REST NUMBERS)
;  "Return T if the arguments are in nondecreasing numerical order."
;  (DO ((A NIL C)
;       (B NUMBERS (CDR B))
;       (C))
;      ((NULL B) T)
;    (SETQ C (CAR B))
;    (UNLESS (NULL A)
;      (WHEN (> A C) (RETURN NIL)))))

(defun <= (&rest numbers)
  "Return T if the arguments are in nondecreasing numerical order."
  (prog (a (b (cdr numbers)) c)
        (if (null b) (return t))
        (setq a (car numbers))
     again
        (setq c (car b))
        (if (> a c) (return nil))
        (setq b (cdr b))
        (if (null b) (return t))
        (setq a c)
        (go again)))

(DEFF  '<=)

;(DEFUN >= (&REST NUMBERS)
;  "Return T if the arguments are in nonincreasing numerical order."
;  (DO ((A NIL C)
;       (B NUMBERS (CDR B))
;       (C))
;      ((NULL B) T)
;    (SETQ C (CAR B))
;    (UNLESS (NULL A)
;      (WHEN (< A C) (RETURN NIL)))))

(defun >= (&rest numbers)
  "Return T if the arguments are in nonincreasing numerical order."
  (prog (a (b (cdr numbers)) c)
        (if (null b) (return t))
        (setq a (car numbers))
     again
        (setq c (car b))
        (if (< a c) (return nil))
        (setq b (cdr b))
        (if (null b) (return t))
        (setq a c)
        (go again)))

(DEFF  '>=)

(DEFUN //= (&REST NUMBERS)
  "Return T if no two arguments are equal."
  (DO ((REST NUMBERS (CDR REST)))
      ((NULL (CDR REST)) T)
    (WHEN (MEM '= (CAR REST) (CDR REST))
      (RETURN NIL))))

(DEFF  '//=)

;;;; Arithmetic functions.

(DEFUN PLUS (&REST NUMBERS)
  "Return the sum of the arguments."
    (DO ((NUMBERS NUMBERS (CDR NUMBERS))
         (ANS 0))
        ((NULL NUMBERS) ANS)
      (SETQ ANS (+ ANS (CAR NUMBERS)))))

(DEFF +  'PLUS)
(DEFF +$ 'PLUS)

(DEFUN DIFFERENCE (NUMBER &REST NUMBERS)
  "Return the first argument minus the remaining arguments."
    (DO ((NUMBERS NUMBERS (CDR NUMBERS))
         (ANS NUMBER))
        ((NULL NUMBERS) ANS)
      (SETQ ANS (- ANS (CAR NUMBERS)))))

(DEFUN - (NUMBER &REST NUMBERS)
 "Return the negation of a single argument, or the first argument minus the rest."
  (COND ((NULL NUMBERS) (MINUS NUMBER))
        ((DO ((NUMBERS NUMBERS (CDR NUMBERS))
              (ANS NUMBER))
             ((NULL NUMBERS) ANS)
           (SETQ ANS (- ANS (CAR NUMBERS)))))))

(DEFF -$ '-)

(DEFUN TIMES (&REST NUMBERS)
  "Return the product of the arguments."
    (DO ((NUMBERS NUMBERS (CDR NUMBERS))
         (ANS 1))
        ((NULL NUMBERS) ANS)
      (SETQ ANS (* ANS (CAR NUMBERS)))))

(DEFF *  'TIMES)
(DEFF *$ 'TIMES)

(DEFUN QUOTIENT (NUMBER &REST NUMBERS)
  "Return the first argument divided by the rest.
If all arguments are fixnums, the value is also a fixnum, truncated!
If you want a correct numeric division, float one argument."
  (DO ((NUMBERS NUMBERS (CDR NUMBERS))
       (ANS NUMBER))
      ((NULL NUMBERS) ANS)
    (SETQ ANS (// ANS (CAR NUMBERS)))))

(DEFUN MOD (DIVIDEND MODULUS)
  "Return DIVIDEND taken in modulus MODULUS.
It is the same as the second value (the remainder) from (FLOOR DIVIDEND MODULUS).
The result will be in the range from zero (inclusive) to MODULUS (exclusive) with the same
sign as MODULUS."
  (MOD DIVIDEND MODULUS))

(DEFUN CLI:REM (DIVIDEND MODULUS)
  "Return DIVIDEND taken in modulus MODULUS.
It is the same as the second value (the remainder) from (TRUNCATE DIVIDEND MODULUS).
It has the same sign as the dividend, if it is not zero."
  (CLI:REM DIVIDEND MODULUS))

(DEFUN // (NUMBER &REST NUMBERS)
  "Return the reciprocal of one argument, or the first argument divided by the rest.
If all arguments are fixnums, the value is also a fixnum, truncated!
If you want a correct numeric division, float one argument."
  (COND ((NULL NUMBERS) (// 1 NUMBER))
        ((DO ((NUMBERS NUMBERS (CDR NUMBERS))
              (ANS NUMBER))
             ((NULL NUMBERS) ANS)
           (SETQ ANS (// ANS (CAR NUMBERS)))))))

(DEFF //$ '//)

(DEFUN CLI:// (NUMBER &REST NUMBERS)
  "Return the reciprocal of one argument, or the first argument divided by the rest.
Division of fixnums returns a rational."
  (COND ((NULL NUMBERS) (%DIV 1 NUMBER))
        ((DO ((NUMBERS NUMBERS (CDR NUMBERS))
              (ANS NUMBER))
             ((NULL NUMBERS) ANS)
           (SETQ ANS (%DIV ANS (CAR NUMBERS)))))))

(DEFUN \\ (&REST NUMBERS)
  "Return the greatest common divisor of the arguments."
  (LOOP WITH ANSWER = 0
        FOR Z IN NUMBERS
     DO (SETQ ANSWER (SYS:INTERNAL-\\ ANSWER Z))
     FINALLY (RETURN ANSWER)))

(DEFF GCD '\\)

(DEFUN LCM (NUMBER &REST NUMBERS)
  "Return the least common multiple of all the numbers."
  (DO ((VALUE (ABS NUMBER))
       (REST NUMBERS (CDR REST)))
      ((NULL REST) VALUE)
    (SETQ VALUE (IF (OR (ZEROP VALUE) (ZEROP (CAR REST)))
                    (RETURN 0)
                  (TRUNCATE (ABS (* VALUE (CAR REST)))
                            (\\ VALUE (CAR REST)))))))


;;; This could use being faster for negative numbers.
(DEFUN INTEGER-LENGTH (INTEGER)
  "Number of bits in field needed to store INTEGER without truncating it.
For nonnegative integers, this gives the exact number of bits
needed in an unsigned field.
For all integers, it gives the number of bits aside from the sign bit
needed in a signed field."
  (CHECK-TYPE INTEGER INTEGER)
  (HAULONG
    (IF (MINUSP INTEGER)
        (1+ INTEGER)
      INTEGER)))

(DEFF ADD1 '1+)
(DEFF SUB1 '1-)
(DEFF 1+$  '1+)
(DEFF 1-$  '1-)
(DEFF REMAINDER '\)
(DEFF ^$   '^)
(DEFF EXPT '^)

(DEFUN FIXR (FLONUM)
  "Convert the argument to the nearest fixnum."
  (VALUES (ROUND FLONUM)))

;;;; Bitwise boolean operations on numbers.

(DEFCONSTANT BOOLE-CLR 0
  "As first arg to BOOLE, makes all output bits be zero.")

(DEFCONSTANT BOOLE-SET 17
  "As first arg to BOOLE, makes all output bits be one.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:33
(DEFCONSTANT BOOLE-1 5
  "As first arg to BOOLE, makes output be the first input.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:33
(DEFCONSTANT BOOLE-2 3
  "As first arg to BOOLE, makes output be the second input.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:34
(DEFCONSTANT BOOLE-C1 12
  "As first arg to BOOLE, makes output be complement of first input.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:34
(DEFCONSTANT BOOLE-C2 14
  "As first arg to BOOLE, makes output be complement of second input.")

(DEFCONSTANT BOOLE-AND 1
  "As first arg to BOOLE, makes output bits be one if both input bits are 1.")

(DEFCONSTANT BOOLE-IOR 7
  "As first arg to BOOLE, makes output bits be one if either input bit is 1.")

(DEFCONSTANT BOOLE-XOR 6
  "As first arg to BOOLE, makes output bits be one if an odd number of input bits are one.")

(DEFCONSTANT BOOLE-EQV 11
  "As first arg to BOOLE, makes output bits be one if an even number of input bits are one.")

(DEFCONSTANT BOOLE-NAND 16
  "As first arg to BOOLE, makes output bits be one unless both input bits are one.")

(DEFCONSTANT BOOLE-NOR 10
  "As first arg to BOOLE, makes output bits be one if both input bits are zero.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:34
(DEFCONSTANT BOOLE-ANDC1 2
  "As first arg to BOOLE, makes output bits be one if first input is 0 and second is 1.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:35
(DEFCONSTANT BOOLE-ANDC2 4
  "As first arg to BOOLE, makes output bits be one if first input is 1 and second is 0.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:35
(DEFCONSTANT BOOLE-ORC1 13
  "As first arg to BOOLE, makes output bits be one if first input is 0 or second is 1.")

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:35
(DEFCONSTANT BOOLE-ORC2 15
  "As first arg to BOOLE, makes output bits be one if first input is 1 or second is 0.")

(DEFUN LOGAND (&REST INTEGERS)
  "Bitwise-AND all the arguments."
  (DO ((ANS -1 (LOGAND ANS (CAR L)))
       (L INTEGERS (CDR L)))
      ((NULL L) ANS)))

(DEFUN LOGIOR (&REST INTEGERS)
  "Bitwise-OR all the arguments."
  (DO ((ANS 0 (LOGIOR ANS (CAR L)))
       (L INTEGERS (CDR L)))
      ((NULL L) ANS)))

(DEFUN LOGXOR (&REST INTEGERS)
  "Bitwise-XOR all the arguments."
  (DO ((ANS 0 (LOGXOR ANS (CAR L)))
       (L INTEGERS (CDR L)))
      ((NULL L) ANS)))

(DEFUN LOGEQV (&REST INTEGERS)
  "Bitwise-EQV all the arguments."
  (DO ((ANS -1 (*BOOLE BOOLE-EQV ANS (CAR L)))
       (L INTEGERS (CDR L)))
      ((NULL L) ANS)))

(DEFSUBST LOGNAND (INTEGER1 INTEGER2)
  "Bitwise-NAND the arguments.  Result bit is 1 if either INTEGER1 bit or INTEGER2 bit is 0."
  (*BOOLE BOOLE-NAND INTEGER1 INTEGER2))

(DEFSUBST LOGNOR (INTEGER1 INTEGER2)
  "Bitwise-NOR the arguments.  Result bit is 1 if INTEGER1 bit and INTEGER2 bit are both 0."
  (*BOOLE BOOLE-NOR INTEGER1 INTEGER2))

(DEFSUBST LOGORC1 (INTEGER1 INTEGER2)
  "Bitwise-ORC1 the arguments.  Result bit is 1 if INTEGER1 bit is 0 or INTEGER2 bit is 1."
  (*BOOLE BOOLE-ORC1 INTEGER1 INTEGER2))

(DEFSUBST LOGORC2 (INTEGER1 INTEGER2)
  "Bitwise-ORC2 the arguments.  Result bit is 1 if INTEGER1 bit is 1 or INTEGER2 bit is 0."
  (*BOOLE BOOLE-ORC2 INTEGER1 INTEGER2))

(DEFSUBST LOGANDC1 (INTEGER1 INTEGER2)
  "Bitwise-ANDC1 the arguments.  Result bit is 1 if INTEGER1 bit is 0 and INTEGER2 bit is 1."
  (*BOOLE BOOLE-ANDC1 INTEGER1 INTEGER2))

(DEFSUBST LOGANDC2 (INTEGER1 INTEGER2)
  "Bitwise-ANDC2 the arguments.  Result bit is 1 if INTEGER1 bit is 1 and INTEGER2 bit is 0."
  (*BOOLE BOOLE-ANDC2 INTEGER1 INTEGER2))

(DEFUN BOOLE (OP ARG1 &REST ARGS)
  "Perform any of the 16 two-operand bitwise operations on ARG1 and ARGS.
OP is a number from 0 to 15 specifying the operation to use.
If there are more than two args (aside from OP)
then the first two are combined, then the result with the next arg, etc.
OP is bit-decoded:
 the 8 bit is the result when applied to 0 and 0,
 the 4 bit is the result when applied to 0 and 1,
 the 2 bit is the result when applied to 1 and 0,
 the 1 bit is the result when applied to 1 and 1.
The constants BOOLE-AND, etc., are provided for use as OP.

For reference:

  0 boole-clr
  1 boole-and
  2 boole-andc1
  3 boole-1
  4 boole-andc2
  5 boole-2
  6 boole-xor
  7 boole-ior
  8 boole-nor
  9 boole-eqv
 10 boole-c1
 11 boole-orc1
 12 boole-c2
 13 boole-orc2
 14 boole-nand
 15 boole-set
"
  (DO ((ANS ARG1 (*BOOLE OP ANS (CAR L)))
       (L ARGS (CDR L)))
      ((NULL L) ANS)))

(DEFSUBST LOGNOT (INTEGER)
  "Return the bitwise complement of INTEGER."
  (LOGXOR INTEGER -1))

(DEFUN LOGCOUNT (INTEGER &AUX (COUNT 0))
  "Count number of bits set in INTEGER's binary representation.
Counts number of 1's in a positive INTEGER
 or number of 0's in a negative INTEGER."
  (IF (MINUSP INTEGER)
      (DOTIMES (I (HAULONG INTEGER))
        (UNLESS (LDB-TEST (BYTE 1 I) INTEGER)
            (INCF COUNT)))
    (DOTIMES (I (HAULONG INTEGER))
      (IF (LDB-TEST (BYTE 1 I) INTEGER)
          (INCF COUNT))))
  COUNT)

(DEFUN LOAD-BYTE (FROM-VALUE POSITION WIDTH)
  "Return a byte extracted from FROM-VALUE, of specified WIDTH and POSITION.
POSITION counts from zero at the least significant bit."
  (LDB (BYTE WIDTH POSITION) FROM-VALUE))

(DEFUN DEPOSIT-BYTE (INTO-VALUE POSITION WIDTH BYTE-VALUE)
  "Deposit BYTE-VALUE into a byte in INTO-VALUE of specified WIDTH and POSITION.
POSITION counts from zero at the least significant bit."
  (DPB BYTE-VALUE (BYTE WIDTH POSITION) INTO-VALUE))

;;; Number functions.

(DEFspecialk SIGNP (&QUOTE TEST &EVAL NUM)
  "Test the sign of NUM, returning T or NIL.
TEST is a symbol, one of L, LE, G, GE, N or E.
If NUM is not a number, the value is NIL."
  (COND ((NOT (NUMBERP NUM)) NIL)
        ((STRING-EQUAL TEST "L") (< NUM 0))
        ((STRING-EQUAL TEST "LE") ( NUM 0))
        ((STRING-EQUAL TEST "E") (= NUM 0))
        ((STRING-EQUAL TEST "N") ( NUM 0))
        ((STRING-EQUAL TEST "GE") ( NUM 0))
        ((STRING-EQUAL TEST "G") (> NUM 0))
        ((FERROR "~S is not a test name for ~S" TEST 'SIGNP))))

;;;; String, pname and character functions.

(DEFF SAMEPNAMEP 'STRING=)

(DEFUN MAKNAM (CHARL)
  "Returns an uninterned symbol whose print-name is a string made up of
the characters in CHARL.  This is obsolete; use strings instead."
  (MAKE-SYMBOL (MACLISP-MAKE-STRING CHARL)))

(DEFUN COPYSYMBOL (SYMBOL &OPTIONAL COPYPROPS &AUX NEWSYM)
  "Return a new uninterned symbol with the same pname as SYMBOL.
If COPYPROPS is non-NIL, the value, function definition and properties
of SYMBOL are all copied into the new symbol."
  (SETQ NEWSYM (MAKE-SYMBOL (SYMBOL-NAME SYMBOL)))
  (WHEN COPYPROPS
    (AND (BOUNDP SYMBOL)
         (RPLACA (VALUE-CELL-LOCATION NEWSYM) (CAR (VALUE-CELL-LOCATION SYMBOL))))
    (AND (FBOUNDP SYMBOL)
         (RPLACA (FUNCTION-CELL-LOCATION NEWSYM) (CAR (FUNCTION-CELL-LOCATION SYMBOL))))
    (RPLACA (PROPERTY-CELL-LOCATION NEWSYM)
            (COPY-LIST (CAR (PROPERTY-CELL-LOCATION SYMBOL)))))
  NEWSYM)
(DEFF COPY-SYMBOL 'COPYSYMBOL)

(DEFVAR *GENSYM-PREFIX* "G"
  "Character or string used as prefix of names made by GENSYM.")
(DEFVAR *GENSYM-COUNTER* 0 "Counter used for next GENSYM'd symbol.")
(FORWARD-VALUE-CELL '*GENSYM-COUNTER '*GENSYM-COUNTER*)
(FORWARD-VALUE-CELL '*GENSYM-PREFIX '*GENSYM-PREFIX*)

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#825 on 27-Mar-87 15:57:37
(DEFUN GENSYM (&OPTIONAL ARG (PERMANENT-P T) &AUX PNAME)
  "Return a new uninterned symbol with a generated name.
SI:*GENSYM-COUNTER* and SI:*GENSYM-PREFIX* are used to generate it.
PERMANENT-P says cons the pname string in a permanent place."
  (COND ((NULL ARG))
        ((NUMBERP ARG)
         (SETQ *GENSYM-COUNTER* ARG))
        ((SYMBOLP ARG)
         (SETQ *GENSYM-PREFIX* (SYMBOL-NAME ARG)))
        ((STRINGP ARG)
         (SETQ *GENSYM-PREFIX* ARG)))
  (AND (> (SETQ *GENSYM-COUNTER* (1+ *GENSYM-COUNTER*)) 9999.)
       (SETQ *GENSYM-COUNTER* 0))
  (LET ((DEFAULT-CONS-AREA (IF PERMANENT-P P-N-STRING DEFAULT-CONS-AREA))
        (%INHIBIT-READ-ONLY T)
        INDEX)
    (SETQ PNAME (STRING-APPEND *GENSYM-PREFIX* "    "))
    (SETQ INDEX (- (LENGTH PNAME) 4))
    (ASET (+ #/0 (TRUNCATE *GENSYM-COUNTER* 1000.)) PNAME INDEX)
    (ASET (+ #/0 (\ (TRUNCATE *GENSYM-COUNTER* 100.) 10.)) PNAME (+ INDEX 1))
    (ASET (+ #/0 (\ (TRUNCATE *GENSYM-COUNTER* 10.) 10.)) PNAME (+ INDEX 2))
    (ASET (+ #/0 (\ *GENSYM-COUNTER* 10.)) PNAME (+ INDEX 3)))
  (MAKE-SYMBOL PNAME PERMANENT-P))

(DEFVAR *GENTEMP-COUNTER* (MAKE-ARRAY 10 :TYPE 'ART-STRING :INITIAL-ELEMENT #\0 :FILL-POINTER 1))

(DEFUN GENTEMP (&OPTIONAL (PREFIX "T") (PKG PACKAGE))
  "Return a unique symbol in package PKG.  Its name starts with PREFIX.
We try appending various numerals to PREFIX until we get a name
that is not interned in PKG; then we intern it and return the
newly created symbol.  Therefore, no two calls to GENTEMP in the
same Lisp world ever return the same value."
  (LET ((S (IF (SYMBOLP PREFIX) (GET-PNAME PREFIX) PREFIX))
        (P (IF (PACKAGEP PKG) PKG (FIND-PACKAGE PKG)))
        (K *GENTEMP-COUNTER*))
  (DO-FOREVER
    (INCREMENT-DECIMAL-STRING K)
    (MULTIPLE-VALUE-BIND (SYMBOL ALREADY-INTERNEDP)
        (INTERN (STRING-APPEND S K) P)
      (OR ALREADY-INTERNEDP (RETURN SYMBOL))))))

(DEFUN INCREMENT-DECIMAL-STRING (S)
  (DO ((J (1- (LENGTH S)) (1- J)))
      ((= J -1)
       (VECTOR-PUSH-EXTEND #\* S)
       (DO ((K (1- (LENGTH S)) (1- K)))
           ((= K 0)
            (SETF (AREF S 0) #\1)
            S)
         (SETF (AREF S K) (AREF S (1- K)))))
    (LET ((D (AREF S J)))
      (COND ((= D #\9)
             (SETF (AREF S J) #\0))
            ('ELSE
             (SETF (AREF S J) (1+ D))
             (RETURN S))))))


(DEFUN MACLISP-MAKE-STRING (CHARL &OPTIONAL AREA &AUX PNAME)
  (LET ((%INHIBIT-READ-ONLY T))
    (SETQ PNAME (MAKE-ARRAY (LENGTH CHARL) ':AREA AREA ':TYPE 'ART-STRING))
    (DO ((I 0 (1+ I))
         (L CHARL (CDR L)))
        ((NULL L))
      (AS-1 (CHARACTER (CAR L)) PNAME I))
    PNAME))

(DEFUN GETCHARN (S N)
  "Obsolete Maclisp function to get Nth char of pname of symbol S, as a number.
N = 1 gets the first character."
  (SETQ S (STRING S))
  (IF ( 1 N (ARRAY-ACTIVE-LENGTH S))
      (AREF S (1- N))
    0))

(DEFUN GETCHAR (S N)
  "Obsolete Maclisp function to get Nth char of pname of symbol S, as a symbol.
N = 1 gets the first character."
  (SETQ S (STRING S))
  (IF ( 1 N (ARRAY-ACTIVE-LENGTH S))
      (ASCII (AREF S (1- N)))
    NIL))

(DEFUN ASCII (N)
  "Obsolete Maclisp function to turn character code number N into a symbol.
The symbol's pname has one character, the one with code N."
  (LET* ((%INHIBIT-READ-ONLY T)
         (DEFAULT-CONS-AREA P-N-STRING)
         (STR (STRING N))
         (SYM (INTERN STR)))
    SYM))

(DEFUN IMPLODE (X)
  "Obsolete Maclisp function to make a new interned symbol.
X is a list of symbols or numbers, each of which specifies one character
of the pname of the new symbol."
  (LET* ((TOK (MACLISP-MAKE-STRING X P-N-STRING))
         (VAL (INTERN TOK)))
    VAL))

;(DEFVAR *IOLST)                                        ;Used by readlist, explode, etc.
;(DEFVAR *IOCH)

(DEFUN EXPLODE (X &AUX (*IOLST NIL) (*IOCH T))
  "Obsolete Maclisp function to examine printed representation ofobject X.
It returns a list of symbols, one for each character that would have been printed.
The printing is done with quoting characters."
  (PRIN1 X (FUNCTION EXPLODE-STREAM))
  (NREVERSE *IOLST))

(DEFUN EXPLODEC (X &AUX (*IOLST NIL) (*IOCH T))
  "Obsolete Maclisp function to examine printed representation ofobject X.
It returns a list of symbols, one for each character that would have been printed.
The printing is done without quoting characters, like PRINC."
  (PRINC X (FUNCTION EXPLODE-STREAM))
  (NREVERSE *IOLST))

(DEFUN EXPLODEN (X &AUX (*IOLST NIL) (*IOCH NIL))
  "Obsolete Maclisp function to examine printed representation ofobject X.
It returns a list of numbers (character codes),
one for each character that would have been printed.
The printing is done without quoting characters, like PRINC."
  (PRINC X (FUNCTION EXPLODE-STREAM))
  (NREVERSE *IOLST))

(DEFPROP EXPLODE-STREAM T IO-STREAM-P)

(DEFUN EXPLODE-STREAM (OPERATION &OPTIONAL ARG1 &REST REST &AUX STR) ; OLDP
  (COND ((EQ OPERATION ':TYO)
         ;; this is a patch. (EXPLODEN 44) would otherwise return '(#\4 #\4)
         ;; presumably a protocol violation to be :TYO'ing characters that
         ;; arent fixnums.
         (OR (FIXP ARG1) (SETQ ARG1 (CHAR-CODE ARG1)))
         (COND (*IOCH
                (SETQ ARG1 (INTERN (SETQ STR (STRING ARG1))))
                ;(MULTIPLE-VALUE (ARG1 OLDP) (INTERN (SETQ STR (STRING ARG1))))
                ;(AND OLDP (RETURN-ARRAY (PROG1 STR (SETQ STR NIL))))
                ))
         (SETQ *IOLST (CONS ARG1 *IOLST)))
        ((EQ OPERATION ':WHICH-OPERATIONS)
         '(:TYO))
        (T (STREAM-DEFAULT-HANDLER 'EXPLODE-STREAM OPERATION ARG1 REST))))

;;;; Array functions.

(DEFUN ARRAY-DIMENSIONS (ARRAY &AUX INDEX-LENGTH RANK LONG-ARRAY-P DIMS PRODUCT)
  "Return a list of the dimensions of ARRAY."
  (AND (SYMBOLP ARRAY) (SETQ ARRAY (FSYMEVAL ARRAY)))
  (CHECK-TYPE ARRAY ARRAY)
        ;SHOULD CHECK FOR INVZ
  (SETQ RANK (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)
        LONG-ARRAY-P (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
  (SETQ INDEX-LENGTH (COND ((= 0 (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0))
                            (COND ((= 1 LONG-ARRAY-P) (%P-LDB-OFFSET %%Q-POINTER ARRAY 1))
                                  (T (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))))
                           ((%P-LDB-OFFSET %%Q-POINTER ARRAY (1+ (+ RANK LONG-ARRAY-P))))))
  (IF (ZEROP RANK) NIL
    (DO ((N RANK (1- N))
         (I (1+ LONG-ARRAY-P) (1+ I)))
        (( N 1))
      (SETQ DIMS (CONS (%P-LDB-OFFSET %%Q-POINTER ARRAY I) DIMS)))
    (SETQ PRODUCT (APPLY #'* DIMS))
    (CONS (COND ((ZEROP PRODUCT) 0)
                (T (TRUNCATE INDEX-LENGTH PRODUCT)))
          DIMS)))

;;; Returns the number of bits that fit in an element of an array.
(DEFUN ARRAY-ELEMENT-SIZE (ARRAY)
  "Return the number of bits per element of ARRAY."
  (OR (AREF #'ARRAY-BITS-PER-ELEMENT (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0))
      %%Q-POINTER))                             ;Q-type, assume going to use unsigned fixnums.

(DEFUN ARRAY-PUSH-EXTEND (ARRAY DATA &OPTIONAL EXTENSION
                          &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Add the new element DATA to the end of ARRAY, making ARRAY larger if needed.
EXTENSION says how many elements to add; the default is a fraction
of the existing size.  ARRAY must have a fill pointer."
  (COND ((ARRAY-PUSH ARRAY DATA))
        (T (ADJUST-ARRAY-SIZE ARRAY (+ (ARRAY-LENGTH ARRAY)
                                       ;; If amount to extend by not specified,
                                       ;; try to guess a reasonable amount
                                       (COND (EXTENSION)
                                             ((< (%STRUCTURE-TOTAL-SIZE ARRAY) PAGE-SIZE)
                                              (MAX (ARRAY-LENGTH ARRAY) 100))
                                             (T (TRUNCATE (ARRAY-LENGTH ARRAY) 4)))))
           (ARRAY-PUSH ARRAY DATA))))

;; Copied from LAD: RELEASE-3.SYS; QFCTNS.LISP#819 on 2-Oct-86 05:01:26
(DEFUN VECTOR-PUSH-EXTEND (DATA VECTOR &OPTIONAL EXTENSION
                           &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Add the new element DATA to the end of VECTOR, making VECTOR larger if needed.
EXTENSION says how many elements to add; the default is a fraction
of the existing size.  VECTOR must have a fill pointer."
    (OR (ARRAY-PUSH VECTOR DATA)
        (PROGN
          (ADJUST-ARRAY-SIZE
            VECTOR
            (+ (ARRAY-LENGTH VECTOR)
               ;; If amount to extend by not specified,
               ;; try to guess a reasonable amount
               (COND (EXTENSION EXTENSION)
                     ((< (%STRUCTURE-TOTAL-SIZE VECTOR) PAGE-SIZE)
                      (MAX (ARRAY-LENGTH VECTOR) 100))
                     (T (TRUNCATE (ARRAY-LENGTH VECTOR) 4)))))
          (ARRAY-PUSH VECTOR DATA))))

(DEFUN ARRAY-DISPLACED-P (ARRAY)
  "T if ARRAY is a displaced array."
  (and (arrayp array)
       (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)))

(DEFUN ARRAY-INDIRECT-P (ARRAY)
  "T if ARRAY is displaced to another array."
  (and (arrayp array)
       (let ((offset (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
         (AND (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
              (= (%P-LDB-OFFSET %%Q-DATA-TYPE ARRAY OFFSET) DTP-ARRAY-POINTER)))))

;;; This is random, maybe it should be flushed.
(DEFUN ARRAY-INDEXED-P (ARRAY)
  "T if ARRAY is indexed to another array and has an index offset."
  (and (arrayp array)
       (let ((offset (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
         (AND (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
              (= (%P-LDB-OFFSET %%Q-DATA-TYPE ARRAY OFFSET) DTP-ARRAY-POINTER)
              (= (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0) 3)))))

(DEFUN ARRAY-INDIRECT-TO (ARRAY)
  "Given an indirect array, return the array it indirects to.  Otherwise NIL."
  (and (arrayp array)
       (let ((offset (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
         (AND (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
              (ARRAYP (%P-CONTENTS-OFFSET ARRAY OFFSET))
              (%P-CONTENTS-OFFSET ARRAY OFFSET)))))

(DEFUN ARRAY-INDEX-OFFSET (ARRAY)
  "Given an array with an index offset, return that.  Otherwise NIL."
  (and (arrayp array)
       (let ((offset (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
         (AND (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
              (ARRAYP (%P-CONTENTS-OFFSET ARRAY OFFSET))
              (= (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0) 3)
              (%P-CONTENTS-OFFSET ARRAY (+ 2 OFFSET))))))

(DEFUN MAKE-ARRAY-INTO-NAMED-STRUCTURE (ARRAY &OPTIONAL NSS)
  "ARRAY is made into a named structure and is returned."
  (CHECK-TYPE ARRAY ARRAY)
  (WHEN NSS
    (IF (ARRAY-HAS-LEADER-P ARRAY)
        (SETF (ARRAY-LEADER ARRAY 1) NSS)
      (SETF (AREF ARRAY 0) NSS)))
  (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0)
  ARRAY)

(DEFUN ARRAY-GROW (ARRAY &REST DIMENSIONS
                         &AUX (OLD-DIMS (ARRAY-DIMENSIONS ARRAY))
                         INDEX NEW-ARRAY)
  "Alter the dimensions of an array, preserving old contents.
A new array is created and the old one is forwarded; the value is the new one.
Any elements of the old array that are within the bounds of the new one
are copied.  The leader if any is also copied."
  (CHECK-TYPE ARRAY ARRAY)
  (PROG ()
        ;; Extend or truncate the supplied list of dimensions.
        ;; Omitted dimensions are left unchanged.
        (AND (< (LENGTH DIMENSIONS) (LENGTH OLD-DIMS))
             (SETQ DIMENSIONS (APPEND DIMENSIONS (NTHCDR (LENGTH DIMENSIONS) OLD-DIMS))))
        (AND (> (LENGTH DIMENSIONS) (LENGTH OLD-DIMS))
             (SETQ DIMENSIONS (FIRSTN (LENGTH OLD-DIMS) DIMENSIONS)))
        ;; If it's 1-dimensional, might as well try to grow it in place.
        (AND (NULL (CDR DIMENSIONS))
             (RETURN (ADJUST-ARRAY-SIZE ARRAY (CAR DIMENSIONS))))
        ;; Make the new array.
        (SETQ NEW-ARRAY (MAKE-ARRAY DIMENSIONS
                                    ':AREA (%AREA-NUMBER ARRAY)
                                    ':TYPE (ARRAY-TYPE ARRAY)
                                    ':LEADER-LENGTH (ARRAY-LEADER-LENGTH ARRAY)))
        ;; Copy the array leader.
        (DO ((I 0 (1+ I))
             (N (OR (ARRAY-LEADER-LENGTH ARRAY) 0) (1- N)))
            ((ZEROP N))
          (SETF (ARRAY-LEADER NEW-ARRAY I) (ARRAY-LEADER ARRAY I)))

        ;; Check for zero-size array, which the code below doesn't handle correctly
        (AND (DO ((L DIMENSIONS (CDR L)) (L1 OLD-DIMS (CDR L1))) ((NULL L) NIL)
               (AND (OR (ZEROP (CAR L)) (ZEROP (CAR L1)))
                    (RETURN T)))
             (GO DONE))

        ;; Create a vector of fixnums to use as subscripts to step thru the arrays.
        (SETQ INDEX NIL)
        (DO ((L DIMENSIONS (CDR L))) ((NULL L))
           (SETQ INDEX (CONS 0 INDEX)))

        ;; Make the first increment of INDEX bring us to element 0 0 0 0..
        (RPLACA INDEX -1)

     LOOP

        ;; Increment the vector of subscripts INDEX.
        ;; Go to DONE if we have exhausted all elements that need copying.
        (DO ((I INDEX (CDR I))
             (O OLD-DIMS (CDR O))
             (N DIMENSIONS (CDR N)))
            ((NULL I) (GO DONE))
           ;; Increment one index
           (INCF (CAR I))
           ;; and decide whether to "carry" to the next one.
           (IF (OR ( (CAR I) (CAR O))
                   ( (CAR I) (CAR N)))
               (SETF (CAR I) 0)
             (RETURN NIL)))

        (APPLY #'ASET (APPLY #'AREF ARRAY INDEX) NEW-ARRAY INDEX)
        (GO LOOP)

     DONE

        ;; The contents have been copied.  Copy a few random things.
        (%P-DPB (%P-LDB %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY)
                %%ARRAY-NAMED-STRUCTURE-FLAG NEW-ARRAY)
        (%P-DPB (%P-LDB %%ARRAY-FLAG-BIT ARRAY)
                %%ARRAY-FLAG-BIT NEW-ARRAY)
        (STRUCTURE-FORWARD ARRAY NEW-ARRAY)
        (RETURN NEW-ARRAY)))

;;; The argument must really be a structure, not a locative into the middle
;;; of something, and must not be in list space.  We store DTP-HEADER-FORWARD
;;; and DTP-BODY-FORWARDs from the old instance to the new instance, and return the old.
(DEFUN STRUCTURE-FORWARD (OLD NEW)
  "Forward the entire contents of the OLD structure to the NEW one."
  (UNLESS (= (%DATA-TYPE OLD) (%DATA-TYPE NEW))
    (FERROR "~S and ~S seem incompatible" OLD NEW))
  (UNLESS (= (%REGION-REPRESENTATION-TYPE (%REGION-NUMBER OLD)) %REGION-REPRESENTATION-TYPE-LISP)
    (FERROR "~S is not in a lisp-structured region" OLD))
  (WITHOUT-INTERRUPTS                   ;Don't let anything move while in inconsistent state
    (LET* ((LEADER (%FIND-STRUCTURE-LEADER OLD))
           ;; Must fill even formerly unboxed words with body-forwards
           ;; since that is how scavenger will tell how big the structure was.
           (SIZE (%STRUCTURE-TOTAL-SIZE LEADER)))
      ;; Note that the body-forwards all point at the old structure's header.
      (%P-STORE-TAG-AND-POINTER LEADER DTP-BODY-FORWARD OLD)
      ;; It's ok to use %BLT instead of %BLT-TYPED here since it only
      ;; replicates the BODY-FORWARD to the new structure, which must be in NEW
      ;; space since it's already pointed to by the PDL.
       ;you may ask, but how about volatilities nowadays???
       ; answer: this is still OK.  These BODY-FORWARDs point within their own object.
       ;   "The volatility of such a pointer cannot be important."
      (%BLT LEADER (%POINTER-PLUS LEADER 1) (1- SIZE) 1)
      ;; The old header points (with header-forward) to the new structure.
      (%P-STORE-TAG-AND-POINTER OLD DTP-HEADER-FORWARD NEW)
      ;; Make sure next array operation is fully decoded.
      (INVALIDATE-ARRAY-CACHE))
    OLD))

(DEFUN FORWARD-VALUE-CELL (FROM-SYMBOL TO-SYMBOL)
  "Make FROM-SYMBOL a synonym for TO-SYMBOL when used as a special variable.
Does not declare either symbol special, however.
Do not do this within a special binding of FROM-SYMBOL;
it would get undone by unbinding FROM-SYMBOL.
Only call this at times when FROM-SYMBOL has its global binding."
  (CHECK-TYPE FROM-SYMBOL SYMBOL)
  (CHECK-TYPE TO-SYMBOL SYMBOL)
  (AND (EQ FROM-SYMBOL TO-SYMBOL)
       (FERROR "Forwarding symbol's value to itself"))
  (%P-STORE-TAG-AND-POINTER (VALUE-CELL-LOCATION FROM-SYMBOL)
                            DTP-ONE-Q-FORWARD
                            (VALUE-CELL-LOCATION TO-SYMBOL)))

;;; Like FOLLOW-STRUCTURE-FORWARDING
(DEFUN FOLLOW-CELL-FORWARDING (LOC EVCP-P)
  "Given a locative pointer to a cell, return a locative to where it is forwarded to.
The value will equal the argument if there is no forwarding.
EVCP-P says whether to follow external-value-cell pointers
as well as other kinds of forwarding."
  (DO-FOREVER
    (SELECT (%P-DATA-TYPE LOC)
      ((DTP-HEADER-FORWARD DTP-BODY-FORWARD DTP-RPLACD-FORWARD)
       (SETQ LOC (FOLLOW-STRUCTURE-FORWARDING LOC)))
      (DTP-ONE-Q-FORWARD
       (SETQ LOC (%MAKE-POINTER (%DATA-TYPE LOC) (%P-CONTENTS-AS-LOCATIVE LOC))))
      (DTP-EXTERNAL-VALUE-CELL-POINTER
       (OR EVCP-P (RETURN LOC))
       (SETQ LOC (%MAKE-POINTER (%DATA-TYPE LOC) (%P-CONTENTS-AS-LOCATIVE LOC))))
      (OTHERWISE (RETURN LOC)))))

(DEFUN CAR-LOCATION (CONS)
  "Returns a locative pointer to the cell containing the car of the cons."
  (CHECK-TYPE CONS CONS)
  (%MAKE-POINTER DTP-LOCATIVE CONS))

(DEFUN GET-LOCATION (SYMBOL PROPERTY &OPTIONAL DEFAULT)
  (IF (TYPEP SYMBOL 'INSTANCE)
      (SEND SYMBOL :GET-LOCATION PROPERTY DEFAULT)
    (DO ((L (PLIST SYMBOL) (CDDR L)))
        ((NULL L)
         (PUTPROP SYMBOL DEFAULT PROPERTY)
         (GET-LOCATION SYMBOL PROPERTY))                        ;Slow, but inefficient!  -smh 9aug88
       (AND (EQ (CAR L) PROPERTY)
            (RETURN (CAR-LOCATION (CDR L)))))))

(defun get-location-from-area  (SYMBOL PROPERTY AREA &OPTIONAL DEFAULT)
  (IF (TYPEP SYMBOL 'INSTANCE)
      (SEND SYMBOL :GET-LOCATION-from-area PROPERTY area DEFAULT)
    (DO ((L (PLIST SYMBOL) (CDDR L)))
        ((NULL L)
         (PUTPROP-in-area SYMBOL DEFAULT PROPERTY area)
         (GET-LOCATION-from-area SYMBOL PROPERTY area))
       (AND (EQ (CAR L) PROPERTY)
            (RETURN (CAR-LOCATION (CDR L)))))))

;;; PROCLAIM, DECLARE, EVAL-WHEN now in SYS; EVAL

;;; ARGLIST returns the list of argument names and the list of
;;; returned value names of the definition of a function spec.
;;; The first value is the arglist: a list of the names
;;; of the arguments, together with lambda list keywords.
;;; The second value is the list of returned value names.
;;; This list is present only if the definition of the function
;;; supplies one, and it is just a comment.  Those names play no
;;; actual role in execution.

;;; The argument REAL-FLAG is T to inhibit the use of any declared
;;; (comment only) arglist information.  Only the actual arglist of the function
;;; is returned.  Normally the arglist specified for human comsumption
;;; with an arglist declaration overrides the real one.

;;; REAL-FLAG is COMPILE to get the arglist automatically generated by the
;;; compiler for functions whose lambda-list contains &keys args or specified-p args
;;; If no such arglist was generated, we return the what we would get with
;;; REAL-FLAG = T.

;;; REAL-FLAG also inhibits following encapsulations.
;;; So you get the arglist of the encapsulation rather than the
;;; original definition.

;;; T should be used by anything that requires a "legitimate" arglist
;;; that reliably corresponds to what the function does with its args.

;;; We accept both functions and function specs.

(DEFUN ARGLIST (FUNCTION &OPTIONAL REAL-FLAG &AUX TEM DEBUG-INFO ARG-MAP LOCAL-MAP)
  "Return the argument list of FUNCTION, and its value-list.
FUNCTION may be a function or a function spec.
If REAL-FLAG is T, return the actual argument list, good for compilation, calling, etc.
If REAL-FLAG is COMPILE, return the argument list generated by the compiler,
 if FUNCTION is compiled. This arglist includes the names of the keys for &KEY arguments,
 if any, and the forms used for defaulting optional args. /"Supplied-p/" args are not included
 If the function is not compiled, this is the same as REAL-FLAG = T
Otherwise, return an argument list intended as documentation for humans.
 This will be the same as if REAL-FLAG were COMPILE, unless there was an explicit
 (DECLARE (ARGLIST ...)) in the defintiion of FUNCTION.
The second value is the value-list, only for documentation for humans.
The third value is NIL, SUBST or MACRO."
  (DECLARE (VALUES ARGLIST VALUES TYPE))
  (TYPECASE FUNCTION
    (SYMBOL
     (COND ((and (boundp 'compiler:*compilation-environment*)
                 (do ((env compiler:*compilation-environment* (compiler:compilation-environment-next env)))
                     ((null env))
                   (unless (eq (setq tem (getf (gethash function (compiler:compilation-environment-plist-hashtab env))
                                               'compiler:compiler-arglist
                                               :no))
                               :no)
                     (return-from arglist tem)))))
           ((GET FUNCTION 'ARGLIST)) ;Handles names defined only in the compiler.
           ((INTERPRETER-SPECIAL-FORM FUNCTION)
            (ARGLIST (INTERPRETER-SPECIAL-FORM-HANDLER (INTERPRETER-SPECIAL-FORM FUNCTION)) REAL-FLAG))
           ('ELSE
            (ARGLIST (FSYMEVAL FUNCTION) REAL-FLAG))))
    (CONS
     (COND ((EQ (CAR FUNCTION) 'LAMBDA)
            (LDIFF (CADR FUNCTION) (MEMQ '&AUX (CADR FUNCTION))))
           ((MEMQ (CAR FUNCTION) '(SUBST CLI:SUBST))
            (VALUES (CADR FUNCTION) NIL 'SUBST))
           ((MEMQ (CAR FUNCTION) '(NAMED-SUBST NAMED-LAMBDA))
            (SETQ DEBUG-INFO (DEBUGGING-INFO FUNCTION))
            (COND ((AND (MEMQ REAL-FLAG '(NIL COMPILE))
                        (ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO))
                   (ARGLIST (CADR (ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO)) REAL-FLAG))
                  (T
                   (VALUES
                     (LET ((TEM (OR (IF (EQ REAL-FLAG 'NIL)
                                        (ASSQ 'ARGLIST DEBUG-INFO))
                                    (IF (MEMQ REAL-FLAG '(COMPILE NIL))
                                        (ASSQ 'COMPILER::COMPILER-ARGLIST DEBUG-INFO)))))
                       (IF TEM (CDR TEM)
                         (LDIFF (CADDR FUNCTION) (MEMQ '&AUX (CADDR FUNCTION)))))
                     (CDR (ASSQ 'VALUES DEBUG-INFO))
                     (AND (EQ (CAR FUNCTION) 'NAMED-SUBST)
                          'SUBST)))))
           ((MEMQ (CAR FUNCTION) '(CURRY-BEFORE CURRY-AFTER))
            '(&REST ARGLIST))
           ((EQ (CAR FUNCTION) 'MACRO)
            ;; Look for (DECLARE (ARGLIST ...)) type arglist
            (SETQ DEBUG-INFO (DEBUGGING-INFO (CDR FUNCTION)))
            (VALUES (CDR (OR (IF (EQ REAL-FLAG 'NIL)
                                 (ASSQ 'ARGLIST DEBUG-INFO))
                             (IF (MEMQ REAL-FLAG '(COMPILE NIL))
                                 (ASSQ 'COMPILER::COMPILER-ARGLIST DEBUG-INFO))
                             '(NIL . MACRO)))
                    (CDR (ASSQ 'VALUES (DEBUGGING-INFO (CDR FUNCTION))))
                    'MACRO))
           ((VALIDATE-FUNCTION-SPEC FUNCTION)
            (ARGLIST (FDEFINITION FUNCTION) REAL-FLAG))
           (T (FERROR "~S not a recognized function" FUNCTION))))
    (STACK-GROUP
     '(STACK-GROUP-ARG))
    (ARRAY
     (DO ((I (%P-LDB %%ARRAY-NUMBER-DIMENSIONS FUNCTION) (1- I))
          (L NIL))
         (( I 0) L)
       (SETQ L (CONS (INTERN (FORMAT NIL "DIM-~D" I) PKG-SYSTEM-INTERNALS-PACKAGE) L))))
    ((OR CLOSURE ENTITY)
     (ARGLIST (CAR (%MAKE-POINTER DTP-LIST FUNCTION)) REAL-FLAG))
    ((OR SELECT-METHOD INSTANCE)
     ;; Can't tell arglist, shouldn't give error though
     '(OP &REST SELECT-METHOD-ARGS-VARY))
    (COMPILED-FUNCTION
     (SETQ DEBUG-INFO (DEBUGGING-INFO FUNCTION))
     (SETQ ARG-MAP (CADR (ASSQ 'COMPILER::ARG-MAP DEBUG-INFO)))
     (SETQ LOCAL-MAP (CADR (ASSQ 'COMPILER::LOCAL-MAP DEBUG-INFO)))
     (VALUES
       (COND ((AND (EQ REAL-FLAG 'NIL)
                   (CDR (ASSQ 'ARGLIST DEBUG-INFO))))
             ((AND (MEMQ REAL-FLAG '(COMPILE NIL))
                   (CDR (ASSQ 'COMPILER::COMPILER-ARGLIST DEBUG-INFO))))
             ((SETQ TEM (GET-MACRO-ARG-DESC-POINTER FUNCTION))
              (DO ((ADL TEM (CDR ADL))
                   (ARGNUM 0 (1+ ARGNUM))
                   (ARGNAME)
                   (OPTIONALP NIL)
                   (SPECIAL FEF-LOCAL)
                   (INIT)
                   (INITP T T)
                   (ADLWORD)
                   (ARGLIS NIL))
                  ((NULL ADL)
                   (NREVERSE ARGLIS))
                (SETQ ADLWORD (CAR ADL))
                (SELECT
                  (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
                  (FEF-ARG-REQ
                   (AND OPTIONALP
                        (FERROR "Required args after optionals in ~S" FUNCTION)))
                      (FEF-ARG-OPT (OR OPTIONALP (SETQ ARGLIS (CONS '&OPTIONAL ARGLIS)))
                                   (SETQ OPTIONALP T))
                      (FEF-ARG-REST (SETQ ARGLIS (CONS '&REST ARGLIS)))
                      (OTHERWISE (RETURN (NREVERSE ARGLIS))))
                (SELECT (MASK-FIELD %%FEF-QUOTE-STATUS ADLWORD)
                  (FEF-QT-QT
                   (FERROR NIL "Obsolete special form, recompile the definition of ~S" FUNCTION))
                  (FEF-QT-EVAL))

                (SETQ TEM (LDB %%FEF-DES-DT ADLWORD))
                (SETQ TEM (LDB %%FEF-SPECIAL-BIT ADLWORD))      ;handle remote some time?
                (WHEN (NEQ TEM SPECIAL)
                  (SETQ SPECIAL TEM)
                  (SETQ ARGLIS (CONS (NTH TEM '(&LOCAL &SPECIAL))
                                     ARGLIS)))
                (SETQ ARGNAME (COND ((= (LOGAND ADLWORD %FEF-NAME-PRESENT)
                                        FEF-NM-YES)
                                     (SETQ ADL (CDR ADL))
                                     (CAR ADL))
                                    (T
                                     (SETQ ARGNAME (COND (( (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
                                                             FEF-ARG-REST)
                                                          (NTH ARGNUM ARG-MAP))
                                                         (T (CAR LOCAL-MAP))))
                                     (IF (SYMBOLP ARGNAME) ARGNAME (CAR ARGNAME)))))
                (SELECT (MASK-FIELD %%FEF-INIT-OPTION ADLWORD)
                  (FEF-INI-NONE (SETQ INITP NIL))
                  (FEF-INI-NIL (SETQ INIT NIL))
                  (FEF-INI-PNTR
                   (SETQ ADL (CDR ADL))
                   (SETQ INIT (CASE (%P-DATA-TYPE ADL)
                                ((#.DTP-EXTERNAL-VALUE-CELL-POINTER)
                                 (MULTIPLE-VALUE-BIND (SYM CELL-FUNCTION)
                                     (DECODE-EVCP (%P-CONTENTS-AS-LOCATIVE ADL))
                                   (CASE CELL-FUNCTION
                                     (SYMEVAL SYM)
                                     (FDEFINITION `(FUNCTION ,SYM))
                                     (T `(,CELL-FUNCTION ',SYM)))))
                                ((#.DTP-SELF-REF-POINTER)
                                 (FLAVOR-DECODE-SELF-REF-POINTER
                                   (FEF-FLAVOR-NAME FUNCTION)
                                   (%P-POINTER ADL)))
                                (T `',(CAR ADL)))))
                  (FEF-INI-C-PNTR
                   (SETQ ADL (CDR ADL))
                   (COND ;((= (%P-DATA-TYPE ADL) DTP-EXTERNAL-VALUE-CELL-POINTER)
                         ; (SETQ INIT                   ;THIS IS A BIT OF A KLUDGE
                         ;       (%FIND-STRUCTURE-HEADER (%P-CONTENTS-AS-LOCATIVE ADL))))
                         ;HOPE IT'S VALUE-CELL-LOCATION
                     ((LOCATIVEP (CAR ADL))
                      (SETQ INIT (%FIND-STRUCTURE-HEADER (CAR ADL))))
                     ((SETQ INIT (CAAR ADL)))))
                  (FEF-INI-OPT-SA (SETQ ADL (CDR ADL))
                                  (SETQ INIT '*HAIRY*))
                  (FEF-INI-COMP-C (SETQ INIT '*HAIRY*))
                  (FEF-INI-EFF-ADR (SETQ ADL (CDR ADL))
                                   (SETQ INIT '*HAIRY*))
                  (FEF-INI-SELF (SETQ INIT ARGNAME)))
                (SETQ ARGLIS (CONS (COND (INITP
                                          (LIST ARGNAME INIT))
                                         (T ARGNAME)) ARGLIS))))
             (T
              ;; No ADL.  Use the fast-arg-option to get the general pattern
              ;;   and the argmap for the names.
              (LET ((FAST-OPT (%ARGS-INFO FUNCTION))
                    (RES NIL))
                (LET ((MIN-ARGS (LDB %%ARG-DESC-MIN-ARGS FAST-OPT))
                      (MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS FAST-OPT))
                      (EVALED-REST (LDB %%ARG-DESC-EVALED-REST FAST-OPT)))
                  (OR (ZEROP (LDB %%ARG-DESC-QUOTED-REST FAST-OPT))
                      (FERROR NIL "Obsolete special form, recompile the definition of ~S" FUNCTION))
                  (DOTIMES (I MIN-ARGS)
                    (PUSH (CAAR ARG-MAP) RES)
                    (SETQ ARG-MAP (CDR ARG-MAP)))
                  (OR (= MIN-ARGS MAX-ARGS)
                      (PUSH '&OPTIONAL RES))
                  (DOTIMES (I (- MAX-ARGS MIN-ARGS))
                    (PUSH (CAAR ARG-MAP) RES)
                    (SETQ ARG-MAP (CDR ARG-MAP)))
                  (WHEN (NOT (ZEROP EVALED-REST))
                    (PUSH '&REST RES)
                    (PUSH (CAAR LOCAL-MAP) RES))
                  (NREVERSE RES)))))
       (CDR (ASSQ 'VALUES DEBUG-INFO))))
    (MICROCODE-FUNCTION
     (MICRO-CODE-ENTRY-ARGLIST-AREA (%POINTER FUNCTION)))
    (T (FERROR "~S is not a function" FUNCTION))))

;;; Note: :ARGLIST will be canonicalized into ARGLIST at compile time,
;;; but in the mean time it will still appear in QFASL files.
;(DEFUN DEBUG-INFO-ARGLIST (DEBUG-INFO)
;  (LET ((AL (CDR (OR (ASSQ 'ARGLIST DEBUG-INFO)
;                    (ASSQ ':ARGLIST DEBUG-INFO)))))
;    ;;Take this check out someday after everything recompiled
;    (IF (OR (ATOM AL) (ATOM (CAR AL)) (CDR AL))
;       AL                                      ;New format
;      (CAR AL))))                              ;Old format

;;; Note: :VALUES, RETURN-LIST and :RETURN-LIST
;;; will be canonicalized into VALUES at compile time,
;;; but in the mean time they will still appear in QFASL files.
;(DEFUN DEBUG-INFO-VALUES (DEBUG-INFO)
;  (LET ((RL (OR (CDR (ASSQ 'RETURN-LIST DEBUG-INFO))
;               (CDR (ASSQ ':RETURN-LIST DEBUG-INFO))
;               (CDR (ASSQ 'VALUES DEBUG-INFO))
;               (CDR (ASSQ ':VALUES DEBUG-INFO)))))
;    ;;Take this check out someday after everything recompiled
;    (IF (ATOM (CAR RL)) RL                     ;New format
;      (CAR RL))))                              ;Old format

;;; Given an EVCP (with a data type of DTP-LOCATIVE, presumably),
;;; return the symbol or function spec whose value or function cell it points to,
;;; and a keyword saying what cell is pointed to.
;;; The keyword is a global function name which, applied to the first value,
;;; would yield the contents of the cell.
(DEFUN DECODE-EVCP (PTR-AS-LOCATIVE &AUX PTR CELL OFFSET)
  (SETQ PTR (%FIND-STRUCTURE-HEADER PTR-AS-LOCATIVE)
        OFFSET (%POINTER-DIFFERENCE PTR-AS-LOCATIVE PTR))
  (TYPECASE PTR
    (SYMBOL
     (SETQ CELL (NTH OFFSET '(%P-CONTENTS SYMEVAL FDEFINITION PLIST SYMBOL-PACKAGE))))
    (CONS
     (SETQ PTR (CAR PTR) CELL 'FDEFINITION))
    (T (SETQ CELL 'CAR)))
  (VALUES PTR CELL))

;;; Like %ARGS-INFO but also works for interpreted functions
(DEFUN ARGS-INFO (FCN)
  "Returns a fixnum called the /"numeric argument descriptor,/" which
describes the way it takes arguments.  This is used internally by the
microcode, the evaluator, and the compiler."
  ;; First, convert FCN from a function-spec to a function
  (LOOP WHILE (OR (SYMBOLP FCN)
                  (AND (CONSP FCN)
                       (NOT (MEMQ (CAR FCN) FUNCTION-START-SYMBOLS))))
     DO (SETQ FCN (FDEFINITION FCN)))
  (COND ((CLOSUREP FCN)
         (ARGS-INFO (CLOSURE-FUNCTION FCN)))
        ((ATOM FCN)
         (%ARGS-INFO FCN))
        ((MEMQ (CAR FCN) '(CURRY-BEFORE CURRY-AFTER MACRO))
         %ARG-DESC-EVALED-REST)         ;Most unspecific value
        (T
         (ARGS-INFO-FROM-LAMBDA-LIST (CAR (LAMBDA-EXP-ARGS-AND-BODY FCN))))))

(DEFUN LAMBDA-EXP-ARGS-AND-BODY (LAMBDA-EXP)
  "Return a list containing the arglist and body of LAMBDA-EXP.
This is a list whose car is the arglist and whose cdr is the body."
  (IF (MEMQ (CAR LAMBDA-EXP) '(NAMED-LAMBDA NAMED-SUBST))
      (CDDR LAMBDA-EXP)
      (CDR LAMBDA-EXP)))

(DEFUN ARGS-INFO-FROM-LAMBDA-LIST (LL &AUX (FLAGS 0) QUOTE MIN (N 0))
  (DOLIST (L LL)
    (CASE L
      (&QUOTE (SETQ QUOTE T))
      (&EVAL (SETQ QUOTE NIL))
      (&OPTIONAL (SETQ MIN N))
      (&AUX (RETURN NIL))
      (&REST (RETURN (SETQ FLAGS (LOGIOR FLAGS
                                         (COND (QUOTE %ARG-DESC-QUOTED-REST)
                                               (T %ARG-DESC-EVALED-REST))))))
      (OTHERWISE                                ;A variable
       (COND ((NOT (MEMQ L LAMBDA-LIST-KEYWORDS))
              (IF QUOTE                         ;Quoted regular args present
                  (SETQ FLAGS (LOGIOR FLAGS (LOGIOR %ARG-DESC-INTERPRETED
                                                    %ARG-DESC-FEF-QUOTE-HAIR))))
              (INCF N))))))
  (OR MIN (SETQ MIN N))                         ;No optionals
  (DPB N %%ARG-DESC-MAX-ARGS
       (DPB MIN %%ARG-DESC-MIN-ARGS
            FLAGS)))

;;; Return the debugging info alist of a function.  NIL if there is none or unreasonable arg.
;;; Elements of the alist look like one of these things:
;;; (SI:ENCAPSULATED-DEFINITION <internal symbol> <type of encapsulation>)
;;;    This means that this definition was made to encapsulate an inner definition.
;;;    See the file SYS2; ENCAPS for more info on these.
;;; (SI:RENAMINGS <alist of renamings>)
;;;    This sort of item is used together with (ENCAPSULATED-DEFINITION ... SI:RENAME-WITHIN).
;;;    It specifies what renamings are to be done to the original definition.
;;;    It is an alist of entries of the form (<symbol to rename>  <new name>).
;;;    See SYS2; ENCAPS for more information.
;;; (ARGLIST . <arglist>)
;;;    The CDR of this element is an arglist to give the user when he or she asks.
;;;    It is set up by having (DECLARE (ARGLIST . <arglist>)) ...)
;;;    around the function definition when it is compiled.
;;;    This is for when the function's actual arglist is not informative enough.
;;; (VALUES . <return-list>)
;;;    The CDR of this is the list of names of returned values,
;;;    to return as the second value if the user calls ARGLIST.
;;;    It is set up only by a local declare like the one which specifies
;;;    an arglist (above).
;;; (COMPILER::LOCAL-MAP <local map>)
;;;    The CADR of this element is a local map which indicates how local variables
;;;    are assigned to slots in the function's local block.
;;;    The n'th element of the map is a list of the locals that live there.
;;;    Actually, as of now, only one local can live in each slot, so the elements
;;;    of the map are at most of length one.  The format was chosen to allow expansion.
;;; (COMPILER::LOCAL-FUNCTION-MAP <local function map>)
;;;    Just like local map, where the nth slot of the map is the name for the nth
;;;    internal function, or nil if it was unnamed (eg #'(lambda (...) ...))
;;; (COMPILER::ARG-MAP <arg map>)
;;;    This is just like a local map except that it describes slots in the argument block
;;;    rather than slots in the local block.  It replaces keeping names in the ADL.
;;; (COMPILER::ARGLIST . <arglist>)
;;;    Like ARGLIST but is generated by the compiler and is not affected by user declarations
;;;    Used by the compiler for checking arguments for function calls
;;; (SYS:FUNCTION-PARENT <name>)
;;;    Gives the name of a definition whose source code includes this function.  This
;;;    is for functions automatically generated by defstruct, defflavor, etc.
;;; (:INTERNAL-FEF-OFFSETS <o1> <o2> ...)
;;;    Offsets within the fef of function cells for the :INTERNAL functions
;;; (SYS:INTERPRETED-DEFINITION
;;;    Gives the interpreted definition of the function.
;;;    This will be present if the function was compiled in core,
;;;    or if the interpreted definition is a SUBST (for then it is required
;;;    in order to expand the SUBST).
;;; (:MACROS-EXPANDED <macro1> <macro2>)
;;;    Gives the list of all macros expanded in compiling this function.
;;;
;;; The debugging info in a fef is made by the (DEBUG-INFO ...) lap instruction.
;;; A NAMED-LAMBDA can also have debugging info.  If its CADR is not a symbol,
;;; then it should be a list whose car is the function name and whose cdr is
;;; the debugging info alist.

;;; Our arg can be a function or a function spec.
(DEFUN DEBUGGING-INFO (FUNCTION &OPTIONAL UNENCAPSULATE-P)
  "Return the debugging info alist of a function or function spec.
UNENCAPSULATE-P non-NIL means if this function is an encapsulation
 return the debugging info of what it ultimately encapsulates."
    (DO () ((NOT (SYMBOLP FUNCTION)))
      (COND ((INTERPRETER-SPECIAL-FORM FUNCTION)
             (SETQ FUNCTION (INTERPRETER-SPECIAL-FORM-HANDLER (INTERPRETER-SPECIAL-FORM FUNCTION))))
            ((not (fboundp function))
             (return nil))      ;avoid error, probably eventually return NIL.
            ('ELSE
             (SETQ FUNCTION (SYMBOL-FUNCTION FUNCTION)))))
    (DO () ((NOT (LAMBDA-MACRO-CALL-P FUNCTION)))
      (SETQ FUNCTION (LAMBDA-MACRO-EXPAND FUNCTION)))
    (COND ((OR (AND (CONSP FUNCTION)
                    (MEMQ (CAR FUNCTION) '(NAMED-LAMBDA NAMED-SUBST))
                    (CONSP (CADR FUNCTION)))
               (TYPEP FUNCTION 'COMPILED-FUNCTION))
           ;; Ok, this is a function with some debug info.  Get its debug info.
           (LET ((DI (IF (CONSP FUNCTION)
                         (CDADR FUNCTION)
                         (FEF-DEBUGGING-INFO FUNCTION))))
             ;; Unencapsulate if requested and appropriate.
             (IF (AND UNENCAPSULATE-P
                      (ASSQ 'ENCAPSULATED-DEFINITION DI))
                 (DEBUGGING-INFO (CADR (ASSQ 'ENCAPSULATED-DEFINITION DI)) T)
               DI)))
          ((ATOM FUNCTION)
           NIL)
          ((MEMQ (CAR FUNCTION) FUNCTION-START-SYMBOLS)
           NIL)
          ((EQ (CAR FUNCTION) 'MACRO)
           (DEBUGGING-INFO (CDR FUNCTION)))
          (T (DEBUGGING-INFO (FDEFINITION FUNCTION) UNENCAPSULATE-P))))

;;; Old name which should be flushed eventually.
(DEFF FUNCTION-DEBUGGING-INFO 'DEBUGGING-INFO)
(compiler:make-obsolete function-debugging-info "Use DEBUGGING-INFO")

(defsubst fef-debugging-info-present-p (fef)
  (ldb-test %%fefhi-ms-debug-info-present
            (%p-contents-offset fef %fefhi-misc)))

(defun fef-debugging-info (fef)
  (and (fef-debugging-info-present-p fef)
       (%p-contents-offset fef (1- (%p-ldb %%fefh-pc-in-words fef)))))

(defsetf fef-debugging-info set-fef-debugging-info)
(deflocf fef-debugging-info locate-fef-debugging-info)

(defun set-fef-debugging-info (fef value)
  (if (fef-debugging-info-present-p fef)
      (let ((%inhibit-read-only t))
        (setf (%p-contents-offset fef (1- (%p-ldb %%fefh-pc-in-words fef))) value))
    (ferror "~S has nowhere to put debugging-info" fef)))

(defun locate-fef-debugging-info (fef)
  (if (fef-debugging-info-present-p fef)
      (%make-pointer-offset dtp-locative (follow-structure-forwarding fef)
                                         (1- (%p-ldb %%fefh-pc-in-words fef)))
    (ferror "~S has no debugging-info" fef)))


;;;; Macro expansion.

(defvar record-macros-expanded nil
  "Non-NIL means whenever a macro is expanded, push its name onto MACROS-EXPANDED.")
(defvar macros-expanded nil
  "When a macro call is expanded, its name is pushed on here, if RECORD-MACROS-EXPANDED is non-NIL.")

(defun record-macro-expanded (name)
  (and record-macros-expanded
       (pushnew name macros-expanded :test #'eq)))

(defvar *macroexpand-hook* 'funcall
  "The value is a function called to expand a macro call.
The first arg is the macro's expander function.
The second arg is the macro call itself.")

(defvar *macroexpand-environment* nil
  "When macro expander functions are called, this is the lexical environment
passed to MACROEXPAND-1.
If the expander calls MACROEXPAND itself, it can pass this as the second arg.")

(defun call-macro-expander (expander macro-call environment)
  (let ((*macroexpand-environment* environment)
        (ainf (args-info expander)))
    (cond ((or (symbolp expander) ;; No check for explicit macro expanders
               (> (ldb %%arg-desc-max-args ainf) 1))
           (values (funcall *macroexpand-hook* expander macro-call environment)
                   t))
          (t
           (values (funcall *macroexpand-hook* expander macro-call)
                   t)))))

;;; Macroexpand MACRO-CALL once, if possible.
;;; If there is nothing to expand, return it unchanged.
;;; Macros, open-coded functions and CURRY-BEFORE and CURRY-AFTER are expanded.
;;; ||| Changes for macroexpansion warnings during cross compilation. smh 29sep88
;;; ||| Cross compilation now ignores native substs, producing a warning. smh 14oct88
(defun expand-macro-call (macro-call environment expand-substs &key inhibitor macro-function-finder)
  (declare (values expansion expanded-flag))
  (if (atom macro-call) macro-call
    (let ((name (car macro-call))
          (out-of-this-world nil))
      (cond ((not (atom name))
             (cond ((eq (caar macro-call) 'curry-after)
                    (values `(,(cadar macro-call) ,@(cdr macro-call) . ,(cddar macro-call))
                            t))
                   ((eq (caar macro-call) 'curry-before)
                    (values `(,(cadar macro-call) ,@(cddar macro-call) . ,(cdr macro-call))
                            t))
                   ;; Have to ignore EXPAND-SUBSTS here since compiler can't deal with ((SUBST ...) ...)
                   ((memq (caar macro-call) '(subst cli:subst named-subst))
                    (values (funcall *macroexpand-hook* 'subst-expand-1 macro-call environment)
                            t))
                   (t macro-call)))
            ((not (symbolp name)) macro-call)
            (t
             (let ((tm (fsymeval-in-environment name environment nil)))
               (cond (tm ; local definition
                      (if (eq (car-safe tm) 'macro)
                          (call-macro-expander (cdr tm) macro-call environment)
                        macro-call))
                     ;; Possibly inhibit macro expansion.
                     ((and inhibitor (funcall inhibitor name)) macro-call)
                     ((and macro-function-finder (setq tm (funcall macro-function-finder name)))
                      (record-macro-expanded name)
                      (values (call-macro-expander tm macro-call environment)
                              t))
                     (t
                      (multiple-value-setq (tm out-of-this-world) (declared-definition name))
                      (cond ((typep tm 'compiled-function)      ; Possible compiler-defined subst
                             (if expand-substs
                                 (if out-of-this-world          ;||| Cross compiler ignores native SUBSTs - 14oct smh
                                     (progn
                                       #+never  ;||| smh 18oct88
                                       (compiler:warn 'cross-compilation :NOT-PORTABLE
                                         "Cross compilation for ~a ignoring ~s subst definition from ~a host."
                                         compiler:*target-computer* name compiler:*host-computer*)
                                       macro-call)
                                   (progn
                                     ;; If function is compiled, see if its interpreted defn is recorded.
                                     (setq tm (assq 'interpreted-definition (debugging-info tm)))
                                     (if (and tm (memq (caadr tm) '(subst cli:subst named-subst)))
                                         (progn
                                           (record-macro-expanded name)
                                           (values (funcall *macroexpand-hook* 'subst-expand-1 macro-call environment)
                                                   t))
                                       macro-call)))
                               macro-call))
                            ((atom tm) macro-call) ; ordinary function
                            ((eq (car tm) 'macro)
                             (when out-of-this-world
                               (compiler:warn 'cross-compilation :NOT-PORTABLE
                                 "Cross compilation for ~a taking ~s macro definition from ~a host."
                                 compiler:*target-computer* name compiler:*host-computer*))
                             (record-macro-expanded name)
                             (values (call-macro-expander (cdr tm) macro-call environment) t))
                            ((memq (car tm) '(subst cli:subst named-subst)) ; interpreter-defined SUBST
                             (if expand-substs
                                 (if out-of-this-world          ;||| Cross compiler ignores native SUBSTs - 14oct smh
                                     (progn
                                       #+never  ;||| smh 18oct88
                                       (compiler:warn 'cross-compilation :NOT-PORTABLE
                                         "Cross compilation for ~a ignoring ~s subst definition from ~a host."
                                         compiler:*target-computer* name compiler:*host-computer*)
                                       macro-call)
                                   (progn
                                     (record-macro-expanded name)
                                     (values (funcall *macroexpand-hook* 'subst-expand-1 macro-call environment)
                                             t)))
                               macro-call))
                            (t macro-call)))
                     (t macro-call))))))))

;;; ||| Changes for macroexpansion warnings during cross compilation. smh 29sep88
(defun macroexpand-1 (macro-call &optional environment)
  "Expand MACRO-CALL once and return the result.
Macro calls, uses of SUBSTs, uses of CURRY-BEFORE and CURRY-AFTER,
and uses of functions for which OPEN-CODE-P is true, are all expanded.
Uses of some Common Lisp special forms will be expanded; use LISP:MACROEXPAND-1
to avoid this.
The second value is T if there was something to expand.
If SYS:RECORD-MACROS-EXPANDED is non-NIL, all macro names are pushed
on SYS:MACROS-EXPANDED.
The value of *MACROEXPAND-HOOK* (which should behave like FUNCALL)
is used to invoke the expander function."
  (declare (values expansion expanded-flag))
  ;; ||| Expand substs only during native compilation. -- smh 28sep88
  (expand-macro-call macro-call environment t))

;;; Expand any macros in top level of a form.
(defun macroexpand (macro-call &optional environment)
  "Expand MACRO-CALL repeatedly until the result is not a macrocall.
Uses of some Common Lisp special forms will be expanded; use LISP:MACROEXPAND
to avoid this."
    (do ((tm macro-call (macroexpand-1 tm environment))
         (otm nil tm))
        ((or (eq tm otm) (atom tm)) tm)))

;;; ||| Changes for macroexpansion warnings during cross compilation. smh 29sep88
(defun cl:macroexpand-1 (macro-call &optional environment)
  "Expand MACRO-CALL once and return the result.
Macro calls, uses of SUBSTs, uses of CURRY-BEFORE and CURRY-AFTER,
and uses of functions for which OPEN-CODE-P is true, are all expanded.
Common Lisp special forms are not expanded even if they are implemented
as macros; use ZL:MACROEXPAND-1 to get the expansion in terms of Zetalisp.
The second value is T if there was something to expand.
If SYS:RECORD-MACROS-EXPANDED is non-NIL, all macro names are pushed
on SYS:MACROS-EXPANDED.
The value of *MACROEXPAND-HOOK* (which should behave like FUNCALL)
is used to invoke the expander function."
  (declare (values expansion expanded-flag))
  ;; ||| Expand substs only during native compilation. -- smh 28sep88
  (expand-macro-call macro-call environment t
                     :inhibitor #'common-lisp-special-form-p
                     :macro-function-finder #'macro-function))

(defun cl:macroexpand (macro-call &optional environment)
  "Expand MACRO-CALL repeatedly until the result is not a macrocall.
Common Lisp special forms are not expanded even if they are implemented
as macros; use ZL:MACROEXPAND to get the expansion in terms of Zetalisp."
    (do ((tm macro-call (cl:macroexpand-1 tm environment))
         (otm nil tm))
        ((or (eq tm otm) (atom tm)) tm)))

;;; Push a random declaration on for the duration of a file being compiled.

(DEFUN PUTDECL (NAME PROP VALUE)
  "Executed while compiling a file, creates a compile-time property.
Compile-time properties are accessed using GETDECL."
  ;; Theoretically, when not somewhere inside a COMPILE-FILE, i.e. when there is no
  ;; *COMPILATION-ENVIRONMENT* established, PUTDECL should not be called at all.
  ;; Unfortunately, certain obnoxious macros like DEFSTRUCT expand to
  ;; EVAL-WHEN-COMPILE DEFDECL forms, effectively solving a problem twice instead
  ;; instead of just once, thereby causing much grief.  So we make PUTDECL do
  ;; the right thing if there is no *COMPILATION-ENVIRONMENT*.
  (if (and (boundp 'compiler:*compilation-environment*) ; $$$ <04-Nov-88 Fixed for makesystem. smh&keith>
           compiler:*compilation-environment*)
      ;; A more trusting loser might try to do this atomically without the intermediate
      ;; fetch of the whole plist.
      (let ((plist (gethash name
                            (compiler:compilation-environment-plist-hashtab
                              compiler:*compilation-environment*))))
        (setf (getf plist prop) value)
        (setf (gethash name (compiler:compilation-environment-plist-hashtab
                              compiler:*compilation-environment*))
              plist))
    (putprop name value prop)))

;;; Get either the current loaded definition or a property
;;; or the actual value of the property.
(DEFUN GETDECL (NAME PROP)
  "GET, for macro expansion and compilation.
Allows the actual property of NAME to be overridden
by a local declaration (prop name value)
such as PUTDECL or DEFDECL would create.
NAME may be any symbol or function spec."
  ;;;Go see the junk in LISP-REINITIALIZE to support random variables used here,
  ;;;which otherwise cause grief in cold load, and tell me, why do you like LISP?
  (DOLIST (DECL LOCAL-DECLARATIONS)
    (AND (EQ (CAR DECL) PROP)
         (EQUAL (CADR DECL) NAME)
         (RETURN-from getdecl (CADDR DECL))))
  (when (boundp 'compiler:*compilation-environment*)
    (do ((env compiler:*compilation-environment* (compiler:compilation-environment-next env)))
        ((null env))
      (multiple-value-bind (val foundp)
          (gethash name (compiler:compilation-environment-plist-hashtab env))
        (when foundp
          (setq val (getf val prop))            ;what about property of NIL ???
          (when val (return-from getdecl val))))))
  #+never
  (DOLIST (DECL FILE-LOCAL-DECLARATIONS)
    (AND (EQ (CAR DECL) PROP)
         (EQUAL (CADR DECL) NAME)
         (RETURN (CADDR DECL))))
  ;; The AND clause added to keep the cross compiler from seeing inappropriate DEFTYPE and
  ;; SETF-METHOD definitions from the compiling host. ||| smh 28sep88
  (and (eq compiler:*host-computer* compiler:*target-computer*)
       (IF (SYMBOLP NAME)
           (GET NAME PROP)
         (FUNCTION-SPEC-GET NAME PROP))))

;;; ||| New function. smh 29sep88
(defmacro compiler::inherit-lambda-macro-definitions (&rest function-specs)
  (dolist (func function-specs)
    (setf (gethash func (compiler:compilation-environment-macro-hashtab compiler:*compilation-environment*))
          :inherit-lambda-macro-definition))
  'nil)

;;; ||| Warnings (etc) for cross compiling macro definitions. smh 29sep88
(DEFUN DECLARED-DEFINITION (FUNCTION-SPEC &AUX DEF (out-of-this-world nil) (allow-out-of-this-world nil))
  "Return the definition of FUNCTION-SPEC for macro expansion purposes.
This may be the actual definition, or it may be specified by
a local declaration.  If it is encapsulated, unencapsulate it."
  (SETQ DEF (OR (DOLIST (L LOCAL-DECLARATIONS)
                  (AND (EQ (CAR L) 'DEF)
                       (EQUAL (CADR L) FUNCTION-SPEC)           ;Not EQ, might be a list
                       (RETURN (CDDR L))))
                (when (boundp 'compiler:*compilation-environment*)
                  (do ((env compiler:*compilation-environment* (compiler:compilation-environment-next env))
                       tem)
                      ((null env))
                    (when (setq tem (gethash function-spec (compiler:compilation-environment-macro-hashtab env)))
                      (if (eq tem :inherit-lambda-macro-definition)
                          (setq allow-out-of-this-world 't)
                        (return tem)))))
                #+never
                (DOLIST (L FILE-LOCAL-DECLARATIONS)
                  (AND (EQ (CAR L) 'DEF)
                       (EQUAL (CADR L) FUNCTION-SPEC)           ;Not EQ, might be a list
                       (RETURN (CDDR L))))
                ;; If we wanted to keep the Falcon cross compiler from seeing Lambda macro definitions we should
                ;; conditionalize out the following form.  While this would unquestionably be the right thing
                ;; to do, in practice it would make the porting job much much more tedious. - smh 26sep88
                (let ((from-this-world (AND (FDEFINEDP FUNCTION-SPEC)
                                            (SETQ DEF (FDEFINITION FUNCTION-SPEC))
                                            (COND ((ATOM DEF) DEF)
                                                  ((EQ (CAR DEF) 'MACRO) DEF)
                                                  (T (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC FUNCTION-SPEC)))))))
                  (and from-this-world
                       (not allow-out-of-this-world)
                       (not (eq compiler:*host-computer* compiler:*target-computer*))
                       (setq out-of-this-world 't))
                  from-this-world)))
  (COND ((AND DEF (SYMBOLP DEF))
         (multiple-value-bind (def out-of-that-world)
             (DECLARED-DEFINITION DEF)
           (values def (or out-of-this-world out-of-that-world))))
        (T (values DEF out-of-this-world))))

(DEFUN SUBST-EXPAND-1 (FORM ENVIRONMENT)
  (LET ((SUBST (CAR FORM))
        SIMPLE-SUBSTITUTION-OK)
    (DO-FOREVER
      (COND ((SYMBOLP SUBST)
             (SETQ SUBST (DECLARED-DEFINITION SUBST)))
            ((TYPEP SUBST 'COMPILED-FUNCTION)
             (LET ((DI (DEBUGGING-INFO SUBST)))
               (SETQ SIMPLE-SUBSTITUTION-OK
                     (NOT (ASSQ ':NO-SIMPLE-SUBSTITUTION DI)))
               (SETQ SUBST (CADR (ASSQ 'INTERPRETED-DEFINITION DI)))))
            (T (RETURN))))
    (SUBST-EXPAND SUBST FORM ENVIRONMENT SIMPLE-SUBSTITUTION-OK)))

;;; Expand a call to a SUBST function.  SUBST is the function definition to use.
;;; FORM is the whole form.
;;; Match the SUBST args with the expressions in the form
;;; and then substitute the expressions for the args in the body of the function with SUBLIS.

(DEFUN SUBST-EXPAND (SUBST FORM ENVIRONMENT SIMPLE-SUBSTITUTION-OK)
  (LET (ALIST OPTIONAL-FLAG REST-ALREADY-FLAG LAMBDA-LIST BODY FN-NAME)
    ;; Extract the lambda-list, body, and function name from the definition.
    (COND ((EQ (CAR SUBST) 'NAMED-SUBST)
           (SETQ LAMBDA-LIST (CADDR SUBST) BODY (CDDDR SUBST))
           (SETQ FN-NAME (COND ((SYMBOLP (CADR SUBST)) (CADR SUBST))
                               (T (CAADR SUBST)))))
          (T (SETQ LAMBDA-LIST (CADR SUBST) BODY (CDDR SUBST)
                   FN-NAME (CAR FORM))))
    ;; Discard documentation string or declarations from front of body.
    (SETQ BODY (EXTRACT-DECLARATIONS BODY NIL T ENVIRONMENT))
    ;; Provide an implicit PROGN for the body.
    (IF (CDR BODY)
        (SETQ BODY `(PROGN . ,BODY))
      (SETQ BODY (CAR BODY)))
    ;;???? Flush the implicitly generated BLOCK.
    ;; This is a kludge, indeed.
    (AND (EQ (CAR-SAFE BODY) 'BLOCK)
         (SETQ BODY (CONS 'PROGN (CDDR BODY))))
    ;; Process the lambda list and args to make the alist.
    (DO ((VALS (CDR FORM) (CDR VALS)))
        (NIL)
      ;; We allow only &OPTIONAL and &REST.
      (DO-FOREVER
        (CASE (CAR LAMBDA-LIST)
          (&OPTIONAL (SETQ OPTIONAL-FLAG T))
          (&REST (OR REST-ALREADY-FLAG
                     (SETQ VALS (LIST (CONS 'LIST VALS))
                           REST-ALREADY-FLAG T)))
          (OTHERWISE (RETURN)))
        (POP LAMBDA-LIST))
      ;; All lambda-list keywords aside from &OPTIONAL and &REST are erroneous.
      (AND (MEMQ (CAR LAMBDA-LIST) LAMBDA-LIST-KEYWORDS)
           (RETURN
             (CONS (CERROR T NIL 'INVALID-FORM
                           "Subst-function ~S contains inappropriate keyword ~A."
                           FN-NAME (CAR LAMBDA-LIST))
                   (CDR FORM))))
      ;; Detect runout of lambda list or of args.
      (COND ((NULL VALS)
             (COND ((NULL LAMBDA-LIST)
                    (RETURN (IF SIMPLE-SUBSTITUTION-OK
                                (SUBLIS ALIST BODY)
                              (SUBLIS-EVAL-ONCE (NREVERSE ALIST) BODY nil nil ENVIRONMENT))))
                   ((NOT OPTIONAL-FLAG)
                    (RETURN (CERROR T NIL 'INVALID-FORM
                                    "Too few arguments for ~S."
                                    FN-NAME FORM)))))
            ((NULL LAMBDA-LIST)
             (RETURN (CERROR T NIL 'INVALID-FORM
                             "Too many arguments for ~S."
                             FN-NAME FORM))))
      ;; Here we have one more arg.  Add it to the alist.
      (PUSH (CONS (COND ((ATOM (CAR LAMBDA-LIST)) (CAR LAMBDA-LIST))
                        (T (CAAR LAMBDA-LIST)))
                  (COND (VALS (CAR VALS))
                        ((ATOM (CAR LAMBDA-LIST)) NIL)
                        (T (CADAR LAMBDA-LIST))))
            ALIST)
      (POP LAMBDA-LIST))))

;;; These are for reading in QCOM, and the like

;;;||| ASSIGN-VALUES got used in QFASL.LISP, so now it is needed in the cold load.
;;;Might as well keep the other (related) functions together. -Keith 27oct88

(DEFUN ASSIGN-ALTERNATE (X)
   (PROG ()
      L  (COND ((NULL X) (RETURN NIL)))
         (proclaim `(special ,(car x)))
         (SET (CAR X) (CADR X))
         (SETQ X (CDDR X))
         (GO L)))

(DEFUN ASSIGN-ALTERNATE-eval (X)
  ;the val had better evaluate "locally".  Example, (byte x y) is OK.
   (PROG ()
      L  (COND ((NULL X) (RETURN NIL)))
         (proclaim `(special ,(car x)))
         (let ((v (cadr x)))
           (SET (CAR X) (eval v)))
         (SETQ X (CDDR X))
         (GO L)))

(DEFUN GET-ALTERNATE (X)
  (PROG (Y)
     L  (COND ((NULL X) (RETURN (REVERSE Y))))
        (SETQ Y (CONS (CAR X) Y))
        (SETQ X (CDDR X))
        (GO L)))

(DEFUN ASSIGN-VALUES (INPUT-LIST &OPTIONAL (SHIFT 0) (INIT 0) (DELTA 1))
    (PROG ()
       L  (COND ((NULL INPUT-LIST) (RETURN INIT)))
          (proclaim `(special ,(car input-list)))
          (SET (CAR INPUT-LIST) (LSH INIT SHIFT))
          (SETQ INPUT-LIST (CDR INPUT-LIST))
          (SETQ INIT (+ INIT DELTA))
          (GO L)))

(DEFUN ASSIGN-VALUES-ALIST (INPUT-LIST &OPTIONAL (SHIFT 0))
    (PROG ()
       L  (COND ((NULL INPUT-LIST) (RETURN nil)))
          (proclaim `(special ,(caar input-list)))
          (SET (CAAR INPUT-LIST) (LSH (cadar input-list) SHIFT))
          (SETQ INPUT-LIST (CDR INPUT-LIST))
          (GO L)))

(DEFUN ASSIGN-VALUES-INIT-DELTA (INPUT-LIST SHIFT INIT DELTA)
  (PROG ()
     L  (COND ((NULL INPUT-LIST) (RETURN INIT)))
        (SET (CAR INPUT-LIST) (LSH INIT SHIFT))
        (SETQ INPUT-LIST (CDR INPUT-LIST))
        (SETQ INIT (+ INIT DELTA))
        (GO L)))

(DEFSUBST GET-FROM-ALTERNATING-LIST (L KEY)
 "Retreive associated item from an alternating list
Like GET, but no initial CAR"
  (GETF L KEY))

;GETF does not serve the purpose in all cases. --rg
;yes it does.  No it doesnt.  Why dont you talk to me about it instead of inserting dumb comments?
;(COMPILER:MAKE-OBSOLETE GET-FROM-ALTERNATING-LIST "use GETF instead")

(DEFUN PUT-ON-ALTERNATING-LIST (ITEM L KEY)
  "Put ITEM on an alternating association list L
Modifies the current association, if any.
Otherwise adds one to the head of the list.
Returns the augmented list as value.
The user should alway use this value unless he is
certain there is a current association."
  (PROG (PNTR)
        (SETQ PNTR L)
     L  (COND ((NULL L)
               (RETURN (CONS KEY (CONS ITEM L))))
              ((EQ KEY (CAR L))
               (RPLACA (CDR L) ITEM)
               (RETURN L)))
        (SETQ L (CDDR L))
        (GO L)))
;(COMPILER:MAKE-OBSOLETE PUT-ON-ALTERNATING-LIST "This function is a crock")
