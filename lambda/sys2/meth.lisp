;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; readtable: ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;Method Stuff.
; This is intended to provide a complete interface to the METHOD system, ie,
;this is the only part of the system which ever knows what a DTP-SELECT-METHOD
;is, etc.  The implementation could be changed to use hash tables, for example,
;with no changes elsewhere in the system.  (Or even, the choice between DTP-SELECT-METHOD
;and hash tables can be made dynamically by the system based on how many methods there are,
;which is probably what we will eventually want).

;What a DTP-SELECT-METHOD does:
;  When applied as a function, it assumes its first argument is
;an operation.  The DTP-SELECT-METHOD itself points to a (somewhat extended) ASSQ
;list, which associates possible operations with METHODs.  The given operation
;is looked up on the ASSQ list, and if found, the DTP-SELECT-METHOD replaces itself
;with the matched METHOD and reinvokes the function application mechanism.  If the
;search reaches the end of the ASSQ list, DTP-SELECT-METHOD reports an error if the
;ASSQ list terminated in NIL.  Otherwise, if it ended in a SYMBOL, the DTP-SELECT-METHOD
;replaces itself with that symbol and reinvokes the function application mechanism.
;Note that in the important case that this symbol happens to contain a DTP-SELECT-METHOD
;in its function cell, the result will be that the search continues using that
;ASSQ list, etc.

;What a DTP-SELECT-METHOD looks like:
; (1) it is normally found in the function cell of a symbol.  This symbol is referred
;     to as the CLASS-SYMBOL.
; (2) it is a list, each of whose elements may be:
;   (a) a CONS of a SYMBOL and a METHOD.  An ASSQ list element associating SYMBOL
;         (as an operation) with METHOD.  METHOD can be anything meaningful in LISP
;         function context.
;   (b) a CONS of a list of symbols and a METHOD.  Similar to 3, but all the symbols
;         are associated with the method
;         At the moment, this possibility is unused, because
;         making it work right through various sequences of redefining
;         some but not all of the symbols requires considerable hair.
;   (c) a SYMBOL (assumed to be a CLASS-SYMBOL).  A one level "subroutine call"
;       to the methods directly associated with the SYMBOL.  If a suitable method is
;       not found, the CLASS-SYMBOL's superclass, etc are not searched, instead,
;       the search resumes with the next element of the original SELECT-METHOD list.
;       This feature is used if the class has more than one superclass.  In that case,
;       ALL superior classes are enumerated in the SELECT-METHOD list of this class,
;       in the desired search order.
;  (3) a tail pointer.  If NIL, an error is reported if search reaches here, otherwise,
;       it should be a CLASS-SYMBOL for the superclass.
;
; A Class has a symbol associated with it, called the class-symbol.
; The value of the class-symbol is the class entity itself.
; The function definition of the class-symbol is the select-method.


;; This is the standard way of defining a method of a class,
;; so that the code will be compiled.  Note that DEFMETHOD works for
;; both Class methods and Flavor methods.
;; SPEC is one of (:message), (:BEFORE :message), or (:AFTER :message),
;; in the case where CLASS-NAME is a flavor.
;; If in place of the lambda-list you have a symbol, and the body
;; is null, that symbol is a function which stands in for the method.
;;*** This has been superseded by a definition in FLAVOR
#+NIL ;comment out next S-expression
(DEFMACRO DEFMETHOD ((CLASS-NAME . SPEC) LAMBDA-LIST . BODY)
  (COND ((AND (SYMBOLP LAMBDA-LIST) (NOT (NULL LAMBDA-LIST)) (NULL BODY))
         `(FDEFINE '(:METHOD ,CLASS-NAME ,@SPEC) ',LAMBDA-LIST))
        ((GET CLASS-NAME 'FLAVOR)
         `(LOCAL-DECLARE ((SPECIAL . ,(FLAVOR-INSTANCE-VARIABLES CLASS-NAME T T)))
            (DEFUN (:METHOD ,CLASS-NAME ,@SPEC) (.OPERATION. . ,LAMBDA-LIST)
              . ,BODY)))
        (T ;; The non-flavor class system
           (AND (CDR SPEC) (FERROR NIL "~S bad in non-flavor DEFMETHOD"
                                       (CONS CLASS-NAME SPEC)))
           (LET ((OPERATION (CAR SPEC)))
             (COND ((ATOM OPERATION)
                    `(PROGN 'COMPILE . ,(DEFMETHOD-1 CLASS-NAME OPERATION LAMBDA-LIST BODY)))
                   (T
                     (COND ((EQ (CAR OPERATION) 'QUOTE)
                            (CERROR NIL NIL ':NO-VALUE
                                    "Quote used in front of operation ~S in DEFMETHOD of ~S"
                                    OPERATION CLASS-NAME)))
                     `(PROGN 'COMPILE
                        . ,(MAPCAN (FUNCTION (LAMBDA (OP)
                                               (DEFMETHOD-1 CLASS-NAME OP LAMBDA-LIST BODY)))
                                   OPERATION))))))))

;; Interface from the Lisp machine's actual definition of DEFMETHOD.
(DEFUN DEFMETHOD-1 (CLASS-SYMBOL OPERATION ARGS BODY)
  `((LOCAL-DECLARE ((SPECIAL . ,(CLASS-VARS CLASS-SYMBOL)))
      (DEFUN (:METHOD ,CLASS-SYMBOL ,OPERATION) (.OPERATION. ,@ARGS)
             ,@BODY))))

(DEFMACRO DEFMETHOD-INSTANCE ((OBJ OPERATION) ARGS . BODY)
  "Defines a method OPERATION, with arguments ARGS, local to the
particular instance OBJ."
  (COND ((ATOM OPERATION)
         `(PROGN 'COMPILE . ,(DEFMETHOD-INSTANCE-1 OBJ OPERATION ARGS BODY)))
        (T
         (COND ((EQ (CAR OPERATION) 'QUOTE)
                (CERROR NIL NIL ':NO-VALUE
                        "Quote used in front of operation ~S in DEFMETHOD-INSTANCE of ~S"
                        OPERATION OBJ)))
         `(PROGN 'COMPILE
                 ,(MAPCAN (FUNCTION (LAMBDA (OP)
                            (DEFMETHOD-INSTANCE-1 OBJ OP ARGS BODY)))
                          OPERATION)))))

;Since it can't know what class OBJ is, no LOCAL-DECLARE of specials can be done.
(DEFUN DEFMETHOD-INSTANCE-1 (OBJ OPERATION ARGS BODY)
  `((DEFUN (:INSTANCE-METHOD ,OBJ ,OPERATION) (.OPERATION. ,@ARGS)
          ,@BODY)))

(DEFUN CLASS-VARS (CLASS-SYMBOL)
    (PROG CLASS-VARS ()
          ;; First, look for a local defclass declaration of this class.
          (DO LDS SYS:FILE-LOCAL-DECLARATIONS (CDR LDS) (NULL LDS)
              (AND (EQ (CAAR LDS) 'DEFCLASS)
                   (EQ (CADAR LDS) CLASS-SYMBOL)
                   ;; If found, get vars from it
                   (RETURN-FROM CLASS-VARS
                                (APPEND (CAR (CDDDAR LDS))
                                        ;; Appending to vars of superclass, with an escape
                                        ;; so we don't loop on OBJECT-CLASS.
                                        (COND ((EQ CLASS-SYMBOL (CADDAR LDS))
                                               NIL)
                                              ((SYMBOLP (CADDAR LDS))
                                               (CLASS-VARS (CADDAR LDS)))
                                              (T (APPLY (FUNCTION APPEND)
                                                        (MAPCAR (FUNCTION CLASS-VARS)
                                                                (CADDAR LDS)))))))))
          (RETURN (COND ((CLASS-SYMBOLP CLASS-SYMBOL)
                         (SYMEVAL-IN-CLOSURE (SYMEVAL CLASS-SYMBOL) 'INSTANCE-PATTERN))
                        (T NIL)))))

;This function is a loss since it precludes compilation.
; Not entirely right, but it will do for the time being.
(DEFUN PUTMETHOD (CLASS-SYMBOL CLASS-METHOD-SYMBOL MESSAGE &REST BODY)
  CLASS-METHOD-SYMBOL ;argument ignored
  (LET* ((OPERATION (CAR MESSAGE))
         (function-spec `(:METHOD ,CLASS-SYMBOL ,OPERATION)))
    (FSET-CAREFULLY function-spec
                     `(LAMBDA (.OPERATION. ,@(CDR MESSAGE)) ,@(APPEND BODY NIL)))
    (record-source-file-name function-spec)))


;This thing is a loss since the functions are not compiled.
; Now used only by DEFCLASS-BOOTSTRAP
(DEFUN DEFINE-ACCESSOR-METHODS (CLASS-SYMBOL CLASS-METHOD-SYMBOL INSTANCE-PATTERN)
  (DO L INSTANCE-PATTERN (CDR L) (NULL L)       ;PUT THE "SET'ERS" ON FIRST AS A SLIGHT
                                                ; EFFICIENCY HACK (ON THE THEORY THEY'RE
                                                ; USED LESS)
      (PUTMETHOD CLASS-SYMBOL
                 CLASS-METHOD-SYMBOL
                 (LIST (INTERN (STRING-APPEND (CAR L) "<-") PKG-KEYWORD-PACKAGE) 'A)
                 `(LOCALLY (DECLARE (SPECIAL ,(CAR L)))
                           (SETQ ,(CAR L) A))))
  (DO L INSTANCE-PATTERN (CDR L) (NULL L)
      (PUTMETHOD CLASS-SYMBOL
                 CLASS-METHOD-SYMBOL
                 (LIST (INTERN (STRING (CAR L)) PKG-KEYWORD-PACKAGE))
                 `(LOCALLY (DECLARE (SPECIAL ,(CAR L)))
                           ,(CAR L)))))

(DEFUN MAKE-METHOD-NAME (CLASS-SYMBOL MESSAGE-KEY)
  (LET ((CLASS (SYMEVAL CLASS-SYMBOL)))
    (INTERN (COND ((AND (FBOUNDP 'FORMAT) (NEQ CLASS-SYMBOL 'CLASS-CLASS))
                   (FORMAT NIL "~A-~A-METHOD-~A"
                           (<- CLASS ':NAME)
                           MESSAGE-KEY
                           (<- CLASS ':CLASS-VERSION-NUMBER)))
                  (T
                   (STRING-APPEND (SYMEVAL-IN-CLOSURE CLASS 'NAME)
                                  "-"
                                  MESSAGE-KEY
                                  "-METHOD"))))))

(DEFUN MAKE-INSTANCE-METHOD-NAME (INST MESSAGE-KEY)
 (INTERN (STRING-APPEND (CLASS-NAME INST)
                        "-"
                        (GENSYM)
                        "-"
                        MESSAGE-KEY
                        "-INSTANCE-METHOD")))

;STRIPS THE -CLASS, IF ITS THERE.
(DEFUN MAKE-CLASS-NAME (CLASS-SYMBOL)
    (COND ((AND (> (STRING-LENGTH CLASS-SYMBOL) 6)
                (EQUAL (NSUBSTRING CLASS-SYMBOL (- (STRING-LENGTH CLASS-SYMBOL) 6))
                       "-CLASS"))
           (INTERN (NSUBSTRING CLASS-SYMBOL 0  (- (STRING-LENGTH CLASS-SYMBOL) 6))))
          (T CLASS-SYMBOL)))

(DEFUN MAKE-PHANTOM-CLASS-NAME (INST)
  (INTERN (STRING-APPEND (CLASS-NAME INST)
                         "-INSTANCE-"
                         (GENSYM)
                         "-PHANTOM-CLASS")))

(DEFUN ADD-METHOD (CLASS-SYMBOL CLASS-METHOD-SYMBOL OPERATION METHOD)
  (OR (SYMBOLP OPERATION) (LISTP OPERATION)
      (FERROR NIL
  "The operation ~S, is not a SYMBOL or a CONS.  CLASS-SYMBOL= ~S, method= ~S -- ADD-METHOD"
              OPERATION CLASS-SYMBOL METHOD))
  (LET ((ML (METHOD-LIST CLASS-METHOD-SYMBOL))
        (TEM))
    (COND ((SETQ TEM (ASSOC-CAREFUL OPERATION ML))
           (RPLACD TEM METHOD))
          (T (FSET CLASS-METHOD-SYMBOL (MAKE-SELECT-METHOD (CONS (CONS OPERATION METHOD) ML)))))))

(DEFUN REMOVE-METHOD (CLASS-SYMBOL CLASS-METHOD-SYMBOL OPERATION METHOD)
  (OR (SYMBOLP OPERATION) (LISTP OPERATION)
      (FERROR NIL
  "The operation ~S, is not a SYMBOL or a CONS.  CLASS-SYMBOL= ~S, method= ~S -- ADD-METHOD"
              OPERATION CLASS-SYMBOL METHOD))
  (LET ((ML (METHOD-LIST CLASS-METHOD-SYMBOL))
        (TEM))
    (COND ((SETQ TEM (ASSOC-CAREFUL OPERATION ML))
           (FSET CLASS-METHOD-SYMBOL (MAKE-SELECT-METHOD (REMQ-SAFE TEM ML)))))))

(defun remq-safe (item list)
  (cond ((atom list) list)
        ((eq item (car list))
         (cdr list))
        ((memq-safe item list)
         (cons (car list) (remq-safe item (cdr list))))
        (t list)))

(defun memq-safe (item list)
  (do p list (cdr p) (atom p)
      (cond ((eq item (car list))
             (return list)))))

(DEFUN ADD-INSTANCE-METHOD (INST OPERATION METHOD)
  (ASSURE-INSTANCE-HAS-PHANTOM-CLASS INST)
  (ADD-METHOD (CLASS INST)
              (CAR (%MAKE-POINTER DTP-LIST INST))
              OPERATION METHOD))

(DEFUN ASSURE-INSTANCE-HAS-PHANTOM-CLASS (INST)
    (COND ((NULL (INSTANCE-HAS-PHANTOM-CLASS-P INST))
           (COND ((ENTITYP INST)
                  (MAKE-PHANTOM-CLASS-FOR-ENTITY INST))
                 (T (FERROR NIL "Can't make phantom class for ~S" INST))))))

(DEFUN INSTANCE-HAS-PHANTOM-CLASS-P (INST)
    (COND ((ENTITYP INST)
           (GET (CLASS-METHOD-SYMBOL INST)
                ':PHANTOM-CLASS))
          (T (FERROR NIL "Phantom classes not defined for ~S" inst))))

(DEFUN MAKE-PHANTOM-CLASS-FOR-ENTITY (INST)
 (LET ((NCS (MAKE-PHANTOM-CLASS-NAME INST))
       (NCSM (GENSYM))
       (OCS (CLASS INST)))
   (LET ((NC (<- CLASS-CLASS ':NEW
                 'CLASS-SYMBOL NCS
                 'CLASS-METHOD-SYMBOL NCSM
                 'SUPERCLASS OCS
                 'CLASS-VERSION-NUMBER 0)))
     (PUTPROP NCSM INST ':PHANTOM-CLASS)
     (RPLACA (%MAKE-POINTER DTP-LIST INST) NCSM)
     NC)))

;; (REMMETHOD 'FOO-CLASS ':BAR) removes any :BAR method from FOO-CLASS.
;;  This form provided for user typein convenience.  Programs should call
;;   REMMETHOD-1 to assure correct CLASS-METHOD-SYMBOL used if class has been
;;   redefined.
(DEFUN REMMETHOD (CLASS-SYMBOL OPERATION)
  "Removes the OPERATION method from CLASS-SYMBOL.  This form is
provided for user typein convenience.  Programs should use REMMETHOD-1
to assure correct CLASS-METHOD-SYMBOL is used if the class has been
redefined."
    (REMMETHOD-1 CLASS-SYMBOL
                 (SYMEVAL-IN-CLOSURE (SYMEVAL CLASS-SYMBOL) 'CLASS-METHOD-SYMBOL)
                 OPERATION))

;Takes arg of CLASS-SYMBOL just for ease of seeing whats going on if your tracing.
(DEFUN REMMETHOD-1 (CLASS-SYMBOL CLASS-METHOD-SYMBOL OPERATION)
    CLASS-SYMBOL  ;ignored
    (FSET CLASS-METHOD-SYMBOL
          (MAKE-SELECT-METHOD
               (DELQ (ASSQ-CAREFUL OPERATION (METHOD-LIST CLASS-METHOD-SYMBOL))
                     (METHOD-LIST CLASS-METHOD-SYMBOL))))
    T)

(DEFUN METHOD-LIST (CLASS-METHOD-SYMBOL)
  (COND ((NULL (FBOUNDP CLASS-METHOD-SYMBOL)) NIL)
        (T (LET ((FB (FSYMEVAL CLASS-METHOD-SYMBOL)))
            (COND ((= (%DATA-TYPE FB) DTP-SELECT-METHOD)
                   (COND ((ZEROP (%POINTER FB))         ;This should never happen
                          (FERROR NIL "Symbol has illegal SELECT-METHOD ~S"
                                  class-method-symbol)) ;delete after decent interval
                         (T (%MAKE-POINTER DTP-LIST FB))))
                  (T FB))))))

(DEFUN SET-METHOD-LIST (CLASS-METHOD-SYMBOL LST)
  (FSET CLASS-METHOD-SYMBOL (MAKE-SELECT-METHOD LST)))

(DEFUN METHOD-SUPERCLASS (CLASS-METHOD-SYMBOL)
  (CDR (LAST (METHOD-LIST CLASS-METHOD-SYMBOL))))

; In the multiple superclass case, a series of subroutine calls
; to all the superior classes must be generated.  The desired order
; has the property that if any superclass can be reached via more than one path,
; all the nodes along any of the paths by which it can be reached are
; enumerated before the node itself or any of its superclasses.  Since the
; tree is "fully expanded" (ie the entire path to the root is enumerated from
; every node each time that node appears), it wins to just delete leading
; duplicates from the flattened tree.

(DEFUN SET-METHOD-SUPERCLASS (CLASS-METHOD-SYMBOL SUPERCLASS)
   (LET ((ML (METHOD-LIST CLASS-METHOD-SYMBOL))
         (NML-TAIL (COND ((ENTITYP SUPERCLASS)
                           (<- SUPERCLASS ':CLASS-METHOD-SYMBOL))
                          (T (MAPCAR (FUNCTION (LAMBDA (X) (<- X ':CLASS-METHOD-SYMBOL)))
                               (FLATTEN-AND-DELETE-LEADING-DUPLICATES
                                (MAPCAR (FUNCTION (LAMBDA (X) (<- X ':CLASS-CLASS-HIERARCHY)))
                                        SUPERCLASS)))))))
;splice in NML-TAIL after any methods defined by this class.
     (COND ((NULL ML)
            (FSET CLASS-METHOD-SYMBOL (MAKE-SELECT-METHOD NML-TAIL)))
           (T (DO ((BP (FUNCTION-CELL-LOCATION CLASS-METHOD-SYMBOL) P)
                   (P ML (CDR P)))
                  ((OR (ATOM P)
                       (ATOM (CAR P)))
                   (RPLACD BP NML-TAIL)))))))

(DEFUN MAKE-SELECT-METHOD (L)
  (COND ((ATOM L) L)
        (T (%MAKE-POINTER DTP-SELECT-METHOD L))))

(DEFUN FLATTEN-AND-DELETE-LEADING-DUPLICATES (SHL)
 (PROG (ANS L P)
       (SETQ ANS (FLATTEN SHL))
       (SETQ P ANS L (VARIABLE-LOCATION ANS))
   L   (COND ((NULL P) (RETURN ANS))
             ((MEMQ (CAR P) (CDR P))  ;IF FROB IN LIST TWICE, DELETE FIRST COPY.
              (RPLACD L (CDR P)))
             (T (SETQ L P)))
       (SETQ P (CDR P))
       (GO L)
))

(DEFUN ALL-LEVELS-MEMQ (X L)
  (PROG NIL
    L   (COND ((ATOM L) (RETURN NIL))
              ((EQ X (CAR L))
               (RETURN T))
              ((CONSP (CAR L))
               (COND ((ALL-LEVELS-MEMQ X (CAR L))
                      (RETURN T)))))
        (SETQ L (CDR L))
        (GO L)))

(DEFUN FLATTEN (L)
  (NREVERSE (FLATTEN-1 L NIL)))

(DEFUN FLATTEN-1 (L HEAD)
  (PROG NIL
   L    (COND ((ATOM L) (RETURN HEAD))
              ((ATOM (CAR L))
               (SETQ HEAD (CONS (CAR L) HEAD)))
              (T (SETQ HEAD (FLATTEN-1 (CAR L) HEAD))))
        (SETQ L (CDR L))
        (GO L)))

;; (:METHOD class-name operation) refers to the method in that class for
;;   that operation; this works for both Class methods and Flavor methods.
(DEFUN CLASS-METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((CS (SECOND FUNCTION-SPEC))
        (OP (THIRD FUNCTION-SPEC)))
    (IF (NOT (AND (= (LENGTH FUNCTION-SPEC) 3)
                  (SYMBOLP CS)
                  (SYMBOLP OP)))
        (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
          (FERROR 'SYS:INVALID-FUNCTION-SPEC
                  "The function spec ~S is invalid." FUNCTION-SPEC))
      (SELECTQ FUNCTION
        (VALIDATE-FUNCTION-SPEC T)
        (FDEFINE (LET ((MN (MAKE-METHOD-NAME CS OP)))
                   (FSET MN ARG1)
                   ;; Can't send message because this has to work during
                   ;; loadup before messages work.
                   (ADD-METHOD CS
                               (SYMEVAL-IN-CLOSURE (SYMEVAL CS) 'CLASS-METHOD-SYMBOL)
                               OP
                               MN)))
        (FDEFINITION (FSYMEVAL (<- (SYMEVAL CS) ':METHOD-FOR OP)))
        (FDEFINEDP (AND (FBOUNDP 'CLASS-METHOD-FOR-METHOD)      ;Bootstrapping
                        (<- (SYMEVAL CS) ':METHOD-FOR OP)))     ;Second arg of NIL?
        (FDEFINITION-LOCATION (LOCF (FSYMEVAL (<- (SYMEVAL CS) ':METHOD-FOR OP))))
        (FUNDEFINE
         (LET ((MN (MAKE-METHOD-NAME CS OP)))
           (remove-method cs
                          (SYMEVAL-IN-CLOSURE (SYMEVAL CS) 'CLASS-METHOD-SYMBOL)
                          OP
                          MN)))
        (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))

;; (:INSTANCE-METHOD exp operation).  exp should evaluate to an DTP-INSTANCE.
;;   Reference is then to the operation directly on that instance.
(DEFPROP :INSTANCE-METHOD INSTANCE-METHOD-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN INSTANCE-METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((INST (EVAL (SECOND FUNCTION-SPEC)))
        (OP (THIRD FUNCTION-SPEC)))
    (IF (NOT (AND (= (LENGTH FUNCTION-SPEC) 3)
                  (ENTITYP INST)))
        (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
          (FERROR 'SYS:INVALID-FUNCTION-SPEC
                  "The function spec ~S is invalid." FUNCTION-SPEC))
      (SELECTQ FUNCTION
        (VALIDATE-FUNCTION-SPEC T)
        (FDEFINE (LET ((MN (MAKE-INSTANCE-METHOD-NAME INST OP)))
                   (FSET MN ARG1)
                   (ADD-INSTANCE-METHOD INST OP MN)))
        (FDEFINITION (FSYMEVAL (<- (CLASS INST) ':METHOD-FOR OP)))
        (FDEFINEDP (<- (CLASS INST) ':METHOD-FOR OP))
        (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))
