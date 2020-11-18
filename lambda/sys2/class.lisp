;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable: ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;a CLASS-SYMBOL is the name by which the user refers to the class.  It usually
;       has a -CLASS suffix.  Its value is
;       a DTP-ENTITY which is an instance of CLASS-CLASS.  Instances of CLASS-CLASS
;       contain a NAME, a CLASS-SYMBOL, and a CLASS-METHOD-SYMBOL (and some others
;       as well).  The NAME is used for printing out, and usually does not contain
;       the -CLASS suffix.
;an INSTANCE is a closure of some INSTANCE-VARIABLES and whose functional
;       component is a CLASS-METHOD-SYMBOL.
;a CLASS-METHOD-SYMBOL is usually a gensym created at class definition time.
;   The value of the CLASS-METHOD-SYMBOL is a DTP-ENTITY which is an instance of
;       CLASS-CLASS.  This is initially the identical instance which is in the
;       value cell of the CLASS-SYMBOL, however, if the class is redefined
;       (ie a new DEFCLASS done), the CLASS-SYMBOL will change, while the
;       current CLASS-METHOD-SYMBOL will not change, and instead a new one will be made.
;   The function cell of the CLASS-METHOD-SYMBOL is a DTP-SELECT-METHOD which holds
;       the methods of the class.
;   The CLASS-METHOD-SYMBOL has an SI:ENTITY-TYPEP property whose value
;       is the class-symbol.

;When DEFCLASS is done, a new CLASS-METHOD-SYMBOL is always created.  Any instances
;of the old class will be unaffected since they close over the old CLASS-METHOD-SYMBOL.
;The system attempts to alter the NAME of the old class so the user will
;be warned if he has one of these floating around.

;  Methods can be defined local to a  particular instance (as opposed to its class)
;by DEFMETHOD-INSTANCE.  However, it is assumed to be fairly rare to want to do this,
;and we dont want to garbage up all instances on this account.
; Accordingly, the first time this is
;done for a particular instance, a phantom CLASS is created which is a subclass
;of the original class.  The instance is then transmuted to the phantom class.
;Phantom classes have the :PHANTOM-CLASS property on the property list of the
;CLASS-METHOD-SYMBOL (for extra connectedness, the value of this property is the
;instance).

;With this new scheme, LISP-OBJECT-CLASS can eventually be flushed in favor of
; regular instances of CLASS-CLASS with special NEW methods.

;A CLASS is held on a lisp symbol, specially created for the purpose.
;  The function cell of the symbol contains the DTP-SELECT-METHOD for the class.

(DECLARE (SPECIAL CLASS-CLASS OBJECT-CLASS SELF))

(DECLARE (SPECIAL PRINT-ENTITY-ADDRESSES-FLAG))
(SETQ PRINT-ENTITY-ADDRESSES-FLAG T)

(DEFMACRO ENTITY (CLOSED-VARS-LIST FCTN)
  "Returns a newly created entity, which is like a closure except in the way some
system functions work on it.  Entities are mostly obsolete; use flavors."
  `(%MAKE-POINTER DTP-ENTITY (CLOSURE ,CLOSED-VARS-LIST ,FCTN)))

;;;; Various functions for sending messages to an instance.

(DEFUN <- (LOCAL-SELF MSG-KEY &REST REST)
  "Sends LOCAL-SELF the message MSG-KEY and REST."
  (typecase local-self
    (ENTITY
     (LEXPR-SEND LOCAL-SELF MSG-KEY REST))
    (instance
     (LEXPR-SEND LOCAL-SELF MSG-KEY REST))
    (T (LET ((SELF LOCAL-SELF))                 ;Avoid binding special var unless necessary.
         (LEXPR-FUNCALL (CLASS-METHOD-SYMBOL SELF) MSG-KEY REST)))))  ;slight speed bum.

;<<-- sends an object several messages in succession, as in
;(<<-- TVOB (:EDGES<- LEFT TOP RIGHT BOTTOM) (:CLOBBER) (:UPDATE))
(DEFMACRO <<-- (OBJ . MESSAGES)
  "Sends the object OBJ the messages MESSAGES in succession, as in
/(<<-- TVOB (:EDGES<- LEFT TOP RIGHT BOTTOM) (:CLOBBER) (:UPDATE))."
  `(PROGN . ,(MAPCAR 'APPEND (CIRCULAR-LIST `(<- ,OBJ)) MESSAGES)))

;(<-AS TVOB ':UPDATE) sends SELF the message ':UPDATE but handles it
;as if SELF were of class TVOB instead of its actual class.
;It is useful in definitions of methods of subclasses of TVOB.
;It is also useful in methods of TVOB, since SELF is not rebound.
;The specified class must be a constant.
;Caveat: Doesn't check to see if TVOB is a superclass of SELF's class.
;Other Caveat: always refers to the most recent class definition of TVOB.

(DEFMACRO <-AS (CLASS-SYMBOL . MESSAGE)
  "Sends SELF the message MESSAGE but handles it as if SELF were of
class CLASS-SYMBOL instead of its actual class.  It is useful in
definitions of methods of subclasses of CLASS-SYMBOL and in methods of
CLASS-SYMBOL, since SELF is not rebound.  CLASS-SYMBOL must be a
constant."
  `(FUNCALL (SYMEVAL-IN-CLOSURE ,CLASS-SYMBOL 'CLASS-METHOD-SYMBOL) . ,MESSAGE))

;(DEFUN <-AS (CLASS-SYMBOL &REST MESSAGE)
;   (LEXPR-FUNCALL (SYMEVAL-IN-CLOSURE CLASS-SYMBOL 'CLASS-METHOD-SYMBOL) MESSAGE))

;; GET TO HERE VIA TAIL POINTER ON OBJECT-CLASS
;; Flavors now use FLAVOR-UNCLAIMED-MESSAGE by default instead.
(DEFUN UNCLAIMED-MESSAGE (KEY &REST REST)
  "Signals KEY as an unclaimed message to SELF.  Includes the rest of
the message."
  (SIGNAL-PROCEED-CASE ((NEW-OPERATION) 'SYS:UNCLAIMED-MESSAGE
                                        :OBJECT SELF
                                        :MESSAGE KEY
                                        :ARGUMENTS (COPY-LIST REST))
    (:NEW-OPERATION (LEXPR-SEND SELF NEW-OPERATION REST))))


;CLASS of any object returns the actual class of that object, an instance of class CLASS.
;  It gets this by SYMEVALing the CLASS-METHOD-SYMBOL.
;CLASS-SYMBOL of an object returns the class-symbol.  It pulls this out of the CLASS.
;  This function should rarely be used.
;CLASS-METHOD-SYMBOL returns a symbol whose function cell holds the select-method.
;  If an entity, the CLASS-METHOD-SYMBOL is its storage-wise CAR.
; this is always different from CLASS-SYMBOL.

(DEFUN CLASS (OBJ)
  "Returns the actual class of OBJ"
   (SYMEVAL (CLASS-METHOD-SYMBOL OBJ)))

(DEFUN CLASS-METHOD-SYMBOL (OBJ)
  "If OBJ is an entity, returns its storage-wise CAR."
  (COND ((ENTITYP OBJ) (CAR (%MAKE-POINTER DTP-LIST OBJ)))
        ((FIXP OBJ) (SYMEVAL-IN-CLOSURE FIXNUM-CLASS 'CLASS-METHOD-SYMBOL))
        ((SYMBOLP OBJ) (SYMEVAL-IN-CLOSURE SYMBOL-CLASS 'CLASS-METHOD-SYMBOL))
        ((FLOATP OBJ) (SYMEVAL-IN-CLOSURE FLONUM-CLASS 'CLASS-METHOD-SYMBOL))
        ((AND (NAMED-STRUCTURE-P OBJ)
              (OR (LET ((C (IF (ARRAY-HAS-LEADER-P OBJ)
                               (ARRAY-LEADER OBJ 1)
                               (AREF OBJ 0))))
                    (AND (TYPEP C :CLOSURE) C))
                  (GET (NAMED-STRUCTURE-P OBJ) 'CLASS-METHOD-SYMBOL))))
        ((ARRAYP OBJ) (SYMEVAL-IN-CLOSURE ARRAY-CLASS 'CLASS-METHOD-SYMBOL))
        ((NOT (ATOM OBJ)) (SYMEVAL-IN-CLOSURE CONS-CLASS 'CLASS-METHOD-SYMBOL))
        (T (FERROR NIL "NO CLASS-METHOD-SYMBOL APPLIES ~S" OBJ))))

(DEFUN CLASS-SYMBOL (OBJ)
  "Returns the class-symbol of OBJ.  This should be used rarely."
  (LET ((CMS (CLASS-METHOD-SYMBOL OBJ)))
    (OR (GET CMS 'ENTITY-TYPEP)
        (SYMEVAL CMS))))

(DEFUN CLASS-NAME (OBJ)
  "Returns the class-name of OBJ."
  (PROG (CSM)
        (SETQ CSM (CLASS-METHOD-SYMBOL OBJ))
     L  (COND ((GET CSM ':PHANTOM-CLASS)
               (SETQ CSM (<- (<- (SYMEVAL CSM) ':SUPERCLASS)
                             ':CLASS-METHOD-SYMBOL))
               (GO L)))
        (RETURN (SYMEVAL-IN-CLOSURE (SYMEVAL CSM) 'NAME))))

(DEFUN IMMEDIATE-CLASS-NAME (OBJ)
  "Returns the immediate-class-name of OBJ."
  (SYMEVAL-IN-CLOSURE (SYMEVAL (CLASS-METHOD-SYMBOL OBJ)) 'NAME))

;CLASS-SYMBOLP moved to QMISC

(DEFUN SUBCLASS-OF-CLASS-SYMBOL-P (SUBCLASS CLASS-SYMBOL)
  "Returns T if SUBCLASS is a subclass of any class whose class symbol
is CLASS-SYMBOL.  Otherwise returns NIL."
  (OR (EQ (<- SUBCLASS ':CLASS-SYMBOL) CLASS-SYMBOL)
      (LET ((SC (<- SUBCLASS ':SUPERCLASS)))
        (COND ((OR (NULL SC)
                   (EQ SC SUBCLASS))
               NIL)
              ((ENTITYP SC)
               (SUBCLASS-OF-CLASS-SYMBOL-P SC CLASS-SYMBOL))
              (T (DOLIST (SC1 SC)
                   (COND ((SUBCLASS-OF-CLASS-SYMBOL-P SC1 CLASS-SYMBOL)
                          (RETURN T)))))))))

(DEFUN SUBCLASS-OF-CLASSP (SUBCLASS CLASS)
  "Returns T if SUBCLASS is a subclass of CLASS.  Otherwise returns
NIL."
  (OR (EQ SUBCLASS CLASS)
      (LET ((SC (<- SUBCLASS ':SUPERCLASS)))
        (COND ((OR (NULL SC)
                   (EQ SC SUBCLASS))
               NIL)
              ((ENTITYP SC)
               (SUBCLASS-OF-CLASSP SC CLASS))
              (T (DOLIST (SC1 SC)
                   (COND ((SUBCLASS-OF-CLASSP SC1 CLASS)
                          (RETURN T)))))))))

(DEFUN SUBINSTANCE-OF-CLASSP (ENT CLASS)
  "Returns T if ENT is a subinstance of CLASS.  Otherwise returns NIL."
  (AND (ENTITYP ENT) (SUBCLASS-OF-CLASSP (CLASS ENT) CLASS)))

(DEFUN SUBINSTANCE-OF-CLASS-SYMBOL-P (ENT CLASS-SYMBOL)
  "Returns T if ENT is a subinstance of CLASS-SYMBOL.  Otherwise returns
NIL."
  (AND (ENTITYP ENT) (SUBCLASS-OF-CLASS-SYMBOL-P (CLASS ENT) CLASS-SYMBOL)))

(DEFUN ALL-SUBCLASSES-OF-CLASS (CLASS &OPTIONAL SO-FAR)
  "Lists the subclasses of CLASS."
    (DOLIST (CL (<- CLASS ':IMMEDIATE-SUBCLASS-LIST))
      (COND ((NOT (MEMQ CL SO-FAR))
             (SETQ SO-FAR (CONS CL SO-FAR))
             (SETQ SO-FAR (ALL-SUBCLASSES-OF-CLASS CL SO-FAR)))))
    SO-FAR)

(DEFUN MAP-CLASS-HIERARCHY (FCTN &OPTIONAL (CLASS OBJECT-CLASS))
  "Lists the class hierarchy of FCTN."
  (FUNCALL FCTN CLASS)
  (DOLIST (CL (<- CLASS ':IMMEDIATE-SUBCLASS-LIST))
    (MAP-CLASS-HIERARCHY FCTN CL)))

;Macros for defining classes.

;Define a class of named structures.
;You must give the class name sans "-CLASS", since that will be used
;as the named-structure-symbol.
(DEFMACRO DEFSTRUCTCLASS (CL SUPERCLASS-SYMBOL)
  "Defines a class of named structures.  You must give the class name
sans /"-CLASS/", since that will be used as the named-structure-symbol."
    (LET ((CLASS-SYMBOL (CLASS-HOLDER CL)))
        `(PROGN 'COMPILE
            (DECLARE (SPECIAL ,CLASS-SYMBOL))
            (DEFCLASS-1 ,CLASS-SYMBOL ,SUPERCLASS-SYMBOL NIL)
            (DEFMETHOD-INSTANCE (,CLASS-SYMBOL :NEW) (&REST IGNORE)
                   (FERROR NIL "The class ~S does not handle NEW messages" ,CLASS-SYMBOL))
            (PUTPROP ',CL
                     (SYMEVAL-IN-CLOSURE ,CLASS-SYMBOL 'CLASS-METHOD-SYMBOL)
                     'CLASS-METHOD-SYMBOL))))

;Should be flushed ...
(DEFUN CLASS-HOLDER (CL)
  (COND ((ATOM CL)
         (INTERN (STRING-APPEND CL "-CLASS")))
        (T (MAPCAR (FUNCTION CLASS-HOLDER) CL))))

;Define a class of ENTITYs.  The superclass must be specified.
;Also, you must specify the names of the instance-variables
;(in addition to those which are inherited from the superclass,
;which you should not mention again).
(DEFMACRO DEFCLASS (CLASS-SYMBOL SUPERCLASS-SYMBOL INSTANCE-PATTERN
                            &OPTIONAL (ACCESSOR-METHODS T))
  "Defines a class of ENTITYs under SUPERCLASS-SYMBOL.  INSTANCE
PATTERN contains only the instance varibles not contained in the
superclass."
    `(PROGN 'COMPILE (SPECIAL ,CLASS-SYMBOL)
            (EVAL-WHEN (COMPILE)
              (PUSH ',*MACROARG* FILE-LOCAL-DECLARATIONS))
            (DEFCLASS-1 ,CLASS-SYMBOL ,SUPERCLASS-SYMBOL ,INSTANCE-PATTERN)
            . ,(COND (ACCESSOR-METHODS
                      (MAKE-ACCESSOR-METHODS CLASS-SYMBOL INSTANCE-PATTERN)))))

;Should only be called from above macro.
(DEFUN MAKE-ACCESSOR-METHODS (CLASS-SYMBOL INSTANCE-PATTERN &AUX RES)
    (DOLIST (L INSTANCE-PATTERN)
      (SETQ RES
            (NCONC RES
                   `((DEFMETHOD (,CLASS-SYMBOL ,(INTERN (STRING-APPEND L "<-")
                                                        PKG-KEYWORD-PACKAGE))
                                (A)
                                (SETQ ,L A))))))
    (DOLIST (L INSTANCE-PATTERN)
      (SETQ RES
            (NCONC RES
                   `((DEFMETHOD (,CLASS-SYMBOL ,(INTERN (STRING L) PKG-KEYWORD-PACKAGE))
                                ()
                                ,L)))))
    RES)

;Should only be called from above macros.
; Makes an instance of CLASS-CLASS
(DEFUN DEFCLASS-1 (&QUOTE CLASS-SYMBOL SUPERCLASS-SYMBOL INSTANCE-PATTERN)
 (LET ((SUPERCLASS (COND ((ATOM SUPERCLASS-SYMBOL)
                          (SYMEVAL SUPERCLASS-SYMBOL))
                         (T (MAPCAR (FUNCTION SYMEVAL)
                                    SUPERCLASS-SYMBOL)))))
  ;Dont redefine class if it is already defined with same instance variables.
  (COND ((OR (NOT (BOUNDP CLASS-SYMBOL))
             (NOT (ENTITYP (SYMEVAL CLASS-SYMBOL)))
             (NOT (EQUAL (<- (SYMEVAL CLASS-SYMBOL) ':INSTANCE-PATTERN)
                         (UNION INSTANCE-PATTERN
                                (COND ((ENTITYP SUPERCLASS)
                                       (<-  SUPERCLASS ':INSTANCE-PATTERN))
                                      (T (APPLY 'UNION
                                                (MAPCAR (FUNCTION (LAMBDA (SC)
                                                            (<- SC ':INSTANCE-PATTERN)))
                                                        SUPERCLASS))))))))
         (<- CLASS-CLASS ':NEW
             'CLASS-SYMBOL CLASS-SYMBOL                 ;NAME is set in the :BORN method now
             'INSTANCE-PATTERN INSTANCE-PATTERN
             'SUPERCLASS SUPERCLASS
             'CLASS-VERSION-NUMBER
             (COND ((CLASS-SYMBOLP CLASS-SYMBOL)
                    (1+ (<- (SYMEVAL CLASS-SYMBOL) ':CLASS-VERSION-NUMBER)))
                   (T 0)))))))

;Funny form of DEFCLASS.  Used only to bootstrap classes CLASS-CLASS and OBJECT-CLASS before
; mechanism necessary to make NEW message work is set up.  Does not set up the
; value of SUPERCLASS because that cant be done until CLASS-CLASS and OBJECT-CLASS exist.
(DEFUN DEFCLASS-BOOTSTRAP (&QUOTE NM C-S METHOD-TAIL VARIABLES)
  (IF (BOUNDP C-S)
      NIL
    (LET ((NAME NM)
          (CLASS-SYMBOL C-S)
          (CLASS-METHOD-SYMBOL (GENSYM))
          (INSTANCE-PATTERN VARIABLES)
          (SUPERCLASS NIL)
          (CLASS-VERSION-NUMBER 0)
          (IMMEDIATE-SUBCLASS-LIST NIL))
      (DECLARE (SPECIAL NAME CLASS-SYMBOL CLASS-METHOD-SYMBOL
                        INSTANCE-PATTERN SUPERCLASS
                        CLASS-VERSION-NUMBER IMMEDIATE-SUBCLASS-LIST))
      (FSET CLASS-METHOD-SYMBOL METHOD-TAIL)
      (PUTPROP CLASS-METHOD-SYMBOL CLASS-SYMBOL 'ENTITY-TYPEP)
      (SET CLASS-METHOD-SYMBOL
           (SET CLASS-SYMBOL
                (ENTITY '(NAME CLASS-SYMBOL CLASS-METHOD-SYMBOL
                               SUPERCLASS INSTANCE-PATTERN
                               CLASS-VERSION-NUMBER IMMEDIATE-SUBCLASS-LIST)
                        (COND ((EQ CLASS-SYMBOL 'CLASS-CLASS)
                               CLASS-METHOD-SYMBOL)
                              (T (SYMEVAL-IN-CLOSURE CLASS-CLASS
                                                     'CLASS-METHOD-SYMBOL))))))
      (DEFINE-ACCESSOR-METHODS CLASS-SYMBOL CLASS-METHOD-SYMBOL VARIABLES))))

;Define for a class of named structures a method which
;simply returns the value of a particular component.
(DEFMACRO TRIVIAL-ACCESS (CLASS COMPONENT)
  `(DEFMETHOD (,CLASS ,(INTERN (STRING COMPONENT) PKG-KEYWORD-PACKAGE)) ()
     (,(INTERN (STRING-APPEND CLASS "-" COMPONENT)) SELF)))

(PROGN 'COMPILE ;Dont return any ENTITIES to READ-EVAL-PRINT loop until object printer
                ; in place to handle them.
;CLASS must be first use of DEFCLASS, since that uses the value of CLASS-CLASS,
;and magically wins if it is setting that value, but loses if it is simply unbound.
;  METHOD-TAIL is to be the CLASS-METHOD-SYMBOL for OBJECT-CLASS, which doesnt
;exist yet.
(DEFCLASS-BOOTSTRAP CLASS CLASS-CLASS NIL (NAME CLASS-SYMBOL CLASS-METHOD-SYMBOL
                                                INSTANCE-PATTERN SUPERCLASS
                                                CLASS-VERSION-NUMBER IMMEDIATE-SUBCLASS-LIST))

;Now that CLASS-CLASS is bound, we can create the class OBJECT.
;It is funny, because if you ask for its superclass, you get it itself;
;but in fact the superclass in the select-method is UNCLAIMED-MESSAGE.
(DEFCLASS-BOOTSTRAP OBJECT OBJECT-CLASS UNCLAIMED-MESSAGE ())

(SET-IN-CLOSURE OBJECT-CLASS 'SUPERCLASS OBJECT-CLASS)
(SET-IN-CLOSURE CLASS-CLASS  'SUPERCLASS OBJECT-CLASS)  ;finish linking up.
(SET-METHOD-SUPERCLASS (<- CLASS-CLASS ':CLASS-METHOD-SYMBOL)
                       OBJECT-CLASS)  ;FILL IN WHERE LEFT BLANK.
(COND ((NULL (SYMEVAL-IN-CLOSURE OBJECT-CLASS 'IMMEDIATE-SUBCLASS-LIST))
       (SET-IN-CLOSURE OBJECT-CLASS 'IMMEDIATE-SUBCLASS-LIST (LIST CLASS-CLASS))))

(EVAL-WHEN (COMPILE)
  (PUSH '(DEFCLASS CLASS-CLASS
                   OBJECT-CLASS
                   (NAME CLASS-SYMBOL CLASS-METHOD-SYMBOL
                         INSTANCE-PATTERN SUPERCLASS
                         CLASS-VERSION-NUMBER IMMEDIATE-SUBCLASS-LIST))
        LOCAL-DECLARATIONS)
  (PUSH '(DEFCLASS OBJECT-CLASS OBJECT-CLASS ())
        LOCAL-DECLARATIONS))

;;; Now define the method for NEW, for creating instances of ENTITY classes,
;;; and related methods.
(DEFMETHOD (CLASS-CLASS :NEW) (&REST REST)
    (LET ((NEWGUY
           (LET ((CMS CLASS-METHOD-SYMBOL)  ;AVOID SCREW WHEN MAKING INSTANCES OF CLASS-CLASS
                 (**VN** INSTANCE-PATTERN))
                (PROGV **VN** (MAKE-LIST (LENGTH **VN**))
                       (DO ((R REST (CDDR R))
                            (V))
                           ((NULL R))
                         (COND ((SETQ V (CAR (MEM #'STRING-EQUAL (CAR R) **VN**)))
                                (SET V (CADR R)))
                               (T (FERROR NIL
                                          "The class ~S has no variable ~A" SELF (CAR R)))))
                       (ENTITY **VN** CMS)))))
         (<- NEWGUY ':BORN)
         NEWGUY))
(DEFMETHOD (OBJECT-CLASS :BORN) () NIL)

;Now define appropriate methods for creating a new class using a NEW message.
(DEFMETHOD (CLASS-CLASS :BORN) ()
  (OR CLASS-SYMBOL (FERROR NIL "CLASS-SYMBOL must be specified when creating a class"))
  (SET CLASS-SYMBOL SELF)
  (OR CLASS-METHOD-SYMBOL (SETQ CLASS-METHOD-SYMBOL (GENSYM)))
  (PUTPROP CLASS-METHOD-SYMBOL CLASS-SYMBOL 'ENTITY-TYPEP)
  (SET CLASS-METHOD-SYMBOL SELF)
  (COND ((NULL NAME)
         (SETQ NAME (MAKE-CLASS-NAME CLASS-SYMBOL))))
        ;SUPERCLASS IS AN ENTITY OR LIST OF ENTITIES.
  (SET-METHOD-SUPERCLASS CLASS-METHOD-SYMBOL SUPERCLASS)
  (SETQ INSTANCE-PATTERN
        (UNION INSTANCE-PATTERN
               (COND ((ENTITYP SUPERCLASS)
                      (<-  SUPERCLASS ':INSTANCE-PATTERN))
                     (T (APPLY 'UNION
                               (MAPCAR (FUNCTION (LAMBDA (SC)
                                                         (<- SC ':INSTANCE-PATTERN)))
                                       SUPERCLASS))))))
  (COND ((ENTITYP SUPERCLASS)
         (<- SUPERCLASS ':ADD-IMMEDIATE-SUBCLASS SELF))
        (T (MAPC (FUNCTION (LAMBDA (SC)
                             (<- SC ':ADD-IMMEDIATE-SUBCLASS SELF)))
                 SUPERCLASS)))
  SELF)

(DEFMETHOD (CLASS-CLASS :ADD-IMMEDIATE-SUBCLASS) (CLASS)
  (COND ((NULL (MEMQ CLASS IMMEDIATE-SUBCLASS-LIST))
         (SETQ IMMEDIATE-SUBCLASS-LIST (CONS CLASS IMMEDIATE-SUBCLASS-LIST)))))

(DEFMETHOD (CLASS-CLASS :CLASS-SYMBOL<-) (IGNORE)
  (FERROR NIL "Attempt to change CLASS-SYMBOL of ~S" SELF))

(DEFMETHOD (CLASS-CLASS :INSTANCE-PATTERN<-) (&REST IGNORE)
  (FERROR NIL "Attempt to change INSTANCE-PATTERN of ~S" SELF))

(DEFMETHOD (CLASS-CLASS :SUPERCLASS<-) (IGNORE)
 (FERROR NIL "Attempt to change SUPERCLASS of ~S" SELF))

;This can be used only to add a class without instance variables.
; To add one that has instance variables, you must  create a new
;subclass (which can be a phantom subclass if desired).
(DEFMETHOD (CLASS-CLASS :ADD-SUPERCLASS) (SC)
 (COND ((NOT (NULL (<- SC ':INSTANCE-PATTERN)))
        (FERROR NIL "You can't add a superclass that has instance variables ~S" SC))
       (T (SETQ SUPERCLASS (CONS SC (COND ((ENTITYP SUPERCLASS) (LIST SUPERCLASS))
                                          (T SUPERCLASS))))
          (SET-METHOD-SUPERCLASS CLASS-METHOD-SYMBOL SUPERCLASS))))

;Returns a tree whose leaves are instances of CLASS-CLASS
(DEFMETHOD (CLASS-CLASS :CLASS-CLASS-HIERARCHY) ()
  (CONS SELF (COND ((EQ CLASS-SYMBOL 'OBJECT-CLASS)
                    NIL)
                   ((ENTITYP SUPERCLASS)
                    (<- SUPERCLASS ':CLASS-CLASS-HIERARCHY))
                   (T (MAPCAR (FUNCTION (LAMBDA (X) (<- X ':CLASS-CLASS-HIERARCHY)))
                              SUPERCLASS)))))

(DEFMETHOD (OBJECT-CLASS :CLASS-HIERARCHY) ()
  (<- (CLASS SELF) ':CLASS-CLASS-HIERARCHY))

(DEFMETHOD (CLASS-CLASS :CLASS-SYMBOL-HIERARCHY) ()
  (CONS CLASS-SYMBOL (COND ((EQ CLASS-SYMBOL 'OBJECT-CLASS)
                            NIL)
                           ((ENTITYP SUPERCLASS)
                            (<- SUPERCLASS ':CLASS-SYMBOL-HIERARCHY))
                           (T (MAPCAR (FUNCTION (LAMBDA (X) (<- X ':CLASS-SYMBOL-HIERARCHY)))
                                      SUPERCLASS)))))

(DEFMETHOD (OBJECT-CLASS :SYMBOL-HIERARCHY) ()
  (<- (CLASS SELF) ':CLASS-SYMBOL-HIERARCHY))

(DEFMETHOD (OBJECT-CLASS :PRINT-SELF) (&OPTIONAL (STREAM T) &REST IGNORE &AUX TEM)
  (COND ((NOT (ENTITYP SELF))
         (PRIN1 SELF STREAM))
        (T (PRINC "#<" STREAM)
           (PRIN1 (CLASS-NAME SELF) STREAM)
           (COND ((SETQ TEM (ASS (FUNCTION STRING-EQUAL) "NAME" (CLOSURE-ALIST SELF)))
                  (TYO #/  STREAM)
                  (PRINC (CDR TEM) STREAM)
                  (AND PRINT-ENTITY-ADDRESSES-FLAG
                       (FORMAT STREAM " ~O" (%POINTER SELF)))
                  (TYO #/> STREAM))
                 (T
                  ;Unfortunately, this gets rid of self recursions but not mutual recursions
                  ;This is rather a crock anyway, comment it out.
;                 (MAPC #'(LAMBDA (E)
;                                   (FORMAT STREAM " ~S: ~S" (CAR E) ;Don't recurse infinitely!
;                                                    (IF (EQ (CDR E) SELF) 'SELF
;                                                        (CDR E))))
;                       (CLOSURE-ALIST SELF))
                  (FORMAT STREAM " ~O" (%POINTER SELF))
                  (TYO #/> STREAM)))))
  SELF)

(DEFMETHOD (OBJECT-CLASS :DESCRIBE) (&OPTIONAL (STREAM STANDARD-OUTPUT) &REST IGNORE)
  (COND ((NOT (ENTITYP SELF))
         (LET ((STANDARD-OUTPUT STREAM))
           (DESCRIBE SELF)))
        (T (FORMAT STREAM "~%~S is an instance of ~S.~%Its components are:~%"
                   SELF (CLASS SELF))
           (MAPC (FUNCTION (LAMBDA (E) (FORMAT STREAM "~S: ~S~%" (CAR E) (CDR E))))
                 (CLOSURE-ALIST SELF))
           (TERPRI STREAM)))
  SELF)

)  ;This closes the PROGN far above.  OK to generate ENTITIES since PRINT of them should
   ; work now

;; Ask a class which operations its instances handle.
(DEFMETHOD (CLASS-CLASS :CLASS-OPERATIONS) (&OPTIONAL (SUPERIORS-FLAG T))
    (DO ((ACCUM) (L (METHOD-LIST CLASS-METHOD-SYMBOL) (CDR L)))
        ((ATOM L)
         (AND L SUPERIORS-FLAG (BOUNDP L)
              (NEQ (SYMEVAL L) SELF)
              (SETQ ACCUM (UNION ACCUM (<- (SYMEVAL L) ':CLASS-OPERATIONS))))
         ACCUM)
        (COND ((CONSP (CAR L))
               (SETQ ACCUM (UNION ACCUM
                                  (COND ((ATOM (CAAR L)) (LIST (CAAR L))) (T (CAAR L))))))
              (SUPERIORS-FLAG
               (SETQ ACCUM
                     (UNION ACCUM (<- (SYMEVAL (CAR L)) ':CLASS-OPERATIONS NIL)))))))

;; Ask an object which operations it handles.
;; This definition is sufficient except for objects with ideosyncratic handlers,
;; which don't exist yet.
(DEFMETHOD (OBJECT-CLASS :WHICH-OPERATIONS) (&OPTIONAL (SUPERIORS-FLAG T))
    (<- (CLASS SELF) ':CLASS-OPERATIONS SUPERIORS-FLAG))

(defmethod (object-class :operation-handled-p) (operation)
  (memq operation (<- self :which-operations)))

;; This message sent to a class returns the method used by that class
;; to handle the specified operation.  If there is none, NIL is returned.
;; If SUPERIORS-FLAG is NIL, then inherited methods are not searched.
;; This can be used to tell whether a class handles a certain operation at all,
;; or to get the handler once and call it many times, for efficiency.
;**can't use OPERATION as name of lambda variable, because that is used
;  to receive the message key!! crock**
(DEFMETHOD (CLASS-CLASS :METHOD-FOR) (OP &OPTIONAL (SUPERIORS-FLAG T))
  (DO ((L (METHOD-LIST CLASS-METHOD-SYMBOL) (CDR L)) (TEM))
      ((ATOM L)
       (AND L SUPERIORS-FLAG (BOUNDP L)
            (NEQ (SYMEVAL L) SELF)
            (<- (SYMEVAL L) ':METHOD-FOR OP)))
    (COND ((CONSP (CAR L))
           (AND (COND ((CONSP (CAAR L)) (MEMQ OP (CAAR L)))
                      (T (EQ OP (CAAR L))))
                (RETURN (CDAR L))))
          (SUPERIORS-FLAG
           (AND (SETQ TEM (<- (SYMEVAL (CAR L)) ':METHOD-FOR OP NIL))
                (RETURN TEM))))))

;; Ask an object how it will handle a given operation.
(DEFMETHOD (OBJECT-CLASS :HANDLER-FOR) (OP)  ;**likewise, dont call this OPERATION**
  (<- (CLASS SELF) ':METHOD-FOR OP))

;;; No documentation for default
(DEFMETHOD (OBJECT-CLASS :DOCUMENTATION) () NIL)

(DEFCLASS NUMBER-CLASS OBJECT-CLASS ())
(DEFMETHOD-INSTANCE (NUMBER-CLASS :NEW) (&REST ARGS)
  (COND ((GETF ARGS ':VALUE))
        (T 0)))

(DEFCLASS SYMBOL-CLASS OBJECT-CLASS ())
;I hope no one uses this method, since it doesn't work
(DEFMETHOD-INSTANCE (SYMBOL-CLASS :NEW) (&REST ARGS)
  (APPLY 'MAKE-SYMBOL (GETF ARGS ':PNAME) ARGS))

(DEFCLASS FIXNUM-CLASS NUMBER-CLASS ())
  ;GETS :NEW MESSAGE FROM NUMBER-CLASS

(DEFCLASS FLONUM-CLASS NUMBER-CLASS ())
(DEFMETHOD-INSTANCE (FLONUM-CLASS :NEW) (&REST ARGS)
  (+ 0.0 (COND ((GETF ARGS ':VALUE))
               (T 0.0))))

(DEFCLASS ARRAY-CLASS OBJECT-CLASS ())
(DEFMETHOD-INSTANCE (ARRAY-CLASS :NEW) (&REST ARGS)
  (APPLY 'MAKE-ARRAY ARGS))

(DEFCLASS CONS-CLASS OBJECT-CLASS ())
(DEFMETHOD-INSTANCE (CONS-CLASS :NEW) (&REST ARGS)
  (CONS (GETF ARGS ':CAR)
        (GETF ARGS ':CDR)))
