;;; The system system. -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980, 1981, 1982 Massachusetts Institute of Technology **

;; We need this during system building, before window system is loaded.
(PROCLAIM '(SPECIAL TV:MORE-PROCESSING-GLOBAL-ENABLE))

(defprop compiler:lambda-interface (:qfasl nil compiler:compile-file) si:make-system)
(defprop compiler:k
         (:fbin #+(target lambda) :fdef         ;When this code runs on the Lambda, in cross-compile mode.
                #-(target lambda) nil           ;When this code runs on the Falcon, in native mode.
                compiler:compile-file-for-falcon)
  si:make-system)

;;; Here for some bizarre reason
(DEFMACRO PUSH* (ITEM LIST)
  `(OR (MEMQ ,ITEM ,LIST) (PUSH ,ITEM ,LIST)))

;;; Define special variables bound during DEFSYSTEM and MAKE-SYSTEM
(DEFMACRO DEFINE-SPECIAL-VARIABLE (NAME INITIAL-VALUE WHERE &OPTIONAL (DEFVAR-P T))
  `(PROGN 'COMPILE
          ,(AND DEFVAR-P `(DEFVAR ,NAME))
          (DEFINE-SPECIAL-VARIABLE-1 ',NAME ',INITIAL-VALUE ',WHERE)))

(DEFUN DEFINE-SPECIAL-VARIABLE-1 (NAME INITIAL-VALUE WHERE &AUX ELEM)
  (IF (SETQ ELEM (ASSQ NAME (SYMEVAL WHERE)))
      (SETF (CDR ELEM) (NCONS INITIAL-VALUE))
      (PUSH (LIST NAME INITIAL-VALUE) (SYMEVAL WHERE))))

;;; Systems
(DEFSTRUCT (SYSTEM :ARRAY :NAMED :CONC-NAME
                   (:CONSTRUCTOR CONSTRUCT-SYSTEM-INTERNAL) (:ALTERANT NIL))
  (NAME nil :documentation "Name of this system, a symbol or string")
  (COMPONENT-SYSTEMS nil :documentation "List of system names")
  MODULES
  TOP-LEVEL-TRANSFORMATIONS
  TRANSFORMATIONS
  PLIST)

(DEFSELECT ((SYSTEM NAMED-STRUCTURE-INVOKE))
  (:PRINT-SELF (SYSTEM STREAM PRINDEPTH SLASHIFY-P)
    PRINDEPTH SLASHIFY-P                        ;Not used
    (SI:PRINTING-RANDOM-OBJECT (SYSTEM STREAM :TYPEP)
      (PRINC (SYSTEM-NAME SYSTEM) STREAM))))

;;; Slots not actually in the defstruct
(DEFMACRO SYSTEM-PACKAGE-DEFAULT (SYSTEM)
  `(GETF (SYSTEM-PLIST ,SYSTEM) :PACKAGE))

(DEFSTRUCT (MODULE :NAMED-ARRAY :CONC-NAME (:ALTERANT NIL))
  (NAME nil :documentation "A symbol")
  (SYSTEM nil :documentation "A SYSTEM object")
  (COMPONENTS nil :documentation
     "A list of MODULE's, PATHNAME lists,or (SYSTEM-NAME . MODULE names)")
  PLIST)

(DEFSELECT ((:PROPERTY MODULE NAMED-STRUCTURE-INVOKE))
  (:PRINT-SELF (MODULE STREAM IGNORE IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (MODULE STREAM)
      (FORMAT STREAM "~A (~A ~A)"
              (NAMED-STRUCTURE-P MODULE)
              (SYSTEM-NAME (MODULE-SYSTEM MODULE))
              (MODULE-NAME MODULE)))))

(DEFSTRUCT (TRANSFORMATION :ARRAY :NAMED :CONC-NAME (:ALTERANT NIL))
  TRANSFORMATION-TYPE                           ;A TRANSFORMATION-TYPE
  (INPUT nil :documentation "A MODULE or a TRANSFORMATION")
  DEPENDENCIES                                  ;A list of TRANSFORMATION's
  (CONDITION-FUNCTION nil :documentation "A symbol")
  (SYSTEM nil :documentation "The system this module belongs to.")
  )

(DEFSELECT ((:PROPERTY TRANSFORMATION NAMED-STRUCTURE-INVOKE))
  (:PRINT-SELF (TRANSFORMATION STREAM IGNORE IGNORE &AUX TEMP)
    (SI:PRINTING-RANDOM-OBJECT (TRANSFORMATION STREAM)
      (FORMAT STREAM "~A~@[ ~A~]"
              (NAMED-STRUCTURE-P TRANSFORMATION)
              (AND (SETQ TEMP (TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION))
                   (TRANSFORMATION-TYPE-NAME TEMP))))))

(DEFSTRUCT (TRANSFORMATION-TYPE :LIST :CONC-NAME (:ALTERANT NIL))
  (NAME nil :documentation "A keyword")
  (PRETTY-NAMES nil :documentation "(/"Foo/" /"Fooing/" /"fooed/")")
  (FUNCTION nil :documentation "A symbol")
  (INPUT-FILE-TYPES nil :documentation "A list of string")
  OUTPUT-FILE-TYPES)

(DEFSTRUCT (TRANSFORMATION-TYPE-PRETTY-NAMES :LIST :CONC-NAME (:ALTERANT NIL)
                                                              (:CONSTRUCTOR NIL))
  IMPERATIVE                                    ;Foo
  PRESENT-PARTICIPLE                            ;Fooing
  PAST-PARTICIPLE)                              ;fooed

(DEFMACRO TRANSFORMATION-TYPE-PRETTY-IMPERATIVE (TRANSFORMATION-TYPE)
  `(TRANSFORMATION-TYPE-PRETTY-NAMES-IMPERATIVE
     (TRANSFORMATION-TYPE-PRETTY-NAMES ,TRANSFORMATION-TYPE)))

(DEFMACRO TRANSFORMATION-TYPE-PRETTY-PRESENT-PARTICIPLE (TRANSFORMATION-TYPE)
  `(TRANSFORMATION-TYPE-PRETTY-NAMES-PRESENT-PARTICIPLE
     (TRANSFORMATION-TYPE-PRETTY-NAMES ,TRANSFORMATION-TYPE)))

(DEFMACRO TRANSFORMATION-TYPE-PRETTY-PAST-PARTICIPLE (TRANSFORMATION-TYPE)
  `(TRANSFORMATION-TYPE-PRETTY-NAMES-PAST-PARTICIPLE
     (TRANSFORMATION-TYPE-PRETTY-NAMES ,TRANSFORMATION-TYPE)))

(DEFSTRUCT (FILE-TRANSFORMATION :LIST* :CONC-NAME (:ALTERANT NIL))
  (STATE nil :documentation "One of NIL, :PENDING, :DONE, :NOT-NEEDED or :REFUSED")
  TRANSFORMATION-TYPE                           ;A TRANSFORMATION-TYPE
  (FORCE-PACKAGE nil :documentation "A symbol; transformation takes place there")
  SYSTEM                                        ;The one to perform within
  (CONDITION-FUNCTION nil :documentation "A symbol or closure")
  (OUTPUTS nil :documentation "An NTHCDR of FILE-TRANSFORMATION-ARGS")
  ARGS)         ;a list of all input files followed by all output files.

(DEFPROP DEFSYSTEM "System" DEFINITION-TYPE-NAME)

(DEFMACRO DEFSYSTEM (NAME &BODY OPTIONS)
  "Define a system, a bunch of files and how to compile or load them."
  (LET ((NAMESYMBOL (INTERN (STRING NAME) PKG-KEYWORD-PACKAGE)))
    `(DEFSYSTEM-1 ',NAMESYMBOL ',(COPYLIST OPTIONS))))

;;; Variables that DEFSYSTEM-MACRO's can look at
(DEFVAR *DEFSYSTEM-SPECIAL-VARIABLES* NIL)

(DEFMACRO DEFINE-DEFSYSTEM-SPECIAL-VARIABLE (NAME FORM)
  `(DEFINE-SPECIAL-VARIABLE ,NAME ,FORM *DEFSYSTEM-SPECIAL-VARIABLES*))

;;; Save a little on evaluating macros
(DEFUN CONSTRUCT-SYSTEM ()
  (CONSTRUCT-SYSTEM-INTERNAL))

(DEFINE-DEFSYSTEM-SPECIAL-VARIABLE *SYSTEM-BEING-DEFINED* (CONSTRUCT-SYSTEM))

;;; record the system name in the :SYSTEMS property of the generic pathname of
;;; each file in the system.
(DEFUN RECORD-SYSTEM-NAME-IN-PATHNAMES (NAME SYSTEM)
  "Record a system's name in all its files' pathnames.
NAME should be the system name and SYSTEM should be the system object."
  (LET ((DBFT (GETF (SYSTEM-PLIST SYSTEM) 'DEFAULT-BINARY-FILE-TYPE)))
    (MAPC #'(LAMBDA (PATHNAME)
              (LET* ((GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
                     (SYSTEMS (SEND GENERIC-PATHNAME :GET :SYSTEMS)))
                ;;if it already has a :SYSTEMS property, push onto it.
                (OR (MEMQ NAME SYSTEMS)
                    (SEND GENERIC-PATHNAME :PUTPROP (CONS NAME SYSTEMS) :SYSTEMS))
                (IF DBFT
                  (SEND GENERIC-PATHNAME :PUTPROP DBFT :DEFAULT-BINARY-FILE-TYPE)
                  (SEND GENERIC-PATHNAME :REMPROP :DEFAULT-BINARY-FILE-TYPE))))
          (SYSTEM-SOURCE-FILES SYSTEM :ALL NIL NIL))))

(DEFUN DEFSYSTEM-1 (NAME OPTIONS &OPTIONAL (ADD-P T) DONT-RECORD-IN-PATHNAMES)
  (AND (RECORD-SOURCE-FILE-NAME NAME 'DEFSYSTEM)
       (PROGW *DEFSYSTEM-SPECIAL-VARIABLES*
              (SETF (SYSTEM-SYMBOLIC-NAME *SYSTEM-BEING-DEFINED*) NAME)
              (SETF (SYSTEM-NAME *SYSTEM-BEING-DEFINED*) (STRING NAME))
              (DOLIST (OPTION OPTIONS)
                (CALL-DEFSYSTEM-MACRO OPTION))
              ;; Put in the components if they weren't mentioned explicitly
              (AND (SYSTEM-COMPONENT-SYSTEMS *SYSTEM-BEING-DEFINED*)
                   (NOT *COMPONENTS-ALREADY-DONE*)
                   (CALL-DEFSYSTEM-MACRO '(DO-COMPONENTS-INTERNAL NIL)))
              ;; Put any patching transformations at the end
              (AND (SYSTEM-PATCHABLE-P *SYSTEM-BEING-DEFINED*)
                   (CALL-DEFSYSTEM-MACRO '(PATCHABLE-INTERNAL)))
              (AND ADD-P (ADD-SYSTEM *SYSTEM-BEING-DEFINED*))
              (UNLESS DONT-RECORD-IN-PATHNAMES
                (RECORD-SYSTEM-NAME-IN-PATHNAMES NAME *SYSTEM-BEING-DEFINED*))))
  NAME)

;;; Don't record a DEFSYSTEM-1 as a random form, since it records a DEFSYSTEM as a definition.
(DEFPROP DEFSYSTEM-1 T QFASL-DONT-RECORD)

(DEFUN CALL-DEFSYSTEM-MACRO (FORM)
  (DO ((MACRO-FUNCTION)
       (VAL1) (VAL2))                           ;Kludge for multiple values
      ((NULL FORM) (VALUES VAL1 VAL2))
    (SETQ MACRO-FUNCTION (GET (CAR FORM) 'DEFSYSTEM-MACRO))
    (OR (EQ (CAR MACRO-FUNCTION) 'MACRO)
        (FERROR NIL "~S is not a valid DEFSYSTEM form" FORM))
    (MULTIPLE-VALUE (FORM VAL1 VAL2)
      (FUNCALL (CDR MACRO-FUNCTION) FORM))))

;;; All the systems in this world.  Items in this list can be either a SYSTEM
;;; structure, or a string, or even a symbol.  If a string or a symbol, its the
;;; name of the system, and its DEFSYSTEM hasn't even been loaded yet.
(DEFVAR *SYSTEMS-LIST* NIL "A list of all the systems in this world.
Each entry can be either a system structure, a string, or a symbol.
A string or symbol indicates that the system hasn't yet been loaded.")

(DEFUN ADD-SYSTEM (SYSTEM)
  (SETQ *SYSTEMS-LIST*
        (CONS SYSTEM
              (DEL #'(LAMBDA (X Y)
                       ;;this should NOT be STRING-EQUAL, which would break on
                       ;;systems with names that are symbols.
                       (EQUALP (IF (TYPEP X 'SYSTEM) (SYSTEM-NAME X) X)
                               (IF (TYPEP Y 'SYSTEM) (SYSTEM-NAME Y) Y)))
                   SYSTEM *SYSTEMS-LIST*))))

(DEFMACRO SYSTEM-SHORT-NAME-INTERNAL (SYSTEM)
  `(GETF (SYSTEM-PLIST ,SYSTEM) :SHORT-NAME))

(DEFMACRO SYSTEM-NICKNAMES (SYSTEM)
  `(GETF (SYSTEM-PLIST ,SYSTEM) :NICKNAMES))

(DEFMACRO SYSTEM-SYMBOLIC-NAME (SYSTEM)
  `(GETF (SYSTEM-PLIST ,SYSTEM) :SYMBOLIC-NAME))

(DEFMACRO SYSTEM-WARNINGS-PATHNAME-DEFAULT (SYSTEM)
  `(GETF (SYSTEM-PLIST ,SYSTEM) :WARNINGS-PATHNAME-DEFAULT))

(defmacro system-maintaining-sites (system)
  `(getf (system-plist ,system) :maintaining-sites))

;;; Some simple defsystem macros
(DEFMACRO (:NAME DEFSYSTEM-MACRO) (NAME)
  ;; If not just changing the case, add the old name as a nickname
  (AND (NOT (STRING-EQUAL NAME (SYSTEM-NAME *SYSTEM-BEING-DEFINED*)))
       (PUSH (SYSTEM-NAME *SYSTEM-BEING-DEFINED*)
             (SYSTEM-NICKNAMES *SYSTEM-BEING-DEFINED*)))
  (SETF (SYSTEM-NAME *SYSTEM-BEING-DEFINED*) NAME)
  NIL)

(DEFMACRO (:SHORT-NAME DEFSYSTEM-MACRO) (NAME)
  (SETF (SYSTEM-SHORT-NAME-INTERNAL *SYSTEM-BEING-DEFINED*) NAME)
  (PUSH NAME (SYSTEM-NICKNAMES *SYSTEM-BEING-DEFINED*))
  NIL)

(DEFMACRO (:NICKNAMES DEFSYSTEM-MACRO) (&REST NAMES)
  (SETF (SYSTEM-NICKNAMES *SYSTEM-BEING-DEFINED*) NAMES)
  NIL)

(defmacro (:maintaining-sites defsystem-macro) (&rest site-names)
  (setf (system-maintaining-sites *system-being-defined*) site-names)
  nil)

(DEFUN SYSTEM-SHORT-NAME (SYSTEM)
  (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM))
  (OR (SYSTEM-SHORT-NAME-INTERNAL SYSTEM)
      (SYSTEM-NAME SYSTEM)))

(DEFMACRO (:PACKAGE DEFSYSTEM-MACRO) (PKG)
  (SETF (SYSTEM-PACKAGE-DEFAULT *SYSTEM-BEING-DEFINED*) PKG)
  NIL)

;used by LAMLP
(DEFMACRO (:USE-FAST-READER DEFSYSTEM-MACRO) (T-OR-NIL)
  (SETF (GETF (SI:SYSTEM-PLIST SI:*SYSTEM-BEING-DEFINED*) ':FAST-READ-SWITCH) T-OR-NIL)
  NIL)

(DEFINE-DEFSYSTEM-SPECIAL-VARIABLE *SYSTEM-PATHNAME-DEFAULT* (FS:MAKE-PATHNAME-DEFAULTS))
(DEFINE-DEFSYSTEM-SPECIAL-VARIABLE *SYSTEM-PATHNAME-DEFAULT-SPECIFIED* NIL)

;;; Pathnames
(DEFMACRO (:PATHNAME-DEFAULT DEFSYSTEM-MACRO) (DEFAULT)
  (SETQ *SYSTEM-PATHNAME-DEFAULT-SPECIFIED* T)
  (FS:MERGE-AND-SET-PATHNAME-DEFAULTS DEFAULT *SYSTEM-PATHNAME-DEFAULT*)
  NIL)

;;; Pathnames
(DEFMACRO (:WARNINGS-PATHNAME-DEFAULT DEFSYSTEM-MACRO) (DEFAULT)
  (SETF (SYSTEM-WARNINGS-PATHNAME-DEFAULT *SYSTEM-BEING-DEFINED*)
        (FS:MERGE-PATHNAME-DEFAULTS DEFAULT *SYSTEM-PATHNAME-DEFAULT*))
  NIL)


(defvar *known-binary-file-producers* nil)
  ;list (<binary-file-keyword> <qc-file-1 function>)


(defsubst system-default-binary-file-type (system)
  (getf (system-plist system) 'default-binary-file-type (first (get compiler:*target-computer* 'si:make-system))))

(defun make-system-binary-pathname (system pathname)
  (send pathname :new-type (system-default-binary-file-type system)))

(DEFMACRO (:DEFAULT-BINARY-FILE-TYPE DEFSYSTEM-MACRO) (TYPE)
  (case type
    ((:qfasl)
     (setq type (first (get compiler:*target-computer* 'si:make-system)))))
  (SETF (system-default-binary-file-type *SYSTEM-BEING-DEFINED*) TYPE)
  (SETQ *SYSTEM-DEFAULT-BINARY-FILE-TYPE* TYPE)
  NIL)

(DEFUN PATHNAME-DEFAULT-BINARY-FILE-TYPE (PATHNAME)
  "Given a pathname, return the default binary file type (possibly canonical) to use with it.
This is computed from the SYSTEM which the pathname belongs to."
  (OR (SEND (SEND PATHNAME :GENERIC-PATHNAME) :GET :DEFAULT-BINARY-FILE-TYPE)
      (first (get compiler:*target-computer* 'si:make-system))
      (error "Unknown target computer ~S." compiler:*target-computer*)))


;used by LAMLP
(DEFMACRO (:OUTPUT-PATHNAME DEFSYSTEM-MACRO) (PATHNAME)
  (SETF (GETF (SI:SYSTEM-PLIST SI:*SYSTEM-BEING-DEFINED*) ':OUTPUT-PATHSTRING) PATHNAME)
  NIL)

(DEFUN PATHNAME-P (X)
  (OR (STRINGP X) (TYPEP X 'PATHNAME)))

(DEFUN CANONICALIZE-PATHNAME
       (PATHNAME &OPTIONAL (DEFAULT *SYSTEM-PATHNAME-DEFAULT*))
  (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* NIL))
    (FS:MERGE-PATHNAME-DEFAULTS PATHNAME DEFAULT NIL)))

(DEFUN MERGE-PATHNAME-TYPE (PATHNAME TYPE &AUX OTYPE)
  (FS:MERGE-PATHNAME-DEFAULTS
    (COND ((MEMQ (SETQ OTYPE (SEND PATHNAME :CANONICAL-TYPE)) '(NIL :UNSPECIFIC))
           (SEND PATHNAME :NEW-PATHNAME :TYPE TYPE :VERSION :NEWEST))
          ((OR (STRING-EQUAL OTYPE TYPE)
               (EQ TYPE :WILD)
               (EQ OTYPE :WILD))
           PATHNAME)
          (T
           (FERROR NIL "Pathname types don't match, ~A is required, ~A is specified."
                   TYPE PATHNAME)))))

;;; Component systems
(DEFMACRO (:COMPONENT-SYSTEMS DEFSYSTEM-MACRO) (&REST COMPONENTS)
  (SETF (SYSTEM-COMPONENT-SYSTEMS *SYSTEM-BEING-DEFINED*) (COPY-LIST COMPONENTS))
  NIL)

(DEFINE-DEFSYSTEM-SPECIAL-VARIABLE *COMPONENTS-ALREADY-DONE* NIL)

(DEFMACRO (:DO-COMPONENTS DEFSYSTEM-MACRO) (DEPENDENCIES)
  (SETQ *COMPONENTS-ALREADY-DONE* T)
  `(DO-COMPONENTS-INTERNAL NIL ,DEPENDENCIES))

;;; Add a new module
(DEFMACRO (:MODULE DEFSYSTEM-MACRO) (NAME COMPONENTS &REST PLIST)
  (ADD-MODULE NAME *SYSTEM-BEING-DEFINED* COMPONENTS (COPYLIST PLIST))
  NIL)

(DEFUN ADD-MODULE (NAME SYSTEM COMPONENTS &OPTIONAL PLIST &AUX MODULE)
  ;;Check for one already there
  (AND (FIND-MODULE-NAMED NAME SYSTEM T)
       (FERROR NIL "Duplicate module name ~A in system ~S" NAME SYSTEM))
  (SETQ MODULE (MAKE-MODULE NAME NAME SYSTEM SYSTEM PLIST PLIST COMPONENTS
                            (PARSE-MODULE-COMPONENTS COMPONENTS SYSTEM)))
  (PUSH MODULE (SYSTEM-MODULES SYSTEM))
  MODULE)

(DEFUN FIND-MODULE-NAMED (NAME SYSTEM &OPTIONAL NO-ERROR-P)
  (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM))
  (OR (DOLIST (MODULE (SYSTEM-MODULES SYSTEM))
        (AND (STRING-EQUAL (MODULE-NAME MODULE) NAME)
             (RETURN MODULE)))
      (IF NO-ERROR-P NIL
          (FERROR NIL "Module ~S not found in ~S" NAME SYSTEM))))

(DEFUN FIND-SYSTEM-NAMED (NAME &OPTIONAL NO-ERROR-P LOADED-ONLY)
  "Return the system object whose name is NAME.
NO-ERROR-P says return NIL if no such system, rather than getting error.
LOADED-ONLY says ignore systems whose DEFSYSTEMs have not been executed."
  ;; LOADED-ONLY = SI:FOO used internally to mean
  ;; do reload system source file but don't check SYS: SITE;.
  (IF (TYPEP NAME 'SYSTEM) NAME
      (OR (DOLIST (SYSTEM *SYSTEMS-LIST*)
            (COND ((TYPEP SYSTEM 'SYSTEM)
                   (AND (OR (STRING-EQUAL NAME (SYSTEM-NAME SYSTEM))
                            (MEM #'STRING-EQUAL NAME (SYSTEM-NICKNAMES SYSTEM)))
                        (RETURN SYSTEM)))
                  ((AND (MEMQ LOADED-ONLY '(NIL FOO))
                        (STRING-EQUAL NAME SYSTEM))
                   (MAYBE-RELOAD-SYSTEM-DECLARATION SYSTEM '(:NOCONFIRM))
                   (LET ((RETRY (FIND-SYSTEM-NAMED NAME T T)))
                     (IF RETRY (RETURN RETRY)
                         (FERROR NIL "~A did not contain a definition of ~A."
                                 (SEND (GET-SOURCE-FILE-NAME SYSTEM 'DEFSYSTEM) :SOURCE-PATHNAME)
                                 SYSTEM))))))
          (AND (NOT LOADED-ONLY)
               (LET ((PATHNAME (FS:PARSE-PATHNAME (STRING-APPEND "SYS: SITE; " NAME " SYSTEM"))))
                 (IF (LOAD PATHNAME :IF-DOES-NOT-EXIST NIL :SET-DEFAULT-PATHNAME NIL
                             :VERBOSE T)
                     (FIND-SYSTEM-NAMED NAME NO-ERROR-P 'FOO)
                   (IF NO-ERROR-P NIL (FERROR NIL "System ~S not found" NAME)))))
          (IF NO-ERROR-P NIL
            (FERROR NIL "System ~S not found" NAME)))))

;;; MODULE-SPECIFICATION := PATHNAME |
;;;                         MODULE-NAME |
;;;                         MODULE-EXTERNAL-COMPONENT |
;;;                         (MODULE-COMPONENT-1 ... MODULE-COMPONENT-N)
;;; PATHNAME := "..."                   ;String merged into a pathname with the defaults
;;; MODULE-NAME := a symbol
;;; MODULE-EXTERNAL-COMPONENT := (SYSTEM-NAME &REST MODULE-NAMES)
;;; MODULE-COMPONENT := MODULE-NAME |
;;;                     MODULE-EXTERNAL-COMPONENT |
;;;                     MODULE-SINGLE-FILE
;;; MODULE-SINGLE-FILE := PATHNAME |
;;;                       (PATHNAME-1 ... PATHNAME-N)   ;When source differs from output
;;; The idea is that you have to have two levels of list structure in the
;;; case where you have a source in a different place than the output.
(DEFUN PARSE-MODULE-COMPONENTS (COMPONENTS SYSTEM)
  (COND ((PATHNAME-P COMPONENTS)
         ;;Single pathname
         (LIST (LIST (CANONICALIZE-PATHNAME COMPONENTS))))
        ((SYMBOLP COMPONENTS)
         (LIST (FIND-MODULE-NAMED COMPONENTS SYSTEM)))  ;Single other module
        ((NLISTP COMPONENTS)
         (FERROR NIL "~S is not a recognized module component specification"
                 COMPONENTS))
        ((AND (SYMBOLP (CAR COMPONENTS))
              (NOT (FIND-MODULE-NAMED (CAR COMPONENTS) SYSTEM T)))
         (DOLIST (NAME (CDR COMPONENTS))                ;External modules
           (OR (SYMBOLP NAME)
               (FERROR NIL
                       "~S is not a recognized external module component specification in ~S"
                       NAME COMPONENTS)))
         (LIST COMPONENTS))
        (T
         (LOOP FOR COMPONENT IN COMPONENTS
               WITH TEM
 ;             WITH DEFAULTS = *SYSTEM-PATHNAME-DEFAULT*
               COLLECT (COND ((PATHNAME-P COMPONENT)
 ;                            (SETQ DEFAULTS
 ;                                  (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* NIL))
 ;                                    (FS:MERGE-PATHNAME-DEFAULTS
 ;                                      COMPONENT DEFAULTS NIL)))
 ;                            (LIST DEFAULTS))
                              (LIST (CANONICALIZE-PATHNAME COMPONENT)))
                             ((SYMBOLP COMPONENT)
                              (FIND-MODULE-NAMED COMPONENT SYSTEM))
                             ((NLISTP COMPONENT)
                              (FERROR NIL
                                      "~S is not a recognized module component specification"
                                      COMPONENT))
                             ((SYMBOLP (SETQ TEM (CAR COMPONENT)))
                              (DOLIST (NAME (CDR COMPONENT))
                                (OR (SYMBOLP NAME)
                                    (FERROR NIL
                        "~S is not a recognized external module component specification in ~S"
                                            NAME COMPONENT)))
                              COMPONENT)
                             ((PATHNAME-P TEM)
                              (LOOP FOR PATHNAME IN COMPONENT
                                    AND DEFAULT = *SYSTEM-PATHNAME-DEFAULT* THEN PATHNAME
                                    COLLECT (SETQ PATHNAME (CANONICALIZE-PATHNAME PATHNAME
                                                                                  DEFAULT))))
                             (T
                              (FERROR NIL
                                      "~S is not a recognized module component specification"
                                      COMPONENT)))))))

(DEFVAR *MAKE-SYSTEM-SPECIAL-VARIABLES* NIL)

(DEFMACRO DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE (NAME FORM &OPTIONAL (DEFVAR-P T))
  `(DEFINE-SPECIAL-VARIABLE ,NAME ,FORM *MAKE-SYSTEM-SPECIAL-VARIABLES* ,DEFVAR-P))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *QUERY-TYPE* :NORMAL)

(DEFUN (:NOCONFIRM MAKE-SYSTEM-KEYWORD) ()
  (SETQ *QUERY-TYPE* :NOCONFIRM))

(DEFUN (:SELECTIVE MAKE-SYSTEM-KEYWORD) ()
  (SETQ *QUERY-TYPE* :SELECTIVE))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *SILENT-P* NIL)

(DEFUN (:SILENT MAKE-SYSTEM-KEYWORD) ()
  (SETQ *SILENT-P* T))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *REDO-ALL* NIL)

(DEFUN (:RELOAD MAKE-SYSTEM-KEYWORD) ()
  (SETQ *REDO-ALL* T))

(DEFVAR *LOAD-TYPE-TRANSFORMATIONS* NIL)
(DEFVAR *COMPILE-TYPE-TRANSFORMATIONS* NIL)
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *TOP-LEVEL-TRANSFORMATIONS* NIL)

(DEFUN (:NOLOAD MAKE-SYSTEM-KEYWORD) ()
  (SETQ *TOP-LEVEL-TRANSFORMATIONS*
        (DEL-IF #'(LAMBDA (X)
                    (MEMQ X *LOAD-TYPE-TRANSFORMATIONS*))
                *TOP-LEVEL-TRANSFORMATIONS*)))

(DEFUN (:COMPILE MAKE-SYSTEM-KEYWORD) ()
  (SETQ *TOP-LEVEL-TRANSFORMATIONS*
        (APPEND *COMPILE-TYPE-TRANSFORMATIONS*
                (DEL-IF #'(LAMBDA (X)
                            (MEMQ X *COMPILE-TYPE-TRANSFORMATIONS*))
                        *TOP-LEVEL-TRANSFORMATIONS*)
                *TOP-LEVEL-TRANSFORMATIONS*)))

;;; For typing convenience; :FOR-FALCON is the official one.
(defun (:for-k make-system-keyword) ()
  (make-system-for-falcon))

(defun make-system-for-falcon ()
  (setq compiler:*target-computer* 'compiler:k)
  (setq *system-default-binary-file-type* :fbin)
  (setq *system-load-binary-file-type* :fdef)
  (setq compiler:*compilation-environment*
        (compiler:make-compilation-environment :target 'compiler:falcon
                                               :next compiler:*falcon-environment*)))

(defun (:for-falcon make-system-keyword) ()
  (make-system-for-falcon))

(DEFUN (:RECOMPILE MAKE-SYSTEM-KEYWORD) ()
  (FUNCALL (GET :RELOAD 'MAKE-SYSTEM-KEYWORD))
  (FUNCALL (GET :COMPILE 'MAKE-SYSTEM-KEYWORD)))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *NO-INCREMENT-PATCH* NIL)

(DEFUN (:NO-INCREMENT-PATCH MAKE-SYSTEM-KEYWORD) ()
  (SETQ *NO-INCREMENT-PATCH* T))

(DEFUN (:INCREMENT-PATCH MAKE-SYSTEM-KEYWORD) ()
  (SETQ *TOP-LEVEL-TRANSFORMATIONS*
        (APPEND '(INCREMENT-COMPILED-VERSION)
                *TOP-LEVEL-TRANSFORMATIONS*)))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *LOAD-PATCHES* T)
(DEFUN (:NO-LOAD-PATCHES MAKE-SYSTEM-KEYWORD) ()
  (SETQ *LOAD-PATCHES* NIL))

(DEFUN (:DO-NOT-DO-COMPONENTS MAKE-SYSTEM-KEYWORD) ()
  (SETQ *TOP-LEVEL-TRANSFORMATIONS*
        (DELQ 'DO-COMPONENTS-INTERNAL *TOP-LEVEL-TRANSFORMATIONS*)))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *BATCH-MODE-P* NIL)
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE
  INHIBIT-FDEFINE-WARNINGS INHIBIT-FDEFINE-WARNINGS NIL)
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE TV:MORE-PROCESSING-GLOBAL-ENABLE
                                     TV:MORE-PROCESSING-GLOBAL-ENABLE NIL)

(DEFUN (:NOWARN MAKE-SYSTEM-KEYWORD) ()
  (SETQ INHIBIT-FDEFINE-WARNINGS :JUST-WARN
        TV:MORE-PROCESSING-GLOBAL-ENABLE NIL
        *BATCH-MODE-P* T
        *QUERY-TYPE* :NOCONFIRM))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *WARNINGS-STREAM* NIL)

(DEFUN (:DEFAULTED-BATCH MAKE-SYSTEM-KEYWORD) (&AUX PATHNAME)
  (SETQ PATHNAME (OR (SYSTEM-WARNINGS-PATHNAME-DEFAULT *SYSTEM-BEING-MADE*)
                     (SEND (FS:USER-HOMEDIR) :NEW-PATHNAME
                           :NAME (format nil "~A-CWARNS"
                                         (string-upcase (or (si:system-symbolic-name *system-being-made*)
                                                            (si:system-short-name *system-being-made*))))
                           :TYPE :LISP
                           :VERSION :NEWEST)))
  (SETQ INHIBIT-FDEFINE-WARNINGS :JUST-WARN
        TV:MORE-PROCESSING-GLOBAL-ENABLE NIL
        *BATCH-MODE-P* T
        *QUERY-TYPE* :NOCONFIRM)
  (FORMAT *QUERY-IO* "~&Writing compiler warnings data base to file ~A.~%" PATHNAME)
  (SETQ *WARNINGS-STREAM* (OPEN PATHNAME :DIRECTION :OUTPUT :CHARACTERS T))
  (FORMAT *WARNINGS-STREAM*
          "~&;System ~A made by ~A at ~\DATIME\  -*-Mode: Lisp; Package: User; Base: 10-*-~2%"
          (SYSTEM-NAME *SYSTEM-BEING-MADE*) USER-ID)
  (write-responsibility-comment *warnings-stream*)
  (PUSH `(CLOSE *WARNINGS-STREAM*)
        *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*))

(DEFUN (:BATCH MAKE-SYSTEM-KEYWORD) (&AUX PATHNAME)
  (SETQ PATHNAME (OR (SYSTEM-WARNINGS-PATHNAME-DEFAULT *SYSTEM-BEING-MADE*)
                     (SEND (FS:USER-HOMEDIR) :NEW-PATHNAME
                           :NAME (format nil "~A-CWARNS"
                                         (string-upcase (or (si:system-symbolic-name *system-being-made*)
                                                            (si:system-short-name *system-being-made*))))
                           :TYPE :LISP
                           :VERSION :NEWEST)))
  (SETQ PATHNAME (PROMPT-AND-READ
                   `(:PATHNAME :DEFAULTS ,PATHNAME)
                   "~&Write compiler warnings data base to file: (default ~A) " PATHNAME))
  (SETQ *WARNINGS-STREAM* (OPEN PATHNAME :DIRECTION :OUTPUT :CHARACTERS T))
  (SETQ INHIBIT-FDEFINE-WARNINGS :JUST-WARN
        TV:MORE-PROCESSING-GLOBAL-ENABLE NIL
        *BATCH-MODE-P* T
        *QUERY-TYPE* :NOCONFIRM)
  (FORMAT *WARNINGS-STREAM*
          "~&;System ~A made by ~A at ~\DATIME\  -*-Mode: Lisp; Package: User; Base: 10-*-~2%"
          (SYSTEM-NAME *SYSTEM-BEING-MADE*) USER-ID)
  (write-responsibility-comment *warnings-stream*)
  (PUSH `(CLOSE *WARNINGS-STREAM*)
        *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *SYSTEM-BEING-MADE* NIL)
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *MAKE-SYSTEM-FORMS-TO-BE-EVALED-BEFORE* NIL)

;The forms that the keywords put on this list
;will be evaluated after all transformations,
;within the compiler warnings context,
;only if we are not aborted.
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER* NIL)

;The forms that the keywords put on this list
;will be evaluated by an UNWIND-PROTECT at the very end of MAKE-SYSTEM.
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY* NIL)

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *SYSTEM-DEFAULT-BINARY-FILE-TYPE* NIL)

#+(target lambda)
(define-make-system-special-variable compiler:*target-computer* 'compiler:lambda-interface nil)
#+(target falcon)
(define-make-system-special-variable compiler:*target-computer* 'compiler:k nil)

(define-make-system-special-variable *system-load-binary-file-type* nil)

(define-make-system-special-variable compiler:*compilation-environment* ; $$$ fixes cold load <04-Nov-88 smh&keith>
                                     (and (boundp 'compiler:*compilation-environment*)
                                          compiler:*compilation-environment*)
  nil)

;;;A mechanism for choosing among a list of files.  Use as a compile and/or load condition.

(define-make-system-special-variable *choose-file-within-system* nil)

(defun choose-file-within-system (pathname binary &optional ignore)
  (setq pathname (pathname pathname))
  (if *choose-file-within-system*
      (and
        (or binary
            (setq pathname (send pathname :source-pathname)))
        (typecase *choose-file-within-system*
          (pathname (eq pathname *choose-file-within-system*))
          (list (member pathname *choose-file-within-system*))
          (t (if (eq *choose-file-within-system* t) t))))
    (and
      (case (fquery '(
                      :type :tyi
                      :choices (((:only "Yes [select]") #/Y #/T #/SPACE #/HAND-UP)
                                ((:no "No [don't select]") #/N #/RUBOUT #/HAND-DOWN)
                                ;;;Broken: ((:include "Include this [among others]") #/I #/+))
                                ((:all "Proceed [include all]") #/P #/A)))
                    "Select ~A ? " pathname)
        (:only (setq *choose-file-within-system* pathname))
        (:no nil)
        (:include
         (if (atom *choose-file-within-system*)
             (setq *choose-file-within-system* (list *choose-file-within-system*)))
         (pushnew pathname *choose-file-within-system*))
        (:all (setq *choose-file-within-system* t)))
      (file-newer-than-file-p pathname binary))))


(DEFVAR *SOMETHING-LOADED* :UNBOUND
  "Bound by MAKE-SYSTEM, set to T if any file is loaded.")

(DEFUN MAKE-SYSTEM (SYSTEM &REST KEYWORDS &AUX *SOMETHING-LOADED*)
  "Operate on the files of the system SYSTEM.
Most commonly used to compile or load those files which need it.
Keywords are not followed by values.
Commonly used keywords include:
 :COMPILE - recompile source files.
 :FOR-FALCON - compile for the Falcon
 :NOLOAD - don't load compiled files.
 :RELOAD - load even files already loaded.
 :RECOMPILE - recompile files already compiled.
 :SELECTIVE - ask user about each file individually.
 :NOCONFIRM - do not ask for confirmation at all.
 :NO-INCREMENT-PATCH - don't increment the patch version number of a patchable system.
 :INCREMENT-PATCH - do increment the patch version number.
 :NO-LOAD-PATCHES - do not load patches for patchable system being loaded.
 :NO-RELOAD-SYSTEM-DECLARATION - don't reload the file that contains the DEFSYSTEM.
 :PRINT-ONLY - don't load or compile anything, just say what needs to be done.
 :DESCRIBE - say when files were compiled or loaded, etc.
 :SILENT - don't print lists of files on the terminal at all.
 :BATCH - write a file containing any warnings produced by compilation.
      Just load the file, as lisp code, to reload the warnings.
 :DO-NOT-DO-COMPONENTS - omit subsystems."
  (catch-error-restart (eh:debugger-condition
                         "Give up on making the ~A system."
                         (let ((sys (find-system-named system t)))
                           (if sys (system-name sys) system)))
    ;; Force the system-defining file to get loaded
    ;; before we bind the variables or anything like that.
    (FIND-SYSTEM-NAMED SYSTEM)
    ;; First check whether there is a new system declaration that can be loaded
    (MAYBE-RELOAD-SYSTEM-DECLARATION SYSTEM KEYWORDS)
    (PROGW *MAKE-SYSTEM-SPECIAL-VARIABLES*
      (UNWIND-PROTECT
          (PROGN
            (SETQ *SYSTEM-BEING-MADE* (FIND-SYSTEM-NAMED SYSTEM))
            (SETQ *SYSTEM-DEFAULT-BINARY-FILE-TYPE*
                  (system-default-binary-file-type *SYSTEM-BEING-MADE*))
            (SETQ *TOP-LEVEL-TRANSFORMATIONS*
                  `(,@*LOAD-TYPE-TRANSFORMATIONS* DO-COMPONENTS-INTERNAL))
            ;; Do all the keywords
            (DOLIST (KEYWORD KEYWORDS)
              (LET ((FUNCTION (GET KEYWORD 'MAKE-SYSTEM-KEYWORD)))
                (OR FUNCTION
                    (FERROR NIL "~S is not a recognized option" KEYWORD))
                (FUNCALL FUNCTION)))
            ;; Make :NO-INCREMENT-PATCH override :COMPILE even if :COMPILE comes later.
            (WHEN *NO-INCREMENT-PATCH*
              (SETQ *TOP-LEVEL-TRANSFORMATIONS*
                    (DEL-IF #'(LAMBDA (X)
                                (MEMQ X '(INCREMENT-COMPILED-VERSION)))
                            *TOP-LEVEL-TRANSFORMATIONS*)))
            ;; Process forms with compiler context
            (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-BEFORE*)
              (EVAL FORM))
            ;; Do the work of the transformations
            (PERFORM-TRANSFORMATIONS (COLLECT-TOP-LEVEL-TRANSFORMATIONS
                                       *SYSTEM-BEING-MADE*))
            ;; Finally process any forms queued by the keywords with compiler context
            (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*)
              (EVAL FORM)))
        ;; Now forms outside of compiler context
        ;; These are done even if there was an error.
        (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*)
          (EVAL FORM))))
    *SOMETHING-LOADED*))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *FORCE-PACKAGE* NIL)

;;; Get all the transformations mentioned in a system or its children
(DEFUN COLLECT-TOP-LEVEL-TRANSFORMATIONS
       (SYSTEM &OPTIONAL FORCE-DEPENDENCIES &AUX PKG)
  (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM)
        PKG (SYSTEM-PACKAGE-DEFAULT SYSTEM))
  (LET-IF PKG ((*FORCE-PACKAGE* PKG))
    (LOOP FOR XFORM IN (SYSTEM-TOP-LEVEL-TRANSFORMATIONS SYSTEM)
          NCONC (IF (EQ (TRANSFORMATION-TYPE-NAME
                          (TRANSFORMATION-TRANSFORMATION-TYPE XFORM))
                        'DO-COMPONENTS-INTERNAL)
                    (AND (MEMQ 'DO-COMPONENTS-INTERNAL *TOP-LEVEL-TRANSFORMATIONS*)
                         (LOOP FOR SUBSYS IN (SYSTEM-COMPONENT-SYSTEMS SYSTEM)
                               WITH FORCE = (APPEND FORCE-DEPENDENCIES
                                                    (TRANSFORMATION-DEPENDENCIES XFORM))
                               NCONC (COLLECT-TOP-LEVEL-TRANSFORMATIONS SUBSYS FORCE)))
                    (NCONS (LIST XFORM *FORCE-PACKAGE* FORCE-DEPENDENCIES))))))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *FILE-TRANSFORMATION-LIST* NIL)
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *FILE-TRANSFORMATION-FUNCTION*
                                     'DO-FILE-TRANSFORMATIONS)

;;; Queue the transformations and pass the result onto the specified function
(DEFUN PERFORM-TRANSFORMATIONS (TRANSFORMATION-LIST)
  ;; First do the work on any transformations which are inputs to these
  (LET ((INPUTS (LOOP FOR ELEM IN TRANSFORMATION-LIST
                      AS XFORM = (FIRST ELEM)
                      AND PKG = (SECOND ELEM)
                      AND FORCE = (THIRD ELEM)
                      AS INPUT = (TRANSFORMATION-INPUT XFORM)
                      WHEN (TYPEP INPUT 'TRANSFORMATION) COLLECT (LIST INPUT PKG FORCE))))
    (AND INPUTS (PERFORM-TRANSFORMATIONS INPUTS)))
  ;;Add files to *FILE-TRANSFORMATION-LIST*
  (DOLIST (ELEM TRANSFORMATION-LIST)
    (LET ((*FORCE-PACKAGE* (SECOND ELEM))
          (*SYSTEM-BEING-MADE* (TRANSFORMATION-SYSTEM (FIRST ELEM))))
      (QUEUE-ONE-TRANSFORMATION (FIRST ELEM) (THIRD ELEM))))
  (FUNCALL *FILE-TRANSFORMATION-FUNCTION*))

;;; This is the usual workhorse, it actually calls the TRANSFORMATION-TYPE-FUNCTION's
(DEFUN DO-FILE-TRANSFORMATIONS ()
  (IF (OR (EQ *QUERY-TYPE* :NOCONFIRM)
           (QUERY-USER-LIST))
      ;;Now actually do the work
      (DOLIST (FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)
        (LET ((STATE (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION)))
          (case STATE
            ((:DONE :REFUSED :NOT-NEEDED NIL))  ;Already done or user said no
            ((:PENDING :PROBABLY)
             (LET ((TYPE (FILE-TRANSFORMATION-TRANSFORMATION-TYPE FILE-TRANSFORMATION))
                   (ARGS (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
                   (*FORCE-PACKAGE* (FILE-TRANSFORMATION-FORCE-PACKAGE FILE-TRANSFORMATION))
                   (*SYSTEM-BEING-MADE* (FILE-TRANSFORMATION-SYSTEM FILE-TRANSFORMATION)))
               (COND ((IF (EQ STATE :PROBABLY)  ;If we suspected something would change
                          (IF (APPLY (FILE-TRANSFORMATION-CONDITION-FUNCTION    ;check again
                                       FILE-TRANSFORMATION)
                                     ARGS)
                              T
                              (SETQ STATE :NOT-NEEDED)  ;Turned out it didn't
                              NIL)              ;Don't do it
                          T)
                      ;;Otherwise perform the transformation
                      (OR *SILENT-P*
                          (FORMAT T "~&~\SI::FILE-XFORM-TYPE\~:[ ~\SI::FILE-XFORM-ARGS\~;~*~]~
                                     ~:[~; in~:[to~] package ~A~]"
                                  TYPE (NULL ARGS) FILE-TRANSFORMATION *FORCE-PACKAGE*
                                  (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION)
                                  *FORCE-PACKAGE*))
                      (CATCH-ERROR-RESTART
                        ;;Fix to previous fix:
                        ;; Added the extra call to FORMAT to handle the simple-transformations
                        ;; whose pretty-name(s) have format directives that, apparently, should
                        ;; be applied to some form of the system name.  Prevents weird error
                        ;; proceed messages.  Also made the printing of transformation arguments
                        ;; occur only when non-NIL.  -KmC
                        (error "Give up ~(~A~)~@[ ~A~]."
                               (format nil
                                       (transformation-type-pretty-present-participle type)
                                       (system-symbolic-name *system-being-made*))
                               (car args))
                        (error-restart
                          (error "Retry ~(~A~)~@[ ~A~]."
                                 (format nil
                                         (transformation-type-pretty-present-participle type)
                                         (system-symbolic-name *system-being-made*))
                                 (car args))
                          (APPLY (TRANSFORMATION-TYPE-FUNCTION TYPE) ARGS))
                        (SETQ STATE :DONE)
                        ;;That probably made new versions of the outputs files
                        (DOLIST (PATHNAME (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION))
                          ;; So, forget any file info for the file.
                          (INVALIDATE-PATHNAME-INFO PATHNAME)
                          ;; Any transformation already done will need to be redone.
                          (DOLIST (FILE-XFORM *FILE-TRANSFORMATION-LIST*)
                            ;; Removed a check for :REFUSED here, 1/25/84,
                            ;; so that once a user says No, the transformation WILL NOT go.
                            (AND (MEMQ (FILE-TRANSFORMATION-STATE FILE-XFORM) '(:DONE))
                                 (DO ((L (FILE-TRANSFORMATION-ARGS FILE-XFORM) (CDR L))
                                      (TAIL (FILE-TRANSFORMATION-OUTPUTS FILE-XFORM)))
                                     ((EQ L TAIL) NIL)
                                   (AND (EQ PATHNAME (CAR L)) (RETURN T)))
                                 (SETF (FILE-TRANSFORMATION-STATE FILE-XFORM) :PROBABLY))))))))
             (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) STATE))
            (OTHERWISE
             (FERROR NIL "Transformation ~S in bad state" FILE-TRANSFORMATION)))))
    ;; If user says No to the entire bunch of transformations
    (DOLIST (FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)
      (AND (MEMQ (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) '(:PENDING :PROBABLY))
           (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) :REFUSED)))))

;;; Ask the user about a set of transformations pending
(DEFUN QUERY-USER-LIST ()
  (DO ((FILE-TRANSFORMATION-LIST *FILE-TRANSFORMATION-LIST* (CDR FILE-TRANSFORMATION-LIST))
       (TYPES-FOUND NIL)
       (N-FOUND 0)
       (LAST-TRANSFORMATION NIL) (LAST-TYPE T)
       (TRANSFORMATION) (TRANSFORMATION-TYPE NIL)
       (FIRST-P T))
      (NIL)
    (SETQ TRANSFORMATION-TYPE
          (AND (NOT (NULL FILE-TRANSFORMATION-LIST))
               (FILE-TRANSFORMATION-TRANSFORMATION-TYPE
                 (SETQ TRANSFORMATION (CAR FILE-TRANSFORMATION-LIST)))))
    (COND ((OR (NULL TRANSFORMATION-TYPE)
               (MEMQ (FILE-TRANSFORMATION-STATE TRANSFORMATION) '(:PENDING :PROBABLY)))
           (COND (LAST-TRANSFORMATION
                  (COND (FIRST-P
                         (FORMAT *QUERY-IO* "~2&")
                         (COND ((NULL (FILE-TRANSFORMATION-ARGS LAST-TRANSFORMATION))
                                (FORMAT *QUERY-IO* "Going to ~\SI::FILE-XFORM-ARGS\"
                                        LAST-TRANSFORMATION)
                                (RPLACA TYPES-FOUND
                                        (SETQ LAST-TYPE (NCONS LAST-TRANSFORMATION))))
                               (T
                                (FORMAT *QUERY-IO* "~2&File~:[s~] to be ~A:~%"
                                        (NEQ TRANSFORMATION-TYPE LAST-TYPE)
                                        (TRANSFORMATION-TYPE-PRETTY-PAST-PARTICIPLE
                                          LAST-TYPE))
                                (SETQ FIRST-P NIL)))
                         (SEND *QUERY-IO* :TYO #/NEWLINE)))
                  (AND (FILE-TRANSFORMATION-ARGS LAST-TRANSFORMATION)
                       (FORMAT *QUERY-IO* "~&~\SI::FILE-XFORM-ARGS\" LAST-TRANSFORMATION))
                  (SETQ N-FOUND (1+ N-FOUND))))
           (AND (NULL TRANSFORMATION-TYPE)
                (RETURN (AND (PLUSP N-FOUND)
                             (SELECTQ
                               (FQUERY `(:timeout #.(* 5 60. 60.)
                                         :default-value t
                                         :CHOICES
                                          (((S "Selective") #/S)
                                           . ,FORMAT:Y-OR-N-P-CHOICES))
                                       "~2&~\XFORM-TYPES\? "
                                       (NREVERSE TYPES-FOUND) N-FOUND)
                               ((T) T)
                               (S (LET ((*QUERY-TYPE* :SELECTIVE))
                                    (REQUERY-SELECTIVE *FILE-TRANSFORMATION-LIST*))
                                  (QUERY-USER-LIST))))))
           (AND (SETQ FIRST-P (NEQ TRANSFORMATION-TYPE LAST-TYPE))
                (PUSH* TRANSFORMATION-TYPE TYPES-FOUND))
           (SETQ LAST-TRANSFORMATION TRANSFORMATION
                 LAST-TYPE TRANSFORMATION-TYPE)))))

(DEFUN REQUERY-SELECTIVE (TRANSFORMATIONS)
  (DOLIST (TRANS TRANSFORMATIONS)
    (WHEN (MEMQ (FILE-TRANSFORMATION-STATE TRANS) '(:PROBABLY :PENDING))
      (UNLESS (QUERY-USER-SELECTIVE TRANS)
        (SETF (FILE-TRANSFORMATION-STATE TRANS) :REFUSED)))))

(DEFUN (FORMAT:XFORM-TYPES FORMAT:FORMAT-CTL-MULTI-ARG) (ARGS IGNORE &AUX TYPES N-FOUND)
  (SETF `(,TYPES ,N-FOUND) ARGS)
  (LOOP FOR PASS2 IN '(NIL T)
        DO (LOOP FOR TYPES ON TYPES
                 AS TYPE = (CAR TYPES)
                 WITH COMMA-P = (AND PASS2 (PLUSP N-FOUND))
                 WHEN (EQ PASS2 (LISTP (CAR TYPE)))
                    IF (NOT PASS2)
                    DO (IF COMMA-P (SEND *STANDARD-OUTPUT* :STRING-OUT
                                         (IF (LOOP FOR TYP IN (CDR TYPES)
                                                   ALWAYS (LISTP (CAR TYP)))
                                             " or " ", "))
                         (SETQ COMMA-P T))
                       (SEND *STANDARD-OUTPUT* :STRING-OUT
                             (TRANSFORMATION-TYPE-PRETTY-IMPERATIVE TYPE))
                    ELSE DO (IF COMMA-P (SEND *STANDARD-OUTPUT* :STRING-OUT
                                              (IF (LOOP FOR TYP IN (CDR TYPES)
                                                        ALWAYS (NLISTP (CAR TYP)))
                                                  " and " ", "))
                              (SETQ COMMA-P T))
                            (FORMAT T "~\SI::FILE-XFORM-ARGS\" (CAR TYPE))
                 ELSE IF (NOT PASS2) DO (DECF N-FOUND)
                 FINALLY (AND (NOT PASS2) (PLUSP N-FOUND)
                              (FORMAT T " ~:[it~;~:[both~;all ~R~] of them~]"
                                      (> N-FOUND 1) (> N-FOUND 2) N-FOUND)))))

(DEFVAR *WHOLE-SYSTEM-TYPE-TRANSFORMATIONS*
        '(INCREMENT-LOADED-VERSION INCREMENT-COMPILED-VERSION))

(FORMAT:DEFFORMAT FILE-XFORM-ARGS (:ONE-ARG) (FILE-TRANSFORMATION IGNORE)
  (LET ((ARGS (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION)))
    (IF (NULL ARGS)
        (LET* ((TYPE (FILE-TRANSFORMATION-TRANSFORMATION-TYPE FILE-TRANSFORMATION))
               (IMPERATIVE (TRANSFORMATION-TYPE-PRETTY-IMPERATIVE TYPE)))
          (IF (MEMQ (TRANSFORMATION-TYPE-NAME TYPE) *WHOLE-SYSTEM-TYPE-TRANSFORMATIONS*)
              (FORMAT T IMPERATIVE (SYSTEM-NAME
                                     (FILE-TRANSFORMATION-SYSTEM FILE-TRANSFORMATION)))
            (SEND *QUERY-IO* :STRING-OUT IMPERATIVE)))
      (DO ((FILE-LIST ARGS (CDR FILE-LIST))
           (OUTPUTS (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION))
           (FIRST-P T NIL))
          ((EQ FILE-LIST OUTPUTS))
        (when (car file-list)
          (unless FIRST-P
            (SEND *STANDARD-OUTPUT* :STRING-OUT (IF (EQ (CDR FILE-LIST) OUTPUTS)
                                                     " and " ", ")))
          (PRINC (CAR FILE-LIST)))))))

(FORMAT:DEFFORMAT FILE-XFORM-TYPE (:ONE-ARG) (TYPE IGNORE &AUX STRING)
  (SETQ STRING (TRANSFORMATION-TYPE-PRETTY-PRESENT-PARTICIPLE TYPE))
  (IF (NOT (MEMQ (TRANSFORMATION-TYPE-NAME TYPE) *WHOLE-SYSTEM-TYPE-TRANSFORMATIONS*))
      (SEND *STANDARD-OUTPUT* :STRING-OUT STRING)
    (FORMAT T STRING (SYSTEM-NAME *SYSTEM-BEING-MADE*))))

(DEFUN (:PRINT-ONLY MAKE-SYSTEM-KEYWORD) ()
  (SETQ *FILE-TRANSFORMATION-FUNCTION* 'PRINT-FILE-TRANSFORMATIONS))

(DEFUN PRINT-FILE-TRANSFORMATIONS ()
  "Implements the :PRINT-ONLY keyword of MAKE-SYSTEM.
This keyword causes MAKE-SYSTEM to print what it would do but not do it."
  (DOLIST (FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)
    (LET ((STATE (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION)))
      (SELECTQ STATE
        ((:DONE :REFUSED :NOT-NEEDED NIL))
        ((:PENDING :PROBABLY)
         (LET ((TYPE (FILE-TRANSFORMATION-TRANSFORMATION-TYPE FILE-TRANSFORMATION))
               (ARGS (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
               (OUTPUTS (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION))
               (*FORCE-PACKAGE* (FILE-TRANSFORMATION-FORCE-PACKAGE FILE-TRANSFORMATION))
               (*SYSTEM-BEING-MADE* (FILE-TRANSFORMATION-SYSTEM FILE-TRANSFORMATION)))
           (COND ((NOT *SILENT-P*)
                  (IF (NULL (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
                      (FORMAT *QUERY-IO* "~&Need to ~\SI::FILE-XFORM-ARGS\"
                              FILE-TRANSFORMATION)
                      (FORMAT T "~&~\SI::FILE-XFORM-ARGS\~:[ probably then~] need~:[s~] to be ~A~
                                 ~:[~; in~:[to~] package ~A~]"
                              FILE-TRANSFORMATION (NEQ STATE :PROBABLY)
                              (NEQ (CDR ARGS) OUTPUTS)
                              (TRANSFORMATION-TYPE-PRETTY-PAST-PARTICIPLE TYPE)
                              *FORCE-PACKAGE*
                              (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION)
                              *FORCE-PACKAGE*)))
                 ('ELSE
                  (SETQ *SOMETHING-LOADED* (NCONC *SOMETHING-LOADED*
                                                  (NCONS FILE-TRANSFORMATION))))))
         (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) :DONE))
        (OTHERWISE
         (FERROR NIL "Transformation ~S in bad state" FILE-TRANSFORMATION))))))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *TRANSFORMATION-OUTPUTS* NIL)

;;; List added by each call to QUEUE-ONE-TRANSFORMATION
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *ADDED-FILE-TRANSFORMATIONS* NIL)

;;Queue one transformation of a system to be done -- if it needs to be done.
;;We also take care of queueing its dependencies.
;;FORCE-DEPENDENCIES is a list of other transformations
;; to be treated as if they were dependencies of this one.
;;But the dependencies are only queued if it appears this transformation
;; really has work to do. We check that there is
;; (1) a file in this transformation that hasn't been loaded/compiled, and
;; (2) a reason to want the results of this transformation in the first place.
;;REST should be any transformations that depend on this one.
(DEFUN QUEUE-ONE-TRANSFORMATION (TRANSFORMATION FORCE-DEPENDENCIES &REST OTHERS
                                 &AUX (*ADDED-FILE-TRANSFORMATIONS* NIL))
  (AND (MEMQ TRANSFORMATION OTHERS) (FERROR NIL "Recursive dependencies detected"))
  (OR (ASSQ TRANSFORMATION *TRANSFORMATION-OUTPUTS*)    ;Unless already pending
      (LET ((INPUT (GET-TRANSFORMATION-INPUT-FILE-TRANSFORMATIONS TRANSFORMATION))
            (NAME (TRANSFORMATION-TYPE-NAME (TRANSFORMATION-TRANSFORMATION-TYPE
                                              TRANSFORMATION))))
        ;;If allowed by user switches, or a dependency
        (COND ((AND (OR OTHERS (MEMQ NAME *TOP-LEVEL-TRANSFORMATIONS*))
                    ;;and some files in there need to be done,
                    (QUEUE-FILES-AS-NEEDED INPUT))
               ;;must do the dependencies first
               (DOLIST (DEPENDENCY FORCE-DEPENDENCIES)
                 (APPLY 'QUEUE-ONE-TRANSFORMATION (FIND-DEPENDENCY DEPENDENCY)
                        NIL TRANSFORMATION OTHERS))
               (DOLIST (DEPENDENCY (TRANSFORMATION-DEPENDENCIES TRANSFORMATION))
                 (APPLY 'QUEUE-ONE-TRANSFORMATION (FIND-DEPENDENCY DEPENDENCY)
                        NIL TRANSFORMATION OTHERS))))))
  ;;These go at the end of the list
  (SETQ *FILE-TRANSFORMATION-LIST*
        (NCONC *FILE-TRANSFORMATION-LIST* (NREVERSE *ADDED-FILE-TRANSFORMATIONS*))))

;;; Get a list of FILE-TRANSFORMATION's from the INPUT to a single TRANSFORMATION
(DEFUN GET-TRANSFORMATION-INPUT-FILE-TRANSFORMATIONS (TRANSFORMATION &AUX INPUT PATHNAME-LIST)
  (COND ((SETQ INPUT (TRANSFORMATION-INPUT TRANSFORMATION))
         (SELECTQ (TYPE-OF INPUT)
           (TRANSFORMATION
            (SETQ PATHNAME-LIST (GET-TRANSFORMATION-PATHNAMES INPUT)))
           (MODULE
            (SETQ PATHNAME-LIST (GET-MODULE-PATHNAMES INPUT)))
           (OTHERWISE
            (FERROR NIL "~S is not a valid transformation input" INPUT)))
         (SETQ PATHNAME-LIST (LOOP FOR PATHNAME IN PATHNAME-LIST
                                   COLLECT (ADD-FILE-TRANSFORMATION TRANSFORMATION
                                                                    PATHNAME))))
        (T
         (SETQ PATHNAME-LIST (NCONS (ADD-FILE-TRANSFORMATION TRANSFORMATION NIL)))))
  PATHNAME-LIST)

;;; This is until circular lists are supported better
(DEFMACRO POP-CAREFULLY (LIST)
  `(PROG1 (CAR ,LIST)
          (SETQ ,LIST (OR (CDR ,LIST) ,LIST))))

;;; Get the pathnames for a transformation.  If it is pending, use that;
;;; else, compute the pathnames by applying the file type transformation for each level
(DEFUN GET-TRANSFORMATION-PATHNAMES (TRANSFORMATION &AUX PATHNAME-LIST)
  (IF (SETQ PATHNAME-LIST (CDR (ASSQ TRANSFORMATION *TRANSFORMATION-OUTPUTS*)))
      (VALUES PATHNAME-LIST T)
      (LET ((INPUT (TRANSFORMATION-INPUT TRANSFORMATION)))
        (SELECTQ (TYPE-OF INPUT)
          (MODULE
           (SETQ PATHNAME-LIST (GET-MODULE-PATHNAMES INPUT)))
          (TRANSFORMATION
           (SETQ PATHNAME-LIST (GET-TRANSFORMATION-PATHNAMES INPUT)))
          (OTHERWISE
           (FERROR NIL "~S is not a valid transformation input" INPUT))))
      (LOOP FOR PATHNAME IN PATHNAME-LIST
            WITH TRANSFORMATION-TYPE = (TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION)
            AS PKG = (POP PATHNAME)
            ;;Take off as many inputs as would be used
            DO (DO L (TRANSFORMATION-TYPE-INPUT-FILE-TYPES TRANSFORMATION-TYPE)
                   (CDR L) (NULL L)
                 (POP-CAREFULLY PATHNAME))
            ;;Now accumulate output types
            AS OUTPUTS = (LOOP FOR FILE-TYPE IN (TRANSFORMATION-TYPE-OUTPUT-FILE-TYPES
                                                  TRANSFORMATION-TYPE)
                               for fte = (eval file-type)
                               COLLECT (when fte
                                         (MERGE-PATHNAME-TYPE (POP-CAREFULLY PATHNAME) fte)))
            COLLECT (CONS PKG (NCONC OUTPUTS PATHNAME)))))

;;; Get PATHNAME's from a MODULE.  Binding package property as we go down if necessary.
;;; OTHER-SYSTEMS-OK is for things like SYSTEM-SOURCE-FILES that only look locally.
(DEFUN GET-MODULE-PATHNAMES (MODULE &OPTIONAL (OTHER-SYSTEMS-OK T) &AUX PKGPROP)
  (LET-IF (SETQ PKGPROP (GETL (LOCF (MODULE-PLIST MODULE)) '(:PACKAGE)))
          ((*FORCE-PACKAGE* (CADR PKGPROP)))
    (GET-MODULE-COMPONENTS-PATHNAMES (MODULE-COMPONENTS MODULE) OTHER-SYSTEMS-OK)))

;;; Get a list of PATHNAME's from a MODULE's COMPONENTS
(DEFUN GET-MODULE-COMPONENTS-PATHNAMES (COMPONENTS &OPTIONAL (OTHER-SYSTEMS-OK T))
  (LOOP FOR COMPONENT IN COMPONENTS
        NCONC (COND ((TYPEP COMPONENT 'MODULE)
                     ;;Another module, get its components
                     (GET-MODULE-PATHNAMES COMPONENT))
                    ((NLISTP COMPONENT)
                     (FERROR NIL "~S is not a valid module component" COMPONENT))
                    ((SYMBOLP (CAR COMPONENT))
                     ;;(SYSTEM . MODULE-NAME's)
                     (AND OTHER-SYSTEMS-OK
                          (LOOP FOR NAME IN (CDR COMPONENT)
                                WITH SYSTEM = (FIND-SYSTEM-NAMED (CAR COMPONENT))
                                NCONC (GET-MODULE-PATHNAMES (FIND-MODULE-NAMED NAME
                                                                               SYSTEM)))))
                    ;;Terminal nodes are pathname lists.  Collect (package . pathnames).
                    (T (NCONS (CONS *FORCE-PACKAGE* COMPONENT))))))

(DEFUN ADD-FILE-TRANSFORMATION (TRANSFORMATION PATHNAMES
                                &AUX TRANSFORMATION-TYPE CONDITION-FUNCTION
                                     INPUT-XFORM PKG INPUTS OUTPUTS ARGS FILE-TRANSFORMATION
                                     SYSTEM)
  (SETQ TRANSFORMATION-TYPE (TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION)
        CONDITION-FUNCTION (TRANSFORMATION-CONDITION-FUNCTION TRANSFORMATION)
        SYSTEM (TRANSFORMATION-SYSTEM TRANSFORMATION))
  (SETQ PKG (POP PATHNAMES))
  (AND (LISTP PKG) (SETQ INPUT-XFORM PKG
                         PKG (FILE-TRANSFORMATION-FORCE-PACKAGE PKG)))
  (SETQ INPUTS (LOOP FOR FILE-TYPE IN (TRANSFORMATION-TYPE-INPUT-FILE-TYPES
                                         TRANSFORMATION-TYPE)
                     for fte = (EVAL FILE-TYPE)
                     COLLECT (when fte
                               (MERGE-PATHNAME-TYPE (POP-CAREFULLY PATHNAMES) fte)))
        OUTPUTS (LOOP FOR FILE-TYPE IN (TRANSFORMATION-TYPE-OUTPUT-FILE-TYPES
                                          TRANSFORMATION-TYPE)
                      for fte = (EVAL FILE-TYPE)
                      COLLECT (when fte
                                (MERGE-PATHNAME-TYPE (POP-CAREFULLY PATHNAMES)
                                                     fte)))
        ARGS (NCONC INPUTS OUTPUTS))
  (COND ((SETQ FILE-TRANSFORMATION (DOLIST (FILE-XFORM *FILE-TRANSFORMATION-LIST*)
                                     (AND (EQ (FILE-TRANSFORMATION-TRANSFORMATION-TYPE
                                                FILE-XFORM)
                                              TRANSFORMATION-TYPE)
                                          (EQUAL (FILE-TRANSFORMATION-ARGS FILE-XFORM) ARGS)
                                          (EQ (FILE-TRANSFORMATION-FORCE-PACKAGE FILE-XFORM)
                                              PKG)
                                          (EQ (FILE-TRANSFORMATION-SYSTEM FILE-XFORM)
                                              SYSTEM)
                                          (RETURN FILE-XFORM))))
         ;;Found, extend the condition
         (SETF (FILE-TRANSFORMATION-CONDITION-FUNCTION FILE-TRANSFORMATION)
               (LET ((OLD-CONDITION-FUNCTION (FILE-TRANSFORMATION-CONDITION-FUNCTION
                                               FILE-TRANSFORMATION)))
                 (COND ((EQ OLD-CONDITION-FUNCTION CONDITION-FUNCTION)
                        CONDITION-FUNCTION)     ;The same
                       ((CLOSUREP OLD-CONDITION-FUNCTION)
                        (PUSH* CONDITION-FUNCTION
                               (SYMEVAL-IN-CLOSURE OLD-CONDITION-FUNCTION
                                                   '*CONDITION-FUNCTIONS*)))
                       (T
                        (LET-CLOSED ((*CONDITION-FUNCTIONS* (LIST OLD-CONDITION-FUNCTION
                                                                  CONDITION-FUNCTION)))
                          'MULTIPLE-FILE-CONDITION)))))
         (SETQ *FILE-TRANSFORMATION-LIST*
               (DELQ FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)))
        (T
         (SETQ FILE-TRANSFORMATION (MAKE-FILE-TRANSFORMATION TRANSFORMATION-TYPE
                                                               TRANSFORMATION-TYPE
                                                             FORCE-PACKAGE PKG
                                                             SYSTEM SYSTEM
                                                             CONDITION-FUNCTION
                                                               CONDITION-FUNCTION
                                                             OUTPUTS OUTPUTS
                                                             ARGS ARGS))))
  (PUSH FILE-TRANSFORMATION *ADDED-FILE-TRANSFORMATIONS*)
  (LET ((OUTPUT (CONS FILE-TRANSFORMATION (APPEND OUTPUTS PATHNAMES)))
        (ELEM (ASSQ TRANSFORMATION *TRANSFORMATION-OUTPUTS*)))
    (IF ELEM (NCONC ELEM (NCONS OUTPUT))
        (PUSH (LIST TRANSFORMATION OUTPUT) *TRANSFORMATION-OUTPUTS*)))
  (CONS INPUT-XFORM FILE-TRANSFORMATION))

;;; This is closed over when a file-transformation is added with two different conditions
;;; It OR's those conditions
(DEFUN MULTIPLE-FILE-CONDITION (&REST ARGS)
  (LOCAL-DECLARE ((SPECIAL *CONDITION-FUNCTIONS*))
    (DOLIST (FUNCTION *CONDITION-FUNCTIONS*)
      (AND (APPLY FUNCTION ARGS)
           (RETURN T)))))

(DEFUN QUEUE-FILES-AS-NEEDED (LIST)
  (DO ((LIST LIST (CDR LIST))
       (FLAG NIL)
       (FILE-TRANSFORMATION) (STATE) (PROBABLY-P))
      ((NULL LIST) FLAG)
    (SETQ FILE-TRANSFORMATION (CAR LIST))
    ;; PROBABLY-P is a weird kludge.  It means we are processing a dependent transformation
    ;; whose input comes from the output of a transformation being done at this same level.
    ;; We cannot check file dates at this point, since they are likely to be invalidated.
    ;; Instead we remember for later that we were in this state and check then.
    (SETQ PROBABLY-P (MEMQ (FILE-TRANSFORMATION-STATE (CAR FILE-TRANSFORMATION))
                           '(:PENDING :PROBABLY))
          FILE-TRANSFORMATION (CDR FILE-TRANSFORMATION))
    (COND ((NULL (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION))
           (IF (NOT (OR *REDO-ALL*
                        (LET ((*FORCE-PACKAGE* (FILE-TRANSFORMATION-FORCE-PACKAGE
                                                 FILE-TRANSFORMATION))
                              (*SYSTEM-BEING-MADE* (FILE-TRANSFORMATION-SYSTEM
                                                     FILE-TRANSFORMATION)))
                          (APPLY (FILE-TRANSFORMATION-CONDITION-FUNCTION FILE-TRANSFORMATION)
                                 (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION)))))
               (SETQ STATE (COND ((NOT PROBABLY-P) :NOT-NEEDED)
                                 ;;Not exactly right to ask at this point,
                                 ;;but avoids asking questions after compilation has started.
                                 ((QUERY-USER-SELECTIVE FILE-TRANSFORMATION)
                                  (SETQ FLAG T)
                                  :PROBABLY)
                                 (T :REFUSED)))
               (IF (NOT (QUERY-USER-SELECTIVE FILE-TRANSFORMATION))
                   (SETQ STATE :REFUSED)
                   (SETQ STATE :PENDING
                         FLAG T)))
           (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) STATE)))))

;;; Define terminal file transformation macro
(DEFMACRO DEFINE-SIMPLE-TRANSFORMATION (NAME FUNCTION DEFAULT-CONDITION
                                        INPUT-FILE-TYPES OUTPUT-FILE-TYPES
                                        &OPTIONAL PRETTY-NAMES
                                                  (COMPILE-LIKE T) (LOAD-LIKE NIL LL-P))
  (OR LL-P (SETQ LOAD-LIKE (NOT COMPILE-LIKE)))
  (OR PRETTY-NAMES (SETQ PRETTY-NAMES (STRING-DOWNCASE NAME)))
  (OR (LISTP PRETTY-NAMES)
      (LET* ((LENGTH (STRING-LENGTH PRETTY-NAMES))
             (E-P (CHAR-EQUAL (AREF PRETTY-NAMES (1- LENGTH)) #/e))
             (UPSTART (CHAR-UPCASE (AREF PRETTY-NAMES 0)))
             (REST (NSUBSTRING PRETTY-NAMES 1))
             (START (IF E-P (NSUBSTRING PRETTY-NAMES 1 (1- LENGTH)) REST)))
        (SETQ PRETTY-NAMES (LIST (STRING-APPEND UPSTART REST)
                                 (STRING-APPEND UPSTART START "ing")
                                 (STRING-APPEND (AREF PRETTY-NAMES 0) START "ed")))))
  `(PROGN 'COMPILE
     (ADD-SIMPLE-TRANSFORMATION ',NAME ',FUNCTION ',INPUT-FILE-TYPES ',OUTPUT-FILE-TYPES
                                ',PRETTY-NAMES ',COMPILE-LIKE ',LOAD-LIKE)
     (DEFMACRO (,NAME DEFSYSTEM-MACRO) (INPUT &OPTIONAL DEPENDENCIES CONDITION)
       (PARSE-TRANSFORMATION ',NAME INPUT DEPENDENCIES (OR CONDITION ',DEFAULT-CONDITION)))))

(DEFVAR *TRANSFORMATION-TYPE-ALIST* NIL)

(DEFUN ADD-SIMPLE-TRANSFORMATION (NAME FUNCTION INPUT-FILE-TYPES OUTPUT-FILE-TYPES
                                  PRETTY-NAMES COMPILE-LIKE LOAD-LIKE
                                  &AUX TRANSFORMATION-TYPE)
  (SETQ TRANSFORMATION-TYPE (MAKE-TRANSFORMATION-TYPE NAME NAME
                                                      PRETTY-NAMES PRETTY-NAMES
                                                      FUNCTION FUNCTION
                                                      INPUT-FILE-TYPES INPUT-FILE-TYPES
                                                      OUTPUT-FILE-TYPES OUTPUT-FILE-TYPES))
  (SETQ *TRANSFORMATION-TYPE-ALIST*
        (CONS TRANSFORMATION-TYPE
              (DEL #'(LAMBDA (X Y) (EQ (TRANSFORMATION-TYPE-NAME X)
                                       (TRANSFORMATION-TYPE-NAME Y)))
                   TRANSFORMATION-TYPE *TRANSFORMATION-TYPE-ALIST*)))
  (AND COMPILE-LIKE (PUSH* NAME *COMPILE-TYPE-TRANSFORMATIONS*))
  (AND LOAD-LIKE (PUSH* NAME *LOAD-TYPE-TRANSFORMATIONS*)))

;;; Here are the initial simple transformations
(DEFINE-SIMPLE-TRANSFORMATION :FASLOAD FASLOAD-1 FILE-NEWER-THAN-INSTALLED-P
                              (*system-default-binary-file-type* *SYSTEM-load-BINARY-FILE-TYPE*) NIL "load" NIL)

(DEFINE-SIMPLE-TRANSFORMATION :COMPILE QC-FILE-1 FILE-NEWER-THAN-FILE-P
                              (:LISP) (*SYSTEM-DEFAULT-BINARY-FILE-TYPE* *system-load-binary-file-type*))

(define-simple-transformation :cross-compile qc-file-cross-1 file-newer-than-file-p
                              (:LISP) (:fbin :fdef))

(DEFINE-SIMPLE-TRANSFORMATION :READFILE READFILE-1 FILE-NEWER-THAN-INSTALLED-P
                              (:LISP) NIL ("Read" "Reading" "read") NIL)

(define-simple-transformation :fasload-falcon-environment fasload-falcon-environment-1 FILE-NEWER-THAN-INSTALLED-P
                              (:fbin :fdef) NIL ("Read" "Reading" "read") NIL)

;:COMPAT-COMPILE, etc., are obsolete as of system 92, but not flushed yet
(DEFINE-SIMPLE-TRANSFORMATION :COMPAT-COMPILE QC-FILE-1 FILE-NEWER-THAN-FILE-P
                              (:LISP) (*SYSTEM-DEFAULT-BINARY-FILE-TYPE*))
(DEFINE-SIMPLE-TRANSFORMATION DO-COMPONENTS-INTERNAL IGNORE TRUE NIL NIL NIL NIL NIL)

;;; Some compound cases
(DEFMACRO (:COMPILE-LOAD DEFSYSTEM-MACRO) (INPUT &OPTIONAL COM-DEP LOAD-DEP
                                           COM-COND LOAD-COND)
  `(:FASLOAD (:COMPILE ,INPUT ,COM-DEP ,COM-COND) ,LOAD-DEP ,LOAD-COND))

(defmacro (:cross-compile-load-falcon-environment defsystem-macro) (input &optional com-dep load-dep com-cond load-cond)
  `(:fasload-falcon-environment (:cross-compile ,input ,com-dep ,com-cond) ,load-dep ,load-cond))

(DEFMACRO (:COMPAT-COMPILE-LOAD DEFSYSTEM-MACRO) (INPUT &OPTIONAL COM-DEP LOAD-DEP
                                                  COM-COND LOAD-COND)
  `(:FASLOAD (:COMPAT-COMPILE ,INPUT ,COM-DEP ,COM-COND) ,LOAD-DEP ,LOAD-COND))

;;; The main transformation parser
(DEFINE-DEFSYSTEM-SPECIAL-VARIABLE *ADD-TRANSFORMATION-TO-SYSTEM* T)

(DEFUN PARSE-TRANSFORMATION (NAME INPUT DEPENDENCIES CONDITION
                             &AUX TRANSFORMATION-TYPE TRANSFORMATION)
  (OR (SETQ TRANSFORMATION-TYPE (ASSQ NAME *TRANSFORMATION-TYPE-ALIST*))
      (FERROR NIL "~S is not a known transformation type" NAME))
  ;;CONDITION is an atom of a function name or some lisp code
;  (AND (LISTP CONDITION)
;       (SETQ CONDITION (GENERATE-INTERNAL-CONDITION CONDITION INPUT TRANSFORMATION-TYPE)))
  ;;INPUT can be either a MODULE-SPECIFICATION or another transformation
  (LET ((*ADD-TRANSFORMATION-TO-SYSTEM* (IF (EQ *ADD-TRANSFORMATION-TO-SYSTEM* :SKIP)
                                            T NIL)))
    (SETQ INPUT (COND ((NULL INPUT) NIL)
                      ((NLISTP INPUT)           ;A single module input
                       (FIND-MODULE-NAMED INPUT *SYSTEM-BEING-DEFINED*))
                      ((AND (SYMBOLP (CAR INPUT))
                            (GET (CAR INPUT) 'DEFSYSTEM-MACRO)) ;Another transformation
                       (CALL-DEFSYSTEM-MACRO INPUT))
                      (T                        ;Otherwise generate a new module to hold them
                       (ADD-MODULE (GENSYM) *SYSTEM-BEING-DEFINED* INPUT)))))
  ;;DEPENDENCIES is (TRANSFORMATION . MODULE-SPECS) or a list of those
  (LET ((*ADD-TRANSFORMATION-TO-SYSTEM* NIL))
    (OR (LISTP (CAR DEPENDENCIES)) (SETQ DEPENDENCIES (NCONS DEPENDENCIES)))
    (SETQ DEPENDENCIES (LOOP FOR DEPENDENCY IN DEPENDENCIES
                             NCONC (BUILD-DEPENDENCIES DEPENDENCY *SYSTEM-BEING-DEFINED*))))
  (SETQ TRANSFORMATION (MAKE-TRANSFORMATION TRANSFORMATION-TYPE TRANSFORMATION-TYPE
                                            INPUT INPUT
                                            DEPENDENCIES DEPENDENCIES
                                            CONDITION-FUNCTION CONDITION
                                            SYSTEM *SYSTEM-BEING-DEFINED*))
  (AND (EQ *ADD-TRANSFORMATION-TO-SYSTEM* T)
       (SETF (SYSTEM-TOP-LEVEL-TRANSFORMATIONS *SYSTEM-BEING-DEFINED*)
             (NCONC (SYSTEM-TOP-LEVEL-TRANSFORMATIONS *SYSTEM-BEING-DEFINED*)
                    (NCONS TRANSFORMATION))))
  (SETF (SYSTEM-TRANSFORMATIONS *SYSTEM-BEING-DEFINED*)
        (NCONC (SYSTEM-TRANSFORMATIONS *SYSTEM-BEING-DEFINED*) (NCONS TRANSFORMATION)))
  (VALUES NIL TRANSFORMATION))

;;; Collect a set of dependencies
(DEFUN BUILD-DEPENDENCIES (DEPENDENCY SYSTEM)
  (AND DEPENDENCY
       (LOOP FOR MODULE IN (OR (CDR DEPENDENCY) '(NIL))
             WITH TRANSFORMATION-TYPE = (OR (ASSQ (CAR DEPENDENCY)
                                                  *TRANSFORMATION-TYPE-ALIST*)
                                            (FERROR NIL "Unknown transformation type ~S"
                                                    (CAR DEPENDENCY)))
             NCONC (BUILD-DEPENDENCY TRANSFORMATION-TYPE SYSTEM MODULE))))

(DEFUN BUILD-DEPENDENCY (TRANSFORMATION-TYPE SYSTEM MODULE)
  (IF (LISTP MODULE)
      (LOOP FOR MODULE-NAME IN (CDR MODULE)
            WITH SYSTEM-NAME = (CAR MODULE)
            COLLECT `(,TRANSFORMATION-TYPE ,SYSTEM-NAME ,MODULE-NAME))
      (NCONS (FIND-DEPENDENCY-1 TRANSFORMATION-TYPE SYSTEM MODULE))))

(DEFUN FIND-DEPENDENCY (DEPENDENCY)
  (IF (TYPEP DEPENDENCY 'TRANSFORMATION)
      DEPENDENCY
      (APPLY #'FIND-DEPENDENCY-1 DEPENDENCY)))

(DEFUN FIND-DEPENDENCY-1 (TRANSFORMATION-TYPE SYSTEM MODULE)
  (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM)
        MODULE (AND MODULE (FIND-MODULE-NAMED MODULE SYSTEM)))
  (OR (DOLIST (TRANSFORMATION (SYSTEM-TRANSFORMATIONS (FIND-SYSTEM-NAMED SYSTEM)))
;       (format t "~&TRANSFORMATION: ~A, TYPE ~A" transformation-type (transformation-transformation-type transformation))
        (AND
;         (progn
;              (format t "~&EQual: ~A" (equal transformation-type (transformation-transformation-type transformation)))
;              t)
             (EQ TRANSFORMATION-TYPE (TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION))
             (EQ MODULE
                 (DO ((X TRANSFORMATION (TRANSFORMATION-INPUT X)))
                     ((NOT (TYPEP X 'TRANSFORMATION)) X)
;                  (progn (format t "~&~A" transformation))
                   ))
             (RETURN TRANSFORMATION)))
      (FERROR NIL "Transformation ~S not found on ~S in ~S"
              TRANSFORMATION-TYPE MODULE SYSTEM)))

;;; This perhaps needs a better name
(DEFMACRO (:COMPILE-LOAD-INIT DEFSYSTEM-MACRO) (INPUT ADD-DEP &OPTIONAL COM-DEP LOAD-DEP
                                                              &AUX FUNCTION)
  (SETQ FUNCTION (LET-CLOSED ((*ADDITIONAL-DEPENDENT-MODULES*
                                (PARSE-MODULE-COMPONENTS ADD-DEP *SYSTEM-BEING-DEFINED*)))
                   'COMPILE-LOAD-INIT-CONDITION))
  `(:FASLOAD (:COMPILE ,INPUT ,COM-DEP ,FUNCTION) ,LOAD-DEP))

(DEFMACRO (:COMPAT-COMPILE-LOAD-INIT DEFSYSTEM-MACRO)
          (INPUT ADD-DEP &OPTIONAL COM-DEP LOAD-DEP
           &AUX FUNCTION)
  (SETQ FUNCTION (LET-CLOSED ((*ADDITIONAL-DEPENDENT-MODULES*
                                (PARSE-MODULE-COMPONENTS ADD-DEP *SYSTEM-BEING-DEFINED*)))
                   'COMPILE-LOAD-INIT-CONDITION))
  `(:FASLOAD (:COMPAT-COMPILE ,INPUT ,COM-DEP ,FUNCTION) ,LOAD-DEP))

(DEFUN COMPILE-LOAD-INIT-CONDITION (SOURCE-FILE QFASL-FILE &optional com-dep load-dep)
  (declare (ignore com-dep load-dep))
  (OR (FILE-NEWER-THAN-FILE-P SOURCE-FILE QFASL-FILE)
      (LOCAL-DECLARE ((SPECIAL *ADDITIONAL-DEPENDENT-MODULES*))
        (OTHER-FILES-NEWER-THAN-FILE-P *ADDITIONAL-DEPENDENT-MODULES* QFASL-FILE))))

;;; Have any files from which compile-flavor-methods or something in this file are generated
;;; changed?
(DEFUN OTHER-FILES-NEWER-THAN-FILE-P (MODULES FILE &AUX CREATION-DATE FILE-TYPE)
  (SETQ CREATION-DATE (SYSTEM-GET-CREATION-DATE FILE))
  (DOLIST (MODULE MODULES)
    (OR (DOLIST (TRANSFORMATION (SYSTEM-TRANSFORMATIONS (MODULE-SYSTEM MODULE)))
          (COND ((EQ (TRANSFORMATION-INPUT TRANSFORMATION) MODULE)
                 (SETQ FILE-TYPE (CAR (TRANSFORMATION-TYPE-INPUT-FILE-TYPES
                                        (TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION))))
                 (RETURN T))))
        (FERROR NIL "Module ~S not found in any transformation" MODULE))
    (AND (DOLIST (PATHNAMES (GET-MODULE-PATHNAMES MODULE))
           (AND (> (SYSTEM-GET-CREATION-DATE (MERGE-PATHNAME-TYPE (CADR PATHNAMES)
                                                                  (EVAL FILE-TYPE)))
                   CREATION-DATE)
                (RETURN T)))
         (RETURN T))))

#|                                      ;This is probably a bad idea
;;; This generates an special compound condition
(DEFINE-DEFSYSTEM-SPECIAL-VARIABLE *GENERATED-CONDITION-COUNTER* 0)

(DEFUN GENERATE-INTERNAL-CONDITION (SEXP INPUT TRANSFORMATION-TYPE &AUX SYMBOL NARGS FUNCTION)
  (SETQ SYMBOL (INTERN (FORMAT NIL "~A-TRANSFORMATION-INTERNAL-~D"
                               (STRING-UPCASE (SYSTEM-NAME *SYSTEM-BEING-DEFINED*))
                               (SETQ *GENERATED-CONDITION-COUNTER*
                                     (1+ *GENERATED-CONDITION-COUNTER*))))
        NARGS (+ (LENGTH (TRANSFORMATION-TYPE-INPUT-FILE-TYPES TRANSFORMATION-TYPE))
                 (LENGTH (TRANSFORMATION-TYPE-OUTPUT-FILE-TYPES TRANSFORMATION-TYPE))))
  (SETQ FUNCTION `(LAMBDA (&REST .INPUTS.)
                    (OR (= (LENGTH .INPUTS.) ,NARGS)
                        (CERROR T NIL :WRONG-NUMBER-OF-ARGUMENTS
                                "Function ~S given too many arguments (~D)"
                                SYMBOL (LENGTH .INPUTS.)))
                    . ,(SUBST '.INPUTS. INPUT SEXP)))
  (IF COMPILER:QC-FILE-IN-PROGRESS
      ;; This case if in QC-FILE or editor-compile
      (COMPILER:QC-TRANSLATE-FUNCTION
        SYMBOL FUNCTION 'COMPILER:MACRO-COMPILE
        (IF (NOT COMPILER:QC-FILE-LOAD-FLAG) 'COMPILER:QFASL 'COMPILER:COMPILE-TO-CORE))
      ;; This case if not doing anything special
      (LET ((FDEFINE-FILE-PATHNAME NIL)
            (INHIBIT-FDEFINE-WARNINGS T))
        (COMPILER:COMPILE SYMBOL FUNCTION)))
  SYMBOL)
 |#

;;; Add a transformation which isn't normally executed, but can be depended upon
(DEFMACRO (:SKIP DEFSYSTEM-MACRO) (&REST REST)
  (LET ((*ADD-TRANSFORMATION-TO-SYSTEM* :SKIP))
    (MULTIPLE-VALUE-BIND (VAL1 VAL2) (CALL-DEFSYSTEM-MACRO REST)
      (VALUES NIL VAL1 VAL2))))

(DEFINE-SIMPLE-TRANSFORMATION :READ-FOR-COMPILATION ReadForCompilation ReadForCompilation?
                              ("") ()
                              ("Read for compilation" "Reading for compilation"
                               "read for compilation")
                              NIL)

(DEFUN ReadForCompilation? (Name &AUX (Source (FUNCALL Name ':NEW-TYPE "LISP"))
                            (Binary (FUNCALL Name ':NEW-TYPE
                                             (or *SYSTEM-load-BINARY-FILE-TYPE*
                                                 *SYSTEM-default-BINARY-FILE-TYPE*))))
  (COND ((FILE-NEWER-THAN-FILE-P Source Binary)
         (FILE-NEWER-THAN-INSTALLED-P Source))
        (T (FILE-NEWER-THAN-INSTALLED-P Binary))))


(DEFUN ReadForCompilation (Name &AUX (Source (FUNCALL Name ':NEW-TYPE "LISP"))
                           (Binary (FUNCALL Name ':NEW-TYPE
                                            *SYSTEM-DEFAULT-BINARY-FILE-TYPE*)))
  (COND ((FILE-NEWER-THAN-FILE-P Source Binary)
         (READFILE-1 Source))
        (T (FASLOAD-1 Binary))))

(DEFMACRO (:PROPERTY :PROGN DEFSYSTEM-MACRO) (&REST Transformations &AUX Values)
  (DOLIST (Transformation Transformations)
    (SETQ Values (MULTIPLE-VALUE-LIST (CALL-DEFSYSTEM-MACRO Transformation))))
  (VALUES-LIST (LIST* NIL Values)))

(DEFMACRO (:PROPERTY :READ-COMPILE-LOAD DEFSYSTEM-MACRO)
          (Input &OPTIONAL Read-Dependencies Compile-Dependencies Load-Dependencies
           Read-Condition    Compile-Condition    Load-Condition)
  `(:PROGN (:SKIP :READ-FOR-COMPILATION ,Input ,Read-Dependencies ,Read-Condition)
           (:FASLOAD (:COMPILE ,Input ((:READ-FOR-COMPILATION ,Input) ,@Compile-Dependencies)
                               ,Compile-Condition)
                     ,Load-Dependencies ,Load-Condition)))



(DEFUN FASLOAD-1 (INFILE &optional defs-file)
  (SETQ *SOMETHING-LOADED* T)
  (FASLOAD (or defs-file INFILE) *FORCE-PACKAGE* T))

(DEFUN READFILE-1 (INFILE)
  (SETQ *SOMETHING-LOADED* T)
  (READFILE INFILE *FORCE-PACKAGE* T))

(DEFUN QC-FILE-1 (INFILE OUTFILE &optional defs-file)
  (multiple-value-bind (*system-default-binary-file-type* *system-load-binary-file-type* compiler-fun)
      (values-list (get compiler:*target-computer* 'make-system))
    (let* ((type *SYSTEM-DEFAULT-BINARY-FILE-TYPE*)
           (special-compiler (assq type *known-binary-file-producers*)))
      (if special-compiler
          (funcall (second special-compiler) infile outfile)
        ;; This was a MULTIPLE-VALUE-CALL, but the Lambda compiler screws up badly
        ;; on that!!! (I think the cross compiler wins though).
        (apply compiler-fun
          infile
          :output-file (send outfile :new-version nil)
          :package *force-package*
          (if defs-file
              (list :environment-pathname defs-file)
            (if (null *system-load-binary-file-type*)
                nil
              (list :environment-pathname
                    (send (pathname (or outfile infile))
                          :new-type *system-load-binary-file-type*)))))))
    (WHEN *WARNINGS-STREAM*
      (PRINT-FILE-WARNINGS INFILE *WARNINGS-STREAM*)
      (SEND-IF-HANDLES *WARNINGS-STREAM* :FORCE-OUTPUT)
      (SEND-IF-HANDLES *WARNINGS-STREAM* :FINISH))))

(DEFUN QC-FILE-cross-1 (INFILE OUTFILE &optional defs-file)
  (unless defs-file
    (setq defs-file (send outfile :new-pathname :type :fdef)))
  (let ((compiler:*target-computer* 'compiler:k)
        (*system-default-binary-file-type* :fbin)
        (*system-load-binary-file-type* :fdef)
        (compiler:*compilation-environment*
          (compiler:make-compilation-environment :target 'compiler:falcon
                                                 :next compiler:*falcon-environment*)))
    (qc-file-1 infile outfile defs-file)))

(DEFUN FASLOAD-falcon-environment-1 (INFILE &optional defs-file)
  (SETQ *SOMETHING-LOADED* T)
  (unless defs-file
    (setq defs-file (send infile :new-pathname :type :fdef)))
  (compiler:load-fdef-file defs-file))

(DEFUN LOAD-FONT-WIDTHS-1 (INFILE)
  (SETQ *SOMETHING-LOADED* T)
  (PKG-BIND *FORCE-PACKAGE*
    (if (fboundp 'press:load-font-widths)
        (PRESS:LOAD-FONT-WIDTHS INFILE NIL T))))

(DEFVAR MAKSYS-BREAKPOINT-FLAG NIL)

(DEFUN FILE-NEWER-THAN-INSTALLED-P (FILE &optional defs-file &AUX
                                    (LOADED-ID (GET-FILE-LOADED-ID FILE *FORCE-PACKAGE*)))
  (AND (NOT *JUST-ACCUMULATING-FILES*)
       MAKSYS-BREAKPOINT-FLAG
       LOADED-ID
       (NOT (EQUAL LOADED-ID (SYSTEM-GET-FILE-INFO (or defs-file FILE))))
       (ERROR "TESTING MAKSYS"))
  (IF LOADED-ID
      (NOT (EQUAL LOADED-ID (SYSTEM-GET-FILE-INFO (or defs-file FILE))))
    T))

;;; FILE-2 need not exist yet (it is assumed to be output from FILE-1 in some way).
(DEFUN FILE-NEWER-THAN-FILE-P (FILE-1 FILE-2 &optional defs)
  ;; We could look at both of them, but that would slow things down a lot.
  (let ((file-2 (or defs file-2)))
    (IF (PROBEF FILE-2)
        (> (SYSTEM-GET-CREATION-DATE FILE-1)
           (SYSTEM-GET-CREATION-DATE FILE-2 T))
      T)))

(DEFUN SYSTEM-GET-FILE-INFO (FILE)
  (LET ((PLIST (SYSTEM-GET-FILE-PROPERTY-LIST FILE)))
    (AND (CDR PLIST)
         (OR (GET PLIST :INFO)
             (CONS (GET PLIST :TRUENAME) (GET PLIST :CREATION-DATE))))))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *JUST-ACCUMULATING-FILES* NIL)

(DEFUN SYSTEM-GET-CREATION-DATE (FILE &OPTIONAL NO-ERROR-P)
  (LET ((PLIST (SYSTEM-GET-FILE-PROPERTY-LIST FILE)))
    (COND ((GET PLIST :CREATION-DATE))
          ((OR NO-ERROR-P *JUST-ACCUMULATING-FILES*) -1)
          (T (FERROR NIL "File ~A does not exist" FILE)))))

(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *INTERESTING-FILES* NIL)
(DEFINE-MAKE-SYSTEM-SPECIAL-VARIABLE *INTERESTING-FILES-INFO* NIL)

(DEFUN SYSTEM-GET-FILE-PROPERTY-LIST (FILE)
  (do ((info (assq file *interesting-files-info*) (assq file *interesting-files-info*))
       (first-time t (setq first-time nil)))
      (info info)

    ;;;$$$Plug hole in logic that caused "file not found" error before we ever looked!
    ;;;This may be the first we've taken an "interest" in this file.  <04-Nov-88 keith>
    (pushnew file *interesting-files*)

    (cond (*just-accumulating-files*
           (return nil)))

    (cond ((null first-time)
           (cerror "Look again for the file."
                   "File ~A not found." file)))

    ;;Found a file we didn't know about, accumulate a lot of info at once
    (ACCUMULATE-INTERESTING-FILES *FILE-TRANSFORMATION-LIST* '(:PROBABLY))
    (ACCUMULATE-INTERESTING-FILES *ADDED-FILE-TRANSFORMATIONS* '(NIL :PROBABLY))

    (let ((plists (FS:MULTIPLE-FILE-PLISTS *INTERESTING-FILES*))
          still-interesting-files)
      ;;remember any files that weren't found
      (setq plists (loop for plist in plists
                         when (null (cdr plist))
                         do (push (car plist) still-interesting-files)
                         else collect plist))
      (SETQ *INTERESTING-FILES-INFO* (NCONC plists *INTERESTING-FILES-INFO*)
            *INTERESTING-FILES* still-interesting-files))
    ))

(DEFUN ACCUMULATE-INTERESTING-FILES (LIST STATES)
  (LET ((*JUST-ACCUMULATING-FILES* T))
    (DOLIST (FILE-TRANSFORMATION LIST)
      (AND (MEMQ (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) STATES)
           (LET ((*FORCE-PACKAGE* (FILE-TRANSFORMATION-FORCE-PACKAGE FILE-TRANSFORMATION)))
             (APPLY (FILE-TRANSFORMATION-CONDITION-FUNCTION FILE-TRANSFORMATION)
                    (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION)))))))

(DEFUN INVALIDATE-PATHNAME-INFO (FILE)
  (SETQ *INTERESTING-FILES-INFO*
        (DEL-IF #'(LAMBDA (X) (EQ (CAR X) FILE)) *INTERESTING-FILES-INFO*)))

(DEFVAR QUERY-USER-SELECTIVE-OPTIONS '(:CHOICES (((:YES "Yes.") #/Y #\SP)
                                                 ((:NO "No.") #/N #\RUBOUT)
                                                 ((:DIRECTORY "Directory.") #/D)
                                                 ((:EDIT "Edit.") #/E)
                                                 ((:SRCCOM "Srccom.") #/S))))
(DEFUN QUERY-USER-SELECTIVE (FILE-TRANSFORMATION)
  (IF (NOT (EQ *QUERY-TYPE* :SELECTIVE)) T
      (DO () (NIL)
        (SELECTQ (FQUERY QUERY-USER-SELECTIVE-OPTIONS
                         "~&~:[~A ~;~*~]~\SI::FILE-XFORM-ARGS\? "
                         (NULL (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
                         (TRANSFORMATION-TYPE-PRETTY-IMPERATIVE
                           (FILE-TRANSFORMATION-TRANSFORMATION-TYPE FILE-TRANSFORMATION))
                         FILE-TRANSFORMATION)
          (:YES (RETURN T))
          (:NO (RETURN NIL))
          (:DIRECTORY
           (PRINT-FILE-TRANSFORMATION-DIRECTORY FILE-TRANSFORMATION))
          (:EDIT
           (ED (FIRST (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))))
          (:SRCCOM
           (LET ((DIRECTORY-LIST (PRINT-FILE-TRANSFORMATION-DIRECTORY FILE-TRANSFORMATION)))
             (MULTIPLE-VALUE-BIND (FILE-1 FILE-2)
                 (FUNCALL (OR (GET (FILE-TRANSFORMATION-CONDITION-FUNCTION
                                     FILE-TRANSFORMATION)
                                   'FILE-TRANSFORMATION-SRCCOM-FILES-FUNCTION)
                              'DEFAULT-FILE-TRANSFORMATION-SRCCOM-FILES-FUNCTION)
                          FILE-TRANSFORMATION DIRECTORY-LIST)
               (SRCCOM:PROMPTED-SOURCE-COMPARE FILE-1 FILE-2))))))))

(DEFUN PRINT-FILE-TRANSFORMATION-DIRECTORY (FILE-TRANSFORMATION &AUX DIRECTORY-LIST)
  (DOLIST (FILE (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
    (SETQ FILE (SEND FILE :GENERIC-PATHNAME))
    (OR (ASSQ FILE DIRECTORY-LIST)
        (PUSH (PRINT-FILE-TRANSFORMATION-DIRECTORY-1 FILE) DIRECTORY-LIST)))
  DIRECTORY-LIST)

(DEFUN PRINT-FILE-TRANSFORMATION-DIRECTORY-1 (FILE &AUX LIST)
  (SEND *STANDARD-OUTPUT* :FRESH-LINE)
  (SETQ LIST (FS:DIRECTORY-LIST (SEND FILE :NEW-PATHNAME :TYPE :WILD
                                                          :VERSION :WILD)
                                :SORTED))
  (SETQ LIST (DELQ (ASSQ NIL LIST) LIST))
  (DOLIST (FILE LIST)
    (FORMAT T "~&~A~15T~D ~D(~D)~30T"
            (SEND (CAR FILE) :STRING-FOR-DIRED) (GET FILE :LENGTH-IN-BLOCKS)
            (GET FILE :LENGTH-IN-BYTES) (GET FILE :BYTE-SIZE))
    (TIME:PRINT-UNIVERSAL-TIME (GET FILE :CREATION-DATE))
    (FORMAT T "~@[ ~A~]~%" (GET FILE :AUTHOR)))
  (CONS FILE LIST))

(DEFUN DEFAULT-FILE-TRANSFORMATION-SRCCOM-FILES-FUNCTION (FILE-TRANSFORMATION DIRECTORY-LIST)
  DIRECTORY-LIST                                ;Not used in this simple-minded case
  (LET ((FILE (FIRST (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))))
    (VALUES (SEND FILE :NEW-VERSION :OLDEST)
            (SEND FILE :NEW-VERSION :NEWEST))))

(DEFUN (FILE-NEWER-THAN-FILE-P FILE-TRANSFORMATION-SRCCOM-FILES-FUNCTION)
       (FILE-TRANSFORMATION DIRECTORY-LIST)
  (SRCCOM-FILES-FUNCTION-NEWEST-FILE-OLDER-THAN-DATE
    FILE-TRANSFORMATION DIRECTORY-LIST
    (SYSTEM-GET-CREATION-DATE (SECOND (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION)))))

(DEFUN (FILE-NEWER-THAN-INSTALLED-P FILE-TRANSFORMATION-SRCCOM-FILES-FUNCTION)
       (FILE-TRANSFORMATION DIRECTORY-LIST)
  (SRCCOM-FILES-FUNCTION-NEWEST-FILE-OLDER-THAN-DATE
    FILE-TRANSFORMATION DIRECTORY-LIST
    (OR (CDR (GET-FILE-LOADED-ID
                (FIRST (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION)) *FORCE-PACKAGE*))
        -1)))   ;File was never loaded

(DEFUN SRCCOM-FILES-FUNCTION-NEWEST-FILE-OLDER-THAN-DATE (FILE-TRANSFORMATION DIRECTORY-LIST
                                                          DATE)
  (LET* ((INPUT (FIRST (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION)))
         (GENERIC-PATHNAME (SEND INPUT :GENERIC-PATHNAME))
         (LIST (CDR (ASSQ GENERIC-PATHNAME DIRECTORY-LIST)))
         (PATHNAME (SEND INPUT :NEW-VERSION :OLDEST)))
    (DOLIST (FILE LIST)
      (LET ((FILENAME (CAR FILE)))
        (AND (MEMBER (SEND FILENAME :CANONICAL-TYPE) '(:LISP NIL :UNSPECIFIC))
             (< (GET FILE :CREATION-DATE) DATE)
             (SETQ PATHNAME FILENAME))))
    (AND (NOT (MEMQ (SEND INPUT :CANONICAL-TYPE) '(:LISP NIL :UNSPECIFIC)))
         (MEMQ (SEND (CAAR LIST) :CANONICAL-TYPE) '(:LISP NIL :UNSPECIFIC))
         (SETQ INPUT (CAAR LIST)))
    (VALUES PATHNAME
            (SEND INPUT :NEW-VERSION :NEWEST))))

;;; For things like M-X Select System as Tags Table
(DEFUN ALL-SYSTEMS-NAME-ALIST ()
  "Return an alist of all system names and system objects."
  (LOOP FOR SYSTEM IN *SYSTEMS-LIST*
        NCONC (CONS (CONS (STRING (IF (TYPEP SYSTEM 'SYSTEM) (SYSTEM-NAME SYSTEM) SYSTEM))
                          SYSTEM)
                    (MAPCAR #'(LAMBDA (NICKNAME) (CONS NICKNAME SYSTEM))
                            (IF (TYPEP SYSTEM 'SYSTEM)
                                (SYSTEM-NICKNAMES SYSTEM))))))

(DEFVAR *SOURCE-FILE-TYPES* '(:LISP))
(DEFUN SYSTEM-SOURCE-FILES (SYSTEM &OPTIONAL (TYPES *SOURCE-FILE-TYPES*) INTERMEDIATE-TOO
                            (INCLUDE-SUBSYSTEMS T))
  "Return the list of all source file pathnames of SYSTEM which have types in TYPES.
TYPES defaults to SI:*SOURCE-FILE-TYPES*, initially (:LISP).
TYPES can also be :ALL, meaning don't filter by filetype.
INTERMEDIATE-TOO says whether to count files produced from others
 but then used as sources to make yet more files.
INCLUDE-SUBSYSTEMS (default T) says whether to include source files
 of subsystems of this one."
  (SI:ELIMINATE-DUPLICATES (SYSTEM-SOURCE-FILES-1 SYSTEM TYPES INTERMEDIATE-TOO
                                                  INCLUDE-SUBSYSTEMS)))

(DEFUN SYSTEM-SOURCE-FILES-1 (SYSTEM TYPES INTERMEDIATE-TOO INCLUDE-SUBSYSTEMS
                              &AUX *SYSTEM-DEFAULT-BINARY-FILE-TYPE*
                              *SYSTEM-LOAD-BINARY-FILE-TYPE*)
  (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM))
  (SETQ *SYSTEM-DEFAULT-BINARY-FILE-TYPE* (system-default-binary-file-type system))
  (SETQ *SYSTEM-LOAD-BINARY-FILE-TYPE* (system-default-binary-file-type system))
  (NCONC (LET ((SYMBOL (SYSTEM-SYMBOLIC-NAME SYSTEM)))
           (AND SYMBOL
                (LET ((FILE (GET-SOURCE-FILE-NAME SYMBOL 'DEFSYSTEM)))
                  (AND FILE
                       (LET ((DEFINING-SYSTEM (SEND FILE :GET 'MAYBE-RELOAD-SYSTEM)))
                         (AND DEFINING-SYSTEM
                              (SYSTEM-SOURCE-FILES-1 DEFINING-SYSTEM TYPES INTERMEDIATE-TOO
                                                     INCLUDE-SUBSYSTEMS)))
                       ))))
         ;; First get inputs that come from files in modules
         ;; We get them from transformations, but the order we consider
         ;; the transformations is the order the modules were specified.
         (LET ((*FORCE-PACKAGE* (SYSTEM-PACKAGE-DEFAULT SYSTEM)))
           (LOOP FOR MODULE IN (REVERSE (SYSTEM-MODULES SYSTEM))
                 NCONC (LOOP FOR TRANSFORMATION IN (SYSTEM-TRANSFORMATIONS SYSTEM)
                             WHEN (EQ (TRANSFORMATION-INPUT TRANSFORMATION) MODULE)
                             NCONC (TRANSFORMATION-SOURCE-FILES TRANSFORMATION TYPES NIL))))
         ;; Now get intermediate source files if wanted.
         ;; Those are files that are "sources" for some transformations
         ;; but are produced by others rather than specified in modules.
         (AND INTERMEDIATE-TOO
              (LET ((*FORCE-PACKAGE* (SYSTEM-PACKAGE-DEFAULT SYSTEM)))
                (LOOP FOR TRANSFORMATION IN (SYSTEM-TRANSFORMATIONS SYSTEM)
                      WHEN (NOT (TYPEP (TRANSFORMATION-INPUT TRANSFORMATION) 'MODULE))
                      NCONC (TRANSFORMATION-SOURCE-FILES TRANSFORMATION TYPES T))))
         (AND INCLUDE-SUBSYSTEMS
              (LOOP FOR SUBSYS IN (SYSTEM-COMPONENT-SYSTEMS SYSTEM)
                    NCONC (SYSTEM-SOURCE-FILES-1 SUBSYS TYPES INTERMEDIATE-TOO
                                                 INCLUDE-SUBSYSTEMS)))))

(DEFUN TRANSFORMATION-SOURCE-FILES (TRANSFORMATION TYPES INTERMEDIATE-TOO)
  (LET* ((INPUT (TRANSFORMATION-INPUT TRANSFORMATION))
         (TRANSFORMATION-TYPE (TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION))
         (FILE-TYPES (TRANSFORMATION-TYPE-INPUT-FILE-TYPES TRANSFORMATION-TYPE))
         (*FORCE-PACKAGE* NIL) (*TRANSFORMATION-OUTPUTS* NIL))
    (AND FILE-TYPES (OR INTERMEDIATE-TOO (TYPEP INPUT 'MODULE))
         (LET ((PATHNAME-LIST (SELECTQ (TYPE-OF INPUT)
                                (MODULE (GET-MODULE-PATHNAMES INPUT))
                                (TRANSFORMATION (GET-TRANSFORMATION-PATHNAMES INPUT))
                                (OTHERWISE
                                 (FERROR NIL "~S is not a valid transformation input"
                                         INPUT)))))
           (LOOP FOR PATHNAME IN PATHNAME-LIST
                 AS PKG = (POP PATHNAME)
                 NCONC (LOOP FOR FILE-TYPE IN FILE-TYPES
                             AS PATH = (POP-CAREFULLY PATHNAME)
                             AS REAL-TYPE = (EVAL FILE-TYPE)
                             WHEN (OR (EQ TYPES :ALL) (MEMBER REAL-TYPE TYPES))
                             COLLECT (MERGE-PATHNAME-TYPE PATH REAL-TYPE)))))))

;;; Automatical system declaration hackery
(DEFUN SET-SYSTEM-SOURCE-FILE (SYSTEM-NAME SOURCE-FILE)
  "Record what file contains the DEFSYSTEM for SYSTEM-NAME.
If a MAKE-SYSTEM is done on that system, the source file will be loaded automatically."
  (IF (STRINGP SYSTEM-NAME)
      (SETQ SYSTEM-NAME (INTERN (STRING-UPCASE SYSTEM-NAME) PKG-KEYWORD-PACKAGE)))
  (LET ((FDEFINE-FILE-PATHNAME (SEND (FS:MERGE-PATHNAME-DEFAULTS SOURCE-FILE)
                                     :GENERIC-PATHNAME)))
    (RECORD-SOURCE-FILE-NAME SYSTEM-NAME 'DEFSYSTEM)
    (OR (FIND-SYSTEM-NAMED SYSTEM-NAME T T)
        (ADD-SYSTEM SYSTEM-NAME))))

;;; This is really a dummy, since handled at a higher level
(DEFUN (:NO-RELOAD-SYSTEM-DECLARATION MAKE-SYSTEM-KEYWORD) ()
  NIL)

(DEFUN MAYBE-RELOAD-SYSTEM-DECLARATION (SYSTEM-NAME KEYWORDS &AUX FILE)
  ;; If we can, ignore package problems
  (LET ((SYSTEM (FIND-SYSTEM-NAMED SYSTEM-NAME T T)))
    (AND SYSTEM (SETQ SYSTEM-NAME (SYSTEM-SYMBOLIC-NAME SYSTEM))))
  (AND (NOT (MEMQ :NO-RELOAD-SYSTEM-DECLARATION KEYWORDS))
       (NOT (STRINGP SYSTEM-NAME))    ;PREVENT BOMBING OUT, THIS SHOULD BE FIXED BETTER
       (SETQ FILE (GET-SOURCE-FILE-NAME SYSTEM-NAME 'DEFSYSTEM))
       ;; To keep from blowing out, disable this whole feature if the FN2 isn't >.
       (EQ (SEND FILE :TYPE) :UNSPECIFIC)
       ;; Also keep from losing when the same file has the defsystem and the make-system in it
       (NEQ FILE FDEFINE-FILE-PATHNAME)
       (let ((*standard-output* *query-io*))
         (MAYBE-RELOAD-FILE FILE KEYWORDS))))

(DEFUN MAYBE-RELOAD-FILE (FILE KEYWORDS &AUX SYSTEM)
  (OR (SETQ SYSTEM (SEND FILE :GET 'MAYBE-RELOAD-SYSTEM))
      (LET* ((LISP (SEND FILE :SOURCE-PATHNAME))
             (QFASL (SEND FILE :NEW-PATHNAME :TYPE :QFASL :VERSION :NEWEST))
             ;; Compiled if qfasl ever loaded, else interpreted if lisp ever loaded,
             ;; else check file computer and default to interpreted.
        ;** this doesnt really win with new GENERIC-PATHNAME scheme.
        ; should be fixed by adding mode of loading to :FILE-ID-PACKAGE-ALIST element,
        ; I guess.  Seems pretty random anyway.
             (COMPILED (COND ((SEND QFASL :GET :FILE-ID-PACKAGE-ALIST) T)
                             ((SEND LISP :GET :FILE-ID-PACKAGE-ALIST) NIL)
                             ((PROBEF QFASL) T)
                             (T NIL))))
        (SETQ SYSTEM (GENSYM))
        (DEFSYSTEM-1 SYSTEM `((,(IF COMPILED :COMPILE-LOAD :READFILE)
                               (,(STRING FILE))))
                     T NIL)
        (SEND FILE :PUTPROP SYSTEM 'MAYBE-RELOAD-SYSTEM)))
  (APPLY 'MAKE-SYSTEM SYSTEM
         (NCONC (LOOP FOR KEY IN KEYWORDS
                      WHEN (memq KEY '(:BATCH :DEFAULTED-BATCH)) COLLECT :NOWARN
                      ELSE UNLESS (MEMQ KEY '(:NOLOAD :RELOAD :RECOMPILE :PRINT-ONLY))
                      COLLECT KEY)
                '(:COMPILE :NO-RELOAD-SYSTEM-DECLARATION))))

;;;; Patch system interface
(DEFMACRO SYSTEM-PATCH-DIRECTORY (SYSTEM)
  "Returns the specified patch directory of the system object SYSTEM."
  `(GETF (SYSTEM-PLIST ,SYSTEM) :PATCH-DIRECTORY))

(DEFMACRO SYSTEM-PATCHABLE-P (SYSTEM)
  "T if SYSTEM (a system object) is a patchable system."
  `(NOT (NULL (SYSTEM-PATCH-DIRECTORY ,SYSTEM))))

(DEFUN SYSTEM-PATCHABLE-SUPERSYSTEM (SYSTEM-NAME-OR-SYSTEM)
  "Return the patchable system which is a supersystem of SYSTEM-NAME-OR-SYSTEM.
SYSTEM is used as the value only as a last resort.
The value is a system object, not a name."
  (LET ((SYSTEM (FIND-SYSTEM-NAMED SYSTEM-NAME-OR-SYSTEM))
        (SYSTEM-SYSTEM (FIND-SYSTEM-NAMED 'SYSTEM)))
    ;;if we're a patchable system, just return us
    (IF (SYSTEM-PATCHABLE-P SYSTEM) SYSTEM
        (OR (DOLIST (SYS *SYSTEMS-LIST*)
              (LET ((MAYBE-SUPERIOR (FIND-SYSTEM-NAMED SYS T T)))
                ;;don't consider the SYSTEM system, because there may be a smaller
                ;;enclosing system.
                (IF (AND MAYBE-SUPERIOR
                         (NEQ MAYBE-SUPERIOR SYSTEM-SYSTEM)
                         (SYSTEM-SUBSYSTEM-P SYSTEM MAYBE-SUPERIOR))
                    ;;if we're a subsystem of this one, get it if it's patchable,
                    ;;or its patchable superior if not.
                    (LET ((PATCHABLE (SYSTEM-PATCHABLE-SUPERSYSTEM MAYBE-SUPERIOR)))
                      (IF (NEQ PATCHABLE SYSTEM-SYSTEM) (RETURN PATCHABLE))))))
            SYSTEM-SYSTEM))))

(DEFSTRUCT (PATCH-DIRECTORY :LIST :CONC-NAME (:ALTERANT NIL))
  PATHNAME
  SAME-DIRECTORY-P
  (:INITIAL-STATUS :EXPERIMENTAL)
  (PATCH-ATOM "PATCH"))

(DEFMACRO (:PATCHABLE DEFSYSTEM-MACRO) (&OPTIONAL DIRECTORY PATCH-ATOM
                                        &AUX DEFAULT PATCH-DIRECTORY)
  (SETQ DEFAULT (FS:DEFAULT-PATHNAME *SYSTEM-PATHNAME-DEFAULT*)
        DIRECTORY (IF DIRECTORY
                      (FS:MERGE-PATHNAME-DEFAULTS DIRECTORY *SYSTEM-PATHNAME-DEFAULT*)
                    (OR DEFAULT
                        (FERROR NIL "Patch directory pathname not specified in patchable DEFSYSTEM"))))
  (SETQ DIRECTORY (SEND DIRECTORY :NEW-PATHNAME :NAME :UNSPECIFIC
                                                 :TYPE :UNSPECIFIC
                                                 :VERSION :UNSPECIFIC))
  (SETQ PATCH-DIRECTORY (MAKE-PATCH-DIRECTORY
                          PATHNAME DIRECTORY
                          SAME-DIRECTORY-P
                           (AND *SYSTEM-PATHNAME-DEFAULT-SPECIFIED*
                                (OR PATCH-ATOM
                                    (AND (EQUAL (SEND DIRECTORY :HOST)
                                                (SEND DEFAULT :HOST))
                                         (EQUAL (SEND DIRECTORY :DEVICE)
                                                (SEND DEFAULT :DEVICE))
                                         (EQUAL (SEND DIRECTORY :DIRECTORY)
                                                (SEND DEFAULT :DIRECTORY)))))))
  (AND PATCH-ATOM (SETF (PATCH-DIRECTORY-PATCH-ATOM PATCH-DIRECTORY) PATCH-ATOM))
  (SETF (SYSTEM-PATCH-DIRECTORY *SYSTEM-BEING-DEFINED*) PATCH-DIRECTORY)
  NIL)

(DEFUN PATCH-SYSTEM-PATHNAME (NAME TYPE &REST ARGS &AUX PATCH-DIRECTORY)
  "Return the pathname to use for the system NAME, for purpose TYPE.
TYPE can be :SYSTEM-DIRECTORY, with no args,
or :VERSION-DIRECTORY, with the major version number as arg,
or :PATCH-FILE, with major and minor version numbers and filetype as three args."
  (OR (SETQ PATCH-DIRECTORY (SYSTEM-PATCH-DIRECTORY (FIND-SYSTEM-NAMED NAME)))
      (FERROR NIL "System ~A not patchable" NAME))
  (LEXPR-SEND (PATCH-DIRECTORY-PATHNAME PATCH-DIRECTORY)
              :PATCH-FILE-PATHNAME (STRING-UPCASE NAME)
              (PATCH-DIRECTORY-SAME-DIRECTORY-P PATCH-DIRECTORY)
              (PATCH-DIRECTORY-PATCH-ATOM PATCH-DIRECTORY)
              TYPE ARGS))

(DEFMACRO (PATCHABLE-INTERNAL DEFSYSTEM-MACRO) (&OPTIONAL COMDEP)
  `(INCREMENT-LOADED-VERSION (INCREMENT-COMPILED-VERSION NIL ,COMDEP)))

(DEFMACRO (:INITIAL-STATUS DEFSYSTEM-MACRO) (STATUS &AUX PATCH-DIRECTORY)
  (OR (SETQ PATCH-DIRECTORY (SYSTEM-PATCH-DIRECTORY *SYSTEM-BEING-DEFINED*))
      (FERROR NIL "~S not patchable" *SYSTEM-BEING-DEFINED*))
  (SETF (PATCH-DIRECTORY-INITIAL-STATUS PATCH-DIRECTORY) STATUS)
  NIL)

(DEFINE-SIMPLE-TRANSFORMATION INCREMENT-LOADED-VERSION INCREMENT-LOADED-VERSION-1
                              PATCH-VERSION-NEWER-THAN-LOADED
                              NIL NIL
                              ("Make ~A patchable" "Making ~A patchable" "~A made patchable")
                              NIL)

(DEFUN INCREMENT-LOADED-VERSION-1 (&AUX NAME VERSION STATUS)
  (SETQ NAME (STRING (SYSTEM-NAME *SYSTEM-BEING-MADE*)))
  (MULTIPLE-VALUE (VERSION STATUS)
    (ADD-PATCH-SYSTEM NAME))
  (SETQ STATUS (CADR (ASSQ STATUS SYSTEM-STATUS-ALIST)))
  (COND ((NOT *SILENT-P*)
         (FORMAT T "~&~A~:[ ~]~A version ~D. loaded~%"
                 STATUS (ZEROP (ARRAY-ACTIVE-LENGTH STATUS)) NAME VERSION)))
  (WHEN *LOAD-PATCHES*
   (IF *SILENT-P*
       (LOAD-PATCHES :SYSTEMS (LIST (SYSTEM-NAME *SYSTEM-BEING-MADE*))
                     :SILENT)
     (IF (EQ *QUERY-TYPE* :SELECTIVE)
         (LOAD-PATCHES :SYSTEMS (LIST (SYSTEM-NAME *SYSTEM-BEING-MADE*)))
       (LOAD-PATCHES :SYSTEMS (LIST (SYSTEM-NAME *SYSTEM-BEING-MADE*))
                     :NOSELECTIVE)))))

(DEFUN PATCH-VERSION-NEWER-THAN-LOADED (&AUX NAME)
  (SETQ NAME (SYSTEM-NAME *SYSTEM-BEING-MADE*))
  (NEQ (GET-PATCH-SYSTEM-MAJOR-VERSION NAME)
       (GET-SYSTEM-VERSION NAME)))

(DEFINE-SIMPLE-TRANSFORMATION INCREMENT-COMPILED-VERSION INCREMENT-COMPILED-VERSION-1
                              TRUE NIL NIL
                              ("Increment ~A patch version"
                               "Incrementing ~A patch version"
                               "~A patch version incremented")
                              T)

(DEFUN INCREMENT-COMPILED-VERSION-1 (&AUX NAME VERSION)
  (SETQ NAME (STRING (SYSTEM-NAME *SYSTEM-BEING-MADE*))
        VERSION (INCREMENT-PATCH-SYSTEM-MAJOR-VERSION
                  (SYSTEM-NAME *SYSTEM-BEING-MADE*)
                  (PATCH-DIRECTORY-INITIAL-STATUS
                    (SYSTEM-PATCH-DIRECTORY *SYSTEM-BEING-MADE*))))
  (OR *SILENT-P*
      (FORMAT T "~&~A version ~D. created~%" NAME VERSION)))


(DEFMACRO (:NOT-IN-DISK-LABEL DEFSYSTEM-MACRO) ()
  (SETF (GETF (SYSTEM-PLIST *SYSTEM-BEING-DEFINED*) ':NOT-IN-DISK-LABEL) T)
  NIL)

(DEFUN SYSTEM-SHOULD-NOT-APPEAR-IN-DISK-LABEL (SYSTEM)
  (GETF (SYSTEM-PLIST (FIND-SYSTEM-NAMED SYSTEM)) ':NOT-IN-DISK-LABEL))

;;; $$$ New keyword for declaring systems to be CTS-controlled <17-Nov-88 smh>
(DEFMACRO (:CTS-Controlled DEFSYSTEM-MACRO) ()
  (SETF (GETF (SYSTEM-PLIST *SYSTEM-BEING-DEFINED*) ':CTS-CONTROLLED) T)
  NIL)

;;; Some compatibility functions with the old stuff
(DEFUN (:NOOP MAKE-SYSTEM-KEYWORD) ())

(DEFUN COMPILE-FILE-ALIST (FILE-ALIST &OPTIONAL (DONT-ASK-P 0) (DONT-CARE-IF-UNCHANGED-P 0)
                             DONT-ASK-FOR-CONFIRMATION PACKAGE-SPEC &AUX SYSTEM)
  "Compile all the files in ALIST that aren't compiled or have changed.
Each element of ALIST is a list whose car is a filename string.
DONT-ASK-INDIVIDUALY says whether to refrain from asking about each file.
DONT-CARE-IF-LOADED-P says whether to recompile unchanged files.
DONT-ASK-FOR-CONFIRMATION says whether to refrain from asking for one final confirmation.
PACKAGE-SPEC is the package to load into."
  (SETQ SYSTEM (FIND-SYSTEM-FROM-FILE-LIST FILE-ALIST PACKAGE-SPEC NIL T))
  (AND (NUMBERP DONT-ASK-P)                             ;If not specified,
       (SETQ DONT-ASK-P (NOT (Y-OR-N-P "Should I ask you about each file? "))))
  (AND (NUMBERP DONT-CARE-IF-UNCHANGED-P)
       (SETQ DONT-CARE-IF-UNCHANGED-P
             (Y-OR-N-P "Should I compile even if the file is unchanged? ")))
  (MAKE-SYSTEM SYSTEM :COMPILE :NOLOAD
                      (COND ((NOT DONT-ASK-P) :SELECTIVE)
                            (DONT-ASK-FOR-CONFIRMATION :NOCONFIRM)
                            (T :NOOP))
                      (IF DONT-CARE-IF-UNCHANGED-P :RELOAD :NOOP)))

(DEFUN LOAD-FILE-ALIST (ALIST &OPTIONAL (DONT-ASK-INDIVIDUALLY 0) (DONT-CARE-IF-LOADED-P 0)
                          DONT-ASK-FOR-CONFIRMATION PKG &AUX SYSTEM)
  "Load all the files in ALIST that aren't loaded or have changed.
Each element of ALIST is a list whose car is a filename string.
DONT-ASK-INDIVIDUALY says whether to refrain from asking about each file.
DONT-CARE-IF-LOADED-P says whether to reload unchanged files.
DONT-ASK-FOR-CONFIRMATION says whether to refrain from asking for one final confirmation.
PKG is the package to load into."
  (SETQ SYSTEM (FIND-SYSTEM-FROM-FILE-LIST ALIST PKG NIL T))
  (AND (NUMBERP DONT-ASK-INDIVIDUALLY)
       (SETQ DONT-ASK-INDIVIDUALLY (NOT (Y-OR-N-P "Should I ask you about each file? "))))
  (AND (NUMBERP DONT-CARE-IF-LOADED-P)
       (SETQ DONT-CARE-IF-LOADED-P (Y-OR-N-P "Should I load even if the file is loaded? ")))
  (MAKE-SYSTEM SYSTEM (COND ((NOT DONT-ASK-INDIVIDUALLY) :SELECTIVE)
                            (DONT-ASK-FOR-CONFIRMATION :NOCONFIRM)
                            (T :NOOP))
                      (IF DONT-CARE-IF-LOADED-P :RELOAD :NOOP)))

(DEFUN LOAD-FILE-LIST (FILE-LIST &OPTIONAL KEYLIST &AUX SYSTEM)
  "Load all the files in FILE-LIST that aren't loaded or have changed.
Each element of FILE-LIST is a list containing a filename string.
KEYLIST is a MAKE-SYSTEM keyword or a list of such, or NIL for none."
  (AND KEYLIST (NLISTP KEYLIST)
       (SETQ KEYLIST (LIST KEYLIST)))
  (SETQ SYSTEM (FIND-SYSTEM-FROM-FILE-LIST FILE-LIST))
  (APPLY #'MAKE-SYSTEM SYSTEM KEYLIST))

(DEFVAR *FILE-LIST-SYSTEM-ALIST* NIL)

(DEFUN FIND-SYSTEM-FROM-FILE-LIST (FILE-LIST &OPTIONAL PACKAGE-SPEC NAME ALIST-P)
  (OR (DO L *FILE-LIST-SYSTEM-ALIST* (CDR L) (NULL L)
          (AND (EQUAL (CDAR L) FILE-LIST) (RETURN (CAAR L))))
      (MAKE-SYSTEM-FROM-FILE-LIST FILE-LIST PACKAGE-SPEC (OR NAME (GENSYM)) ALIST-P)))

(DEFUN MAKE-SYSTEM-FROM-FILE-LIST (FILE-LIST PACKAGE-SPEC NAME ALIST-P &AUX OPTIONS)
  (AND PACKAGE-SPEC
       (PUSH `(:PACKAGE ,PACKAGE-SPEC) OPTIONS))
  (DO ((FILES FILE-LIST (CDR FILES))
       (FILE-ELEM) (FILE) (TYPE) (MODULE-NAME)
       (LAST-DEFS-TYPE) (LAST-DEFS-MODULE) (LAST-REGULAR-TYPE) (LAST-REGULAR-MODULE)
       (N-DEFS 0) (N-REGULAR 0) (COMPILED-DEFS NIL) (INTERPRETED-DEFS NIL)
       (DEFS-MODULES NIL) (DEFS-TRANSFORMATIONS NIL)
       (REGULAR-MODULES NIL) (REGULAR-TRANSFORMATIONS NIL))
      ((NULL FILES)
       (AND (OR COMPILED-DEFS INTERPRETED-DEFS)
            (LET* ((TEM1 (AND COMPILED-DEFS `(:FASLOAD . ,(NREVERSE COMPILED-DEFS))))
                   (TEM2 (AND INTERPRETED-DEFS `(:READFILE . ,(NREVERSE INTERPRETED-DEFS))))
                   (TEM (COND ((AND TEM1 TEM2) (LIST TEM1 TEM2))
                              (TEM1)
                              (TEM2))))
              (DOLIST (XFORM REGULAR-TRANSFORMATIONS)
                (AND (EQ (CAR XFORM) :COMPILE-LOAD)
                     (NCONC XFORM (NCONS TEM))))))
       (SETQ OPTIONS (NCONC REGULAR-TRANSFORMATIONS DEFS-TRANSFORMATIONS
                            REGULAR-MODULES DEFS-MODULES
                            OPTIONS)))
    (SETQ FILE-ELEM (CAR FILES)
          FILE (FS:MERGE-PATHNAME-DEFAULTS (CAR FILE-ELEM) FS:LOAD-PATHNAME-DEFAULTS :LISP))
    (SETQ TYPE (COND ((NOT (EQ (SEND FILE :CANONICAL-TYPE) :QFASL)) :READFILE)
                     ((AND (NOT ALIST-P)
                           (OR (MEM #'STRING-EQUAL 'NO-SOURCE-FILE (CDR FILE-ELEM))
                               (STRING-EQUAL NAME "FONTS")) :FASLOAD))

                     (T :COMPILE-LOAD)))
    (SETQ FILE (SEND FILE :NEW-PATHNAME :TYPE NIL :VERSION NIL))
    (IF (AND (NOT ALIST-P) (MEM #'STRING-EQUAL 'DEFS (CDR FILE-ELEM)))  ;A DEFS file
        (IF (EQ LAST-DEFS-TYPE TYPE)
            (SETF (THIRD LAST-DEFS-MODULE) (NCONC (THIRD LAST-DEFS-MODULE) (NCONS FILE)))
            (SETQ LAST-DEFS-TYPE TYPE)
            (SETQ MODULE-NAME (INTERN (FORMAT NIL "DEFS-~D" (SETQ N-DEFS (1+ N-DEFS)))))
            (SETQ LAST-DEFS-MODULE `(:MODULE ,MODULE-NAME (,FILE)))
            (PUSH LAST-DEFS-MODULE DEFS-MODULES)
            (PUSH `(,TYPE ,MODULE-NAME) DEFS-TRANSFORMATIONS)
            (IF (EQ TYPE :READFILE)
                (PUSH MODULE-NAME INTERPRETED-DEFS)
                (PUSH MODULE-NAME COMPILED-DEFS)))
        (IF (EQ LAST-REGULAR-TYPE TYPE)
            (SETF (THIRD LAST-REGULAR-MODULE) (NCONC (THIRD LAST-REGULAR-MODULE)
                                                     (NCONS FILE)))
            (SETQ LAST-REGULAR-TYPE TYPE)
            (SETQ MODULE-NAME (INTERN (FORMAT NIL "REGULAR-~D"
                                              (SETQ N-REGULAR (1+ N-REGULAR)))))
            (SETQ LAST-REGULAR-MODULE `(:MODULE ,MODULE-NAME (,FILE)))
            (PUSH LAST-REGULAR-MODULE REGULAR-MODULES)
            (PUSH `(,TYPE ,MODULE-NAME) REGULAR-TRANSFORMATIONS))))
  (PUSH (CONS NAME FILE-LIST) *FILE-LIST-SYSTEM-ALIST*)
  (LET ((FDEFINE-FILE-PATHNAME NIL))            ;Prevent MAKE-SYSTEM trying to load bogus file
    (DEFSYSTEM-1 NAME (NREVERSE OPTIONS))))

(DEFINE-SIMPLE-TRANSFORMATION :GENERATE-HOST-TABLE NET:GENERATE-FROM-HOSTS2-TABLE-1
  FILE-NEWER-THAN-FILE-P (:TEXT) (:LISP) ("Generate host table from"
                                            "Generating host table from"
                                            "generated into host table"))

(DEFINE-SIMPLE-TRANSFORMATION :LOAD-FONTS-WIDTHS LOAD-FONT-WIDTHS-1
  FILE-NEWER-THAN-INSTALLED-P (:WIDTHS) NIL ("Load Fonts Widths from"
                                              "Loading Fonts Widths from"
                                              "loaded for fonts widths") NIL)

(DEFINE-SIMPLE-TRANSFORMATION LOAD-SITE-FILE LOAD-SITE-FILE-1
  FILE-NEWER-THAN-INSTALLED-P (:QFASL) NIL
  ("Load site files because of changes in"
   "Loading site files because of"
   "loaded") NIL)

(DEFUN LOAD-SITE-FILE-1 (&REST IGNORE)
  (SETQ *SOMETHING-LOADED* T)
  (PUSH* '(UPDATE-SITE-CONFIGURATION-INFO) *MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*))

(DEFUN DESCRIBE-SYSTEM (SYSTEM-NAME &KEY (SHOW-FILES T) (SHOW-TRANSFORMATIONS T) &AUX SYSTEM)
  "Print all about the system named SYSTEM-NAME.
SHOW-FILES is T to give the history of each file in the system, NIL not to,
 or :ASK meaning query the user whether to.
SHOW-TRANSFORMATIONS is similar, for whether to show the transformations
 which MAKE-SYSTEM would execute.
Note that calling DESCRIBE on a system-object prints somewhat lower level information."
  (IF (NULL (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM-NAME)))
      (FORMAT T "~&There is no system named ~A.~%" SYSTEM-NAME)
    (SETQ SYSTEM-NAME (SYSTEM-NAME SYSTEM))
    (LET* ((SYSTEM-SOURCE-FILE
             (GET-SOURCE-FILE-NAME (SYSTEM-SYMBOLIC-NAME SYSTEM) 'DEFSYSTEM))
           (*FORCE-PACKAGE*
             (PKG-FIND-PACKAGE (OR (AND SYSTEM-SOURCE-FILE (GET SYSTEM-SOURCE-FILE ':PACKAGE))
                                   "USER"))))
      (WHEN SYSTEM-SOURCE-FILE
        (FORMAT T "~&System ~A~@[ is defined in file ~A~]~%"
                SYSTEM-NAME SYSTEM-SOURCE-FILE)
        (DESCRIBE-FILE-TRANSFORMATION-COMPILED-FILE SYSTEM-SOURCE-FILE)
        (DESCRIBE-FILE-TRANSFORMATION-LOADED-FILE SYSTEM-SOURCE-FILE)))
    (COND ((SYSTEM-PATCHABLE-P SYSTEM)
           (FORMAT T "~&~%~A is patchable" SYSTEM-NAME)
           (MULTIPLE-VALUE-BIND (MAJOR MINOR STATUS)
               (GET-SYSTEM-VERSION SYSTEM)
             (LET ((STATUS-NAME (OR (SECOND (ASSQ STATUS SYSTEM-STATUS-ALIST)) STATUS)))
               (OR (EQUAL STATUS-NAME "")
                   (FORMAT T ", ~A" STATUS-NAME)))
             (IF MAJOR (FORMAT T ", ~D.~D is loaded" MAJOR MINOR))
             (FORMAT T ";~%  a typical patch file is ~A~%"
                     (PATCH-SYSTEM-PATHNAME SYSTEM-NAME ':PATCH-FILE (OR MAJOR 1) (OR MINOR 0)
                                            ':LISP))
             (AND MAJOR
                  (FQUERY NIL "Do you want to see the patches for ~A? " SYSTEM-NAME)
                  (PRINT-PATCHES SYSTEM)))))
    (IF (SYSTEM-PACKAGE-DEFAULT SYSTEM)
        (FORMAT T "~& Files in ~A are forcibly read in package ~A.~%"
                SYSTEM-NAME (SYSTEM-PACKAGE-DEFAULT SYSTEM)))
    (WHEN SHOW-FILES
      (FORMAT T "~%Compilation and loading of files in this system:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':RELOAD ':DO-NOT-DO-COMPONENTS
                   ':DESCRIBE ':NO-INCREMENT-PATCH ':NO-RELOAD-SYSTEM-DECLARATION))
    (WHEN SHOW-TRANSFORMATIONS
      (FORMAT T "~%Transformations required to MAKE-SYSTEM now:~2%")
      (MAKE-SYSTEM SYSTEM-NAME ':COMPILE ':DO-NOT-DO-COMPONENTS ':PRINT-ONLY
                   ':NO-RELOAD-SYSTEM-DECLARATION))
    (LET ((COMPONENTS (SYSTEM-COMPONENT-SYSTEMS SYSTEM)))
      (COND (COMPONENTS
             (FORMAT T "~2&~A is made up of component system~P "
                     SYSTEM-NAME (LENGTH COMPONENTS))
             (FORMAT:PRINT-LIST T "~A" COMPONENTS)
             (WHEN (Y-OR-N-P "Describe the component system~P?" (LENGTH COMPONENTS))
               (DOLIST (COMPONENT COMPONENTS)
                 (FORMAT T "~2&")
                 (DESCRIBE-SYSTEM COMPONENT ':SHOW-FILES SHOW-FILES
                                  ':SHOW-TRANSFORMATIONS SHOW-TRANSFORMATIONS)))))))
  SYSTEM-NAME)

(DEFUN (:DESCRIBE MAKE-SYSTEM-KEYWORD) ()
  (SETQ *FILE-TRANSFORMATION-FUNCTION* 'DESCRIBE-FILE-TRANSFORMATIONS))

(DEFUN DESCRIBE-FILE-TRANSFORMATIONS ()
  (DOLIST (FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)
    (LET ((STATE (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION)))
      (SELECTQ STATE
        ((:DONE :REFUSED :NOT-NEEDED NIL))
        ((:PENDING :PROBABLY)
         (LET ((TYPE (FILE-TRANSFORMATION-TRANSFORMATION-TYPE FILE-TRANSFORMATION))
               (ARGS (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
               (OUTPUTS (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION))
               (*FORCE-PACKAGE* (FILE-TRANSFORMATION-FORCE-PACKAGE FILE-TRANSFORMATION))
               (*SYSTEM-BEING-MADE* (FILE-TRANSFORMATION-SYSTEM FILE-TRANSFORMATION)))
           (FORMAT T "~&~\SI::FILE-XFORM-ARGS\~:[ ~:[are~;is~] ~A~
                                 ~:[~; in~:[to~] package ~A~]~]"
                   FILE-TRANSFORMATION (NULL ARGS) (EQ (CDR ARGS) OUTPUTS)
                   (TRANSFORMATION-TYPE-PRETTY-PAST-PARTICIPLE TYPE)
                   *FORCE-PACKAGE*
                   (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION)
                   *FORCE-PACKAGE*)
           (COND ((MEMQ (TRANSFORMATION-TYPE-NAME TYPE) *LOAD-TYPE-TRANSFORMATIONS*)
                  (DO L ARGS (CDR L) (EQ L OUTPUTS)
                      (DESCRIBE-FILE-TRANSFORMATION-LOADED-FILE (CAR L))))
                 ((EQ (TRANSFORMATION-TYPE-FUNCTION TYPE) 'QC-FILE-1)
                  (DESCRIBE-FILE-TRANSFORMATION-COMPILED-FILE (CAR ARGS)))))
         (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) :DONE))
        (OTHERWISE
         (FERROR NIL "Transformation ~S in bad state" FILE-TRANSFORMATION))))))

(DEFUN DESCRIBE-FILE-TRANSFORMATION-LOADED-FILE (FILE &AUX ID)
  (AND (SETQ ID (GET-FILE-LOADED-ID FILE *FORCE-PACKAGE*))
       (FORMAT T "~& ~A was created ~\TIME\~%" (CAR ID) (CDR ID))))

(DEFUN DESCRIBE-FILE-TRANSFORMATION-COMPILED-FILE (FILE &AUX SID CDATA)
  (LET ((GENERIC-PATHNAME (SEND FILE :GENERIC-PATHNAME)))
    (SETQ SID (SEND GENERIC-PATHNAME :GET :QFASL-SOURCE-FILE-UNIQUE-ID)
          CDATA (SEND GENERIC-PATHNAME :GET :COMPILE-DATA)))
  (COND ((OR SID CDATA)
         (FORMAT T "~& ~A was compiled" (OR SID FILE))
         (AND CDATA
              (APPLY 'FORMAT T " by ~A on ~A at ~\TIME\~%~10@Twith system ~D.~D~%" CDATA)))))

(DEFUN SYSTEM-SUBSYSTEM-P (MAYBE-SUBSYSTEM SUPERSYSTEM)
  "Non-NIL if the system MAYBE-SUBSYSTEM is a subsystem of SUPERSYSTEM.
The actual value is the subsystem.
Both arguments may be systems or names of systems."
  ;;get the actual system objects for the systems.
  (LET* ((SUPERSYSTEM-SYSTEM (FIND-SYSTEM-NAMED SUPERSYSTEM))
         (MAYBE-SUBSYSTEM  (FIND-SYSTEM-NAMED MAYBE-SUBSYSTEM))
         (MAYBE-SUBSYSTEM-NAME (SYSTEM-NAME MAYBE-SUBSYSTEM)))
    (DOLIST (SUBSYSTEM-NAME (SYSTEM-COMPONENT-SYSTEMS SUPERSYSTEM-SYSTEM))
      ;;COMPONENT-SYSTEMS is a list of system names, first try seeing if the
      ;;name of MAYBE-SUBSYSTEM is the same as that of the system under
      ;;consideration.
      (IF (and (or (stringp subsystem-name)  ;a bit of defensive programming..
                   (symbolp subsystem-name)) ; dont allow a wedged system somebody
                                             ; defined to wedge everything..
               (OR (EQUALP SUBSYSTEM-NAME MAYBE-SUBSYSTEM-NAME))
              (EQ (FIND-SYSTEM-NAMED SUBSYSTEM-NAME) MAYBE-SUBSYSTEM))
          (RETURN MAYBE-SUBSYSTEM)))))

(DEFUN TRANSFORMATIONS-THAT-COMPILE (TRANSFORMATIONS)
  ;; THIS IS USEFUL:
  ;; (TRANSFORMATIONS-THAT-COMPILE (MAKE-SYSTEM'FOO :RECOMPILE :PRINT-ONLY :SILENT))
  ;;
  (MAPCAN #'(LAMBDA (TRANSFORMATION)
              (WHEN (EQ (CAR (FILE-TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION))
                        :COMPILE)
                (LIST (FILE-TRANSFORMATION-ARGS TRANSFORMATION))))
          TRANSFORMATIONS))
