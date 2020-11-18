;;; Tasteful Flavors    -*- Mode:LISP; Package:SI; Base:8; Readtable:ZL -*-
;;; This is SYS: SYS2; FLAVOR

;;; A flavor-name is a symbol which names a type of objects defined
;;; by the combination of several flavors.  The SI:FLAVOR
;;; property is a data-structure (of type FLAVOR) defining the
;;; nature of the flavor, as defined below.
;;;
;;; Flavors come in essentially three kinds.  The first kind defines a class
;;; of flavors, and provides the basic instance variables and methods for
;;; that class.  This kind typically includes only VANILLA-FLAVOR as a
;;; component, and uses the :REQUIRED-INSTANCE-VARIABLES and
;;; :REQUIRED-METHODS options.  The second kind of flavor represents a
;;; particular option that may be combined in (a "mix-in").  The third
;;; kind of flavor is the kind that can usefully be instantiated; it is
;;; a combination of one of the first kind and several of the second kind,
;;; to achieve the behavior desired for a particular application.
;;;
;;; The following symbols are interesting to outsiders:
;;; DEFFLAVOR - macro for defining a flavor
;;; DEFMETHOD - macro for defining a method
;;; DEFWRAPPER - macro for defining a flavor-wrapper
;;; INSTANTIATE-FLAVOR - create an object of a specified flavor
;;; MAKE-INSTANCE - easier to call version of INSTANTIATE-FLAVOR
;;; COMPILE-FLAVOR-METHODS - macro which does the right thing in the compiler
;;; RECOMPILE-FLAVOR - function to recompile a flavor and maybe any flavors
;;;             that depend on it.  Usually this happens automatically.
;;; *ALL-FLAVOR-NAMES* - list of all symbols which have been used as the name of a flavor
;;; *ALL-FLAVOR-NAMES-AARRAY* - completion aarray of flavor names to flavors.
;;;             Each flavor is included twice, once with and once without its package prefix.
;;; *FLAVOR-COMPILATIONS* - list of all methods which had to be compiled
;;;             this is useful for finding flavors which weren't compiled in qfasl files
;;;             or which need to be recompiled to bring them up to date.
;;; *FLAVOR-COMPILE-TRACE* - if non-NIL, a FORMAT destination for messages about
;;;             recompilation of combined methods
;;; *USE-OLD-FLAVOR-INFO* - if NIL, re-DEFFLAVORing a flavor always makes a new one.
;;;             For debugging weird screws.
;;;             Also makes it possible to redefine a flavor and leave old
;;;             instances with the old methods, even if the flavor instance variables
;;;             are not being changed.
;;; FLAVOR-ALLOWS-INIT-KEYWORD-P - determine whether a certain flavor allows
;;;             a certain keyword in its init-plist.
;;; FLAVOR-ALLOWED-INIT-KEYWORDS - returns all the init keywords a flavor handles.

;;; Roads not taken:
;;;  o Changing the size of all extant instances of a flavor.
;;;  o Nothing to stop you from instantiating a flavor of the first or
;;;    second kind.  In practice you will usually get an error if you try it.

;;; Philosophy with respect to multiple processes
;;;  Interrupts are inhibited such that multiple processes munging unrelated
;;;  flavors should work.  Multiple processes instantiating related flavors
;;;  will work, however multiple processes defining methods for the same
;;;  flavor at the same time, and things like that, will not.

;;; This macro is used to define a flavor.  Use DEFMETHOD to define
;;; methods (responses to messages sent to an instance of a flavor.)

(DEFMACRO DEFFLAVOR (NAME INSTANCE-VARIABLES COMPONENT-FLAVORS &REST OPTIONS)
  "Defines a class of Flavor data-structures known as NAME,
built on COMPONENT-FLAVORS, that contains INSTANCE-VARIABLES.
/
INSTANCE-VARIABLES is a list; each element may be a symbol, or
  a list of a symbol and initialization form.
/
COMPONENT-FLAVORS are searched from left to right for methods,
 and contribute their instance variables.
/
OPTIONS include:
 - :GETTABLE-INSTANCE-VARIABLES
   (:GETTABLE-INSTANCE-VARIABLES var1 var2...)
     Methods are automatically generated for the listed instance variables
       for retrieving their values.  Method names are keywords.
     For the atomic form, methods are automatically generated for retrieving the
       values of all local instance variables (those declared in this DEFFLAVOR).
 - :INITTABLE-INSTANCE-VARIABLES
   (:INITTABLE-INSTANCE-VARIABLES var1 var2...)
     These instance variables may be initialized via the options to MAKE-INSTANCE.
 - :SETTABLE-INSTANCE-VARIABLES
   (:SETTABLE-INSTANCE-VARIABLES var1 var2...)
     Methods are automatically generated for the listed instance variables
       for changing their values.
     SETTABLE instance variables are also INITTABLE.
 - (:REQUIRED-INSTANCE-VARIABLES var1 var2...)
     Any flavor incorporating this flavor and actually instantiated must have
       instance variables with the specified names.  This is typically used
       for defining general and abstract types of flavors.
 - (:REQUIRED-METHODS method1 method2...)
     Any flavor incorporating this flavor and actually instantiated must have
       methods for the specified operations.
 - (:REQUIRED-FLAVORS flavor1 flavor2...)
     Any flavor incorporating this flavor and actually instantiated must have
       the component flavors as specified.
 - (:INIT-KEYWORDS key1 key2...)
     Specifies legal keywords for the INIT-PLIST (keywords that can be passed as
       arguments to initialize instances of this flavor).
     This option may be used to avoid restrictions on initialization keywords.
 - (:DEFAULT-INIT-PLIST key1 val1 key2 val2...)
     Specifies default keyword//value pairs to put in the INIT-PLIST.
      The keywords are used only if are not already specified when instantiating.
      The corresponding values get evaluated when and only if they are used.
 - (:DEFAULT-HANDLER function)
     Specifies a function to be called if a message is sent for which
       there is no corresponding method.  The function is given the message name,
       and SELF is bound to the instance which was sent the message.
     The default is a function which signals an UNCLAIMED-MESSAGE condition.
 - (:INCLUDED-FLAVORS flavor1 flavor2...)
     Specifies flavors to be included in this flavor.  This is different from
       specifying components in that included flavors go at the end, so they
       act as defaults.  This makes a difference when this flavor is depended on
       by other flavors.
 - :NO-VANILLA-FLAVOR
     Do not automatically include VANILLA-FLAVOR. [For exotic purposes only.]
 - (:ORDERED-INSTANCE-VARIABLES var1 var2...)
     Requires that in any instance built on this flavor, the specified instance variables
       must reside in the specified memory order.  Used internally, e.g. for slots
       specially referenced by microcode. The atomic form works too.
 - (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES var1 var2...)
     Defines DEFSUBSTs which act like DEFSTRUCT accessors for the specified variables.
       These functions, called with an argument of an instance, return the value of
       the corresponding instance variable within that instance.
     By default, the names of DEFSUBSTs are constructed as: <flavor-name>-<variable-name>.
     If the instance variable is ordered, the accessor will know its index
       in the instance and access it directly; otherwise it will call
       SYMEVAL-IN-INSTANCE at run-time.
     The atomic form works too.
 - (:ACCESSOR-PREFIX symbol)
     For OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES, specifies a symbol as the prefix
       to use instead of the flavor name for the names of the generated DEFSUBSTs.
 - (:METHOD-ORDER method1 method2...)
     Specifies important method keywords, indicating methods that should reside first
       in the select-method lookup table for increased efficiency.
 - (:METHOD-COMBINATION (type order operation1 operation2...) ...)
     Specifies ways of combining methods from different flavors.  :DAEMON NIL is
       the default.  Order is usually :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST,
       but this depends on type.
 - (:DOCUMENTATION args...)
     The list of arguments is put on the flavor's :DOCUMENTATION property.
     The arguments may include keyword symbols and documentation strings,
       which are used to construct the flavor's documentation (type FLAVOR).
     Typical keywords include :MIXIN, :ABSTRACT-FLAVOR, and :COMBINATION.
     For example:
        /(defflavor example ((example-thing /"example/")) (other-flavor)
           (:documentation :bogus /"Just an example./" :combination /"Illustration only./"))
        /(princ (documentation 'example 'flavor)) =>
        Just an example.
        Illustration only.
        A BOGUS COMBINATION Flavor.
 - (:SPECIAL-INSTANCE-VARIABLES var1 var2 ...)
     Specifies instance variables that must be bound as SPECIAL (dynamic)
       when instances are sent messages.
 - :ABSTRACT-FLAVOR
     Prohibits instantiation; allows COMPILE-FLAVOR-METHODS without complaining about
       missing requirements.
 - :ALIAS-FLAVOR
     Specifies that this flavor has only one component and is an alias for it.
     This flavor may not have local instance variables, etc.
     Instantiating the alias flavor makes an instance of the true (component) flavor."
  (declare (zwei:indentation 1 10 3 1))
  (declare (values name))
  (LET ((COPIED-OPTIONS (COPYLIST OPTIONS)))
   `(PROGN
      ;; Define flavor at load time.
      ;; Must come before the compile-time COMPOSE-AUTOMATIC-METHODS,
      ;; which puts methods in the QFASL file.
      (EVAL-WHEN (LOAD EVAL)
        (DEFFLAVOR2 ',NAME ',INSTANCE-VARIABLES
          ',COMPONENT-FLAVORS ',COPIED-OPTIONS))
      ;; Define the flavor if not loading.
      (EVAL-WHEN (COMPILE)
        (IF (JUST-COMPILING)
            (LET ((*JUST-COMPILING* T))
              (DEFFLAVOR2 ',NAME ',INSTANCE-VARIABLES
                ',COMPONENT-FLAVORS ',COPIED-OPTIONS)
              ;; Compile the automatic instance-variable get/set methods into QFASL file
              (COMPOSE-AUTOMATIC-METHODS (COMPILATION-FLAVOR ',NAME)))
          ;; Compiling in editor buffer.  Must define automatic methods for real now.
          (COMPOSE-AUTOMATIC-METHODS (GET ',NAME 'FLAVOR))))
     (EVAL-WHEN (EVAL)
       ;; Create the instance-variable get/set methods if evaling.
       (COMPOSE-AUTOMATIC-METHODS (GET ',NAME 'FLAVOR)))
     (EVAL-WHEN (COMPILE LOAD EVAL)
       ;; Make any instance-variable accessor macros, needed at both compile and run times.
       . ,(DO ((VS (DO ((OPTS OPTIONS (CDR OPTS)))
                       ((NULL OPTS) NIL)
                     (WHEN (EQ (CAAR-SAFE OPTS) :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
                       (RETURN (CDAR OPTS)))
                     (WHEN (EQ (CAR OPTS) :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
                       (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
                                       INSTANCE-VARIABLES))))
                   (CDR VS))
               (PREFIX (OR (CADR (ASSQ-CAREFUL :ACCESSOR-PREFIX OPTIONS))
                           (STRING-APPEND NAME "-")))
               (ORDS (DO ((OPTS OPTIONS (CDR OPTS)))
                         ((NULL OPTS) NIL)
                       (WHEN (EQ (CAAR-SAFE OPTS) :ORDERED-INSTANCE-VARIABLES)
                         (RETURN (CDAR OPTS)))
                       (WHEN (EQ (CAR OPTS) :ORDERED-INSTANCE-VARIABLES)
                         (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
                                         INSTANCE-VARIABLES)))))
               (RES NIL (CONS `(DEFSUBST-WITH-PARENT ,(INTERN1 (STRING-APPEND PREFIX (CAR VS)))
                                                     ,NAME
                                                     (,NAME)
                                 ,(IF (MEMQ (CAR VS) ORDS)
                                      `(%INSTANCE-REF ,NAME
                                           ,(1+ (FIND-POSITION-IN-LIST (CAR VS) ORDS)))
                                    `(SYMEVAL-IN-INSTANCE ,NAME ',(CAR VS))))
                              RES)))
              ((NULL VS) RES)))
     ,@(MAKE-RUN-TIME-ALTERNATIVE-DEFFLAVORS
         NAME (OR (CDR (ASSQ-CAREFUL ':RUN-TIME-ALTERNATIVES OPTIONS))
                  (CDR (ASSQ-CAREFUL ':MIXTURE OPTIONS))))
     ',NAME)))

(DEFPROP DEFFLAVOR2 T QFASL-DONT-RECORD)
(DEFUN DEFFLAVOR2 (NAME INSTANCE-VARIABLES COMPONENT-FLAVORS COPIED-OPTIONS)
  (COND ((AND (VARIABLE-BOUNDP FILE-WARNINGS-DATUM)
              FILE-WARNINGS-DATUM)
         (OBJECT-OPERATION-WITH-WARNINGS (NAME)
           (COMPILER:WARN-ON-ERRORS ('FLAVOR-DEFINITION-ERROR "Error in flavor definition")
             (DEFFLAVOR1 NAME INSTANCE-VARIABLES
               COMPONENT-FLAVORS COPIED-OPTIONS))))
        (T
         (DEFFLAVOR1 NAME INSTANCE-VARIABLES
           COMPONENT-FLAVORS COPIED-OPTIONS))))

(DEFUN UNDEFFLAVOR (FLAVOR-NAME &AUX FL)
  "Make the flavor named FLAVOR-NAME cease to be defined."
  (CHECK-ARG FLAVOR-NAME (EQ 'FLAVOR (TYPE-OF (SETQ FL (IF (SYMBOLP FLAVOR-NAME)
                                                           (GET FLAVOR-NAME 'FLAVOR)
                                                           FLAVOR-NAME))))
             "a flavor or the name of one")
  (DOLIST (DEPENDENT (FLAVOR-DEPENDED-ON-BY FL))
    (PUSH (CONS (FLAVOR-NAME FL) DEPENDENT)
          *FLAVOR-PENDING-DEPENDS*))
  (PERFORM-FLAVOR-REDEFINITION (FLAVOR-NAME FL) T)
  (SETQ *ALL-FLAVOR-NAMES* (DELQ FL *ALL-FLAVOR-NAMES*))
  (REMPROP (FLAVOR-NAME FL) 'FLAVOR))

;;; This wraps a local-declare special of the instance variables around its body.
;;; It's good for things like defining functions that deal with a flavor but
;;; are not methods (generally they are called by methods.)
(DEFMACRO DECLARE-FLAVOR-INSTANCE-VARIABLES ((FLAVOR-NAME MAP-SET-BY-CALLER)
                                             &BODY BODY)
  "Enable the BODY to access instance variables of SELF, being an instance of FLAVOR-NAME.
The instance variables of SELF are made accessible under the assumption
that, when this code is executed, SELF's flavor will include FLAVOR-NAME
as a component flavor.
This macro may go around expressions in a function, or around
entire function definitions.  In the latter case, it is equivalent
to writing (DECLARE (:SELF-FLAVOR flavor-name)) inside the functions."
  (LET ((FLAVOR-DECLARATION (IF (EQ FLAVOR-NAME 'VANILLA-FLAVOR)
                                `(:SELF-FLAVOR VANILLA-FLAVOR NIL)
                              (LET ((*JUST-COMPILING*
                                      (JUST-COMPILING)))
                                (FLAVOR-DECLARATION FLAVOR-NAME))))
        DECLS)
    (OR MAP-SET-BY-CALLER
        (SETQ BODY (LIST `(WITH-SELF-ACCESSIBLE ,FLAVOR-NAME . ,BODY))))
    (IF FLAVOR-DECLARATION
        (PUSH FLAVOR-DECLARATION DECLS))
    `(LOCAL-DECLARE ,DECLS
       (COMPILER-LET ((SELF-FLAVOR-DECLARATION ',(CDR FLAVOR-DECLARATION)))
                     . ,BODY))))

;;; Interpreted definition.  Only works compiled.
(DEFUN WITH-SELF-VARIABLES-BOUND (&QUOTE &REST BODY)
  "Execute the body with all instance variables of SELF bound as specials.
This means that the body can use SYMEVAL, BOUNDP, etc. on them."
  (WITH-SELF-VARIABLES-BOUND
    (EVAL-BODY BODY)))

(DEFUN SELF-BINDING-INSTANCES ()
  "Produces a list suitable for %USING-BINDING-INSTANCES.  Provides
run-time support for the compiled code for WITH-SELF-VARIABLES-BOUND."
  (AND (TYPEP SELF 'INSTANCE)
       (DO ((INDEX 1 (1+ INDEX))
            (IVARS (FLAVOR-ALL-INSTANCE-VARIABLES (INSTANCE-FLAVOR SELF)) (CDR IVARS))
            (BINDINGS)
            (NORMAL-BINDINGS-LEFT (FLAVOR-BINDINGS (INSTANCE-FLAVOR SELF)))
            (NEXT-NORMAL-BINDING))
           ((NULL IVARS) BINDINGS)
         ;; Figure out whether the next ivar is bound as special by message sending.
         (OR (AND (NUMBERP NEXT-NORMAL-BINDING) (PLUSP NEXT-NORMAL-BINDING))
             (SETQ NEXT-NORMAL-BINDING (POP NORMAL-BINDINGS-LEFT)))
         (IF (NUMBERP NEXT-NORMAL-BINDING)
             (DECF NEXT-NORMAL-BINDING))
         ;; If it isn't, we must put it on our binding list to be bound now.
         (OR (LOCATIVEP NEXT-NORMAL-BINDING)
             (SETQ BINDINGS
                   (LIST* (LOCF (SYMBOL-VALUE (CAR IVARS)))
                          (LOCF (%INSTANCE-REF SELF INDEX))
                          BINDINGS))))))

;;; Interpreted definition.
(DEFUN WITH-SELF-ACCESSIBLE (&QUOTE FLAVOR-NAME &REST BODY)
  "Binds all instance variables of FLAVOR-NAME as specials within BODY."
  FLAVOR-NAME
  (WITH-SELF-VARIABLES-BOUND
    (eval-body body)))

(eval-when (load compile eval)
(defun arglist-has-lambda-list-keywords-p (arglist)
  (lisp:some #'(lambda (x) (member x lambda-list-keywords)) (the list arglist)))
)

;;; These two for compatibility with the symbolix system.
(DEFMACRO DEFUN-METHOD (FSPEC FLAVOR-NAME ARGLIST &BODY BODY)
  "Defines a /"function/" FSPEC with the argument list ARGLIST and the
body BODY which has direct access to all the instance variables of
FLAVOR-NAME."
  `(DECLARE-FLAVOR-INSTANCE-VARIABLES (,FLAVOR-NAME)
     (DEFUN ,FSPEC ,ARGLIST . ,BODY)))

(DEFMACRO INSTANCE-VARIABLE-BOUNDP (X)
  "Returns T if X is bound, otherwise NIL."
  `(BOUNDP ',X))

(DEFMACRO DEFWHOPPER ((FLAVOR-NAME OPERATION) ARGLIST &BODY BODY)
  "Defines an :AROUND method OPERATION for the flavor FLAVOR-NAME.  The
:AROUND method is able to control when, whether, and how the remaining
methods will be executed."
  `(DEFMETHOD (,FLAVOR-NAME :AROUND ,OPERATION)
              (.CONTINUATION. .MAPPING-TABLE. .AROUND-ARGS. . ,ARGLIST)
     . ,BODY))

(DEFMACRO CONTINUE-WHOPPER (&REST ARGUMENTS)
  "Calls for a continuation of the remaining methods within a
DEFWHOPPER.  ARGUMENTS are the arguments to be passed on to the
remaining methods."
  `(FUNCALL-WITH-MAPPING-TABLE .CONTINUATION. .MAPPING-TABLE.
                               (CAR .AROUND-ARGS.)
                               . ,ARGUMENTS))

(DEFMACRO LEXPR-CONTINUE-WHOPPER (&REST ARGUMENTS)
  "Calls for a continuation of the remaining methods within a
DEFWHOPPER.  ARGUMENTS are the arguments to be passed on to the
remaining methods.  Calls on LEXPR-FUNCALL to do its dirty work."
  `(LEXPR-FUNCALL-WITH-MAPPING-TABLE .CONTINUATION. .MAPPING-TABLE.
                                     (CAR .AROUND-ARGS.)
                                     . ,ARGUMENTS))

(DEFVAR *ALL-FLAVOR-NAMES* NIL) ;List of names of all flavors (mostly for editor)
(DEFVAR *ALL-FLAVOR-NAMES-AARRAY*       ;For editor's completing reader
        (MAKE-ARRAY #o2400              ;#o736 flavors in system 75, #o1140 in system 98.
                    :TYPE 'ART-Q-LIST
                    :LEADER-LIST '(0 NIL)))

(ADD-INITIALIZATION "Condense Flavor Name Tables"
                    '(PROGN (ZWEI:SORT-COMPLETION-AARRAY *ALL-FLAVOR-NAMES-AARRAY*)
                            (IF (= (%P-CDR-CODE *ALL-FLAVOR-NAMES*) CDR-NORMAL)
                                (SETQ *ALL-FLAVOR-NAMES* (COPYLIST *ALL-FLAVOR-NAMES*))))
                    '(:BEFORE-COLD))

;;; Don't let these get left bound losingly after a warm boot.
(ADD-INITIALIZATION "Reinit possibly bound flavor globals"
                    '(SETQ *USE-OLD-COMBINED-METHODS* T
                           *JUST-COMPILING* NIL)
                    '(:WARM))

(DEFVAR *USE-OLD-FLAVOR-INFO* T)        ;T means DEFFLAVOR1 only "unhooks" if the flavor
                                        ;has changed incompatibly, NIL means always unhook
                                        ;if flavor already existed.
(DEFVAR *USE-OLD-COMBINED-METHODS* T)   ;T means recycle old, NIL means generate new.
                                        ; This is an implicit argument to certain routines.
(DEFVAR *FLAVOR-PENDING-DEPENDS* NIL)   ;Used by DEFFLAVOR1
(DEFVAR *FLAVOR-COMPILATIONS* NIL)      ;List of methods compiled
(DEFVAR *FLAVOR-COMPILE-TRACE* NIL)

(DEFVAR *JUST-COMPILING* NIL)   ;T means putting combined methods into qfasl file,
                                ; not updating the current flavor data-structure

;;; T if we are inside a compilation going to a binary file.
;;; We do not simply call this function wherever we want to check,
;;; but instead bind *JUST-COMPILING* at various points
;;; and check that.  The reason is that those points are all
;;; inside (EVAL-WHEN (COMPILE) ..)'s; as a result, any flavor
;;; hacking done randomly inside the compiler's execution
;;; finds *JUST-COMPILING* is NIL, as it should.
(DEFUN JUST-COMPILING ()
  (AND (BOUNDP 'COMPILER::QC-FILE-IN-PROGRESS)
       COMPILER::QC-FILE-IN-PROGRESS
       (NOT COMPILER::QC-FILE-LOAD-FLAG)))

(DEFVAR *FLAVOR-AREA* WORKING-STORAGE-AREA
  "This is an area in which to cons data internal to the flavor system.  It is used
rather than default-cons-area as a hedge against temporary area lossage which can
happen if you do things from an error in a compilation, or if you make instances
in a temporary area and that requires composing flavors or methods.")

;;; These two functions are used when sending a message to yourself, for extra efficiency.
(DEFMACRO FUNCALL-SELF (&REST ARGS)
  "Like FUNCALL of SELF, but a little faster,"
  `(FUNCALL (%FUNCTION-INSIDE-SELF)
            . ,ARGS))

(DEFMACRO LEXPR-FUNCALL-SELF (&REST ARGS)
  "Like LEXPR-FUNCALL of SELF, but a little faster."
  `(LEXPR-FUNCALL (%FUNCTION-INSIDE-SELF)
                  . ,ARGS))

(DEFSUBST INSTANCE-FLAVOR (INSTANCE)
  "Returns the flavor-object of a given flavor instance."
  (%MAKE-POINTER DTP-ARRAY-POINTER (%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)))

(DEFSUBST %INSTANCE-FLAVOR (INSTANCE)
  "Returns the flavor-object of a given flavor instance."
  (%MAKE-POINTER DTP-ARRAY-POINTER (%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)))

(DEFSUBST INSTANCE-FUNCTION (INSTANCE)
  "Returns the handler-function of the flavor of INSTANCE."
  (%P-CONTENTS-OFFSET (%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)
                      %INSTANCE-DESCRIPTOR-FUNCTION))

(DEFSUBST %INSTANCE-FUNCTION (INSTANCE)
  "Returns the handler-function of the flavor of INSTANCE."
  (%P-CONTENTS-OFFSET (%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)
                      %INSTANCE-DESCRIPTOR-FUNCTION))

;;; When compiling files, we make a new flavor object for each flavor
;;; defined in the file.  That way we win if the definition in the file
;;; does not match the one loaded.  These flavors live in a FILE-LOCAL-DECLARATION
;;; element which looks like (FLAVORS name flavor name flavor ...)
;;; This function, given a flavor name or flavor object,
;;; gives the right flavor object to use.  If compiling a file,
;;; it uses the compilation flavor if any; otherwise, it uses the installed flavor.
(DEFUN COMPILATION-FLAVOR (FLAVOR-OR-NAME
                           &OPTIONAL (USE-COMPILATION-FLAVORS *JUST-COMPILING*))
  "Returns the appropriate flavor object for the specified flavor.
If compiling, it returns the compilation-time flavor object corresponding
to the specified flavor or flavor name.  If not compiling, returns the
actual installed flavor object.  USE-COMPILATION-FLAVORS specifies whether
to assume we are compiling or not; it defaults to the truth."
  (OR (AND USE-COMPILATION-FLAVORS
           (getdecl (IF (SYMBOLP FLAVOR-OR-NAME)
                        FLAVOR-OR-NAME
                     (FLAVOR-NAME FLAVOR-OR-NAME))
                    'flavor))
      (IF (SYMBOLP FLAVOR-OR-NAME)
          (GET FLAVOR-OR-NAME 'FLAVOR)
        FLAVOR-OR-NAME)))

;;; The data-structure on the FLAVOR property of a flavor-name
;;; This must agree with INSTANCE-DESCRIPTOR-OFFSETS in SYS: SYS; QCOM
(DEFSTRUCT (FLAVOR :NAMED :ARRAY (:CONSTRUCTOR MAKE-FLAVOR) (:ALTERANT NIL)
                                 (:MAKE-ARRAY (:AREA PERMANENT-STORAGE-AREA)))
  (FLAVOR-INSTANCE-SIZE nil :documentation
    "1+ the number of instance variables")
  (FLAVOR-BINDINGS nil :documentation
    "List of locatives to instance variable internal value cells.  MUST BE CDR-CODED!!
Fixnums can also appear.  They say to skip that number of instance variable slots.")

  (FLAVOR-METHOD-HASH-ARRAY nil :documentation
    "The hash table for methods of this flavor.
NIL means method-combination not composed yet.
T means abstract flavor with COMPILE-FLAVOR-METHODS done.")

  (FLAVOR-NAME nil :documentation
    "Symbol which is the name of the flavor. This is returned by TYPEP-OF")

  (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST nil :documentation
    "Alist of component flavor names vs. locatives into vector containing mapping tables.")

  (FLAVOR-LIST-FLAG nil :documentation
    "Non-NIL if this flavor's instances can be non-atomic.
If T, they are all non-atomic.  Otherwise, it can be a fixnum
which is the index (origin 1) of an instance variable;
the instance is non-atomic if that instance variable is non-NIL.
Nonatomic means that CONSP will return T!!!")

  ;; End of magic locations known in microcode and QCOM.

  (FLAVOR-ALL-INSTANCE-VARIABLES nil :documentation
    "Just names, only valid when flavor-combination composed.
Corresponds directly to FLAVOR-BINDINGS and the instances.")

  ;; Defined below
  FLAVOR-METHOD-TABLE

  ;; End of locations depended on in many other files.

  (FLAVOR-DEPENDS-ON nil :documentation
    "List of names of flavors incorporated into this flavor.")
  (FLAVOR-DEPENDED-ON-BY nil :documentation
    "List of names of flavors which incorporate this one.
These are only immediate dependencies.")
  (FLAVOR-INCLUDES nil :documentation
    "List of names of flavors to include at the end rather than as immediate depends-on's.")
  (FLAVOR-PACKAGE nil :documentation
    "Package in which the DEFFLAVOR was done.")
  (FLAVOR-DEPENDS-ON-ALL nil :documentation
    "Names of all flavors depended on, to all levels, including this flavor itself.
NIL means flavor-combination not composed yet.  This is used by TYPEP")
  (FLAVOR-WHICH-OPERATIONS NIL :documentation
    "List of operations handled, created when needed.
This is NIL if it has not been computed yet.")
  (FLAVOR-MAPPED-INSTANCE-VARIABLES NIL :documentation
    "This is the list of instance variables accessable from this flavor
which are mapped by mapping tables with this flavor as the method-flavor.")
  (FLAVOR-DEFAULT-HANDLER NIL :documentation
    "Redundant copy of :DEFAULT-HANDLER property, for speed in calling it.")
  (FLAVOR-INITTABLE-INSTANCE-VARIABLES NIL :documentation
    "Alist from init keyword to name of variable")
  (FLAVOR-INIT-KEYWORDS NIL)
  ;;Esoteric things stored here as properties
  ;;Known: :ORDERED-INSTANCE-VARIABLES, :DEFAULT-HANDLER
  ;;       :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES, :ACCESSOR-PREFIX,
  ;;       :REQUIRED-INSTANCE-VARIABLES, :REQUIRED-METHODS,
  ;;       :REQUIRED-FLAVORS, :METHOD-ORDER,
  ;;       :DEFAULT-INIT-PLIST, :DOCUMENTATION, :NO-VANILLA-FLAVOR
  ;;       :GETTABLE-INSTANCE-VARIABLES :SETTABLE-INSTANCE-VARIABLES
  ;;       :SPECIAL-INSTANCE-VARIABLES
  ;;       :ABSTRACT-FLAVOR, :ALIAS-FLAVOR
  ;;       :INSTANTIATION-FLAVOR-FUNCTION
  ;;       :RUN-TIME-ALTERNATIVES or :MIXTURE
  ;;       RUN-TIME-ALTERNATIVE-ALIST --  the alist of lists of flavors vs names
  ;;                                      we constructed for those combinations.
  ;;       ADDITIONAL-INSTANCE-VARIABLES
  ;;       COMPILE-FLAVOR-METHODS
  ;;       UNMAPPED-INSTANCE-VARIABLES
  ;;       MAPPED-COMPONENT-FLAVORS
  ;;       ALL-INSTANCE-VARIABLES-SPECIAL
  ;;       INSTANCE-VARIABLE-INITIALIZATIONS
  ;;       ALL-INITTABLE-INSTANCE-VARIABLES
  ;;       REMAINING-DEFAULT-PLIST, REMAINING-INIT-KEYWORDS,  UNHANDLED-INIT-KEYWORDS
  ;;       :INSTANCE-AREA-FUNCTION - the one specified for this fl.
  ;;       INSTANCE-AREA-FUNCTION - what to use (maybe inherited)
  ;;       :REQUIRED-INIT-KEYWORDS - the ones specified for this fl.
  ;;       REQUIRED-INIT-KEYWORDS - all required ones incl. inherited.
  ;;The convention on these is supposed to be that ones in the keyword package
  ;; are allowed to be used by users.
  ;; Some of these are not used by the flavor system; they are just remembered on the plist
  ;; in case anyone cares.  The flavor system does all its handling of them during the
  ;; expansion of the DEFFLAVOR macro.
  (FLAVOR-PLIST NIL)

  (FLAVOR-LOCAL-INSTANCE-VARIABLES nil :documentation
    "Names and initializations; does not include inherited ones.")
  )

;;; Named-structure handler for above structure, to make it print nicer
(defselect ((:property flavor named-structure-invoke) ignore)
  (:print-self (self *standard-output* ignore &optional ignore)
     (printing-random-object (self *standard-output*)
       (format t "Flavor ~S" (flavor-name self))))
  (:describe (self)
    (describe-flavor self))
  ((:get :get-location-or-nil :get-location :getl :putprop :putprop-in-area :remprop :push-property :plist
    :property-list :plist-location :property-list-location :setplist :set-property-list :set)
   . flavor-property-list-handler))
(defun flavor-property-list-handler (op self &rest args)
  (apply #'property-list-handler op (locf (flavor-plist self)) args))

;;; These properties are not discarded by redoing a DEFFLAVOR.
(DEFCONST DEFFLAVOR1-PRESERVED-PROPERTIES
          '(ADDITIONAL-INSTANCE-VARIABLES
            ALL-INSTANCE-VARIABLES-SPECIAL
            COMPILE-FLAVOR-METHODS
            UNMAPPED-INSTANCE-VARIABLES
            MAPPED-COMPONENT-FLAVORS
            INSTANCE-VARIABLE-INITIALIZATIONS
            ALL-SPECIAL-INSTANCE-VARIABLES
            ALL-INITTABLE-INSTANCE-VARIABLES
            REMAINING-DEFAULT-PLIST
            REMAINING-INIT-KEYWORDS
            REQUIRED-INIT-KEYWORDS
            INSTANCE-AREA-FUNCTION))

;;; A little slower, but eliminates compile-time dependency on details of flavor defstruct.
(DEFUN FLAVOR-ALL-INSTANCE-VARIABLES-SLOW (FLAVOR)
  (FLAVOR-ALL-INSTANCE-VARIABLES FLAVOR))

;;; Used by other files to avoid compile-time dependency on our defstruct.
(DEFUN FLAVOR-GET (FLAVOR PROP &OPTIONAL DEFAULT)
  (GETF (FLAVOR-PLIST FLAVOR) PROP DEFAULT))

(DEFUN FLAVOR-GET-LOCATION (FLAVOR PROP)
  (LOCF (GETF (FLAVOR-PLIST FLAVOR) PROP)))

(DEFSUBST FLAVOR-GETTABLE-INSTANCE-VARIABLES (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) :GETTABLE-INSTANCE-VARIABLES))

(DEFSUBST FLAVOR-SETTABLE-INSTANCE-VARIABLES (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) :SETTABLE-INSTANCE-VARIABLES))

(DEFSUBST FLAVOR-SPECIAL-INSTANCE-VARIABLES (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) :SPECIAL-INSTANCE-VARIABLES))

(DEFSUBST FLAVOR-ALL-INSTANCE-VARIABLES-SPECIAL (FLAVOR)
  "T if all instance variables of FLAVOR must be special due to old compiled methods."
  (GETF (FLAVOR-PLIST FLAVOR) 'ALL-INSTANCE-VARIABLES-SPECIAL))

(DEFSUBST FLAVOR-ALL-SPECIAL-INSTANCE-VARIABLES (FLAVOR)
  "Return a list of all the special instance variables of FLAVOR (a flavor object)."
  (GETF (FLAVOR-PLIST FLAVOR) 'ALL-SPECIAL-INSTANCE-VARIABLES))

;;; These are instance variables that don't belong to this flavor or its components
;;; but can be accessed by methods of this flavor.
(DEFSUBST FLAVOR-ADDITIONAL-INSTANCE-VARIABLES (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'ADDITIONAL-INSTANCE-VARIABLES))

;;; The next four are distillations of info taken from this flavor and its components,
;;; used for instantiating this flavor.  See COMPOSE-FLAVOR-INITIALIZATIONS.
(DEFSUBST FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'INSTANCE-VARIABLE-INITIALIZATIONS))

(DEFSUBST FLAVOR-REMAINING-DEFAULT-PLIST (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'REMAINING-DEFAULT-PLIST))

(DEFSUBST FLAVOR-REMAINING-INIT-KEYWORDS (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'REMAINING-INIT-KEYWORDS))

(DEFSUBST FLAVOR-UNHANDLED-INIT-KEYWORDS (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'UNHANDLED-INIT-KEYWORDS))

(DEFSUBST FLAVOR-ALL-INITTABLE-INSTANCE-VARIABLES (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'ALL-INITTABLE-INSTANCE-VARIABLES))

;;; This is a vector in which the mapping table locations in the alist point.
(DEFSUBST FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'COMPONENT-MAPPING-TABLE-VECTOR))

;;; This is a list of flavors we depend on whose methods are referred
;;; to by our combined methods.
(DEFSUBST FLAVOR-MAPPED-COMPONENT-FLAVORS (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'MAPPED-COMPONENT-FLAVORS))

;;; This is a list of instance variables which are ordered
;;; because of an :ORDERED-INSTANCE-VARIABLES declaration in some flavor we depend on.
;;; They do not need to be mapped in mapping tables.
(DEFSUBST FLAVOR-UNMAPPED-INSTANCE-VARIABLES (FLAVOR)
  (GETF (FLAVOR-PLIST FLAVOR) 'UNMAPPED-INSTANCE-VARIABLES))

;;; Called by open-compiled TYPEP if second arg is a flavor name.
(DEFUN TYPEP-FLAVOR (X TYPE &AUX FL)
  (COND ((AND (INSTANCEP X)
              (= (%P-DATA-TYPE (SETQ FL (%P-CONTENTS-AS-LOCATIVE-OFFSET X 0)))
                 DTP-ARRAY-HEADER)
              (EQ (AREF (SETQ FL (%MAKE-POINTER DTP-ARRAY-POINTER FL)) 0) 'FLAVOR))
         (NOT (NULL (MEMQ TYPE (FLAVOR-DEPENDS-ON-ALL FL)))))
        ((GET TYPE 'FLAVOR) NIL)
        (T (TYPEP X TYPE))))                    ;Optimization turned out to be wrong

;(DEFVAR FLAVOR-DATA-AREA (MAKE-AREA :NAME 'FLAVOR-DATA-AREA
;                                   :REGION-SIZE #o100000
;                                   :REPRESENTATION :LIST)
;  "Area for flavor plists and other lists associated with a flavor definition.")

(defun rehash-flavor-method-hash-arrays ()
  (loop for flavor-name in *all-flavor-names*
        for flavor = (get flavor-name 'flavor)
     when flavor
       do (unless (symbolp (flavor-method-hash-array flavor))
            (send (hash-array-hash-table-instance (flavor-method-hash-array flavor))
                  :get-hash nil))))

(add-initialization "Rehash method hash arrays"
                    '(rehash-flavor-method-hash-arrays)
                    '(:after-full-gc))

;;; Format of flavor-method-table:
;;;  New format of a flavor-method-table entry is:
;;;    (message combination-type combination-order meth...)
;;;  A meth is:
;;;    (function-spec definition plist)
;;;  Thus the second element of a meth is actually a function-cell.
;;;  The meth's are stored in permanent-storage-area so that they will be compact.
;;;     [That might not be the best area, the select-methods, and component
;;;      lists, and instanc-variable lists, and which-operations's, are also there.]
;;;  A magic-list entry is:
;;;    (message combination-type combination-order (method-type function-spec...)...)
;;;  In the magic-list, there can be more than one method listed under a method-type,
;;;  the base flavor always comes first.  The :COMBINED methods are elided from
;;;  the magic-list.
;;;
;;;  Special method-types:
;;;    NIL - no type specified
;;;    :DEFAULT - like NIL but only taken if there are no type-NIL methods
;;;    :WRAPPER - wrappers are remembered this way
;;;    :COMBINED - a combined method; it has a debug info entry
;;;             (COMBINED-METHOD-DERIVATION derivation) or else the function spec
;;;             has a property COMBINED-METHOD-DERIVATION whose value is the derivation.
;;;             The derivation is the magic list entry used to make the combined method.
;;;             The CDDDR is canonicalized; each contained list of method symbols is
;;;             of course ordered by the order in which flavors are combined (base
;;;             flavor first).  Canonical order is alphabetical by method-type.
;;;  Non-special method-types:
;;;    :BEFORE, :AFTER - these are used by the default combination-type, :DAEMON
;;;
;;;  Special hair for wrappers: changing a wrapper can invalidate the combined method
;;;  without changing anything in the flavor-method-table entry.  Rather than having
;;;  it automatically recompile, which turns out to be a pain when the wrapper was
;;;  just reloaded or changed trivially, it will fail to recompile and you must use
;;;  RECOMPILE-FLAVOR with a 3rd argument of NIL.
;;;
;;;  A combination-type of NIL means it has not been explicitly specified.
;;;
;;;  Method-combination functions.  Found on the SI:METHOD-COMBINATION property
;;;  of the combination-type.  These are passed the flavor structure, and the
;;;  magic-list entry, and must return the function spec to use as the handler.
;;;  It should also define or compile thew definition for that function spec if nec.
;;;  This function interprets combination-type-arg,
;;;  which for many combination-types is either :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.
;;;
;;; This is an a-list from method type to function to write the code to go
;;; in the combined method.  Users can add to this.
;;; These types of method are added to the combined method
;;; in the order they are listed here.
;;; So if one flavor defines a wrapper and an :around method,
;;; the wrapper goes outside.
(DEFCONST *SPECIALLY-COMBINED-METHOD-TYPES*
          '((:AROUND PUT-AROUND-METHOD-INTO-COMBINED-METHOD)
            (:WRAPPER PUT-WRAPPER-INTO-COMBINED-METHOD)))

;;; These specially combined method types go in with base flavor outermost.
(DEFCONST *INVERSE-SPECIALLY-COMBINED-METHOD-TYPES*
          '((:INVERSE-AROUND PUT-AROUND-METHOD-INTO-COMBINED-METHOD)
            (:INVERSE-WRAPPER PUT-WRAPPER-INTO-COMBINED-METHOD)))

;;; Definitions of a meth (the datum which stands for a method)
(DEFSTRUCT (METH :LIST :CONC-NAME (:CONSTRUCTOR NIL) (:ALTERANT NIL))
                ;No constructor because defstruct doesn't let me specify the area
  FUNCTION-SPEC
  DEFINITION
  (PLIST NIL))

;;; If there is no definition, it contains DTP-NULL and a pointer to the meth

;;; Extract the method-type of a meth
(DEFSUBST METH-METHOD-TYPE (METH)
  (AND (CDDDR (METH-FUNCTION-SPEC METH))
       (THIRD (METH-FUNCTION-SPEC METH))))

(DEFSUBST METH-METHOD-SUBTYPE (METH)
  (FIFTH (METH-FUNCTION-SPEC METH)))

;;; Return a meth of specified type from a list of meth's.
(DEFUN METH-LOOKUP (METH-LIST METHOD-TYPE &OPTIONAL METHOD-SUBTYPE)
  (LOOP FOR METH IN METH-LIST
        WHEN (AND (EQ (METH-METHOD-TYPE METH) METHOD-TYPE)
                  (OR (NOT METHOD-SUBTYPE)
                      (EQ (METH-METHOD-SUBTYPE METH) METHOD-SUBTYPE)))
          RETURN METH))

(DEFUN NULLIFY-METHOD-DEFINITION (METH)
  (LET ((P (LOCF (METH-DEFINITION METH))))
    (WITHOUT-INTERRUPTS
      (%P-STORE-POINTER P METH)
      (%P-STORE-DATA-TYPE P DTP-NULL))))

(DEFUN METH-DEFINEDP (METH)
  (AND (LOCATION-BOUNDP (LOCF (METH-DEFINITION METH)))
       (NEQ (METH-DEFINITION METH) 'UNDEFINITION-IN-PROGRESS)))

(DEFUN METHOD-PLIST (FUNCTION-SPEC)             ;For debugging ease only
  (METH-PLIST (FLAVOR-METHOD-ENTRY FUNCTION-SPEC T)))

(DEFPROP DEFFLAVOR "Flavor" DEFINITION-TYPE-NAME)

;;; Function to define or redefine a flavor (used by DEFFLAVOR macro).
;;; Note that to ease initialization problems, the flavors depended upon need
;;; not be defined yet.  You will get an error the first time you try to create
;;; an instance of this flavor if a flavor it depends on is still undefined.
;;; When redefining a flavor, we reuse the same FLAVOR defstruct so that
;;; old instances continue to get the latest methods, unless you change
;;; something incompatibly, in which case you will get a warning.
(DEFPROP DEFFLAVOR1 T QFASL-DONT-RECORD)
(DEFUN DEFFLAVOR1 (FLAVOR-NAME INSTANCE-VARIABLES COMPONENT-FLAVORS OPTIONS
                   &AUX FFL ALREADY-EXISTS INSTV IDENTICAL-COMPONENTS
                        GETTABLE SETTABLE INITTABLE SPECIAL-IVS
                        OLD-SPECIAL-IVS OLD-DEFAULT-HANDLER
                        OLD-DEFAULT-INIT-PLIST OLD-LOCAL-IVS OLD-INITTABLE-IVS
                        OLD-INIT-KWDS OLD-INSTANCE-AREA-FUNCTION
                        OLD-REQUIRED-INIT-KEYWORDS
                        INIT-KEYWORDS INCLUDES METH-COMB
                        NEW-PLIST (PL (LOCF NEW-PLIST))
                        (DEFAULT-CONS-AREA
                          (IF *JUST-COMPILING* DEFAULT-CONS-AREA
                            *FLAVOR-AREA*)))
  (OR *JUST-COMPILING* (RECORD-SOURCE-FILE-NAME FLAVOR-NAME 'DEFFLAVOR))
  (WITHOUT-INTERRUPTS
    (COND ((AND (NOT *JUST-COMPILING*)
                (NOT (MEMQ FLAVOR-NAME *ALL-FLAVOR-NAMES*)))
           (PUSH FLAVOR-NAME *ALL-FLAVOR-NAMES*)
           ;; Push on the name without the package prefix.
           (VECTOR-PUSH-EXTEND (CONS (SYMBOL-NAME FLAVOR-NAME) FLAVOR-NAME)
                              *ALL-FLAVOR-NAMES-AARRAY*)
           ;; Push on the name with the package prefix.
           (VECTOR-PUSH-EXTEND (LET ((*PACKAGE* NIL))
                                (CONS (FORMAT NIL "~S" FLAVOR-NAME) FLAVOR-NAME))
                              *ALL-FLAVOR-NAMES-AARRAY*)
           ;; Array is no longer sorted.
           (SETF (ARRAY-LEADER *ALL-FLAVOR-NAMES-AARRAY* 1) NIL))))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (SETQ INSTV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) INSTANCE-VARIABLES))
  (DOLIST (IV INSTV)
    (IF (OR (NULL IV) (NOT (SYMBOLP IV)))
        (FERROR "~:S, which is not a symbol, was specified as an instance variable" IV)))
  (DOLIST (CF COMPONENT-FLAVORS)
    (IF (OR (NULL CF) (NOT (SYMBOLP CF)))
        (FERROR "~:S, which is not a symbol, was specified as a component flavor" CF)))
  ;; Certain properties are inherited from the old property list, while
  ;; others are generated afresh each time from the defflavor-options.
  (WHEN (AND (SETQ ALREADY-EXISTS (COMPILATION-FLAVOR FLAVOR-NAME))
             *USE-OLD-FLAVOR-INFO*)
    (DOLIST (PROP DEFFLAVOR1-PRESERVED-PROPERTIES)
      (SETF (GET PL PROP) (GETF (FLAVOR-PLIST ALREADY-EXISTS) PROP))))
  ;; First, parse all the defflavor options into local variables so we can see
  ;; whether the flavor is being redefined incompatibly.
  (DO ((L OPTIONS (CDR L))
       (OPTION) (ARGS))
      ((NULL L))
    (IF (ATOM (CAR L))
        (SETQ OPTION (CAR L) ARGS NIL)
      (SETQ OPTION (CAAR L) ARGS (CDAR L)))
    (SELECTQ OPTION
      (:GETTABLE-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ GETTABLE (UNION-EQ GETTABLE (OR ARGS INSTV))))
      (:SETTABLE-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ SETTABLE (UNION SETTABLE (OR ARGS INSTV))))
      ((:INITTABLE-INSTANCE-VARIABLES :INITABLE-INSTANCE-VARIABLES)
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ INITTABLE (UNION-EQ INITTABLE (OR ARGS INSTV))))
      (:SPECIAL-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (SETQ SPECIAL-IVS (UNION-EQ SPECIAL-IVS (OR ARGS INSTV))))
      (:INIT-KEYWORDS
       (SETQ INIT-KEYWORDS (UNION-EQ INIT-KEYWORDS ARGS)))
      (:INCLUDED-FLAVORS
       (SETQ INCLUDES (UNION-EQ INCLUDES ARGS)))
      (:NO-VANILLA-FLAVOR
       (SETF (GET PL :NO-VANILLA-FLAVOR) T))
      (:ORDERED-INSTANCE-VARIABLES
       ;;Don't validate.  User may reasonably want to specify non-local instance
       ;;variables, and any bogus names here will get detected by COMPOSE-FLAVOR-COMBINATION
       ;;(VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (PUTPROP PL (OR ARGS INSTV) ':ORDERED-INSTANCE-VARIABLES))
      (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
       (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (PUTPROP PL (UNION-EQ (GET PL ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
                             (OR ARGS INSTV))
                ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES))
      (:METHOD-COMBINATION
       (SETQ METH-COMB (NUNION-EQUAL METH-COMB ARGS)))
      (:DEFAULT-HANDLER
       (PUTPROP PL (CAR ARGS) OPTION))
      ((:REQUIRED-INSTANCE-VARIABLES :REQUIRED-METHODS
                                     :REQUIRED-FLAVORS :REQUIRED-INIT-KEYWORDS)
       (PUTPROP PL (UNION-EQ ARGS (GET PL OPTION)) OPTION))
      ((:method-order :select-method-order)
       (putprop pl args ':method-order))
      ((:DOCUMENTATION :DEFAULT-INIT-PLIST :ACCESSOR-PREFIX)
       (PUTPROP PL ARGS OPTION))
      (:ALIAS-FLAVOR
       (PUTPROP PL T ':ALIAS-FLAVOR))
      (:ABSTRACT-FLAVOR
       (PUTPROP PL T ':ABSTRACT-FLAVOR))
      (:INSTANCE-AREA-FUNCTION
       (PUTPROP PL (CAR ARGS) ':INSTANCE-AREA-FUNCTION))
      (:INSTANTIATION-FLAVOR-FUNCTION
       (PUTPROP PL (CAR ARGS) ':INSTANTIATION-FLAVOR-FUNCTION))
      ((:RUN-TIME-ALTERNATIVES :MIXTURE)
       (PUTPROP PL ARGS ':RUN-TIME-ALTERNATIVES)
       (PUTPROP PL 'CHOOSE-RUN-TIME-ALTERNATIVE ':INSTANTIATION-FLAVOR-FUNCTION)
       (PUTPROP PL (MAKE-RUN-TIME-ALTERNATIVE-ALIST FLAVOR-NAME ARGS)
                'RUN-TIME-ALTERNATIVE-ALIST))
      (OTHERWISE (FERROR "~S is not a known ~S option." OPTION 'DEFFLAVOR))))
  ;; All settable instance variables should also be gettable and inittable.
  (DOLIST (V SETTABLE)
    (OR (MEMQ V GETTABLE)
        (PUSH V GETTABLE))
    (OR (MEMQ V INITTABLE)
        (PUSH V INITTABLE)))
  ;; See whether there are any changes in component flavor structure from last time
  (SETQ IDENTICAL-COMPONENTS
        (AND ALREADY-EXISTS
             *USE-OLD-FLAVOR-INFO*
             (EQUAL COMPONENT-FLAVORS (FLAVOR-DEPENDS-ON ALREADY-EXISTS))
             (EQUAL INCLUDES (FLAVOR-INCLUDES ALREADY-EXISTS))
             (EQUAL (GET PL :REQUIRED-FLAVORS)
                    (GETF (FLAVOR-PLIST ALREADY-EXISTS) :REQUIRED-FLAVORS))))
  (AND ALREADY-EXISTS
       (SETQ OLD-SPECIAL-IVS (FLAVOR-SPECIAL-INSTANCE-VARIABLES ALREADY-EXISTS)
             OLD-DEFAULT-HANDLER (GETF (FLAVOR-PLIST ALREADY-EXISTS) :DEFAULT-HANDLER)
             OLD-DEFAULT-INIT-PLIST (GETF (FLAVOR-PLIST ALREADY-EXISTS) :DEFAULT-INIT-PLIST)
             OLD-LOCAL-IVS (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
             OLD-INITTABLE-IVS (FLAVOR-INITTABLE-INSTANCE-VARIABLES ALREADY-EXISTS)
             OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET ALREADY-EXISTS :INSTANCE-AREA-FUNCTION)
             OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET ALREADY-EXISTS :REQUIRED-INIT-KEYWORDS)
             OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS ALREADY-EXISTS)))
  ;; If the flavor is being redefined, and the number or order of instance variables
  ;; is being changed, and this flavor or any that depends on it
  ;; has a select-method table (i.e. has probably been instantiated), give a warning
  ;; and disconnect from the old FLAVOR defstruct so that old instances will
  ;; retain the old information.  The instance variables can get changed either
  ;; locally or by rearrangement of the component flavors.
  (AND ALREADY-EXISTS
       (IF (AND *USE-OLD-FLAVOR-INFO*
                (EQUAL (GET PL :ORDERED-INSTANCE-VARIABLES)
                       (GETF (FLAVOR-PLIST ALREADY-EXISTS) :ORDERED-INSTANCE-VARIABLES))
                (OR (EQUAL (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
                           INSTANCE-VARIABLES)
                    (EQUAL (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
                                   (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS))
                           INSTV))
                (EQ (GET PL :ALIAS-FLAVOR)
                    (FLAVOR-GET ALREADY-EXISTS :ALIAS-FLAVOR))
                (OR IDENTICAL-COMPONENTS
                    (EQUAL (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
                                                       COMPONENT-FLAVORS INCLUDES)
                           (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
                                                       (FLAVOR-DEPENDS-ON ALREADY-EXISTS)
                                                       (FLAVOR-INCLUDES ALREADY-EXISTS)))))
           (IF *JUST-COMPILING*
               (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS NIL)))
         (IF *JUST-COMPILING*
             (SETQ ALREADY-EXISTS (FLAVOR-REDEFINITION-FOR-COMPILATION ALREADY-EXISTS T))
           (SETQ ALREADY-EXISTS (PERFORM-FLAVOR-REDEFINITION FLAVOR-NAME)))))
  (WHEN (GET PL :ALIAS-FLAVOR)
    (IF (CDR COMPONENT-FLAVORS)
        (FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS :IMPOSSIBLE
                     "This alias flavor has more than one component."))
    (UNLESS COMPONENT-FLAVORS
      (FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS :IMPOSSIBLE
                   "This alias flavor has no component to be the alias of."))
    (IF INSTANCE-VARIABLES
        (FLAVOR-WARN FLAVOR-NAME 'ALIAS-FLAVOR-MULTIPLE-COMPONENTS :IMPOSSIBLE
                     "This alias flavor has instance variables; they will be ignored.")))
  ;; Make the information structure unless the flavor already exists.
  (LET ((FL (OR ALREADY-EXISTS
                (AND (NOT *JUST-COMPILING*)
                     (GET FLAVOR-NAME 'UNDEFINED-FLAVOR))
                (MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))))
    (SETF (FLAVOR-PACKAGE FL) *PACKAGE*)
    (SETF (FLAVOR-LOCAL-INSTANCE-VARIABLES FL) INSTANCE-VARIABLES)
    (SETF (FLAVOR-DEPENDS-ON FL) COMPONENT-FLAVORS)
    (LET ((OVEC (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL)))
      (SETF (FLAVOR-PLIST FL) NEW-PLIST)
      (IF OVEC (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL) OVEC)))
    (IF GETTABLE
        (SETF (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL) GETTABLE))
    (IF SETTABLE
        (SETF (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL) SETTABLE))
    (IF SPECIAL-IVS
        (SETF (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL) SPECIAL-IVS))
    (SETF (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL)
          (LOOP FOR V IN INITTABLE COLLECT (CONS (CORRESPONDING-KEYWORD V) V)))
    (SETF (FLAVOR-INIT-KEYWORDS FL) INIT-KEYWORDS)
    (SETF (FLAVOR-INCLUDES FL) INCLUDES)
    ;; This can't be computed for real until flavor composition,
    ;; but this at least contains some of the right ones.
    (SETF (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FL)
          (FLAVOR-KNOWN-UNMAPPED-INSTANCE-VARIABLES FL))
    ;; First remove old method-combination declarations, then add new ones
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
      (COND ((LOOP FOR DECL IN METH-COMB NEVER (MEMQ (CAR MTE) (CDDR DECL)))
             (SETF (SECOND MTE) NIL)
             (SETF (THIRD MTE) NIL))))
    (DOLIST (DECL METH-COMB)
      (LET ((TYPE (CAR DECL)) (ORDER (CADR DECL)) ELEM)
        ;; Don't error-check TYPE now, its definition might not be loaded yet
        (DOLIST (MSG (CDDR DECL))
          (OR (SETQ ELEM (ASSQ MSG (FLAVOR-METHOD-TABLE FL)))
              (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
          (SETF (SECOND ELEM) TYPE)
          (SETF (THIRD ELEM) ORDER))))
    (IF *JUST-COMPILING*
        (COMPILATION-DEFINE-FLAVOR FLAVOR-NAME FL)
      ;; Make this a depended-on-by of its depends-on, or remember to do it later in
      ;; the case of depends-on's not yet defined.
      (DOLIST (COMPONENT-FLAVOR COMPONENT-FLAVORS)
        (WITHOUT-INTERRUPTS
          (COND ((SETQ FFL (GET COMPONENT-FLAVOR 'FLAVOR))
                 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
                     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
                (T (PUSH (CONS COMPONENT-FLAVOR FLAVOR-NAME)
                         *FLAVOR-PENDING-DEPENDS*)))))
      ;; Likewise for its includes
      (DOLIST (INCLUDED-FLAVOR (FLAVOR-INCLUDES FL))
        (WITHOUT-INTERRUPTS
          (COND ((SETQ FFL (GET INCLUDED-FLAVOR 'FLAVOR))
                 (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
                     (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
                (T (PUSH (CONS INCLUDED-FLAVOR FLAVOR-NAME)
                         *FLAVOR-PENDING-DEPENDS*)))))
      ;; If someone depends on this flavor, which wasn't defined until now, link them up.
      ;; If that flavor was flavor-composed, recompose it now.
      (WITHOUT-INTERRUPTS
        (DOLIST (X *FLAVOR-PENDING-DEPENDS*)
          (COND ((EQ (CAR X) FLAVOR-NAME)
                 (OR (MEMQ (CDR X) (FLAVOR-DEPENDED-ON-BY FL))
                     (PUSH (CDR X) (FLAVOR-DEPENDED-ON-BY FL)))
                 (SETQ *FLAVOR-PENDING-DEPENDS*
                       (DELQ X *FLAVOR-PENDING-DEPENDS*))))))
      (SETF (GET FLAVOR-NAME 'FLAVOR) FL)
      (REMPROP FLAVOR-NAME 'UNDEFINED-FLAVOR)
      ;; Now, if the flavor was redefined in a way that changes the methods but doesn't
      ;; invalidate old instances, we have to propagate some changes.
      (IF (AND ALREADY-EXISTS
               (NOT IDENTICAL-COMPONENTS))
          (PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION FLAVOR-NAME)
        ;; If the methods and instances are ok but other things have changed, notice that too.
        (OR (AND (EQUAL OLD-SPECIAL-IVS
                        (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL))
                 (EQUAL OLD-DEFAULT-INIT-PLIST
                        (GETF (FLAVOR-PLIST FL) :DEFAULT-INIT-PLIST))
                 (EQUAL OLD-LOCAL-IVS
                        (FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
                 ;; Get a warning every time, if there is a variable
                 ;; that is globally special but not in a :SPECIAL-INSTANCE-VARIABLES
                 (NOT (DOLIST (IV (FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
                        ;; Elements can be lists (var init)
                        (IF (CONSP IV) (SETQ IV (CAR IV)))
                        (AND (GET IV 'SPECIAL)
                             (NOT (MEMQ IV (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL)))
                             (RETURN T))))
                 (EQUAL OLD-INITTABLE-IVS
                        (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL))
                 (EQUAL OLD-DEFAULT-HANDLER (GETF (FLAVOR-PLIST FL) :DEFAULT-HANDLER))
                 (EQUAL OLD-INSTANCE-AREA-FUNCTION (FLAVOR-GET FL :INSTANCE-AREA-FUNCTION))
                 (EQUAL OLD-REQUIRED-INIT-KEYWORDS (FLAVOR-GET FL :REQUIRED-INIT-KEYWORDS))
                 (EQUAL OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS FL)))
            (PERFORM-FLAVOR-BINDINGS-REDEFINITION FLAVOR-NAME)))
      (flavor-hack-documentation flavor-name))
    FLAVOR-NAME))

(DEFUN FLAVOR-KNOWN-UNMAPPED-INSTANCE-VARIABLES (FL)
  "Determines as many of FL's ordered instance variables as it can at a
time when FL's components need not all be defined.  This is used to init
FL's unmapped instance variables list at defflavor time.  That list's
final value will be computed when FL is composed.  This is so that
methods of FL loaded before FL is composed will not need to make mapping
table entries for these instance variables."
  (LET ((FLS (APPEND (FLAVOR-DEPENDS-ON FL)
                     (FLAVOR-GET FL :REQUIRED-FLAVORS)))
        (ORDS (FLAVOR-GET FL :ORDERED-INSTANCE-VARIABLES)))
    (DOLIST (F FLS)
      (SETQ F (GET F 'FLAVOR))
      (WHEN F
        (LET ((ORD (FLAVOR-UNMAPPED-INSTANCE-VARIABLES F)))
          ;; Merge into existing order requirement.  Shorter of the two must be
          ;; a prefix of the longer, and we take the longer.
          (DO ((L1 ORD (CDR L1))
               (L2 ORDS (CDR L2)))
              (NIL)
            (COND ((NULL L1) (RETURN NIL))
                  ((NULL L2) (RETURN (SETQ ORDS ORD)))
                  ((NEQ (CAR L1) (CAR L2))
                   (FERROR "~S conflict, ~S vs ~S."
                           :ORDERED-INSTANCE-VARIABLES (CAR L1) (CAR L2))))))))
    ORDS))

;;; Check for typos in user-specified lists of instance variables.
;;; This assumes that only locally-specified (not inherited) instance variables
;;; may be mentioned in DEFFLAVOR declaration clauses.
(DEFUN VALIDATE-INSTANCE-VARIABLES-SPEC (VARS-SPECD VARS-ALLOWED FLAVOR-NAME OPTION)
  (DOLIST (VAR VARS-SPECD)
    (OR (MEMQ VAR VARS-ALLOWED)
        (FLAVOR-WARN FLAVOR-NAME 'NONEXISTENT-INSTANCE-VARIABLE :IMPOSSIBLE
                     "~S includes instance variable ~S, which this flavor lacks."
                     OPTION VAR))))

(DEFUN FLAVOR-WARN (FLAVOR-NAME TYPE SEVERITY FORMAT-STRING &REST FORMAT-ARGS)
  (IF OBJECT-WARNINGS-OBJECT-NAME
      (APPLY #'COMPILER:WARN TYPE SEVERITY FORMAT-STRING FORMAT-ARGS)
    (FORMAT *ERROR-OUTPUT* "~&In the definition of flavor ~S,~%" FLAVOR-NAME)
    (APPLY #'FORMAT *ERROR-OUTPUT* FORMAT-STRING FORMAT-ARGS)))

;;; List of those components which affect the names, number, and ordering of the
;;; instance variables.  Don't worry about undefined components, by definition
;;; they must be different from the already-existing flavor, so the right
;;; thing will happen.  (I wonder what that comment means?  Undefined components
;;; will not even appear in the list.)
(DEFUN FLAVOR-RELEVANT-COMPONENTS (FL COMPONENT-FLAVORS INCLUDED-FLAVORS)
  (%BIND (LOCF (FLAVOR-DEPENDS-ON FL)) COMPONENT-FLAVORS)
  (%BIND (LOCF (FLAVOR-INCLUDES FL)) INCLUDED-FLAVORS)
  (DEL-IF-NOT #'(LAMBDA (FLAVOR)                ;Splice out the uninteresting ones
                  (LET ((TEM (COMPILATION-FLAVOR FLAVOR)))
                    (OR (NULL TEM)
                        (FLAVOR-LOCAL-INSTANCE-VARIABLES TEM))))
              (COMPOSE-FLAVOR-INCLUSION (FLAVOR-NAME FL) NIL)))

(DEFUN FLAVOR-REDEFINITION-FOR-COMPILATION (OLD-FLAVOR NEW-COMPONENTS-P)
  "Prepare for compile-time redefinition of a flavor.
Copies the flavor, but installs the copy only for the current compilation."
  NEW-COMPONENTS-P
  (LET ((NEW-FLAVOR (MAKE-FLAVOR FLAVOR-NAME (FLAVOR-NAME OLD-FLAVOR))))
    (COPY-ARRAY-CONTENTS OLD-FLAVOR NEW-FLAVOR)
    ;; Do copy any combined methods.  If we have dependents also in this file
    ;; and they have COMPILE-FLAVOR-METHODS in this file,
    ;; they will want to see our combined methods in case they can use them.
    (COPY-METHOD-TABLE OLD-FLAVOR NEW-FLAVOR NIL)
    (SETF (FLAVOR-INSTANCE-SIZE NEW-FLAVOR) NIL)        ;Defuse error check
    (SETF (FLAVOR-DEPENDS-ON-ALL NEW-FLAVOR) NIL)       ;Will need to be flavor-composed again
    ;; Cause an error if these are looked at before they are valid.
    (SETF (FLAVOR-ALL-INSTANCE-VARIABLES NEW-FLAVOR) 'NOT-COMPUTED)
    (SETF (FLAVOR-DEPENDED-ON-BY NEW-FLAVOR) 'COMPILATION)
    (SETF (FLAVOR-METHOD-HASH-ARRAY NEW-FLAVOR) NIL)    ;Will need to be method-composed again
    (SETF (FLAVOR-WHICH-OPERATIONS NEW-FLAVOR) NIL)
    NEW-FLAVOR))

(DEFUN COPY-METHOD-TABLE (OLD-FLAVOR NEW-FLAVOR DISCARD-COMBINED-METHODS)
  (LET ((L (COPYLIST (FLAVOR-METHOD-TABLE OLD-FLAVOR)))
        (METH-AREA
          (IF *JUST-COMPILING* DEFAULT-CONS-AREA PERMANENT-STORAGE-AREA)))
    (DO ((TAIL L (CDR TAIL)))
        ((NULL TAIL))
      ;; Copy the method-table element, including the list of METH's.
      (SETF (CAR TAIL) (COPYLIST (CAR TAIL)))
      (IF DISCARD-COMBINED-METHODS
          ;; Flush from the copy all combined methods.
          (DO ((TAIL2 (CDDDR (CAR TAIL)) (CDR TAIL2)))
              ((NULL TAIL2))
            (AND (EQ (METH-METHOD-TYPE (CAR TAIL2)) :COMBINED)
                 (SETF (CDDDAR TAIL)
                       (DELQ (CAR TAIL2) (CDDDAR TAIL))))))
      ;; Now copy each METH that we didn't delete.
      ;; Copying a METH is not trivial because it can contain a DTP-NULL.
      (DO ((TAIL2 (CDDDR (CAR TAIL)) (CDR TAIL2)))
          ((NULL TAIL2))
        (LET ((NEW-METH (LIST-IN-AREA METH-AREA
                                      (FIRST (CAR TAIL2))
                                      NIL
                                      (COPYLIST (THIRD (CAR TAIL2))))))
          (IF (METH-DEFINEDP (CAR TAIL2))
              (SETF (METH-DEFINITION NEW-METH) (METH-DEFINITION (CAR TAIL2)))
            (NULLIFY-METHOD-DEFINITION NEW-METH))
          (SETF (CAR TAIL2) NEW-METH))))
    (SETF (FLAVOR-METHOD-TABLE NEW-FLAVOR) L)))

;;; Record a flavor definition, during compiling a file.
;;; Instead of setting the name's FLAVOR property, we put an entry on the
;;; FLAVORS element in the FILE-LOCAL-DECLARATIONS, where COMPILATION-FLAVOR looks.

(DEFUN COMPILATION-DEFINE-FLAVOR (FLAVOR-NAME FL)
  (putdecl flavor-name 'FLAVOR fl))

;;; Call here when a flavor has been changed in a way that is not compatible
;;; with old instances of this flavor or its dependents.
;;; Arranges for those old instances to keep the old flavor structures and methods.
;;; Return new copy of the FLAVOR defstruct, and propagate to those that depend on it.
;;; Note that we tell copy-method-table to discard our combined methods.
;;; This is because they point to METHs in our method table,
;;; so we must make new combined methods that point at our new method table.
(DEFUN PERFORM-FLAVOR-REDEFINITION (FLAVOR-NAME &OPTIONAL FOR-UNDEFFLAVOR-P &AUX FL NFL)
  (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
  (COND ((FLAVOR-METHOD-HASH-ARRAY FL)
         (SETQ NFL (MAKE-FLAVOR))
         (COPY-ARRAY-CONTENTS FL NFL)
         (COPY-METHOD-TABLE FL NFL T)                      ;Copy, but discard combined methods
         (SETQ FL NFL)
         (SETF (FLAVOR-PLIST FL) (COPYLIST (FLAVOR-PLIST FL) PROPERTY-LIST-AREA))
         (SETF (FLAVOR-MAPPED-INSTANCE-VARIABLES FL)
               (COPYLIST (FLAVOR-MAPPED-INSTANCE-VARIABLES FL)))
         (REMF (FLAVOR-PLIST FL) 'MAPPED-COMPONENT-FLAVORS)
                                                           ;They are used only by the combined
                                                           ;methods, which we just flushed.
         (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST FL) NIL)
         (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR FL) NIL)
         (SETF (GET FLAVOR-NAME 'FLAVOR) FL)
         (FORMAT *ERROR-OUTPUT*
                 (IF FOR-UNDEFFLAVOR-P
                     "~&Flavor ~S no longer instantiable; old instances are not affected.~%"
                   "~&Flavor ~S changed incompatibly; old instances will not get the new version.~%")
                 FLAVOR-NAME))
        ;; Even if this flavor wasn't instantiated,
        ;; probably some of its dependents were,
        ;; and their hash arrays and combined methods point to our method table.
        (T (COPY-METHOD-TABLE FL FL T)))
  (SETF (FLAVOR-INSTANCE-SIZE FL) NIL)  ;Defuse error check
  (SETF (FLAVOR-DEPENDS-ON-ALL FL) NIL) ;Will need to be flavor-composed again
  (SETF (FLAVOR-METHOD-HASH-ARRAY FL) NIL)      ;Will need to be method-composed again
  (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL)
  (DOLIST (FN (FLAVOR-DEPENDED-ON-BY FL))
    (PERFORM-FLAVOR-REDEFINITION FN FOR-UNDEFFLAVOR-P))
  FL)

;;; This one is when the old instances don't have to be discarded, but recomposition
;;; does have to occur because something was changed in the order of flavor combination
(DEFUN PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION (FLAVOR-NAME)
  ;; If we define any combined methods, they don't "belong" to any file
  ;; that happens to be being loaded when this is called.
  (LET ((FDEFINE-FILE-PATHNAME NIL)
        (INHIBIT-FDEFINE-WARNINGS T))           ;Don't give warnings for combined methods
    ;; Reverse the list so that this flavor comes first, followed by directest descendents.
    (DOLIST (FN (REVERSE (FLAVOR-DEPENDED-ON-BY-ALL (GET FLAVOR-NAME 'FLAVOR)
                                                    (LIST FLAVOR-NAME))))
      (LET ((FL (GET FN 'FLAVOR)))
        (IF (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
        (IF (FLAVOR-METHOD-HASH-ARRAY FL) (COMPOSE-METHOD-COMBINATION FL))))))

;;; This one is when the old instances don't have to be discarded,
;;; and methods have not changed, just to check whether specialness
;;; of instance variables has changed.
(DEFUN PERFORM-FLAVOR-BINDINGS-REDEFINITION (FLAVOR-NAME)
  (DOLIST (FL1 (FLAVOR-DEPENDED-ON-BY-ALL (GET FLAVOR-NAME 'FLAVOR) (LIST FLAVOR-NAME)))
    (SETQ FL1 (GET FL1 'FLAVOR))
    (COND ((FLAVOR-METHOD-HASH-ARRAY FL1)
           (COMPOSE-FLAVOR-BINDINGS FL1)
           (COMPOSE-FLAVOR-INITIALIZATIONS FL1)))))

(DEFUN MAKE-FLAVOR-ALL-SPECIAL (FLAVOR)
  (IF (SYMBOLP FLAVOR) (SETQ FLAVOR (GET FLAVOR 'FLAVOR)))
  (COND ((NOT (FLAVOR-ALL-INSTANCE-VARIABLES-SPECIAL FLAVOR))
         (OR (FQUERY FORMAT:Y-OR-N-P-OPTIONS
                     "~&Loading old compiled methods for flavor ~S.  Make that flavor all-special? "
                     (FLAVOR-NAME FLAVOR))
             (FERROR "Loading old compiled methods which require all instance variables
to be special, for flavor ~S" (FLAVOR-NAME FLAVOR)))
         (SETF (FLAVOR-ALL-INSTANCE-VARIABLES-SPECIAL FLAVOR)
               (OR FDEFINE-FILE-PATHNAME T))
         (PERFORM-FLAVOR-BINDINGS-REDEFINITION (FLAVOR-NAME FLAVOR)))))

(DEFUN DESCRIBE-FLAVOR (FLAVOR-NAME &AUX FL)
  "Prints out descriptive information about FLAVOR-NAME including the combined list of
its component flavors."
  (CHECK-ARG FLAVOR-NAME (EQ 'FLAVOR (TYPEP (SETQ FL (IF (SYMBOLP FLAVOR-NAME)
                                                         (GET FLAVOR-NAME 'FLAVOR)
                                                         FLAVOR-NAME))))
             "a flavor or the name of one")
  (FORMAT T "~&Flavor ~S directly depends on flavors: ~:[none~;~:*~{~<~%   ~3:;~S~>~^, ~}~]~%"
            FLAVOR-NAME (FLAVOR-DEPENDS-ON FL))
  (AND (FLAVOR-INCLUDES FL)
       (FORMAT T " and directly includes ~{~<~%   ~3:;~S~>~^, ~}~%" (FLAVOR-INCLUDES FL)))
  (AND (FLAVOR-DEPENDED-ON-BY FL)
       (FORMAT T " and is directly depended on by ~{~<~%   ~3:;~S~>~^, ~}~%" (FLAVOR-DEPENDED-ON-BY FL)))
  (AND (FLAVOR-DEPENDS-ON-ALL FL)       ;If this has been computed, show it
       (FORMAT T " and directly or indirectly depends on ~{~<~%   ~3:;~S~>~^, ~}~%"
                 (FLAVOR-DEPENDS-ON-ALL FL)))
  (COND ((NOT (NULL (FLAVOR-METHOD-TABLE FL)))
         (FORMAT T "Not counting inherited methods, the methods for ~S are:~%" FLAVOR-NAME)
         (DOLIST (M (FLAVOR-METHOD-TABLE FL))
           (LET ((METHODS (SUBSET 'METH-DEFINEDP (CDDDR M))))
             (FORMAT T "   ")
             (DO ((TPL METHODS (CDR TPL))) ((NULL TPL))
               (IF (METH-METHOD-TYPE (CAR TPL))
                   (FORMAT T ":~A " (METH-METHOD-TYPE (CAR TPL))))
               (FORMAT T ":~A" (CAR M))
               (LET ((SUBOP (FIFTH (METH-FUNCTION-SPEC (CAR TPL)))))
                 (WHEN SUBOP (FORMAT T " :~A" SUBOP)))
               (IF (CDR TPL) (PRINC ", ")))
             ;; Print the method combination type if there is any.
             (AND (CADR M)
                  (FORMAT T "    :~A~@[ :~A~]" (CADR M) (CADDR M)))
             (TERPRI)))))
  (AND (FLAVOR-INSTANCE-SIZE FL)        ;If has been composed
       (FORMAT T "Flavor ~S has instance size ~D, " FLAVOR-NAME (FLAVOR-INSTANCE-SIZE FL)))
  (WHEN (FLAVOR-ALL-INSTANCE-VARIABLES FL)
    (OR (FLAVOR-INSTANCE-SIZE FL) (FORMAT T "Flavor ~s has " FLAVOR-NAME))
    (FORMAT T "Instance variables: ~{~<~%   ~3:;~S~>~^, ~}~%" (FLAVOR-ALL-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Automatically-generated methods to get instance variables: ~{~<~%   ~3:;~S~>~^, ~}~%"
                 (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Automatically-generated methods to set instance variables: ~{~<~%   ~3:;~S~>~^, ~}~%"
                 (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Instance variables that may be set by initialization: ~{~<~%   ~3:;~S~>~^, ~}~%"
                 (MAPCAR #'CDR (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL))))
  (AND (FLAVOR-INIT-KEYWORDS FL)
       (FORMAT T "Keywords in the :INIT message handled by this flavor: ~{~<~%   ~3:;~S~>~^, ~}~%"
                 (FLAVOR-INIT-KEYWORDS FL)))
  (FORMAT T "Defined in package ~A~%" (FLAVOR-PACKAGE FL))
  (WHEN (FLAVOR-PLIST FL)
    (FORMAT T "Properties:~%")
    (DO ((L (FLAVOR-PLIST FL) (CDDR L)))
        ((NULL L))
      (FORMAT T "~5T~S: ~S~%" (CAR L) (CADR L))))
  (COND ((NULL (FLAVOR-METHOD-HASH-ARRAY FL))
         (FORMAT T "Flavor ~S does not yet have a method hash array~%" FLAVOR-NAME))
        ((EQ T (FLAVOR-METHOD-HASH-ARRAY FL))
         (FORMAT T "Flavor ~S has been method-composed but has no hash array since it is an ~S.~%"
                 FLAVOR-NAME :ABSTRACT-FLAVOR))
        (T (FORMAT T "Flavor ~S has method hash array:~%" FLAVOR-NAME)
           (DESCRIBE (FLAVOR-METHOD-HASH-ARRAY FL)))))

(DEFUN FLAVOR-HACK-DOCUMENTATION (FLAVOR-NAME)
  (LET* ((DOC (GETF (FLAVOR-PLIST (GET FLAVOR-NAME 'FLAVOR)) :DOCUMENTATION))
         (STRINGS NIL) FOO)
    (IF DOC
        (PROGN
          (DOLIST (TEM DOC)
            (AND (STRINGP TEM)
                 (SETQ STRINGS (NCONC STRINGS (if strings (list #\return tem) (NCONS TEM))))))
          (DOLIST (TEM DOC)
            (UNLESS (STRINGP TEM)
              (SETQ STRINGS (NCONC STRINGS (LIST* (IF (AND STRINGS (NOT FOO)) #/RETURN "")
                                                  (IF FOO "" (SETQ FOO "A "))
                                                  TEM #/SPACE NIL)))))
          (IF FOO (NCONC STRINGS (LIST "Flavor.")))
          (SETF (DOCUMENTATION FLAVOR-NAME 'FLAVOR) (APPLY #'STRING-APPEND STRINGS)))
      (IF (DOCUMENTATION FLAVOR-NAME 'DEFFLAVOR)
          (SETF (DOCUMENTATION FLAVOR-NAME 'FLAVOR) NIL)))))

;;; This is the standard way of defining a method of a class,
;;; so that the code will be compiled.  Note that DEFMETHOD works for
;;; both Class methods and Flavor methods.
;;; If in place of the lambda-list you have a symbol, and the body
;;; is null, that symbol is a function which stands in for the method.
(DEFMACRO DEFMETHOD (SPEC LAMBDA-LIST . BODY)
  "Defines a method to handle a particular operation for instances of
a particular flavor.  The usual form is looks like this:

/(defmethod (FLAVORNAME METHOD-TYPE OPERATION) LAMBDA-LIST FORM1 FORM2 ...)

FLAVORNAME is a symbol which is the name of the flavor to receive the
method.  OPERATION is a keyword symbol which names the operation
handled.  METHOD-TYPE is a keyword symbol for the type of method
/(omitted when defining a primary method).

The following are some defined method types:

  (no type)   A primary method.
  :BEFORE
  :AFTER      Used for the before-daemon and after-daemon methods used
              by :DAEMON method-combination.  :BEFORE methods are called
              first, then the primary methods, then the :AFTER methods.
  :DEFAULT    If there are no untyped methods among any of the flavors
              being combined, then the :DEFAULT methods are treated as
              untyped.  If there are any untyped methods, the :DEFAULT
              methods are ignored.
  :OR
  :AND        Used for :DAEMON-WITH-OR and :DAEMON-WITH-AND method
              combination.  The :OR methods are wrapped in an OR, or the
              :AND methods are wrapped in an AND, together with the
              primary method, between the :BEFORE and :AFTER methods.
  :OVERRIDE   Allows the features of :OR method-combination to be used
              together with daemons.  If :DAEMON-WITH-OVERRIDE is
              specified :OVERRIDE methods may be used.  :OVERRIDE
              methods are executed first, until one returns non-NIL.  In
              this case that method's value(s)are returned and no
              more methods are used.  If all :OVERRIDE methods return
              NIL,  execution of :BEFORE, primary and :AFTER methods
              take place as per normal.
  :CASE       Used by :CASE method combination."
  (LET ((CLASS-NAME (CAR SPEC))
        (FUNCTION-SPEC (CONS :METHOD SPEC))
        FL)
    `(PROGN
       ;; At compile-time, add enough information so that combined-methods
       ;; can be compiled.  But don't recompile the flavor now, and don't define
       ;; methods interpretively.  Assume that the output of this compilation
       ;; will get loaded, so that the method is defined, before the flavor
       ;; next gets compiled, so that undefined methods don't get called.
       ,(AND (JUST-COMPILING)
             (COMPILATION-FLAVOR CLASS-NAME T)
             (NEQ CLASS-NAME 'VANILLA-FLAVOR)   ;This kludge avoids bootstrapping problems!
             `(EVAL-WHEN (COMPILE)
                (LET ((*JUST-COMPILING* T))
                  (FLAVOR-NOTICE-METHOD ',FUNCTION-SPEC))))
       ;; At load-time, define the method function
       ,(COND ((AND (SYMBOLP LAMBDA-LIST) (NOT (NULL LAMBDA-LIST)) (NULL BODY))
               `(FDEFINE-FOR-DEFMETHOD ',FUNCTION-SPEC ',LAMBDA-LIST T))
              ((SETQ FL (COMPILATION-FLAVOR CLASS-NAME T))
               (IF (FLAVOR-GET FL :ALIAS-FLAVOR)
                   (FERROR "Attempt to define ~S; the flavor is an alias flavor."
                           (CONS :METHOD SPEC)))
               `(DECLARE-FLAVOR-INSTANCE-VARIABLES (,CLASS-NAME T)
                  (DEFUN ,FUNCTION-SPEC ,(METHOD-ARGUMENT-LIST LAMBDA-LIST FUNCTION-SPEC)
                    . ,BODY)))
              (T ;; The non-flavor class system
                (IF (NOT (NULL (CDDR SPEC)))
                    (FERROR "~S is not a flavor" (CAR SPEC)))
                (LET ((OPERATION (CADR SPEC)))
                  (COND ((ATOM OPERATION)
                         `(PROGN . ,(DEFMETHOD-1 CLASS-NAME OPERATION LAMBDA-LIST BODY)))
                        (T
                          (COND ((EQ (CAR OPERATION) 'QUOTE)
                                 (CERROR NIL NIL :NO-VALUE
                                     "Quote used in front of operation ~S in DEFMETHOD of ~S"
                                     OPERATION CLASS-NAME)))
                          `(PROGN . ,(MAPCAN #'(LAMBDA (OP)
                                                 (DEFMETHOD-1 CLASS-NAME OP LAMBDA-LIST BODY))
                                             OPERATION))))))))))

(DEFPROP .OPERATION. T COMPILER::IGNORABLE-VARIABLE)
(DEFPROP .SUBOPERATION. T COMPILER::IGNORABLE-VARIABLE)
(DEFPROP .DAEMON-CALLER-ARGS. T COMPILER::IGNORABLE-VARIABLE)
(DEFPROP .DAEMON-MAPPING-TABLE. T COMPILER::IGNORABLE-VARIABLE)

(DEFF FDEFINE-FOR-DEFMETHOD 'FDEFINE)
(DEFPROP FDEFINE-FOR-DEFMETHOD T QFASL-DONT-RECORD)

(DEFUN METHOD-ARGUMENT-LIST (SPECIFIED-LAMBDA-LIST FUNCTION-SPEC)
  "Given an arglist specified in DEFMETHOD, return an arglist for the actual method.
This involves adding OPERATION to the front, and sometimes other things
depending on the method type"
  (CONS '.OPERATION.
        (APPEND (IF (CDDDR FUNCTION-SPEC)
                    (GET (CADDR FUNCTION-SPEC) 'IMPLICIT-METHOD-ARGUMENTS))
                SPECIFIED-LAMBDA-LIST)))

(DEFPROP :CASE (.SUBOPERATION.) IMPLICIT-METHOD-ARGUMENTS)

;;; This lets you specify code to be wrapped around the invocation of the
;;; various methods for an operation.  For example,
;;; (DEFWRAPPER (FOO-FLAVOR :OPERATION) ((ARG1 ARG2) . BODY)
;;;   `(WITH-FOO-LOCKED (SELF)
;;;      (PRE-FROBULATE SELF ARG1 ARG2)
;;;      ,@BODY
;;;      (POST-FROBULATE SELF ARG2 ARG1)))
;;; Note that the wrapper needs to be defined at both compile and run times
;;; so that compiling combined methods as part of the qfasl file works.
(DEFMACRO DEFWRAPPER ((FLAVOR-NAME OPERATION) (DEFMACRO-LAMBDA . GUTS)
                      &BODY BODY)
  "Specifies code to be wrapped around the invocation of the various
methods for an operation.  Note that the wrapper needs to be defined at
both compile and run times so that compiling combined methods as part of
the qfasl file works."
  (LET ((FUNCTION-SPEC `(:METHOD ,FLAVOR-NAME :WRAPPER ,OPERATION)))
    `(PROGN
         ;; At compile-time, add enough information so that combined-methods
         ;; can be compiled.  The compile-time definition of macros does not
         ;; go through FDEFINE, so this is necessary to record the existence
         ;; of the wrapper.
         ,(AND (COMPILATION-FLAVOR FLAVOR-NAME T)
               (JUST-COMPILING)
               `(EVAL-WHEN (COMPILE)
                  (LET ((*JUST-COMPILING* T))
                   (FLAVOR-NOTICE-METHOD ',FUNCTION-SPEC))))
         ;; The following optimization could go away if defmacro was made very smart
         ,(IF (AND (SYMBOLP DEFMACRO-LAMBDA) (STRING-EQUAL DEFMACRO-LAMBDA 'IGNORE))
              `(DEFMACRO ,FUNCTION-SPEC (IGNORE . ,GUTS)
                 . ,BODY)
            `(DEFMACRO ,FUNCTION-SPEC (ARGLISTNAME . ,GUTS)
               `(DESTRUCTURING-BIND ,',DEFMACRO-LAMBDA (CDR ,ARGLISTNAME)
                  ,,@BODY))))))

;;; This just exists to be called at compile-time from the DEFMETHOD macro,
;;; so that any combined methods generated by COMPILE-FLAVOR-METHODS will
;;; know that this method will be around at run time and should be called.
;;; Returns non-NIL if the method is really defined (not just noticed).
(DEFUN FLAVOR-NOTICE-METHOD (FUNCTION-SPEC)
  (IF (FBOUNDP 'COMPILER:COMPILATION-DEFINE)
      (COMPILER:COMPILATION-DEFINE FUNCTION-SPEC))
  (CONDITION-CASE ()
      (LET ((METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC NIL T)))
        (IF (METH-DEFINEDP METH)
            (METH-DEFINITION METH)
          (SETF (METH-DEFINITION METH) NIL)
          NIL))
    (SYS:INVALID-FUNCTION-SPEC NIL)))

;;; Find or create a method-table entry for the specified method.
;;; DONT-CREATE is NIL if method is to be created if necessary.
;;;     The flavor is "created" too, as an UNDEFINED-FLAVOR property
;;;     of the flavor name, just to record any properties of methods.
;;; COPY-FLAVOR-IF-UNDEFINED-METH says we are going to alter the METH
;;;for compilation if it is not defined, so the flavor should be copied in that case.
(DEFUN FLAVOR-METHOD-ENTRY (FUNCTION-SPEC DONT-CREATE &OPTIONAL COPY-FLAVOR-IF-UNDEFINED-METH)
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
        (FLAVOR-NAME (SECOND FUNCTION-SPEC))
        (TYPE (THIRD FUNCTION-SPEC))
        (SUBTYPE (FIFTH FUNCTION-SPEC))
        (MESSAGE (FOURTH FUNCTION-SPEC)))
    (IF (NULL MESSAGE) (SETQ MESSAGE TYPE TYPE NIL))    ;If no type
    (IF (OR (NULL MESSAGE) (NEQ (FIRST FUNCTION-SPEC) :METHOD) (> (LENGTH FUNCTION-SPEC) 5)
            (NOT (SYMBOLP FLAVOR-NAME)) (NOT (SYMBOLP TYPE)) (NOT (SYMBOLP MESSAGE))
            (NOT (SYMBOLP SUBTYPE)))
        (FERROR 'SYS:INVALID-FUNCTION-SPEC
                "~S is not a valid :METHOD function spec." FUNCTION-SPEC))
    (LET* ((FL (OR (COMPILATION-FLAVOR FLAVOR-NAME)
                   (UNLESS *JUST-COMPILING*
                     (GET FLAVOR-NAME 'UNDEFINED-FLAVOR))
                   (AND (NOT DONT-CREATE)
                        (IF *JUST-COMPILING*
                            (COMPILATION-DEFINE-FLAVOR FLAVOR-NAME
                                                       (MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))
                          (SETF (GET FLAVOR-NAME 'UNDEFINED-FLAVOR)
                                (MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))))))
           (MTE (AND FL (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FL))))
           (METH (METH-LOOKUP (CDDDR MTE) TYPE SUBTYPE)))
      ;; If we are compiling a file, don't modify an installed flavor.
      ;; Make a new flavor object just for compilation and modify it instead.
      (AND (OR (AND (NOT DONT-CREATE)
                    (NULL METH))
               (AND METH
                    COPY-FLAVOR-IF-UNDEFINED-METH
                    (NOT (METH-DEFINEDP METH))))
           *JUST-COMPILING*
           FL
           (EQ FL (GET FLAVOR-NAME 'FLAVOR))
           (COMPILATION-DEFINE-FLAVOR
             FLAVOR-NAME
             (SETQ FL (FLAVOR-REDEFINITION-FOR-COMPILATION FL NIL))))
      (AND (NULL MTE) (NOT DONT-CREATE)
           ;; Message not previously known about, put into table
           FL
           (PUSH (SETQ MTE (LIST* MESSAGE NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
      ;; Message known, search for the type entry
      (COND (METH)                              ;Known by flavor
            (DONT-CREATE NIL)                   ;Not to be created
            ((NULL FL) NIL)                     ;Create, but no flavor defined
            (T ;; Type not known, create a new meth with an unbound definition cell
             (LET ((METH (LIST-IN-AREA (IF *JUST-COMPILING*
                                           DEFAULT-CONS-AREA
                                         PERMANENT-STORAGE-AREA)
                                       ;; Copy the function spec for paging efficiency.
                                       (IF *JUST-COMPILING*
                                           FUNCTION-SPEC
                                         (COPYLIST FUNCTION-SPEC PERMANENT-STORAGE-AREA))
                                       NIL NIL)))
               (NULLIFY-METHOD-DEFINITION METH)
               (PUSH METH (CDDDR MTE))
               METH))))))

(DEFUN FLAVOR-METHOD-FUNCTION-SPECS (FLAVOR &AUX METHODS)
  "Return a list of function specs for all the methods (except combined) of FLAVOR."
  (IF (SYMBOLP FLAVOR) (SETQ FLAVOR (COMPILATION-FLAVOR FLAVOR)))
  (DOLIST (MTE (FLAVOR-METHOD-TABLE FLAVOR))
    (DOLIST (METH (CDDDR MTE))
      (OR (EQ (SI:METH-METHOD-TYPE METH) :COMBINED)
          (NOT (SI:METH-DEFINEDP METH))
          (PUSH (METH-FUNCTION-SPEC METH) METHODS))))
  METHODS)

(DEFMACRO UNDEFMETHOD (METHOD-SPEC)
  "Forcibly remove a method definition from a flavor's method table
Syntax is identical to the beginning of a defmethod for the same method."
  `(FUNDEFINE '(:METHOD . ,METHOD-SPEC)))

;;;; Interface to function-spec system
;;; (:METHOD class-name operation) refers to the method in that class for
;;;   that operation; this works for both Class methods and Flavor methods.
;;;   In the case of Flavor methods, the specification may also be of the form
;;;   (:METHOD flavor-name method-type operation).
;>> this looks incredibly much as if it would lose with multiple processes!
(DEFVAR *LAST-FASLOAD-COMBINED-METHOD* NIL)
(DEFPROP :METHOD METHOD-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2 &AUX FL)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
        (METHOD-TYPE (THIRD FUNCTION-SPEC))
        (MESSAGE (FOURTH FUNCTION-SPEC))
        (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (IF (NULL (CDDDR FUNCTION-SPEC))
        (SETQ MESSAGE (THIRD FUNCTION-SPEC) METHOD-TYPE NIL))
    (COND ((NOT (AND (SYMBOLP FLAVOR)
                     (SYMBOLP METHOD-TYPE)
                     (SYMBOLP MESSAGE)
                     ( 3 (LENGTH FUNCTION-SPEC) 5)))
           (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
             (FERROR 'SYS:INVALID-FUNCTION-SPEC
                     "The function spec ~S is invalid." FUNCTION-SPEC)))
          ((EQ T (SETQ FL (COMPILATION-FLAVOR FLAVOR)))
           ;; Silly pseudo-flavor for cold-load stream
           (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
               T
             ;;The property-list operations need to work for the editor
             (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))
          ((OR FL               ;A defined flavor
               (NOT (CLASS-SYMBOLP FLAVOR)))    ;Not defined, assume flavor
           (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
               T
             ;; Ignore FASLOAD-COMBINED methods if flavor methods composed already.
             (IF (AND FL (FLAVOR-METHOD-HASH-ARRAY FL)
                      (EQ (THIRD FUNCTION-SPEC) 'FASLOAD-COMBINED))
                 ;; This hair makes defining (INTERNAL (:METHOD FOO FASLOAD-COMBINED ...) ...)
                 ;; get ignored properly and not get an error.
                 (CASE FUNCTION
                   (FDEFINITION *LAST-FASLOAD-COMBINED-METHOD*)
                   (FDEFINEDP T)
                   (FDEFINE
                    (SETQ *LAST-FASLOAD-COMBINED-METHOD* ARG1))
                   (FDEFINITION-LOCATION (LOCF *LAST-FASLOAD-COMBINED-METHOD*))
                   (T NIL))
               ;; Otherwise refer to or define the :COMBINED method.
               (IF (EQ METHOD-TYPE 'FASLOAD-COMBINED)
                   (SETQ FUNCTION-SPEC (LIST* (FIRST FUNCTION-SPEC) FLAVOR
                                              :COMBINED (CDDDR FUNCTION-SPEC))
                         METHOD-TYPE :COMBINED))
               (LET ((METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC
                             (CASE FUNCTION
                               ((PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE)
                                NIL)            ;Create.
                               (OTHERWISE T)))))        ;Don't create
                 (OR (AND METH (METH-DEFINEDP METH))
                     (MEMQ FUNCTION '(FDEFINEDP COMPILER-FDEFINEDP
                                      PUTPROP PUSH-PROPERTY FDEFINITION-LOCATION FDEFINE
                                      GET FUNCTION-PARENT DWIMIFY))
                     (IF FL
                         (FERROR "~S is not a defined method; it is not possible to ~S it"
                                 FUNCTION-SPEC FUNCTION)
                         (FERROR "~S is neither the name of a flavor nor the name ~
                                      of a class;~% it is not possible to ~S ~S."
                                 FLAVOR FUNCTION FUNCTION-SPEC)))
                 (SELECTQ FUNCTION
                   (FDEFINE
                     (OR FL
                         (FERROR "~S is neither the name of a flavor nor the name ~
                                      of a class;~% it is not possible to ~S ~S."
                                 FLAVOR FUNCTION FUNCTION-SPEC))
                     (LET ((DEFINITION-NEW (NOT (METH-DEFINEDP METH)))
                           (OLD-DEFINITION (AND (METH-DEFINEDP METH) (METH-DEFINITION METH))))
                       (SETF (METH-DEFINITION METH) ARG1)
                       ;; If we load a method compiled before system 83,
                       ;; that expects instance variables to be bound,
                       ;; make it work by forcing this flavor to bind all variables.
                       (IF (AND (TYPEP ARG1 'COMPILED-FUNCTION)
                                (ZEROP (%P-LDB %%FEFH-GET-SELF-MAPPING-TABLE ARG1))
                                (NOT (ASSQ 'ENCAPSULATED-DEFINITION (DEBUGGING-INFO ARG1))))
                           (MAKE-FLAVOR-ALL-SPECIAL FL))
                       ;; Incrementally recompile the flavor if this is a new method, unless
                       ;; it is a :COMBINED method, which is the result of compilation,
                       ;; not a client of it.
                       (COND ((MEMQ METHOD-TYPE '(:WRAPPER :INVERSE-WRAPPER))
                              (OR (AND (CONSP OLD-DEFINITION)
                                       (FEF-EQUAL (CDR ARG1) (CDR OLD-DEFINITION)))
                                  ;; Wrapper is really changed; must recompile flavors.
                                  ;; Arrange that if we abort, the definition is set
                                  ;; to the symbol ABORTED-DEFINITION.  This is a no-op,
                                  ;; and redefining or undefining the wrapper will recompile.
                                  (LET (SUCCESS)
                                    (UNWIND-PROTECT
                                      (PROGN
                                        (RECOMPILE-FLAVOR FLAVOR MESSAGE NIL)
                                        (SETQ SUCCESS T))
                                      (OR SUCCESS
                                          (SETF (METH-DEFINITION METH)
                                                'ABORTED-DEFINITION))))))
                             ((eq method-type :case)
                ;This seems to be necessary for updated :case methods to "TAKE" in all
                ; cases.  See RG if you are interested in looking into this more.
                ; It could probably be avoided by various changes.
                              (or (and (typep arg1 'compiled-function)
                                       (typep old-definition 'compiled-function)
                                       (fef-equal arg1 old-definition))
                                  ;; Wrapper is really changed; must recompile flavors.
                                  ;; Arrange that if we abort, the definition is set
                                  ;; to the symbol ABORTED-DEFINITION.  This is a no-op,
                                  ;; and redefining or undefining the wrapper will recompile.
                                  (LET (SUCCESS)
                                    (UNWIND-PROTECT
                                      (PROGN
                                        (RECOMPILE-FLAVOR FLAVOR MESSAGE NIL)
                                        (SETQ SUCCESS T))
                                      (OR SUCCESS
                                          (SETF (METH-DEFINITION METH)
                                                'ABORTED-DEFINITION))))))
                             ((EQ METHOD-TYPE :COMBINED) NIL)
                             (DEFINITION-NEW
                              ;; This SETF, by virtue of the preceding clause,
                              ;; arranges that if we abort out before finishing recompilation
                              ;; then the recompilation will be done again if the user
                              ;; either redoes the defmethod or does undefmethod.
                              (SETF (METH-DEFINITION METH) 'ABORTED-DEFINITION)
                              (RECOMPILE-FLAVOR FLAVOR MESSAGE)
                              (SETF (METH-DEFINITION METH) ARG1))
                             ;; If method defined as a random symbol,
                             ;; must fix up hash array each time it changes.
                             ((OR (SYMBOLP OLD-DEFINITION)
                                  (SYMBOLP ARG1))
                              (RECOMPILE-FLAVOR FLAVOR MESSAGE)))))
                   (FDEFINITION (METH-DEFINITION METH))
                   (FDEFINEDP (AND METH (VALUES (METH-DEFINEDP METH)
                                                (AND (METH-DEFINEDP METH)
                                                     (METH-DEFINITION METH)))))
                   (FDEFINITION-LOCATION (LOCF (METH-DEFINITION METH)))
                   (FUNDEFINE
                    (SETF (METH-DEFINITION METH) 'UNDEFINITION-IN-PROGRESS)
                    (RECOMPILE-FLAVOR (FLAVOR-NAME FL) MESSAGE) ;Propagate the change
                    (NULLIFY-METHOD-DEFINITION METH))   ;Say propagation is complete.
                   (COMPILER-FDEFINEDP METH)
                   (GET (AND METH (GETF (METH-PLIST METH) ARG1 ARG2)))
                   (PUTPROP (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
                              (SETF (GETF (METH-PLIST METH) ARG2) ARG1)))
                   (REMPROP (REMF (METH-PLIST METH) ARG1))
                   (PUSH-PROPERTY
                    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
                      (PUSH ARG1 (GETF (METH-PLIST METH) ARG2))))
                   (DWIMIFY
                    (CATCH-CONTINUATION 'DWIMIFY-PACKAGE
                        #'(LAMBDA (NEW-SPEC) NEW-SPEC)
                        #'(LAMBDA () NIL)
                      (DOLIST (COMPONENT
                                (OR (FLAVOR-DEPENDS-ON-ALL FL)
                                    (COMPOSE-FLAVOR-COMBINATION FL NIL)))
                        (LET ((FLAVOR (COMPILATION-FLAVOR COMPONENT))
                              (METHS))
                          (AND FLAVOR
                               (SETQ METHS
                                     (CDDDR (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FLAVOR)))))
                          (DOLIST (METH METHS)
                            (AND (METH-DEFINEDP METH)
                                 (DWIMIFY-PACKAGE-2 (METH-FUNCTION-SPEC METH)
                                                    ARG1 ARG2 T)))))))
                   (OTHERWISE
                    (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))))
          (T
           (CLASS-METHOD-FUNCTION-SPEC-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

(DEFUN FEF-EQUAL (FEF1 FEF2)
  "Similar to EQUAL, but compares the contents of FEFs."
  (OR (EQUAL FEF1 FEF2)
      (AND (= (%STRUCTURE-TOTAL-SIZE FEF1) (%STRUCTURE-TOTAL-SIZE FEF2))
           (= (%STRUCTURE-BOXED-SIZE FEF1) (%STRUCTURE-BOXED-SIZE FEF2))
           (LET ((BOXED (%STRUCTURE-BOXED-SIZE FEF1))
                 (TOTAL (%STRUCTURE-TOTAL-SIZE FEF1)))
             (AND
               (DOTIMES (I BOXED T)
                 (UNLESS (AND (= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF1 I)
                                 (%P-LDB-OFFSET %%Q-DATA-TYPE FEF2 I))
                              (EQUAL (%P-SAFE-CONTENTS-OFFSET FEF1 I)
                                     (%P-SAFE-CONTENTS-OFFSET FEF2 I)))
                   (RETURN NIL)))
               (DO ((I BOXED (1+ I)))
                   ((= I TOTAL) T)
                 (OR (AND (= (%P-LDB-OFFSET (BYTE 16. 0.) FEF1 I)
                             (%P-LDB-OFFSET (BYTE 16. 0.) FEF2 I))
                          (= (%P-LDB-OFFSET (BYTE 16. 16.) FEF1 I)
                             (%P-LDB-OFFSET (BYTE 16. 16.) FEF2 I)))
                     (RETURN NIL))))))))

;;; This is left as the method definition if you abort out of the recompilation
;;; caused by defining a previously undefined method.
(DEFF ABORTED-DEFINITION 'PROG1)

;;; This is what the method definition is while the method is being FUNDEFINEd.
(DEFF UNDEFINITION-IN-PROGRESS 'PROG1)

;;;; Run-time alternative flavors.

(DEFUN GET-RUN-TIME-ALTERNATIVE-FLAVOR-NAMES (FLAVOR)
  (MAPCAR 'CDR (FLAVOR-GET FLAVOR 'RUN-TIME-ALTERNATIVE-ALIST)))

(DEFUN MAKE-RUN-TIME-ALTERNATIVE-DEFFLAVORS (FLAVOR-NAME SPECS)
  "Return a list of defflavor forms for the run-time alternatives of FLAVOR-NAME.
These are the flavors generated automatically by defining FLAVOR-NAME
and one of which you get when you instantiate FLAVOR-NAME.
SPECS should be the value of the :RUN-TIME-ALTERNATIVES option in its definition;
this function can be called before the definition is really in effect."
  (LOOP FOR ALT IN (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR-NAME SPECS)
        WHEN (AND (NOT (MEMBER-IF #'STRINGP ALT))
                  (> (LENGTH ALT) 1))
        COLLECT `(DEFFLAVOR ,(INTERN (COMBINATION-FLAVOR-NAME ALT))
                            () ,ALT)))

(DEFUN MAKE-RUN-TIME-ALTERNATIVE-ALIST (FLAVOR-NAME SPECS)
  (MAPCAR #'(LAMBDA (COMBINATION)
              (CONS COMBINATION (INTERN (COMBINATION-FLAVOR-NAME COMBINATION))))
          (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR-NAME SPECS)))

(DEFUN COMBINATION-FLAVOR-NAME (FLAVOR-LIST &AUX COMBINED-NAME)
  (DOLIST (NAME (REMOVE-DUPLICATES FLAVOR-LIST))
    (IF (STRING-EQUAL NAME "-FLAVOR" :START1 (- (STRING-LENGTH NAME) 7))
        (SETQ NAME (SUBSTRING NAME 0 (- (STRING-LENGTH NAME) 7))))
    (IF (STRING-EQUAL NAME "-MIXIN" :START1 (- (STRING-LENGTH NAME) 6))
        (SETQ NAME (SUBSTRING NAME 0 (- (STRING-LENGTH NAME) 6))))
    (IF COMBINED-NAME
        (SETQ COMBINED-NAME (STRING-APPEND COMBINED-NAME "-" NAME))
      (SETQ COMBINED-NAME NAME)))
  COMBINED-NAME)

(DEFUN MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS (FLAVOR)
  "Return a list of flavor combinations which are run-time alternatives of FLAVOR-NAME.
Each combination is a list of the flavor names to be combined."
  (LET ((SPECS (FLAVOR-GET FLAVOR :RUN-TIME-ALTERNATIVES)))
    (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR SPECS)))

(DEFUN MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 (FLAVOR-NAME SPECS)
  (IF (NULL SPECS)
      (IF FLAVOR-NAME `((,FLAVOR-NAME)) '(()))
    (LET ((REMAINING-SPECS-ALTERNATIVES
            (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 FLAVOR-NAME (CDR SPECS)))
          (THIS-SPEC-ALTERNATIVES (MAKE-RUN-TIME-ALTERNATIVES (CAR SPECS))))
      (LOOP FOR THIS-SPEC IN THIS-SPEC-ALTERNATIVES
            NCONC (LOOP FOR REMAINING IN REMAINING-SPECS-ALTERNATIVES
                        COLLECT (APPEND THIS-SPEC REMAINING))))))

(DEFUN MAKE-RUN-TIME-ALTERNATIVES (SPEC)
  (IF (CONSP (CADR SPEC))
      (LOOP FOR ALTERNATIVE IN (CDR SPEC)
            APPEND (MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1
                     (CADR ALTERNATIVE) (CDDR ALTERNATIVE)))
    `(NIL . ,(MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS-1 (CADR SPEC) (CDDR SPEC)))))

;;; Note that it is vital that the combination to be used
;;; be consed up in the same order as the combination was made by
;;; MAKE-RUN-TIME-ALTERNATIVE-COMBINATIONS, or it will not be recognized
;;; in the RUN-TIME-ALTERNATIVE-ALIST.
(DEFUN CHOOSE-RUN-TIME-ALTERNATIVE (FLAVOR INIT-PLIST)
  "This is the :INSTANTIATION-FLAVOR-FUNCTION used for run-time alternative flavors."
  (LET* ((SPECS (FLAVOR-GET FLAVOR ':RUN-TIME-ALTERNATIVES))
         (COMBINATION (CHOOSE-RUN-TIME-ALTERNATIVE-1 SPECS INIT-PLIST (FLAVOR-NAME FLAVOR))))
    (OR (CDR (ASSOC-EQUAL (APPEND COMBINATION
                                  (LIST (FLAVOR-NAME FLAVOR)))
                          (FLAVOR-GET FLAVOR 'RUN-TIME-ALTERNATIVE-ALIST)))
        (IF (MEMBER-IF #'STRINGP COMBINATION)
            (FERROR (CAR (MEMBER-IF #'STRINGP COMBINATION)))
          (FERROR "Bug in ~S processing:~%Flavor ~S, combination ~S."
                  :RUN-TIME-ALTERNATIVE FLAVOR COMBINATION)))))

(DEFUN CHOOSE-RUN-TIME-ALTERNATIVE-1 (SPECS INIT-PLIST FLAVOR-NAME)
  (LOOP FOR SPEC IN SPECS
        APPEND (CHOOSE-RUN-TIME-ALTERNATIVE-2 SPEC INIT-PLIST FLAVOR-NAME)))

(DEFUN CHOOSE-RUN-TIME-ALTERNATIVE-2 (SPEC INIT-PLIST FLAVOR-NAME)
  (LET ((VALUE (GET INIT-PLIST (CAR SPEC)))
        TEM)
    (IF (CONSP (CADR SPEC))
        (SETQ TEM (ASSQ VALUE (CDR SPEC)))
      (SELECTQ VALUE
        ((T) (SETQ TEM SPEC))
        ((NIL) (SETQ TEM '(FOO)))))
    (UNLESS TEM (FERROR "Keyword ~S with value ~S is not legitimate for flavor ~S."
                        (CAR SPEC) VALUE FLAVOR-NAME))
    (WHEN (STRINGP (CADR TEM))
      (FERROR (CADR TEM) (CAR SPEC) VALUE FLAVOR-NAME))
    (LET ((SUBS (CHOOSE-RUN-TIME-ALTERNATIVE-1 (CDDR TEM) INIT-PLIST FLAVOR-NAME)))
      (IF (CADR TEM) (APPEND SUBS (LIST (CADR TEM))) SUBS))))

(DEFUN ASSURE-FLAVOR-COMPOSED (FLAVOR-NAME &AUX FL)
  "Compose flavor FLAVOR-NAME and its methods if that has not already been done."
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))
             "the name of an instantiable flavor, or alias thereof")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-ARRAY FL) (COMPOSE-METHOD-COMBINATION FL)))

(DEFUN MAKE-INSTANCE (FLAVOR-NAME &REST INIT-OPTIONS)
  "Create and return an instance of flavor FLAVOR-NAME.
INIT-OPTIONS is an alternating list of init keywords and their values.
The new instance is sent an :INIT message."
  (INSTANTIATE-FLAVOR FLAVOR-NAME (LOCF INIT-OPTIONS) T))

;;;Make an object of a particular flavor.
;;;If the flavor hasn't been composed yet, must do so now.
;;; Delaying it until the first time it is needed aids initialization,
;;; e.g. up until now we haven't depended on the depended-on flavors being defined yet.
;;;Note that INIT-PLIST can be modified, if the :DEFAULT-INIT-PLIST option was
;;; used or the init methods modify it.
(DEFUN INSTANTIATE-FLAVOR (FLAVOR-NAME INIT-PLIST
                           &OPTIONAL SEND-INIT-MESSAGE-P
                                     RETURN-UNHANDLED-KEYWORDS-P ;as second value
                                     AREA-TO-CONS-INSTANCE-IN
                           &AUX FL UNHANDLED-KEYWORDS INSTANCE VARS
                                   NEW-PLIST)
  "Create and return an instance of the specified FLAVOR-NAME, low level.
INIT-PLIST's CDR is the list of init keywords and their values.
This list will be modified destructively so that any default init plist
keywords (except those that just set instance variables) are on it.
We send a :INIT message only if SEND-INIT-MESSAGE-P is non-nil.
That may further modify the INIT-PLIST.

If RETURN-UNHANDLED-KEYWORDS-P is non-nil, our second value is an
alternating list of keywords and values for those keywords specified in
INIT-PLIST (or in the default init plist) which the flavor doesn't handle.
If RETURN-UNHANDLED-KEYWORDS-P is nil, it is an error if there are any such."
  ;; Trace any chain of alias flavors to a non-alias flavor.
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))
             "the name of an instantiable flavor, or alias thereof")
  (LET ((TEM (FLAVOR-GET FL :INSTANTIATION-FLAVOR-FUNCTION)))
    (WHEN TEM
      (SETQ TEM (FUNCALL TEM FL INIT-PLIST))
      (UNLESS (AND (SYMBOLP TEM)
                   (GET TEM 'FLAVOR))
        (FERROR "The ~S for flavor ~S
returned an invalid value, ~S, not a flavor name." :INSTANTIATION-FLAVOR-FUNCTION FLAVOR-NAME))
      (SETQ FLAVOR-NAME TEM
            FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))))
  (WHEN (FLAVOR-GET FL :ABSTRACT-FLAVOR)
    (FERROR "~S is an abstract flavor (or alias of one) and may not be instantiated."
            FLAVOR-NAME))
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-ARRAY FL) (COMPOSE-METHOD-COMBINATION FL))
  (UNLESS AREA-TO-CONS-INSTANCE-IN
    (SETQ AREA-TO-CONS-INSTANCE-IN
          (FUNCALL (OR (FLAVOR-GET FL 'INSTANCE-AREA-FUNCTION) 'IGNORE)
                   INIT-PLIST)))
  (LET ((MISSING-KEYWORDS
          (SUBSET-NOT #'(LAMBDA (KEYWORD) (GET-LOCATION-OR-NIL INIT-PLIST KEYWORD))
                      (FLAVOR-GET FL 'REQUIRED-INIT-KEYWORDS))))
    (WHEN MISSING-KEYWORDS
      (FERROR "Flavor ~S requires init keywords ~S that are missing."
              FLAVOR-NAME MISSING-KEYWORDS)))
  ;; Make the instance object, then fill in its various fields
  (SETQ INSTANCE (%make-structure dtp-instance dtp-instance-header
                           fl nil area-to-cons-instance-in
                           (flavor-instance-size fl) (flavor-instance-size fl)))
  (SETQ VARS (FLAVOR-ALL-INSTANCE-VARIABLES FL))
  ;; Default all instance variables to unbound
  (DO ((V VARS (CDR V))
       (I 1 (1+ I)))
      ((NULL V))
    (%P-STORE-TAG-AND-POINTER (%MAKE-POINTER-OFFSET DTP-LOCATIVE INSTANCE I)
                              DTP-NULL (CAR V)))
  (SETQ UNHANDLED-KEYWORDS (FLAVOR-UNHANDLED-INIT-KEYWORDS FL))
  (LET ((VAR-KEYWORDS (FLAVOR-ALL-INITTABLE-INSTANCE-VARIABLES FL))
        (REMAINING-KEYWORDS (FLAVOR-REMAINING-INIT-KEYWORDS FL)))
    ;; First, process any user-specified init keywords that
    ;; set instance variables.  When we process the defaults,
    ;; we will see that these are already set, and will
    ;; refrain from evaluating the default forms.
    ;; At the same time, we record any init keywords that this flavor doesn't handle.
    (DO ((PL (CDR INIT-PLIST) (CDDR PL))) ((NULL PL))
      (LET ((INDEX (FIND-POSITION-IN-LIST (CAR PL) VAR-KEYWORDS)))
        (COND (INDEX
               (OR (LOCATION-BOUNDP (%INSTANCE-LOC INSTANCE (1+ INDEX)))
                   (SETF (%INSTANCE-REF INSTANCE (1+ INDEX)) (CADR PL))))
              ((NOT (MEMQ (CAR PL) REMAINING-KEYWORDS))
               (PUSHNEW (CAR PL) UNHANDLED-KEYWORDS)))))
    ;; Now do all the default initializations, of one sort or other,
    ;; that have not been overridden.
    (LET ((SELF INSTANCE))
      (DOLIST (D (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL))
        (OR (LOCATION-BOUNDP (%INSTANCE-LOC INSTANCE (1+ (CAR D))))
            (SETF (%INSTANCE-REF INSTANCE (1+ (CAR D))) (EVAL (CADR D)))))
      ;; Now stick any default init plist items that aren't handled by that
      ;; onto the actual init plist.
      (DO ((PL (FLAVOR-REMAINING-DEFAULT-PLIST FL) (CDDR PL))) ((NULL PL))
        (OR (MEMQ-ALTERNATED (CAR PL) (CDR INIT-PLIST))
            (PROGN
              (UNLESS (EQ INIT-PLIST (LOCF NEW-PLIST))
                (SETQ NEW-PLIST (CDR INIT-PLIST)
                      INIT-PLIST (LOCF NEW-PLIST)))
              (SETQ NEW-PLIST (LIST* (CAR PL) (EVAL (CADR PL)) NEW-PLIST)))))))
  ;; Complain if any keywords weren't handled, unless our caller
  ;; said it wanted to take care of this.
  (AND (NOT RETURN-UNHANDLED-KEYWORDS-P)
       UNHANDLED-KEYWORDS
       (NOT (GET INIT-PLIST :ALLOW-OTHER-KEYS))
       (FERROR "Flavor ~S does not handle the init keyword~P ~{~S~^, ~}"
               FLAVOR-NAME
               (LENGTH UNHANDLED-KEYWORDS)
               UNHANDLED-KEYWORDS))
  (IF SEND-INIT-MESSAGE-P
      (SEND INSTANCE :INIT INIT-PLIST))
  (VALUES INSTANCE UNHANDLED-KEYWORDS))

(DEFUN %MAKE-INSTANCE (FLAVOR-NAME &REST CONTENTS &AUX FL INSTANCE)
  "Create an instance of flavor FLAVOR-NAME and init all slots from CONTENTS.
This ignores completely the default initializations,
and dos not send the :INIT message.  But it is very fast.
CONTENTS must have exactly the right number of elements,
 and must be a cdr-coded list."
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  (OR (FLAVOR-DEPENDS-ON-ALL FL)
      (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-ARRAY FL)
      (COMPOSE-METHOD-COMBINATION FL))
  (SETQ INSTANCE (%make-structure dtp-instance dtp-instance-header
                       fl nil default-cons-area
                       (flavor-instance-size fl) (flavor-instance-size fl)))
  (%BLT-TYPED CONTENTS (%INSTANCE-LOC INSTANCE 1)
              (1- (FLAVOR-INSTANCE-SIZE FL)) 1)
  INSTANCE)

(DEFUN MEMQ-ALTERNATED (ELT LIST)
  (DO ((L LIST (CDDR L))) ((NULL L) NIL)
    (IF (EQ (CAR L) ELT) (RETURN L))))

(DEFUN FLAVOR-DEFAULT-INIT-PLIST (FLAVOR-NAME &OPTIONAL (INIT-PLIST (NCONS NIL)) &AUX FL)
  "Returns the default init plist for FLAVOR-NAME.
If INIT-PLIST is specified, it is modified to contain any
default init plist entries which it does not override."
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (GET FFL 'FLAVOR))
    (DO ((L (GETF (FLAVOR-PLIST FFL) :DEFAULT-INIT-PLIST) (CDDR L)))
        ((NULL L))
      (DO ((M (CDR INIT-PLIST) (CDDR M)))
          ((NULL M) (SETF (GET INIT-PLIST (CAR L)) (EVAL (CADR L))))
        (WHEN (EQ (CAR M) (CAR L))
          (RETURN)))))
  INIT-PLIST)

(DEFUN FLAVOR-ALLOWS-INIT-KEYWORD-P (FLAVOR-NAME KEYWORD)
  "Return non-nil if flavor FLAVOR-NAME handles init keyword KEYWORD.
The actual value is the particular component flavor which handles it."
  (MAP-OVER-COMPONENT-FLAVORS 0 T T
      #'(LAMBDA (FL IGNORE KEYWORD)
          (AND (OR (ASSQ KEYWORD (FLAVOR-INITTABLE-INSTANCE-VARIABLES FL))
                   (MEMQ KEYWORD (FLAVOR-INIT-KEYWORDS FL)))
               (FLAVOR-NAME FL)))
      FLAVOR-NAME NIL KEYWORD))

(DEFUN FLAVOR-ALLOWED-INIT-KEYWORDS (FLAVOR-NAME)
 "Return a list of all init keywords handled by flavor FLAVOR-NAME."
 (LET ((INIT-KEYWORDS NIL))
   (MAP-OVER-COMPONENT-FLAVORS 0 T NIL
                               #'(LAMBDA (FLAVOR IGNORE)
                                   (SETQ INIT-KEYWORDS
                                         (APPEND (MAPCAR #'(LAMBDA (KWD)
                                                             (IF (CONSP KWD) (CAR KWD) KWD))
                                                         (FLAVOR-LOCAL-INIT-KEYWORDS FLAVOR))
                                                 INIT-KEYWORDS)))
                               FLAVOR-NAME NIL)
   (SORT (ELIMINATE-DUPLICATES INIT-KEYWORDS) #'ALPHALESSP)))
;this is the name documented in the orange manual
(DEFF FLAVOR-ALL-ALLOWED-INIT-KEYWORDS 'FLAVOR-ALLOWED-INIT-KEYWORDS)

(DEFUN FLAVOR-LOCAL-INIT-KEYWORDS (FLAVOR)
  (APPEND (FLAVOR-INITTABLE-INSTANCE-VARIABLES FLAVOR)
          (FLAVOR-INIT-KEYWORDS FLAVOR)))


(DEFUN FLAVOR-DEFAULT-INIT-PUTPROP (FLAVOR FORM INIT-KEYWORD &AUX FL)
  "Add or change an entry in FLAVOR's default init plist.
The entry is for init keyword INIT-KEYWORD, and the value
will be computed by evaluating FORM."
  (SETQ FL (IF (SYMBOLP FLAVOR) (COMPILATION-FLAVOR FLAVOR) FLAVOR))
  (UNLESS (FLAVOR-ALLOWS-INIT-KEYWORD-P FLAVOR INIT-KEYWORD)
    (FERROR "Init keyword ~S invalid for flavor ~S."
            INIT-KEYWORD FLAVOR))
  (SETF (GETF (GETF (FLAVOR-PLIST FL) :DEFAULT-INIT-PLIST) INIT-KEYWORD) FORM)
  (PERFORM-FLAVOR-BINDINGS-REDEFINITION FLAVOR))

(DEFUN FLAVOR-DEFAULT-INIT-REMPROP (FLAVOR INIT-KEYWORD &AUX FL)
  "Remove any entry for INIT-KEYWORD from FLAVOR's default init plist."
  (SETQ FL (IF (SYMBOLP FLAVOR) (COMPILATION-FLAVOR FLAVOR) FLAVOR))
  (REMF (GETF (FLAVOR-PLIST FL) ':DEFAULT-INIT-PLIST)
        INIT-KEYWORD)
  (PERFORM-FLAVOR-BINDINGS-REDEFINITION FLAVOR))

(DEFUN FLAVOR-DEFAULT-INIT-GET (FLAVOR INIT-KEYWORD &OPTIONAL DEFAULT &AUX FL)
  "Return the form for INIT-KEYWORD in FLAVOR's default init plist, or DEFAULT."
  (SETQ FL (IF (SYMBOLP FLAVOR) (COMPILATION-FLAVOR FLAVOR) FLAVOR))
  (GETF (GETF (FLAVOR-PLIST FL) ':DEFAULT-INIT-PLIST)
       INIT-KEYWORD DEFAULT))

(DEFSETF FLAVOR-DEFAULT-INIT-GET (FLAVOR INIT-KEYWORD &OPTIONAL (DEFAULT NIL DEFAULTP))
                                 (VALUE)
  (IF DEFAULTP (SETQ INIT-KEYWORD `(PROG1 ,INIT-KEYWORD ,DEFAULT)))
  `(FLAVOR-DEFAULT-INIT-PUTPROP ,FLAVOR ,VALUE ,INIT-KEYWORD))


;;; Function to map over all components of a specified flavor.  We must do the
;;;  DEPENDS-ON's to all levels first, then the INCLUDES's at all levels and
;;;  what they depend on.
;;; Note that it does the specified flavor itself as well as all its components.
;;; Note well: if there are included flavors, this does not do them in the
;;;  right order.  Also note well: if there are multiple paths to a component,
;;;  it will be done more than once.
;;; RECURSION-STATE is 0 except when recursively calling itself.
;;; ERROR-P is T if not-yet-defflavored flavors are to be complained about,
;;;  NIL if they are to be ignored.  This exists to get rid of certain
;;;  bootstrapping problems.
;;; RETURN-FIRST-NON-NIL is T if the iteration should terminate as soon
;;;  as FUNCTION returns a non-null result.
;;; At each stage FUNCTION is applied to the flavor (not the name), the
;;;  STATE, and any ARGS.  STATE is updated to whatever the function returns.
;;; The final STATE is the final result of this function.
;;; RECURSION-STATE is:
;;;  0  top-level
;;;  1  first-pass over just depends-on's
;;;  6          second-pass, this flavor reached via depends-on's so don't do it again
;;;  2  second-pass, this flavor reached via includes's so do it.
(DEFVAR *SOME-COMPONENT-UNDEFINED* NIL)  ;If we find an undefined component, we put its name here.

(DEFUN MAP-OVER-COMPONENT-FLAVORS (RECURSION-STATE ERROR-P RETURN-FIRST-NON-NIL
                                   FUNCTION FLAVOR-NAME STATE &REST ARGS)
  (PROG MAP-OVER-COMPONENT-FLAVORS (FL)
    (COND ((OR ERROR-P (COMPILATION-FLAVOR FLAVOR-NAME))
           (CHECK-ARG FLAVOR-NAME (SETQ FL (COMPILATION-FLAVOR FLAVOR-NAME))
                      "a defined flavor")
           ;; First do this flavor, unless this is the second pass and it shouldn't be done
           (OR (BIT-TEST 4 RECURSION-STATE)
               (SETQ STATE (APPLY FUNCTION FL STATE ARGS)))
           ;; After each call to the function, see if we're supposed to be done now
           (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
                (RETURN-FROM MAP-OVER-COMPONENT-FLAVORS))
           ;; Now do the depends-on's.
           (DOLIST (COMPONENT-FLAVOR (FLAVOR-DEPENDS-ON FL))
             (SETQ STATE (APPLY #'MAP-OVER-COMPONENT-FLAVORS
                                (IF (ZEROP RECURSION-STATE) 1 RECURSION-STATE)
                                ERROR-P RETURN-FIRST-NON-NIL
                                FUNCTION COMPONENT-FLAVOR STATE ARGS))
             (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
                  (RETURN-FROM MAP-OVER-COMPONENT-FLAVORS)))
           ;; Unless this is the first pass, do the includes.
           (OR (BIT-TEST 1 RECURSION-STATE)
               (DOLIST (COMPONENT-FLAVOR (FLAVOR-INCLUDES FL))
                 (SETQ STATE (APPLY #'MAP-OVER-COMPONENT-FLAVORS
                                    2 ERROR-P RETURN-FIRST-NON-NIL
                                    FUNCTION COMPONENT-FLAVOR STATE ARGS))
                 (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
                      (RETURN-FROM MAP-OVER-COMPONENT-FLAVORS))))
           ;; If this is the top-level, run the second pass on its depends-on's
           ;; which doesn't do them but does do what they include.
           (OR (NOT (ZEROP RECURSION-STATE))
               (DOLIST (COMPONENT-FLAVOR (FLAVOR-DEPENDS-ON FL))
                 (SETQ STATE (APPLY #'MAP-OVER-COMPONENT-FLAVORS
                                    6 ERROR-P RETURN-FIRST-NON-NIL
                                    FUNCTION COMPONENT-FLAVOR STATE ARGS))
                 (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
                      (RETURN-FROM MAP-OVER-COMPONENT-FLAVORS)))))
          ((NULL *SOME-COMPONENT-UNDEFINED*)
           (SETQ *SOME-COMPONENT-UNDEFINED* FLAVOR-NAME))))
  STATE)

(DEFCONST *DONT-RECOMPILE-FLAVORS* NIL
  "T means RECOMPILE-FLAVOR does nothing.
Used to speed up multiple redefinitions on flavors.
Turn this on for the redefinitions, turn this off, then recompile by hand.")

(DEFUN RECOMPILE-FLAVOR (FLAVOR-NAME
                         &OPTIONAL (SINGLE-OPERATION NIL) (*USE-OLD-COMBINED-METHODS* T)
                                   (DO-DEPENDENTS T)
                         &AUX FL)
  "Recompute some or all combined methods for flavor FLAVOR-NAME and dependents.
If SINGLE-OPERATION is NIL, all operations are done;
otherwise that specifies which operation to do.
If DO-DEPENDENTS is specified as NIL, the dependents are not done.
If *USE-OLD-COMBINED-METHODS* is specified as NIL, existing combined
methods are replaced even if they appear to be valid when checked.
Do this to correct for a bug in a combined method creation function
or a change in a macro that a wrapper expands into."
  ;; If this is called during file compilation, the output goes to the QFASL file.
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  (UNLESS *DONT-RECOMPILE-FLAVORS*
    ;; Only update the method combination if it has been done before, else doesn't matter
    (WHEN (FLAVOR-METHOD-HASH-ARRAY FL)
      (OR (FLAVOR-DEPENDS-ON-ALL FL)
          (COMPOSE-FLAVOR-COMBINATION FL))
      (COMPOSE-METHOD-COMBINATION FL SINGLE-OPERATION))
    (WHEN DO-DEPENDENTS
      (LET ((INHIBIT-FDEFINE-WARNINGS T)        ;Don't give warnings for combined methods
            (FDEFINE-FILE-PATHNAME NIL))        ;And they don't "belong" to a file that calls this.
        (DOLIST (FN (FLAVOR-DEPENDED-ON-BY-ALL FL))
          (IF (FLAVOR-METHOD-HASH-ARRAY (GET FN 'FLAVOR))
              (RECOMPILE-FLAVOR FN SINGLE-OPERATION *USE-OLD-COMBINED-METHODS* NIL)))))))

(DEFUN FLAVOR-DEPENDED-ON-BY-ALL (FL &OPTIONAL LIST-SO-FAR &AUX SCAN-POINTER TAIL FFL)
  "Return a list of the names of all flavors that depend on the flavor FL.
Values are in breadth-first order, a good though not perfect order for doing redefinitions."
  (PUSH FL LIST-SO-FAR)
  (SETQ TAIL (LAST LIST-SO-FAR))
  (SETQ SCAN-POINTER LIST-SO-FAR)
  (DO ()
      ((NULL SCAN-POINTER)
       (CDR LIST-SO-FAR))
    (LET* ((FN (CAR SCAN-POINTER))
           (FL (IF (SYMBOLP FN) (COMPILATION-FLAVOR FN) FN)))
      (DOLIST (FN1 (FLAVOR-DEPENDED-ON-BY FL))
        (OR (MEMQ FN1 LIST-SO-FAR)
            (NOT (SETQ FFL (GET FN1 'FLAVOR)))
            (SETF (CDR TAIL) (SETQ TAIL (CONS FN1 NIL))))))
    (POP SCAN-POINTER)))

;;;This function takes care of flavor-combination.  It sets up the list
;;;of all component flavors, in appropriate order, and the list of all
;;;instance variables.  It generally needs to be called only once for a
;;;flavor, and must be called before method-combination can be dealt with.
(DEFVAR *FLAVORS-BEING-COMPOSED* NIL)

(DEFUN COMPOSE-FLAVOR-COMBINATION (FL &OPTIONAL (ERROR-P T) &AUX FLS VARS ORDS REQS SPECS SIZE
                                   (*SOME-COMPONENT-UNDEFINED* NIL)
                                   (*FLAVORS-BEING-COMPOSED*
                                     (CONS FL *FLAVORS-BEING-COMPOSED*))
                                   (PERM-AREA
                                     (IF *JUST-COMPILING* DEFAULT-CONS-AREA
                                       PERMANENT-STORAGE-AREA))
                                   (DEFAULT-CONS-AREA
                                     (IF *JUST-COMPILING* DEFAULT-CONS-AREA
                                       *FLAVOR-AREA*)))
  "Find and record component flavors of flavor object FL.
ERROR-P says whether to get error on undefined components.
We return a list of all known components;
if they are all defined, then they are really all the components,
and the flavor is marked as composed by setting its FLAVOR-DEPENDS-ON-ALL to that list."
  ;; Make list of all component flavors' names.
  ;; This list is in outermost-first order.
  ;; Would be nice for this not to have to search to all levels, but for
  ;; the moment that is hard, so I won't do it.
  ;; Included-flavors are hairy: if not otherwise in the list of components, they
  ;; are stuck in after the rightmost component that includes them, along with
  ;; any components of their own not otherwise in the list.
  (SETQ FLS (COPYLIST (COMPOSE-FLAVOR-INCLUSION (FLAVOR-NAME FL) ERROR-P)
                      PERM-AREA))
  ;; Vanilla-flavor may have been put in by magic, so maintain the dependencies
  ;; in case new methods get added to it later.
  (LET ((VAN (COMPILATION-FLAVOR 'VANILLA-FLAVOR))
        (FLAV (FLAVOR-NAME FL)))
    (AND (NOT (NULL VAN))
         (NEQ FLAV 'VANILLA-FLAVOR)
         (MEMQ 'VANILLA-FLAVOR FLS)
         (NOT *JUST-COMPILING*)
         (NOT (MEMQ FLAV (FLAVOR-DEPENDED-ON-BY VAN)))
         (PUSH FLAV (FLAVOR-DEPENDED-ON-BY VAN))))
  ;; Compute what the instance variables will be, and in what order.
  ;; Also collect the required but not present instance variables, which go onto the
  ;; ADDITIONAL-INSTANCE-VARIABLES property.  The instance variables of the
  ;; :REQUIRED-FLAVORS work the same way.  Such instance variables are ok
  ;; for our methods to access.
  (DOLIST (F FLS)
    (SETQ F (COMPILATION-FLAVOR F))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES F))
      (OR (ATOM V) (SETQ V (CAR V)))
      (OR (MEMQ V VARS) (PUSH V VARS)))
    (SETQ SPECS (NUNION-EQ SPECS (FLAVOR-SPECIAL-INSTANCE-VARIABLES F)))
    (SETQ REQS (NUNION-EQ REQS
                          (GETF (FLAVOR-PLIST F) :REQUIRED-INSTANCE-VARIABLES)))
    ;; Any variables our required flavors have or require, we require.
    (DOLIST (FF (GETF (FLAVOR-PLIST F) :REQUIRED-FLAVORS))
      (COND ((AND (NOT (MEMQ FF FLS))
                  (SETQ FF (COMPILATION-FLAVOR FF))
                  (NOT (MEMQ FF (CDR *FLAVORS-BEING-COMPOSED*))))
             (OR (FLAVOR-DEPENDS-ON-ALL FF) (COMPOSE-FLAVOR-COMBINATION FF NIL))
             (SETQ SPECS (NUNION-EQ SPECS (FLAVOR-ALL-SPECIAL-INSTANCE-VARIABLES FF)))
             (SETQ REQS
                   (NUNION-EQ REQS (FLAVOR-ALL-INSTANCE-VARIABLES FF)
                              (GETF (FLAVOR-PLIST FF) 'ADDITIONAL-INSTANCE-VARIABLES))))))
    (LET ((ORD (GETF (FLAVOR-PLIST F) :ORDERED-INSTANCE-VARIABLES)))
      ;; Merge into existing order requirement.  Shorter of the two must be
      ;; a prefix of the longer, and we take the longer.
      (DO ((L1 ORD (CDR L1))
           (L2 ORDS (CDR L2)))
          (NIL)
        (COND ((NULL L1) (RETURN NIL))
              ((NULL L2) (RETURN (SETQ ORDS ORD)))
              ((NEQ (CAR L1) (CAR L2))
               (FERROR "~S conflict, ~S vs ~S"
                       :ORDERED-INSTANCE-VARIABLES (CAR L1) (CAR L2)))))))
  ;; Must not merge this with the previous loop,
  ;; to avoid altering order of instance variables
  ;; if a DEFFLAVOR is redone.
  (DOLIST (F FLS)
    (SETQ F (COMPILATION-FLAVOR F)))
;    ;; Any variables our components's methods reference, we must keep having.
;    (SETQ VARS (UNION-EQ VARS (FLAVOR-MAPPED-INSTANCE-VARIABLES F))))
  ;; This NREVERSE makes it compatible with the old code.  There is no other reason for it.
  (SETQ VARS (NREVERSE VARS))
  ;; Apply ordering requirement by moving those variables to the front.
  (DOLIST (V ORDS)
    (OR (MEMQ V VARS)
        (FERROR "Flavor ~S lacks instance variable ~S which has an order requirement"
                (FLAVOR-NAME FL) V))
    (SETQ VARS (DELQ V VARS)))
  (SETQ VARS (APPEND ORDS VARS))
  (SETF (FLAVOR-ALL-INSTANCE-VARIABLES FL) (COPYLIST VARS PERM-AREA))
  (IF (OR ORDS (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FL))
      (SETF (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FL) ORDS))
  ;; Instance size must be at least 2 or microcode blows out - fix some day?
  (SETQ SIZE (MAX (1+ (LENGTH VARS)) 2))
  (AND (FLAVOR-INSTANCE-SIZE FL)
       ( (FLAVOR-INSTANCE-SIZE FL) SIZE)
       (FORMAT *ERROR-OUTPUT* "~&Warning: changing the size of an instance of ~S from ~S to ~S
This may cause you problems.~%"         ;* This should perhaps do something about it *
                 (FLAVOR-NAME FL) (FLAVOR-INSTANCE-SIZE FL) SIZE))
  (SETF (FLAVOR-INSTANCE-SIZE FL) SIZE)
  ;; If there are any instance variables required but not present, save them
  ;; so that they can be accessed in methods.
  (DOLIST (V VARS)
    (SETQ REQS (DELQ V REQS)))
  (AND REQS (SETF (GETF (FLAVOR-PLIST FL) 'ADDITIONAL-INSTANCE-VARIABLES) REQS))
  (AND SPECS (SETF (FLAVOR-ALL-SPECIAL-INSTANCE-VARIABLES FL) SPECS))
  ;; Don't mark this flavor as "composed" if there were errors.
  (OR *SOME-COMPONENT-UNDEFINED*
      (SETF (FLAVOR-DEPENDS-ON-ALL FL) FLS))
  FLS)

(DEFUN COMPOSE-FLAVOR-INCLUSION (FLAVOR ERROR-P)
  (MULTIPLE-VALUE-BIND (FLS ADDITIONS) (COMPOSE-FLAVOR-INCLUSION-1 FLAVOR NIL ERROR-P)
    ;; The new additions may themselves imply more components
    (DO L ADDITIONS (CDR L) (NULL L)
      (LET ((MORE-FLS (COMPOSE-FLAVOR-INCLUSION-1 (CAR L) FLS ERROR-P)))
        (DOLIST (F MORE-FLS)
          ;; This hair inserts F before (after) the thing that indirectly included it
          ;; and then puts that next on ADDITIONS so it gets composed also
          (LET ((LL (MEMQ (CAR L) FLS)))
            (RPLACA (RPLACD LL (CONS (CAR LL) (CDR LL))) F)
            (SETF (CDR L) (CONS F (CDR L)))))))
    ;; Now attach vanilla-flavor if desired
    (OR (LOOP FOR FLAVOR IN FLS
           THEREIS (LET ((TEM (COMPILATION-FLAVOR FLAVOR)))
                     (AND TEM (GETF (FLAVOR-PLIST TEM) ':NO-VANILLA-FLAVOR))))
        (PUSH 'VANILLA-FLAVOR FLS))
    (NREVERSE FLS)))

(DEFUN COMPOSE-FLAVOR-INCLUSION-1 (FLAVOR OTHER-COMPONENTS ERROR-P &AUX FLAVOR-1)
  ;; First, make a backwards list of all the normal (non-included) components
  (declare (special other-components))
  (LET ((FLS (MAP-OVER-COMPONENT-FLAVORS 1 ERROR-P NIL
               #'(LAMBDA (FL LIST)
                   (SETQ FL (FLAVOR-NAME FL))
                   (OR (MEMQ FL LIST)
                       (MEMQ FL OTHER-COMPONENTS)
                       (PUSH FL LIST))
                   LIST)
               FLAVOR NIL))
        (ADDITIONS NIL))
    ;; If there are any inclusions that aren't in the list, plug
    ;; them in right after (before in backwards list) their last (first) includer
    (DO L FLS (CDR L) (NULL L)
      (DOLIST (FL (FLAVOR-INCLUDES (COMPILATION-FLAVOR (CAR L))))
        (OR (MEMQ FL FLS)
            (MEMQ FL OTHER-COMPONENTS)
            (PUSH (CAR (RPLACA (RPLACD L (CONS (CAR L) (CDR L))) FL)) ADDITIONS))))
    (OR (MEMQ FLAVOR FLS)
        ;; Avoid error if FLAVOR is undefined and ERROR-P is NIL.
        (NOT (OR (SETQ FLAVOR-1 (COMPILATION-FLAVOR FLAVOR))
                 ERROR-P))
        (SETQ FLS (NCONC FLS
                         (NREVERSE
                           (LOOP FOR FL IN (FLAVOR-INCLUDES FLAVOR-1)
                                 UNLESS (OR (MEMQ FL FLS) (MEMQ FL OTHER-COMPONENTS))
                                   COLLECT FL
                                   AND DO (PUSH FL ADDITIONS))))))
    (VALUES FLS ADDITIONS)))

;;;; Mapping tables.

;;; Each mapping table relates a method-flavor to an instance-flavor.
;;; It maps several of the instance vars accessible from the method-flavor
;;; to slot positions in the instances of the instance flavor.
;;; This instance variables mapped are those in the (FLAVOR-MAPPED-INSTANCE-VARIABLES ...)
;;; of the method flavor.  Those conprise all the instance variables actually
;;; referred to by compiled code of methods of the method flavor,
;;; except for ordered instance variables, which are not mapped at all.

;;; Note that "method-flavor" simply means a flavor on which a method has been defined
;;; and "instance-flavor" simply means a flavor which depends on the method-flavor
;;; and has been instantiated.

;;; Pointers to the mapping tables for one instance-flavor (and various method-flavors)
;;; are stored in an art-q-list array called
;;; (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR instance-flavor).
;;; But they are found through an alist, (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST instance-flavor).
;;; The CDRs of alist elements are locatives into the vector.
;;; When a new method-flavor is seen to need a mapping table,
;;; the entire alist is recopied so it will be compact;
;;; and a previously unused slot in the vector is used.
;;; This way, we keep the alist maximally short and compact,
;;; while keeping the vector short but avoiding forwarding it
;;; unless the flavor gets recomposed with new mixins.

;;; Methods called by message passing get their mapping tables
;;; from the method hash array.

;;; Methods called from combined methods are given mapping tables
;;; by the combined method.  This does not search the alist.
;;; Instead, the combined method looks in its own mapping table,
;;; in the array leader, to find the mapping table to supply for the
;;; method it is calling.

;;; Given a list (FLAVOR-NAME VAR-NAME), return the number of the slot
;;; in mapping tables from that flavor as the method flavor
;;; for the specified variable.
;;; If necessary, add this variable to the flavor's mapped variables
;;; and update all the flavor's mapping tables.
;;; Given instead a list (FLAVOR-NAME T COMPONENT-FLAVOR-NAME),
;;; we pass it on to FLAVOR-COMPONENT-FLAVOR-SELF-REF-INDEX.
(DEFUN FLAVOR-VAR-SELF-REF-INDEX (FLAVOR-AND-VARNAME)
  (LET ((FLAVOR (GET (CAR FLAVOR-AND-VARNAME) 'FLAVOR)))
    (OR FLAVOR (FERROR "Loading a method for flavor ~S which is not defined"
                       (CAR FLAVOR-AND-VARNAME)))
    (IF (= 3 (LENGTH FLAVOR-AND-VARNAME))
        (FLAVOR-COMPONENT-FLAVOR-SELF-REF-INDEX FLAVOR-AND-VARNAME)
      (LET* ((VARNAME (CADR FLAVOR-AND-VARNAME))
             (POS (FIND-POSITION-IN-LIST VARNAME (FLAVOR-MAPPED-INSTANCE-VARIABLES FLAVOR)))
             (OPOS (FIND-POSITION-IN-LIST VARNAME (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FLAVOR))))
        (COND (OPOS)
              (POS (DPB 1 %%SELF-REF-RELOCATE-FLAG POS))
              (T
               (SETF (FLAVOR-MAPPED-INSTANCE-VARIABLES FLAVOR)
                     (NCONC (FLAVOR-MAPPED-INSTANCE-VARIABLES FLAVOR)
                            (CONS-IN-AREA VARNAME NIL WORKING-STORAGE-AREA)))
               (REMAKE-MAPPING-TABLES FLAVOR FLAVOR)
               (DPB 1 %%SELF-REF-RELOCATE-FLAG
                    (FIND-POSITION-IN-LIST VARNAME
                                           (FLAVOR-MAPPED-INSTANCE-VARIABLES FLAVOR)))))))))

;;;Don't record evaluations of this function in QFASL files.
(DEFPROP FLAVOR-VAR-SELF-REF-INDEX T QFASL-DONT-RECORD)

;;;Given a list (FLAVOR-NAME T COMPONENT-FLAVOR-NAME), return the number of the slot
;;;in the array leader of a mapping table between any-flavor and FLAVOR-NAME
;;;which contains the locative to the ptr to the mapping table between
;;;any-flavor and COMPONENT-FLAVOR-NAME.  Adds such an array leader slot if none yet.
(DEFUN FLAVOR-COMPONENT-FLAVOR-SELF-REF-INDEX (FLAVOR-AND-COMPONENT-FLAVOR-NAME)
  (LET* ((FLAVOR (GET (CAR FLAVOR-AND-COMPONENT-FLAVOR-NAME) 'FLAVOR))
         (COMPONENT-FLAVOR-NAME (CADDR FLAVOR-AND-COMPONENT-FLAVOR-NAME))
         (POS (FIND-POSITION-IN-LIST COMPONENT-FLAVOR-NAME
                                     (FLAVOR-MAPPED-COMPONENT-FLAVORS FLAVOR))))
    (OR POS
        (SETQ POS
              (PROGN
                (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
                  ;; Note that the SETF does a PUTPROP which can cons.
                  (SETF (FLAVOR-MAPPED-COMPONENT-FLAVORS FLAVOR)
                        (NCONC (FLAVOR-MAPPED-COMPONENT-FLAVORS FLAVOR)
                               (CONS COMPONENT-FLAVOR-NAME NIL))))
                (REMAKE-MAPPING-TABLES FLAVOR FLAVOR)
                (FIND-POSITION-IN-LIST COMPONENT-FLAVOR-NAME
                                       (FLAVOR-MAPPED-COMPONENT-FLAVORS FLAVOR)))))
    (DPB 1 %%SELF-REF-RELOCATE-FLAG
         (DPB 1 %%SELF-REF-MAP-LEADER-FLAG (+ POS 3)))))

(DEFUN FLAVOR-DECODE-SELF-REF-POINTER (FLAVOR-NAME POINTER-NUMBER)
  "Decode the pointer field of a DTP-SELF-REF-POINTER.
Assumes that it is used with flavor FLAVOR-NAME.
Values are an instance variable name and NIL,
or a component flavor name and T."
  (DECLARE (RETURN-LIST INSTANCE-VAR-OR-COMPONENT-FLAVOR T-IF-COMPONENT-FLAVOR))
  (LET ((FLAVOR (GET FLAVOR-NAME 'FLAVOR)))
    (COND ((NULL FLAVOR) NIL)
          ((LDB-TEST %%SELF-REF-MAP-LEADER-FLAG POINTER-NUMBER)
           (VALUES (NTH (- (LDB %%SELF-REF-INDEX POINTER-NUMBER) 3)
                        (FLAVOR-MAPPED-COMPONENT-FLAVORS FLAVOR))
                   T))
          ((LDB-TEST %%SELF-REF-RELOCATE-FLAG POINTER-NUMBER)
           (NTH (LDB %%SELF-REF-INDEX POINTER-NUMBER)
                (FLAVOR-MAPPED-INSTANCE-VARIABLES FLAVOR)))
          (T
           (NTH (LDB %%SELF-REF-INDEX POINTER-NUMBER)
                (FLAVOR-UNMAPPED-INSTANCE-VARIABLES FLAVOR))))))

(DEFUN FLAVOR-INHERIT-MAPPING-TABLE-FLAVORS (FL)
  "Return a list of component flavor objects of FL from which FL can inherit mapping tables."
  (IF (SYMBOLP FL) (SETQ FL (GET FL 'FLAVOR)))
  (LOOP FOR FN1 IN (CDR (FLAVOR-DEPENDS-ON-ALL FL))
        AS FL1 = (GET FN1 'FLAVOR)
     WHEN (AND (FLAVOR-ALL-INSTANCE-VARIABLES FL1)
               (FLAVOR-METHOD-HASH-ARRAY FL1)
               (DO ((VS (FLAVOR-ALL-INSTANCE-VARIABLES FL) (CDR VS))
                    (V1S (FLAVOR-ALL-INSTANCE-VARIABLES FL1) (CDR V1S)))
                   ((NULL V1S) T)
                 (IF (OR (NULL VS)
                         (NEQ (CAR VS) (CAR V1S)))
                     (RETURN NIL))))
       COLLECT FL1))

;;; Update the mapping tables from method-flavor to instance-flavor
;;; and all flavors that depend on instance-flavor.
;;; Don't create any new mapping tables; only update those that exist.
;;; We take short cuts that assume that this is being done because a new mapped instance var
;;; or mapped component-flavor has been added, and that the goal is to make the maps longer.
(DEFUN REMAKE-MAPPING-TABLES (INSTANCE-FLAVOR METHOD-FLAVOR)
  (AND INSTANCE-FLAVOR
       (LET ((LOC (ASSQ (FLAVOR-NAME METHOD-FLAVOR)
                        (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST INSTANCE-FLAVOR))))
         ;; If this instance-flavor's mapping table already maps as many variables
         ;; as need to be mapped, it must have been reached by a different path,
         ;; so don't bother with it or its dependants again.
         (IF (AND (CDDR LOC)
                  (EQ (ARRAY-LEADER (CDDR LOC) 0)
                      (LENGTH (FLAVOR-MAPPED-INSTANCE-VARIABLES METHOD-FLAVOR)))
                  (= (ARRAY-LEADER-LENGTH (CDDR LOC))
                     (+ 3 (LENGTH (FLAVOR-MAPPED-COMPONENT-FLAVORS METHOD-FLAVOR)))))
             NIL
           (COND ((CDDR LOC)
                  (LET ((OMAP (CDDR LOC)))
                    (SETF (CDDR LOC)
                          (UPDATE-MAPPING-TABLE INSTANCE-FLAVOR METHOD-FLAVOR (CDDR LOC)))
                    (AND (ARRAYP (FLAVOR-METHOD-HASH-ARRAY INSTANCE-FLAVOR))
                         (REPLACE-THROUGH-HASH-ARRAY
                           (FLAVOR-METHOD-HASH-ARRAY INSTANCE-FLAVOR)
                           OMAP (CDDR LOC))))))
           (DOLIST (SUBFLAVOR (FLAVOR-DEPENDED-ON-BY INSTANCE-FLAVOR))
             (REMAKE-MAPPING-TABLES (IF (SYMBOLP SUBFLAVOR) (GET SUBFLAVOR 'FLAVOR) SUBFLAVOR)
                                    METHOD-FLAVOR))))))

(DEFUN REPLACE-THROUGH-HASH-ARRAY (HARRY OLD NEW)
  (LET ((LEN (ARRAY-LENGTH HARRY)))
    (DO ((I 2 (+ 3 I)))
        ((>= I LEN))
      (IF (EQ (AREF HARRY I) OLD)
          (SETF (AREF HARRY I) NEW)))))

(DEFVAR *TRACE-MAPPING-TABLE-GROWTH* NIL
  "T => print a message every time an existing flavor mapping table is made bigger.")

;;;Construct a new map for a pair of flavors, or reuse an old map if it is long enough.
;;;If we construct a new map, we make it a little bigger than necessary
;;;so that if only a couple more mapped vars are needed we can reuse it.
(DEFUN UPDATE-MAPPING-TABLE (INSTANCE-FLAVOR METHOD-FLAVOR &OPTIONAL OLD-MAP)
  (IF (SYMBOLP METHOD-FLAVOR)
      (SETQ METHOD-FLAVOR (GET METHOD-FLAVOR 'FLAVOR)))
  (LET ((MAPVARS (FLAVOR-MAPPED-INSTANCE-VARIABLES METHOD-FLAVOR))
        (MAPFLAVS (FLAVOR-MAPPED-COMPONENT-FLAVORS METHOD-FLAVOR))
        (IVARS (FLAVOR-ALL-INSTANCE-VARIABLES INSTANCE-FLAVOR)))
    (LET ((MAP OLD-MAP))
      (WHEN (OR (NULL MAP)
                (> (LENGTH MAPVARS) (ARRAY-LENGTH MAP))
                (> (LENGTH MAPFLAVS) (- (ARRAY-LEADER-LENGTH MAP) 3)))
        (AND MAP *TRACE-MAPPING-TABLE-GROWTH*
             (FORMAT T "~&Growing mapping table for method flavor ~S, instance flavor ~S."
                     (FLAVOR-NAME METHOD-FLAVOR)
                     (IF (SYMBOLP INSTANCE-FLAVOR) INSTANCE-FLAVOR
                       (FLAVOR-NAME INSTANCE-FLAVOR))))
        (SETQ MAP (MAKE-ARRAY (+ 4 (LENGTH MAPVARS)) :TYPE ART-16B
                              :LEADER-LENGTH (+ 3 (LENGTH MAPFLAVS))
                              :AREA PERMANENT-STORAGE-AREA)))
      ;; Fill in the extra leader slots with mapping table locatives
      ;; for this instance flavor and the method flavor's mapped component-flavors
      ;; as method flavors.
      (DO ((I 3 (1+ I))
           (FLAVS MAPFLAVS (CDR FLAVS)))
          ((NULL FLAVS))
        (SETF (ARRAY-LEADER MAP I)
              (GET-MAPPING-TABLE-LOCATION INSTANCE-FLAVOR (CAR FLAVS))))
      ;; Fill in the array elements of the mapping table
      ;; with indices in the instance flavor of the method flavor's mapped variables.
      (DO ((I 0 (1+ I))
           (VARS MAPVARS (CDR VARS)))
          ((NULL VARS)
           (SETF (ARRAY-LEADER MAP 0) I))
        (SETF (AREF MAP I)
              (OR (FIND-POSITION-IN-LIST (CAR VARS) IVARS)
                  #o7771)))
      (SETF (ARRAY-LEADER MAP 1)
            METHOD-FLAVOR)
      (SETF (ARRAY-LEADER MAP 2)
            INSTANCE-FLAVOR)
      MAP)))

(DEFVAR *CREATE-MAPPING-TABLES* NIL
  "T while method-composing; create any mapping table a method wants to use.")

;;;Get a cell whose CDR is or will be the mapping table for a pair of flavors.
;;;If the instance flavor has been instantiated, we also create a mapping table
;;;if there isn't one.  Otherwise, we just make a slot in the alist and leave it nil.
;;;The mapping tables will be created when the flavor is instantiated.
(DEFUN GET-MAPPING-TABLE-LOCATION (INSTANCE-FLAVOR METHOD-FLAVOR)
  (IF (SYMBOLP INSTANCE-FLAVOR)
      (SETQ INSTANCE-FLAVOR (GET INSTANCE-FLAVOR 'FLAVOR)))
  (OR (SYMBOLP METHOD-FLAVOR)
      (SETQ METHOD-FLAVOR (FLAVOR-NAME METHOD-FLAVOR)))
  (OR (CDR (ASSQ METHOD-FLAVOR
                 (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST INSTANCE-FLAVOR)))
      ;; If the method-flavor is no longer a component of the instance-flavor,
      ;; it must be someone's mapped-component-flavor that is no longer used.
      ;; Just ignore it.
      (AND (MEMQ METHOD-FLAVOR
                 (FLAVOR-DEPENDS-ON-ALL INSTANCE-FLAVOR))
           ;; This method flavor is not in the alist, so make a slot for its mapping table.
           (LET ((VECTOR (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR INSTANCE-FLAVOR))
                 VECTOR-INDEX)
             ;; Make sure vector exists and is long enough for all our component flavors.
             (LET ((LEN (LENGTH (FLAVOR-DEPENDS-ON-ALL INSTANCE-FLAVOR))))
               (OR VECTOR
                   (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR INSTANCE-FLAVOR)
                         (SETQ VECTOR
                               (MAKE-ARRAY LEN :TYPE 'ART-Q-LIST
                                           :AREA PERMANENT-STORAGE-AREA
                                           :LEADER-LIST '(0)))))
               (IF (OR (> LEN (ARRAY-LENGTH VECTOR))
                       (= (ARRAY-ACTIVE-LENGTH VECTOR) (ARRAY-LENGTH VECTOR)))
                   (ADJUST-ARRAY-SIZE VECTOR (MAX LEN (1+ (LENGTH VECTOR))))))
             ;; Add a slot for the new mapping table to the vector.
             (SETQ VECTOR-INDEX (ARRAY-PUSH VECTOR NIL))
             ;; Add an entry to the alist, pointing at newly added vector slot.
             (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
               (PUSH (CONS METHOD-FLAVOR
                           (LOCF (AREF VECTOR VECTOR-INDEX)))
                     (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST INSTANCE-FLAVOR)))
             ;; Now fill in the slot in the vector with a mapping table
             ;; if the instance flavor may have been instantiated already.
             (AND (OR *CREATE-MAPPING-TABLES*
                      (FLAVOR-METHOD-HASH-ARRAY INSTANCE-FLAVOR))
                  (SETF (AREF VECTOR VECTOR-INDEX)
                        (UPDATE-MAPPING-TABLE INSTANCE-FLAVOR METHOD-FLAVOR)))
             (LOCF (AREF VECTOR VECTOR-INDEX))))))

(DEFUN FEF-FLAVOR-NAME (FEF)
  "Return the flavor which the compiled function FEF assumes SELF is an instance of."
  (AND (TYPEP FEF 'COMPILED-FUNCTION)
       (memq (%P-LDB %%HEADER-TYPE-FIELD FEF)
             '(#.%HEADER-TYPE-FEF
                ;no, the ultra fast option cannot be used with methods
                ; or DECLARE-FLAVOR-INSTANCE-VARIABLES, etc.
               ;#.%HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS
               ;#.%HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
               ;#.%HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
               ;#.%HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
               ))
       (NOT (ZEROP (%P-LDB %%FEFH-GET-SELF-MAPPING-TABLE FEF)))
       (%P-CONTENTS-OFFSET FEF
                           (1- (%P-LDB-OFFSET %%FEFHI-MS-ARG-DESC-ORG
                                              FEF
                                              %FEFHI-MISC)))))

(DEFUN GET-HANDLER-MAPPING-TABLE (FLAVOR HANDLER DEFINITION-LOCATION)
  (OR (CDR (GET-MAPPING-TABLE-LOCATION
             FLAVOR
             (OR (AND (= DTP-SYMBOL (%P-DATA-TYPE DEFINITION-LOCATION))
                      (FBOUNDP (CAR DEFINITION-LOCATION))
                      (FEF-FLAVOR-NAME (SYMBOL-FUNCTION (CAR DEFINITION-LOCATION))))
                 (CADR HANDLER))))
      (FERROR "No mapping table for method ~S in flavor ~S"
              HANDLER FLAVOR)))

(DEFVAR TOTAL-INHERITED-MAPPING-TABLE-SIZE 0)

;;; Update all the mapping tables for INSTANCE-FLAVOR and various method-flavors.
;;; Creates a mapping table for each slot which is empty.
;;; If REPLACE-ALL is set, creates a new mapping table for every slot,
;;; throwing away the old mapping tables.  That is used when a flavor has
;;; changed incompatibly.
(DEFUN MAKE-COMPONENT-MAPPING-TABLES (INSTANCE-FLAVOR &OPTIONAL REPLACE-ALL
                                      &AUX
                                      (INHERIT-MAPPING-TABLE-FLAVORS
                                        (FLAVOR-INHERIT-MAPPING-TABLE-FLAVORS
                                          INSTANCE-FLAVOR)))
  ;; Make sure vector exists and is long enough for all our component flavors.
  (LET ((LEN (LENGTH (FLAVOR-DEPENDS-ON-ALL INSTANCE-FLAVOR))))
    (OR (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR INSTANCE-FLAVOR)
        (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR INSTANCE-FLAVOR)
              (MAKE-ARRAY LEN :TYPE 'ART-Q-LIST :AREA PERMANENT-STORAGE-AREA
                          :LEADER-LIST '(0))))
    (IF (> LEN (ARRAY-LENGTH (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR INSTANCE-FLAVOR)))
        (ADJUST-ARRAY-SIZE (FLAVOR-COMPONENT-MAPPING-TABLE-VECTOR INSTANCE-FLAVOR) LEN)))
  ;; Make sure all components are in the vector and alist.
  (DOLIST (MF (FLAVOR-DEPENDS-ON-ALL INSTANCE-FLAVOR))
    (GET-MAPPING-TABLE-LOCATION INSTANCE-FLAVOR MF))
  ;; Copy the alist now so it is compact, if it has changed.
  ;; It is now copied by LINEARIZE-FLAVOR-PLISTS after full-gc.
;  (OR (EQ OALIST (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST INSTANCE-FLAVOR))
;      (SETF (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST INSTANCE-FLAVOR)
;           (COPYALIST (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST INSTANCE-FLAVOR)
;                      PERMANENT-STORAGE-AREA)))
  ;; Make sure all mapping tables exist and are up to date.
  (DOLIST (ELT (FLAVOR-COMPONENT-MAPPING-TABLE-ALIST INSTANCE-FLAVOR))
    (WHEN (OR REPLACE-ALL (NULL (CDDR ELT)))
      ;; Inherit mapping tables when possible.
      (DOLIST (IFL INHERIT-MAPPING-TABLE-FLAVORS)
        (WHEN (MEMQ (CAR ELT) (FLAVOR-DEPENDS-ON-ALL IFL))
          (SETF (CDDR ELT) (CAR (GET-MAPPING-TABLE-LOCATION IFL (CAR ELT))))
          (INCF TOTAL-INHERITED-MAPPING-TABLE-SIZE
                (%STRUCTURE-TOTAL-SIZE (CDDR ELT)))))
      (SETF (CDDR ELT)
            (UPDATE-MAPPING-TABLE INSTANCE-FLAVOR (CAR ELT))))))

(defconst *flavor-hash-array-rehash-threshold* 0.7s0)   ;Was 0.8 --- Mly

;;; Once the flavor-combination stuff has been done, do the method-combination stuff.
;;; The above function usually only gets called once, but this function gets called
;;; when a new method is added.
;;; Specify SINGLE-OPERATION to do this for just one operation, for incremental update.
;;; This function should not be called for a single operation until it has
;;; been called at least once to do all operations.
;;; NOTE WELL: If a meth is in the method-table at all, it is considered to be defined
;;;  for purposes of compose-method-combination.  Thus merely putprop'ing a method,
;;;  or calling flavor-notice-method, will make the flavor think that method exists
;;;  when it is next composed.  This is necessary to make compile-flavor-methods work.
;;;  (Putprop must create the meth because loading does putprop before fdefine.)
(DEFUN COMPOSE-METHOD-COMBINATION (FL &OPTIONAL (SINGLE-OPERATION NIL)
                                   &AUX TEM MAGIC-LIST ORDER
                                        MSG ELEM HANDLERS FFL PL
                                        (DEFAULT-CONS-AREA *FLAVOR-AREA*))
  (IF (FLAVOR-GET FL :ALIAS-FLAVOR)
      (FERROR "Attempt to compose methods of ~S, an alias flavor."
              (FLAVOR-NAME FL)))
  ;; If we are doing wholesale method composition,
  ;; compose the flavor bindings list also.
  ;; This way it is done often enough, but not at every defmethod.
  (OR SINGLE-OPERATION *JUST-COMPILING*
      (FLAVOR-GET FL ':ABSTRACT-FLAVOR)
      (PROGN
        (COMPOSE-FLAVOR-BINDINGS FL)
        (COMPOSE-FLAVOR-INITIALIZATIONS FL)))
  ;; Look through all the flavors depended upon and collect the following:
  ;; A list of all the operations handled and all the methods for each, called MAGIC-LIST.
  ;; The default handler for unknown operations.
  ;; The declared order of entries in the select-method alist.
  ;; Also generate any automatically-created methods not already present.
  ;; MAGIC-LIST is roughly the same format as the flavor-method-table, see its comments.
  ;; Each magic-list entry is (message comb-type comb-order (type function-spec...)...)
  (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS)))
      ((NULL FFLS))
    (SETQ FFL (COMPILATION-FLAVOR (CAR FFLS)) PL (LOCF (FLAVOR-PLIST FFL)))
    ;;>> Note that this loses TOTALLY since the method-hash-array doesn't record this
    ;;>>  information, so we lose on the next gc rehash.  Fixed when the format of
    ;;>>  method-hash-arrays is rationalized. (SOON, I hope...)
    (AND (NOT SINGLE-OPERATION)
         (SETQ TEM (GET PL ':METHOD-ORDER))
         (SETQ ORDER (NCONC ORDER (COPY-LIST TEM))))
    ;; Add data from flavor method-table to magic-list
    ;; But skip over combined methods, they are not relevant here
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FFL))
      (SETQ MSG (CAR MTE))
      (COND ((OR (NOT SINGLE-OPERATION) (EQ MSG SINGLE-OPERATION))
             ;; Well, we're supposed to concern ourselves with this operation
             (SETQ ELEM (ASSQ MSG MAGIC-LIST))  ;What we already know about it
             (COND ((DOLIST (METH (CDDDR MTE))
                      (OR (EQ (METH-METHOD-TYPE METH) :COMBINED)
                          (NOT (METH-DEFINEDP METH))
                          (RETURN T)))
                    ;; OK, this flavor really contributes to handling this operation
                    (OR ELEM (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) MAGIC-LIST))
                    ;; For each non-combined method for this operation, add it to the front
                    ;; of the magic-list element, thus they are in base-flavor-first order.
                    (DOLIST (METH (CDDDR MTE))
                      (LET ((TYPE (METH-METHOD-TYPE METH)))
                        (COND ((EQ TYPE ':COMBINED))
                              ((NOT (METH-DEFINEDP METH)))
                              ((NOT (SETQ TEM (ASSQ TYPE (CDDDR ELEM))))
                               (PUSH (LIST TYPE (METH-FUNCTION-SPEC METH)) (CDDDR ELEM)))
                              ;; Don't let the same method get in twice (how could it?)
                              ((NOT (MEMQ (METH-FUNCTION-SPEC METH) (CDR TEM)))
                               (PUSH (METH-FUNCTION-SPEC METH) (CDR TEM))))))))
             ;; Pick up method-combination declarations
             (AND (CADR MTE) (CADR ELEM)        ;If both specify combination-type, check
                  (OR (NEQ (CADR MTE) (CADR ELEM)) (NEQ (CADDR MTE) (CADDR ELEM)))
                  (FERROR "Method-combination mismatch ~S-~S vs. ~S-~S; check your ~S's"
                          (CADR MTE) (CADDR MTE) (CADR ELEM) (CADDR ELEM) 'DEFFLAVOR))
             (COND ((CADR MTE)                  ;Save combination-type when specified
                    (OR ELEM (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) MAGIC-LIST))
                    (SETF (CADR ELEM) (CADR MTE))
                    (SETF (CADDR ELEM) (CADDR MTE)))) ))))
  ;; This NREVERSE tends to put base-flavor methods last
  (SETQ MAGIC-LIST (NREVERSE MAGIC-LIST))
  ;; Re-order the magic-list according to any declared required order
  (DOLIST (MSG (NREVERSE ORDER))
    (AND (SETQ TEM (ASSQ MSG MAGIC-LIST))
         (SETQ MAGIC-LIST (CONS TEM (DELQ TEM MAGIC-LIST 1)))))
  ;; Map over the magic-list.  For each entry call the appropriate method-combining
  ;; routine, which will return a function spec for the handler to use for this operation.
  (DOLIST (MTE MAGIC-LIST)
    ;; Punt if there are no methods at all (just a method-combination declaration)
    (COND ((CDDDR MTE)
           ;; Process the :DEFAULT methods; if there are any untyped methods the
           ;; default methods go away, otherwise they become untyped methods.
           (AND (SETQ TEM (ASSQ :DEFAULT (CDDDR MTE)))
                (IF (ASSQ NIL (CDDDR MTE))
                    (SETF (CDDDR MTE) (DELQ TEM (CDDDR MTE)))
                    (SETF (CAR TEM) NIL)))
           (OR (SETQ TEM (GET (OR (CADR MTE) :DAEMON) 'METHOD-COMBINATION))
               (FERROR "~S unknown method combination type for ~S operation"
                       (CADR MTE) (CAR MTE)))
           (PUSH (FUNCALL TEM FL MTE) HANDLERS))
          (T (SETQ MAGIC-LIST (DELQ MTE MAGIC-LIST 1)))))
  (OR *JUST-COMPILING* (FLAVOR-GET FL :ABSTRACT-FLAVOR)
      (PROGN
        ;; Make sure that the required variables and methods are present.
        (UNLESS SINGLE-OPERATION
          (VERIFY-REQUIRED-FLAVORS-METHODS-AND-IVARS FL MAGIC-LIST))
        ;; If the flavor does not have mapping tables yet, make some.
        (MAKE-COMPONENT-MAPPING-TABLES FL)))
  ;; Get back into declared order.  We now have a list of function specs for handlers.
  (SETQ HANDLERS (NREVERSE HANDLERS))
  (COND (*JUST-COMPILING* )     ;If just compiling, don't affect hash array.
        ((FLAVOR-GET FL :ABSTRACT-FLAVOR)
         (SETF (FLAVOR-METHOD-HASH-ARRAY FL) T))
        (SINGLE-OPERATION
          ;; If doing SINGLE-OPERATION, put it into the hash array.
          ;; If the operation is becoming defined and wasn't, or vice versa,
          ;; must recompute the which-operations list.
          (WITHOUT-INTERRUPTS   ;SWAPHASH or REMHASH might rehash.
            (COND ((NULL HANDLERS)              ;Deleting method
                   ;; Remove entry from the which-operations list.
                   (AND (MEMQ SINGLE-OPERATION (FLAVOR-WHICH-OPERATIONS FL))
                        (SETF (FLAVOR-WHICH-OPERATIONS FL)
                              (DELQ SINGLE-OPERATION (FLAVOR-WHICH-OPERATIONS FL))))
                   (SEND (DONT-OPTIMIZE
                           (hash-array-hash-table-instance
                             (FLAVOR-METHOD-HASH-ARRAY FL)))
                         :rem-hash SINGLE-OPERATION))
                  (T
                   ;; Add an entry to the which-operations list.
                   (UNLESS (MEMQ SINGLE-OPERATION (FLAVOR-WHICH-OPERATIONS FL))
                     (WHEN (FLAVOR-WHICH-OPERATIONS FL)
                       (SETF (FLAVOR-WHICH-OPERATIONS FL)
                             (COPY-LIST (CONS SINGLE-OPERATION
                                              (FLAVOR-WHICH-OPERATIONS FL))))))
                   ;; Add one to the hash array.
                   (LET (DEF)
                     (SWAPHASH SINGLE-OPERATION
                               (SETQ DEF (FDEFINITION-LOCATION (CAR HANDLERS)))
                               (DONT-OPTIMIZE
                                 (hash-array-hash-table-instance
                                   (FLAVOR-METHOD-HASH-ARRAY FL)))
                               (GET-HANDLER-MAPPING-TABLE FL (CAR HANDLERS) DEF))))))
          ;; snap out
          (SETF (FLAVOR-METHOD-HASH-ARRAY FL)
                (SYMEVAL-IN-INSTANCE (DONT-OPTIMIZE (hash-array-hash-table-instance
                                                      (FLAVOR-METHOD-HASH-ARRAY FL)))
                                     'HASH-ARRAY)))
        ;; Working on all operations at once.
        (T
         (MULTIPLE-VALUE-BIND (harry instance)
             (MAKE-FLAVOR-HASH-ARRAY PERMANENT-STORAGE-AREA
                                     (1+ (CEILING (// (LENGTH MAGIC-LIST)
                                                      *flavor-hash-array-rehash-threshold*))))
           (DO ((HANDLERS HANDLERS (CDR HANDLERS))
                (ML MAGIC-LIST (CDR ML))
                (*CREATE-MAPPING-TABLES* T)
                DEF)
               ((NULL ML))
             (PUTHASH-ARRAY (CAAR ML) (SETQ DEF (FDEFINITION-LOCATION (CAR HANDLERS))) harry
                            (GET-HANDLER-MAPPING-TABLE FL (CAR HANDLERS) DEF)))
           (SETF (FLAVOR-METHOD-HASH-ARRAY FL) harry)
           (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL) ;This will have to be recomputed
           ;; If a hash-instance exists, make sure SEND will use the latest
           ;; version of the hash array of that hash instance.
           (WHEN INSTANCE
             (SETF (FLAVOR-METHOD-HASH-ARRAY FL)
                   (SYMEVAL-IN-INSTANCE INSTANCE 'HASH-ARRAY))))))
  (UNLESS (OR *JUST-COMPILING* (FLAVOR-WHICH-OPERATIONS FL)
              (FLAVOR-GET FL :ABSTRACT-FLAVOR))
    ;; Make the :WHICH-OPERATIONS list.
    (LET ((HARRY (FLAVOR-METHOD-HASH-ARRAY FL))
          LIST)
      (MAPHASH-ARRAY #'(LAMBDA (OP &REST IGNORE)
                         (PUSH OP LIST))
                     HARRY)
      (SETQ LIST (SORT LIST #'ALPHALESSP))
      (UNLESS (EQUAL LIST (FLAVOR-WHICH-OPERATIONS FL))
        (SETF (FLAVOR-WHICH-OPERATIONS FL) (COPYLIST LIST)))))
  NIL)

(DEFUN FLAVOR-ALL-INHERITABLE-METHODS (FLAVOR-NAME OPERATION &AUX FL)
  "Return a list of function specs of all methods used by OPERATION on FLAVOR-NAME.
This may include some that are shadowed by others in the list."
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "a flavor name")
  (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS))
       MTE LIST)
      ((NULL FFLS)
       (NREVERSE LIST))
    (SETQ MTE (ASSQ OPERATION (FLAVOR-METHOD-TABLE (COMPILATION-FLAVOR (CAR FFLS)))))
    (WHEN MTE
      ;; For each non-combined method for this operation, add it to the front
      ;; of the list, thus they are in base-flavor-first order.
      (DOLIST (METH (CDDDR MTE))
        (LET ((TYPE (METH-METHOD-TYPE METH)))
          (COND ((EQ TYPE :COMBINED))
                ((NOT (METH-DEFINEDP METH)))
                (T
                 (PUSH (METH-FUNCTION-SPEC METH) LIST))))))))

(DEFUN VERIFY-REQUIRED-FLAVORS-METHODS-AND-IVARS (FL MAGIC-LIST)
  (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS))
       (MISSING-METHODS NIL)
       (MISSING-INSTANCE-VARIABLES NIL)
       (MISSING-FLAVORS NIL)
       (REQUIRING-FLAVOR-ALIST NIL))
      ((NULL FFLS)
       (AND (OR MISSING-INSTANCE-VARIABLES MISSING-METHODS MISSING-FLAVORS)
            (FERROR "Flavor ~S is missing ~
                        ~:[~2*~;instance variable~P ~{~S~^, ~} ~]~
                        ~:[~3*~;~:[~;and ~]method~P ~{~S~^, ~}~]~
                        ~:[~3*~;~:[~;and ~]component flavor~P ~{~S~^, ~}~]
Requiring Flavor alist: ~S"
                    (FLAVOR-NAME FL)
                    MISSING-INSTANCE-VARIABLES
                    (LENGTH MISSING-INSTANCE-VARIABLES)
                    MISSING-INSTANCE-VARIABLES
                    MISSING-METHODS
                    MISSING-INSTANCE-VARIABLES
                    (LENGTH MISSING-METHODS)
                    MISSING-METHODS
                    MISSING-FLAVORS
                    (OR MISSING-INSTANCE-VARIABLES MISSING-METHODS)
                    (LENGTH MISSING-FLAVORS)
                    MISSING-FLAVORS
                    REQUIRING-FLAVOR-ALIST)))
    (LET ((PL (LOCF (FLAVOR-PLIST (GET (CAR FFLS) 'FLAVOR)))))
      (DOLIST (REQM (GET PL :REQUIRED-METHODS))
        (OR (ASSQ REQM MAGIC-LIST)
            (MEMQ REQM MISSING-METHODS)
            (PROGN (PUSH REQM MISSING-METHODS)
                   (PUSH (CONS (FIRST FFLS) REQM) REQUIRING-FLAVOR-ALIST))))
      (DOLIST (REQV (GET PL :REQUIRED-INSTANCE-VARIABLES))
        (OR (MEMQ REQV (FLAVOR-ALL-INSTANCE-VARIABLES FL))
            (MEMQ REQV MISSING-INSTANCE-VARIABLES)
            (PROGN (PUSH REQV MISSING-INSTANCE-VARIABLES)
                   (PUSH (CONS (FIRST FFLS) REQV) REQUIRING-FLAVOR-ALIST))))
      (DOLIST (REQF (GET PL :REQUIRED-FLAVORS))
        (OR (MEMQ REQF (FLAVOR-DEPENDS-ON-ALL FL))
            (MEMQ REQF MISSING-FLAVORS)
            (PROGN (PUSH REQF MISSING-FLAVORS)
                   (PUSH (CONS (FIRST FFLS) REQF) REQUIRING-FLAVOR-ALIST)))))))

;;; This function is called whenever the microcode fails to find an operation
;;; in the flavor's hash array.
;;; It could be because it is really undefined.
;;; Or maybe a GC has taken place and the method hash array must be rehashed.
;;; Or maybe the hash array has been forwarded.  The ucode doesn't follow the
;;; forwarding, but rather gives up, so that we can un-forward it permanently.
;;; note: instance-hash-failure is called from the microcode via the
;;; support vector
(DEFUN INSTANCE-HASH-FAILURE (OP &REST ARGS
                              &AUX (harry (%FUNCTION-INSIDE-SELF)) FN-LOCATION FUNC)
  (with-lock ((hash-array-lock harry))
    (when (dont-optimize (hash-array-gc-rehash-necessary-p harry))
    ;; Some %POINTER's may have changed, try rehashing
      (LET ((NEW (dont-optimize (rehash-hash-array harry nil))))
        (SET-IN-INSTANCE (DONT-OPTIMIZE (hash-array-hash-table-instance HARRY))
                         'HASH-ARRAY
                         NEW)
        (SETF (INSTANCE-FUNCTION SELF) NEW)
        (SETQ HARRY NEW))))
  ;; In case a GC has happened or the hash array has been rehashed and forwarded,
  ;; search it again using GETHASH to find out if the operation is really there.
  (WHEN (SETQ FN-LOCATION
              ;; GETHASH does follow forwarding, and rehashes if neccessary.
              (send (DONT-OPTIMIZE (hash-array-hash-table-instance harry)) :GET-HASH OP))
    ;; In case GETHASH rehashed, snap out forwarding.
    (SETF (INSTANCE-FUNCTION SELF)
          (SYMEVAL-IN-INSTANCE (DONT-OPTIMIZE (hash-array-hash-table-instance HARRY)) 'HASH-ARRAY)))
  (COND ((SETQ FUNC (OR (CONTENTS FN-LOCATION)  ;Found a definition
                        (FLAVOR-DEFAULT-HANDLER (INSTANCE-FLAVOR SELF))))
         (APPLY FUNC OP ARGS))
        ((SETQ FUNC (AND (NEQ OP :UNCLAIMED-MESSAGE)    ;user defined handler
                         (GET-HANDLER-FOR SELF :UNCLAIMED-MESSAGE)))
         (APPLY FUNC :UNCLAIMED-MESSAGE OP ARGS))
        (T (APPLY #'FLAVOR-UNCLAIMED-MESSAGE OP ARGS))))        ;default handler

;;; This is the default handler for flavors.
(DEFUN FLAVOR-UNCLAIMED-MESSAGE (&REST MESSAGE)
  (REPORT-UNCLAIMED-MESSAGE (%STACK-FRAME-POINTER) MESSAGE))

(DEFUN REPORT-UNCLAIMED-MESSAGE (FRAME-POINTER MESSAGE)
  (DECLARE (DBG:ERROR-REPORTER))
  ;; Make this frame be a call to SELF so retrying it works.
  (SETF (CONTENTS FRAME-POINTER) SELF)
  (SIGNAL-PROCEED-CASE ((NEW-OPERATION) 'SYS:UNCLAIMED-MESSAGE
                                        :OBJECT SELF
                                        :MESSAGE (CAR MESSAGE)
                                        :ARGUMENTS (COPY-LIST (CDR MESSAGE)))
    (:NEW-OPERATION (LEXPR-SEND SELF NEW-OPERATION (CDR MESSAGE)))))

(DEFUN FLAVOR-METHOD-ALIST (FL)
  "Return an alist of operations and their handlers, for flavor FL."
  (IF (SYMBOLP FL) (SETQ FL (COMPILATION-FLAVOR FL)))
  (WHEN FL
    (LET ((HARRY (FLAVOR-METHOD-HASH-ARRAY FL))
          ALIST)
      (AND (ARRAYP HARRY)
           (send (DONT-OPTIMIZE
                   (hash-array-hash-table-instance harry))
                 :MAP-HASH #'(LAMBDA (OP METH-LOCATIVE &REST IGNORE)
                               (PUSH (CONS OP (CAR METH-LOCATIVE)) ALIST))))
      ALIST)))

;;; Make the instance-variable getting and setting methods
(DEFPROP COMPOSE-AUTOMATIC-METHODS T QFASL-DONT-RECORD)
;;; ??? This needs to get changed so that the methods are always compiled
;;; once most files are compiled so that this is not called at load time.
(DEFUN COMPOSE-AUTOMATIC-METHODS (FL)
  ;; Avoid lossage on PROPERTY-LIST-MIXIN while reading this file into the cold load.
  (WHEN (FBOUNDP 'COMPILE-AT-APPROPRIATE-TIME)
    (DOLIST (V (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL))
      (LET* ((VV (CORRESPONDING-KEYWORD V))
             (METH `(:METHOD ,(FLAVOR-NAME FL) ,VV)))
        (IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
                *JUST-COMPILING*)
            (COMPILE-AT-APPROPRIATE-TIME
              FL METH
              `(NAMED-LAMBDA (,METH) (IGNORE)
                 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
                          (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
                 ,V))
          (RECORD-SOURCE-FILE-NAME METH))))
    (DOLIST (V (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL))
      (LET* ((SV (INTERN1 (FORMAT NIL "SET-~A" V) PKG-KEYWORD-PACKAGE))
             (METH `(:METHOD ,(FLAVOR-NAME FL) ,SV)))
        (IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
                *JUST-COMPILING*)
            (COMPILE-AT-APPROPRIATE-TIME
              FL METH
              `(NAMED-LAMBDA (,METH) (IGNORE .NEWVALUE.)
                 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
                          (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
                 (SETQ ,V .NEWVALUE.)))
          (RECORD-SOURCE-FILE-NAME METH)))
      (LET* ((VV (CORRESPONDING-KEYWORD V))
             (METH `(:METHOD ,(FLAVOR-NAME FL) :CASE :SET ,VV)))
        (IF (OR (NOT (FLAVOR-NOTICE-METHOD METH))
                *JUST-COMPILING*)
            (COMPILE-AT-APPROPRIATE-TIME
              FL METH
              `(NAMED-LAMBDA (,METH) (IGNORE IGNORE .NEWVALUE.)
                 (DECLARE (FUNCTION-PARENT ,(FLAVOR-NAME FL) DEFFLAVOR)
                          (:SELF-FLAVOR ,(FLAVOR-NAME FL)))
                 (SETQ ,V .NEWVALUE.)))
          (RECORD-SOURCE-FILE-NAME METH))))))

;;; INTERN but always return-storage the print-name argument
(DEFUN INTERN1 (PNAME &OPTIONAL (PKG *PACKAGE*))
  (INTERN PNAME PKG))

;;; Given a symbol return the corresponding one in the keyword package
(DEFUN CORRESPONDING-KEYWORD (SYMBOL)
  (INTERN (SYMBOL-NAME SYMBOL) PKG-KEYWORD-PACKAGE))

;;; Make sure that the flavor bindings are up to date;
;;; see which instance variables are supposed to be special.
;;; We assume that the flavor has been composed.
(DEFUN COMPOSE-FLAVOR-BINDINGS (FL)
  (LET ((FLS (FLAVOR-DEPENDS-ON-ALL FL))
        (SPECIALS (FLAVOR-SPECIAL-INSTANCE-VARIABLES FL)))
    (DOLIST (F FLS)
      (SETQ F (GET F 'FLAVOR))
      (SETQ SPECIALS
            (UNION-EQ SPECIALS
                      (FLAVOR-SPECIAL-INSTANCE-VARIABLES F)))
      (COND ((FLAVOR-ALL-INSTANCE-VARIABLES-SPECIAL F)
             (OR (FLAVOR-DEPENDS-ON-ALL F)
                 (COMPOSE-FLAVOR-COMBINATION F))
             (SETQ SPECIALS
                   (UNION-EQ SPECIALS
                             (FLAVOR-ALL-INSTANCE-VARIABLES F)
                             (FLAVOR-ADDITIONAL-INSTANCE-VARIABLES F))))))
    ;; Any instance variables which the user has declared special elsewhere
    ;; ought to be special.
    (DOLIST (V (FLAVOR-ALL-INSTANCE-VARIABLES FL))
      ;>> BARF!
      (COND ((AND (NOT (MEMQ V SPECIALS))
                  (FBOUNDP 'COMPILER::SPECIALP)
                  (LET ((COMPILER::BARF-SPECIAL-LIST NIL))
                    (COMPILER::SPECIALP V)))
             (FORMAT *ERROR-OUTPUT* "~&Instance variable ~S of ~S being made special
because that variable is globally special~%"
                     V (FLAVOR-NAME FL))
             (PUSH V SPECIALS))))
    ;; Tell microcode about the instance variables
    (LET ((B (MAPCAR #'(LAMBDA (V) (IF (MEMQ V SPECIALS) (LOCF (SYMBOL-VALUE V))))
                     (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
      (DO ((BB B (CDR BB))
           (PREV (LOCF B) BB))
          ((NULL BB))
        (IF (NULL (CAR BB))
            (DO ((BBB BB (CDR BBB))
                 (I 0 (1+ I)))
                ((CAR BBB)
                 (SETF (CAR BB) I)
                 (SETF (CDR BB) BBB))
              (IF (NULL BBB)
                  (PROGN
                    (SETF (CDR PREV) NIL)
                    (SETF (CDR BB) NIL)
                    (RETURN NIL))))))
      (SETF (FLAVOR-BINDINGS FL) (COPY-LIST B)))))

;;;; Figure out the information needed to instantiate a flavor quickly.

;;; We store these three properties on the flavor:
;;; INSTANCE-VARIABLE-INITIALIZATIONS - alist of (ivar-index . init-form)
;;; REMAINING-DEFAULT-PLIST - default plist from which kwds that init ivars have been removed
;;; ALL-INITTABLE-INSTANCE-VARIABLES - list parallel to FLAVOR-ALL-INSTANCE-VARIABLES
;;;                                    which has either the keyword to init with or NIL.
;;; REMAINING-INIT-KEYWORDS - init keywords that are handled and dont just init ivars.

;;; We also set up the FLAVOR-DEFAULT-HANDLER of the flavor.

(DEFUN COMPOSE-FLAVOR-INITIALIZATIONS (FL &AUX ALIST
                                       REMAINING-DEFAULT-PLIST ALL-INITTABLE-IVARS
                                       AREA-FUNCTION REQUIRED-INIT-KEYWORDS
                                       remaining-init-keywords
                                       unhandled-init-keywords)
  (SETQ ALL-INITTABLE-IVARS (MAKE-LIST (LENGTH (FLAVOR-ALL-INSTANCE-VARIABLES FL))
                                      :AREA (IF *JUST-COMPILING* DEFAULT-CONS-AREA
                                               BACKGROUND-CONS-AREA)))
  (setf (flavor-default-handler fl) nil)
  ;; First make the mask saying which ivars can be initted by init kywords.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (LET ((FFL (COMPILATION-FLAVOR FFL)))
      (OR AREA-FUNCTION
          (SETQ AREA-FUNCTION (FLAVOR-GET FFL :INSTANCE-AREA-FUNCTION)))
      (SETQ REQUIRED-INIT-KEYWORDS
            (UNION-EQ REQUIRED-INIT-KEYWORDS (FLAVOR-GET FFL :REQUIRED-INIT-KEYWORDS)))
      (OR (FLAVOR-DEFAULT-HANDLER FL)
          (SETF (FLAVOR-DEFAULT-HANDLER FL)
                (GETF (FLAVOR-PLIST FFL) :DEFAULT-HANDLER)))
      (DOLIST (IIV (FLAVOR-INITTABLE-INSTANCE-VARIABLES FFL))
        (LET ((INDEX (FIND-POSITION-IN-LIST (CDR IIV) (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
          (AND INDEX
               (SETF (NTH INDEX ALL-INITTABLE-IVARS)
                     (CAR IIV)))))))
  (SETQ REMAINING-INIT-KEYWORDS
        (SUBSET-NOT #'MEMQ (FLAVOR-ALLOWED-INIT-KEYWORDS FL)
                    (CIRCULAR-LIST ALL-INITTABLE-IVARS)))
  (PUSHNEW :ALLOW-OTHER-KEYS REMAINING-INIT-KEYWORDS :TEST 'EQ)
  (SETF (FLAVOR-REMAINING-INIT-KEYWORDS FL) REMAINING-INIT-KEYWORDS)
  ;; Then look at all the default init plists, for anything there
  ;; that initializes an instance variable.  If it does, make an entry on ALIST.
  ;; Any that doesn't initialize a variable, put on the "remaining" list.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (COMPILATION-FLAVOR FFL))
    (DO ((L (GETF (FLAVOR-PLIST FFL) :DEFAULT-INIT-PLIST) (CDDR L))) ((NULL L))
      (LET* ((KEYWORD (CAR L)) (ARG (CADR L))
             (INDEX (FIND-POSITION-IN-LIST KEYWORD ALL-INITTABLE-IVARS)))
        ;; Remove this keyword from the list of required ones,
        ;; since it is cannot ever be missing.
        (SETQ REQUIRED-INIT-KEYWORDS
              (DELQ KEYWORD REQUIRED-INIT-KEYWORDS))
        (IF INDEX
            ;; This keyword initializes an instance variable,
            ;; so record an initialization of that variable if none found yet.
            (OR (ASSQ INDEX ALIST)
                (PUSH (LIST INDEX ARG)
                      ALIST))
          ;; This keyword does not just initialize an instance variable.
          (UNLESS (GET (LOCF REMAINING-DEFAULT-PLIST) KEYWORD)
            (PUTPROP (LOCF REMAINING-DEFAULT-PLIST) ARG KEYWORD))
          (UNLESS (MEMQ KEYWORD REMAINING-INIT-KEYWORDS)
            (PUSHNEW KEYWORD UNHANDLED-INIT-KEYWORDS))
          ;;(IF (MEMQ KEYWORD (FLAVOR-REMAINING-INIT-KEYWORDS FL))
          ;;    (OR (GET (LOCF REMAINING-DEFAULT-PLIST) KEYWORD)
          ;;        (PUTPROP (LOCF REMAINING-DEFAULT-PLIST) ARG KEYWORD))
          ;;  (FERROR "The flavor ~S has keyword ~S in its default init plist, but doesn't handle it" (FLAVOR-NAME FL) KEYWORD))
          ))))
  (SETF (FLAVOR-UNHANDLED-INIT-KEYWORDS FL) UNHANDLED-INIT-KEYWORDS)
  ;; Then, look for default values provided in list of instance vars.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (COMPILATION-FLAVOR FFL))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES FFL))
      (AND (NOT (ATOM V))
           ;; When we find one, put it in if there is no init for that variable yet.
           (LET ((INDEX (FIND-POSITION-IN-LIST (CAR V) (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
             (AND (NOT (ASSQ INDEX ALIST))
                  (PUSH (LIST INDEX
                              (CADR V))
                        ALIST))))))
  (IF AREA-FUNCTION
      (SETF (GETF (FLAVOR-PLIST FL) 'INSTANCE-AREA-FUNCTION) AREA-FUNCTION)
    (REMF (FLAVOR-PLIST FL) 'INSTANCE-AREA-FUNCTION))
  (IF REQUIRED-INIT-KEYWORDS
      (SETF (GETF (FLAVOR-PLIST FL) 'REQUIRED-INIT-KEYWORDS) REQUIRED-INIT-KEYWORDS)
    (REMF (FLAVOR-PLIST FL) 'REQUIRED-INIT-KEYWORDS))
  (SETF (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL) ALIST)
  (SETF (FLAVOR-REMAINING-DEFAULT-PLIST FL) REMAINING-DEFAULT-PLIST)
  (SETF (FLAVOR-ALL-INITTABLE-INSTANCE-VARIABLES FL) ALL-INITTABLE-IVARS))

;;; Method-combination functions.  Found on the SI:METHOD-COMBINATION property
;;; of the combination-type.  These are passed the flavor structure, and the
;;; magic-list entry, and must return the function-spec for the handler
;;; to go into the select-method, defining any necessary functions.
;;; This function interprets combination-type-arg,
;;; which for many combination-types is either :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.

;;; :DAEMON combination
;;; The primary method is the outermost untyped-method (or :DEFAULT).
;;; The :BEFORE methods are called base-flavor-last, the :AFTER methods are called
;;; base-flavor-first.  An important optimization is not to generate a combined-method
;;; if there is only a primary method.  You are allowed to omit the primary method
;;; if there are any daemons (I'm not convinced this is really a good idea) in which
;;; case the method's returned value will be NIL.
(DEFUN (:PROPERTY :DAEMON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER) T
                                                  ':BASE-FLAVOR-LAST)))
        (BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
                                             ':BASE-FLAVOR-LAST))
        (AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
                                            ':BASE-FLAVOR-FIRST))
        (WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
           (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS) PRIMARY-METHOD)
        (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
        (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
           (DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS)))))

(DEFUN DAEMON-COMBINATION (PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS
                           &OPTIONAL OR-METHODS AND-METHODS)
  (LET ((INNER-CALL (AND PRIMARY-METHOD (METHOD-CALL PRIMARY-METHOD))))
    (AND OR-METHODS (SETQ INNER-CALL
                          `(OR ,@(MAPCAR 'METHOD-CALL OR-METHODS)
                               ,INNER-CALL)))
    (AND AND-METHODS (SETQ INNER-CALL
                           `(AND ,@(MAPCAR 'METHOD-CALL AND-METHODS)
                                 ,INNER-CALL)))
    `(PROGN
       ,@(MAPCAR 'METHOD-CALL BEFORE-METHODS)
       ,(IF AFTER-METHODS
            `(MULTIPLE-VALUE-PROG1
               ,INNER-CALL
               . ,(MAPCAR 'METHOD-CALL AFTER-METHODS))
          ;; You are allowed to not have a primary method
          INNER-CALL))))

(DEFUN METHOD-CALL (METHOD)
  `(LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL
     #',METHOD
     (METHOD-MAPPING-TABLE ,METHOD)
     .DAEMON-CALLER-ARGS.))

;;; :DAEMON-WITH-OVERRIDE combination
;;; This is the same as :DAEMON (the default), except that :OVERRIDE type methods
;;; are combined with the :BEFORE-primary-:AFTER methods in an OR.  This allows
;;; overriding of the main methods function.  For example, a combined method as follows
;;; might be generated: (OR (FOO-OVERRIDE-BAR-METHOD) (PROGN (FOO-BEFORE-BAR-METHOD)))
(DEFUN (:PROPERTY :DAEMON-WITH-OVERRIDE METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
                                                  '(:BEFORE :AFTER :OVERRIDE) T
                                                  ':BASE-FLAVOR-LAST)))
        (BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
                                             ':BASE-FLAVOR-LAST))
        (AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
                                            ':BASE-FLAVOR-FIRST))
        (WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY))
        (OVERRIDE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY
                                               ':OVERRIDE T T NIL)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
           (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
             (NULL OVERRIDE-METHODS)
             PRIMARY-METHOD)
        (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
        (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
          `(OR ,@(MAPCAR #'METHOD-CALL OVERRIDE-METHODS)
               ,(DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS))))))

;;; :DAEMON-WITH-OR combination
;;; This is the same as :DAEMON (the default), except that :OR type methods
;;; are combined with the primary methods inside an OR, and used in place of
;;; the primary method in :DAEMON type combination.
;;; For example, the following combined method might be generated:
;;; (PROGN (FOO-BEFORE-BAR-METHOD)
;;;        (OR (FOO-OR-BAR-METHOD)
;;;            (BAZ-OR-BAR-METHOD)
;;;            (MULTIPLE-VALUE-PROG1
;;;              (BUZZ-PRIMARY-METHOD)
;;;              (FOO-AFTER-BAR-METHOD)))

(DEFUN (:PROPERTY :DAEMON-WITH-OR METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER :OR) T
                                                  ':BASE-FLAVOR-LAST)))
        (BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
                                             ':BASE-FLAVOR-LAST))
        (AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
                                            ':BASE-FLAVOR-FIRST))
        (WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY))
        (OR-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':OR T T NIL)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
           (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
             (NULL OR-METHODS)
             PRIMARY-METHOD)
        (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
        (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
          (DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS
                              OR-METHODS)))))

;;; :DAEMON-WITH-AND combination
;;; This is the same as :DAEMON (the default), except that :AND type methods
;;; are combined with the primary methods inside an AND, and used in place of
;;; the primary method in :DAEMON type combination.
;;; For example, the following combined method might be generated:
;;; (PROGN (FOO-BEFORE-BAR-METHOD)
;;;      (AND (FOO-AND-BAR-METHOD)
;;;           (BAZ-AND-BAR-METHOD)
;;;           (MULTIPLE-VALUE-PROG1
;;;             (BUZZ-PRIMARY-METHOD)
;;;             (FOO-AFTER-BAR-METHOD)))

(DEFUN (:PROPERTY :DAEMON-WITH-AND METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER :AND)
                                                  T ':BASE-FLAVOR-LAST)))
        (BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
                                             ':BASE-FLAVOR-LAST))
        (AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
                                            ':BASE-FLAVOR-FIRST))
        (WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY))
        (AND-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AND T T NIL)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
           (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
             (NULL AND-METHODS)
             PRIMARY-METHOD)
        (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
        (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
          (DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS
                              NIL AND-METHODS)))))

;;; :LIST combination
;;; No typed-methods allowed.  Returns a list of the results of all the methods.
;;; There will always be a combined-method, even if only one method to be called.
(DEFUN (:PROPERTY :LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
            `(LIST
               . ,(MAPCAR 'METHOD-CALL
                          (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':LIST '(NIL) T NIL)
                                  (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:LIST) NIL NIL)))))))

;;; :INVERSE-LIST combination
;;; No typed-methods allowed.  Apply each method to an element of the list.  Given
;;; the result of a :LIST-combined method with the same ordering, and corresponding
;;; method definitions, the result that emerged from each component flavor gets handed
;;; back to that same flavor.  The combined-method returns no particular value.
(DEFUN (:PROPERTY :INVERSE-LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
         `(LET ((.FOO. (CADR .DAEMON-CALLER-ARGS.)))
            . ,(DO ((ML (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':INVERSE-LIST '(NIL)
                                                     T NIL)
                                (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:INVERSE-LIST)
                                                     NIL NIL))
                        (CDR ML))
                    (R NIL))
                   ((NULL ML) (NREVERSE R))
                 (PUSH `(FUNCALL-WITH-MAPPING-TABLE-INTERNAL
                          #',(CAR ML) (METHOD-MAPPING-TABLE ,(CAR ML))
                          (CAR .DAEMON-CALLER-ARGS.) (CAR .FOO.))
                       R)
                 (AND (CDR ML) (PUSH '(SETQ .FOO. (CDR .FOO.)) R)))))))

;;; Combination types PROGN, AND, OR, MAX, MIN, +, APPEND, NCONC
;;; These just call all the untyped methods, inside the indicated special form.
;;; As an optimization, if there is only one method it is simply called.
;;; ?? There should be hair where methods with an extra keyword in them
;;; get to act as conditionals controlling which other methods get called,
;;; if anyone can ever specify exactly what this means.
(DEFPROP :PROGN SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :AND SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :OR SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :MAX SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :MIN SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :+ SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :APPEND SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :NCONC SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)

(DEFPROP :PROGN PROGN SIMPLE-METHOD-COMBINATION)
(DEFPROP :AND AND SIMPLE-METHOD-COMBINATION)
(DEFPROP :OR OR SIMPLE-METHOD-COMBINATION)
(DEFPROP :MAX MAX SIMPLE-METHOD-COMBINATION)
(DEFPROP :MIN MIN SIMPLE-METHOD-COMBINATION)
(DEFPROP :+ + SIMPLE-METHOD-COMBINATION)
(DEFPROP :APPEND APPEND SIMPLE-METHOD-COMBINATION)
(DEFPROP :NCONC NCONC SIMPLE-METHOD-COMBINATION)

(DEFUN SIMPLE-METHOD-COMBINATION (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY (CADR MAGIC-LIST-ENTRY) '(NIL)
                                              T NIL)
                         (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
                                              (LIST (CADR MAGIC-LIST-ENTRY)) NIL NIL)))
        (WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY)))
    (OR (AND (NOT WRAPPERS-P) (NULL (CDR METHODS)) (CAR METHODS))
        (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
        (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
           (CONS (GET (CADR MAGIC-LIST-ENTRY) 'SIMPLE-METHOD-COMBINATION)
                 (MAPCAR 'METHOD-CALL
                         METHODS))))))

(DEFUN (:PROPERTY :CASE METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET* ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
                                                   '(:CASE :OR :OTHERWISE :BEFORE :AFTER) T
                                                   ':BASE-FLAVOR-LAST)))
         (OTHERWISE-METHOD
           (OR (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':OTHERWISE T T
                                         ':BASE-FLAVOR-LAST))
               PRIMARY-METHOD))
         (BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
                                              ':BASE-FLAVOR-LAST))
         (AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
                                             ':BASE-FLAVOR-FIRST))
         (OR-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':OR T T
                                          ':BASE-FLAVOR-LAST))
         (METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':CASE T T NIL)))
    ;; Remove shadowed :otherwise methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ ':OTHERWISE (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
           (SETF (CDR MLE) (LIST OTHERWISE-METHOD))))
    ;; Remove shadowed primary methods too.
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (IF (EQ OTHERWISE-METHOD PRIMARY-METHOD)
          (AND (CDDR MLE)
               (SETF (CDR MLE) (LIST PRIMARY-METHOD)))
        ;; If there is a :OTHERWISE method, all the primary ones are shadowed.
        (AND MLE (DELQ MLE MAGIC-LIST-ENTRY))))
    (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
        (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
          (LET ((INNER-CALL
                  `(PROGN
                     ,@(MAPCAR #'METHOD-CALL BEFORE-METHODS)
                     (CASE (CADR .DAEMON-CALLER-ARGS.)
                       ,@(MAPCAR #'(LAMBDA (METHOD)
                                     `(,(FIFTH METHOD)
                                       ,(METHOD-CALL METHOD)))
                                 METHODS)
                       ((:GET-HANDLER-FOR :OPERATION-HANDLED-P :CASE-DOCUMENTATION)
                        (APPLY #'CASE-METHOD-DEFAULT-HANDLER
                               ',(FLAVOR-NAME FL)
                               ',(CAR MAGIC-LIST-ENTRY)
                               ',METHODS
                               (CDR .DAEMON-CALLER-ARGS.)))
                       (:WHICH-OPERATIONS
                        ;; Do not use FIFTH here; can lose at cold-load time.
                        ',(MAPCAR #'(LAMBDA (X) (CAR (CDDDDR X))) METHODS))
                       (T (OR ,@(MAPCAR #'METHOD-CALL OR-METHODS)
                              ,(IF OTHERWISE-METHOD
                                   (METHOD-CALL OTHERWISE-METHOD)
                                 `(APPLY #'CASE-METHOD-UNCLAIMED-MESSAGE-HANDLER
                                         ',(FLAVOR-NAME FL)
                                         ',(CAR MAGIC-LIST-ENTRY)
                                         ',METHODS
                                         (CDR .DAEMON-CALLER-ARGS.)))))))))
            ;; Copied from DAEMON-COMBINATION.
            (IF AFTER-METHODS
                `(MULTIPLE-VALUE-PROG1
                   ,INNER-CALL
                   . ,(MAPCAR #'METHOD-CALL AFTER-METHODS))
              ;; No :AFTER methods, hair not required
              ;; You are allowed to not have a primary method
              INNER-CALL))))))

;;; what a bogus name
(DEFUN CASE-METHOD-DEFAULT-HANDLER (FLAVOR OPERATION CASE-METHODS SUBOPERATION &REST ARGS)
  (DECLARE (IGNORE FLAVOR OPERATION))
  (DOLIST (CM CASE-METHODS)
    (IF (EQ (FIFTH CM) (CAR ARGS))
        (RETURN
          (CASE SUBOPERATION
            (:GET-HANDLER-FOR (FDEFINITION CM))
            (:OPERATION-HANDLED-P T)
            (:CASE-DOCUMENTATION (DOCUMENTATION CM)))))))

;;; what really should be called that
(DEFUN CASE-METHOD-UNCLAIMED-MESSAGE-HANDLER
       (FLAVOR OPERATION CASE-METHODS SUBOPERATION &REST ARGS)
  (DECLARE (IGNORE FLAVOR CASE-METHODS))
  (ERROR 'SYS:UNCLAIMED-MESSAGE
         :OBJECT SELF
         :MESSAGE `(,OPERATION ,SUBOPERATION)
         :ARGUMENTS ARGS))

;;; :PASS-ON combination
;;; The values from the individual methods are the arguments to the next one;
;;; the values from the last method are the values returned by the combined
;;; method.  Format is (:METHOD-COMBINATION (:PASS-ON (ORDERING . ARGLIST) . OPERATION-NAMES)
;;; ORDERING is :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.  ARGLIST can have &AUX and &OPTIONAL.

(DEFUN (:PROPERTY :PASS-ON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (APPEND (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':PASS-ON '(NIL)
                                              T (CAADDR MAGIC-LIST-ENTRY))
                         (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:PASS-ON)
                                              NIL (CAADDR MAGIC-LIST-ENTRY))))
        (ARGLIST (CDADDR MAGIC-LIST-ENTRY))
        ARGS REST-ARG-P)
    (DO ((L ARGLIST (CDR L))
         (ARG)
         (NL NIL))
        ((NULL L)
         (SETQ ARGS (NREVERSE NL)))
      (SETQ ARG (CAR L))
      (AND (CONSP ARG)
           (SETQ ARG (CAR ARG)))
      (COND ((EQ ARG '&REST)
             (SETQ REST-ARG-P T))
            ((EQ ARG '&AUX))
            (T
             (PUSH ARG NL))))
    (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
        (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
          `(DESTRUCTURING-BIND ,(CONS '.OPERATION. ARGLIST) SI:.DAEMON-CALLER-ARGS.
             . ,(DO ((METHS METHODS (CDR METHS))
                     (LIST NIL)
                     (METH))
                    ((NULL METHS)
                     (NREVERSE LIST))
                  (SETQ METH `(,(IF REST-ARG-P
                                    'LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL
                                  'FUNCALL-WITH-MAPPING-TABLE-INTERNAL)
                               #',(CAR METHS) (METHOD-MAPPING-TABLE ,(CAR METHS))
                               .OPERATION. . ,ARGS))
                  (AND (CDR METHS)
                       (SETQ METH (IF (NULL (CDR ARGS))
                                      `(SETQ ,(CAR ARGS) ,METH)
                                    `(MULTIPLE-VALUE ,ARGS ,METH))))
                  (PUSH METH LIST)))))))

;;; This function does most of the analysis of the magic-list-entry needed by
;;; method-combination functions, including most error checking.
(DEFUN GET-CERTAIN-METHODS (MAGIC-LIST-ENTRY METHOD-TYPE
                            OTHER-METHODS-ALLOWED NO-METHODS-OK ORDERING-DECLARATION
                            &AUX METHODS DEFAULT-METHODS)
  "Perform analysis needed by method-combination functions.
Returns a list of the method symbols for METHOD-TYPE extracted from MAGIC-LIST-ENTRY.
This value is shared with the data structure, don't bash it.
OTHER-METHODS-ALLOWED is a list of method types not to complain about (T = allow all).
NO-METHODS-OK = NIL means to complain if the returned value would be NIL.
ORDERING-DECLARATION is :BASE-FLAVOR-FIRST, :BASE-FLAVOR-LAST, or NIL meaning
take one of those symbols from the MAGIC-LIST-ENTRY."
  ;; Find the methods of the desired type, and barf at any extraneous methods
  (DOLIST (X (CDDDR MAGIC-LIST-ENTRY))
    (COND ((EQ (CAR X) METHOD-TYPE) (SETQ METHODS (CDR X)))
          ((ASSQ (CAR X) *SPECIALLY-COMBINED-METHOD-TYPES*) ) ;Wrappers ignored at this level
          ((ASSQ (CAR X) *INVERSE-SPECIALLY-COMBINED-METHOD-TYPES*) ) ;Wrappers ignored at this level
          ((EQ (CAR X) :DEFAULT)
           (SETQ DEFAULT-METHODS (CDR X)))
          ((OR (EQ OTHER-METHODS-ALLOWED T) (MEMQ (CAR X) OTHER-METHODS-ALLOWED)) )
          (T (FERROR "~S ~S method(s) illegal when using ~S method-combination"
                     (CAR X) (CAR MAGIC-LIST-ENTRY)
                     (OR (CADR MAGIC-LIST-ENTRY) :DAEMON)))))
  ;; If we were looking for primary methods and there are none, use the :DEFAULT methods.
  (AND (NULL METHOD-TYPE) (NULL METHODS)
       (SETQ METHODS DEFAULT-METHODS))
  ;; Complain if no methods supplied
  (AND (NULL METHODS) (NOT NO-METHODS-OK)
       (FERROR "No ~S ~S method(s) supplied to ~S method-combination"
               METHOD-TYPE (CAR MAGIC-LIST-ENTRY) (CADR MAGIC-LIST-ENTRY)))
  ;; Get methods into proper order.  Don't use NREVERSE!
  (CASE (OR ORDERING-DECLARATION (SETQ ORDERING-DECLARATION (CADDR MAGIC-LIST-ENTRY)))
    (:BASE-FLAVOR-FIRST )
    (:BASE-FLAVOR-LAST (SETQ METHODS (REVERSE METHODS)))
    (OTHERWISE (FERROR "~S invalid method combination order;
 must be ~S or ~S"
                        ORDERING-DECLARATION :BASE-FLAVOR-FIRST :BASE-FLAVOR-LAST)))
  METHODS)

(DEFUN SPECIALLY-COMBINED-METHODS-PRESENT (MLE)
  (LOOP FOR (TYPE) IN (CDDDR MLE)
     THEREIS (ASSQ TYPE *SPECIALLY-COMBINED-METHOD-TYPES*)))

;;; It is up to the caller to decide that a combined-method is called for at all.
;;; If one is, this function decides whether it already exists OK or needs
;;; to be recompiled.  Returns the symbol for the combined method if it is
;;; still valid, otherwise returns NIL.
;;; Always canonicalizes the magic-list-entry, since it will be needed
;;; canonicalized later.
(DEFUN HAVE-COMBINED-METHOD (FL MAGIC-LIST-ENTRY
                             &AUX OPERATION-NAME CMS MTE OLD-MLE OLD-CMS TEM OMETH)
  ;; Canonicalize the magic-list-entry so can compare with EQUAL
  (SETF (CDDDR MAGIC-LIST-ENTRY)                ;Canonicalize before comparing
        (SORT (CDDDR MAGIC-LIST-ENTRY) #'STRING-LESSP :KEY #'CAR))      ;Sort by method-type
  (SETQ OPERATION-NAME (CAR MAGIC-LIST-ENTRY))
  ;; See if we can inherit one in either the current or future (being-compiled) world,
  ;; or use an existing combined method of this flavor.
  ;; Get the :COMBINED method function spec for this flavor.  Note that if a suitable
  ;; one can be inherited, we will do so.
  ;; *USE-OLD-COMBINED-METHODS* controls whether we reuse an existing one for this
  ;; flavor; if we inherit one it will always be up-to-date already.
  ;; If all OK, return the function spec, else return NIL if new combined method must be made.
  (OR (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
        (LET ((FLAVOR1 (COMPILATION-FLAVOR FFL)))
          (AND (OR (NEQ FLAVOR1 FL) *USE-OLD-COMBINED-METHODS*)
               ;; ^ Combined methods of this flavor can be used only if permitted.
               (SETQ MTE (ASSQ OPERATION-NAME (FLAVOR-METHOD-TABLE FLAVOR1)))
               (SETQ OMETH (METH-LOOKUP (CDDDR MTE) :COMBINED))
               (METH-DEFINEDP OMETH)
               (OR (METH-DEFINITION OMETH)
                   (AND *JUST-COMPILING* (NEQ FL FLAVOR1)))
               (SETQ CMS (METH-FUNCTION-SPEC OMETH))
               (EQUAL MAGIC-LIST-ENTRY
                      (SETQ TEM
                            (OR (CADR (ASSQ 'COMBINED-METHOD-DERIVATION
                                            (AND (METH-DEFINITION OMETH)
                                                 (DEBUGGING-INFO (METH-DEFINITION OMETH) T))))
                                (GETF (METH-PLIST OMETH) 'COMBINED-METHOD-DERIVATION))))
               (OR (NOT (FBOUNDP 'COMPILER:EXPR-SXHASH))
                   (DOLIST (ELT (CDR (ASSQ 'WRAPPER-SXHASHES
                                           (AND (METH-DEFINITION OMETH)
                                                (DEBUGGING-INFO (METH-DEFINITION OMETH) T))))
                                T)              ;Return T if get thru whole list without mismatch.
                     ;; If any wrappers were used, make sure their definitions now
                     ;; match the definitions that were used to make the combined method.
                     (UNLESS (EQL (COMPILER:EXPR-SXHASH (CAR ELT)) (CADR ELT))
                       (RETURN NIL))))          ;Return NIL if mismatch.
               (RETURN CMS)))
        ;Save first combined-method seen for tracing, it's the one we would
        ;have been most likely to inherit
        (OR OLD-CMS (NULL CMS)
            (SETQ OLD-CMS CMS OLD-MLE TEM)))

      ;; Have to make a new combined method.  Trace if desired, but return NIL in any case.
      (PROGN
        (COND (*FLAVOR-COMPILE-TRACE*
               (FORMAT *FLAVOR-COMPILE-TRACE*
                       "~&~S's ~S combined method needs to be recompiled~%to come from "
                       (FLAVOR-NAME FL) OPERATION-NAME)
               (PRINT-COMBINED-METHOD-DERIVATION MAGIC-LIST-ENTRY *FLAVOR-COMPILE-TRACE*)
               (COND (OLD-CMS
                      (FORMAT *FLAVOR-COMPILE-TRACE*
                              "~%rather than using ~S which comes from " OLD-CMS)
                      (PRINT-COMBINED-METHOD-DERIVATION OLD-MLE *FLAVOR-COMPILE-TRACE*))
                     ((NOT *USE-OLD-COMBINED-METHODS*)
                      (FORMAT *FLAVOR-COMPILE-TRACE* "~%because of forced recompilation.")))))
        NIL)))

(DEFUN PRINT-COMBINED-METHOD-DERIVATION (MLE STREAM)
  (LOOP FOR (TYPE . FUNCTION-SPECS) IN (CDDDR MLE)
        DO (LOOP FOR FUNCTION-SPEC IN FUNCTION-SPECS DO (FORMAT STREAM "~S " FUNCTION-SPEC)))
  (IF (OR (CADR MLE) (CADDR MLE))
      (FORMAT STREAM "with method-combination ~S ~S" (CADR MLE) (CADDR MLE))))

;;; This function creates a combined-method, and returns the appropriate function spec.
;;; Its main job in life is to take care of wrappers.  Note the combined method
;;; always takes a single &REST argument named .DAEMON-CALLER-ARGS.
;;; FORM is a single form to be used as the body.
(DEFUN MAKE-COMBINED-METHOD (FL MAGIC-LIST-ENTRY FORM &AUX FSPEC
                             WRAPPERS WRAPPER-SXHASHES)
  "Creates a combined-method and returns the appropriate function spec.
Its main purpose in life is to take care of wrappers.  Note the combined
method always take a single &REST argument named .DAEMON-CALLER-ARGS.
FORM is a single form to be used as the body."
  (SETQ FORM `(COMPILE-TIME-REMEMBER-MAPPING-TABLE ,(FLAVOR-NAME FL) ,FORM))
  ;; Get the function spec which will name the combined-method
  (SETQ FSPEC `(:METHOD ,(FLAVOR-NAME FL) :COMBINED ,(CAR MAGIC-LIST-ENTRY)))
  ;; Put the wrappers and :AROUND methods around the form.
  ;; The base-flavor wrapper goes on the inside.
  (SETQ WRAPPERS (APPEND (GET-SPECIALLY-COMBINED-METHODS MAGIC-LIST-ENTRY FL)
                         (GET-INVERSE-SPECIALLY-COMBINED-METHODS MAGIC-LIST-ENTRY FL)))
  (DO ((WR WRAPPERS (CDR WR))
       (LAST-METHOD-TYPE NIL))
      ((NULL WR))
    (LET ((METHOD (CAR WR)))
      ;; Record sxhash of each wrapper that goes in.
      ;; This way we can tell if the combined method is obsolete when fasloaded.
      (WHEN (AND (MEMQ (CADDR METHOD) '(:WRAPPER :INVERSE-WRAPPER))
                 (FBOUNDP 'COMPILER:EXPR-SXHASH))
        (PUSH (LIST METHOD (COMPILER:EXPR-SXHASH METHOD))
              WRAPPER-SXHASHES))
      (SETQ FORM (FUNCALL (CADR (OR (ASSQ (CADDR METHOD) *SPECIALLY-COMBINED-METHOD-TYPES*)
                                    (ASSQ (CADDR METHOD)
                                          *INVERSE-SPECIALLY-COMBINED-METHOD-TYPES*)))
                          FL LAST-METHOD-TYPE METHOD FORM))
      (SETQ LAST-METHOD-TYPE (CADDR METHOD))))
  ;; Remember that it's going to be there, for HAVE-COMBINED-METHOD
  (FLAVOR-NOTICE-METHOD FSPEC)
  (IF *JUST-COMPILING*
      (FUNCTION-SPEC-PUTPROP FSPEC MAGIC-LIST-ENTRY 'COMBINED-METHOD-DERIVATION))
  ;; Compile the function.  It will be inserted into the flavor's tables either
  ;; now or when the QFASL file is loaded.
  (COMPILE-AT-APPROPRIATE-TIME
    FL
    FSPEC
    `(NAMED-LAMBDA (,FSPEC
                    ,@(IF WRAPPER-SXHASHES
                          `((SI:WRAPPER-SXHASHES . ,WRAPPER-SXHASHES)))
                    (SI:COMBINED-METHOD-DERIVATION ,MAGIC-LIST-ENTRY))
                   (&REST .DAEMON-CALLER-ARGS.
                          &AUX (.DAEMON-MAPPING-TABLE. SELF-MAPPING-TABLE))
                   ;Above variable referenced automatically by ucode
       .DAEMON-MAPPING-TABLE.  ;Prevent "unused local variable" warnings.
       .DAEMON-CALLER-ARGS.
       ,FORM)
    NIL)
  FSPEC)

;;; These macros are used in combined methods to compile the appropriate code
;;; to set the self mapping table from time to time.
;;; COMPILE-TIME-REMEMBER-MAPPING-TABLE goes around the entire method combination
;;; and METHOD-MAPPING-TABLE goes at each place where a specific mapping table
;;; is wanted.  METHOD-MAPPING-TABLE takes a method function spec as quoted arg
;;; and turns into code to return the appropriate mapping table.

(DEFVAR *COMPILER-FLAVOR*)

(DEFMACRO COMPILE-TIME-REMEMBER-MAPPING-TABLE (FLAVOR &BODY BODY)
  `(COMPILER-LET ((*COMPILER-FLAVOR* ',FLAVOR))
     . ,BODY))

(DEFMACRO METHOD-MAPPING-TABLE (METHOD-FUNCTION-SPEC)
  (OR (EQ (CAR METHOD-FUNCTION-SPEC) :METHOD)
      (FERROR "~S is not a method function-spec" METHOD-FUNCTION-SPEC))
  (LET ((FLAVOR (CADR METHOD-FUNCTION-SPEC)))
    (IF (EQ FLAVOR *COMPILER-FLAVOR*)
        '.DAEMON-MAPPING-TABLE.
      `(COMPILER::SELF-REF ,*COMPILER-FLAVOR* T ,FLAVOR))))

(DEFUN GET-SPECIALLY-COMBINED-METHODS (MLE *FL*)
  (DECLARE (SPECIAL *FL*))
  ;; First get all :AROUNDs followed by all :WRAPPERs,
  ;; then reorder by flavor but preserve the order of things for a given flavor.
  (STABLE-SORT (MAPCAN (LAMBDA (METHOD-TYPE-CONS)
                         (COPY-LIST (CDR (ASSQ (CAR METHOD-TYPE-CONS) (CDDDR MLE)))))
                       *SPECIALLY-COMBINED-METHOD-TYPES*)
               (LAMBDA (FS1 FS2)
                 ;; Return T if FS1's flavor comes later
                 ;; in our list of dependents than FS2's flavor.
                 (MEMQ (CADR FS1) (CDR (MEMQ (CADR FS2) (FLAVOR-DEPENDS-ON-ALL *FL*)))))))

(DEFUN GET-INVERSE-SPECIALLY-COMBINED-METHODS (MLE *FL*)
  (DECLARE (SPECIAL *FL*))
  ;; First get all :INVERSE-AROUNDs followed by all :INVERSE-WRAPPERs,
  ;; then reorder by flavor but preserve the order of things for a given flavor.
  (STABLE-SORT (MAPCAN #'(LAMBDA (METHOD-TYPE-CONS)
                           (COPYLIST (CDR (ASSQ (CAR METHOD-TYPE-CONS) (CDDDR MLE)))))
                       *INVERSE-SPECIALLY-COMBINED-METHOD-TYPES*)
               #'(LAMBDA (FS1 FS2)
                   ;; Return T if FS2's flavor comes later
                   ;; in our list of dependents than FS1's flavor.
                   (MEMQ (CADR FS2) (CDR (MEMQ (CADR FS1) (FLAVOR-DEPENDS-ON-ALL *FL*)))))))

(DEFUN PUT-WRAPPER-INTO-COMBINED-METHOD (FLAVOR PREVIOUS-METHOD-TYPE WRAPPER-NAME FORM)
  FLAVOR
  ;; Before any sequence of wrappers, stick on a binding of SELF-MAPPING-TABLE
  ;; because the body, a typical combined method, clobbers it,
  ;; but the code expanded by the wrapper itself may assume it is preserved.
  ;; If the last thing done was another wrapper, this is not necessary.
  (AND (NOT (MEMQ PREVIOUS-METHOD-TYPE '(:WRAPPER :INVERSE-WRAPPER)))
       (SETQ FORM `(LET ((SELF-MAPPING-TABLE SELF-MAPPING-TABLE))
                     ,FORM)))
  (LET ((DEF (COND ((DECLARED-DEFINITION WRAPPER-NAME))
                   ((FDEFINEDP WRAPPER-NAME)
                    (FDEFINITION WRAPPER-NAME))
                   (T (FERROR "~S supposed to be a wrapper macro, but missing!"
                              WRAPPER-NAME)))))
    (IF (EQ DEF 'ABORTED-DEFINITION)
        FORM
      (COND ((OR (ATOM DEF)
                 (NEQ (CAR DEF) 'MACRO))
             (FERROR "~S, supposed to be a wrapper macro, is poorly formed. Definiton is ~s"
                     WRAPPER-NAME DEF)))
      ;; Here we just put the wrapper in as a macro.  It will be expanded by the compiler.
      `(MACROCALL ,WRAPPER-NAME .DAEMON-CALLER-ARGS. ,FORM))))

;;;Sort of a macro version of funcall, for wrappers
(DEFMACRO MACROCALL (&REST X)
  (LET ((MACRO (COND ((DECLARED-DEFINITION (CAR X)))
                     ((FDEFINEDP (CAR X))
                      (FDEFINITION (CAR X)))
                     (T (FERROR "Unable to find definition of wrapper ~s at expand time"
                                (CAR X))))))
    (IF (AND (CONSP MACRO) (EQ (CAR MACRO) 'MACRO))
        (CALL (CDR MACRO) NIL X :OPTIONAL *MACROEXPAND-ENVIRONMENT*)
      ;;--- Temporary code so I can test things in the kludge environment
      (IF (AND (SYMBOLP MACRO) (EQ (CAR-SAFE (SYMBOL-FUNCTION MACRO)) 'MACRO))
          (CALL (CDR (SYMBOL-FUNCTION MACRO)) NIL X :OPTIONAL *MACROEXPAND-ENVIRONMENT*)
        (FERROR "~S evaluated to ~S, which is not a macro" (CAR X) MACRO)))))

(DEFUN PUT-AROUND-METHOD-INTO-COMBINED-METHOD (FLAVOR PREVIOUS-METHOD-TYPE
                                               METHOD-FUNCTION-SPEC FORM)
  PREVIOUS-METHOD-TYPE
  `(COMPILE-TIME-REMEMBER-MAPPING-TABLE ,(FLAVOR-NAME FLAVOR)
     (LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL
       #',METHOD-FUNCTION-SPEC
       (METHOD-MAPPING-TABLE ,METHOD-FUNCTION-SPEC)
       (CAR .DAEMON-CALLER-ARGS.)
       ;; This is the continuation.
       #'(LAMBDA (&REST .DAEMON-CALLER-ARGS.
                  &AUX (.DAEMON-MAPPING-TABLE. SYS:SELF-MAPPING-TABLE))
           .DAEMON-CALLER-ARGS. .DAEMON-MAPPING-TABLE.
           ,FORM)
       ;; This is the mapping table to give the continuation.
       .DAEMON-MAPPING-TABLE.
       ;; This is the list of args to give the continuation.
       .DAEMON-CALLER-ARGS.
       ;; These are the args, for the :around method to decode.
       (CDR .DAEMON-CALLER-ARGS.))))

(DEFSUBST AROUND-METHOD-CONTINUE (CONTINUATION MAPPING-TABLE ARGS)
  "Calls the continuation of the remaining methods inside an :AROUND
method.  Should pass the first three arguments the :AROUND method
received."
  (LEXPR-FUNCALL-WITH-MAPPING-TABLE CONTINUATION MAPPING-TABLE ARGS))

;;; (EVAL-WHEN (COMPILE EVAL LOAD)
(DEFUN FLAVOR-DECLARATION (FLAVOR-NAME &AUX FL)
  "Returns the FLAVOR declaration for use in methods,
DECLARE-FLAVOR-INSTANCE-VARIABLES, etc.  Declares all the instance
variables of the flavor, as well as the flavor name."
  (LET ((*JUST-COMPILING* (JUST-COMPILING)))
    (when (SETQ FL (COMPILATION-FLAVOR FLAVOR-NAME))
      (COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME)
             (OR (FLAVOR-DEPENDS-ON-ALL FL)
                 (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
                   (COMPOSE-FLAVOR-COMBINATION FL)))
             (LET ((VARS (FLAVOR-ALL-INSTANCE-VARIABLES FL))
                   (MORE-VARS (GETF (FLAVOR-PLIST FL) 'ADDITIONAL-INSTANCE-VARIABLES)))
               `(:SELF-FLAVOR ,FLAVOR-NAME ,(FLAVOR-GET-ALL-SPECIAL-INSTANCE-VARIABLES FL)
                              . ,(APPEND MORE-VARS VARS))))
            (T          ;Try to get as many variables as we can.
             `(:SELF-FLAVOR ,FLAVOR-NAME ,(FLAVOR-SPECIAL-INSTANCE-VARIABLES FL)
                            . ,(APPEND (GETF (FLAVOR-PLIST FL) 'ADDITIONAL-INSTANCE-VARIABLES)
                                       (MAP-OVER-COMPONENT-FLAVORS
                                         0 NIL NIL
                                         #'(LAMBDA (FL VL)
                                             (DOLIST (X (FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
                                               (OR (ATOM X) (SETQ X (CAR X)))
                                               (OR (MEMQ X VL) (PUSH X VL)))
                                             (APPEND VL (GETF (FLAVOR-PLIST FL)
                                                              :REQUIRED-INSTANCE-VARIABLES)))
                                         FLAVOR-NAME NIL))))))))

(DEFUN FLAVOR-GET-ALL-SPECIAL-INSTANCE-VARIABLES (FLAVOR)
  "Return a list of all the special instance variables of FLAVOR (a flavor object or name).
This function is for compatibility with flavors composed before
the ALL-SPECIAL-INSTANCE-VARIABLES property started being used."
  (IF (SYMBOLP FLAVOR) (SETQ FLAVOR (COMPILATION-FLAVOR FLAVOR)))
  (OR (FLAVOR-ALL-SPECIAL-INSTANCE-VARIABLES FLAVOR)
      (DO ((IVARS (FLAVOR-ALL-INSTANCE-VARIABLES FLAVOR) (CDR IVARS))
           (SPECIALS)
           (NORMAL-BINDINGS-LEFT (FLAVOR-BINDINGS FLAVOR))
           (NEXT-NORMAL-BINDING))
          ((NULL IVARS) SPECIALS)
        ;; Figure out whether the next ivar is bound as special by message sending.
        (OR (AND (NUMBERP NEXT-NORMAL-BINDING) (PLUSP NEXT-NORMAL-BINDING))
            (SETQ NEXT-NORMAL-BINDING (POP NORMAL-BINDINGS-LEFT)))
        (IF (NUMBERP NEXT-NORMAL-BINDING)
            (DECF NEXT-NORMAL-BINDING))
        ;; If it isn't, we must put it on our binding list to be bound now.
        (IF (LOCATIVEP NEXT-NORMAL-BINDING)
            (PUSH (CAR IVARS) SPECIALS)))))


;;;; Vanilla Flavor
;;; This is a flavor which is automatically made a component of nearly all
;;; other flavors.  It provides some basic facilities such as PRINT
;;; and DESCRIBE.

(EVAL-WHEN (LOAD EVAL)                          ;Allow this file to compile if it isn't loaded
(DEFFLAVOR VANILLA-FLAVOR () ()
  :NO-VANILLA-FLAVOR  ;No instance variables, no other flavors
  (:METHOD-COMBINATION (:CASE :BASE-FLAVOR-LAST :SET))
  (:DOCUMENTATION :MIXIN "The default base flavor.
This flavor provides the normal handlers for :PRINT, :DESCRIBE, :WHICH-OPERATIONS, etc
operations.  Only esoteric hacks should give the :NO-VANILLA-FLAVOR option to DEFFLAVOR to
prevent this inclusion."))
)       ;eval-when

(DEFMETHOD (VANILLA-FLAVOR :DEFAULT :INIT) (IGNORE) NIL)

(DEFMETHOD (VANILLA-FLAVOR :SET) (.SUBOPERATION. &REST ARGUMENTS)
  (ERROR 'SYS:UNCLAIMED-MESSAGE
         :OBJECT SELF :MESSAGE `(:SET , .SUBOPERATION.) :ARGUMENTS ARGUMENTS))

(DEFMETHOD (VANILLA-FLAVOR :PRINT-SELF) (STREAM &REST IGNORE)
  (PRINTING-RANDOM-OBJECT (SELF STREAM :TYPE)))

(DEFMETHOD (VANILLA-FLAVOR :DESCRIBE) ()
  (FORMAT T "~&~S, an object of flavor ~S,~% has instance variable values:~%"
            SELF (TYPE-OF SELF))
  (DO ((IVARS (FLAVOR-ALL-INSTANCE-VARIABLES (INSTANCE-FLAVOR SELF))
              (CDR IVARS))
       (I 1 (1+ I)))
      ((NULL IVARS))
    (FORMAT T " ~S:~27T " (CAR IVARS))
    (IF (= (%P-LDB-OFFSET %%Q-DATA-TYPE SELF I) DTP-NULL)
        (FORMAT T "void~%")
      (FORMAT T "~S~%" (%INSTANCE-REF SELF I)))))

(DEFMETHOD (VANILLA-FLAVOR :WHICH-OPERATIONS) ()
  (FLAVOR-WHICH-OPERATIONS (INSTANCE-FLAVOR SELF)))

(DEFMETHOD (VANILLA-FLAVOR :OPERATION-HANDLED-P) (OP)
  (LET ((FL (INSTANCE-FLAVOR SELF)))
    (IF (ARRAYP (FLAVOR-METHOD-HASH-ARRAY FL))
        (nth-value 1
          (WITHOUT-INTERRUPTS
            (send (DONT-OPTIMIZE
                    (hash-array-hash-table-instance (FLAVOR-METHOD-HASH-ARRAY FL)))
                  :get-hash op)))
      (NOT (NULL (MEMQ OP (OR (FLAVOR-WHICH-OPERATIONS FL) (SEND SELF :WHICH-OPERATIONS))))))))

(DEFMETHOD (VANILLA-FLAVOR :SEND-IF-HANDLES) (OP &REST TO-SEND)
  (LET ((FL (INSTANCE-FLAVOR SELF)))
    (IF (ARRAYP (FLAVOR-METHOD-HASH-ARRAY FL))
        (MULTIPLE-VALUE-BIND (FN-LOCATION DEFINEDP)
            (WITHOUT-INTERRUPTS
              (SEND (DONT-OPTIMIZE
                      (hash-array-hash-table-instance (FLAVOR-METHOD-HASH-ARRAY FL)))
                    :GET-HASH OP))
          (IF DEFINEDP (APPLY (CAR FN-LOCATION) OP TO-SEND)))
      (AND (MEMQ OP (OR (FLAVOR-WHICH-OPERATIONS FL) (SEND SELF :WHICH-OPERATIONS)))
           (LEXPR-SEND SELF OP TO-SEND)))))

(DEFMETHOD (VANILLA-FLAVOR :GET-HANDLER-FOR) (OP)
  (GET-HANDLER-FOR SELF OP))

(DEFMETHOD (VANILLA-FLAVOR :EVAL-INSIDE-YOURSELF) (FORM)
  (WITH-SELF-VARIABLES-BOUND (si:eval-special-ok FORM)))

(DEFMETHOD (VANILLA-FLAVOR :FUNCALL-INSIDE-YOURSELF) (FUNCTION &REST ARGS)
  (WITH-SELF-VARIABLES-BOUND (APPLY FUNCTION ARGS)))

(DEFMETHOD (VANILLA-FLAVOR :BREAK) ()
  (WITH-SELF-VARIABLES-BOUND (BREAK "~S" SELF)))


;;;; PROPERTY-LIST-MIXIN
;;; This flavor is a useful mixin that provides messages for a property list protocol.

(DEFFLAVOR PROPERTY-LIST-MIXIN ((PROPERTY-LIST NIL)) ()
  :SETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :MIXIN "A mixin that provides property list messages."))

(DEFMETHOD (PROPERTY-LIST-MIXIN :GET) (PROPERTY-NAME &OPTIONAL DEFAULT)
  (GETF PROPERTY-LIST PROPERTY-NAME DEFAULT))

(DEFMETHOD (PROPERTY-LIST-MIXIN :GET-LOCATION-OR-NIL) (PROPERTY-NAME &OPTIONAL IGNORE)
  (GET-LOCATION-OR-NIL (LOCF PROPERTY-LIST) PROPERTY-NAME))

(DEFMETHOD (PROPERTY-LIST-MIXIN :GET-LOCATION) (PROPERTY-NAME &OPTIONAL DEFAULT)
  (LOCF (GETF PROPERTY-LIST PROPERTY-NAME DEFAULT)))

(DEFMETHOD (PROPERTY-LIST-MIXIN :GET-LOCATION-from-area) (PROPERTY-NAME area &OPTIONAL DEFAULT)
  (LOCF (GETF-from-area PROPERTY-LIST PROPERTY-NAME area DEFAULT)))

(DEFMETHOD (PROPERTY-LIST-MIXIN :GETL) (PROPERTY-NAME-LIST)
  (GETL (LOCF PROPERTY-LIST) PROPERTY-NAME-LIST))

(DEFMETHOD (PROPERTY-LIST-MIXIN :PUTPROP) (VALUE PROPERTY-NAME)
  (SETF (GETF PROPERTY-LIST PROPERTY-NAME) VALUE))

(DEFMETHOD (PROPERTY-LIST-MIXIN :PUTPROP-in-area) (VALUE PROPERTY-NAME area)
  (SETF (GETF-from-area PROPERTY-LIST PROPERTY-NAME area) VALUE))

(DEFMETHOD (PROPERTY-LIST-MIXIN :CASE :SET :GET) (PROPERTY-NAME &REST VALUE)
  (DECLARE (ARGLIST PROPERTY-NAME VALUE))
  ;; use CAR LAST to ignore optional default eg from "(push zap (send foo :get bar baz))"
  (SETF (GETF PROPERTY-LIST PROPERTY-NAME) (CAR (LAST VALUE))))

(DEFMETHOD (PROPERTY-LIST-MIXIN :REMPROP) (PROPERTY-NAME)
  (REMF PROPERTY-LIST PROPERTY-NAME))

(DEFMETHOD (PROPERTY-LIST-MIXIN :PUSH-PROPERTY) (VALUE PROPERTY-NAME)
  (PUSH VALUE (GETF PROPERTY-LIST PROPERTY-NAME)))

(DEFMETHOD (PROPERTY-LIST-MIXIN :PLIST) () PROPERTY-LIST)

(DEFMETHOD (PROPERTY-LIST-MIXIN :PLIST-LOCATION) () (LOCF PROPERTY-LIST))
(DEFMETHOD (PROPERTY-LIST-MIXIN :PROPERTY-LIST-LOCATION) () (LOCF PROPERTY-LIST))

(DEFMETHOD (PROPERTY-LIST-MIXIN :SETPLIST) (NEW-PLIST)
  (SETQ PROPERTY-LIST NEW-PLIST))
(DEFMETHOD (PROPERTY-LIST-MIXIN :CASE :SET :PLIST) (NEW-PLIST)
  (SETQ PROPERTY-LIST NEW-PLIST))

(DEFCONST INSTANCE-INVOKE-VECTOR-CONTENTS
          '(:GET :GETL :GET-LOCATION-OR-NIL :CAR :CDR :SET-CAR :SET-CDR)
  "A list of elements to copy into the value of INSTANCE-INVOKE-VECTOR.")

(DEFVAR INSTANCE-INVOKE-VECTOR :UNBOUND
  "A vector of operations that the microcode wants to perform on instances.
Indices in this vector are defined in SYS:UCODE;UC-PARAMETERS LISP.
The vector may not be forwarded.")

(DEFUN INIT-INSTANCE-INVOKE-VECTOR ()
  (SETQ INSTANCE-INVOKE-VECTOR (MAKE-ARRAY (LENGTH INSTANCE-INVOKE-VECTOR-CONTENTS)
                                           :INITIAL-CONTENTS INSTANCE-INVOKE-VECTOR-CONTENTS)))

(ADD-INITIALIZATION 'INIT-INSTANCE-INVOKE-VECTOR '(INIT-INSTANCE-INVOKE-VECTOR) '(ONCE))


;;;; PRINT-READABLY-MIXIN
;;; This flavor makes your instance print out using horseshoes, and read back in.
(DEFFLAVOR PRINT-READABLY-MIXIN () ()
  (:REQUIRED-METHODS :RECONSTRUCTION-INIT-PLIST))

(DEFMETHOD (PRINT-READABLY-MIXIN :PRINT-SELF) (STREAM &REST IGNORE)
  (SEND STREAM :STRING-OUT "#")
  (LET ((*PACKAGE* PKG-USER-PACKAGE))
    (PRIN1 (TYPE-OF SELF) STREAM))
  (SEND STREAM :TYO #/SP)
  (DO ((INIT-OPTIONS (SEND SELF :RECONSTRUCTION-INIT-PLIST) (CDDR INIT-OPTIONS)))
      ((NULL INIT-OPTIONS))
    (PRIN1 (CAR INIT-OPTIONS) STREAM)
    (SEND STREAM :TYO #/SP)
    (PRIN1 (CADR INIT-OPTIONS) STREAM)
    (IF (CDDR INIT-OPTIONS)
        (SEND STREAM :TYO #/SP)))
  (SEND STREAM :TYO #/))

;>>> **BLORP**
(DEFMETHOD (PRINT-READABLY-MIXIN :READ-INSTANCE) (FLAVOR STREAM)
  (DO (CH INIT-OPTIONS)
      (())
    ;; Skip past spaces.
    (DO ()
        ((NOT (= (SETQ CH (SEND STREAM :TYI)) #/SP))
         (SEND STREAM :UNTYI CH)))
    (IF (= CH #/)
        (RETURN (APPLY #'MAKE-INSTANCE FLAVOR INIT-OPTIONS)))
    (SETQ INIT-OPTIONS
          (LIST* (CL:READ STREAM T NIL T)
                 (CL:READ STREAM T NIL T)
                 INIT-OPTIONS))))

(DEFUN GET-HANDLER-FOR (FUNCTION OPERATION &OPTIONAL (SUPERIORS-P T) &AUX TEM)
  "Given a functional object, return its subfunction to do the given operation or NIL.
   Returns NIL if it does not reduce to a select-method or if it does not handle that."
  (DO-FOREVER                                   ;Repeat until reduced to a select-method
    (SELECT (%DATA-TYPE FUNCTION)               ;  (if possible)
      (DTP-ARRAY-POINTER
       ;; Set function to NIL or named-structure handler
       (SETQ FUNCTION (GET (NAMED-STRUCTURE-P FUNCTION) 'NAMED-STRUCTURE-INVOKE)))
      (DTP-SYMBOL
       (OR (FBOUNDP FUNCTION) (RETURN NIL))
       (SETQ FUNCTION (FSYMEVAL FUNCTION)))
      ((DTP-ENTITY DTP-CLOSURE)
       (SETQ FUNCTION (CAR (%MAKE-POINTER DTP-LIST FUNCTION))))
      (DTP-SELECT-METHOD
       (SETQ FUNCTION (%MAKE-POINTER DTP-LIST FUNCTION))
       (DO-FOREVER
         ;; Iterate down select-method, then continue with tail
         (COND ((SYMBOLP (CAR FUNCTION))        ;One level subroutine call
                (AND SUPERIORS-P
                     (SETQ TEM (GET-HANDLER-FOR FUNCTION OPERATION NIL))
                     (RETURN-FROM GET-HANDLER-FOR TEM)))
               ((IF (CONSP (CAAR FUNCTION))
                    (MEMQ OPERATION (CAAR FUNCTION))
                  (EQ OPERATION (CAAR FUNCTION)))
                (RETURN-FROM GET-HANDLER-FOR (CDAR FUNCTION))))
         (SETQ FUNCTION (CDR FUNCTION))
         (OR (CONSP FUNCTION) (RETURN NIL))))
      (DTP-INSTANCE
       (SETQ FUNCTION (INSTANCE-FUNCTION FUNCTION))
       (IF (ARRAYP FUNCTION)
           (RETURN-FROM GET-HANDLER-FOR
             (CAR (WITHOUT-INTERRUPTS
                    (SEND (DONT-OPTIMIZE
                            (hash-array-hash-table-instance FUNCTION))
                          :GET-HASH OPERATION))))))
      (OTHERWISE
       (RETURN-FROM GET-HANDLER-FOR NIL)))))

(DEFUN GET-FLAVOR-TRACING-ALIASES (FLAVOR-NAME &AUX FL)
  "Return the flavor object for FLAVOR-NAME, or the one it is an alias for ..."
  (DO ((NAME FLAVOR-NAME)) (())
    (SETQ FL (GET NAME 'FLAVOR))
    (UNLESS FL (RETURN NIL))
    (IF (FLAVOR-GET FL :ALIAS-FLAVOR)
        (SETQ NAME (CAR (FLAVOR-DEPENDS-ON FL)))
      (RETURN FL))))

;;; Get the function that would handle an operation for a flavor
(DEFUN GET-FLAVOR-HANDLER-FOR (FLAVOR-NAME OPERATION &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET-FLAVOR-TRACING-ALIASES FLAVOR-NAME))
             "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-ARRAY FL) (COMPOSE-METHOD-COMBINATION FL))
  (IF (EQ (FLAVOR-METHOD-HASH-ARRAY FL) T)
      (FERROR "The flavor ~S is an ~S." FLAVOR-NAME :ABSTRACT-FLAVOR))
  (CAR (WITHOUT-INTERRUPTS
         (SEND (DONT-OPTIMIZE
                 (hash-array-hash-table-instance (FLAVOR-METHOD-HASH-ARRAY FL)))
               :GET-HASH OPERATION))))

;;; (:HANDLER flavor operation) refers to the function that is called when
;;;   an object of flavor FLAVOR is sent the message OPERATION.
;;; Storing into this changes the value in the method table for that specific flavor
;;;  which should make it possible to trace and so forth.
(DEFPROP :HANDLER HANDLER-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN HANDLER-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
        (OPERATION (THIRD FUNCTION-SPEC)))
    ;; Checking structure like :INTERNAL
    (AND (SYMBOLP FLAVOR)
         (LET ((FL (GET-FLAVOR-TRACING-ALIASES FLAVOR)))
           (OR FL (FERROR 'SYS:INVALID-FUNCTION-SPEC
                          "In the function spec ~S, ~S is not the name of a flavor"
                          FUNCTION-SPEC FLAVOR))
           ;; Do any composition (compilation) of combined stuff, if not done already
           (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
           (OR (FLAVOR-METHOD-HASH-ARRAY FL) (COMPOSE-METHOD-COMBINATION FL))
           (IF (EQ (FLAVOR-METHOD-HASH-ARRAY FL) T)
               (FERROR "The flavor ~S is an ~S." FLAVOR :ABSTRACT-FLAVOR))
           (LET ((LOC (WITHOUT-INTERRUPTS ;Location of method
                        (SEND (DONT-OPTIMIZE
                                (hash-array-hash-table-instance (FLAVOR-METHOD-HASH-ARRAY FL)))
                              :get-hash OPERATION))))
             (OR (NOT (NULL LOC))
                 (MEMQ FUNCTION '(VALIDATE-FUNCTION-SPEC FDEFINEDP))
                 (FERROR "The flavor ~S does not handle the ~S operation."
                         FLAVOR OPERATION))
             (CASE FUNCTION
               (VALIDATE-FUNCTION-SPEC T)
               (FDEFINE (RPLACD LOC ARG1))
               (FDEFINITION (CDR LOC))
               (FDEFINEDP LOC)
               (FDEFINITION-LOCATION LOC)
               (FUNDEFINE (FERROR "~S is not implemented for ~S" 'FUNDEFINE 'HANDLER))
               (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))))


(DEFSETF %INSTANCE-REF SET-%INSTANCE-REF)
(DEFLOCF %INSTANCE-REF %INSTANCE-LOC)

;This is in SYS: WINDOW; COLD now
;(DEFSUBST SYMEVAL-IN-INSTANCE (INSTANCE PTR)
;  "Return the value of instance variable PTR in INSTANCE.
;PTR can be a pointer to a value cell instead of a symbol.
;Error if PTR does not work out to be a symbol which is an
;instance variable in INSTANCE."
;  (CONTENTS (LOCATE-IN-INSTANCE INSTANCE PTR)))

(DEFSUBST SET-IN-INSTANCE (INSTANCE PTR VAL)
  "Set the value of instance variable PTR in INSTANCE to VAL.
PTR can also be a locative pointer to a value cell."
  (SETF (CONTENTS (LOCATE-IN-INSTANCE INSTANCE PTR)) VAL))

(DEFLOCF SYMEVAL-IN-INSTANCE LOCATE-IN-INSTANCE)
(DEFSETF SYMEVAL-IN-INSTANCE SET-IN-INSTANCE)

; LOCATE-IN-INSTANCE is now microcoded.

(DEFUN SYMEVAL-MAYBE-IN-INSTANCE (INSTANCE PTR)
  "Try SYMEVAL-IN-INSTANCE; if not an instance variable of INSTANCE, then do SYMBOL-VALUE"
  (CHECK-TYPE INSTANCE INSTANCE)
  (OR (SYMBOLP PTR) (SETQ PTR (%FIND-STRUCTURE-HEADER PTR)))
  (LET ((N (FIND-POSITION-IN-LIST PTR (FLAVOR-ALL-INSTANCE-VARIABLES
                                        (INSTANCE-FLAVOR INSTANCE)))))
    (IF N
        (%INSTANCE-REF INSTANCE (1+ N))
        (SYMBOL-VALUE PTR))))

;;; Interface to the compiler.
;;; If called in *JUST-COMPILING* mode, during a QC-FILE, sends output into the QFASL file.
;;; If called during a compilation to core, for instance from
;;; the editor Compile-to-Core command, compiles to core as part of the compilation
;;; in progress (assuming you are in the top level macro-expanding part of the
;;; compiler rather than deep inside its guts).  If called at a random time,
;;; simply compiles to core.
;;; Note that if LOCAL-DECLARATIONS is bound when this is called it will be obeyed.
(DEFUN COMPILE-AT-APPROPRIATE-TIME (FL NAME LAMBDA-EXP &OPTIONAL FORM-TO-EVAL)
  ;; Switch to the appropriate package so gensyms get defined in that package and
  ;; and error messages about wrong package defining a function are avoided.  But
  ;; if compiling, don't mess with the package, so that symbols in the qfasl file
  ;; get interned in the proper place.
  (LET ((*PACKAGE* (IF COMPILER::QC-FILE-IN-PROGRESS *PACKAGE* (FLAVOR-PACKAGE FL)))
        ;; Declare the instance variables for the code being compiled.
        (LOCAL-DECLARATIONS (LIST* (FLAVOR-DECLARATION (FLAVOR-NAME FL))
                                   LOCAL-DECLARATIONS)))
    (IF COMPILER::QC-FILE-IN-PROGRESS
        ;; This case if in QC-FILE or editor-compile
        (IF *JUST-COMPILING*
            ;; Here if QC-FILE.  If it's a combined method,
            ;; actually FDEFINE a FASLOAD-COMBINED method when we load,
            ;; but make the FEF's name say :COMBINED.
            (COMPILER::QC-TRANSLATE-FUNCTION
              (IF (AND (= 4 (LENGTH NAME)) (EQ (THIRD NAME) :COMBINED))
                  (LIST* (FIRST NAME) (SECOND NAME) 'FASLOAD-COMBINED (CDDDR NAME))
                NAME)
              LAMBDA-EXP 'COMPILER::MACRO-COMPILE
              'COMPILER::QFASL NAME)
          ;; Here for compiling from editor buffer, or QC-FILE to core.
          (COMPILER::LOCKING-RESOURCES-NO-QFASL
            (LET ((INHIBIT-FDEFINE-WARNINGS T))
              (PUSH (LIST NAME FDEFINE-FILE-PATHNAME) *FLAVOR-COMPILATIONS*)
              (COMPILER::QC-TRANSLATE-FUNCTION
                NAME LAMBDA-EXP 'COMPILER::MACRO-COMPILE 'COMPILER::COMPILE-TO-CORE))))
      ;; This case if not doing anything special
      (PUSH (LIST NAME FDEFINE-FILE-PATHNAME) *FLAVOR-COMPILATIONS*)
      (LET ((FDEFINE-FILE-PATHNAME NIL)
            (INHIBIT-FDEFINE-WARNINGS T))
        ;; If the compiler is not loaded, try to limp through with interpreted methods
        (FUNCALL (IF (FBOUNDP 'COMPILE)
                     'COMPILE
                   'FDEFINE)
                 NAME LAMBDA-EXP)))
    ;; Evaluate form now or send it over in the qfasl file
    (AND FORM-TO-EVAL
         (IF *JUST-COMPILING*
             (COMPILER:FASD-FORM FORM-TO-EVAL)
           (EVAL FORM-TO-EVAL)))))

(DEFMACRO COMPILE-FLAVOR-METHODS (&REST FLAVOR-NAMES)
  "In a file being compiled, put combined methods of flavors into the QFASL file."
  `(PROGN
     (EVAL-WHEN (COMPILE)
       . ,(MAPCAN #'(LAMBDA (FLAVOR-NAME)
                      (NCONC (AND (GET FLAVOR-NAME 'FLAVOR)
                                  (NCONS `(PUTPROP (LOCF (FLAVOR-PLIST
                                                           (GET ',FLAVOR-NAME 'FLAVOR)))
                                                   T
                                                   'COMPILE-FLAVOR-METHODS)))
                             (NCONS `(COMPILE-FLAVOR-METHODS-1 ',FLAVOR-NAME))))
                  FLAVOR-NAMES))
     (EVAL-WHEN (LOAD EVAL)
       . ,(MAPCAR #'(LAMBDA (FLAVOR-NAME) `(COMPILE-FLAVOR-METHODS-2 ',FLAVOR-NAME))
                  FLAVOR-NAMES))))

;;; Cause the combined-methods to get compiled.
;;; Executed only from the compiler, and does something
;;; only if compiling to a file.
(DEFUN COMPILE-FLAVOR-METHODS-1 (FLAVOR-NAME &AUX FL)
  (IF (JUST-COMPILING)
      (LET ((*JUST-COMPILING* T)
            (*USE-OLD-COMBINED-METHODS* NIL))
        (COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME 'COMPILE-FLAVOR-METHODS)
               (SETQ FL (COMPILATION-FLAVOR FLAVOR-NAME))
               ;; Make sure we are not hacking the installed flavor object,
               ;; in case there is no defflavor or defmethod for the flavor in this file.
               (AND (EQ FL (GET FLAVOR-NAME 'FLAVOR))
                    (COMPILATION-DEFINE-FLAVOR
                      FLAVOR-NAME
                      (SETQ FL (FLAVOR-REDEFINITION-FOR-COMPILATION FL NIL))))
               (OR (FLAVOR-DEPENDS-ON-ALL FL)
                   (COMPOSE-FLAVOR-COMBINATION FL))
               (COMPOSE-METHOD-COMBINATION FL NIL)
               (DOLIST (ALTERNATIVE (GET-RUN-TIME-ALTERNATIVE-FLAVOR-NAMES FL))
                 (COMPILE-FLAVOR-METHODS-1 ALTERNATIVE)))))))

;;; Do the composition now.  This should normally just generate data-structure
;;; as the methods should already all have been compiled, unless something has changed.
(DEFPROP COMPILE-FLAVOR-METHODS-2 T QFASL-DONT-RECORD)
(DEFUN COMPILE-FLAVOR-METHODS-2 (FLAVOR-NAME &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  (SETF (GETF (FLAVOR-PLIST FL) 'COMPILE-FLAVOR-METHODS) (OR FDEFINE-FILE-PATHNAME T))
  (WHEN (FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME)
    (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
    (OR (FLAVOR-METHOD-HASH-ARRAY FL)
        (COMPOSE-METHOD-COMBINATION FL))
    (DOLIST (ALTERNATIVE (GET-RUN-TIME-ALTERNATIVE-FLAVOR-NAMES FL))
      (COMPILE-FLAVOR-METHODS-2 ALTERNATIVE)))
  FLAVOR-NAME)

(DEFUN FLAVOR-COMPONENTS-DEFINED-P (FLAVOR-NAME &OPTIONAL COMPLAINT &AUX FL)
  "Returns T if all components of this flavor are defined.
If COMPLAINT is non-NIL, a message containing it is printed
if not all components are defined."
  (COND ((SETQ FL (COMPILATION-FLAVOR FLAVOR-NAME))
         (OR (NOT (NULL (FLAVOR-DEPENDS-ON-ALL FL)))    ;Already composed, be fast
             (AND (DO ((L (FLAVOR-DEPENDS-ON FL) (CDR L))) ((NULL L) T)
                    (OR (FLAVOR-COMPONENTS-DEFINED-P (CAR L)) (RETURN NIL)))
                  (DO ((L (FLAVOR-INCLUDES FL) (CDR L))) ((NULL L) T)
                    (OR (FLAVOR-COMPONENTS-DEFINED-P (CAR L)) (RETURN NIL)))
                  (DO ((L (GETF (FLAVOR-PLIST FL) :REQUIRED-FLAVORS)
                          (CDR L)))
                      ((NULL L) T)
                    (OR (FLAVOR-COMPONENTS-DEFINED-P (CAR L)) (RETURN NIL))))))
        (COMPLAINT (FORMAT *ERROR-OUTPUT* "~&~A - ~S undefined flavor" COMPLAINT FLAVOR-NAME)
                   NIL)
        (T NIL)))

;;; Tentative support for generic functions.
;;; The main motivation for this is to make it easy to define functional interfaces
;;; to system facilities that used to only have message interfaces.
(defvar *function-support* '())

(defstruct (function-support (:type list) (:conc-name fs-))
  function
  message
  optimizer)

(defun find-function-support (function)
  (lisp:assoc function *function-support*))

(defun generic-function-message (function)
  (fs-message (find-function-support function)))

;;; Let's get rid of obsolete messages while we're are it.
(defvar *message-synonyms* '())

(defsubst get-message-synonym (message)
  (cdr (lisp:assoc message *message-synonyms*)))

(defun find-message-support-1 (message)
  (car (lisp:member message *function-support* :key #'fs-message)))

(defun find-message-support (message)
  (find-message-support-1 (or (get-message-synonym message) message)))

(defmacro defmessage-synonym (message for)
  `(set-message-synonym ',message ',for))

(defun set-message-synonym (message for)
  (setq *function-support* (lisp:delete (find-message-support message) *function-support*))
  (let ((entry (lisp:assoc message *message-synonyms*)))
    (if entry
        (setf (cdr entry) for)
      (consing-in-area (permanent-storage-area)
        (push (cons message for) *message-synonyms*))))
  message)

(defun message-generic-function (message)
  (fs-function (find-message-support message)))

(defun msg-function (message)
  (or (message-generic-function message)
      (error "No generic function for message ~S" message)))

(defun function-msg (function)
  (or (generic-function-message function)
      (error "No message for generic function ~S" function)))

;;; Note that this preserves optimizers
(defun set-generic-operation (function flavor-message)
  (let ((fs (find-function-support function)))
    (if fs
        (setf (fs-message fs) flavor-message)
      (consing-in-area (permanent-storage-area)
        (push (make-function-support :function function :message flavor-message)
              *function-support*))))
  function)

(defun set-optimizer-support (function optimizer)
  (let ((fs (find-function-support function)))
    (unless fs
      (error "No support previously defined for ~S" function))
    (setf (fs-optimizer fs) optimizer)))

(defun optimize-generic-call (form)
  (let ((optimizer (fs-optimizer (find-function-support (car form)))))
    (if optimizer
        (funcall optimizer form)
      form)))

(defun generic-inline-send (form)
  (let ((message (generic-function-message (car form))))
    (if message
        `(zl:send ,(cadr form) ',message ,@(cddr form))
      form)))

(defvar *use-generic-function-as-message* nil)

(defun check-compatible-message (generic-function-name message)
  (cond (message)
        (*use-generic-function-as-message*
         (setq message generic-function-name)
         (when (eq *use-generic-function-as-message* :warn)
           (warn "No explicit message message for generic function ~S." generic-function-name)))
        (t
         (multiple-cerror '(no-compatible-message) ()
             ("No compatible message for generic function ~S." generic-function-name)
           ("Use the generic function name as the message."
            (setq message generic-function-name))
           ("Supply a message name for the generic function."
            (loop
              (setq message (prompt-and-read (list :eval-read :default generic-function-name)
                                              "Message name for ~S: " generic-function-name))
              (if (typep message '(and symbol (not null)))
                  (return nil)
                (format *query-io* "~&~S is not a valid message name.~%" message)))))))
  message)

(defun expand-defgeneric-old-flavors (function arglist options)
  (let ((declarations '()) (documentation nil) (message nil) (pending-methods '())
        ;; There seems to be a bug in the spec -- Brand S says that :METHOD-ARGLIST
        ;; is used only by :FUNCTION (which we can't implement right now).  The :METHOD
        ;; option, which allows DEFMETHODS to happen, says it uses main arglist.  But it
        ;; would seem more proper to use the :METHOD-ARGLIST if :METHODS and :FUNCTION were
        ;; supplied as well.
        (method-arglist (cdr arglist)))
    (dolist (option options)
      (typecase option
        (string (setq documentation option))
        ((member :inline-methods)) ; ignore
        (cons
         (let ((keyword (car option)))
           (case keyword
             (:compatible-message
              (setq message (second option))
              (check-type message (and symbol (not null))))
             (declare
              (setq declarations (nconc declarations (copy-list (cdr option)))))
             (:documentation
              (setq documentation (cadr option))
              (check-type documentation string))
             ((:dispatch :function)
              (cerror "Ignore the option."
                      "The old flavors version of ~S cannot implement the ~S option."
                      'defgeneric keyword))
             ((:inline-methods :optimize)) ; safe to ignore
             (:method-arglist (setq method-arglist (cdr option)))
             (:method (setq pending-methods (nconc pending-methods (ncons (cdr option)))))
             (:method-combination
              (warn "The old flavors version of ~S cannot implement ~S; use ~S instead."
                    'defgeneric keyword 'defflavor))
             (otherwise
              (error "Unknown option keyword to ~S: ~S" 'defgeneric keyword)))))
        (t (error "Strange object in ~S options: ~S" 'defgeneric option))))
    (setq message (check-compatible-message function message))
    (let ((defmethods (mapcar #'(lambda (spec)
                                  `(defmethod (,(caar spec) ,@(cdar spec) ,message)
                                              ,method-arglist ; See note above
                                     ,@(cdr spec)))
                              pending-methods))
          (hairy-p (arglist-has-lambda-list-keywords-p arglist)))
      (values
        (list* 'progn
               (if hairy-p
                   `(defun ,function (,(car arglist) &rest .args.)
                      ,@(and documentation (list documentation))
                      (declare (zl:arglist ,@arglist) ,@declarations)
                      (zl:lexpr-send ,(car arglist) ',message .args.))
                 `(zl:defsubst ,function ,arglist
                    ,@(and documentation (list documentation))
                    (declare ,@declarations)
                    (zl:send ,(car arglist) ',message ,@(cdr arglist))))
               defmethods)
        message
        (and hairy-p 'generic-inline-send)))))

(defmacro defgeneric (function arglist &rest options)
  "Define a generic FUNCTION with ARGLIST.
ARGLIST should have at least one parameter.

Understood options are:
 a string : the documentation string.
/(:DOCUMENTATION string) works as well.
/(DECLARE declarations), which can be repeated any number of times.
 The declarations should refer to the function as whole.
/(:COMPATIBLE-MESSAGE message), specifying which message implements this
 generic function.  In old Flavors, this is required.
/(:METHOD (flavor . options) body), which defines a method for the flavor
 with body.  This can appear more than once, with different flavors.
/(:METHOD-ARGLIST . arglist), specifying the argument list of the methods
 which implements this generic function.

:INLINE-METHODS and :OPTIMIZE have no effect.  :METHOD-COMBINATION is not
 accepted and must be specified under DEFFLAVOR.
:DISPATCH and :FUNCTION are currently not allowed."
  (check-type function (and symbol (not keyword)))
  (check-type arglist cons)
  (assert (and (symbolp (car arglist))
               (not (lisp:member (car arglist) lambda-list-keywords)))
          ()
          "The first element of the argument list must be an argument")
  (multiple-value-bind (functional-support message optimizer)
      (expand-defgeneric-old-flavors function arglist options)
    `(progn
       (eval-when (compile load eval)
         (set-generic-operation ',function ',message)
         ,@(when optimizer
             `((compiler:defoptimizer optimize-generic-call ,function)))
         (set-optimizer-support ',function ,(and optimizer (list 'quote optimizer))))
       ,functional-support)))
