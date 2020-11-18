;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-
;;; These are the macros in the Lisp Machine system.

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFUN MACRO-TYPE-CHECK-WARNING (MACRO OBJECT)
  (declare (dbg:error-reporter))
  "Detect attempts by macros to check type at compile time of an eval-at-load-time.
A macro should call this function with OBJECT being the subexpression whose
type is to be checked and MACRO being the macro name.  If OBJECT is an
eval-at-load-time, an error happens."
  (IF (EQ (CAR-SAFE OBJECT) COMPILER::EVAL-AT-LOAD-TIME-MARKER)
      (FERROR "The macro ~S is attempting to check the type of an argument
at compile time, but the argument is #,~S,
whose type is not known until load time"
              MACRO (CDR OBJECT))))

;;;; Macros which look at the processor type at run time.

;; use select-processor
(DEFMACRO IF-IN-CADR (&BODY FORMS)
  "Uses FORMS only if running or compiling on a CADR processor."
  `(WHEN (EQ PROCESSOR-TYPE-CODE #.CADR-TYPE-CODE)
    ,@FORMS))
(compiler:make-obsolete if-in-cadr "use SELECT-PROCESSOR")
(DEFMACRO IF-IN-LAMBDA (&BODY FORMS)
  "Uses FORMS only if running or compiling on an LMI Lambda processor."
  `(WHEN (EQ PROCESSOR-TYPE-CODE #.LAMBDA-TYPE-CODE)
    ,@FORMS))
(compiler:make-obsolete if-in-lambda "use SELECT-PROCESSOR")
(DEFMACRO IF-IN-EXPLORER (&BODY FORMS)
  "Uses FORMS only if running or compiling on a TI Explorer processor."
  `(WHEN (EQ PROCESSOR-TYPE-CODE #.EXPLORER-TYPE-CODE)
    ,@FORMS))
(compiler:make-obsolete if-in-explorer "use SELECT-PROCESSOR")

;These are evaluated at read time and compared with CASE instead of SELECT
; to avoid losing in the cold load. Problem is, the setqs for lambda-type-code,
; etc, are on the LISP-CRASH-LIST which doesn't get evaluated soon enough.
(eval-when (eval compile load)
  (defprop :cadr #.cadr-type-code processor-type-symbol)
  (defprop :lambda #.lambda-type-code processor-type-symbol)
  (defprop :explorer #.explorer-type-code processor-type-symbol)
  (defprop :falcon #.falcon-type-code processor-type-symbol)
  ;;dont generate warnings for obsolete types:
  (defconst *all-known-processor-types* '(:lambda :falcon))
  (defconst *all-historical-processor-types* '(:cadr :lambda :explorer :falcon))
)

;;;Save this in case a hacker (me) has screwn up. -Keith
;(defmacro select-processor (&rest clauses)
;  (let* ((processor-types-not-used (copy-list *all-known-processor-types*)))
;    (prog1 `(case processor-type-code
;             ,@(mapcar
;                (lambda (clause &aux types)
;                  (block nil
;                    (cond
;                          ;this is a bad way to lose when bringing up new processors - pace
;                          ;((memq (car clause) '(t otherwise :otherwise))
;                          ; (setq processor-types-not-used nil)
;                          ; (return clause))
;                          ((symbolp (car clause))
;                           (setq types (list (car clause))))
;                          ((not (consp (car clause)))
;                           (compiler:warn 'compiler::bad-argument :impossible
;                                          "~S is a bogus ~S clause"
;                                           clause 'select-processor)
;                           (return nil))
;                          (t
;                           (setq types (car clause))))
;                    (cons
;                      (loop for ptype in types
;                            do (setq processor-types-not-used
;                                     (delq ptype processor-types-not-used))
;                            when (null (get ptype 'processor-type-symbol))
;                              do (compiler:warn 'unknown-processor :impossible
;                                                 "The processor type ~S, in a ~S, is unknown."
;                                                 ptype 'select-processor)
;                            else
;                              collect (get ptype 'processor-type-symbol))
;                      (cdr clause))))
;                clauses))
;          (when processor-types-not-used
;            (compiler:warn 'extra-processors :implausible
;                           "A ~S expression failed to give alternatives for ~S"
;                           'select-processor processor-types-not-used)))))

(defmacro select-processor (&rest clauses)
  "At run-time select an action to perform based on the hardware processor type.
The format is like CASE:  the first element of each clause is a processor type or
 a list of processor type keywords -- :LAMBDA, :FALCON, etc."
  `(case processor-type-code
     . ,(loop for clause in clauses collect
              (flet ((spbarf ()
                      (error "Ill-formed clause in ~S: /"~S/". Format should be as for ~S."
                             'select-processor clause 'case)))
                (if (not (listp clause))
                    (spbarf)
                  (let ((procs (car clause))
                        (stuff (cdr clause)))
                    (cons
                      (typecase procs
                        (symbol (get procs 'processor-type-symbol))
                        (cons (mapcar #'(lambda (p)
                                          (check-type p keyword)
                                          (get p 'processor-type-symbol))
                                      procs))
                        (t (spbarf)))
                      stuff)))))))

(defun (:property select-processor compiler:style-checker) (form)
  (block checking
    (let ((clauses (cdr form))
          processor-types-used)
      (labels ((are-we-done? ()
                (when (>= (length processor-types-used)
                          (length *all-historical-processor-types*))
                  (return-from checking nil)))
               (note-type (type)
                (cond
                  ((lisp:member type '(t otherwise) :test #'string-equal)
                   (return-from checking
                     (compiler:warn
                       'compiler:bad-argument :implausible
                       "Default processor type /"~S/" is not permitted in ~S."
                       type (car form))))
                  ((get type 'processor-type-symbol)
                   (pushnew type processor-types-used)
                   (are-we-done?))
                  (t (return-from checking
                       (compiler:warn
                         'compiler:bad-argument :implausible
                         "Unknown processor type ~S in ~S. ~&   Use one of: ~S"
                         type (car form) *all-known-processor-types*))))))
        (dolist (clause clauses)
          (cond
            ((not (consp clause))
             (compiler:warn 'compiler:bad-argument :implausible
               "Ill-formed clause ~S in ~S. Format should be as for CASE."
               clause (car form)))
            ((symbolp (car clause))
             (note-type (car clause)))
            ((consp (car clause))
             (dolist (type (car clause))
               (note-type type)))))
        (are-we-done?))
      (compiler:warn 'extra-processors :implausible
        "A ~S expression failed to give alternatives for ~S"
        'select-processor
        (set-difference *all-known-processor-types* processor-types-used)))))

(defmacro select-target-processor (&rest clauses)
  (let* ((processor-types-not-used (copy-list *all-known-processor-types*)))
    (prog1
      (let ((processor-symbol
              (compiler:target-processor-symbol)) ;works off compiler:*target-computer*
            (ans nil))  ;LIST of real ans.
        (dolist (c clauses)
          (let ((proc (first c))
                (clause (second c))
                (types nil))
            (cond ((symbolp proc)
                   (setq types (list proc)))
                  ((not (consp proc))
                   (compiler:warn 'compiler::bad-argument :impossible
                                  "~S is a bogus ~s clause"
                                  c 'select-target-processor))
                  (t (setq types proc)))
            (if (memq processor-symbol types)
                (setq ans (list clause)))
            (loop for ptype in types
                  do (setq processor-types-not-used
                           (delq ptype processor-types-not-used))
                  when (null (get ptype 'processor-type-symbol))
                  do (compiler:warn 'unknown-processor :impossible
                                    "The processor type ~S, in a ~S, is unknown."
                                    ptype 'select-target-processor))))
        (if (null ans)
            (compiler:warn 'no-option-for-target-processor :fatal
                           "No alternative was given for ~S, the target processor"
                           processor-symbol)
          (car ans)))
      (when processor-types-not-used
        (compiler:warn 'extra-processors :implausible
                       "A ~S expression failed to give alternatives for ~S"
                       'select-processor processor-types-not-used)))))



;;; The IF-IN-MACLISP/IF-IN-LISPM conditionals have to do with not breaking
;;; the Maclisp environment when compiling.  The optimizers in COMPAT take
;;; over these functions when compiling in Maclisp.

;(DEFMACRO IF-IN-MACLISP (&BODY FORMS)
;  "Use FORMS only if running or compiling in Maclisp.  No-op on the Lisp machine."
;  FORMS
;  NIL)

;(DEFMACRO IF-IN-LISPM (&BODY FORMS)
;  "Use FORMS only if running or compiling on the Lisp machine."
;  `(PROGN . ,FORMS))

;(DEFMACRO IF-FOR-MACLISP (&BODY FORMS)
;  "Use FORMS only if evaluating on or compiling FOR Maclisp.
;Nowadays this is the same as IF-IN-MACLISP since there is no longer
;a cross-compiler in Maclisp for the Lisp machine."
;  FORMS
;  NIL)

;(DEFMACRO IF-FOR-LISPM (&BODY FORMS)
;  "Use FORMS only if evaluating on or compiling FOR the Lisp machine.
;Nowadays this is the same as IF-IN-LISPM since there is no longer
;a cross-compiler in Maclisp for the Lisp machine."
;  `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL))
;     (PROGN . ,FORMS)))

;(DEFMACRO IF-FOR-MACLISP-ELSE-LISPM (MACLISP-FORM LISPM-FORM)
;  "Use MACLISP-FORM only if in Maclisp, use LISPM-FORM if on the Lisp machine."
;  MACLISP-FORM
;  `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL))
;     ,LISPM-FORM))

;; sigh
(DEFMACRO SEND (OBJECT OPERATION &REST ARGUMENTS)
  "Send a message to OBJECT, with operation OPERATION and ARGUMENTS."
  `(FUNCALL ,OBJECT ,OPERATION . ,ARGUMENTS))

;;sigh^2
;;>> this is a macro to get around fuxking bd in the sublis-eval-once crock.
;;>>  I want lunar language tools!!
(DEFMACRO LEXPR-SEND (OBJECT OPERATION &REST ARGS)
  "Send a message to OBJECT, with operation OPERATION and ARGUMENTS.
The last one of ARGUMENTS actually is a list of arguments, not one argument."
  `(APPLY ,OBJECT ,OPERATION . ,ARGS))

(DEFSUBST SEND-IF-HANDLES (OBJECT OPERATION &REST ARGUMENTS)
  "Send the message OPERATION to OBJECT if OBJECT handles that message.
If it does, return the result of sending it that message on the given ARGUMENTS.
Otherwise, return NIL."
  (LEXPR-SEND OBJECT :SEND-IF-HANDLES OPERATION ARGUMENTS))

(DEFSUBST LEXPR-SEND-IF-HANDLES (OBJECT OPERATION &REST ARGUMENTS)
  "Send the message OPERATION to OBJECT if OBJECT handles that message.
If it does, return the result of sending it that message on the given ARGUMENTS.
Otherwise, return NIL."
  ;;>> (apply #'lexpr-send ...) loses due to sublis-eval-once
  (APPLY #'APPLY OBJECT :SEND-IF-HANDLES OPERATION ARGUMENTS))

(DEFSUBST OPERATION-HANDLED-P (OBJECT &REST OPERATION)
  "Non-NIL if OBJECT has a method defined for OPERATION."
  (LEXPR-SEND OBJECT :OPERATION-HANDLED-P OPERATION))


;;; GETF and GET-PROPERTIES are now functions in QRAND -- RpK, 14-Nov-86 15:50:00
(DEFMACRO GET-from-area (PLACE PROPERTY area &OPTIONAL (DEFAULT NIL))
  "Like GET, but takes AREA arg which it ignores.
However, SETF will invert to SETPROP-IN-AREA, which uses the arg."
  (declare (ignore area))
  `(GET ,PLACE ,PROPERTY ,DEFAULT))

(DEFMACRO GETF-from-area (PLACE PROPERTY area &OPTIONAL (DEFAULT NIL))
  "Like GETF, but takes AREA arg which could be used by SETF inversion."
  `(GET-from-area (LOCF ,PLACE) ,PROPERTY ,area ,DEFAULT))

;GET-LOCATION-FROM-AREA has to be a real function because it can do
; consing (ie PUTPROPs).
;(defmacro get-location-from-area (place property area &optional (default nil))
;  "Like GET-LOCATION, but takes AREA arg which could be used by SETF inversion."
;  (declare (ignore area))
;  `(get-location ,place ,property ,default))

(DEFMACRO REMF (PLACE PROPERTY)
  "Removes the PROPERTY property from the plist stored in PLACE.
PLACE should be such that simply evaluating it would return
the contents of the property list."
  `(REMPROP (LOCF ,PLACE) ,PROPERTY))

(DEFMACRO DEFSUBST-WITH-PARENT (FUNCTION-SPEC PARENT LAMBDA-LIST &BODY BODY)
  "Like DEFSUBST but says that the parent of FUNCTION-SPEC is PARENT.
This is for DEFSUBSTs generated by macros, as in DEFSTRUCT.
This tells the editor to find FUNCTION-SPEC's definition in PARENT's definition.
PARENT is either a symbol, or a list of a function spec and a definition type."
  (UNLESS (CONSP PARENT) (SETQ PARENT (LIST PARENT)))
  `(DEFSUBST ,FUNCTION-SPEC ,LAMBDA-LIST
     (DECLARE (FUNCTION-PARENT . , PARENT))
     . ,BODY))


;;;; Macros in Lambda position of function

(DEFMACRO LAMBDA-MACRO (LAMBDA-MACRO-NAME LAMBDA-LIST &BODY BODY)
  (LET ((DEF1 `(NAMED-LAMBDA ,LAMBDA-MACRO-NAME ,LAMBDA-LIST . ,BODY)))
    `(PROGN (EVAL-WHEN (COMPILE)
                       (PUTDECL ',LAMBDA-MACRO-NAME 'LAMBDA-MACRO ',DEF1))
            (DEFUN (:LAMBDA-MACRO ,LAMBDA-MACRO-NAME) . ,(CDDR DEF1)))))

(LAMBDA-MACRO DISPLACED (FCTN) ; moved from qfctns because it doesn't work in cold load.
  (CADDR FCTN))

(DEFMACRO DEFFUNCTION (FUNCTION-SPEC LAMBDA-TYPE LAMBDA-LIST &BODY REST)
  "Define FUNCTION-SPEC as a function, like DEFUN, but use LAMDBA-TYPE instead of 'LAMBDA.
With this you can define a function with a definition that uses a lambda macro.
The lambda macro is expanded and should yield a LAMBDA, SUBST or MACRO;
then this macro expands into a suitable use of DEFUN, DEFSUBST or MACRO."
  (LET ((FUNCTION (LAMBDA-MACRO-EXPAND `(,LAMBDA-TYPE ,LAMBDA-LIST . ,REST))))
    `(,(CASE (CAR FUNCTION)
         ((LAMBDA NAMED-LAMBDA) 'DEFUN)
         ((SUBST NAMED-SUBST CL:SUBST) 'DEFSUBST)
         (MACRO 'MACRO)
         (T (FERROR "~A (a lambda macro) did not expand into a suitable function"
                    LAMBDA-TYPE)))
      ,FUNCTION-SPEC
      . ,(CASE (CAR FUNCTION)
           ((LAMBDA SUBST CL:SUBST)
            (CDR FUNCTION))
           ((NAMED-LAMBDA NAMED-SUBST)
            (CDDR FUNCTION))
           (MACRO (CDDR FUNCTION))))))

(DEFMACRO DEFDECL (NAME PROP VALUE)
  "Declare that the PROP property of NAME is VALUE, for GETDECL.
When executed, this makes a property, like DEFPROP.
In file compilation, this makes a declaration, so that GETDECL
done in macros being expanded will see this property."
  `(PROGN
     (EVAL-WHEN (EVAL LOAD)
       (PUTPROP ',NAME ',VALUE ',PROP))
     (EVAL-WHEN (COMPILE)
       (PUTDECL ',NAME ',PROP ',VALUE))))

(DEFMACRO @DEFINE (&REST IGNORE) NIL)


(DEFSUBST SYMBOL-PACKAGE (SYMBOL)
  "Returns the package which SYMBOL belongs to, or NIL if none."
  (CONTENTS (PACKAGE-CELL-LOCATION SYMBOL)))

(DEFSUBST SYMBOL-PLIST (SYMBOL)
  "Returns the property-list of SYMBOL."
  (CONTENTS (PROPERTY-CELL-LOCATION SYMBOL)))

(DEFSUBST FILL-POINTER (ARRAY)
  "Return the fill pointer of ARRAY."
  (ARRAY-LEADER ARRAY 0))

(DEFSUBST CAAR-SAFE (OBJECT)
  "Like CAAR, but treats all non-lists as NIL rather than getting an error."
  (CAR-SAFE (CAR-SAFE OBJECT)))

(DEFSUBST CDAR-SAFE (OBJECT)
  "Like CDAR, but treats all non-lists as NIL rather than getting an error."
  (CDR-SAFE (CAR-SAFE OBJECT)))

(DEFSUBST CADDR-SAFE (OBJECT)
  "Like CADDR, but treats all non-lists as NIL rather than getting an error."
  (CAR-SAFE (CDDR-SAFE OBJECT)))

(DEFSUBST CADDDR-SAFE (OBJECT)
  "Like CADDDR, but treats all non-lists as NIL rather than getting an error."
  (CADR-SAFE (CDDR-SAFE OBJECT)))

(DEFSUBST CDDDR-SAFE (OBJECT)
  "Like CDDDR, but treats all non-lists as NIL rather than getting an error."
  (CDR-SAFE (CDDR-SAFE OBJECT)))

(DEFSUBST FIRST (LIST)
  "Return the first element of LIST."
  (CAR LIST))

(DEFSUBST SECOND (LIST)
  "Return the second element of LIST."
  (CADR LIST))

(DEFSUBST THIRD (LIST)
  "Return the third element of LIST."
  (CADDR LIST))

(DEFSUBST FOURTH (LIST)
  "Return the fourth element of LIST."
  (CADDDR LIST))

(DEFSUBST FIFTH (LIST)
  "Return the fifth element of LIST."
  (CAR (CDDDDR LIST)))

(DEFSUBST SIXTH (LIST)
  "Return the sixth element of LIST."
  (CADR (CDDDDR LIST)))

(DEFSUBST SEVENTH (LIST)
  "Return the seventh element of LIST."
  (CADDR (CDDDDR LIST)))

(DEFSUBST EIGHTH (LIST)
  "Return the eighth element of LIST."
  (CADDDR (CDDDDR LIST)))

(DEFSUBST NINTH (LIST)
  "Return the ninth element of LIST."
  (CADDR (CDDR (CDDDDR LIST))))

(DEFSUBST TENTH (LIST)
  "Return the tenth element of LIST."
  (CADDR (CDDDR (CDDDDR LIST))))

(DEFSUBST REST (LIST)
  "Return LIST sans its first element."
  (CDR LIST))

(DEFSUBST REST1 (LIST)
  "Return LIST sans its first element."
  (CDR LIST))

(DEFSUBST REST2 (LIST)
  "Return LIST sans its first two elements."
  (CDDR LIST))

(DEFSUBST REST3 (LIST)
  "Return LIST sans its first three elements."
  (CDDDR LIST))

(DEFSUBST REST4 (LIST)
  "Return LIST sans its first four elements."
  (CDDDDR LIST))

(DEFSUBST CONTENTS (LOCATIVE)
  "Return the contents of the cell LOCATIVE points to.
/(CONTENTS (LOCF <expression>)) is equivalent to <expression>."
  (CDR LOCATIVE))
;; Brand S incompatibility.  Sigh.  Only took them 3 years to add this.
(DEFF LOCATION-CONTENTS 'CONTENTS)

(DEFSUBST CONSP (OBJECT)
  "T if OBJECT is a cons (a non-null list)."
  (NOT (ATOM OBJECT)))

(DEFSUBST HASH-TABLE-P (OBJECT)
  "T if OBJECT is a hash table."
  (TYPEP OBJECT 'HASH-TABLE))

(DEFSUBST CL:LISTP (OBJECT)
  "T if OBJECT is a list (either a cons or NIL)."
  (COMMON-LISP-LISTP OBJECT))

;; cli:, not cl:
(DEFSUBST CLI:NLISTP (OBJECT)
  "T if OBJECT is not a list (neither a cons nor NIL)."
  (NOT (COMMON-LISP-LISTP OBJECT)))

(DEFSUBST CL:AREF (ARRAY &REST INDICES)
  "Access an element of ARRAY according to INDICES."
  (APPLY #'COMMON-LISP-AREF ARRAY INDICES))

;; cli:, not cl:
(DEFSUBST CLI:AR-1 (ARRAY INDEX)
  "Access an element of ARRAY, an array of rank 1, according to INDEX."
  (COMMON-LISP-AR-1 ARRAY INDEX))

;; cli:,not cl:
(DEFSUBST CLI:AR-1-FORCE (ARRAY INDEX)
  "Access an element of ARRAY, indexing it by INDEX as if it were one dimensional."
  (COMMON-LISP-AR-1-FORCE ARRAY INDEX))
;; brand s incompatibility
(DEFF %1D-AREF 'ZL:AR-1-FORCE)
(DEFF %1D-ASET 'AS-1-FORCE)
(DEFF %1D-ALOC 'AP-1-FORCE)

(defsubst svref (vector index)
  "Accesses an element of a simple vector.  Actually the same as CL:AREF."
  (common-lisp-aref vector index))

(defsubst bit (bitarray &rest subscripts)
  "Accesses an element of a bit array.  Actually the same as CL:AREF."
  (apply #'common-lisp-aref bitarray subscripts))

(defsubst sbit (bitarray &rest subscripts)
  "Accesses an element of a simple bit array.  Actually the same as CL:AREF."
  (apply #'common-lisp-aref bitarray subscripts))

(defsubst char (string index)
  "Accesses the character at index INDEX in STRING.  Really the same as CL:AREF."
  (COMMON-LISP-AREF STRING INDEX))

(DEFSUBST SCHAR (STRING INDEX)
  "Accesses the character at index INDEX in the simple-string STRING.
Really the same as CL:AREF."
  (COMMON-LISP-AREF STRING INDEX))


;(DEFSUBST FIXP (OBJECT)
;  "T if OBJECT is an integer; NIL for other numbers and non-numbers."
;  (INTEGERP OBJECT))

(DEFSUBST CLOSUREP (X)
  "T if X is a closure."
  (= (%DATA-TYPE X) #.DTP-CLOSURE))

(DEFSUBST ENTITYP (X)
  "T if X is an entity."
  (= (%DATA-TYPE X) #.DTP-ENTITY))

(DEFSUBST RANDOM-STATE-P (OBJECT)
  "T if OBJECT is a random-state -- a seed for use by RANDOM."
  (TYPEP OBJECT 'RANDOM-STATE))

(DEFSUBST READTABLEP (OBJECT)
  "T if OBJECT is a readtable -- a syntax table for READ and PRINT."
  (TYPEP OBJECT 'READTABLE))

;;; PACKAGEP already exists.

(DEFSUBST COMPILED-FUNCTION-P (OBJECT)
  "T if OBJECT is a compiled function (a FEF)."
  (TYPEP OBJECT 'COMPILED-FUNCTION))

(DEFSUBST INSTANCEP (OBJECT)
  "T if OBJECT is an instance (of any flavor)."
  (TYPEP OBJECT 'INSTANCE))

(DEFSUBST KEYWORDP (SYMBOL)
  "T if SYMBOL belongs to the KEYWORD package."
  (AND (SYMBOLP SYMBOL)
       (EQ (SYMBOL-PACKAGE SYMBOL) PKG-KEYWORD-PACKAGE)))

(DEFSUBST SUBRP (OBJECT)
  "T if OBJECT is a compiled or built-in function."
  (MEMQ (%DATA-TYPE OBJECT) '(#.DTP-U-ENTRY #.DTP-FEF-POINTER)))

(DEFSUBST LOCATIVEP (X)
  "T if X is a locative."
  (EQ (%DATA-TYPE X) #.DTP-LOCATIVE))

(DEFSUBST %POINTERP (X)
  "T if X points to storage; NIL if it is an immediate quantity."
  (NOT (MEMQ (%DATA-TYPE X)
        ;Note: other non-pointer data types would be illegal as arg to %pointerp
             '(#.DTP-FIX #.DTP-SMALL-FLONUM #.DTP-U-ENTRY #.DTP-CHARACTER))))

(DEFSUBST %POINTER-TYPE-P (DATA-TYPE-CODE)
  "T if DATA-TYPE-CODE is a the code for a data type that points to storage."
  (NOT (MEMQ DATA-TYPE-CODE
             '(#.DTP-FIX #.DTP-SMALL-FLONUM #.DTP-U-ENTRY #.DTP-CHARACTER
               #.DTP-TRAP #.DTP-NULL #.DTP-SELF-REF-POINTER
   ;note that DTP-INSTANCE-HEADER IS a pointer!.
               #.DTP-HEADER #.DTP-ARRAY-HEADER
   ;note that DTP-HEADER-FORWARD IS a pointer!.
               ))))

(DEFSUBST %P-POINTERP (POINTER)
  "T if the word POINTER points to contains a data type that points to some storage.
This includes various header and forwarding data types
which point to storage."
  (%POINTER-TYPE-P (%P-DATA-TYPE POINTER)))

(DEFSUBST %P-POINTERP-OFFSET (POINTER OFFSET)
  "T if the word POINTER+OFFSET points to contains a data type that points to some storage.
This includes various header and forwarding data types
which point to storage."
  (%POINTER-TYPE-P (%P-LDB-OFFSET %%Q-DATA-TYPE POINTER OFFSET)))

(DEFSUBST %DATA-TYPE-SAFE-P (DATA-TYPE)
  (MEMQ DATA-TYPE
        '(#.DTP-SYMBOL #.DTP-FIX #.DTP-EXTENDED-NUMBER #.DTP-LOCATIVE #.DTP-LIST
          #.DTP-U-ENTRY #.DTP-FEF-POINTER #.DTP-ARRAY-POINTER
          #.DTP-STACK-GROUP #.DTP-CLOSURE #.DTP-SMALL-FLONUM #.DTP-SELECT-METHOD
          #.DTP-INSTANCE #.DTP-ENTITY #.DTP-CHARACTER)))

(DEFSUBST %P-CONTENTS-SAFE-P (POINTER)
  "T if the word POINTER points to contains data safe to read out.
It will be NIL if the word contains a forwarding pointer or a header."
  (%DATA-TYPE-SAFE-P (%P-DATA-TYPE POINTER)))

(DEFSUBST %P-CONTENTS-SAFE-P-OFFSET (POINTER OFFSET)
  "T if the word POINTER+OFFSET points to contains data safe to read out.
It will be NIL if the word contains a forwarding pointer or a header."
  (%DATA-TYPE-SAFE-P (%P-LDB-OFFSET %%Q-DATA-TYPE POINTER OFFSET)))

(DEFUN %P-SAFE-CONTENTS-OFFSET (POINTER OFFSET)
  "Extract the contents of a word (which contains typed data) in a way that is always safe.
The word is OFFSET words after where POINTER points.
If the contents are a valid Lisp data type, they are returned accurately.
If the contents point to storage but are not valid Lisp data
 (such as, forwarding pointers and symbol and instance headers)
 then a locative is returned.
If the contents do not point to storage and are not valid Lisp data
 (such as, a self-ref-pointer or an array header) then a fixnum is returned.
The second value is T if the accurate contents were returned."
  (DECLARE (VALUES SAFE-CONTENTS RETURNED-VALUE-ACCURATE-P))
  (IF (%P-CONTENTS-SAFE-P-OFFSET POINTER OFFSET)
      (VALUES (%P-CONTENTS-OFFSET POINTER OFFSET) T)
    (IF (%P-POINTERP-OFFSET POINTER OFFSET)
        (%P-CONTENTS-AS-LOCATIVE-OFFSET POINTER OFFSET)
      (%P-LDB-OFFSET %%Q-POINTER POINTER OFFSET))))

(DEFSUBST %POINTER-PLUS (PTR DISP)
  "Return a fixnum which represents a pointer DISP words past PTR.
The arguments had better be locatives into the same object
for this operation to be meaningful;
otherwise, their relative position will be changed by GC."
  (%MAKE-POINTER-OFFSET DTP-FIX PTR DISP))

(DEFSUBST %POINTER-LESSP (PTR1 PTR2)
  "T if PTR1 points to a lower memory address than PTR2"
  (MINUSP (%POINTER-DIFFERENCE PTR1 PTR2)))


(DEFSUBST NEQ (X Y)
  "T if X and Y are not the same object."
  (NOT (EQ X Y)))

;; actually microcoded itself
;(DEFSUBST ENDP (X)
;  "T if X as the cdr of a list terminates it."
;  (ATOM X))

(DEFSUBST BIT-TEST (BITS WORD)
  "T if the bits specified by BITS in WORD are not all zero.
BITS is a mask in which the bits to be tested are ones."
  (NOT (ZEROP (LOGAND BITS WORD))))
(DEFF LOGTEST 'BIT-TEST)

(DEFSUBST LDB-TEST (PPSS WORD)
  "T if the field specified by PPSS in WORD is not zero.
PPSS is a position (from the right) times 64., plus a size."
  (NOT (ZEROP (LDB PPSS WORD))))

(DEFSUBST %LOGLDB-TEST (PPSS WORD)
  "T if the field specified by PPSS in WORD is not zero.
PPSS is a position (from the right) times 64., plus a size.
Like LDB-TEST except that when SETF'd it does a %LOGDPB rather than a DPB."
  (NOT (ZEROP (%LOGLDB PPSS WORD))))

(DEFSUBST BYTE (WIDTH POSITION)
  "Return a byte specifier for a byte WIDTH bits long starting POSITION bits from the lsb."
  (+ WIDTH (DPB POSITION %%BYTE-SPECIFIER-POSITION 0)))

(DEFSUBST BYTE-POSITION (BYTE-SPECIFIER)
  "Return the byte position specified by BYTE-SPECIFIER.
This is the index of the least significant bit included in the byte."
  (LDB %%BYTE-SPECIFIER-POSITION BYTE-SPECIFIER))

(DEFSUBST BYTE-SIZE (BYTE-SPECIFIER)
  "Return the byte size specified by BYTE-SPECIFIER."
  (LDB %%BYTE-SPECIFIER-SIZE BYTE-SPECIFIER))

(DEFSUBST LOGBITP (INDEX INTEGER)
  "T if INTEGER's binary representation contains the 2^INDEX bit as a 1."
  (LDB-TEST (BYTE 1 INDEX) INTEGER))

(DEFSUBST SHORT-FLOAT (NUMBER)
  "Convert NUMBER to a short float."
  (SMALL-FLOAT NUMBER))

(DEFMACRO CATCH-CONTINUATION (TAG-EXPRESSION
                              THROW-CONTINUATION NON-THROW-CONTINUATION
                              &BODY BODY)
  "Execute BODY with a catch for TAG-EXPRESSION, then call one continuation or the other.
If BODY THROWs to the tag, THROW-CONTINUATION is called with the values thrown as args,
 and the its values are returned.
 However, if THROW-CONTINUATION is NIL in the source code,
 the CATCH's values are returned directly.
If BODY returns normally, NON-THROW-CONTINUATION is called and its values returned,
 with the values of BODY as arguments.
 However, if NON-THROW-CONTINUATION is NIL in the source code,
 BODY's values are returned directly."
  (DECLARE (ZWEI:INDENTATION 1 3 3 1))
  `(CATCH-CONTINUATION-IF T ,TAG-EXPRESSION ,THROW-CONTINUATION ,NON-THROW-CONTINUATION
     . ,BODY))

(defmacro catch-continuation-if (cond-form tag-expression
                                 throw-continuation non-throw-continuation
                                 &body body &aux (tag (gensym)))
  "Like CATCH-CONTINUATION but catch only if COND-FORM evals non-NIL."
  (declare (zwei:indentation 2 3 4 1))
  ;;>> unfortunately, multiple-value-call is a real dog and conses. Foo.
  ;;>> Fixed in the new instruction set (I hope)
  (if non-throw-continuation
      `(block ,tag
         ,(if throw-continuation
              `(multiple-value-call ,throw-continuation
                 (catch (if ,cond-form ,tag-expression
                          '|If you throw to this tag, you deserve to lose.|)
                   (return-from ,tag
                     (multiple-value-call ,non-throw-continuation
                       (progn ,@body)))))
            `(catch (if ,cond-form ,tag-expression
                      '|If you throw to this tag, you deserve to lose.|)
               (return-from ,tag
                 (multiple-value-call ,non-throw-continuation
                   (progn ,@body))))))
    `(block ,tag
       ,(if throw-continuation
            `(multiple-value-call ,throw-continuation
               (catch (if ,cond-form ,tag-expression
                        '|If you throw to this tag, you deserve to lose.|)
                 (return-from ,tag (progn . ,body))))
          `(catch (if ,cond-form ,tag-expression
                    '|If you throw to this tag, you deserve to lose.|)
             (return-from ,tag (progn . ,body)))))))

;;>> barf
(DEFMACRO CATCH-ALL (&BODY BODY)
  "Catch all throws to all tags within the body."
  `(CATCH NIL (VALUES-LIST (APPEND (MULTIPLE-VALUE-LIST (PROGN . ,BODY)) '(NIL NIL NIL)))))


;; condition-case, condition-case-if, condition-call, condition-call-if,
;; condition-bind, condition-bind-if, condition-bind-default, condition-bind-default-if,
;; errset, err, ignore-errors, error-restart, error-restart-loop, catch-error-restart,
;; catch-error-restart-if, catch-error-restart-explicit-if,
;; condition-resume, condition-resume-if, signal-proceed-case, catch-error
;;  moved to debugger; errmac

;;; Flow-of-control macros
(defmacro cli:if (test then &optional else)
  "Execute THEN if TEST comes out non-NIL; otherwise, execute the ELSE."
  (declare (zwei:indentation 2 1))
  `(cond (,test ,then)
         (t ,else)))

(DEFUN DEAD-CLAUSES-WARNING (COND-CLAUSES FUNCTION-NAME)
  "Given a list of COND-clauses, warn if any but the last starts with T.
FUNCTION-NAME (usually a macro name) is used in the warning.
The warning is made iff we are now accumulating warnings for an object."
  (DO ((CLAUSES COND-CLAUSES (CDR CLAUSES)))
      ((NULL (CDR CLAUSES)))
    (AND (EQ (CAAR CLAUSES) T)
         OBJECT-WARNINGS-OBJECT-NAME
         (RETURN
           (COMPILER:WARN 'COMPILER::DEAD-CODE :IMPLAUSIBLE
             "Unreachable clauses following otherwise-clause in ~S." FUNCTION-NAME)))))

(defun expand-case-macro (macro-name place clauses clause-function &optional last)
  (declare (values test-exp let clauses))
  ;; If place is an eval-at-load-time,
  ;; we will treat it as a random expression, which is right.
  (flet ((hack (test-exp)
           (setq clauses
                 (loop for x in clauses
                    do (macro-type-check-warning macro-name (car x))
                    collect `(,(funcall clause-function test-exp (car x))
                              nil . ,(cdr x))))
           (when last
             (setq clauses `(,@clauses (t ,(funcall last test-exp)))))
           (dead-clauses-warning clauses macro-name)
           clauses))
      (cond ((or (atom place)
                 (and (memq (car place) '(car cdr caar cadr cdar cddr))
                      (atom (cadr place))))
             `(cond . ,(hack place)))
            ((and (null (cdr clauses))
                  (not last))
             ;; one clause case.
             `(cond . ,(hack place)))
            (t
             `(let ((.selectq.item. ,place))
                (cond . ,(hack '.selectq.item.)))))))

(defmacro case (&whole whole place &body clauses)
  "Execute the first clause that matches PLACE.
The first element of each clause is a match value or a list of match values.
PLACE is compared with the match values using EQL.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the CASE.
T or OTHERWISE as the first element of a clause matches any test object."
  (declare (zwei:indentation 1 1))
  (expand-case-macro (car whole) place clauses
                     (lambda (test-exp c)
                       (cond ((memq c '(otherwise t))
                              `t)
                             ((and (eq c ':otherwise)
                                   ;; Old zetalisp cases
                                   ;;  >> Do not do this for the commonlisp CASE!!
                                   (memq (car whole) '(selectq caseq)))
                              (compiler:warn 'foo :obsolete
                                             "~S case used with a ~S clause; use ~S"
                                             (car whole) ':otherwise 'otherwise)
                              `t)
                             ((atom c)
                              `(eql ,test-exp ',c))
                             (t
                              `(member-eql ,test-exp ',c))))))
(deff-macro selectq 'case)
(deff-macro caseq 'case)

(defmacro select-memq (test-list &body clauses)
  "Execute the first clause that matches some element of TEST-LIST.
The first element of each clause is a match value or a list of match values.
Each match value is compare with each element of TEST-LIST, using EQ.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the SELECT-MEMQ.
T or OTHERWISE as the first element of a clause matches any test object."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'select-memq test-list clauses
                     (lambda (test-exp c)
                       (cond ((memq c '(otherwise t))
                              `t)
                             ((eq c ':otherwise)
                              (compiler:warn 'foo :obsolete
                                             "~S case used with a ~S clause; use ~S"
                                             'select-memq ':otherwise 'otherwise)
                              `t)
                             ((atom c)
                              `(memq ',c ,test-exp))
                             (t
                              `(or . ,(mapcar (lambda (match-value)
                                                `(memq ',match-value ,test-exp))
                                              c)))))))

(defmacro select (place &body clauses)
  "Execute the first clause that matches PLACE.
The first element of each clause is a match value expression or a list of such.
PLACE is compared with the VALUES of the match expressions, using EQL.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the SELECT.
T or OTHERWISE as the first element of a clause matches any test object.
This is a special exception, in that OTHERWISE is not evaluated."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'select place clauses
                     (lambda (test-exp c)
                       (cond ((memq c '(otherwise t))
                              `t)
                             ((eq c ':otherwise)
                              (compiler:warn 'foo :obsolete
                                             "~S case used with a ~S clause~% use ~S"
                                             'select ':otherwise 'otherwise)
                              `t)
                             ((atom c)
                              `(eql ,test-exp ,c))
                             (t
                              `(or . ,(mapcar (lambda (form) `(eql ,test-exp ,form))
                                              c)))))))

(defmacro selector (place test-function &body clauses)
  "Execute the first clause that matches PLACE.
The first element of each clause is a match value expression or a list of such.
PLACE is compared with the VALUES of the match expressions, using TEST-FUNCTION.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the SELECTOR.
T or OTHERWISE as the first element of a clause matches any test object.
This is a special exception, in that OTHERWISE is not evaluated."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'selector place clauses
                     (lambda (test-exp c)
                       (cond ((memq c '(otherwise t))
                              `t)
                             ((eq c ':otherwise)
                              (compiler:warn 'foo :obsolete
                                             "~S case used with a ~S clause; use ~S"
                                             'selector ':otherwise 'otherwise)
                              `t)
                             ((atom c)
                              `(,test-function ,test-exp ,c))
                             (t
                              `(or . ,(mapcar (lambda (form) `(,test-function ,test-exp ,form))
                                              c)))))))

;;; Will be used to flag cases of people using the old Zetalisp keyword message
;;; protocol.

(defmacro messagecase (place &rest clauses)
  "Like CASE, but the keys are assumed to be message names."
  ;; No transformations for now
  (list* 'case place clauses))

(make-obsolete messagecase "actually, it's just CASE, and indicates old ZetaLISP keyword message protocol")

(DEFMACRO DISPATCH (PPSS WORD &BODY CLAUSES)
  "Extract the byte PPSS from WORD and execute a clause selected by the value.
The first element of each clause is a value to compare with the byte value,
or a list of byte values.  These byte values are evaluated!.
T or OTHERWISE as the first element of a clause matches any test object.
This is a special exception, in that OTHERWISE is not evaluated."
  (declare (zwei:indentation 1 1 2 1))
  (LET ((FOO (GENSYM)))
    `(LET ((,FOO (LDB ,PPSS ,WORD)))
       (COND ,@(MAPCAR (LAMBDA (CLAUSE)
                         (MACRO-TYPE-CHECK-WARNING 'DISPATCH (CAR CLAUSE))
                         `(,(COND ((MEMQ (CAR CLAUSE) '(OTHERWISE :OTHERWISE T))
                                   'T)
                                  ((ATOM (CAR CLAUSE))
                                   `(= ,FOO ,(CAR CLAUSE)))
                                  (T
                                     `(OR ,@(MAPCAR (LAMBDA (ITEM)
                                                      `(= ,FOO ,ITEM))
                                                    (CAR CLAUSE)))))
                             NIL . ,(CDR CLAUSE)))
                       CLAUSES)))))


;;;; TYPECASE, ETYPECASE, CTYPECASE, ECASE, CCASE

;;; CHECK-ARG, CHECK-TYPE, ASSERT moved to DEBUGGER;ERRMAC

;; This really should try to be a lot smarter...

(defmacro typecase (place &body clauses)
  "Execute the first clause whose type specifier PLACE fits.
/
The first element of each clause is a type specifier.
  It is used as the second argument to TYPEP to test the type of PLACE.
  It may be a type symbol, or a list such as /(OR NUMBER SYMBOL/).
If the type comparison is satisfied, the rest of that clause
  is executed, and the values of the last form in it are the
  values returned from the TYPECASE form.
If no clause fits, the value of the TYPECASE is NIL.
/
For example:
/
  /(let ((thing (read instream)))
     (typecase thing
       (number (princ 'a-number))
       ((member :special :better) (princ 'special))
       (symbol (princ 'random-symbol))
       (t (princ 'something-else))))"
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'typecase place clauses
                     (lambda (test-exp c)
                       (if (memq c '(otherwise t))
                           `t
                         `(typep ,test-exp ',c)))))

(defun no-case-error (proceedable function place value typespec)
  (declare (dbg:error-reporter))
  (let ((cond (make-condition 'si::no-case-found
                              :function function
                              :place place :value value :type-specifier typespec)))
    (if proceedable
        (signal-proceed-case ((val) cond)
          (:new-value val))
      (signal cond :proceed-types ()))))

(defmacro etypecase (place &body clauses)
  "Execute the first clause whose type specifier PLACE fits.
/
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of PLACE.
If the result is T, the rest of that clause is excuted and the values
  of the last form in it are the values of the ETYPECASE form.
If no clause fits, an uncorrectable error is signaled."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'etypecase place clauses
                     (lambda (test-exp c)
                       (if (memq c '(otherwise t))
                           `t
                         `(typep ,test-exp ',c)))
                     (lambda (test-exp)
                       `(no-case-error nil 'etypecase ',place ,test-exp
                                       '(or . ,(mapcar #'car clauses))))))

(defmacro ctypecase (place &body clauses)
  "Execute the first clause whose type specifier PLACE fits.
/
The first element of each clause is a type specifier.
  It is used as the second argument to TYPEP to test the type of PLACE.
If the result is T, the rest of that clause is executed and the values
  of the last form in it are the values of the TYPECASE form.
If no clause fits, a correctable error is signalled."
  (declare (zwei:indentation 1 1))
  (let ((tag (gensym)))
    `(block ,tag
       (tagbody
        ,tag
           (return-from ,tag
             ,(expand-case-macro 'ctypecase place clauses
                                 (lambda (test-exp c)
                                   (if (memq c '(otherwise t))
                                       `t
                                     `(typep ,test-exp ',c)))
                                 (lambda (test-exp)
                                   `(progn
                                      (setf ,place
                                            (no-case-error t 'ctypecase ',place ,test-exp
                                                           '(or . ,(mapcar #'car clauses))))
                                      (go ,tag)))))))))

(defmacro ecase (place &body clauses)
  "Execute the first clause that matches PLACE.
The first element of each clause is a match value or a list of match values.
/
PLACE is compared with the match values using EQL.
When a match-value matches, the rest of that clause is executed
  and the value of the last thing in the clause is the value of the ECASE.
If no clause matches, an uncorrectable error is signaled."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'ecase place clauses
                     (lambda (test-exp c)
                       (cond ((memq c '(otherwise t))
                              `t)
                             ((atom c)
                              `(eql ,test-exp ',c))
                             (t
                              `(member-eql ,test-exp ',c))))
                     (lambda (test-exp)
                       `(no-case-error nil 'ecase ',place ,test-exp
                                       '(cl:member
                                          . ,(mapcan (lambda (clause)
                                                       (let ((match (car clause)))
                                                         (if (not (consp match))
                                                             (list match)
                                                           (copy-list match))))
                                                     clauses))))))

(defmacro ccase (place &body clauses)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQL.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the ECASE.
If no clause matches, an uncorrectable error is signaled."
  (declare (zwei:indentation 1 1))
  (let ((tag (gensym)))
    `(block ,tag
       (tagbody
        ,tag
           (return-from ,tag
             ,(expand-case-macro 'ccase place clauses
                                 (lambda (test-exp c)
                                   (cond ((memq c '(otherwise t))
                                          `t)
                                         ((atom c)
                                          `(eql ,test-exp ',c))
                                         (t
                                          `(member-eql ,test-exp ',c))))
                                 (lambda (test-exp)
                                   `(progn
                                      (setf ,place
                                            (no-case-error t 'ccase ',place ,test-exp
                                              '(cl:member
                                                 . ,(mapcan (lambda (clause)
                                                              (let ((match (car clause)))
                                                                (if (not (consp match))
                                                                    (list match)
                                                                  (copy-list match))))
                                                            clauses))))
                                      (go ,tag)))))))))

(defun let-globally-expander (name cond-form letf place-list body)
  (loop for v in place-list
        with condvar = (if (eq cond-form 't) 't (gensym))
        with place and val
     do (cond (letf
               (unless (list-match-p v `(,place ,val))
                 (ferror "~S is not a list of a place and value for ~S" v name)))
              ((symbolp v) (setq place v val nil))
              ((and (list-match-p v `(,place ,val)) (symbolp place)))
              (t (ferror "~S is not a list of a symbol and value for ~S" v name)))
     collect place into places
     collect val into vals
     collect (gensym) into genvars
     finally
       (return
         `(let ,(if (eq cond-form 't) `,genvars `((,condvar ,cond-form) . ,genvars))
            (unwind-protect
                (progn
                  (when ,condvar
                    (without-interrupts
                      . ,(loop for genvar in genvars
                               for place in places
                            collect ;;>> The below generates a voluminous amount of code!
                                    ;;>> (%blt-typed (locf ,genvar) (locf ,place) 1 1)
                                    ;;>>  is nearly the thing we want here,
                                    ;;>>  except that it copies the cdr code.  Foo.
                                    `(%p-store-data-type (locf ,genvar)
                                                         (%p-data-type (locf ,place)))
                            collect `(%p-store-pointer (locf ,genvar)
                                                       (%p-pointer (locf ,place)))))
                    (setf . ,(mapcan #'list places vals)))
                  . ,body)
              (when ,condvar
                (without-interrupts
                  . ,(loop for genvar in genvars
                           for place in places
                        collect `(%p-store-data-type (locf ,place)
                                                     (%p-data-type (locf ,genvar)))
                        collect `(%p-store-pointer (locf ,place)
                                                   (%p-pointer (locf ,genvar)))))))))))

(defmacro let-globally (varlist &body body)
  "Like LET, but sets the variables on entry and sets them back on exit.
No new binding is created.  As a result, the changed values are visible
  in other stack groups while this frame is dynamically active.
Note that it is only really useful to use this on special variables,
  although it works for lexical variables (for which it is just the same as LET)
Note also that the /"globally/" in the name of this macro is only vaguely related
  to /"globally/" as in the function SET-GLOBALLY.
 /"Globally/" in this context means /"don't bind, but set instead,/" which means
  that it we have already bound one of the variables in VARLIST, that binding,
  rather than the global value, is affected.
 SET-GLOBALLY and friends, on the other hand, always affect the global value of
  a location (i.e. the value it has outside of any binding made by any process)"
  (let-globally-expander 'let-globally t nil varlist body))

(defmacro let-globally-if (cond-form varlist &body body)
  "Like LET-IF, but sets the variables on entry and sets them back on exit.
No new binding is created.  As a result, the changed values are visible
  in other stack groups while this frame is dynamically active.
For more information, see the documentation of LET-GLOBALLY"
  (let-globally-expander 'let-globally-if cond-form nil varlist body))

;;>> Is is useful to have these advertised names?
(defmacro letf-globally (place-list &body body)
  "Like LET-GLOBALLY, except that variables may in fact be any SETFable form."
  (let-globally-expander 'letf-globally t t place-list body))

(defmacro letf-globally-if (cond-form place-list &body body)
  "Like LET-GLOBALLY-IF, except that variables may in fact be any SETFable form."
  (let-globally-expander 'letf-globally-if cond-form t place-list body))

;; used to be used by let-globally.  I don't think any body else uses it.
(DEFSUBST COPY-VALUE (TO-CELL FROM-CELL)
  "Copy whatever value is in FROM-CELL into TO-CELL."
  ;; This used to used %BLT-TYPED, which had the additional feature of copying
  ;; the cdr-code from from-cell to to-cell, causing consternation and pdl-lossage
  ;; in several cases.  Note also that this code works even if from-cell contains
  ;; :UNBOUND.
  (WITHOUT-INTERRUPTS
    (%p-store-tag-and-pointer to-cell
                              (%p-ldb %%q-all-but-pointer from-cell) (%p-pointer from-cell))
    ;; Danger! Danger!! Warning, Will Robinson!
    ;;>> Actually, this is OK, since %p-store-* do a gc-write-test.
    ;(%P-STORE-DATA-TYPE TO-CELL (%P-DATA-TYPE FROM-CELL))
    ;(%P-STORE-POINTER TO-CELL (%P-POINTER FROM-CELL))
    ))

;;; DEFUNP is like DEFUN but provides an implicit PROG.
;;; However, the value on falling off the end is the last thing in the body.

(DEFMACRO DEFUNP (&ENVIRONMENT ENV FUNCTION-SPEC LAMBDA-LIST &REST BODY)
  "Like DEFUN, but provides an implicit PROG of no variables around the BODY.
So you can use RETURN to return from the function, and use GO.
There is one difference from an ordinary PROG:
the value of the last element of the BODY is returned.
This is so even if it is an atom.  This is like ordinary DEFUNs.

The use of this function is not recommended."
  (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)
        (LAST NIL))
    (SETQ BODY (COPY-LIST BODY))
    (SETQ LAST (LAST BODY))
    (MULTIPLE-VALUE-BIND (BODY DECLARES DOC)
        (EXTRACT-DECLARATIONS BODY NIL T ENV)
      (WHEN (NEQ (CAAR-SAFE LAST) 'RETURN)
        (SETF (CAR LAST) `(RETURN ,(CAR LAST))))
      `(DEFUN ,FUNCTION-SPEC ,LAMBDA-LIST
         ,DOC
         (DECLARE . ,DECLARES)
         (PROG () . ,BODY)))))

(DEFMACRO WHEN (PRED &BODY BODY)
  "(WHEN pred form1 from2 ...) ==> (AND pred (PROGN form1 form2 ...))
WHEN first evaluates PRED.  If the result is NIL, WHEN returns NIL.
Otherwise, the BODY is executed and its last expression's value returned."
  `(AND ,PRED (PROGN ,@BODY)))

(DEFMACRO UNLESS (PRED &BODY BODY)
  "(UNLESS pred form1 form2 ...) ==> (IF pred () (progn form1 form2 ...))
UNLESS first evaluates PRED.  If the result is non-NIL., UNLESS returns NIL.
Otherwise, the BODY executed and its last expression's value is returned."
  `(CLI:IF ,PRED () (progn ,@BODY)))


(DEFMACRO PSETQ (&REST REST)
  "Like SETQ, but no variable value is changed until all the values are computed.
The returned value is NIL."
  ;; To improve the efficiency of do-stepping, by using the SETE-CDR, SETE-CDDR,
  ;; SETE-1+, and SETE-1- instructions, we try to do such operations with SETQ
  ;; rather than PSETQ.  To avoid having to do full code analysis, never rearrange
  ;; the order of any code when doing this, and only do it when there are no
  ;; variable name duplications.
  (let ((len (length rest)))
    ;;We let SETQ handle all the weird cases but this, which
    ;;avoids expanding to and crapping out on a SETQ of NIL:
    (when (oddp len)
      (ferror "~S appears with an odd number of arguments; the last one is ~S"
              'psetq (car (last rest)))))
  (LOOP FOR (VAL VAR) ON (REVERSE REST) BY 'CDDR
        WITH SETQS = NIL WITH PSETQS = NIL
     DO (UNLESS (EQ VAR VAL)
          (IF (AND (NULL PSETQS)
                   (OR (AND (CONSP VAL)
                            (MEMQ (CAR VAL) '(1+ 1- CDR CDDR))
                            (EQ (CADR VAL) VAR))
                       (EQ VAR VAL))
                   (NOT (MEMQ VAR SETQS)))
              (SETQ SETQS (CONS VAR (CONS VAL SETQS)))
            (SETQ PSETQS (CONS VAR (CONS VAL PSETQS)))))
     FINALLY
        (LABELS ((PROG1IFY (X)
                   (COND ((NULL X) NIL)
                         ((NULL (CDDR X)) (CONS 'SETQ X))
                         (T `(SETQ ,(CAR X) (PROG1 ,(CADR X) ,(PROG1IFY (CDDR X))))))))
          (SETQ PSETQS (PROG1IFY PSETQS))
          (RETURN (COND ((NULL SETQS)
                         `(progn ,PSETQS nil))
                        ((NULL PSETQS)
                         `(progn (SETQ ,@SETQS) nil))
                        (T
                         `(PROGN ,PSETQS (SETQ ,@SETQS) nil)))))))

;;; (LOCAL-DECLARE ((SPECIAL FOO) (UNSPECIAL BAR)) code)
;;; declares FOO and BAR locally within <code>.
;;; LOCAL-DECLARE can also be used by macros to pass information down
;;; to other macros that expand inside the code they produce.
;;; The list of declarations (in this case, ((MUMBLE FOO BAR))) is appended
;;; onto the front of LOCAL-DECLARATIONS, which can be searched by
;;; macros expending inside of <code>.
(DEFMACRO LOCAL-DECLARE (DECLARATIONS &BODY BODY)
  "Evaluates or compiles BODY with DECLARATIONS in effect.
DECLARATIONS is a list of declarations, each of which is a list.
Declarations include /(SPECIAL variables...), /(UNSPECIAL argument-names...),
/(NOTINLINE function-names...), /(:SELF-FLAVOR flavorname)."
  `(COMPILER-LET ((LOCAL-DECLARATIONS (APPEND ',DECLARATIONS LOCAL-DECLARATIONS)))
     . ,BODY))

(DEFMACRO INHIBIT-STYLE-WARNINGS (BODY)
  "Inhibit style warnings from compilation of BODY."
  BODY)


(defmacro let-closed (vars-and-vals &body body)
  "Binds VARS-AND-VALS like LAMBDA, except that all bindings made are special,
then returns a dynamic closure of the value of BODY over those variables."
  (let ((varnames (mapcar (lambda (v) (if (atom v) v (car v))) vars-and-vals)))
    ;; Must use this lambda, rather than let, so that the special declaration doesn't
    ;;  end up affecting the init forms.  Eg. (lambda (x) (let-closed ((x x)) (lambda () x)))
    `((lambda ,varnames
        (declare (special . ,varnames))
        (closure ',varnames (progn . ,body)))
      . ,(mapcar (lambda (v) (if (atom v) nil (cadr v))) vars-and-vals))))


;;;; DEFVAR, DEFPARAMETER, DEFCONSTANT

;;; Make a variable special and, optionally, initialize it.
;;; This is recorded as a definition by ZWEI.
(DEFMACRO DEFVAR (VARIABLE . ARGS)
  "Define a special variable named VARIABLE, and initialize to INITIAL-VALUE if unbound.
Normally, reevaluating the DEFVAR does not change the variable's value.
But in patch files, and if you do C-Shift-E with no region on a DEFVAR,
the variable is reinitialized.  DOCUMENTATION is available if the user
asks for the documentation of the symbol VARIABLE.
If you want your variable to be initially unbound, yet have documentation,
use :UNBOUND as the initial value."
  (DECLARE (ARGLIST VARIABLE &OPTIONAL INITIAL-VALUE DOCUMENTATION))
  `(PROGN (EVAL-WHEN (COMPILE)
            (PROCLAIM '(SPECIAL ,VARIABLE)))
          (EVAL-WHEN (LOAD EVAL)
            (DEFVAR-1 ,VARIABLE . ,ARGS))))

(defmacro defvar-resettable (variable initial-value &optional warm-boot-value documentation)
  "Define a special variable named VARIABLE, and initialize to INITIAL-VALUE if unbound.
Normally, reevaluating the DEFVAR-RESETTABLE does not change the variable's value.
But in patch files, and if you do C-Shift-E with no region on a DEFVAR-RESETTABLE,
the variable is reinitialized.  DOCUMENTATION is available if the user
asks for the documentation of the symbol VARIABLE.
If you want your variable to be initially unbound, yet have documentation,
use :UNBOUND as the initial value."
  (declare (zwei:indentation 3 1))
  `(progn (eval-when (compile)
            (proclaim '(special ,variable)))
          (eval-when (load eval)
            (defvar-resettable-1 ',variable ,initial-value ,warm-boot-value ,documentation))))

(DEFMACRO DEFPARAMETER (VARIABLE INITIAL-VALUE . ARGS)
  "Define a special variable which the program won't change but the user may.
It is set unconditionally to the value of INITIAL-VALUE.
DOCUMENTATION is available if the user asks for the documentation of the symbol VARIABLE."
  (DECLARE (ARGLIST VARIABLE INITIAL-VALUE &OPTIONAL DOCUMENTATION))
  `(PROGN (EVAL-WHEN (COMPILE)
            (PROCLAIM '(SPECIAL ,VARIABLE)))
          (EVAL-WHEN (LOAD EVAL)
            (DEFCONST-1 ,VARIABLE ,INITIAL-VALUE . ,ARGS))))
(DEFF-MACRO DEFCONST 'DEFPARAMETER)

(DEFMACRO DEFCONSTANT (VARIABLE INITIAL-VALUE . ARGS)
  "Define a special variable which will never be changed, and the compiler may assume so.
It is set unconditionally to the value of INITIAL-VALUE.
DOCUMENTATION is available if the user asks for the documentation of the symbol VARIABLE."
  (DECLARE (ARGLIST VARIABLE INITIAL-VALUE &OPTIONAL DOCUMENTATION))
  `(PROGN (EVAL-WHEN (COMPILE)
            (PROCLAIM '(SPECIAL ,VARIABLE))
            ;; The following added 22aug88 by smh.
            ;; The CATCH-ERROR checks that the value form evaluates without error.
            ;; We allow this without comment, although arguably if the value form *won't*
            ;; evaluate, the user probably wants to find out about it because he has
            ;; loaded files in the wrong order, or some such lossage.
            ;; There is potential lossage if the compile-time value is something that
            ;; can't be fasdumped, like a stream or package.
            (catch-error
              (putdecl ',variable 'compiler:constant-for-compilation (list ,initial-value))
              nil))
          (EVAL-WHEN (LOAD EVAL)
            (DEFPROP ,VARIABLE T SYSTEM-CONSTANT))
          (EVAL-WHEN (LOAD EVAL)
            (DEFCONST-1 ,VARIABLE ,INITIAL-VALUE . ,ARGS))))


(DEFMACRO DOLIST ((VAR LIST &OPTIONAL RESULTFORM) &BODY BODY)
  "Iterate BODY with VAR bound to successive elements of the value of LIST.
If LIST is exhausted, RESULTFORM is executed and returned.
RETURN and GO can be used inside the BODY."
  (LET ((ITERATION-VAR (GENSYM)))
    `(DO ((,ITERATION-VAR ,LIST (CDR ,ITERATION-VAR))
          (,VAR ))
         ((NULL ,ITERATION-VAR) ,RESULTFORM)
       (SETQ ,VAR (CAR ,ITERATION-VAR))
       . ,BODY)))

(defmacro dosequence ((var sequence &OPTIONAL RESULTFORM) &BODY BODY)
  "Iterate BODY with VAR bound to successive ELT's of SEQUENCE"
  ;; not common lisp, but useful like NIL's DOVECTOR
  (LET ((ITERATION-VAR (GENTEMP "VAR"))
        (LENGTH-VAR (GENTEMP "LENGTH"))
        (SEQUENCE-VAR (GENTEMP "SEQUENCE")))
    `(LET ((,SEQUENCE-VAR ,SEQUENCE))
       (DO ((,ITERATION-VAR 0 (1+ ,ITERATION-VAR))
            (,LENGTH-VAR (LENGTH ,SEQUENCE-VAR))
            (,VAR))
           ((= ,ITERATION-VAR ,LENGTH-VAR) ,RESULTFORM)
         (SETQ ,VAR (ELT ,SEQUENCE-VAR ,ITERATION-VAR))
         ,@BODY))))


(DEFMACRO DOTIMES ((VAR LIMIT &OPTIONAL RESULTFORM) &BODY BODY)
  "Iterate BODY with VAR bound to successive integers from 0 up to LIMIT's value.
LIMIT is evaluated only once.  When it is reached, RESULTFORM is executed and returned.
RETURN and GO can be used inside the BODY."
  (IF (FIXNUMP LIMIT)
      `(DO ((,VAR 0 (1+ ,VAR)))
           (( ,VAR ,LIMIT) ,RESULTFORM)
         . ,BODY)
    (LET ((ITERATION-VAR (GENSYM)))
      `(DO ((,VAR 0 (1+ ,VAR))
            (,ITERATION-VAR ,LIMIT))
           (( ,VAR ,ITERATION-VAR) ,RESULTFORM)
         . ,BODY))))

(DEFMACRO DO-FOREVER (&BODY BODY)
  "Execute BODY until it does a RETURN or a THROW."
  `(DO ()
       (())
     . ,BODY))


(defmacro without-interrupts (&rest body)
  "Execute BODY not allowing process-switching or sequence breaks.
If Control-Abort or Control-Break is typed while inside BODY,
it will not take effect until after they are finished."
  `(let ((inhibit-scheduling-flag t))
     . ,body))

(defmacro without-floating-underflow-traps (&body body)
  "Executes BODY with floating-point underflow traps disabled.
If a floating-point operation within body would normally underflow, zero is used instead."
  `(let ((zunderflow t))
     . ,body))

(defmacro gc:without-flipping (&body body)
  "Execute BODY inhibiting all new GC flips (a previous GC flip could still be active, though)."
  `(flet ((gc::.without-flipping. ()
             (declare ;(dbg:uninteresting-function macro)
                      (sys:downward-function))
             . ,body))
     (gc::without-flipping-internal #'gc::.without-flipping.)))

(defmacro gc:without-scavenging (&body body)
  "Execute BODY with scavenger disabled.  WITHOUT-INTERRUPTS effectively disables
idle scavenging, but scavenger can still run if you cons, so use this form instead."
  ;; The scavenger won't run if scheduling is inhibited.  Initially this was just because
  ;; there appeared to be lots of critical sections that already used without-interrupts
  ;; but seemed to want to preclude scavenging also.  This behavior was going to be temporary,
  ;; but I think it's OK to promote it to a Feature, and perhaps even document it.
  `(without-interrupts
     . ,body))

(deff-macro inhibit-gc-flips 'gc:without-flipping)
(make-obsolete inhibit-gc-flips "use GC:WITHOUT-FLIPPING")

(defmacro consing-in-area ((area) &body body)
  "Causes the default consing area to be AREA within the dynamic extent of BODY.
If AREA evaluates to NIL, then the usual area of memory is used."
  `(let ((zl:default-cons-area (or ,area zl:working-storage-area)))
     ,@body))


;;; Used to flag uses of functions as streams.
(defmacro define-functional-stream (name lambda-list &body body)
  "Defines a stream named NAME as a function.
The BODY does the work of stream, which usually involves dispatching on the
first argument.  If a stream encounters an operation it does not handle itself,
and doesn't have any other stream to which to send the operation, it can call
the function CALL-STREAM-DEFAULT-HANDLER with arguments of the operation,
the first argument, and a list of the arguments after that.

It is preferable to use DEFINE-SELECT-STREAM, since the dispatching on the
operation name is explicit, and the default handler will get called as
needed.  If you still decide to use this, you should try to use the macro
GENERICP:MESSAGECASE."
  (multiple-value-bind (body decls doc) (extract-declarations body nil t)
    `(progn
       (setf (get ',name 'si:io-stream-p) t)
       (defun ,name ,lambda-list
         (declare ,@decls)
         ,@(and doc (list doc))
         (macrolet ((call-stream-default-handler (operation arg1 &rest args)
                      `(zl:stream-default-handler ',',name ,operation ,arg1 ,@args)))
           ,@body)))))

(defun call-stream-default-handler (operation arg1 &rest args)
  "Call the stream default handler.
Valid only inside DEFINE-FUNCTIONAL-STREAM."
  (declare (eh:error-reporter) (ignore operation arg1 args))
  (error "~S called outside valid context" 'call-stream-default-handler))

(defmacro define-select-stream (name &body clauses)
  "Defines a stream named NAME as a function.
Each CLAUSE, whose first element is an operation name or a list thereof, is invoked
when that matching stream operation is invoked.  The second element of the clause
is the lambda list used in the rest of the clause.

This is the preferred way to define ``one of a kind'' function streams."
  (let ((default-handler
          (intern (zl:string-append (symbol-name name) "-DEFAULT-HANDLER"))))
    `(progn
       (defun ,default-handler (op &optional arg1 &rest rest)
         (zl:stream-default-handler ',name op arg1 rest))
       (setf (get ',name 'si:io-stream-p) t)
       (zl:defselect (,name ,default-handler) ,@clauses))))


;;;; WITH-OPEN-STREAM, WITH-OPEN-STREAM-CASE, WITH-OPEN-FILE, WITH-OPEN-FILE-CASE,
;;;;  WITH-OPEN-FILE-SEARCH

(DEFMACRO WITH-OPEN-STREAM ((STREAM CONSTRUCTION-FORM) &BODY BODY)
  "Execute the BODY with the variable STREAM bound to the value of CONSTRUCTOR-FORM.
On normal exit, close STREAM normally.
On abnormal exit (throwing, errors, etc) close STREAM with argument :ABORT."
  (LET ((GENSYM (GENSYM)))
    `(LET ((,GENSYM NIL)
           (.FILE-ABORTED-FLAG. :ABORT))
       (UNWIND-PROTECT
           (PROGN (SETQ ,GENSYM ,CONSTRUCTION-FORM)
                  (MULTIPLE-VALUE-PROG1 (LET ((,STREAM ,GENSYM))
                                          . ,BODY)
                                        (SETQ .FILE-ABORTED-FLAG. NIL)))
         (AND ,GENSYM (NOT (ERRORP ,GENSYM))
              (SEND ,GENSYM :CLOSE .FILE-ABORTED-FLAG.))))))

(DEFMACRO WITH-OPEN-STREAM-CASE ((STREAM CONSTRUCTION-FORM) &BODY CLAUSES)
  "Use CONSTRUCTOR-FORM to open a stream, using the CLAUSES as in CONDITION-CASE.
The CLAUSES may contain a :NO-ERROR clause which will be executed,
with STREAM bound to the resulting stream, if CONSTRUCTOR-FORM does not get an error.
On normal exit from the :NO-ERROR clause, STREAM is closed normally.
On abnormal exit (throwing, errors, etc) STREAM is closed with argument :ABORT."
  (LET ((GENSYM (GENSYM)))
    `(LET ((,GENSYM NIL)
           (.FILE-ABORTED-FLAG. ':ABORT))
       (UNWIND-PROTECT
           (MULTIPLE-VALUE-PROG1
             (CONDITION-CASE (,STREAM) (SETQ ,GENSYM ,CONSTRUCTION-FORM)
               . ,CLAUSES)
             (SETQ .FILE-ABORTED-FLAG. NIL))
         (AND ,GENSYM (NOT (ERRORP ,GENSYM))
              (SEND ,GENSYM :CLOSE .FILE-ABORTED-FLAG.))))))

(DEFMACRO WITH-OPEN-FILE ((STREAM FILENAME &REST OPTIONS) &BODY BODY)
  "Execute the BODY with the variable STREAM bound to a stream for file FILENAME.
FILENAME is opened using OPTIONS, which are the same as for the OPEN function.
On normal exit, close STREAM normally.
On abnormal exit (throwing, errors, etc) close STREAM with argument :ABORT."
  `(WITH-OPEN-STREAM (,STREAM (OPEN ,FILENAME . ,OPTIONS))
     . ,BODY))

(DEFMACRO WITH-OPEN-FILE-CASE ((STREAM FILENAME &REST OPTIONS) &BODY CLAUSES)
  "Use open a file stream from FILENAME and OPTIONS, using the CLAUSES as in CONDITION-CASE.
FILENAME and OPTIONS are passed to OPEN.
The CLAUSES may contain a :NO-ERROR clause which will be executed,
with STREAM bound to the resulting stream, if OPEN does not get an error.
On normal exit from the :NO-ERROR clause, STREAM is closed normally.
On abnormal exit (throwing, errors, etc) STREAM is closed with argument :ABORT."
  `(WITH-OPEN-STREAM-CASE (,STREAM (OPEN ,FILENAME . ,OPTIONS))
     . ,CLAUSES))

(DEFMACRO WITH-OPEN-FILE-SEARCH (&ENVIRONMENT ENV
                                 (STREAM (OPERATION DEFAULTS AUTO-RETRY)
                                         TYPE-LIST-AND-PATHNAME-FORM
                                         &REST OPEN-OPTIONS)
                                 &BODY BODY)
  "Open one of several filenames, the same except for the type component.
Binds the variable STREAM to the resulting stream, executes the BODY, then closes the stream.
OPEN-OPTIONS are alternating keywords and values, passed to OPEN.
TYPE-LIST-AND-PATHNAME-FORM is evaluated to get two values:
 a list of pathname types to try, and a base pathname.
The base pathname is merged successively with each type in the list.
This is done using FS:MERGE-PATHNAME-DEFAULTS, with DEFAULTS's value
used as the second argument and the type to be tried as the third argument.
As soon as a merged pathname succeeds in being opened, we execute BODY.
If they all fail, an error is signaled with condition FS:MULTIPLE-FILE-NOT-FOUND.
OPERATION should eval to the name of the calling function; it is used for signaling.
If AUTO-RETRY evals to non-NIL, then the user is asked to type a new
pathname to retry with."
  (LET ((BASE-PATHNAME-VAR (GENSYM))
        (TYPE-LIST-VAR (GENSYM))
        (DEFAULTS-VAR (GENSYM))
        (AUTO-RETRY-VAR (GENSYM)))
    (MULTIPLE-VALUE-BIND (REAL DECLS)
        (EXTRACT-DECLARATIONS BODY NIL NIL ENV)
      `(LET ((,DEFAULTS-VAR ,DEFAULTS)
             (,AUTO-RETRY-VAR ,AUTO-RETRY))
         (DECLARE . ,DECLS)
         (MULTIPLE-VALUE-BIND (,TYPE-LIST-VAR ,BASE-PATHNAME-VAR)
             ,TYPE-LIST-AND-PATHNAME-FORM
           (DECLARE . ,DECLS)
           (FILE-RETRY-NEW-PATHNAME-IF ,AUTO-RETRY-VAR (,BASE-PATHNAME-VAR FS:FILE-ERROR)
             (WITH-OPEN-STREAM (,STREAM
                                (FS:OPEN-FILE-SEARCH ,BASE-PATHNAME-VAR ,TYPE-LIST-VAR
                                                     ,DEFAULTS-VAR ,OPERATION
                                                     . ,OPEN-OPTIONS))
               (DECLARE . ,DECLS)
               . ,REAL)))))))

;;;; FILE-RETRY-NEW-PATHNAME, FILE-RETRY-NEW-PATHNAME-IF, WITH-OPEN-FILE-RETRY

(DEFMACRO FILE-RETRY-NEW-PATHNAME
          ((PATHNAME-VARIABLE . CONDITION-NAMES) &BODY BODY)
  "Execute BODY with a handler for CONDITION-NAMES that reads a new pathname and tries again.
If one of those conditions is signaled within BODY,
a new pathname is read and put in PATHNAME-VARIABLE,
and then BODY is executed again.
This is most useful when BODY is an OPEN, DELETEF, etc."
  (LET ((TAG (GENSYM)))
    `(BLOCK ,TAG
       (TAGBODY
        ,tag
           (SETQ ,PATHNAME-VARIABLE
             (CATCH ',TAG
               (RETURN-FROM ,TAG
                 (CONDITION-RESUME `(,',CONDITION-NAMES :NEW-PATHNAME T
                                     ("Try again with a new pathname, not telling the callers.")
                                     FILE-RETRY-RESUME-HANDLER ,',TAG)
                   (CONDITION-BIND ((,CONDITION-NAMES 'FILE-RETRY-HANDLER
                                     ,PATHNAME-VARIABLE ',TAG))
                     . ,BODY)))))
           (GO ,tag)))))


(DEFUN FILE-RETRY-RESUME-HANDLER (ERROR-OBJECT TAG &OPTIONAL NEW-PATHNAME)
  (DECLARE (IGNORE ERROR-OBJECT))
  (THROW TAG NEW-PATHNAME))

(DEFUN FILE-RETRY-HANDLER (ERROR-OBJECT PATHNAME TAG)
  (FORMAT *QUERY-IO* "~&~A" ERROR-OBJECT)
  (SETQ PATHNAME (FS:PARSE-PATHNAME PATHNAME))
  (LET* ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T)
         (FS:*NAME-SPECIFIED-DEFAULT-TYPE* NIL)
         (INPUT
           (PROMPT-AND-READ `(:PATHNAME-OR-NIL :DEFAULTS ,PATHNAME)
                            "~&Pathname to use instead (default ~A)~%or ~C to enter debugger: "
                            PATHNAME #/END)))
    (IF INPUT
        (THROW TAG INPUT)
      ;; else tell the condition system that we punted.
      NIL)))

(DEFMACRO FILE-RETRY-NEW-PATHNAME-IF
          (COND-FORM (PATHNAME-VARIABLE . CONDITION-NAMES) &BODY BODY)
  "Execute BODY with a handler for CONDITION-NAMES that reads a new pathname and tries again.
If COND-FORM evaluates non-NIL, then if one of those conditions is signaled within BODY,
a new pathname is read and put in PATHNAME-VARIABLE, and then BODY is executed again.
This is most useful when BODY is an OPEN, DELETEF, etc."
  (LET ((TAG (GENSYM)))
    `(BLOCK ,TAG
       (TAGBODY
        ,tag
           (setq ,pathname-variable
             (catch ',tag
               (RETURN-FROM ,TAG
                 (CONDITION-RESUME `(,',CONDITION-NAMES :NEW-PATHNAME T
                                     ("Try again with a new pathname, not telling the callers.")
                                     FILE-RETRY-RESUME-HANDLER ,',TAG)
                   (CONDITION-BIND-IF ,COND-FORM
                                      ((,CONDITION-NAMES 'FILE-RETRY-HANDLER
                                        ,PATHNAME-VARIABLE ',TAG))
                     . ,BODY)))))
           (go ,tag)))))

(DEFMACRO WITH-OPEN-FILE-RETRY ((STREAM (FILENAME . CONDITION-NAMES)
                                                 . OPTIONS)
                                         &BODY BODY)
  "Like WITH-OPEN-FILE, but provides a :NEW-PATHNAME resume handler around the OPEN.
Thus, if the open fails, condition handlers or the user can specify a
new pathname and retry the open."
  `(WITH-OPEN-STREAM (,STREAM
                      (FILE-RETRY-NEW-PATHNAME-IF T (,FILENAME . ,CONDITION-NAMES)
                        (OPEN ,FILENAME . ,OPTIONS)))
     . ,BODY))


;;;; FS:READING-FROM-STREAM, FS:READING-FROM-FILE-CASE FS:READING-FROM-FILE-CASE

(DEFUN READING-FROM-STREAM-EXPANDER (FORM STREAM STREAM-CONSTRUCTION-FORM BODY)
  (LET ((AVARS (GENSYM))
        (AVALS (GENSYM)))
    `(WITH-OPEN-STREAM (,STREAM ,STREAM-CONSTRUCTION-FORM)
       (MULTIPLE-VALUE-BIND (,AVARS ,AVALS)
           (FS:EXTRACT-ATTRIBUTE-BINDINGS ,STREAM)
         (LET ((.EOF. '(())))
           (PROGV ,AVARS ,AVALS
             (DO ((,FORM (CL:READ ,STREAM NIL .EOF.) (CL:READ ,STREAM NIL .EOF.)))
                 ((EQ ,FORM .EOF.))
               ,@BODY)))))))

(DEFMACRO READING-FROM-STREAM ((FORM STREAM-CONSTRUCTION-FORM) &BODY BODY)
  "Read one FORM at a file from FILE, reading till executing BODY each time.
Reads until end of STREAM."
  (READING-FROM-STREAM-EXPANDER FORM (GENSYM) STREAM-CONSTRUCTION-FORM BODY))

(DEFMACRO FS:READING-FROM-FILE ((FORM FILE &REST OPEN-OPTIONS) &BODY BODY)
  "Read one FORM at a time from FILE, executing BODY each time."
  `(READING-FROM-STREAM (,FORM (OPEN ,FILE :DIRECTION :INPUT . ,OPEN-OPTIONS))
     ,@BODY))

(DEFMACRO FS:READING-FROM-FILE-CASE ((FORM FILE ERROR) &BODY CLAUSES)
  "Like READING-FROM-FILE, but ERROR is bound to the error instance if
an error happens.  CLAUSES are are like those in WITH-OPEN-FILE-CASE;
:NO-ERROR must appear in one of them."
  ;; The implementation has to search for the clause containing :NO-ERROR
  ;; and wrap a READING-FROM-STREAM around its consequent body.
  ;; ERROR actually is the STREAM in the WITH-OPEN-FILE-CASE, though this
  ;; may change in the future.
  (LET ((NO-ERROR-CLAUSE ()) (FIRST-PART ()))
    (DOLIST (CLAUSE CLAUSES)
      (SETQ FIRST-PART (FIRST CLAUSE))
      (COND ((NULL FIRST-PART)                  ;Is this really the right thing ?
             ())
            ((EQ FIRST-PART :NO-ERROR)
             (RETURN (SETQ NO-ERROR-CLAUSE CLAUSE)))
            ((AND (CONSP FIRST-PART)
                  (MEMQ ':NO-ERROR FIRST-PART))
             (RETURN (SETQ NO-ERROR-CLAUSE CLAUSE)))
            (T ())))
    (IF (NOT NO-ERROR-CLAUSE)                   ; A little misleading, I must admit
        (FERROR "~S must have a ~S clause" 'FS:READING-FROM-FILE-CASE ':NO-ERROR)
      (SETQ CLAUSES (DELQ NO-ERROR-CLAUSE (COPY-LIST CLAUSES))) ; Remove :NO-ERROR,
      `(CONDITION-CASE (,ERROR) (OPEN ,FILE :DIRECTION :INPUT :ERROR NIL)
         ,@CLAUSES
         (,FIRST-PART
          ,(READING-FROM-STREAM-EXPANDER FORM (GENSYM) ERROR (CDR NO-ERROR-CLAUSE)))))))


;;>> fix by way of lunar language-tools
(DEFMACRO ONCE-ONLY (VARIABLE-LIST &BODY BODY)
  "Generate code that evaluates certain expressions only once.
This is used in macros, for computing expansions.
VARIABLE-LIST is a list of symbols, whose values are subexpressions
to be substituted into a larger expression.  BODY is what uses those
symbols' values and constructs the larger expression.

ONCE-ONLY modifies BODY so that it constructs a different expression,
which when run will evaluate the subsexpressions only once, save the
values in temporary variables, and use those from then on.
Example:
/(DEFMACRO DOUBLE (ARG) `(+ ,ARG ,ARG)) expands into code that computes ARG twice.
/(DEFMACRO DOUBLE (ARG) (ONCE-ONLY (ARG) `(+ ,ARG ,ARG))) will not."
  (DOLIST (VARIABLE VARIABLE-LIST)
    (IF (NOT (SYMBOLP VARIABLE))
        (FERROR "~S is not a variable" VARIABLE)))
  (LET ((BIND-VARS (GENSYM))
        (BIND-VALS (GENSYM))
        (TEM (GENSYM)))
    `(LET ((,BIND-VARS NIL)
           (,BIND-VALS NIL))
       (LET ((RESULT ((LAMBDA ,VARIABLE-LIST . ,BODY)
                      . ,(LOOP FOR VARIABLE IN VARIABLE-LIST
                               COLLECT `(IF (OR (ATOM ,VARIABLE)
                                                (EQ (CAR ,VARIABLE) 'QUOTE)
                                                (EQ (CAR ,VARIABLE) 'FUNCTION))
                                            ,VARIABLE
                                          (LET ((,TEM (GENSYM)))
                                            (PUSH ,TEM ,BIND-VARS)
                                            (PUSH ,VARIABLE ,BIND-VALS)
                                            ,TEM))))))
         (IF (NULL ,BIND-VARS)
             RESULT
           `((LAMBDA ,(NREVERSE ,BIND-VARS) ,RESULT) . ,(NREVERSE ,BIND-VALS)))))))

;;; (KEYWORD-EXTRACT <keylist> KEY (FOO (UGH BLETCH) BAR) (FLAG FALG) <otherwise> ...)
;;; parses a list of alternating keywords and values, <keylist>.
;;; The symbol KEY is bound internally to remaineder of the keyword list.
;;; The keywords recognized are :FOO, :BAR and UGH;  whatever follows
;;; the keyword UGH is put in the variable BLETCH, whatever follows the
;;; keyword :FOO is put in the variable FOO, and similar for BAR.
;;; The flags are :FLAG and :FALG;  if :FLAG is seen, FLAG is set to T.
;;; <otherwise> is one or more CASE clauses which can be used
;;; to recognize whatever else you like, in nonstandard format.
;;; To gobble the next thing from the <keylist>, say (CAR (SETQ KEY (CDR KEY))).
(DEFMACRO KEYWORD-EXTRACT (KEYLIST KEYVAR KEYWORDS &OPTIONAL FLAGS &BODY OTHERWISE)
  "Look through KEYLIST for keywords and set some variables and flags.
KEYLIST's value should be a list of keywords, some followed by values.
KEYWORDS describes the keywords to check for.  Each element describes one keyword.
An element can be a list of a keyword and the variable to store its value in,
or just the variable to store in (the keyword has the same pname, in the keyword package).
FLAGS is like KEYWORDS except that the flags are not followed by values;
the variable is set to T if the flag is present at all.

KEYVAR is a variable used internally by the generated code, to hold
the remaining part of the list.
OTHERWISE is some CASE clauses that will be executed if an element of KEYLIST
is not a recognized flag or keyword.  It can refer to KEYVAR."
  `(DO ((,KEYVAR ,KEYLIST (CDR ,KEYVAR)))
       ((NULL ,KEYVAR))
     (CASE (CAR ,KEYVAR)
       ,@(MAPCAR (LAMBDA (KEYWORD)
                   (COND ((ATOM KEYWORD)
                          `(,(INTERN (STRING KEYWORD) "KEYWORD")
                            (SETQ ,KEYWORD (CAR (SETQ ,KEYVAR (CDR ,KEYVAR))))))
                         (T `(,(CAR KEYWORD)
                              (SETQ ,(CADR KEYWORD)
                                    (CAR (SETQ ,KEYVAR (CDR ,KEYVAR))))))))
                 KEYWORDS)
       ,@(MAPCAR (LAMBDA (KEYWORD)
                   (COND ((ATOM KEYWORD)
                          `(,(INTERN (STRING KEYWORD) PKG-KEYWORD-PACKAGE)
                            (SETQ ,KEYWORD T)))
                         (T `(,(CAR KEYWORD)
                              (SETQ ,(CADR KEYWORD) T)))))
                 FLAGS)
       ,@OTHERWISE
       ,@(IF (NOT (MEMQ (CAAR (LAST OTHERWISE)) '(T OTHERWISE)))
             `((OTHERWISE
                 (FERROR "~S is not a recognized keyword" (CAR ,KEYVAR))))))))

;;; PROCESS-DEFAULTS is for use in macros which handle keyword arguments
;;; (via KEYWORD-EXTRACT or whatever).
;;;
;;; Usage:
;;;   (PROCESS-DEFAULTS DEFAULT-LIST)
;;;
;;; DEFAULT-LIST is a list of two-element lists.  Each element of the list should
;;; contain the name of a variable to default as its CAR, and the default value
;;; as its CADR.  If the variable is NIL, its value will be set to the default.
;;;
;;; This macro-expands into a series of IF statements (with a SETQ statement as the
;;; consequent).  The variable and default-value names are copied directly into the
;;; SETQ's, so you probably want to quote the values.
;;;
;;; The DEFAULT-LIST is evaluated, so you have to quote it as well if it is constant.

(defmacro process-defaults (default-list)
  `(progn . ,(mapcar (lambda (default)
                       (let ((var (car default))
                             (val (cadr default)))
                         `(if (null ,var) (setq ,var ,val))))
                     (eval default-list))))


;These are handy for flushing with-stack-list, etc, without restructuring code.
(defmacro with-list (variable-and-elements &body body)
  "Similiar to with-stack-list, but conses normal list"
  `(let ((,(car variable-and-elements) (list ,@(cdr variable-and-elements))))
     ,@body))

(defmacro with-list* (variable-and-elements &body body)
  "Similiar to with-stack-list*, but conses normal list"
  `(let ((,(car variable-and-elements) (list* ,@(cdr variable-and-elements))))
     ,@body))

; gee, guys, how about
;   "(defmacro with-cons ((var car cdr)) `(let ((,var (cons ,car ,cdr))) . ,body))"?


;;; Bind NAME-TO-BIND to a cleanup-list,
;;; and on exit do any cleanup-actions stored in the list.
;;; The body can pass NAME-TO-BIND to various allocation functions,
;;; which will attach cleanups to the car of the cleanup-list
;;; so that the objects they allocate will be returned.
;;; A cleanup is just a cons of a function and arguments.
;;; the arguments are not evaluated.
(DEFMACRO WITH-CLEANUP-LIST (&ENVIRONMENT ENV NAME-TO-BIND &BODY BODY)
  (MULTIPLE-VALUE-BIND (REAL DECLS)
      (EXTRACT-DECLARATIONS BODY NIL NIL ENV)
    `(LET ((,NAME-TO-BIND (LIST NIL)))
       (DECLARE . ,DECLS)
       (UNWIND-PROTECT
           (PROGN . ,REAL)
         (LOOP FOR ELEM IN (CAR ,NAME-TO-BIND)
                           DO (APPLY (CAR ELEM) (CDR ELEM)))))))

;;; Add a cleanup to a list, returns the cleanup object.
(DEFUN ADD-CLEANUP (CLEANUP-LIST FUNCTION &REST ARGS)
  (WITHOUT-INTERRUPTS
    (PUSH (CONS FUNCTION (COPYLIST ARGS))
          (CAR CLEANUP-LIST))
    (CAAR CLEANUP-LIST)))

;;; Delete a cleanup from a list
(DEFUN DELETE-CLEANUP (CLEANUP CLEANUP-LIST)
  (WITHOUT-INTERRUPTS
    (SETF (CAR CLEANUP-LIST) (DELQ CLEANUP (CAR CLEANUP-LIST)))))

;;; Move a specific cleanup action from one cleanup-list to another, atomically.
(DEFUN MOVE-CLEANUP (CLEANUP FROM-CLEANUP-LIST TO-CLEANUP-LIST)
  (WITHOUT-INTERRUPTS
    (SETF (CAR FROM-CLEANUP-LIST) (DELQ CLEANUP (CAR FROM-CLEANUP-LIST)))
    (PUSH CLEANUP (CAR TO-CLEANUP-LIST))))

;;; Replace one cleanup with another, atomically.
(DEFUN REPLACE-CLEANUP (OLD-CLEANUP NEW-CLEANUP CLEANUP-LIST)
  (WITHOUT-INTERRUPTS
    (SETF (CAR CLEANUP-LIST) (CONS NEW-CLEANUP (DELQ OLD-CLEANUP (CAR CLEANUP-LIST))))))

;;;; Quiche

(DEFMACRO UNWIND-PROTECT-CASE ((&OPTIONAL ABORTED-P-VAR) BODY &REST CLAUSES)
  "Execute BODY inside an UNWIND-PROTECT form.
Each element of CLAUSES is a list of the form (<keyword> . <cruft>), where
<keyword> specifies under which condition to execute the associated <cruft>
Each keyword must be one of :ALWAYS, meaning to execute the cruft regardless
of whether BODY is exited non-locally or terminates normally,
:ABORT meaning to execute it only if BODY exits non-locally, or
:NORMAL meaning do it only if BODY returns /"normally./"
ABORTED-P-VAR, if supplied, is used to flag whether BODY exited abnormally:
 it is normally set to NIL by successful execution of BODY, but may be set to
 NIL within BODY, meaning not to execute :ABORT clauses even if BODY later exits abnormally.
The values returned are those of BODY.

This macro is for wimps who can't handle raw, manly UNWIND-PROTECT"
  (OR ABORTED-P-VAR (SETQ ABORTED-P-VAR '.ABORTED-P.))
  `(LET ((,ABORTED-P-VAR T))
     (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 ,BODY (SETQ ,ABORTED-P-VAR NIL))
       ,@(LOOP FOR (KEYWORD . FORMS) IN CLAUSES
               COLLECT (ECASE KEYWORD
                         (:NORMAL `(WHEN (NOT ,ABORTED-P-VAR) ,@FORMS))
                         (:ABORT `(WHEN ,ABORTED-P-VAR ,@FORMS))
                         (:ALWAYS `(PROGN ,@FORMS)))))))

;;; WITH-HELP-STREAM sets up a stream for printing a long help message.
;;; This is a pop-up window (like FINGER windows) if the parent stream is a window,
;;; otherwise the stream is simply the parent stream (this avoids lossage if the
;;; stream is a file stream or the cold-load stream.).
;;;
;;; Usage:
;;;   (WITH-HELP-STREAM (STREAM . OPTIONS) &BODY BOD)
;;;
;;; STREAM is a symbol to assign the new stream to.  Output operations in the body
;;; (PRINT, FORMAT, etc.) should use this symbol as the stream argument.
;;;
;;; OPTIONS is a keyword argument list.  The following options are recognized:
;;;   :LABEL     Label to give the help window.  Default is a null string.
;;;   :WIDTH     Symbol to take on the value of the width in characters of the stream.
;;;              An internal symbol is used (and ignored) if none is specified.
;;;   :HEIGHT    Symbol to take on the value of the height in characters of the stream.
;;;              An internal symbol is used (and ignored) if none is specified.
;;;   :SUPERIOR  The superior stream.  Defaults to *TERMINAL-IO*.

(defmacro with-help-stream (&environment env
                            (stream &key (label "") (superior '*terminal-io*)
                                         width height)
                            &body body)
  "Execute the BODY with STREAM bound to a stream for printing help text on.
If *TERMINAL-IO* or the specified superior is a window, a special /"help window/"
is popped up and used as STREAM.
If *TERMINAL-IO* or the specified superior is not a window, it is used directly.
LABEL is a string to use as the label of the help window if one is used.
WIDTH is a symbol to bind to the width in characters of STREAM.
 It is bound while BODY is executed.
HEIGHT is a symbol to bind to the height in characters.
SUPERIOR is a window to use as the superior of the help window."
  (multiple-value-bind (body decls)
      (extract-declarations body nil nil env)
    `(flet ((.with-help-stream. (,stream &aux ,@(if width (list width))
                                              ,@(if height (list height)))
              (declare ;(dbg:uninteresting-function macro)
                       (sys:downward-function)
                       . ,decls)
              (if (operation-handled-p ,stream :size-in-characters)
                  ,(if (or width height)
                       `(multiple-value-setq (,width ,height)
                          (send ,stream :size-in-characters)))
                ,(if width `(setq ,width 85.))
                ,(if height `(setq ,height 66.)))
              . ,body))
       (with-help-stream-1 ,label ,superior #'.with-help-stream.))))

(defun with-help-stream-1 (label superior continuation &aux input-p)
  (cond ((send superior :send-if-handles :no-inferior-windows-p)
         (funcall continuation superior))
        ((typep superior 'tv:sheet)
         (using-resource (stream tv::pop-up-finger-window)
           (lexpr-send stream :set-edges (multiple-value-list (send superior :edges)))
           (send stream :set-label label)
           (tv::window-call (stream :deactivate)
             (send stream :clear-window)
             (funcall continuation stream)
             (format stream "~2&Type any character to continue: ")
             (send stream :wait-for-input-or-deexposure)
             ;; This hair is so that if we woke up due to deexposure
             ;; we do not try to read anything;
             ;; if we woke up due to input, we do not read until after
             ;; we deactivate the help window, so that if the input is Break
             ;; the break-loop is not entered until our normal window is usable again.
             (if (and (send stream :exposed-p)
                      (send stream :listen))
                 (setq input-p t)))
           (if input-p
               (send (if (operation-handled-p superior :tyi-no-hang)
                         superior
                         *terminal-io*)
                     :tyi-no-hang))))
        (t (funcall continuation superior))))

;;;; Macros relating to warnings (compiler, etc).

;;; Variables bound by the following macros.
(PROCLAIM '(SPECIAL OBJECT-WARNINGS-DATUM OBJECT-WARNINGS-LOCATION-FUNCTION
                    OBJECT-WARNINGS-OBJECT-NAME
                    OBJECT-WARNINGS-PUSHING-LOCATION)
          '(SPECIAL FILE-WARNINGS-DATUM FILE-WARNINGS-PATHNAME
                    FILE-WARNINGS-PUSHING-LOCATION
                    PREMATURE-WARNINGS PREMATURE-WARNINGS-THIS-OBJECT))
;;; Use this around an operation that goes through some or all the objects in a file.
;;; WHOLE-FILE-P should evaluate to T if we are doing the entire file.
(DEFMACRO FILE-OPERATION-WITH-WARNINGS ((GENERIC-PATHNAME OPERATION-TYPE WHOLE-FILE-P)
                                        &BODY BODY)
  "Execute BODY, recording warnings for performing OPERATION-TYPE on file GENERIC-PATHNAME.
WHOLE-FILE-P should evaluate to non-NIL if the body will process all of the file.
OPERATION-TYPE is most frequently ':COMPILE, in the compiler."
  `(LET* ((FILE-WARNINGS-DATUM FILE-WARNINGS-DATUM)
          (FILE-WARNINGS-PATHNAME FILE-WARNINGS-PATHNAME)
          (FILE-WARNINGS-PUSHING-LOCATION FILE-WARNINGS-PUSHING-LOCATION)
          (PREMATURE-WARNINGS PREMATURE-WARNINGS)
          (PREMATURE-WARNINGS-THIS-OBJECT PREMATURE-WARNINGS-THIS-OBJECT)
          (NEW-FILE-THIS-LEVEL
            (BEGIN-FILE-OPERATION ,GENERIC-PATHNAME ,OPERATION-TYPE)))
     (MULTIPLE-VALUE-PROG1
       (PROGN . ,BODY)
       (DISPOSE-OF-WARNINGS-AFTER-LAST-OBJECT)
       (AND ,WHOLE-FILE-P NEW-FILE-THIS-LEVEL (END-FILE-OPERATION)))))

;;; Use this around operating on an individual object,
;;; inside (dynamically) a use of the preceding macro.
(DEFMACRO OBJECT-OPERATION-WITH-WARNINGS ((OBJECT-NAME LOCATION-FUNCTION INCREMENTAL)
                                          &BODY BODY)
  "Execute BODY, recording warnings for OBJECT-NAME.
If INCREMENTAL evaluates to NIL, all previous warnings about that object
are discarded when the body is finished.  OBJECT-NAME is the name
of an object in the file set up with FILE-OPERATION-WITH-WARNINGS;
each file is its own space of object names, for recording warnings.
This macro's expansion must be executed inside the body of a
FILE-OPERATION-WITH-WARNINGS.  LOCATION-FUNCTION's value tells the editor
how to find this object's definition in the file; usually it is NIL."
  `(LET-IF (NOT (EQUAL ,OBJECT-NAME OBJECT-WARNINGS-OBJECT-NAME))
           ((OBJECT-WARNINGS-DATUM OBJECT-WARNINGS-DATUM)
            (OBJECT-WARNINGS-LOCATION-FUNCTION OBJECT-WARNINGS-LOCATION-FUNCTION)
            (OBJECT-WARNINGS-OBJECT-NAME OBJECT-WARNINGS-OBJECT-NAME)
            (OBJECT-WARNINGS-PUSHING-LOCATION OBJECT-WARNINGS-PUSHING-LOCATION))
     (LET ((NEW-OBJECT-THIS-LEVEL
             (BEGIN-OBJECT-OPERATION ,OBJECT-NAME ,LOCATION-FUNCTION)))
       (MULTIPLE-VALUE-PROG1
         (PROGN . ,BODY)
         (AND ,(NOT INCREMENTAL) NEW-OBJECT-THIS-LEVEL (END-OBJECT-OPERATION))))))


;;;; WITH-INPUT-FROM-STRING, WITH-OUTPUT-TO-STRING

(defmacro with-input-from-string (&environment env
                                  (stream string . keyword-args) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
The values of BODY's last expression are returned.
Keywords allowed are :START, :END and :INDEX.
:START and :END can be used to specify a substring of STRING to be read from.
 Eof will then occur when :END is reached.
 If the :END value is NIL, that means the end of STRING.
:INDEX specifies a SETF-able place to store the index of where reading stopped.
 This is done after exit from the body.  It stores the index of the first unread character,
 or the index of eof if that was reached.
/
Old calling format: (STREAM STRING &OPTIONAL INDEX END), where INDEX serves
 as the value for the :START keyword and for the :INDEX keyword."
  (declare (arglist (stream string &key (start 0) end index) &body body))
  (let (start end index)
    (multiple-value-bind (realbody decls)
        (extract-declarations body nil nil env)
      (cond ((keywordp (car keyword-args))
             (setq start (getf keyword-args :start)
                   index (getf keyword-args :index)
                   end (getf keyword-args :end)))
            (keyword-args
             (setq start (car keyword-args)
                   index (car keyword-args)
                   end (cadr keyword-args))
             (compiler::warn 'warn :obsolete
                             "~~S called with obsolete calling sequence~&~
                                 ~2,T(~:*~S (~S ~S ~{ ~S~} ...) ...);~%~
                                 ~2,TUse (~S (~S ~S ~@{~:[~*~*~; ~S ~S~]~}) ...) instead.~"
                             'with-input-from-string stream string keyword-args
                             'with-input-from-string stream string
                             start :start start index :index index end :end end))
            (t nil))
      `(let ((,stream (make-string-input-stream ,string ,(or start 0) ,end)))
         (declare . ,decls)
         (unwind-protect
             (progn . ,realbody)
           ,(if index `(setf ,index (send ,stream :get-string-index))))))))

(defmacro with-output-to-string (&environment env (stream &optional string index) &body body)
  "Execute BODY with STREAM bound to a stream to output into STRING.
If STRING is omitted, a new string with no fill pointer is created and returned.
If STRING is supplied, that string's contents are modified destructively,
and the values of BODY's last expression are returned.
If INDEX is supplied, it should be a SETFable accessor which describes
where to find the index to store into STRING, instead of at the end.
The value of INDEX will be updated after the BODY is finished."
  (multiple-value-bind (realbody decls)
      (extract-declarations body nil nil env)
    (if string
        `(let ((,stream (make-string-output-stream ,string ,index)))
           (declare . ,decls)
           (unwind-protect
               (progn . ,realbody)
             ,(if index `(setf ,index (send ,stream :get-string-index)))))
      `(let ((,stream (make-string-output-stream)))
         (declare . ,decls)
         ,@realbody
         (get-output-stream-string ,stream)))))

(defmacro with-input-editing ((stream rubout-options . brand-s-compatibility-args)
                              &body body)
  "Execute BODY inside of STREAM's :RUBOUT-HANDLER method.
If BODY does input from STREAM, it will be done with rubout processing
if STREAM implements any.
RUBOUT-OPTIONS should be the options for the :RUBOUT-HANDLER message, such as
 (:NO-INPUT-SAVE T)   -- don't save this batch of input in the history.
 (:FULL-RUBOUT flag)  -- return from this construct if rubout buffer becomes empty
        with two values: NIL and flag.
 (:INITIAL-INPUT string) -- start out with that string in the buffer.
 (:INITIAL-INPUT-POINTER n) -- start out with editing pointer n chars from start.
 (:ACTIVATION fn x-args) -- fn is used to test characters for being activators.
        fn's args are the character read followed by the x-args from the option.
        If fn returns non-NIL, the character is an activation.
        It makes a blip (:ACTIVATION char numeric-arg)
        which BODY can read with :ANY-TYI.
 (:DO-NOT-ECHO chars...) -- poor man's activation characters.
        This is like the :ACTIVATION option except that: characters are listed explicitly;
        and the character itself is returned when it is read,
        rather than an :ACTIVATION blip.
 (:COMMAND fn x-args) -- tests like :ACTIVATION, but command chars do a different thing.
        If fn returns non-NIL, the character is a command character.
        The :RUBOUT-HANDLER operation (and therefore the WITH-INPUT-EDITING)
        returns instantly these two values: (:COMMAND char numeric-arg) :COMMAND.
        The input that was buffered remains in the buffer.
 (:PREEMPTABLE token) -- makes all blips act like command chars.
        If the rubout handler encounters a blip while reading input,
        it instantly returns two values: the blip itself, and the specified token.
        Any buffered input remains buffered for the next request for input editing.
 (:EDITING-COMMAND (char doc)...) -- user-implemented /"editing/" commands.
        If any char in the alist is read by the rubout handler,
        it is returned to the caller (that is, to an :ANY-TYI in BODY).
        BODY should process these characters in appropriate ways and keep reading.
 (:PASS-THROUGH chars...) -- makes chars not be treated specially by the rubout
        handler. Useful for getting characters such as  into the buffer.
        Only works for characters with no control, meta, etc bits set.
 (:PROMPT fn-or-string)
        Says how to prompt initially for the input.
        If a string, it is printed; otherwise it is called with two args,
        the stream and a character which is an editing command that says
        why the prompt is being printed.
 (:REPROMPT fn-or-string)
        Same as :PROMPT except used only if the input is reprinted
        for some reason after editing has begun.  The :REPROMPT option
        is not used on initial entry.  If both :PROMPT and :REPROMPT
        are specified, :PROMPT is used on initial entry and :REPROMPT thereafter.
 (:NONRECURSIVE T)
        Means to ignore previously-specified rubout-handler options and only use the
        options specified to this call to WITH-INPUT-EDITING."
  (declare (arglist (stream rubout-handler-options) &body body))
  (let ((keyword (cadr brand-s-compatibility-args))
        (strmvar (gensym)))
    `(flet ((.with-input-editing. ()
               (declare ;(dbg:uninteresting-function macro)
                        (sys:downward-function))
               . ,body))
       (let ((,strmvar (decode-read-arg ,stream)))
         (if (operation-handled-p ,strmvar :rubout-handler)
             ,(if keyword
                  `(with-stack-list* (options
;character lossage
                                       ',(case keyword
                                           (:end-activation '(:activation = #/End))
                                           ((:line :line-activation)
                                            '(:activation memq (#/End #/Return))))
                                       ,rubout-options)
                     (send ,strmvar :rubout-handler options #'.with-input-editing.))
                `(send ,strmvar :rubout-handler ,rubout-options #'.with-input-editing.))
           (let ((rubout-handler nil))
             (.with-input-editing.)))))))

(DEFMACRO WITH-LOCK (&ENVIRONMENT ENV
                     (LOCATOR &KEY NORECURSIVE (WHOSTATE "Lock" WHOSTATEP) TIMEOUT)
                     &BODY BODY)
  "Execute the BODY with a lock locked.
LOCATOR is an expression whose value is the lock status;
it should be suitable for use inside LOCF.
NORECURSIVE means do not allow locking a lock already locked by this process.
WHOSTATE is what to display if we hang waiting for the lock.
TIMEOUT, if non-NIL, say to signal a SYS:LOCK-TIMEOUT condition if the lock remains
  unavailable for that many 60'ths of a second.  Otherwise, we wait indefinitely."
  (let ((lock (macroexpand `(locf ,locator) env)))
    `(LET* ((.POINTER. ,lock)
            (.ALREADY.MINE. (EQ (CAR .POINTER.) CURRENT-PROCESS)))
       ;; Kludge due to the fact the fact that the compiler knows nothing about types.
       ;; Common cases which are guaranteed to return locatives.
       ,@(if (memq (car-safe lock) '(variable-location aloc locate-in-instance %instance-loc))
             ()
           `((IF (CONSP .POINTER.)
                 (SETQ .POINTER. (CDR-LOCATION-FORCE .POINTER.)))))
       (UNWIND-PROTECT
           (PROGN
             (IF .ALREADY.MINE.
                 ,(IF NORECURSIVE `(FERROR "Attempt to lock ~S recursively."
                                           ',LOCATOR))
               ;; Redundant, but saves time if not locked.
               (OR (%STORE-CONDITIONAL .POINTER. NIL CURRENT-PROCESS)
                   ,(cond (timeout
                           `(process-lock .pointer. nil ,whostate ,timeout))
                          (whostatep
                           `(process-lock .pointer. nil ,whostate))
                          (t
                           `(process-lock .pointer.)))))
             . ,BODY)
         (UNLESS .ALREADY.MINE.
           (%STORE-CONDITIONAL .POINTER. CURRENT-PROCESS NIL))))))

(DEFMACRO WITH-TIMEOUT ((DURATION &BODY TIMEOUT-FORMS) &BODY BODY)
  "Execute BODY with a timeout set for DURATION 60'ths of a second from time of entry.
If the timeout elapses while BODY is still in progress,
the TIMEOUT-FORMS are executed and their values returned, and
whatever is left of BODY is not done, except for its UNWIND-PROTECTs.
If BODY returns, is values are returned and the timeout is cancelled.
The timeout is also cancelled if BODY throws out of the WITH-TIMEOUT."
  `(LET ((.PROC. (PROCESS-RUN-FUNCTION "WITH-TIMEOUT"
                                       'WITH-TIMEOUT-INTERNAL
                                       ,DURATION CURRENT-PROCESS)))
     (CONDITION-CASE ()
         (UNWIND-PROTECT
             (PROGN . ,BODY)
           (SEND .PROC. :KILL))
       (TIMEOUT
        . ,TIMEOUT-FORMS))))

(DEFMACRO WITH-SYS-HOST-ACCESSIBLE (&BODY BODY)
  "Execute the BODY, making sure we can read files without user interaction.
This is done by logging in if necessary (and logging out again when done)."
  `(LET (UNDO-FORM)
     (UNWIND-PROTECT
         (PROGN
           (SETQ UNDO-FORM (MAYBE-SYS-LOGIN))
           . ,BODY)
       (EVAL UNDO-FORM))))

;; mucklisp array stuff (store, get-locative-pointer-into-array, arraycall)
;;  moved into sys2; macarray

;;;TIME comparison functions
;;;

;;;||| Portabilize, 'tho Falcon may have bigger/different time values(?)  -Keith 10/18/88

#+(target falcon)
(warn "Don't you want to think about how many bits in the Falcon CPU clock time values?   [The Fantom]")

(defconstant %%time-compare-byte-spec (BYTE 23. 0.)
  "Byte spec for comparable portion of time values.")

(defconstant %time-compare-bit-mask #o20000000
  "Bit mask for comparing order of two time values.")

(DEFSUBST TIME-LESSP (TIME1 TIME2)
  "Compare two values of processor clock time [see (ZL:TIME)];
return T if TIME1 is less than TIME2.
Both time values are measured in 60'ths of a second.
If two time values too far apart are compared, you get the wrong answer,
since the processor clock wraps around.
DO NOT USE (ZL:TIME) for applications where that can matter."
  (BIT-TEST %time-compare-bit-mask (%POINTER-DIFFERENCE TIME1 TIME2)))

(DEFSUBST TIME-DIFFERENCE (TIME1 TIME2)
  "Subtract one value of processor clock time [see (ZL:TIME)] from another,
or subtract an interval from a time.
Both time values are measured in 60'ths of a second.
This works correctly with clock wrap-around provided that
the time values are not too far apart or the interval is not too long.
DO NOT USE (ZL:TIME) for applications where that can matter."
  (LDB %%time-compare-byte-spec (%POINTER-DIFFERENCE TIME1 TIME2)))

(DEFSUBST TIME-INCREMENT (TIME INCREMENT)
  "Add an incremental (interval) value to value of processor clock time [see (ZL:TIME)].
Both the TIME and the INCREMENT are measured in 60'ths of a second.
This works correctly with wrap around provided times are not too far apart
or the interval is not too long.
DO NOT USE (ZL:TIME) for applications where that can matter."
  (LDB %%time-compare-byte-spec (%POINTER-PLUS TIME INCREMENT)))
