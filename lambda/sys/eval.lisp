;;-*- Mode:LISP; Package:SI; Lowercase:T; Base:10; Cold-Load:T; Readtable:ZL -*-

;;;EVAL.LISP

;;; Simple lexical evaluator.  Written by RMS.
;;; You can use it, provided you return all improvements to him.

;;; Special form definitions moved to SPECIAL-FORMS.LISP
;;; during port of new interpreter. - Keith 9/10/88

;;; 4-Mar-87 09:20:37, insert these to help the new interpreter
;;; come up:

(defun proclaim-special (symbol)
  (setf (get symbol 'special) t))

(defun proclaim-unspecial (symbol)
  (remprop symbol 'special))

(defparameter variable-special-properties '(special system-constant))

(defsubst proclaimed-special-p (symbol)
  (cadr (getl symbol variable-special-properties)))

;;; 5-Mar-86 13:27:05, got rid of &QUOTE'd and got rid of storing the special
;;; form definition in the function cell of the symbol. -George Carrette
;;; we have retained &QUOTE via DEFUN-COMPATIBILITY.

(DEFMACRO INTERPRETER-SPECIAL-FORM (SYMBOL)
  ;; this is as good a place as any to store this info. We hope to
  ;; replace this intepreter with a good, clean, fast, explicit control
  ;; interpreter of some kind in the future.
  `(GET ,SYMBOL 'INTERPRETER-SPECIAL-FORM))

(DEFPROP :SPECIAL-FORM SPECIAL-FORM-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)

(DEFSTRUCT (INTERPRETER-SPECIAL-FORM :CONC-NAME :NAMED)
  NAME
  HANDLER)

(DEFUN SPECIAL-FORM-FUNCALL-ERROR-MAKER (NAME)
  #'(LAMBDA (&REST L)
      (CERROR "kludge it with eval" "funcalling or applying the special form ~S" NAME)
      (EVAL (CONS NAME (MAPCAR #'(LAMBDA (X) `',X) L)))))

(DEFUN SPECIAL-FORM-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((SYMBOL (SECOND FUNCTION-SPEC)))
    (OR (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
        (SPECIAL-FORM-FUNCTION-SPEC-HANDLER 'VALIDATE-FUNCTION-SPEC FUNCTION-SPEC)
        (FERROR 'SYS:INVALID-FUNCTION-SPEC "Invalid function spec ~S." FUNCTION-SPEC))
    (CASE FUNCTION
      (VALIDATE-FUNCTION-SPEC
       (AND (= (LENGTH FUNCTION-SPEC) 2) (SYMBOLP SYMBOL)))
      (FDEFINE
       (FSET SYMBOL (SPECIAL-FORM-FUNCALL-ERROR-MAKER SYMBOL))
       (SETF (INTERPRETER-SPECIAL-FORM SYMBOL)
             (IF (TYPEP ARG1 'INTERPRETER-SPECIAL-FORM)
                 ARG1
               (MAKE-INTERPRETER-SPECIAL-FORM NAME SYMBOL HANDLER ARG1))))
      (FDEFINITION
       (INTERPRETER-SPECIAL-FORM SYMBOL))
      (fdefinedp
       (let ((f (INTERPRETER-SPECIAL-FORM SYMBOL)))
         (values (not (null f)) f)))
      (FDEFINITION-LOCATION
       (LOCF (INTERPRETER-SPECIAL-FORM SYMBOL)))
      (FUNDEFINE
       (SETF (INTERPRETER-SPECIAL-FORM SYMBOL) NIL))
      (putprop
       (putprop symbol arg1 arg2)               ;Save property for the symbol itself too
       (function-spec-default-handler function function-spec arg1 arg2))
      (push-property
       (push arg1 (get symbol arg2))
       (function-spec-default-handler function function-spec arg1 arg2))
      (remprop
       (remprop symbol arg1)                    ;Delete property for the symbol itself too
       (function-spec-default-handler 'putprop function-spec nil arg1))
      (OTHERWISE
       (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

(DEFMACRO DEFSPECIALK (NAME ARGLIST &BODY BODY)
  ;; stands for defSPECIALkludge. for use in cold-load files.
  "Bootstraps a special form for the interpreter."
  (LET ((NEW-DEFUN (COMPILER:DEFUN-COMPATIBILITY-OLD-LISPM `(DEFUN ,NAME ,ARGLIST ,@BODY) T)))
    (OR (AND (NOT (ATOM NEW-DEFUN))
             (EQ (CAR NEW-DEFUN) 'DEFUN)
             (NOT (ATOM (CADR NEW-DEFUN)))
             (EQ (CAR (CADR NEW-DEFUN)) :SPECIAL-FORM))
        (FERROR NIL "Not a (DEFUN (:SPECIAL-FORM ...) ...) ~S" NEW-DEFUN))
    `(progn (defun (:property ,(cadr (cadr new-defun)) special-form-bootstrap)
                   ,@(cddr new-defun))
            (bootstrap-special-form ',(cadr (cadr new-defun))))))

(defun bootstrap-special-form (name)
  (fdefine (list :special-form name) (get name 'special-form-bootstrap) t))

;(eval-when (eval compile)
;  (unless (get '%regular-pdl-index 'compiler::qlval)
;    (defmacro %regular-pdl-index ()
;      `(with-stack-list (tem nil)
;        (%make-pointer-offset dtp-locative tem -1)))))

;this should work, but is bletcherous.
;(defmacro one-q-forwardify (address pointer)
;  `(without-interrupts
;     (%p-store-pointer ,address ,pointer)
;     (%p-store-data-type ,address dtp-one-q-forward)))

;uses new macro instruction.
(defmacro one-q-forwardify (address pointer)
  `(compiler::%p-store-data-type-and-pointer ,address dtp-one-q-forward ,pointer))

(defmacro %locative-plus (pointer offset)
  `(%make-pointer-offset dtp-locative ,pointer ,offset))

(defsubst variable-globally-special-p (variable)
  (proclaimed-special-p variable))
;  ;; VARIABLE is assumed to be a symbol.
;  (getl variable variable-special-properties))

;;; NOTE: it is vital that every link of *INTERPRETER-VARIABLE-ENVIRONMENT*,
;;; *INTERPRETER-FRAME-ENVIRONMENT* and of *INTERPRETER-FUNCTION-ENVIRONMENT*
;;; be a full two-word pair.

(defvar-resettable *interpreter-variable-environment* nil nil
  "The current lexical environment for evaluation.
The value is a list of environment frames, each of which looks like
 (CELL VALUE CELL VALUE ...)
Each CELL is a locative (usually to a value cell),
and the following VALUE is the lexical value for that cell.
For a special binding, the VALUE is actually a DTP-ONE-Q-FORWARD to the cell.

Each place where a group of variables is bound (each LET, LAMBDA, PROG,...)
makes its own environment frame.

The tail of the list can be T rather than NIL.  This means
that all variables should be considered special if not found
in the entries in the environment.")

;;; This is a separate variable because usually will be NIL
;;; and that way the search for functions is not slowed down by local variables.
(defvar-resettable *interpreter-function-environment* nil nil
  "Like SI::*INTERPRETER-VARIABLE-ENVIRONMENT* but contains lexical functions
rather than variables.")

(defvar-resettable *interpreter-frame-environment* nil nil
  "Like SI::*INTERPRETER-VARIABLE-ENVIRONMENT* but contains stuff for TAGBODY and BLOCK, etc.
Each frame starts with the symbol BLOCK or TAGBODY;
the second element is a list of further data:
 for a BLOCK: (NAME CATCH-TAG-ADDR-ON-STACK)
 for a TAGBODY: (BODY CATCH-TAG-ADDR-ON-STACK)
SI::UNSTACKIFY-ENVIRONMENT needs to recognize these special kinds
of frames; if you add more, you may need to change it.")

(defvar-resettable *interpreter-macrocache* nil nil)

(defstruct (interpreter-environment (:type :list) (:conc-name "INTERPRETER-ENVIRONMENT-")
                                    (:alterant nil))
  (functions nil :documentation "Stuff established by FLET and MACROLET, etc.")
  (declarations nil :documentation "Not used in the interpreter.
This slot is here for consistency with (forseen) compiler environment structures.")
  (variables nil :documentation "Stuff established by LAMBDA and LET, etc.")
  (frames nil :documentation "Stuff established by BLOCK and TAGBODY, etc.")
  (macrocache nil :documentation "Cached stuff.")
  ;; anything else?
  )

(defmacro binding-interpreter-environment ((environment) &body body)
  "Execute BODY with the interpreter's environment initialized by ENVIRONMENT."
  (once-only (environment)
    `(let ((*interpreter-variable-environment*
             (interpreter-environment-variables ,environment))
           (*interpreter-function-environment*
             (interpreter-environment-functions ,environment))
           (*interpreter-frame-environment*
             (interpreter-environment-frames ,environment))
           (*interpreter-macrocache*
             (interpreter-environment-macrocache ,environment)))
       . ,body)))

(defmacro with-current-interpreter-environment
          ((var &optional (vars-env '*interpreter-variable-environment*))
           &body body)
  "Execute BODY with VAR bound to an environment object constructed from the current
interpreter environment."
  `(with-stack-list (,var *interpreter-function-environment*
                          nil                   ;declarations
                          ,vars-env
                          *interpreter-frame-environment*
                          *interpreter-macrocache*)
     . ,body))


(defconstant lambda-parameters-limit 60.
  "Functions accepting less than this many arguments are allowed.")

(defconstant call-arguments-limit 60.
  "Passing fewer than this many arguments in a function call is guaranteed to be ok.
Note that elements of a &rest arg that is never actually spread
do not count in this limit.")

(defconstant multiple-values-limit 60.
  "Ostensible upper bound on number of values a function call can return.
In fact, this is not what is limited, and you can get away with three times
as many if you don't fill up the maximum stack frame size in other ways.")

(defconstant lambda-list-keywords
             '(&optional &rest &key &allow-other-keys &aux
               &environment &body &whole
               ;; Losers, soon to be no more
               &special &local &functional &eval &quote &list-of)
  "List of all &-keywords that have special meanings in argument lists of functions.")

(defvar-resettable *evalhook* nil nil
  "Value is function used on calls to EVAL, inside calls to EVALHOOK.")
(defvar evalhook :unbound
  "Value is function used on calls to EVAL, inside calls to EVALHOOK.")
(forward-value-cell 'evalhook '*evalhook*)

(defvar-resettable *applyhook* nil nil
  "Value is function used on applications performed by EVAL, inside calls to EVALHOOK.
The function receives two arguments, like those which APPLY would receive.")
(defvar applyhook :unbound
  "Value is function used on applications performed by EVAL, inside calls to EVALHOOK.
The function receives two arguments, like those which APPLY would receive.")
(forward-value-cell 'applyhook '*applyhook*)


;;;; Basic primitives for operating on interpreter variables.

(defun require-bindable-symbol (var &optional (use "bind"))
  (cond ((not (symbolp var))
         (ferror "Attempt to ~A ~S; a symbol is required" use var))
        #|((and (> (length (symbol-name var)) 0)
         (eq (char (symbol-name var) 0) #/&)))|#
        ((memq var lambda-list-keywords)
         (ferror "Attempt to ~A the lambda-list-keyword ~S" use var))
        ((eq var 'nil)
         (ferror "Nihil ex nihil: Don't ~A ~S" use var))
        ((eq var 't)
         (ferror "Veritas aeternae: Don't ~A ~S" use var))
        ((keywordp var)
         (ferror "Attempt to ~A the keyword ~S" use var))
        (t (locf (symbol-value var)))))

(defvar-resettable *all-free-interpreter-variable-references-special* nil nil
  "T means to make all free references to variables in the interpreter
act as special references.
It is a much better idea to use the function SI:EVAL-SPECIAL-OK than to bind this.")

(defun var-not-special (symbol attempt must-be-bound-p special-function &rest additional-args)
  (declare (dbg:error-reporter))
  (if *all-free-interpreter-variable-references-special*
      (apply special-function symbol additional-args)
    (signal-proceed-case ((val) 'free-variable-reference
                                :symbol symbol
                                :attempt attempt
                                :must-be-bound-p must-be-bound-p)
      (:new-value
       val)
      (:use-dynamic-value
       (apply special-function symbol additional-args))
      (:make-special
       (proclaim-special symbol)
       (apply special-function symbol additional-args)))))

(defun interpreter-symeval (symbol)
  (declare (dbg:uninteresting-function eval))
  (do ((tail *interpreter-variable-environment* (cdr tail))
       (loc (locf (symbol-value symbol)))
       tem)
      ((atom tail)                              ;assume free references are special
       (if (or tail
               (variable-globally-special-p symbol))
           (symbol-value symbol)
         (var-not-special symbol "evaluate it" t #'symbol-value)))
    (when (setq tem (get-lexical-value-cell (car tail) loc))
      (return (contents tem)))))

(defun interpreter-set (symbol value)
  (declare (dbg:uninteresting-function))
  (do ((tail *interpreter-variable-environment* (cdr tail))
       (loc (require-bindable-symbol symbol "set"))
       tem)
      ((atom tail)                              ;assume free references are special
       (if (or tail (variable-globally-special-p symbol))
           (set symbol value)
         (var-not-special symbol "set it" nil #'set value)))
    (when (setq tem (get-lexical-value-cell (car tail) loc))
      (return (setf (contents tem) value)))))

(defun interpreter-value-cell-location (symbol)
  (declare (dbg:uninteresting-function))
  (do ((tail *interpreter-variable-environment* (cdr tail))
       (loc (locf (symbol-value symbol)))
       tem)
      ((atom tail)                              ;assume free references are special
       (if (or tail (variable-globally-special-p symbol))
           (%external-value-cell symbol)
         (var-not-special symbol "find its value cell" nil #'value-cell-location)))
    (when (setq tem (get-lexical-value-cell (car tail) loc))
      (return (follow-cell-forwarding tem t)))))

(defun interpreter-fsymeval (symbol)
  (let ((loc (locf (symbol-function symbol)))
        tem)
    (dolist (frame *interpreter-function-environment* (symbol-function symbol))
      (when (setq tem (get-location-or-nil (locf frame) loc))
        (return (contents tem))))))


(defun interpreter-fsymeval1 (symbol)
  (let ((loc (locf (symbol-function symbol)))
        tem)
    (dolist (frame *interpreter-function-environment* (OR (INTERPRETER-SPECIAL-FORM SYMBOL)
                                                          (SYMBOL-FUNCTION SYMBOL)))
      (when (setq tem (get-location-or-nil (locf frame) loc))
        (return (contents tem))))))

;;; T if there was a special declaration made in the current construct.
;;; FRAME-INTERPRETER-ENVIRONMENT should be specified as the NEWENV-VAR
;;; of the GOBBLE-DECLARATIONS-FROM-BODY done in that construct.
;;; Or it should be NIL if this construct doesn't process declarations,
;;; though in general a construct that binds variables ought to allow declarations!

;;; This one is unusual, as it is passed a locative to a cell rather than a symbol.
;;; It is interfaced this way due to the way the code works out in PARALLEL-BINDING-LIST.
(defsubst interpreter-variable-special-in-frame-p (cell frame)
  (let ((tem (get-lexical-value-cell (car frame) cell)))
    (if tem
        (= (%p-data-type tem) dtp-one-q-forward)
      (cadr (getl (%find-structure-header cell) '(special system-constant))))))


(defun constantp (form)
  "T if FORM always evaluates to the same thing.
This includes keyword symbols, and lists starting with QUOTE."
  (cond ((consp form)
         (eq (car form) 'quote))
        ((symbolp form)
         (or (eq form 'nil) (eq form 't)
             (keywordp form)
             (get form 'system-constant)))
        (t t)))

;;; Same as constantp, except excludes defconstants...
;;; pleblisp doesn't specify this, but it's sure as hell useful...
(defun self-evaluating-p (form)
  "T if FORM always evaluates to itself.
Like CONSTANTP, but excludes DEFCONSTANTs."
  (cond ((consp form)
         (eq (car form) 'quote))
        ((symbolp form)
         (or (eq form 'nil) (eq form 't)
             (keywordp form)))
        (t t)))

;;;; Processing of local declarations in special forms.

;;; When (LET () (DECLARE (SPECIAL A)) ...) is seen,
;;; it is necessary to push a binding frame onto *INTERPRETER-VARIABLE-ENVIRONMENT*
;;; containing a binding for A to mark A as special.
;;; This binding contains as its value
;;; a one-q-forward to the special value cell of A.

;;; GOBBLE-DECLARATIONS-FROM-BODY is the form which is used to accomplish this.
;;; (GOBBLE-DECLARATIONS-FROM-BODY (vars-env-var body-exp)
;;;   (EVAL-BODY body-exp))
;;; causes the appropriate binding frame to be pushed
;;; for declarations at the front of body-exp's value
;;; before the EVAL-BODY is done.

;;; Forms such as LET which do variable binding
;;; must process the declarations FIRST so they know which vars are special.
;;; Also, these forms should note that the vars-env-var
;;; is bound to a list whose car is a frame that describes any SPECIAL declarations found.
;;; The vars-env-var's value should be passed to INTERPRETER-VARIABLE-SPECIAL-IN-FRAME-P
;;; in order to decide whether a binding done in this frame should be special.
;;; All macros for binding variables for Common Lisp (SERIAL-BINDING-LIST, etc.)
;;; expect the vars-env-var as an argument.

;;; UNSPECIAL declarations also work!

(defvar *interpreter-declaration-type-alist*
  '(;; declarations actually used by the interpreter
    ;;  --- done specially in GOBBLE-DECLARATIONS-INTERNAL
;   (SPECIAL special//unspecial-interpreter-declaration)
;   (UNSPECIAL special//unspecial-interpreter-declaration)

    ;; lispm declarations
    (:SELF-FLAVOR self-flavor-interpreter-declaration)
    (downward-function ignore)
    (downward-funarg ignore)

    ;; type declarations -- ignored
    (TYPE ignore)
    (ARRAY ignore)
    (ATOM ignore)
    (BIGNUM ignore)
    (BIT ignore)
    (BIT-VECTOR ignore)
    (CL:CHARACTER ignore)
    (CHARACTER ignore)
    (COMMON ignore)
    (COMPILED-FUNCTION ignore)
    (COMPLEX ignore)
    (CONS ignore)
    (DOUBLE-FLOAT ignore)
    (FIXNUM ignore)
    (FLOAT ignore)
    (FUNCTION ignore)
    (HASH-TABLE ignore)
    (INTEGER ignore)
    (KEYWORD ignore)
    (LIST ignore)
    (LONG-FLOAT ignore)
    (NIL ignore)
    (NULL ignore)
    (NUMBER ignore)
    (PACKAGE ignore)
    (PATHNAME ignore)
    (RANDOM-STATE ignore)
    (RATIO ignore)
    (RATIONAL ignore)
    (READTABLE ignore)
    (SEQUENCE ignore)
    (SHORT-FLOAT ignore)
    (SIMPLE-ARRAY ignore)
    (SIMPLE-BIT-VECTOR ignore)
    (SIMPLE-STRING ignore)
    (SIMPLE-VECTOR ignore)
    (SINGLE-FLOAT ignore)
    (STANDARD-CHAR ignore)
    (STREAM ignore)
    (STRING ignore)
    (STRING-CHAR ignore)
    (SYMBOL ignore)
    (T ignore)
    (VECTOR ignore)

    (FTYPE ignore)
    (FUNCTION ignore)

    ;; can these mean anything to the interpreter?
    (INLINE ignore)
    (NOTINLINE ignore)
    (IGNORE ignore)
    (OPTIMIZE ignore)

    (DECLARATION define-declaration)
    (DOCUMENTATION ignore)

    ;; anything else with a si::debug-info property is just ignored
    )
  "Alist of elements (decl-type interpreter-handler-function)
decl-type is a symbol such as SPECIAL or TYPE.
The handler-function is called with the declaration and the current interpreter environment
as args.")

(defun self-flavor-interpreter-declaration (decl ignore)
  (unless (typep self (cadr decl))
    (cerror "Ignore the ~S declaration (and hope for the best)"
            "~S is declared to be ~S,~% but ~S is of type ~S"
            'self-flavor (cadr decl) 'self (type-of self))))

;;;>> not right since this defines a declaration globally, rather than
;;; just within  the scope of this declaration.
;;;  Of course, anybody who is declaring declarations locally is losing
;;; completely and pretty weirdly!
(defun define-declaration (declaration ignore)
  (dolist (decl (cdr declaration))
    (push `(,decl ignore) *interpreter-declaration-type-alist*)))

(defun proclaim (&rest declarations &aux d)
  "Make DECLARATIONS be in effect globally.
Only SPECIAL declarations make sense to do this way,
and they are better made using DEFVAR or DEFPARAMETER."
  (dolist (decl declarations)
    (if (or (atom decl) (not (atom (setq d (car decl)))))
        (ferror "~S is an invalid declaration" decl)
      (case d
        (special
         (dolist (x (cdr decl))
           ;; Gag me with type check!!
;          ;; getf of symbol-plist is for the symbolp check inherent in symbol-plist
;          (setf (getf (symbol-plist x) 'special) t))
           (check-type x symbol)
           (proclaim-special x)))
        (unspecial
         (dolist (x (cdr decl))
           (check-type x symbol)
           (proclaim-unspecial x)))
        (inline
         )
        (notinline                              ;Bug: CLtL insists that NOTINLINE may not be ignored!
         )
        (declaration
         (dolist (x (cdr decl))
           (pushnew `(,x ignore) *interpreter-declaration-type-alist*
                    :test #'eq :key #'car)))
        (t
         (cond ((get d 'debug-info))
               ((assq d *interpreter-declaration-type-alist*))
               (t (cerror "Ignore it" "~S is an unknown declaration" decl)))
         ;; else do nothing...
         ))))
  nil)

(defmacro gobble-declarations-from-body ((vars-env-var caller-body-exp) &body macro-body)
  `(with-stack-list* (,vars-env-var nil *interpreter-variable-environment*)
     ;;>> BUG!! This must macroexpand to see whether macro expands into declaration.
     ;;>>  See wimplementation issues on common-lisp@sail
     (when (eq (caar-safe ,caller-body-exp) 'declare)
       (%bind (locf (symbol-value '*interpreter-variable-environment*)) ,vars-env-var)
       (gobble-declarations-internal ,caller-body-exp ,vars-env-var))
     . ,macro-body))

;;; This is called from expansions of the preceding macro.
;;; *INTERPRETER-VARIABLE-ENVIRONMENT* has already been rebound
;;; but this function actually puts the declaration info into their values.
;;; BODY is the body of the special form that the user is evaluating,
;;; at the front of which appear the declarations if any.
(defun gobble-declarations-internal (body vars-env &aux tem)
  (dolist (bodyelt body)
    ;;>> BUG!! This must macroexpand to see whether macro expands into declaration.
    ;;>>  See wimplementation issues on common-lisp@sail
    (unless (eq (car-safe bodyelt) 'declare)
      (return nil))
    (dolist (decl (cdr bodyelt))
      (cond ((memq (car decl) '(special unspecial))
             ;; these are the important ones.
             (dolist (var (cdr decl))
               ;; ** CONS **
               (setf (car vars-env) (list* (locf (symbol-value var)) nil (car vars-env)))
               (when (eq (car decl) 'special)
                 (one-q-forwardify (locf (cadr (car vars-env)))
                                   (locf (symbol-value var))))))
            ((get (car decl) 'debug-info)
             nil)
            ((setq tem (or (assq (car decl) *interpreter-declaration-type-alist*)
                           ;; gratuitous.  Until ftype, etc globalized...
                           (assq (intern-soft (car decl) (symbol-package 'foo))
                                 *interpreter-declaration-type-alist*)))
             (with-current-interpreter-environment (env vars-env)
               (funcall (cadr tem) decl env)))
            (t (cerror "Proceeds, ignoring the declaration"
                       "The interpreter encountered the unknown declaration ~S"
                       decl))))))


;;; The standard externally called forms of EVAL are here.
;;; They handle all kinds of atoms themselves,
;;; to save the extra function call.
;;; They use EVAL1 to handle combinations.

(defun eval (form &optional nohook)
  "Evaluate FORM in the global environment, returning its value(s).
Free variables in FORM must be special.
If there is an *EVALHOOK*, it is invoked to do the work, unless NOHOOK is true."
  (binding-interpreter-environment (())
    (cond ((and *evalhook* (not nohook))
           (let ((tem *evalhook*)
                 (*evalhook* nil)
                 (*applyhook* nil))
             (with-current-interpreter-environment (env)
               (funcall tem form env))))
          ((symbolp form)
           (if (or (keywordp form) (variable-globally-special-p form))
               (symbol-value form)
             (var-not-special form "evaluate it" t #'symbol-value)))
          ((atom form) form)
          (t
           (eval1 form)))))

(defparameter specials-ok-environment nil)
(defun eval-special-ok (form &optional nohook)
  "Evaluate FORM in the global environment, allowing free variables, returning its value(s).
If there is an *EVALHOOK*, it is invoked to do the work, unless NOHOOK is true."
  (unless specials-ok-environment
    (setq specials-ok-environment (make-interpreter-environment :variables t)))
  (binding-interpreter-environment (specials-ok-environment)
    (cond ((and *evalhook* (not nohook))
           (let ((tem *evalhook*)
                 (*evalhook* nil)
                 (*applyhook* nil))
             (with-current-interpreter-environment (env)
               (funcall tem form env))))
          ((symbolp form)
           (symbol-value form))
          ((atom form) form)
          (t
           (eval1 form)))))

(defparameter old-dynamic-environment nil)
(defun old-dynamic-eval (form &optional nohook)
  "Evaluate FORM using the old-style dynamic evaluator
/(ie all variables and functions bound as specials,
 and free reference to non-special variables allowed)
This is a kludge: you should not be using this function for anything but to bootstrap old
code to make it work with the new winning interpreter.

This function will not be supported indefinitely: please update your code to reflect
the /"New Order/"."
  (unless old-dynamic-environment
    (setq old-dynamic-environment (make-interpreter-environment :functions t :variables t)))
  (binding-interpreter-environment (old-dynamic-environment)
    (cond ((and *evalhook* (not nohook))
           (let ((tem *evalhook*)
                 (*evalhook* nil)
                 (*applyhook* nil))
             (with-current-interpreter-environment (env)
               (funcall tem form env))))
          ((symbolp form)
           (symbol-value form))
          ((atom form) form)
          (t
           (eval1 form)))))

(defun eval-abort-trivial-errors (top-level-form)
  "Evaluate TOP-LEVEL-FORM, returning the value, but aborting on trivial errors.
A trivial error is one involving a symbol present in the form itself.
Aborting is done by signaling SYS:ABORT, like the Abort key.
The user gets to choose whether to do that or to enter the debugger as usual.
Uses SI:EVAL-SPECIAL-OK, so will not err on free variable references."
  (condition-bind (((sys:too-few-arguments sys:too-many-arguments
                     sys:cell-contents-error sys:wrong-type-argument
                     ;; can't do EH:WRONG-TYPE-VALUE without DBG:WITH-ERRING-FRAME
                     ;;  Bitch at Mly
                     sys:invalid-function-spec sys:unclaimed-message)
                    'eval-abort-trivial-errors-handler))
    ;; Eval, making all free variable references special
    (let ((*top-level-form* top-level-form))
      (declare (special *top-level-form*))
      (eval-special-ok top-level-form))))

(defun eval-abort-trivial-errors-handler (condition)
  (declare (special *top-level-form*))
  (when (cond ((condition-typep condition 'sys:cell-contents-error)
               (and (symbolp (send condition :containing-structure))
                    (mem*q-fwd (send condition :containing-structure) *top-level-form*)))
              ((condition-typep condition 'sys:invalid-function-spec)
               (mem*q (send condition :function-spec) *top-level-form*))
              ((condition-typep condition 'sys:unclaimed-message)
               (mem*q (send condition :message) *top-level-form*))
              ((condition-typep condition 'eh:wrong-type-value)
               (mem*q (send condition :place) *top-level-form*))
              ;; Older CHECK-TYPE uses this
              ((condition-typep condition 'wrong-type-argument)
               (mem*q (send condition :function) *top-level-form*))
              (t ; too-many/few-arguments
               (mem*q (function-name (send condition :function)) *top-level-form*)))
    (send *query-io* :fresh-line)
    (send condition :print-error-message current-stack-group t *query-io*)
    (send *query-io* :clear-input)
    (let ((*evalhook* nil)
          (*applyhook* nil))
      (unless (fquery `(:choices
                         ,(mapcar (lambda (choice)
                                    (if (eq (caar choice) nil)
                                        (append choice '(#/c-Z))
                                      choice))
                                  format:y-or-n-p-choices))
                      "Enter the debugger (No means abort instead)? ")
        (signal-condition eh:*abort-object*))))
  (values))

(defun mem*q-fwd (elt tree)
  "T if ELT is TREE or an element of TREE or an element of an element, etc.
Does not compare the CDRs (the links of the lists of TREE), just the elements.
Regards two symbols as equal if their value cells are forwarded together."
  ;; Cannot use MEMQ since it gets an error if a list ends in a non-NIL atom.
  (or (eq elt tree)
      (and (symbolp tree)
           (symbolp elt)
           (eq (follow-cell-forwarding (locf (symbol-value elt)) t)
               (follow-cell-forwarding (locf (symbol-value tree)) t)))
      (do ((tail tree (cdr tail)))
          ((atom tail) nil)
        (if (or (eq (car tail) elt)
                (mem*q-fwd elt (car tail)))
            (return t)))))

(defun mem*q (elt tree)
  "T if ELT is TREE or an element of TREE or an element of an element, etc.
Does not compare the CDRs (the links of the lists of TREE), just the elements."
  ;; Cannot use MEMQ since it gets an error if a list ends in a non-NIL atom.
  (or (eq elt tree)
      (do ((tail tree (cdr tail)))
          ((atom tail) nil)
        (if (or (eq (car tail) elt)
                (mem*q elt (car tail)))
            (return t)))))


(defun evalhook (form *evalhook* *applyhook* &optional environment)
  "Evaluate FORM, using specified *EVALHOOK* and *APPLYHOOK* except at the top level.
ENVIRONMENT is the lexical environment to eval in.
Or use the environment argument passed to an EVALHOOK function."
  (binding-interpreter-environment (environment)
    (eval1 form t)))

(defun applyhook (function args *evalhook* *applyhook* &optional environment)
  "Apply FUNCTION to ARGS, using specified *EVALHOOK* and *APPLYHOOK* except at the top level.
ENVIRONMENT is the lexical environment to eval in.
 Or use the environment argument passed to an EVALHOOK function."
  (if (typep function '(or cons closure))
      (apply-lambda function args environment)
    (apply function args)))

;;; This is the real guts of eval.  It uses the current lexical context.
;;; If that context includes *INTERPRETER-FUNCTION-ENVIRONMENT* = T,
;;; then Zetalisp evaluation is done.
;;; All special forms call EVAL1 directly to eval their arguments.

(defun eval1 (form &optional nohook)
  "Evaluate FORM in the current lexical environment, returning its value(s).
If the current environment says /"traditional Zetalisp/", we do that.
This is the function that special forms such as COND use to evaluate
their subexpressions, as it allows the subexpressions to access
lexical variables of the containing code.  Contrast with EVAL."
  (declare (dbg:uninteresting-function eval))
  ;; Make sure all instances of ARGNUM, below, are local slot 0.
  (let (argnum) argnum)
  (with-current-interpreter-environment (env)
    (cond ((and *evalhook* (not nohook))
           (let ((tem *evalhook*)
                 (*evalhook* nil)
                 (*applyhook* nil))
             (funcall tem form env)))
          ((symbolp form)
           (cond ((keywordp form)
                  form)
                 ((eq *interpreter-function-environment* t)
                  (symbol-value form))
                 (t
                  (interpreter-symeval form))))
          ((atom form) form)
          (t
           (let* ((final-function (car form))
                  call-function
                  arg-desc num-args tem)
              ;; Trace FINAL-FUNCTION through symbols and closures to get the ultimate function
              ;; which will tell us whether to evaluate the args.
              (tagbody
               loop
                  (typecase final-function
                    (symbol
                     (setq final-function
                           (COND ((EQ final-function '*CATCH)
                                  'CATCH)
                                 ((eq *interpreter-function-environment* t)
                                  (OR (INTERPRETER-SPECIAL-FORM FINAL-FUNCTION)
                                      (symbol-function final-function)))
                                 ('ELSE
                                  (interpreter-fsymeval1 final-function))))
                     (go loop))
                    ((or closure entity)
                     (setq tem (%make-pointer dtp-list final-function))
                     (or call-function (setq call-function final-function))
                     (setq final-function (car tem))
                     (go loop))
                    ;;>> sigh.
                    (microcode-function
                     ;; Detect ucode entry that is not actually microcoded.
                     (and (bit-test %arg-desc-interpreted (%args-info final-function))
                          (not (integerp (aref (symbol-function 'sys:micro-code-entry-area)
                                               (%pointer final-function))))
                          (setq final-function (aref (symbol-function 'sys:micro-code-entry-area)
                                           (%pointer final-function)))
                          (go loop)))
                    (t nil)))
              (or call-function (setq call-function final-function))
              (setq arg-desc (%args-info call-function))
              (COND ((bit-test %arg-desc-interpreted arg-desc)
                     (typecase final-function
                       (cons
                        (case (car final-function)
                          ((lambda subst cl:subst)
                           (eval-lambda (cadr final-function) call-function (cdr form) env))
                          ((named-lambda named-subst)
                           (eval-lambda (caddr final-function) call-function (cdr form) env))
                          (macro (eval1 (error-restart (error "Retry macro expansion.")
                                          ;;>> UGH!!
                                          (let ((*macroexpand-environment* env))
                                            (automatic-displace (cdr call-function) form)))
                                        t))
                          ((curry-before curry-after)
                           (if *applyhook*
                               (progn (%open-call-block 'applyhook1 0 2)        ;d-return
                                      (%push env)
                                      (%push call-function))
                             (%open-call-block call-function 0 2))
                           (%assure-pdl-room (length (cdr form)))
                           (do ((argl (cdr form) (cdr argl))
                                (argnum 0 (1+ argnum)))
                               ((null argl))
                             (%push (eval1 (car argl))))
                           (%activate-open-call-block))
                          (t
                           (if (lambda-macro-call-p call-function)
                               (eval1 (cons (lambda-macro-expand call-function) (cdr form)))
                             (invalid-function (car form) (cdr form) #'eval1 nohook)))))
                       ((or select-method instance)
                        (if *applyhook*
                            (progn (%open-call-block 'applyhook1 0 2)   ;d-return
                                   (%push env)
                                   (%push call-function))
                          (%open-call-block call-function 0 2))
                        (%assure-pdl-room (length (cdr form)))
                        (do ((argl (cdr form) (cdr argl))
                             (argnum 0 (1+ argnum)))
                            ((null argl))
                          (%push (eval1 (car argl))))
                        (%activate-open-call-block))
                       (t
                        (invalid-function (car form) (cdr form) #'eval1 nohook))))
                    ((TYPEP FINAL-FUNCTION 'INTERPRETER-SPECIAL-FORM)
                     (FUNCALL (INTERPRETER-SPECIAL-FORM-HANDLER FINAL-FUNCTION)
                              FORM))
                    ((OR (bit-test %arg-desc-quoted-rest arg-desc)
                         (bit-test %arg-desc-fef-quote-hair arg-desc))
                     (FERROR NIL "Obsolete special form. Recompile the definition of ~S"
                             (CAR FORM)))
                    ('ELSE
                     (setq num-args (length (cdr form)))
                     (when (not (< num-args call-arguments-limit))
                       (FERROR NIL
                               "Too many arguments. This cant possibly work compiled:~%~S"
                               FORM))
                     (if *applyhook*
                         (progn (%open-call-block 'applyhook1 0 2)      ;d-return
                                (%push env)
                                (%push call-function))
                       (%open-call-block call-function 0 2))
                     (%assure-pdl-room num-args)
                     (dolist (arg (cdr form))
                       (%push (eval1 arg)))
                     (%activate-open-call-block))))))))

(defun eval-lambda (lambda-list call-function args-to-eval env)
  (declare (dbg:uninteresting-function eval))
  (let ((num-args 0)
        args
        pdl-args)
    ;; start of our manual list or list*
    ;; %regular-pdl-index is off by one because of dtp-list being pushed before it.
    (setq pdl-args (%make-pointer dtp-list (%regular-pdl-index)))
    (do ((ll lambda-list (cdr ll))
         (quote-status '&eval)
         rest-flag)
        ((or (null ll)
             (memq (car ll) '(&aux &key)))
         (setq num-args (length args-to-eval))
         (%assure-pdl-room num-args))
      (cond ((memq (car ll) '(&eval &quote))
             (setq quote-status (car ll)))
            ((eq (car ll) '&rest)
             (setq rest-flag t))
            ((memq (car ll) lambda-list-keywords))
            (rest-flag
             ;; Here if we encounter a rest arg.
             (if ( (length args-to-eval)
                    (if (eq quote-status '&quote)
                        num-args
                        ;; stack frames may be moby!
                        200.))
                 ;; If there aren't enough args supplied to actually
                 ;; reach it, arrange to exit via the DO's end-test.
                 (setq ll nil)
               ;; If the quoted rest arg is non-nil,
               ;; set NUM-ARGS to number of spread args,
               ;; and call with ADI.
               (%assure-pdl-room (1+ num-args))
               (return)))
            (t (incf num-args))))
    ;; Now push the args, evalling those that need it.
    (do ((ll lambda-list (cdr ll))
         (argl args-to-eval (cdr argl))
         (quote-status '&eval)
         (argnum 0 (1+ argnum))
         pdl)
        (())
      (do () ((null ll))
        (cond ((memq (car ll) '(&eval &quote))
               (setq quote-status (car ll)))
              ((memq (car ll) '(&rest &aux &key))
               (setq ll nil))
              ((memq (car ll) lambda-list-keywords))
              (t (return)))
        (pop ll))
      (cond ((= argnum num-args)
             ;; Done with spread args => push the rest arg.
             (setq pdl (%regular-pdl-index))
             (cond (argl
                    ;; push on either the quoted rest or the extra parameters beyond the
                    ;; stack-frame-size-limited number above (200.)
                    (let ((tem (if (eq quote-status '&eval)
                                   (mapcar #'eval1 argl)
                                   argl)))
                      (cond ((eq num-args 0)
                             (setq args tem)
                             (setq pdl-args nil))
                            (t
                             (%push tem)
                             ;; list*-ify to terminate the list of args
                             (%p-store-cdr-code pdl cdr-normal)
                             (%p-store-cdr-code (%locative-plus pdl 1) cdr-error)))))
                   ((eq num-args 0)
                    (setq args ())
                    (setq pdl-args nil))
                   (t
                    ;; terminate the list of args
                    (%p-store-cdr-code pdl cdr-nil)))
             (return))
            ((eq quote-status '&eval)
             (%push (eval1 (car argl))))
            (t
             (%push (car argl)))))
    (if pdl-args (setq args pdl-args))
    (if *applyhook*
        (let ((*evalhook* nil)
              (*applyhook* nil)
              (tem *applyhook*))
          (funcall tem call-function args env))
      ;;>> what a waste! We just decoded the lambda-list, and now we go and do it again...
      (apply-lambda call-function args env))))


(defun invalid-function (fn args cont &rest rest)
  (declare (dbg:error-reporter))
  (signal-proceed-case ((val) 'sys:invalid-function :function fn)
    (:new-function (apply cont val args rest))))

;;; Invoke the applyhook on a function which does not have an explicitly passed rest arg.
(defun applyhook1 (env function &rest args)
  (let ((*evalhook* nil)
        (*applyhook* nil)
        (tem *applyhook*))
    (funcall tem function args env)))

;;; Invoke the applyhook for a function with an explicitly passed rest arg.
;;; ARGS* is like the arguments to LIST*.
(defun applyhook2 (env function &rest args*)
  (let ((*evalhook* nil)
        (*applyhook* nil)
        (tem *applyhook*)
        ;; list* => list
        (args (if (cdr args*)
                  (let ((tem (last args*)))     ;always stack-consed (I hope!!)
                    (%p-store-cdr-code (%locative-plus tem -1) cdr-normal)
                    args*)
                (car args*))))
    (funcall tem function args env)))

;;; Bind variables, given a list of variables and separate list of (already evaluated) values.
;;; This is needed for MULTIPLE-VALUE-BIND, which appears below.  It assumes that NIL
;;; is a kosher thing to want to bind, and ignores the value  supplied in that case.
;;; This makes the useful idiom (multiple-value-bind (a nil nil b) ...) work without having to
;;; bother with (declare (ignore ...)) lossage

;;; It does not work to have a CATCH around an invocation of this macro.
;;; It works properly only when compiled to exit to D-RETURN.
;;; Otherwise, it leave the stack screwed up due to the unknown number of %PUSHes executed.

(defmacro bind-variables-spread ((varlist value-list-exp vars-env) &body body)
  `(prog (vars-left bindframe vals-left thisvarloc)
         ;; Trivial case of empty varlist would lose in code below.
         (cond ((null ,varlist)
                (go trivial))
               ((nthcdr 16. ,varlist)
                (setq bindframe (make-list (* 2 (length ,varlist)) :initial-element nil)
                      vals-left bindframe
                      vars-left ,varlist)
                (do ((v ,value-list-exp))
                    ((null vars-left))
                  (if (eq (car vars-left) 'nil)
                      nil
                    (setq thisvarloc (require-bindable-symbol (car vars-left)))
                    (setf (car vals-left) thisvarloc
                          (cadr vals-left) (car v))
                    (setq vals-left (cddr vals-left)))
                  (pop vars-left)
                  (pop v))
                (go long)))
         ;; The following code is equivalent to the above mapcar
         ;; except that the list is constructed on the stack
         ;; by pushing the elements one by one and fiddling with cdr codes.
         ;; BINDFRAME gets a pointer to where the list will go.
         ;;>> (multiple-value-bind (a a) (values 1 2))
         ;;>>  is so ambiguous that I'm not going to expend the effort to use the last value.
         ;;>>  (see serial-binding-list for how to do that)
         ;; %regular-pdl-index is off by one because of dtp-list being pushed before it.
         (setf bindframe (%make-pointer dtp-list (%regular-pdl-index)))
         (setq vars-left ,varlist)
         (setq vals-left ,value-list-exp)
      short-nextvar
         (when vars-left
           (if (eq (car vars-left) 'nil)
               ;; allow (multiple-value-bind (foo nil bar) ...)
               nil
             (setq thisvarloc (require-bindable-symbol (car vars-left)))
             (%push thisvarloc)
             (%push (car vals-left)))
           (pop vars-left)
           (pop vals-left)
           (go short-nextvar))
         (%p-store-cdr-code (%regular-pdl-index) cdr-nil)
      long
         ;; Here BINDFRAME has the correct variables and values.
         ;; Now for each variable that is supposed to be special
         ;; bind it to its value (as found in BINDFRAME)
         ;; and forward the BINDFRAME slot to the variable's value cell.
         (setq vals-left bindframe)
      bindloop
         (when vals-left
           (when (setq thisvarloc (car vals-left))
             (when (interpreter-variable-special-in-frame-p thisvarloc ,vars-env)
               (%bind thisvarloc (cadr vals-left))
               (one-q-forwardify (locf (cadr vals-left)) thisvarloc)))
           (setq vals-left (cddr vals-left))
           (go bindloop))
      trivial
         (return
           (with-stack-list* (*interpreter-variable-environment*
                               bindframe *interpreter-variable-environment*)
             . ,body))))

(defmacro zl-bind-variables-spread ((varlist value-list-exp) &body body)
  `(prog (vars-left vals-left)
         ;; Now loop over the varlist, computing and pushing initial values.
         (setq vars-left ,varlist)
         (setq vals-left ,value-list-exp)
      short-nextvar
         (when vars-left
           (if (eq (car vars-left) 'nil)
               ;; allow (multiple-value-bind (foo nil bar) ...)
               nil
             (%bind (locf (symbol-value (car vars-left)))
                    (car vals-left)))
           (pop vars-left)
           (pop vals-left)
           (go short-nextvar))
         (return (progn . ,body))))

;;; Produce code to bind a single variable in a special form.
;;; VARIABLE-EXP should be an expression that computes the variable (a symbol)
;;; and VALUE-EXP should compute the value for the variable (NOT code to compute the value).
(defmacro bind-variable ((variable-exp value-exp form-body) &body body)
 `(if (eq *interpreter-function-environment* t)
      (progn
        (%bind (locf (symbol-value ,variable-exp)) ,value-exp)
        . ,body)
    (gobble-declarations-from-body (vars-env ,form-body)
      (bind-variable-1 (,variable-exp ,value-exp vars-env)
        . ,body))))

(defmacro bind-variable-1 ((variable-exp value-exp vars-env) &body body)
  `(with-stack-list (frame (locf (symbol-value ,variable-exp)) ,value-exp)
     (when (interpreter-variable-special-in-frame-p (car frame) ,vars-env)
       (%bind (car frame) (cadr frame))
       (one-q-forwardify (locf (cadr frame)) (car frame)))
     (with-list* (*interpreter-variable-environment*
                         frame *interpreter-variable-environment*)
       . ,body)))



;;;; Basic variable binding primitives.

;;; The following two macros implement binding variables according to a LET binding list,
;;; either in parallel or sequentially.

;;; It does not work to have a CATCH (such as ENTER-BLOCK)
;;; around an invocation of these macros.
;;; They work properly only when compiled to exit to D-RETURN.
;;; Otherwise, they leave the stack screwed up due to the unknown number of %PUSHes executed.
;;; (The compiler changes in system 98 will eliminate this problem).

(defmacro serial-binding-list ((varlist vars-env) &body body)
  `(with-stack-list* (*interpreter-variable-environment*
                       nil *interpreter-variable-environment*)
     (prog (vars-left vals-left thisvarloc thisval pdl bindframe)
           (cond ((null ,varlist)
                  (go varsdone))
                 ((nthcdr 16. ,varlist)
                  (go long)))
           ;; Here if varlist is less than 16. long.
           ;; Construct bindframe on the stack
           ;; by pushing the elements one by one and fiddling with cdr codes.
           ;; Now loop over the varlist, computing and pushing initial values.
           (setq vars-left ,varlist)
        short-nextvar
           (when vars-left
             (setq thisvarloc (require-bindable-symbol
                                (if (symbolp (car vars-left)) (car vars-left) (caar vars-left)))
                   thisval (if (consp (car vars-left)) (eval1 (cadar vars-left)) nil))
             ;; If multiply binding the same symbol, only leave the latest version in the stack.
             (if (setq pdl (get-lexical-value-cell bindframe thisvarloc))
                 ;; we don't need to hack special-in-frame-p since the binding and
                 ;; one-q-forward to the real value cell were made on the first pass though.
                 (setf (contents pdl) thisval)
               (if bindframe
                   ;; Env may have been copied out by enclosed lambda consing in the above eval1
                   ;; However in this case it is not closed over the following cruft, and so a
                   ;; %p-store-cdr-code, which frobs the stack-list part of the env and not the
                   ;; copied-out part, is completely the right thing.
                   ;;  (the stack part of the env is dtp-one-q-forwarded when copied)
                   ;; Bang my cdr-codes, d|i|g|i|t|a|l Man!
                   (%p-store-cdr-code (%regular-pdl-index) cdr-next)
                 ;; %regular-pdl-index is off by one because of dtp-list being pushed before it.
                 (setf bindframe (%make-pointer dtp-list (%regular-pdl-index))))
               ;; must do this every time around as car of env might get forwarded by unstackification.
               (setf (car *interpreter-variable-environment*) bindframe)
               (%push thisvarloc)
               (%push thisval)
               ;; Hack cdr-codes to extend the bindframe.
               (setq pdl (%regular-pdl-index))
               (%p-store-cdr-code pdl cdr-nil)
               ;; Bind the variable as special, if appropriate.
               (when (interpreter-variable-special-in-frame-p
                       thisvarloc ,vars-env)
                 (%bind thisvarloc thisval)
                 (one-q-forwardify pdl thisvarloc)))
             (pop vars-left)
             (go short-nextvar))
           (go varsdone)
        long
           ;; Now loop over the varlist, computing and pushing initial values.
           (setf vars-left ,varlist
                 vals-left (make-list (* 2 (length ,varlist)) :initial-element nil)
                 bindframe vals-left
                 (car *interpreter-variable-environment*) vals-left)
        long-nextvar
           (when vars-left
             (setq thisvarloc (require-bindable-symbol
                                (if (symbolp (car vars-left)) (car vars-left) (caar vars-left)))
                   thisval (if (consp (car vars-left)) (eval1 (cadar vars-left)) nil))
             (if (setq pdl (get-lexical-value-cell bindframe thisvarloc))
                 (setf (contents pdl) thisval)
               (setf (car vals-left) thisvarloc)
               (setf (cadr vals-left) thisval)
               ;; Bind the variable as special, if appropriate.
               (when (interpreter-variable-special-in-frame-p
                       thisvarloc ,vars-env)
                 (%bind thisvarloc (cadr vals-left))
                 (one-q-forwardify (locf (cadr vals-left)) thisvarloc))
               (setq vals-left (cddr vals-left)))
             (pop vars-left)
             (go long-nextvar))
           (go varsdone)
        varsdone
           (return
             (progn . ,body)))))

(defmacro parallel-binding-list ((varlist vars-env) &body body)
  `(prog (vars-left bindframe vals-left thisvarloc thisval)
         (cond ((null ,varlist)
                (go bindloop))
               ((nthcdr 16. ,varlist)
                (setq bindframe (make-list (* 2 (length ,varlist)) :initial-element nil)
                      vals-left bindframe
                      vars-left ,varlist)
                (do (var)
                    ((null vars-left))
                  (setq var (pop vars-left))
                  (setq thisvarloc (require-bindable-symbol (if (consp var) (car var) var))
                        thisval (if (consp var) (eval1 (cadr var))))
                  (let ((tem (get-lexical-value-cell bindframe thisvarloc)))
                    (if tem
                        (setf (contents tem) thisval)
                      (setf (car vals-left) thisvarloc
                            (cadr vals-left) thisval)
                      (setq vals-left (cddr vals-left)))))
                (go long)))
         ;; The following code is equivalent to the above mapcar
         ;; except that the list is constructed on the stack
         ;; by pushing the elements one by one and fiddling with cdr codes.
         ;; BINDFRAME gets a pointer to where the list will go.
         ;; Now loop over the varlist, computing and pushing initial values.
         (setq vars-left ,varlist)
      short-nextvar
         (when vars-left
           (setq thisvarloc (require-bindable-symbol
                              (if (symbolp (car vars-left)) (car vars-left) (caar vars-left)))
                 thisval (if (consp (car vars-left)) (eval1 (cadar vars-left))))
           ;; If multiply binding the same symbol, only leave the latest version in the stack.
           (if (setq vals-left (get-lexical-value-cell bindframe thisvarloc))
               (setf (contents vals-left) thisval)
             (if bindframe
                 (%p-store-cdr-code (%regular-pdl-index) cdr-next)
               ;; %regular-pdl-index is off by one because of dtp-list being pushed before it.
               (setf bindframe (%make-pointer dtp-list (%regular-pdl-index))))
             (%push thisvarloc)
             (%push thisval)
             (%p-store-cdr-code (%regular-pdl-index) cdr-nil))
           (pop vars-left)
           (go short-nextvar))
         ;; Modify cdr-code of last word pushed, to terminate the list.
         (%p-store-cdr-code (%regular-pdl-index) cdr-nil)
      long
         ;; Here BINDFRAME has the correct variables and values.
         ;; Now for each variable that is supposed to be special
         ;; bind it to its value (as found in BINDFRAME)
         ;; and forward the BINDFRAME slot to the variable's value cell.
         (setq vals-left bindframe)
      bindloop
         (when vals-left
           ;; thisvarloc may be nil in the case of long number of variables and duplicate
           ;; variable names.
           (when (setq thisvarloc (car vals-left))
             (when (interpreter-variable-special-in-frame-p thisvarloc ,vars-env)
               (%bind thisvarloc (cadr vals-left))
               (one-q-forwardify (locf (cadr vals-left)) thisvarloc)))
           (setq vals-left (cddr vals-left))
           (go bindloop))
         (return
           (with-stack-list* (*interpreter-variable-environment*
                               bindframe *interpreter-variable-environment*)
             . ,body))))


(defmacro zl-serial-binding-list ((varlist) &body body)
  `(prog (vars-left)
         (setq vars-left ,varlist)
      bindloop
         (when vars-left
           (if (atom (car vars-left))
               (%bind (require-bindable-symbol (car vars-left))
                      nil)
               (%bind (require-bindable-symbol (caar vars-left))
                      (eval1 (cadar vars-left))))
           (setq vars-left (cdr vars-left))
           (go bindloop))
         (return
           (progn . ,body))))

(defmacro zl-parallel-binding-list ((varlist) &body body)
  `(prog (vars-left)
         ;; Now bind all the prog-variables.
         ;; DO cannot be used, since the scope of the BINDs would be wrong.
         (setq vars-left ,varlist)
      bindloop
         (when vars-left
           ;; For each symbol, push 2 words on stack:
           ;; value cell location and new value.
           (cond ((atom (car vars-left))
                  (%push (require-bindable-symbol (car vars-left)))
                  (%push nil))
                 (t
                  (%push (require-bindable-symbol (caar vars-left)))
                  (%push (eval1 (cadar vars-left)))))
           (pop vars-left)
           (go bindloop))
         (setq vars-left ,varlist)
      bindloop1
         (when vars-left
           ;; Pop off next symbol and value, and bind them.
           (%bind (%pop) (%pop))
           ;; Step down VARS-LEFT just so we pop as many pairs as we pushed.
           (pop vars-left)
           (go bindloop1))
         (return (progn . ,body))))

;;;; Support for lexical function definitions (FLET and LABELS).

(defmacro function-binding-list ((varlist type) &body body)
  `(prog (vars-left bindframe)
         ;; Trivial case of empty varlist would lose in code below.
         (unless ,varlist
           (go done))
         (when (nthcdr 16. ,varlist)
           (setq bindframe
                 (mapcan (lambda (var)
                           (list* (locf (symbol-function (car var)))
                                  ,(ecase type
                                     (macrolet
                                      ``(macro . ,(with-current-interpreter-environment (env)
                                                    (expand-defmacro var env))))
                                     (flet
                                      `(interpreter-enclose `(lambda . ,(cdr var))))
                                     (labels
                                      ``(lambda . ,(cdr var))))
                                  nil))
                         ,varlist))
           (go done))
         ;; The following code is equivalent to the above mapcar
         ;; except that the list is constructed on the stack
         ;; by pushing the elements one by one and fiddling with cdr codes.
         ;; BINDFRAME gets a pointer to where the list will go.
         ;; Now loop over the varlist, computing and pushing initial values.
         (setq vars-left ,varlist)
      short-nextvar
         (when vars-left
           ;; %regular-pdl-index is off by one because of dtp-list being pushed before it.
           (or bindframe (setq bindframe (%make-pointer dtp-list (%regular-pdl-index))))
           (%push (locf (symbol-function (caar vars-left))))
           (%push ,(case type
                     (macrolet
                      ``(macro . ,(with-current-interpreter-environment (env)
                                    (expand-defmacro (car vars-left) env))))
                     (flet
                      `(interpreter-enclose `(lambda . ,(cdar vars-left))))
                     (labels
                      ``(lambda . ,(cdar vars-left)))))
           (pop vars-left)
           (go short-nextvar))
         (%p-store-cdr-code (%regular-pdl-index) cdr-nil)
      done
         ;; Here BINDFRAME has the correct variables and values.
         (return
           (with-stack-list* (*interpreter-function-environment*
                               bindframe *interpreter-function-environment*)
             . ,body))))

;(defmacro zl-function-binding-list ((varlist ignore macroflag) &body body)
;  `(prog (vars-left)
;        ;; Now bind all the prog-variables.
;        ;; DO cannot be used, since the scope of the BINDs would be wrong.
;        (setq vars-left ,varlist)
;      bindloop
;        (when vars-left
;          ;; For each symbol, push 2 words on stack:
;          ;; value cell location and new value.
;          (%push (locf (symbol-function (caar vars-left))))
;          (%push ,(if macroflag
;                     ``(macro . ,(expand-defmacro (car vars-left) nil))
;                     ``(lambda . ,(cdar vars-left))))
;          (pop vars-left)
;          (go bindloop))
;        (setq vars-left ,varlist)
;      bindloop1
;        (when vars-left
;          ;; Pop off next symbol and value, and bind them.
;          (%bind (%pop) (%pop))
;          ;; Step down VARS-LEFT just so we pop as many pairs as we pushed.
;          (pop vars-left)
;          (go bindloop1))
;        (return (progn . ,body))))



;;;; PROG, GO, RETURN, RETURN-LIST, RETURN-FROM

(defmacro enter-block (name-exp &body body)
  `(with-stack-list (tem ,name-exp nil)
     (with-stack-list (frame 'block tem)
       (with-stack-list* (*interpreter-frame-environment*
                           frame *interpreter-frame-environment*)
         (catch (cdr tem)
           ;; pointer to tag (cdr tem) in catch-frame, so that copying environment off
           ;;  stack can bash the catchtag to point at the copied environment.
           (let ((tem1 (%regular-pdl-index)))
             (setf (cadr tem) tem1))
           (unwind-protect
               (progn . ,body)
             ;;>> Make the current block no longer lexically active.
             ;; This is for the benefit of closed-over functions whose environments
             ;; were copied off the stack using unstackify-environment.
             ;; Since (cadr tem) will contain a one-q-forward to the copy of the
             ;; environment in that case, bashing it will bash the copied environment
             ;; and tell it that the frame it no longer lexically there.
             ;; Example:
             ;;  (funcall (block foo (lambda () (return-from foo))))
             ;; (Of course, it would be really nice to have winning continuations ala Scheme,
             ;;  but That Ain't In the Aluminium Edition...)
             (setf (cadr tem) nil)))))))


(defun interpreter-enclose (function)
  "Close over FUNCTION in the interpreter's current lexical environment"
  (check-type function cons)
  (macrolet ((foo (foo)
               ;; this is just to make (breakon 'unstackify-environment) interesting for debugging
               `(if (consp ,foo) (unstackify-environment ,foo) ,foo)))
    (%make-pointer dtp-closure
                   (list function
                         (make-interpreter-environment
                           :functions (foo *interpreter-function-environment*)
                           :variables (foo *interpreter-variable-environment*)
                           :frames (foo *interpreter-frame-environment*)
                           :macrocache *interpreter-macrocache*)))))

(defun interpreter-environment-closure-p (closure)
  "T if CLOSURE is a closure over the interpreter environment variables"
  (and (closurep closure)
       (consp (car (%make-pointer dtp-list closure)))
       (and (cdr (%make-pointer dtp-list closure))
            (null (cddr (%make-pointer dtp-list closure))))))

(defun stack-list-p (list)
  "T if LIST resides in the stack of the current stack group."
  (and (plusp (%pointer-difference list (sg-regular-pdl current-stack-group)))
       (plusp (%pointer-difference (%stack-frame-pointer) list))))

;;; Make sure that none of ENV lives in a stack.
;;; Copy any parts that do, forwarding the old parts to the new ones,
;;; and returning a pointer to the new one in case the first link was copied.
;;; NOTE: this function knows specially about frames made by BLOCK or TAGBODY
;;;  and copies them appropriately

(defun unstackify-environment (env &aux (newenv env))
  (when (consp env)
    (flet ((copy (stack-list)
             (let ((new (make-list (length stack-list))))
               ;; Copy each word of the old frame to the new, then
               ;; forward each word of the old frame to the new.
               ;; Uses %BLT-TYPED to copy in case what's there is a DTP-ONE-Q-FORWARD.
               (do ((l new (cdr l))
                    (m stack-list (cdr m)))
                   ((null l))
                 (%blt-typed m l 1 0)
                 (one-q-forwardify m l))
               new)))
      (when (stack-list-p env)
        (setq newenv (cons (car env) (cdr env)))
        (one-q-forwardify env newenv)
        (one-q-forwardify (%locative-plus env 1) (%locative-plus newenv 1)))
      (let ((frame (car newenv))
            newframe)
        (when (stack-list-p frame)
          (setf (car newenv) (setq newframe (copy frame)))
          ;; Special kinds of frames contain additional stack lists
          ;;  which point at words on the stack which hold catch tags.
          ;;  Copy the list and stick the appropriate copied part of the environment
          ;;  in as the catch's tag.
          ;; The catch tag is (cdr (cadr <frame>)) and the pointer to the tag is (cadr <frame>)
          ;;  If (cadr (cadr <frame>)) is null, then the frame is no longer lexically active
          ;;  and we don't need to worry
          (when (and (memq (car newframe) '(block tagbody))
                     (stack-list-p (cadr newframe)))
            (let ((tem (copy (cadr newframe))))
              (setf (cadr newframe) tem)
              (when (cadr tem)
                (setf (contents (cadr tem)) (cdr tem))
                (setf (cadr tem) (%make-pointer dtp-locative (cdr tem))))))))
      (when (consp (cdr newenv))
        (let ((newrest (unstackify-environment (cdr newenv))))
          (unless (eq (cdr newenv) newrest)
            (setf (cdr newenv) newrest))))))
  newenv)

;;; this is here rather than in sys; describe to modularize knowledge about interpreter
;;;  internals
(defun describe-interpreter-closure (closure)
  (let ((env (car (closure-bindings closure)))
        (*print-level* *describe-print-level*)
        (*print-length* *describe-print-length*))
    (format t "~%~S is an interpreter closure of ~S~%Environment is:"
            closure (closure-function closure))
    (describe-interpreter-environment *standard-output* env))
  closure)

;;>> It would be nice to be able to say whether a given environment is stack-consed
;;>> To do this, a stack-group would be passed into this function and thence to stack-list-p
(defun describe-interpreter-environment (stream env &optional venv fnenv frenv)
  (if env (setq venv (interpreter-environment-variables env)
                fnenv (interpreter-environment-functions env)
                frenv (interpreter-environment-frames env)))
  (flet ((frob-bind-frames (env name &aux special-kludge)
            (when (do ((frames env (cdr frames)))
                      ((atom frames)
                       (format stream "~&  (No ~A)" name)
                       nil)
                    (if (plusp (length frames)) (return t)))
              (format stream "~&  ~A:" name)
              (do ((frames env (cdr frames)))
                  ((atom frames))
                (loop for p on (car frames) by 'cddr
                      with kludge = nil
                      as slot-pointer = (locf (cadr p))
                      as slot-dtp = (%p-data-type slot-pointer)
                      as header = (%find-structure-header (car p))
                   do (cond ;; special
                            ((= slot-dtp dtp-one-q-forward)
                             ;; this is to get around the fact that specials occur in
                             ;;  in two successive bind-frames. Ugh!
                             (unless (memq (car p) special-kludge)
                               (push (car p) kludge)
                               ;(format stream "~%    ~S:~30T(special)" header)
                               ))
                            ;; instance variable or lexical variable
                            (t
                             (setq slot-pointer (follow-cell-forwarding slot-pointer t))
                             (format stream "~%    ~:[~;Instance var ~]~S:~30T ~:[Void~;~S~]"
                                     (= slot-dtp dtp-external-value-cell-pointer)
                                     header
                                     (location-boundp slot-pointer)
                                     (and (location-boundp slot-pointer)
                                          (contents slot-pointer)))))
                   finally (setq special-kludge kludge))
;               (format stream "~%")
                )))
          (frob-block-frames (env name type fn)
            (if (dolist (frame env t)
                  (when (eq (car frame) type) (return nil)))
                (format stream "~&  (No ~A)" name)
              (format stream "~&  ~A:" name)
              (dolist (frame env)
                (when (eq (car frame) type) (funcall fn frame))))))
    (if (eq fnenv t)
        (format stream "~& This is a old-dynamic-eval environment: ~
                        All bindings and references are special"))
    (if (or (eq venv t)
            (eq (cdr (last venv)) t))
        (format stream "~& All free variable references are special"))
    (frob-bind-frames venv "Variables")
    (unless (eq fnenv t)
      (frob-bind-frames fnenv "Functions"))
    (frob-block-frames frenv "Tagbodies" 'tagbody
                       (lambda (frame)
                         (let ((count (loop for x in (car (cadr frame))
                                         count (not (consp x)))))
                           (if (zerop count)
                               (format stream "~%    A ~S with no ~S tags" 'tagbody 'go)
                             (format stream "~%    ~S with tag~P" 'tagbody count)
                             (loop for x in (car (cadr frame))
                                   with firstp = t
                                do (when (not (consp x))
                                     (format stream "~:[,~] ~S" firstp x)
                                     (setq firstp nil))))
                           (if (null (cadr (cadr frame)))
                               (princ " (Lexically exited, inactive)")))))
    (frob-block-frames frenv "Blocks" 'block
                       (lambda (frame)
                         (format stream "~%    ~S named ~S"
                                 'block (car (cadr frame)))
                         (if (null (cadr (cadr frame)))
                             (princ " (Lexically exited, inactive)"))))))



;;; Ucode interpreter trap comes here.
;;; Note will never be called by fexpr-call; instead, the ucode
;;; will pseudo-spread the rest-argument-list by hacking the cdr codes.

;;>> apply-lambda is a bit of a misnomer, really.
;; Perhaps the brand s name "call-funny-function" is a little closer to the mark.
;; Note that this is called by applyhook in the case of a closure over any kind of function
;;  (not just `funny')
(defun apply-lambda (fctn a-value-list &optional environment &aux tem)
  (declare (dbg:uninteresting-function eval)
           (dbg:error-reporter))
  (block top
    (tagbody
     tail-recurse
        (binding-interpreter-environment (environment)
          (typecase fctn
            (cons
             (case (car fctn)
               (curry-after
                (tagbody
                    (setq tem (cddr fctn))
                    (%open-call-block (cadr fctn) 0 2)  ;d-return
                    (%assure-pdl-room (+ (length tem) (length a-value-list)))
                 loop1
                    (when a-value-list
                      (%push (pop a-value-list))
                      (go loop1))
                 loop2
                    (when tem
                      (%push (eval1 (pop tem)))
                      (go loop2))
                    (%activate-open-call-block)))
               (curry-before
                (tagbody
                    (setq tem (cddr fctn))
                    (%open-call-block (cadr fctn) 0 2)  ;d-return
                    (%assure-pdl-room (+ (length tem) (length a-value-list)))
                 loop1
                    (when tem
                      (%push (eval1 (pop tem)))
                      (go loop1))
                 loop2
                    (when a-value-list
                      (%push (pop a-value-list))
                      (go loop2))
                    (%activate-open-call-block)))
               ((lambda named-lambda subst cl:subst named-subst)
                (return-from top (apply-lambda-lambda fctn a-value-list)))
               (macro
                (ferror "Funcalling the macro ~S." (function-name (cdr fctn)))
;               (return-from top
;                 (eval1 (cons fctn (mapcar (lambda (arg) `',arg) a-value-list))))
                ))
             ;; A list, but don't recognize the keyword.  Check for a LAMBDA position macro.
             (when (lambda-macro-call-p fctn)
               (setq fctn (lambda-macro-expand fctn))
               (go retry)))
            (closure
             ;; faster than closure-bindings/function
             (setq tem (%make-pointer dtp-list fctn))
             (if (and (cdr tem)
                      (null (cdr (cdr tem))))
                 ;; Barf bletch!  We got called on a lexiclosure.
                 (if (not (consp (car tem)))
                     ;; The closure is of a compiled lexical environment over a non-lambda frob.
                     ;;  Get the ucode to do things.  (We only can get this case from applyhook)
                     (apply fctn a-value-list)
                   ;; The closure is of an interpreter environment over a lambda.
                   ;;  Set up the environment to be that closed-over and try again.
                   (setq fctn (car tem)
                         environment (car (cdr tem)))
                   (go tail-recurse))
               ;; It's a dynamic-closure of a `funny-function'
               (setq fctn (car tem)
                     environment nil)
               (%using-binding-instances (cdr tem))
               (go tail-recurse)))
            ;; turns out we didn't need to trap out.
            (t
             (if tem
                 ;; tem can only be non-nil so far if we've been through the closure case
                 ;; This may happen in the case of applyhook calling us with a closure over
                 ;;  something not closurep or consp above.  If this really is a losing thing
                 ;;  to call, we will get the error next time apply-lambda is called.
                 (return-from top (apply fctn a-value-list))
               (go bad-function)))))
     bad-function
        ;; Can drop through to here for a totally unrecognized function.
        (setq fctn (invalid-function fctn a-value-list #'values))
        (go retry)
     retry
        (if (consp fctn) (go tail-recurse)
          (return-from top (apply fctn a-value-list))))))

;;; The non-special variable .slots.bound.instance.
;;; is bound lexically to the instance (if any)
;;; whose instance variables are lexically bound
;;; in the same environment.
(defun apply-lambda-lambda (fctn a-value-list)
  (declare (dbg:uninteresting-function eval))
  (block top
    (let* (optionalf quotef tem restf init this-restf special-lossage
           (fctn1 (ecase (car fctn)
                    ((lambda subst cl:subst) fctn)
                    ((named-lambda named-subst) (cdr fctn))))
           (lambda-list (cadr fctn1))
           (body (cddr fctn1))
           (value-list a-value-list)
           keynames keyinits keykeys keyflags
           keynames1 keykeys1 keyflags1 (unspecified '(()))
           allow-other-keys
           thisvar thisvarloc bindframe pdl)
      (and (cdr body) (stringp (car body)) (pop body))  ;doc string.

      ;; Make a binding frame to represent any instance variables
      (with-stack-list* (vars-env nil *interpreter-variable-environment*)
        ;; If SELF is an instance, and instance vars aren't bound, bind them.
        ;;  Do it this way in case self is unbound.
        (when (and (variable-boundp self)
                   (instancep self))
          ;;>> this is horrible.  It means that any closure made when self is bound
          ;;  to an instance is closed over those instance variables, even if they
          ;;  are never needed.
          ;; (eg break inside a method, and then "(funcall (lambda () (lambda ())))")
          ;; Of course this is all due to the wretched ugliness of self and self-mapping-table
          ;;  being special whilst instance variables are lexical.  ack gag barf.
          (unless (do ((tail (cdr vars-env) (cdr tail)))
                      ((atom tail) nil)
                    (when (setq tem (get-lexical-value-cell
                                      (car tail)
                                      (locf (symbol-value '.slots.bound.instance.))))
                      (return (eq (contents tem) self))))
            ;;??? Here should take care of special instance variables!!!
            ;; Probably just omit them, since they were bound when
            ;; the message was sent, weren't they?
            ;;>> Actually, they should really be hacked specially, to inhibit
            ;;>>  references to them from being treated as free references. (even though
            ;;>>  the special value is bound)
            ;;>> However, special instance variables are such an incredibly ugly misferature
            ;;>>  that I'm not going to waste my time dealing with them.
            ;; %regular-pdl-index is off by one because of dtp-list being pushed before it.
            (setq tem (%make-pointer dtp-list (%regular-pdl-index)))
            (setf (car vars-env) tem)
            (tagbody
                (setq tem (self-binding-instances))
             loop
                (when tem
                  (%push (pop tem))
                  ;; Store an evcp to flag this as a self-variables binding
                  (%push (pop tem))
                  (%p-store-data-type (%regular-pdl-index) dtp-external-value-cell-pointer)
                  (go loop)))
            ;; now bind .slots.bound.instance. nonspecial
            (%push (locf (symbol-value '.slots.bound.instance.)))
            (%push self)
            ;; Modify cdr-code of last word pushed, to terminate the list.
            (%p-store-cdr-code (%regular-pdl-index) cdr-nil)))

        ;; Make a bindframe to represent and SPECIAL or UNSPECIAL declarations
        (with-stack-list* (vars-env nil vars-env)
          ;; Find any declarations at the front of the function body
          ;; and put them onto VARS-ENV
          ;; Note that any declarations will override instance bindings made
          (gobble-declarations-internal body vars-env)

          ;; Now this bindframe is the one actually used to bind variables...
          (with-stack-list* (*interpreter-variable-environment* nil vars-env)
            (macrolet ((xbind (var value &optional ignore-special-lossage)
                         `(progn
                            ;; see serial-binding-list
                            (setq thisvarloc (require-bindable-symbol ,var))
                            (if (setq pdl (get-lexical-value-cell bindframe thisvarloc))
                                (setf (contents pdl) ,value)
                              (if bindframe
                                  (%p-store-cdr-code (%regular-pdl-index) cdr-next)
                                ;; %regular-pdl-index is off by one because of dtp-list being pushed before it.
                                (setf bindframe (%make-pointer dtp-list (%regular-pdl-index))
                                      (car *interpreter-variable-environment*) bindframe))
                              (%push thisvarloc)
                              (%push ,value)
                              (setq pdl (%regular-pdl-index))
                              (%p-store-cdr-code pdl cdr-nil)
                              ;; Bind the variable as special, if appropriate.
                              (when (or ,(if ignore-special-lossage `nil `special-lossage)
                                        (interpreter-variable-special-in-frame-p thisvarloc vars-env))
                                (%bind thisvarloc (contents pdl))
                                (one-q-forwardify pdl thisvarloc))))))
              (tagbody
               l
                  (cond ((null value-list) (go lp1))
                        ((or (null lambda-list)
                             (eq (car lambda-list) '&aux))
                         (if restf (go lp1)
                           (go too-many-args)))
                        ((eq (car lambda-list) '&key)
                         (go key))
                        ((eq (car lambda-list) '&optional)
                         (setq optionalf t)
                         (go l1))               ;Do next value.
                        ;>> BARF!!
                        ((eq (car lambda-list) '&quote)
                         (setq quotef t)
                         (go l1))
                        ((eq (car lambda-list) '&eval)
                         (setq quotef nil)
                         (go l1))
                        ;>> BARF^2!!
                        ((memq (car lambda-list) '(&special &local))
                         (setq special-lossage (eq (car lambda-list) '&special))
                         (go l1))
                        ((eq (car lambda-list) '&rest)
                         (setq this-restf t)
                         (go l1))               ;Do next value.
                        ((memq (car lambda-list) lambda-list-keywords)
                         (go l1))
                        ((atom (car lambda-list))
                         (setq thisvar (car lambda-list)))
                        ((atom (caar lambda-list))
                         (setq thisvar (caar lambda-list))
                         ;; If it's &OPTIONAL (FOO NIL FOOP),
                         ;; bind FOOP to T since FOO was specified.
                         (when (and optionalf (cddar lambda-list))
                           (and (null (caddar lambda-list)) (go bad-lambda-list))
                           (xbind (caddar lambda-list) t t)))
                        (t (go bad-lambda-list)))
                  ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
                  ;;  It is in THISVAR.
                  (if (null thisvar) (go bad-lambda-list))
                  (cond (restf
                         ;; Something follows a &REST arg???
                         (go bad-lambda-list))
                        (this-restf     ;This IS the &REST arg.
                         ;; If quoted arg, and the list of values is in a pdl, copy it.
                         (and quotef
                              (ldb-test %%pht2-map-access-code
                                        (area-region-bits (%area-number value-list)))
                              (let ((default-cons-area background-cons-area))
                                (setq value-list (copy-list value-list))))
                         (xbind thisvar value-list)
                         ;; We don't clear out VALUE-LIST
                         ;; in case keyword args follow.
                         (setq this-restf nil restf t)
                         (go l1)))
                  (xbind thisvar (car value-list))
                  (pop value-list)
               l1 (pop lambda-list)
                  (go l)
               key
                  (multiple-value-setq (nil nil lambda-list nil nil
                                        keykeys keynames keyinits keyflags
                                        allow-other-keys)
                    (decode-keyword-arglist lambda-list t))
                  ;; Process the special keyword :ALLOW-OTHER-KEYS if present as arg.
                  (if (getf value-list ':allow-other-keys)
                      (setq allow-other-keys t))

                  (setq keykeys1 keykeys        ;life is tough without LET...
                        keynames1 keynames
                        keyflags1 keyflags)
               key1
                  (when keykeys1
                    (setq tem (getf value-list (pop keykeys1) unspecified))
                    (setq init (if (eq tem unspecified) (eval1 (car keyinits)) tem))
                    (xbind (car keynames1) init t)
                    (if (car keyflags1)
                        (xbind (car keyflags1) (neq tem unspecified) t))
                    (pop keynames1)
                    (pop keyflags1)
                    (pop keyinits)
                    (go key1))
                  (do ((x value-list (cddr x))
                       keyword)
                      ((null x))
                    (unless (cdr x)
                      (ferror 'sys:bad-keyword-arglist
                              "No argument after keyword ~S"
                              (car x)))
                    (setq keyword (car x))
                    (setq tem (find-position-in-list keyword keykeys))
                    (unless (or tem allow-other-keys)
                      (do-forever
                        (setq keyword
                              (cerror :new-keyword nil 'sys:undefined-keyword-argument
                                      "Keyword arg keyword ~S, with value ~S, is unrecognized."
                                      keyword (cadr value-list)))
                        (when (setq tem (find-position-in-list keyword keykeys))
                          (interpreter-set (nth tem keynames) (cadr x))
                          (and (setq tem (nth tem keyflags))
                               (interpreter-set tem t))
                          (return)))))
                  ;; Keyword args always use up all the values that are left...

                  ;; Here when all values used up.
               lp1
                  (cond ((null lambda-list) (go ex1))
                        ((eq (car lambda-list) '&rest)
                         (if restf (go bad-lambda-list))
                         (setq this-restf t)
                         (go lp2))
                        ((eq (car lambda-list) '&key)
                         (go key))
                        ((memq (car lambda-list) '(&optional &aux))
                         (setq optionalf t)     ;Suppress too few args error
                         (go lp2))
                        ;;>> BARF!!
                        ((memq (car lambda-list) '(&special &local))
                         (setq special-lossage (eq (car lambda-list) '&special))
                         (go lp2))
                        ((memq (car lambda-list) lambda-list-keywords)
                         (go lp2))
                        ((and (null optionalf) (null this-restf))
                         (if restf (go bad-lambda-list)
                           (go too-few-args)))
                        ((atom (car lambda-list)) (setq tem (car lambda-list))
                                                  (setq init nil))
                        ((atom (caar lambda-list))
                         (setq tem (caar lambda-list))
                         (setq init (eval1 (cadar lambda-list)))
                         ;; For (FOO NIL FOOP), bind FOOP to NIL since FOO missing.
                         (when (cddar lambda-list)
                           (and (null (caddar lambda-list)) (go bad-lambda-list))
                           (xbind (caddar lambda-list) nil t)))
                        (t (go bad-lambda-list)))
               lp3
                  (and (null tem) (go bad-lambda-list))
                  (xbind tem init)
                  (and this-restf (setq restf t))
                  (setq this-restf nil)
               lp2
                  (setq lambda-list (cdr lambda-list))
                  (go lp1)

               ex1; Here to evaluate the body.
                  ;; Terminate frame
                  (%p-store-cdr-code pdl cdr-nil)
                  (return-from top (eval-body body))

               bad-lambda-list
                  (ferror 'sys:invalid-lambda-list "~S has an invalid lambda list" fctn)
;              retry
;                 (return-from top
;                   ;; this gets called in a null lexical environment.
;                   ;;  Too bad.  You lose.
;                   (apply fctn a-value-list))

               too-few-args
                  (return-from top (signal-proceed-case
                                     ((args) 'sys:too-few-arguments
                                             :function fctn :argument-list a-value-list)
                                     (:additional-arguments
                                      (apply fctn (append a-value-list args)))
                                     (:return-value args)
                                     (:new-argument-list (apply fctn args))))

               too-many-args
                  (return-from top (signal-proceed-case
                                     ((args) 'sys:too-many-arguments
                                             :function fctn :argument-list a-value-list)
                                     (:fewer-arguments
                                      (apply fctn args))
                                     (:return-value args)
                                     (:new-argument-list (apply fctn args))))))))))))

;;;; DECODE-KEYWORD-ARGLIST

;;; Given a lambda list, return a decomposition of it and a description
;;; of all the keyword args in it.
;;; POSITIONAL-ARGS is the segment of the front of the arglist before any keyword args.
;;; KEYWORD-ARGS is the segment containing the keyword args.
;;; AUXVARS is the segment containing the aux vars.
;;; REST-ARG is the name of the rest arg, if any, else nil.
;;; POSITIONAL-ARG-NAMES is a list of all positional args
;;;  and the supplied-flags of all optional positional args.
;;; The rest of the values describe the keyword args.
;;; There are several lists, equally long, with one element per arg.
;;; KEYNAMES contains the keyword arg variable names.
;;; KEYKEYS contains the key symbols themselves (in the keyword package).
;;; KEYOPTFS contains T for each optional keyword arg, NIL for each required one.
;;; KEYINITS contains for each arg the init-form, or nil if none.
;;; KEYFLAGS contains for each arg its supplied-flag's name, or nil if none.
;;; Finally,
;;;  ALLOW-OTHER-KEYS is T if &ALLOW-OTHER-KEYS appeared among the keyword args.

;;; POSITIONAL-ARGS, KEYWORD-ARGS, REST-ARG, POSITIONAL-ARG-NAMES, are not computed
;;;  if FOR-APPLY-LAMBDA


(defun decode-keyword-arglist (lambda-list &optional for-apply-lambda)
  (declare (values positional-args keyword-args auxvars
                   rest-arg positional-arg-names
                   keykeys keynames keyinits keyflags allow-other-keys))
  (let (positional-args keyword-args auxvars
        this-rest rest-arg positional-arg-names
        keykeys keynames keyinits keyflags allow-other-keys)
    (setq auxvars (memq '&aux lambda-list))
    (unless for-apply-lambda
      (setq positional-args (ldiff lambda-list auxvars))
      (setq keyword-args (memq '&key positional-args))
      (setq positional-args (ldiff positional-args keyword-args))
      (setq keyword-args (ldiff keyword-args auxvars))
      ;; Get names of all positional args and their supplied-flags.
      ;; Get name of rest arg if any.  Find out whether they end optional.
      (dolist (a positional-args)
        (cond ((eq a '&rest) (setq this-rest t))
              ((memq a lambda-list-keywords))
              (t (if (symbolp a)
                     (push a positional-arg-names)
                   (and (cddr a) (push (caddr a) positional-arg-names))
                   (push (car a) positional-arg-names))
                 (and this-rest (not rest-arg)
                      (setq rest-arg (car positional-arg-names))))))
      (setq positional-arg-names (nreverse positional-arg-names)))
    ;; Decode the keyword args.  Set up keynames, keyinits, keykeys, keyflags.
    (dolist (a (cdr (memq '&key lambda-list)))
      (cond ((eq a '&aux) (return))
            ((eq a '&allow-other-keys) (setq allow-other-keys t))
            ((memq a lambda-list-keywords))
            (t (let (keyname keyinit keyflag keykey)
                 (if (and (consp a) (consp (car a)))    ;((:foo foo) bar)
                     ;; Key symbol specified explicitly.
                     (setq keykey (caar a) keyname (cadar a))
                   ;; Else determine it from the variable name.
                   (setq keyname (if (consp a) (car a) a))      ;(foo bar)
                   (unless (setq keykey (get keyname 'keykey))
                     (setq keykey (intern (symbol-name keyname) pkg-keyword-package))
                     (putprop keyname keykey 'keykey)))
                 (if (consp a) (setq keyinit (cadr a) keyflag (caddr a)))
                 (push keyname keynames)
                 (push keyinit keyinits)
                 (push keyflag keyflags)
                 (push keykey keykeys)))))
    ;; Get everything about the keyword args back into forward order.
    (setq keynames (nreverse keynames)
          keyinits (nreverse keyinits)
          keykeys (nreverse keykeys)
          keyflags (nreverse keyflags))
    (values positional-args keyword-args auxvars
            rest-arg positional-arg-names
            keykeys keynames keyinits keyflags allow-other-keys)))
