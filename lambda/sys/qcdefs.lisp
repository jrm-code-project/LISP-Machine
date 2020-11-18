;;; -*- Mode:LISP; Package:COMPILER; Lowercase:T; Base:8; Readtable:ZL -*-
;;; Definitions and specials for the Lisp machine Lisp compiler
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

(defmacro compiler-fasd-switch (form)
  `(let ((fctn (get *fasd-interface* ',(car form))))
     (if (null fctn) (ferror nil "No fasd interface function.  ~s in ~s mode"
                             ',(car form) *fasd-interface*))
     (funcall fctn . ,(cdr form))))

(defmacro compiler-target-switch (form)
  `(let ((fctn (get *target-computer* ',(car form))))
     (if (null fctn) (ferror nil "No target interface function. ~s in ~s mode"
                             ',(car form) *target-computer*))
     (funcall fctn . ,(cdr form))))

(defvar-resettable *target-computer* 'lambda-interface 'lambda-interface)       ;or 'k
                ;get generators off this.

(defun target-processor-symbol ()
  (case *target-computer*
    (lambda-interface :lambda)
    (k :falcon)
    (t (ferror nil "*TARGET-COMPUTER* is ~s, which is unknown" *target-computer*))))

(defprop lambda-interface fdefine fdefine)      ;to define things we decide not to compile.
(defprop lambda-interface peep peep)
(defprop lambda-interface qlapp qlapp)
(defprop lambda-interface p2sbind p2sbind-for-toplevel) ;note: P2SBIND-FOR-TOPLEVEL goes to P2SBIND for LAMBDA.
(defprop lambda-interface assign-lap-addresses assign-lap-addresses)
(defprop lambda-interface var-compute-init var-compute-init)
(defprop lambda-interface p2 p2)

(defvar-resettable *fasd-interface* 'lambda-fasd-interface 'lambda-fasd-interface)
                ;or nlisp-fasd-interface.

(defprop lambda-fasd-interface fasd-file-property-list fasd-file-property-list)

(DEFVAR-RESETTABLE QCOMPILE-TEMPORARY-AREA NIL NIL
  "Area for compilation itself (within QC-TRANSLATE-FUNCTION) to cons in.")

(DEFVAR GENERATING-MICRO-COMPILER-INPUT-P NIL
  "This is T if the compiler is generating macro-code to pass to the micro compiler.
In that case, the code is generated a little differently
so as to lead to more optimal microcode.
/(Actually, it can fail to be valid macrocode, in little ways).")

(DEFVAR FUNCTION-BEING-PROCESSED :UNBOUND
  "Function the compiler was called on.  Bound only inside the closure used in EH:*ERROR-MESSAGE-HOOK*")

(DEFFLAVOR INTERNAL-ERROR () (FERROR)
  (:DEFAULT-INIT-PLIST :FORMAT-STRING "~Compiler-internal error~%~A: ~S~")
  (:DOCUMENTATION "Signaled for a compiler bug or fatal condition"))
(DEFUN BARF (EXP REASON &OPTIONAL IGNORE)
  (FORMAT *ERROR-OUTPUT* "~%<< While compiling ~S >>" FUNCTION-BEING-PROCESSED)
  (SIGNAL 'INTERNAL-ERROR :FORMAT-ARGS (LIST REASON EXP)))

(DEFVAR FUNCTION-TO-BE-DEFINED NIL
  "Function spec to be defined as the result of the current compilation.")

(DEFVAR NAME-TO-GIVE-FUNCTION NIL
  "This is the function spec to put in the name of the fef produced by the compilation.
Usually this is the same as FUNCTION-TO-BE-DEFINED.")

(DEFCONST COMPILER-VERBOSE NIL
  "T means print name of each function when its compilation begins.")

;;; Holds the arglist for the function being compiled, and the name that goes with it,
;;; to avoid mistaken warnings about wrong number of args in recursive function call.
(DEFVAR THIS-FUNCTION-ARGLIST nil
  "Arglist of the function being compiled.  Used for warnings on recursive calls.")
(DEFVAR THIS-FUNCTION-ARGLIST-FUNCTION-NAME NIL
  "Name of function that THIS-FUNCTION-ARGLIST is the arglist for.")

(DEFCONST HOLDPROG T
  "If NIL, the lap instructions are typed out on the terminal
instead of being saved up for lap.")

(DEFVAR *SPECIALFLAG* :UNBOUND
  "T means the function has bound a special variable.
This information goes into the FEF.")

;;; LOCAL-DECLARATIONS (on SYSTEM) is a list of local declarations.
;;; Each local declaration is a list starting with an atom which says
;;; what type of declaration it is.  The meaning of the rest of the
;;; list depends on the type of declaration.
;;; The compiler is interested only in SPECIAL and UNSPECIAL declarations,
;;; for which the rest of the list contains the symbols being declared,
;;; and MACRO declarations, which look like (DEF symbol MACRO LAMBDA args ..body...),
;;; and ARGLIST declarations, which specify arglists to go in the debugging info
;;; (to override the actual arglist of the function, for user information)
;;; which look like (ARGLIST FOO &OPTIONAL BAR ...), etc.

;;; Things get onto LOCAL-DECLARATIONS in two ways:
;;;  1) inside a LOCAL-DECLARE, the specified declarations are bound onto the front.
;;;  2) if UNDO-DECLARATIONS-FLAG is T, some kinds of declarations
;;;     in a file being compiled into a QFASL file
;;;     are consed onto the front, and not popped off until LOCAL-DECLARATIONS
;;;     is unbound at the end of the whole file.
(DEFVAR-RESETTABLE LOCAL-DECLARATIONS NIL NIL
  "List of local declarations made by LOCAL-DECLARE or DECLARE.
Each one is a list starting with a local declaration type,
followed by more information meaningful according to that type.")
(DEFVAR-RESETTABLE UNDO-DECLARATIONS-FLAG NIL NIL
  "T during file-to-file compilation, causes DEFMACRO and DEFSUBST to work differently.
They push elements on FILE-LOCAL-DECLARATIONS rather than
actually defining functions in the environment.")

(DEFVAR-RESETTABLE FILE-SPECIAL-LIST NIL NIL
  "List of symbols declared globally special in file being compiled.")

(DEFVAR-RESETTABLE FILE-UNSPECIAL-LIST NIL NIL
  "List of symbols declared globally unspecial in file being compiled.")

;;; FILE-LOCAL-DECLARATIONS is just like LOCAL-DECLARATIONS except that it is
;;; local to the file being compiled.  The reason this exists is so that if
;;; you have a (LOCAL-DECLARE ((ARGLIST ...)) ...) around a (MACRO...),
;;; at compile-time the macro wants to be saved on LOCAL-DECLARATIONS, but that
;;; is bound by the LOCAL-DECLARE, so it uses FILE-LOCAL-DECLARATIONS instead.
(DEFVAR-RESETTABLE FILE-LOCAL-DECLARATIONS NIL NIL
  "Like LOCAL-DECLARATIONS for declarations at top level in file being compiled.
However, SPECIAL and UNSPECIAL declarations are handled differently
using FILE-SPECIALS and FILE-UNSPECIALS, for greater speed in SPECIALP.")

(DEFVAR-RESETTABLE SELF-FLAVOR-DECLARATION NIL NIL
  "This is non-NIL if we are supposed to compile
direct accesses to instance variables of SELF.
Its value then is (flavor-name (special-instance-var-names...) instance-var-names...)")

;;; BARF-SPECIAL-LIST is a list of all variables automatically declared special
;;; by the compiler.  Those symbols are special merely by virtue of being on
;;; this list, which is bound for the duration of the compilation
;;; (for the whole file, whole editor buffer, or just the one function in COMPILE).
;;; All users of QC-TRANSLATE-FUNCTION MUST bind this variable.
;;; NOTE!! This list must not be CONSed in a temporary area!!  It lives across
;;;  whack boundaries.
(DEFVAR BARF-SPECIAL-LIST NIL
  "List of symbols automatically made special in this file.")

;;; This is like BARF-SPECIAL-LIST but only lists those symbols
;;; used in the function now being compiled.
;;; If a variable used free is not on this list, it gets a new warning
;;; even though it may already be special because it is on BARF-SPECIAL-LIST.
;;; So there is a new warning for each function that uses the symbol.
(DEFVAR THIS-FUNCTION-BARF-SPECIAL-LIST NIL
  "List of symbols used free in this function but not declared special.
These are the symbols that have been warned about for this function.")

;;; If this is not NIL, there is no warning about using an undeclared free variable.
;;; This is for compiling DEFSUBSTs, which often refer to free variables.
;;; That's ok if you intend them only for expansion.
(DEFVAR INHIBIT-SPECIAL-WARNINGS NIL
  "If non-NIL, no warning is made about free references to variables.")

;;; This is a list of lists; each element of each list
;;; is a symbol which is a variable in the function being compiled.
;;; When lap addresses are assigned, each variable which is not special
;;; is RPLAC'd with NIL.
;;; Further, each list is RPLACD'd with NIL after the last non-NIL element.
(DEFVAR *CLOBBER-NONSPECIAL-VARS-LISTS*)

(DEFVAR *BINDP* :UNBOUND
  "*BINDP* on pass 1 is T if %BIND is called in the current frame.
It is then consed into the internal form of the frame, for pass 2's sake.")

(DEFVAR QCMP-OUTPUT :unbound
  "An ART-Q-LIST array into which the lap-instructions are stored by pass 2.")

(DEFVAR QC-TF-PROCESSING-MODE :unbound
  "What kind of compilation this is. Either MACRO-COMPILE or MICRO-COMPILE.")

(DEFVAR QC-TF-OUTPUT-MODE :unbound
  "QC-TF-OUTPUT-MODE is used by LAP to determine where to put the compiled code.
It is COMPILE-TO-CORE for making an actual FEF, or QFASL, or REL, or
QFASL-NO-FDEFINE to simply dump a FEF without trying to define a function
/(poor modularity).")

(DEFVAR *TLEVEL* :unbound
  "*TLEVEL* on pass 1 is T if we are at /"top level/" within the function being compiled,
not within any actual function calls.
If a LET is seen when *TLEVEL* is set, the locals of the prog can
be initialized by the entry to the function.")

(DEFVAR TLFUNINIT :unbound
  "TLFUNINIT on pass 1 is T if we have already seen a variable initialized to the
result of a function call.  Such initializations can't be done except
by compiled code, and once we have initialized one thing that way
all succeeding variables must be initialized by code as well.
/(This applies to serial binding constructs. Parallel is a little different)")

(DEFVAR *FAST-ARGS-POSSIBLE* :unbound
  "*FAST-ARGS-POSSIBLE* on pass 1 is T if we haven't come across
any argument to this function with a non-NIL initialization.
If this remains T after all the arguments are processed,
then it is an optimization to make top-level prog vars
be initialized at function entry instead of by code.")

(DEFVAR *P1VALUE* :unbound
  "During pass 1, *P1VALUE* is NIL when compiling a form whose value is to be discarded.
P1VALUE is COMPILER::PREDICATE when compiling for predicate value (nilness or non-nilness)
P1VALUE is an integer n when compiling for at most n values (1  most-positive-fixnum)
P1VALUE is T when compiling for all the values which the form will return.
On pass 2, /"destinations/" are used instead, with a bit more information.")
(defvar p1value)
(forward-value-cell 'p1value '*p1value*)

(DEFVAR *CHECK-STYLE-P* T
  "During pass 1, means to perform style checking on the current outermost form being
compiler. Inner forms will have their style checked regardless of this flag, unless
somebody arranges otherwise.")

(DEFVAR *SELF-REFERENCES-PRESENT* :UNBOUND
  "Set to T during pass 1 if any SELF-REFs are generated.")

(DEFVAR PEEP-ENABLE T
  "PEEP-ENABLE, if T, means that the peephole optimizer should be used.
The only reason for setting this to NIL is if you suspect a bug in peephole optimization.")

(defvar *inhibit-optimizers* nil
  "If T, bypasses all calls to optimizers (Rewriters still happen).
The only reason for setting this to T is if you suspect a bug introduced by an optimizer.")

(DEFVAR FUNCTIONS-REFERENCED :UNBOUND
  "FUNCTIONS-REFERENCED is a list of all functions referred to in the file being
compiled, and not defined in the world.  Each element has as its CAR the
name of the function, and as its CDR a list of the names of the functions
which referenced it.")

(DEFVAR *CHECK-STYLE-P* T
  "During pass 1, means to perform style checking on the current outermost form being
compiler. Inner forms will have their style checked regardless of this flag, unless
somebody arranges otherwise.")

;;; Counter for breakoff functions
(DEFVAR *BREAKOFF-COUNT*)

(DEFVAR *LEXICAL-CLOSURE-COUNT* :unbound
  "Counts number of lexical closures we had to make.")

(defvar *highest-lexical-closure-disconnected* :unbound
  "Hopefully this will work!
Bound to 0 in QCOMPILE0 just before calling P2.  Then, it is used to
ensure that each stack closure is only disconnected only once.
This depends on our guess that they get disconnected in increasing order.")

(defvar *compiler-area* (make-area :name 'compiler-area :region-size #o1000000 :room t))

(DEFVAR-RESETTABLE INSIDE-QC-TRANSLATE-FUNCTION NIL NIL
  "T while inside QC-TRANSLATE-FUNCTION")

(defvar *compiling-breakoffs-p* :unbound
  "T when compiling breakoffs for a functions.")

;used to do dynamic MISC linkage
(defvar *placeholder-function-number*)
(defvar *placeholder-alist*)

(ADD-INITIALIZATION "Compiler warm boot"
                    '(COMPILER-WARM-BOOT)
                    :WARM)

(DEFUN COMPILER-WARM-BOOT ()
  (SI:DEALLOCATE-WHOLE-RESOURCE 'COMPILER-TEMPORARIES-RESOURCE))

; there seems to be a bug in the below stuff ...
;(DEFMACRO LOCKING-RESOURCES (&BODY BODY)
;  "Allocate a temporary area, QCMP-OUTPUT and fasd tables for this process."
;  `(locking-resources-internal
;     (lambda ()
;       (declare (sys:downward-function) (dbg:uninteresting-function macro))
;       . ,body)))
;(DEFUN LOCKING-RESOURCES-INTERNAL (CONTINUATION)
;  (IF QCOMPILE-TEMPORARY-AREA
;      (FUNCALL CONTINUATION)
;    (USING-RESOURCE (TEMPS COMPILER-TEMPORARIES-RESOURCE)
;      (LET ((QCOMPILE-TEMPORARY-AREA *compiler-area*)
;            (FASD-HASH-TABLE (second TEMPS))
;            (FASD-EVAL-HASH-TABLE (third TEMPS))
;            (QCMP-OUTPUT (fourth TEMPS))
;            (fasd-tyo-buffer-array (fifth temps)))
;       (SEND FASD-HASH-TABLE :CLEAR-HASH)
;       (SEND FASD-EVAL-HASH-TABLE :CLEAR-HASH)
;       (SETF (FILL-POINTER QCMP-OUTPUT) 0)
;       (MULTIPLE-VALUE-PROG1
;         (FUNCALL CONTINUATION)
;         ;; Get rid of pointers so GC can collect more stuff.
;         (SEND FASD-HASH-TABLE :CLEAR-HASH)
;         (SEND FASD-EVAL-HASH-TABLE :CLEAR-HASH)
;         (ARRAY-INITIALIZE QCMP-OUTPUT NIL))))))
;(DEFMACRO LOCKING-RESOURCES-NO-QFASL (&BODY BODY)
;  "Allocate a temporary area and a QCMP-OUTPUT for this process.
;Use this when compiling to core.
;Does not set up fasd tables, to save time."
;  `(locking-resources-no-qfasl-internal
;     (lambda ()
;       (declare (sys:downward-function) (dbg:uninteresting-function macro))
;       . ,body)))
;(DEFUN LOCKING-RESOURCES-NO-QFASL-INTERNAL (CONTINUATION)
;  (IF QCOMPILE-TEMPORARY-AREA
;      (FUNCALL CONTINUATION)
;    (USING-RESOURCE (TEMPS COMPILER-TEMPORARIES-RESOURCE)
;      (LET ((QCOMPILE-TEMPORARY-AREA *compiler-area*)
;           (FASD-HASH-TABLE NIL)
;           (FASD-EVAL-HASH-TABLE NIL)
;           (QCMP-OUTPUT (fourth TEMPS))
;           (fasd-tyo-buffer-array (fifth temps)))
;       (SETF (FILL-POINTER QCMP-OUTPUT) 0)
;       (MULTIPLE-VALUE-PROG1
;         (FUNCALL CONTINUATION)
;         (ARRAY-INITIALIZE QCMP-OUTPUT NIL))))))


(DEFMACRO LOCKING-RESOURCES (&BODY BODY)
  "Allocate a temporary area, QCMP-OUTPUT and fasd tables for this process."
  `(flet ((.continuation. ()
             (declare (sys:downward-function) (dbg:uninteresting-function macro))
             . ,body))
     (IF QCOMPILE-TEMPORARY-AREA
         (.CONTINUATION.)
       (USING-RESOURCE (TEMPS COMPILER-TEMPORARIES-RESOURCE)
         (LET ((QCOMPILE-TEMPORARY-AREA *COMPILER-AREA*)
               (FASD-HASH-TABLE (SECOND TEMPS))
               (FASD-EVAL-HASH-TABLE (THIRD TEMPS))
               (QCMP-OUTPUT (FOURTH TEMPS))
               (fasd-tyo-buffer-array (fifth temps)))
           (SEND FASD-HASH-TABLE :CLEAR-HASH)
           (SEND FASD-EVAL-HASH-TABLE :CLEAR-HASH)
           (SETF (FILL-POINTER QCMP-OUTPUT) 0)
           (MULTIPLE-VALUE-PROG1
             (.CONTINUATION.)
             ;; Get rid of pointers so GC can collect more stuff.
             (SEND FASD-HASH-TABLE :CLEAR-HASH)
             (SEND FASD-EVAL-HASH-TABLE :CLEAR-HASH)
             (ARRAY-INITIALIZE QCMP-OUTPUT NIL)))))))

(DEFMACRO LOCKING-RESOURCES-NO-QFASL (&BODY BODY)
  "Allocate a temporary area and a QCMP-OUTPUT for this process.
Use this when compiling to core.
Does not set up fasd tables, to save time."
  `(flet ((.continuation. ()
             (declare (sys:downward-function) (dbg:uninteresting-function macro))
             . ,body))
     (IF QCOMPILE-TEMPORARY-AREA
         (.CONTINUATION.)
       (USING-RESOURCE (TEMPS COMPILER-TEMPORARIES-RESOURCE)
         (LET ((QCOMPILE-TEMPORARY-AREA *compiler-area*)
               (FASD-HASH-TABLE NIL)
               (FASD-EVAL-HASH-TABLE NIL)
               (QCMP-OUTPUT (FOURTH TEMPS))
               (fasd-tyo-buffer-array nil))
           (SETF (FILL-POINTER QCMP-OUTPUT) 0)
           (MULTIPLE-VALUE-PROG1
             (.CONTINUATION.)
             (ARRAY-INITIALIZE QCMP-OUTPUT NIL)))))))

(DEFRESOURCE COMPILER-TEMPORARIES-RESOURCE ()
  :CONSTRUCTOR (LIST ;; qcompile-temporary-area (obsolete)
                     *compiler-area*
                     ;; fasd-hash-table
                     (MAKE-HASH-TABLE :SIZE #o40000 :AREA *COMPILER-AREA*)
                     ;; fasd-eval-hash-table
                     (MAKE-EQUAL-HASH-TABLE :SIZE #o400 :AREA *COMPILER-AREA*)
                     ;; qcmp-output
                     (MAKE-ARRAY #o3000 :AREA *compiler-area*
                                        :TYPE 'ART-Q-LIST
                                        :FILL-POINTER 0)
                     ;; fasd-tyo-buffer-array
                     (MAKE-ARRAY 512. :ELEMENT-TYPE '(UNSIGNED-BYTE 16.)
                                      :area *compiler-area*
                                      :FILL-POINTER 0))
  :FREE-LIST-SIZE 5)


(DEFVAR-RESETTABLE COMPILER-WARNINGS-CONTEXT NIL NIL
  "Flag when compiler warnings are being saved for a higher level, like in MAKE-SYSTEM")

(DEFMACRO COMPILER-WARNINGS-CONTEXT-BIND (&BODY BODY)
  "Bind some variables used for compiler warnings."
  (LET ((TOP-LEVEL-P-VAR (GENSYM)))
    `(LET ((,TOP-LEVEL-P-VAR (NOT COMPILER-WARNINGS-CONTEXT)))
       (LET-IF ,TOP-LEVEL-P-VAR
               ((COMPILER-WARNINGS-CONTEXT T)
                (FUNCTIONS-REFERENCED NIL)
                (BARF-SPECIAL-LIST NIL))
         (MULTIPLE-VALUE-PROG1
           (PROGN . ,BODY)
           (WHEN ,TOP-LEVEL-P-VAR
             (PRINT-FUNCTIONS-REFERENCED-BUT-NOT-DEFINED)))))))

(DEFUN FUNCTION-REFERENCED-P (FUNCTION)
  (ASSOC-EQUAL FUNCTION FUNCTIONS-REFERENCED))

(DEFUN COMPILATION-DEFINE (FUNCTION-SPEC)
  "Record that a definition of FUNCTION-SPEC has been compiled."
  (OR (CONSP FUNCTION-SPEC)
      (SI:FUNCTION-SPEC-PUTPROP FUNCTION-SPEC (OR FDEFINE-FILE-PATHNAME T)
                                ':COMPILATION-DEFINED)))

(DEFUN COMPILATION-DEFINEDP (FUNCTION-SPEC)
  "T if the function spec is defined or a definition of it has been compiled.
Always returns T if function spec is not a symbol."
  (OR (CONSP FUNCTION-SPEC)
      (AND FUNCTION-SPEC
           (OR (FDEFINEDP FUNCTION-SPEC)
               (NOT (MEMQ (SI:FUNCTION-SPEC-GET FUNCTION-SPEC ':COMPILATION-DEFINED)
                          '(NIL :UNDEFINED)))))))

(DEFMACRO EXTRACT-DECLARATIONS-RECORD-MACROS (&REST ARGS)
  "Like EXTRACT-DECLARATIONS, but also record names of macros that expand into declarations."
  (DECLARE (ARGLIST BODY &OPTIONAL INITIAL-DECLS DOC-STRING-VALID-P ENVIRONMENT))
  `(LET ((RECORD-MACROS-EXPANDED T))
     (EXTRACT-DECLARATIONS . ,ARGS)))

;;; Inside WARN-ON-ERRORS, this is bound to the TYPE arg to pass to WARN
;;; when an error happens.
(DEFVAR ERROR-WARNING-TYPE :UNBOUND
  "Holds the WARNING-TYPE arg inside a WARN-ON-ERRORS.")

;;; This is a list of a format-string and some args, whose purpose is
;;; to describe the context in which an error generated a warning.
;;; For example, it might be ("Error expanding macro ~S" LOSING-MACRO).
(DEFVAR ERROR-WARNING-ARGS NIL
  "Holds the WARNING-FORMAT-STRING and WARNING-ARGS args inside a WARN-ON-ERRORS.")

(DEFVAR WARN-ON-ERRORS T
  "T to enable the WARN-ON-ERRORS feature while compiling. NIL means get an error.")

(DEFVAR WARN-ON-ERRORS-STREAM NIL
  "Non-NIL => this is stream that read errors are happening on.")

;;; Use this macro to turn errors into compiler warnings.
;;; Used around reading, macroexpanding, etc.
(DEFMACRO WARN-ON-ERRORS ((WARNING-TYPE WARNING-FORMAT-STRING . WARNING-ARGS)
                          &BODY BODY)
  "Execute the body, arranging to make a warning if any error happens.
WARNING-TYPE, WARNING-FORMAT-STRING and WARNING-ARGS
are used to create those warnings, together with the error message."
  `(CATCH 'WARN-ON-ERRORS
     (CONDITION-RESUME-IF T '(ERROR
                              WARN-ON-ERRORS T
                              ("Continue with compilation.")
                              (LAMBDA (&REST IGNORE) (THROW 'WARN-ON-ERRORS NIL)))
       (LET ((ERROR-WARNING-TYPE ,WARNING-TYPE)
             (ERROR-WARNING-ARGS (LIST ,WARNING-FORMAT-STRING . ,WARNING-ARGS)))
         (CONDITION-BIND ((ERROR 'WARN-ON-ERRORS-CONDITION-HANDLER))
           . ,BODY)))))

(DEFUN WARN-ON-ERRORS-CONDITION-HANDLER (CONDITION
                                         &AUX
                                         (CONDITION-NAMES (SEND CONDITION :CONDITION-NAMES))
                                         (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (IF SI:OBJECT-WARNINGS-OBJECT-NAME
      (PROGN (SI:MAYBE-PRINT-OBJECT-WARNINGS-HEADER)
             (FORMAT T "~%Warning: ")
             (APPLY #'FORMAT T ERROR-WARNING-ARGS))
    (PRINT-ERROR-WARNING-HEADER))
  (COND ((AND (MEMQ 'SYS:PARSE-ERROR CONDITION-NAMES)
              (SEND CONDITION :PROCEED-TYPE-P :NO-ACTION))
         (WARN 'READ-ERROR :ERROR "~A" (SEND CONDITION :REPORT-STRING))
         (LET (BP REG)
           (IF WARN-ON-ERRORS-STREAM
               (SETQ BP (SEND WARN-ON-ERRORS-STREAM :SEND-IF-HANDLES :READ-BP)))
           (AND BP
                (SETQ REG (ZWEI::MAKE-REGISTER-NAME #/.))
                (NOT (GET REG 'ZWEI::POINT))
                (PROGN
                  (FORMAT T "~&Position of this error saved in ZWEI register /"./".")
                  (ZWEI::SAVE-POSITION-IN-REGISTER REG BP))))
         :NO-ACTION)
        (T
         (SI:RECORD-WARNING NIL :ERROR NIL "~A"
                            (APPLY #'FORMAT NIL ERROR-WARNING-ARGS))
         ;; Make a string now, in case the condition object points at data
         ;; that is in a temporary area.
         (WARN ERROR-WARNING-TYPE :ERROR "~A" (SEND CONDITION :REPORT-STRING))
         (COND ((AND WARN-ON-ERRORS
                     (NOT (MEMQ 'SYS:PDL-OVERFLOW CONDITION-NAMES))
                     (NOT (SEND CONDITION :DANGEROUS-CONDITION-P))
                     (NOT (SEND CONDITION :DEBUGGING-CONDITION-P)))
                (FORMAT T "~&To debug this, recompile with ~S set to ~S."
                        'COMPILER:WARN-ON-ERRORS NIL)
                'WARN-ON-ERRORS)))))

(DEFUN PRINT-ERROR-WARNING-HEADER ()
  (FORMAT T "~%<< ~A >>"  (APPLY #'FORMAT NIL ERROR-WARNING-ARGS)))


;;; this adds optimizers in the right order -- earliest-defined optimizations get done first.
(defun add-optimizer-internal (target-function optimizer-name)
  (let ((opts (get target-function 'optimizers)))
    (or (memq optimizer-name opts)
        (putprop target-function (nconc opts (ncons optimizer-name)) 'optimizers))
    (setq opts (get target-function 'rewriters))
    (and (memq optimizer-name opts)
         (putprop target-function (delq optimizer-name opts) 'rewriters))
    target-function))

(defun add-rewriter-internal (target-function rewriter-name)
  (let ((opts (get target-function 'rewriters)))
    (or (memq rewriter-name opts)
        (putprop target-function (nconc opts (ncons rewriter-name)) 'rewriters))
    (setq opts (get target-function 'optimizers))
    (and (memq rewriter-name opts)
         (putprop target-function (delq rewriter-name opts) 'optimizers))
    target-function))

(defmacro add-optimizer (target-function optimizer-name &rest ignore)
  `(add-optimizer-internal ',target-function ',optimizer-name))
(make-obsolete add-optimizer "use defoptimzer or defrewrite")

(defmacro defoptimizer (optimizer-name function-to-optimize
                        &optional arglist &body body)
  (if (null arglist)
      (if (not (null body))
          (ferror "Optimizer must accept at least one argument")
      `(add-optimizer-internal ',function-to-optimize ',optimizer-name))
    `(progn (defun ,optimizer-name ,arglist
              (declare (function-parent ,optimizer-name defoptimizer))
              . ,body)
            (add-optimizer-internal ',function-to-optimize ',optimizer-name))))

(defmacro defrewrite (rewriter-name function-to-rewrite
                      &optional arglist &body body)
  (if (null arglist)
      (if (not (null body))
          (ferror "Optimizer must accept at least on argument")
        `(add-rewriter-internal ',function-to-rewrite ',rewriter-name))
    `(progn (defun ,rewriter-name ,arglist
              (declare (function-parent ,rewriter-name defrewrite))
              . ,body)
            (add-rewriter-internal ',function-to-rewrite ',rewriter-name))))

(defmacro defcompiler-synonym (function synonym-function)
  "Make the compiler substitute SYNONYM-FUNCTION for FUNCTION when compiling.
eg (defcompiler-synonym plus +)"
  `(defrewrite ,(intern (string-append function "-TO-" synonym-function)) ,function
               (form)
     (cons ',synonym-function (cdr form))))

;;;; Variables data bases:

;;; Bound (local or special) variables are described by two lists of variable descriptors:
;;; *VARS*, which describes only variables visible from the current point of compilation,
;;; and *ALLVARS*, which describes all variables seen so far in the current compilation.

;each element is either a VAR structure, OR (t . <list of VAR structures at higher lexical level>)
(DEFVAR *VARS* :UNBOUND
  "List of variable descriptors of currently lexically visible variables.")

;;; *ALLVARS* is passed to lap to allocate slots, while *VARS* is used on both passes
;;; for figuring out what to do with a variable.
(DEFVAR *ALLVARS* :UNBOUND
  "List of variable descriptors of all variables used within this function.")

(DEFVAR *FREEVARS* :unbound
  "List of all special variables references free.")

;;; By the time pass 2 is done, this is
;;; a list of *VARS* entries for the variables of the current function
;;; that are used in lexical closures within it.
(DEFVAR *VARIABLES-USED-IN-LEXICAL-CLOSURES*)

;;; *ARG-MAP* and *LOCAL-MAP* are given the arg map and local map for the debugging info.
;;; This is done by ASSIGN-LAP-ADDRESSES, so that special vars that get a slot
;;; can be put in the map even though their places in it will not be recogizable
;;; from their lap addresses.
(DEFVAR *ARG-MAP*)
(DEFVAR *LOCAL-MAP*)
(DEFVAR *BASE-STACK-SLOTS*)             ;stack-slots required to hold variable homes > 16.
(DEFVAR *STACK-SLOTS*)                  ;Currently existing stack-slots.  Base plus slots for pending calls
                                        ;with >16. args.

;open frames, buffering intermediate values, and multiple values in cross-compile mode:
; (1)  We dont have a stack to buffer things, so we use call frames allocated specifically for the purpose
;   to hold things.  That works pretty well, but we actually have to call a function (usually prog1-internal)
;   to eventually get rid of it (them).
; (2)  thus, open frames are used for the following purposes:
;      (a)  The "main" one, to hold evaluated args while prepareing a function call (P2ARGC-FOR-K)
;      (b)  P2PROG12N-FOR-K (open this out just to sop up any number of args).
;the mechanism for keeping track of things is *open-frames*, which is a list of open-frame structures.
;*open-frames* works stack-wise, ie, it should always be at the same level when entering as when leaving
;functions like P2-for-K, P2ARGC-FOR-K, etc.
;the timing is:
;  the entry gets put on when the "logical" open is about to be output.  This is just before K:CH-OPEN, etc,
;    in usual case, but can be substantually before in case of k:new-open, etc.  This is necessary
;    to assure the stackwise property.
;  when the code which creates the frame is recognized at OUTI-FOR-K, (either the K:CH-OPEN or K:NEW-OPEN)
;    the frame is marked as THERE-P.
;  At P2ARGC-FOR-K, when all args are "loaded", it does a OUTI-CLOSE-FOR-K, when "sends" a NIL message to the
;    frame, which outputs the activating code (K:CALL or K:TAIL-CALL).
;  when the code which activates the frame is recognized (K:CALL OR K:TAIL-CALL),  FINISH-OPEN-FRAME is called,
;    which pops *open-frames*. Then, if a return is involved, the value is generated to k:o0 and a clean-up-open-frames
;    in mode :RETURN is invoked, followed by OUTPUT-FULL-EXIT-SEQUENCE, followed by returnning k:o0.
;  This latter also happens if a K:MOVE to a *return-destination* is encountered.

;K:OPEN operations should always be output as separate instructions.  K:OPEN-CALL, K:CH-OPEN, etc, should never
;  be directly output.  This simplifies OUTI-FOR-K, and POSTPROCESS optimizes them well.

;Calling p2-for-k and p2-mv-for-k.
;  if you want multiple-values, *m-v-target* gets bound to the "instruction" for the lower guys.
;  For the lambda, if *m-v-target* is non-null, DEST was always D-PDL.  It is then possible that
;   the lower guy either "sees" the instruction or not.  If so, *m-v-target* is set to NIL.  Otherwise,
;   if it was left alone, the lower level really didnt do anything at all special vis-a-vis multiple-values.
;

;keeping the actual code and *open-frames* in sync:
;  outi-for-k

(DEFVAR *OPEN-FRAMES*)                  ;In cross compile mode list of open frames.  The elements are
                                        ;structures of type OPEN-FRAME (see below).

;;; Something much more human-readable than GENSYM.  Don't use GENSYM, use
;;; GENSYMBOL.  GENSYMBOL takes a series of things to be coerced to strings
;;; to be included in the symbol name, to help identify its origins to any
;;; human who comes across it.

(defvar *gensymbol-counter* 0)

(defun gensymbol (&rest strings)
  (make-symbol (with-output-to-string (stream)
                 (dolist (frob strings)
                   (typecase frob
                     ((or number list) (write frob :stream stream :radix nil :base 10.))
                     (otherwise (write-string (string frob) stream))))
                 (write-char (if strings
                                 #\-
                               #\G)             ;For old times' sake, sigh.
                             stream)
                 (write (incf *gensymbol-counter*) :stream stream
                        :radix nil :base 10.))))

;;; Destinations:

(defstruct (multiple-values :conc-name :named :copier (:alterant nil)
                            (:print-function print-multiple-values))
  (values nil :documentation "A list of destinations.") ;List of destinations.
  (open-frame nil :documentation "An OPEN-FRAME that this destination activates."))

(defun print-multiple-values (multiple-values stream level)
  (ignore level)
  (printing-random-object (multiple-values stream)
    (format stream "Multiple-values: ~:[None~*~;~{~S~^, ~}~]~:[~;; Frame: ~S~]"
            (multiple-values-values multiple-values)
            (multiple-values-values multiple-values)
            (multiple-values-open-frame multiple-values)
            (when (multiple-values-open-frame multiple-values)
              (open-frame-open-instruction (multiple-values-open-frame multiple-values))))))


(defstruct (open-frame :conc-name :named :copier (:print-function print-open-frame))
  (open-instruction)                    ;For debugging.
  (tail-p)                              ;For error checking.
  (there-p)                             ;If NIL, frame does not exist at runtime yet at all.
                                        ; this is set T by OUTI-FOR-K when instruction which creates
                                        ; frame is processed.  In case of destination k:NEW-OPEN, etc,
                                        ; that can be quite a while after frame is logically generated.
  (cleanup-generator)                   ;Function of three arguments.
                                        ;The first argument is the OPEN-FRAME object
                                        ;The second argument is one of:
                                        ;NIL -- Normal completion of the frame.
                                        ;:DISCARD -- Discard the frame, no value.
                                        ;:RETURN -- Discard the frame, return a value.
                                        ;The third argument is the destination, or where
                                        ;the return value may be found (in the case of :RETURN).
  (pdest)                               ;Where to put the value this frame needs.
  (idest)                               ;Where to put the value when this frame is done.
  )

(defun print-open-frame (open-frame stream level)
  (ignore level)
  (printing-random-object (open-frame stream :type)
    (format stream "~A ~:[(Unfinished) ~]~:[~;(Tail-called) ~]~:[~;~A~A~]"
            (open-frame-open-instruction open-frame)
            (open-frame-there-p open-frame)
            (open-frame-tail-p open-frame)
            (or (open-frame-pdest open-frame)
                (open-frame-idest open-frame))
            (or (open-frame-pdest open-frame) "??")
            (or (open-frame-idest open-frame) "??"))))

;;; Use this macro when we do something which creates an open frame.
;;; The cleanup-body is queued up to be run when we're finished with
;;; the open frame.  It may be run many times, in the presence of
;;; conditional branching or returning.

(defmacro with-open-frame (open-instruction ((&optional open-frame discardp destination) &body cleanup-body) &body body)
  (let ((cleanup-fun (gensymbol "CLEANUP-FUN"))
        (open-i (gensymbol "OPEN-INSTRUCTION"))
        (nopen-frame open-frame)
        (ndestination destination)
        (ndiscardp discardp))
    (unless nopen-frame
      (setq nopen-frame (gensymbol "OPEN-FRAME")))
    (unless ndiscardp
      (setq ndiscardp (gensymbol "DISCARDP")))
    (unless ndestination
      (setq ndestination (gensymbol "DESTINATION")))
    `(flet ((,cleanup-fun (,nopen-frame ,ndiscardp ,ndestination)
             ,@(unless discardp
                 ;; Only burn up symbols we created.  We want to get the "unused" warning iff
                 ;; he supplied the arg.
                 `(,ndiscardp))
             ,@(unless open-frame
                 `(,nopen-frame))
             ,@(unless destination
                 `(,ndestination))
             ,@cleanup-body))
       (let* ((,open-i ,open-instruction))
         (opening-frames (,destination :new-frame (make-open-frame :open-instruction ,open-i
                                                                   :tail-p (tail-open-p ,open-i)
                                                                   :cleanup-generator #'cleanup-fun))
           (outi-for-k ,open-i)
           ,@body)))))

;;; This is used both as a subroutine of the above, and for P2ARGC-for-K
;;; In the P2ARGC-for-K case, the caller wraps the following macro around
;;; the entire generation of the call, and P2ARGC-for-K does the missing pieces
;;; by calling OUTI-OPEN-FOR-K.

(defmacro opening-frames ((dest new-frame) &body body)
  (let ((original-open (gensymbol "ORIGINAL-OPEN-FRAMES"))
        (original-dest (gensymbol "ORIGINAL-DESTINATION"))
        (new-frame-symbol (gensymbol "NEW-FRAME")))
    `(let* ((,original-open *open-frames*)
            (,original-dest ,dest)
            (,new-frame-symbol ,new-frame)
            (,dest (or ,new-frame-symbol ,dest)))
       (multiple-value-prog1
         (progn (when ,new-frame-symbol
                  (setf (open-frame-idest ,new-frame-symbol) ,original-dest)
                  (add-frame ,new-frame-symbol))
                ,@body)
         (when ,new-frame-symbol
           (unless (or (not *dropthru*)
                       (eq *open-frames* ,original-open))
             (fsignal "How does this happen?")
             (clean-up-open-frames ,original-open nil ,dest)))))))

;;; Call this when doing a "temporary" discard of excess stack.
;;; For example, when generating a branch or return.
;;; Note that this adjusts the frame level *before* running the body.
;;; Do not call P2 from within!

(defmacro discarding-open-frames ((level destination) &body body)
  `(let ((*stack-slots* *stack-slots*))
     (with-frames
       (clean-up-open-frames ,level :discard ,destination)
       ,@body)))

(defmacro with-frames (&body body)
  (let ((old-frames (gensymbol "OLD-FRAMES")))
    `(let* ((,old-frames *open-frames*)
            (*open-frames* *open-frames*))
       (multiple-value-prog1 (progn ,@body)
                             ;; Let the person debugging the compiler notice that
                             ;; the frames list is being modified.
                             (restore-frame ,old-frames)))))


(DEFVAR *LOCAL-FUNCTION-MAP*)

;;; Each elt of *VARS* or *ALLVARS* describes one variable and is called a VAR or a "home".
;;; *VARS* can also contain elements that represent local SPECIAL declarations
;;; and do not mean that any binding has taken place.  These have FEF-ARG-FREE as the KIND,
;;; FEF-SPECIAL as the TYPE, and the variable as the LAP-ADDRESS.

;;; A VAR has these components:

(DEFSTRUCT (VAR :named (:CONC-NAME VAR-) (:ALTERANT NIL) :copier (:print-function print-var))
  (NAME NIL :DOCUMENTATION "The variable's name.
If this is the gensym variable that is used to implement
a local (FLET) function, then the name has a LOCAL-FUNCTION-NAME property
which is the symbol actually defined as a function in the FLET.")
  (KIND NIL :DOCUMENTATION
    "One of FEF-ARG-REQ, FEF-ARG-OPT, FEF-ARG-REST, FEF-ARG-AUX, FEF-ARG-INTERNAL-AUX
Can also be FEF-ARG-FREE for the entry pushed by a local SPECIAL declaration.")
  (TYPE NIL :DOCUMENTATION
    "Either FEF-LOCAL or FEF-SPECIAL.")
  (USE-COUNT 0 :DOCUMENTATION
    "Number of times variable is used, not counting binding and initialization.")
  (LAP-ADDRESS NIL :DOCUMENTATION
    "(ARG n) for an argument, (LOCAL n) for a local, (SPECIAL symbol) for a special variable.")
  (INIT NIL :DOCUMENTATION "Describes how the variable should be initted on binding.")
  ;; See below for how to interpret this field.
  (EVAL NIL :DOCUMENTATION "FEF-QT-QT for &QUOTE arg, otherwise FEF-QT-EVAL.")
  (MISC NIL :DOCUMENTATION
    "List of additional FEF-... symbols serving as flags about this variable.
FEF-ARG-FUNCTIONAL means it is an &FUNCTIONAL arg.
FEF-ARG-SPECIFIED-FLAG means it is the specified-p variable of an optional arg.
FEF-ARG-USED-IN-LEXICAL-CLOSURES means that lexical closures refer free to this variable.
FEF-ARG-OVERLAPPED means this is the OVERLAP-VAR of some other VAR.
Lap will add the values of these symbols into the ADL word for the variable.")
  (DECLARATIONS NIL :DOCUMENTATION "Declarations pertaining to this variable.")
                    ;Not used currently.
  (OVERLAP-VAR NIL :DOCUMENTATION "Another VAR, the one whose slot this one shares;
or NIL if there is none.")
  (PLIST NIL :DOCUMENTATION "other info!"))

(defun print-var (struct stream level)
  (ignore level)
  (printing-random-object (struct stream :type)
    (prin1 (var-name struct) stream)))


(DEFCONST FEF-ARG-SPECIFIED-FLAG 0)
(DEFCONST FEF-ARG-USED-IN-LEXICAL-CLOSURES 0)
(DEFCONST FEF-ARG-OVERLAPPED 0)

;-- the comment below applies AFTER VAR-COMPUTE-INIT has been called.
;-- before VAR-COMPUTE-INIT, VAR-INIT has (init-form arg-supplied-flag-name).

;;; The INIT is of the form ( <type> <data> . <arg-supplied-flag home>)
;;; The arg-supplied-flag name is the home of FOOP in &OPTIONAL (FOO NIL FOOP).
;;; It appears only for optional arguments which have such a flag.
;;; If there is none, the cddr of INIT will be nil.
;;; The type is of of several symbols starting with "FEF-INI-", that
;;; signify one of the ways of initializing the variable.
;;; FEF-INI-COMP-C indicates that compiled code will be used to
;;; do the initialization.  It is the most general.  The other types
;;; exist to make special cases more efficient.  They are:

;;; FEF-INI-NONE        No initialization (for a local variable which should be nil).
;;; FEF-INI-SELF        Initialize to self (for special variable).
;;; FEF-INI-NIL         Initialize to NIL (for special variable).
;;; FEF-INI-PNTR        Initialize to a constant.  <data> is that constant.
;;; FEF-INI-C-PNTR      Initialize to the contents of a location.  <data> points to it.
;;; FEF-INI-EFF-ADR     Initialize to the contents of an "effective address".
;;;                     This is used to copy the value of a previous arg or local variable.
;;;                     <data> specifies which one, using an instruction source field
;;;                     which will specify the arg block or the local block, plus offset.
;;; FEF-INI-OPT-SA      For an optional variable with a complicated default value.
;;;                     <data> specifies a starting address inside the function
;;;                     which is where to start if the argument IS supplied.
;;;                     It follows the code used to compute and store the default value.
;;; FEF-INI-COMP-C      Indicates that the variable will be initialized by the
;;;                     compiled code of the function.

;;;     - IN GENERAL -
;;;     Internal variables are bound by internal LAMBDA's and PROGS
;;;     Others are bound at entry time
;;;     All internal variables are initialized by code
;;;     Arg variables are never initialized
;;;     Optional and aux variables are initialized at bind time
;;;     If possible otherwise by code
;;;     This "possibility" is determined as follows:
;;;             Initially, it is possible
;;;             It remains possible until you come to a variable
;;;             initialized to a fctn, at which point it is no longer possible
;;;     If var to be initialized by code, code 0 (special) or
;;;     1 (local) is used in initialization field


;;; A NEW-VAR is a destination that creates a variable.

(defstruct (new-var :named (:conc-name NEW-VAR-) (:alterant nil) :copier
                    (:print-function print-new-var))
  (var nil :documentation "The VAR struct this destination will create.")
  (open-frame nil :documentation "The OPEN-FRAME struct this destination will activate."))

(defun print-new-var (new-var stream level)
  (ignore level)
  (printing-random-object (new-var stream :type)
    (if (new-var-var new-var)
        (prin1 (var-name (new-var-var new-var)) stream)
      (write-string "(none)" stream))))


;;;; GO tag data base.

;;; The variable GOTAGS contains an alist describing all the tags
;;; of TAGBODYs the code we are currently compiling is contained in.
;;; Each element of GOTAGS is a GOTAG, as defined below.

;;; In addition, each BLOCK puts one GOTAG on the list.
;;; That is the block's rettag, which we jump to to return from the block.

(DEFVAR *GOTAG-ENVIRONMENT* :unbound
  "Alist of GOTAGs for each lexically visible tag meaningful to GO.")

;;; ALLGOTAGS is a list of all prog-tags defined so far in the current function,
;;; whether the progs defining them contain the current one or not.
;;; The elements are atoms (the actual tags).
;;; This list is not inherited from the lexically containing function.
;;; ALLGOTAGS is used to determine when the lap-tag of a new tag must be different
;;; from the user-specified prog-tag.
(DEFVAR ALLGOTAGS :unbound
  "List of all tags (actual symbols, not GOTAGs) defined within the current function.")

(DEFSTRUCT (GOTAG :named (:CONC-NAME GOTAG-) (:CONSTRUCTOR MAKE-GOTAG-INTERNAL) (:ALTERANT NIL))
  (PROG-TAG NIL :DOCUMENTATION
    "Actual tag name that the user used. For rettags of blocks, this is a gensym.")
  (LAP-TAG NIL :DOCUMENTATION
    "Tag name to use for LAP. May be the same as GOTAG-PROG-TAG.")
  ;; I don't know how PDL-LEVEL can be different for various points in the same PROG.
  ;; For K, I'm forcing open frames to be at the same level at any top-level point in
  ;; a TAGBODY, so an OPEN-FRAMES slot isn't needed here.  Instead, use the one from the
  ;; PROGDESC.  --RWK
  (PDL-LEVEL NIL :DOCUMENTATION
    "Pdl level we are supposed to have at that point in the code.
Used to tell how many words to pop when you branch.")
  (PROGDESC NIL :DOCUMENTATION
    "Pointer to the element of PROGDESCS for the BLOCK or TAGBODY
that generated this GOTAG.")
  (USED-IN-LEXICAL-CLOSURES-FLAG NIL :DOCUMENTATION
    "T if this tag used in internal lambdas.
The PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG of our GOTAG-PROGDESC
will also be non-NIL in that case."))


(DEFMACRO MAKE-GOTAG (&OPTIONAL PROG-TAG LAP-TAG PDL-LEVEL PROGDESC)
  `(make-gotag-internal :prog-tag ,prog-tag :lap-tag ,lap-tag :pdl-level ,pdl-level
                        :progdesc ,progdesc))

(DEFVAR *PROGDESC-ENVIRONMENT* :UNBOUND
  "The elements describe the active BLOCK, LET and TAGBODY constructs, innermost first.
Each element is a PROGDESC structure.")

(DEFSTRUCT (PROGDESC :named (:CONC-NAME PROGDESC-) (:ALTERANT NIL) :COPIER
                     (:print-function print-progdesc))
  "Describes one element of PROGDESCS."
  (NAME NIL :DOCUMENTATION "Name of this block, or (TAGBODY) or (LET).
/(TAGBODY) is used for PROGDESCs for TAGBODY forms,
/(LET) is used for PROGDESCs for variable binding forms,
a symbol is used for BLOCKs.")
  (RETTAG NIL :DOCUMENTATION
   "Tag to branch to to exit this construct.  Used for blocks only.
The rettag is followed, if necessary, by code to transfer the
block's value from its IDEST to the actual destination.")
  (IDEST NIL :DOCUMENTATION "Destination to compile contents of block with, on pass 2.
Used for blocks only.")
  (M-V-TARGET NIL :DOCUMENTATION "Value of *M-V-TARGET* around this block.
Says whether the block's caller wants multiple values.  Used for blocks only.
If it is NIL, only one value is wanted.
If it is MULTIPLE-VALUE-LIST, then the block should really
return the list of the values that RETURN wants to return.
If it is THROW or RETURN, the block should do the hairy things
to pass all but the last value to the frame that is going to get them,
then return the last value on the stack.
If it is a number, the block should return that many values on the stack.

On pass 1, this is *P1VALUE* on entry to the block.")
  (PDL-LEVEL NIL :DOCUMENTATION
   "The PDL-LEVEL is the pdl level at entry to the construct,
which is also the level in between statements in the construct.
Used in all PROGDESCs, for blocks, binding forms and TAGBODYs.")
  (NBINDS NIL :DOCUMENTATION
   "Number of special bindings to unbind at exit from the construct.
Can also be a list containing the number to unbind, which means
that in addition an unknown number of %BINDs will be done
and therefore UNBIND-TO-INDEX must be used to unbind them
to a specpdl pointer saved at the beginning of the construct.")
  (VARS NIL :DOCUMENTATION "Value of *VARS* at entry to this block.  Used only in blocks.")
  (ENTRY-LEXICAL-CLOSURE-COUNT NIL :DOCUMENTATION
   "Unused.")
  (EXIT-LEXICAL-CLOSURE-COUNT NIL :DOCUMENTATION
   "Unused.")
  (USED-IN-LEXICAL-CLOSURES-FLAG NIL :DOCUMENTATION
   "For blocks, non-NIL if any lexical closure within this block tries to RETURN from it.
The actual value is a list which is used as a catch tag to implement such RETURNs.")
  (open-frames *open-frames* :documentation
   "Open frames at entry to the construct, which is also the level in between
statements in the construct.  Used in all PROGDESCs, for blocks, binding forms, and
TAGBODYs.  This is the K's generalization of PDL-LEVEL."))

(defun print-progdesc (progdesc stream level)
  (ignore level)
  (printing-random-object (progdesc stream :type)
    (format stream "~S  ~A; Frame level = ~S"
            (progdesc-name progdesc)
            (progdesc-idest progdesc)
            (progdesc-open-frames progdesc))))

;(DEFVAR RETPROGDESC :UNBOUND
;  "PROGDESC element for the block that plain RETURN should return from, or NIL if none.")

;;; Queue of functions to be compiled.
;;; Any internal lambdas are put on the queue
;;; so that they get compiled after the containing function.
(DEFVAR COMPILER-QUEUE :UNBOUND
  "List of pending functions to be compiled inside QC-TRANSLATE-FUNCTION.
Each element is a COMPILER-QUEUE-ENTRY.")

;(defstruct (compiler-environment (:type :list) (:conc-name "COMPILER-ENVIRONMENT-")
;                                (:alterant nil) (:callable-constructors nil))
;  (functions nil :documentation "Lexical functions as estblished by FLET and MACROLET, etc.
;Same format as SI::*INTERPRETER-FUNCTION-ENVIRONMENT*")
;  (declarations nil :documentation nil)
;  (function-declarations nil :documentation "Declarations for local functions.
;Format of each elt is (function-name . declarations-list)
;When a new function becomes lexically visible, a new element is created with no declarations.")
;  (local-functions nil :documentation "Lexical accessible functions.")
;  (variables nil :documentation "Lexically accessible variables.")
;  (progdescs nil :documentation "Structures created by BLOCK, TAGBODY and LET frames")
;  (gotags nil :documentation "Structures created by GO's")

;(defmacro binding-compiler-environment ((environment) &body body)
;  "Execute BODY with the compiler's environment initialized by ENVIRONMENT."
; ******

;(defmacro with-current-compiler-environment
; ******

(DEFSTRUCT (COMPILER-QUEUE-ENTRY :named (:CONC-NAME COMPILER-QUEUE-ENTRY-) (:ALTERANT NIL))
  "Describes one element of COMPILER-QUEUE."
  (FUNCTION-SPEC NIL :DOCUMENTATION
    "Function spec to define once compilation is done.")
  (FUNCTION-NAME NIL :DOCUMENTATION
    "Function spec to record in the compiled function as its name.")
  (DEFINITION NIL :DOCUMENTATION
    "Lambda expression to compile.")
;this slot will replace all those below.
; (environment nil :documentation
;   "Compiler environment we use to compile this function."
  (DECLARATIONS NIL :DOCUMENTATION "Declarations in effect from containing function.")
  (VARIABLES NIL :DOCUMENTATION "Variables accessible through containing function.
This becomes the value of *VARS*.")
  (LOCAL-FUNCTIONS NIL :DOCUMENTATION "Lexical functions inherited from containing function.")
  (PROGDESCS NIL :DOCUMENTATION "PROGDESCS inherited from containing function.
This becomes the value of *PROGDESC-ENVIRONMENT*.")
  (GOTAGS NIL :DOCUMENTATION "GOTAGS from containing function that we can go to.
This becomes the value of *GOTAG-ENVIRONMENT*.")
  (FUNCTION-ENVIRONMENT NIL :DOCUMENTATION
   "Standard environment function environment frame containing local macros and functions
we are inheriting. This becomes the value of *FUNCTION-ENVIRONMENT*")
; (RETPROGDESC)                                 ;no longer used
  )

(DEFVAR *OUTER-CONTEXT-VARS* :UNBOUND
  "Intended lexical variable environment of the function being compiled.
For a top-level function in a file, it is NIL.
In general it is a list of *VARS* lists, to be scanned in the order listed.
Each element corresponds to one environment level,
and the position of a variable home within the element
is its index within that level.
The level number and the index within the level are the two quantities
that are passed to %LOCATE-IN-HIGHER-CONTEXT.")

(DEFVAR *OUTER-CONTEXT-LOCAL-FUNCTIONS* :UNBOUND
  "Used to initialize *LOCAL-FUNCTIONS* for compilation of one function.")

(DEFVAR *OUTER-CONTEXT-FUNCTION-ENVIRONMENT* :UNBOUND
  "Used to initialize *FUNCTION-ENVIRONMENT* for compilation of one function.")

(DEFVAR *OUTER-CONTEXT-PROGDESC-ENVIRONMENT* :UNBOUND
  "Used to initialize *PROGDESC-ENVIRONMENT* for compilation of one function.")

(DEFVAR *OUTER-CONTEXT-GOTAG-ENVIRONMENT* :UNBOUND
  "Used to initialize *GOTAG-ENVIRONMENT* for compilation of one function.")

(DEFVAR *FUNCTION-ENVIRONMENT* :UNBOUND
  "List of frames describing local macro and function definitions.
The format of this object is the same as that of SI::*INTERPRETER-FUNCTION-ENVIRONMENT*.")

(DEFVAR *LOCAL-FUNCTIONS* :UNBOUND
  "Alist of elements (local-function-name vars-entry function-definition)
It records, for each local function name (defined by FLET or LABELS)
the local variable in which the function definition actually lives.")


(defun fsymeval-in-function-environment (symbol &optional (fenv *function-environment*))
  "Returns SYMBOL's function or macro definition within the function environment FENV,
or NIL if it is not defined *by* the environment."
  (with-list (env fenv)
    (si:fsymeval-in-environment symbol env nil)))


;;; Compiler switches:  set these with (EVAL-WHEN (COMPILE) (SETQ ...))
;;; These are reinitialized in QC-PROCESS-INITIALIZE

(DEFVAR OPEN-CODE-MAP-SWITCH T
  "This, if T, causes MAP, etc. to be open-coded.  It is normally T.")
;The use of this variable is obsolete: instead declare (NOTINLINE MAPCAR) or whatever.")

;(DEFVAR ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH NIL
;  "If T causes a check to be made for the use of a local variable
;as a function to be called, meaning funcall.  This should be set to T
;only for compiling very old-fashioned Maclisp code.")

(DEFVAR ALL-SPECIAL-SWITCH NIL
  "If T makes all variabes special.")

;; This, if T (as it usually is), warns the user if any obsolete
;; Maclisp functions are used.
;(DEFVAR OBSOLETE-FUNCTION-WARNING-SWITCH T)

;(DEFVAR RUN-IN-MACLISP-SWITCH NIL
;  "If T warns the user if he does anything that clearly cannot work in Maclisp.
;This is getting fairly useless, now that so little Lisp Machine code could possibly
;run in Maclisp anymore.")

(DEFVAR INHIBIT-STYLE-WARNINGS-SWITCH NIL
  "If T prevents warnings about a lot of stylistic losses.")
