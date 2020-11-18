;;  -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-
;;; This file contains pass 1 and the top level of the Lisp machine Lisp compiler

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

(PROCLAIM '(SPECIAL MC-HOLDPROG ULAP-DEBUG LAP-DEBUG))

(defmacro compiler-with-stack-list ((var . args) &body body)
  `(with-list (,var . ,args)
     ,@body))

(defmacro dcd (&rest x) `(disassemble (compile (defun . ,x))))

;;;In QCDEFS
;(defvar *compiling-breakoffs-p*)
(defvar this-frame-declarations '())

;;; Initialize all global variables and compiler switches
(DEFUN QC-PROCESS-INITIALIZE ()
  (SETQ HOLDPROG T)
  (SETQ MC-HOLDPROG T)
  (SETQ ULAP-DEBUG NIL)
  (SETQ LAP-DEBUG NIL)
  (SETQ FUNCTION-BEING-PROCESSED NIL)   ;For error printouts.  Avoid any unbound problems
  (SETQ OPEN-CODE-MAP-SWITCH T)
  ;(SETQ ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH NIL)
  (SETQ ALL-SPECIAL-SWITCH NIL)
  ;(SETQ OBSOLETE-FUNCTION-WARNING-SWITCH T)
  ;(SETQ RUN-IN-MACLISP-SWITCH NIL)
  (SETQ INHIBIT-STYLE-WARNINGS-SWITCH NIL)
  (SETQ *CHECK-STYLE-P* T))

(DEFF MICROCOMPILE 'MICRO-COMPILE)

(DEFUN MICRO-COMPILE (NAME &OPTIONAL LAMBDA-EXP)
  "Similar to COMPILE, but microcompiles it.  Leaves result as
LAP on property list as MCLAP, does not load anything into control memory"
  (COMPILE NAME LAMBDA-EXP 'MICRO-COMPILE))

;;; Compile a function which already has an interpreted definition,
;;; or define it to a newly supplied definition's compilation.
;;; If the definition is one which is legal but cannot meaningfully
;;; be compiled, we just leave it unchanged.
(DEFUN COMPILE (NAME &OPTIONAL LAMBDA-EXP (PROCESSING-MODE 'MACRO-COMPILE))
  "Compile the definition of NAME,
or its previous interpreted definition if it is already compiled.
If LAMBDA-EXP is supplied, it is compiled and made the definition of NAME.
If NAME is NIL, LAMBDA-EXP is compiled and the result is just returned."
  (IF (NULL NAME)
      (COMPILE-LAMBDA LAMBDA-EXP (gensymbol "ANON-FN"))
    (LOCKING-RESOURCES-NO-QFASL
      (FILE-OPERATION-WITH-WARNINGS (T ':COMPILE)
        (COMPILER-WARNINGS-CONTEXT-BIND
          (LET (TEM)
            (QC-PROCESS-INITIALIZE)
            (OR LAMBDA-EXP
                (AND (FDEFINEDP NAME)
                     (SETQ TEM (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC NAME)))
                     (TYPECASE TEM
                       (CONS (SETQ LAMBDA-EXP TEM))
                       (CLOSURE
                        (IF (AND (FIND-PACKAGE "INTERPRETER")
                                 (BOUNDP-IN-CLOSURE TEM
                                   (INTERN "INTERPRETER-CLOSURE" "INTERPRETER"))
                                 (SYMEVAL-IN-CLOSURE TEM
                                   (INTERN "INTERPRETER-CLOSURE" "INTERPRETER")))
                            (SETQ LAMBDA-EXP
                                  (SYMEVAL-IN-CLOSURE TEM
                                    (INTERN "ORIGINAL-DEFINITION" "INTERPRETER")))
                          (FERROR "Compilation of closures not yet hacked.")))
                       (COMPILED-FUNCTION
                        (SETQ TEM (ASSQ 'INTERPRETED-DEFINITION
                                        (DEBUGGING-INFO TEM)))
                        (SETQ LAMBDA-EXP (CADR TEM)))
                       (T
                        NIL)))
                (FERROR "Can't find LAMBDA expression for ~S" NAME))
            (LET ((INHIBIT-FDEFINE-WARNINGS T))
              (COMPILE-1 NAME LAMBDA-EXP PROCESSING-MODE))
            NAME))))))

(DEFUN COMPILE-1 (NAME LAMBDA-EXP &OPTIONAL (PROCESSING-MODE 'MACRO-COMPILE)
                  (NAME-FOR-FUNCTION NAME))
  "Compile LAMBDA-EXP and define NAME, while already inside the compiler environment.
NAME-FOR-FUNCTION is recorded as the name of the compiled function
 (the default is NAME).
PROCESSING-MODE is how to compile: COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE."
  (SETQ LAMBDA-EXP (LAMBDA-MACRO-EXPAND LAMBDA-EXP))
  (COND ((ATOM LAMBDA-EXP)
         (compiler-target-switch (FDEFINE NAME LAMBDA-EXP T)))
        ((OR (MEMQ (CAR LAMBDA-EXP) '(LAMBDA NAMED-LAMBDA ZL:SUBST NAMED-SUBST CL:SUBST))
             (AND (EQ (CAR LAMBDA-EXP) 'MACRO)
                  (MEMQ (CADR-SAFE LAMBDA-EXP)
                        '(LAMBDA NAMED-LAMBDA))))
         (QC-TRANSLATE-FUNCTION NAME LAMBDA-EXP
                                (or (and *microcompile-switch*
                                         (getdecl name 'microcompile)
                                         'micro-compile)
                                    processing-mode)
                                'COMPILE-TO-CORE
                                NAME-FOR-FUNCTION))
        (T
         (compiler-target-switch (FDEFINE NAME LAMBDA-EXP T)))))

;;; $$$ new function, called only (mostly?) from the editor <22-Nov-88 smh>
(defun compile-defafun (form)
  (compiler-target-switch (defafun form 'compile-to-core)))

(DEFUN COMPILE-LAMBDA (LAMBDA-EXP &OPTIONAL NAME (PROCESSING-MODE 'MACRO-COMPILE))
  "Compile the function LAMBDA-EXP and return a compiled-function object.
That compiled function will record NAME as its name,
but we do not actually define NAME."
; (AND QC-FILE-IN-PROGRESS      ;Check for condition likely to cause temporary area lossage
;      (FORMAT *ERROR-OUTPUT* "~&COMPILE: Compiler recursively entered, you may lose.~%"))
  (LOCKING-RESOURCES-NO-QFASL
    (FILE-OPERATION-WITH-WARNINGS (T ':COMPILE)
      (COMPILER-WARNINGS-CONTEXT-BIND
        (LET (TEM
              (INHIBIT-FDEFINE-WARNINGS T))
          (QC-PROCESS-INITIALIZE)
          (COMPILE-1 `(:LOCATION ,(LOCF TEM)) LAMBDA-EXP PROCESSING-MODE NAME)
          TEM)))))


(DEFUN COMPILE-LAMBDAS (LAMBDA-EXPS &OPTIONAL NAMES (PROCESSING-MODE 'MACRO-COMPILE))
  "Compile the functions LAMBDA-EXPS and return a list of compiled-function objects.
That compiled function will record NAME as its name,
but we do not actually define NAME."
  (LOCKING-RESOURCES-NO-QFASL
    (FILE-OPERATION-WITH-WARNINGS (T ':COMPILE)
      (COMPILER-WARNINGS-CONTEXT-BIND
        (LET ((INHIBIT-FDEFINE-WARNINGS T))
          (QC-PROCESS-INITIALIZE)
          (DO ((L1 LAMBDA-EXPS (CDR L1))
               (L2 NAMES (CDR L2))
               (RESULTS NIL))
              ((NULL L1)
               (NREVERSE RESULTS))
            (LET ((TEM NIL)
                  (LAMBDA-EXP (CAR L1))
                  (NAME (CAR L2)))
              (COMPILE-1 `(:LOCATION ,(LOCF TEM)) LAMBDA-EXP PROCESSING-MODE NAME)
              (PUSH TEM RESULTS))))))))


;;; Restore the saved old interpreted definition of a function on which
;;; COMPILE was used.

(DEFUN UNCOMPILE (FUNCTION-SPEC &OPTIONAL DONT-UNENCAPSULATE &AUX OLD)
  "Replaces compiled definition of FUNCTION-SPEC with interpreted definition.
If the interpreted function which was compiled is known,
installs that as the definition in place of the compiled one."
  (UNLESS DONT-UNENCAPSULATE
    (SETQ FUNCTION-SPEC (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION-SPEC)))
  (LET ((DEF (FDEFINITION FUNCTION-SPEC)))
    (COND ((EQ (CAR-SAFE DEF) 'MACRO)
           (COND ((SETQ OLD (ASSQ 'INTERPRETED-DEFINITION (DEBUGGING-INFO (CDR DEF))))
                  (FDEFINE FUNCTION-SPEC `(MACRO . ,(CADR OLD)) (NOT DONT-UNENCAPSULATE) T))
                 ((TYPEP (CDR DEF) 'COMPILED-FUNCTION)
                  "No interpreted definition recorded")
                 (T "Not compiled")))
          ((SETQ OLD (ASSQ 'INTERPRETED-DEFINITION (DEBUGGING-INFO DEF)))
           (FDEFINE FUNCTION-SPEC (CADR OLD) (NOT DONT-UNENCAPSULATE) T))
          ((TYPEP DEF 'COMPILED-FUNCTION)
           "No interpreted definition recorded")
          (T "Not compiled"))))

(defparameter *qc-translate-function-hook* nil)

(defun set-qc-translate-function-hook (symbol)
  "A hack for experimenting with a P0 pass. Symbol gets called on FUNCTION-SPEC LAMBDA-EXP
and should return a new LAMBDA-EXP. This gets called only on the explicitly compiled
function, not on so-called breakoff functions which get generated internal to the compiler."
  (check-type symbol symbol)
  (setq *qc-translate-function-hook* symbol))

(defun qc-translate-function-hook (function-spec exp)
  (if *qc-translate-function-hook*
      (funcall *qc-translate-function-hook* function-spec exp)
    exp))

(DEFUN QC-TRANSLATE-FUNCTION (FUNCTION-SPEC EXP QC-TF-PROCESSING-MODE QC-TF-OUTPUT-MODE
                              &OPTIONAL (NAME-FOR-FUNCTION FUNCTION-SPEC))
  "Compile one function.  All styles of the compiler come through here.
QC-TF-PROCESSING-MODE should be MACRO-COMPILE or MICRO-COMPILE.
QC-TF-OUTPUT-MODE is used by LAP to determine where to put the compiled code.
 It is COMPILE-TO-CORE for making an actual FEF, QFASL, REL, or
 QFASL-NO-FDEFINE to simply dump a FEF without trying to define a function
EXP is the lambda-expression.
NAME-FOR-FUNCTION is what the fef's name field should say;
 if omitted, FUNCTION-SPEC is used for that too.
In MACRO-COMPILE mode, the return value is the value of QLAPP for the first function."
 (WHEN COMPILER-VERBOSE
   (FORMAT T "~&Compiling ~S" FUNCTION-SPEC))
 (let ((*just-once-for-style-checkers-per-inner-form-alist* NIL))
   (OBJECT-OPERATION-WITH-WARNINGS (NAME-FOR-FUNCTION)
     (LET ((EH:*ERROR-MESSAGE-HOOK* (LET-CLOSED ((FUNCTION-BEING-PROCESSED NAME-FOR-FUNCTION))
                                      (LAMBDA ()
                                        (AND FUNCTION-BEING-PROCESSED
                                             (FORMAT T "Error occurred while compiling ~S"
                                                     FUNCTION-BEING-PROCESSED)))))
           (COMPILER-QUEUE
             (NCONS
               (MAKE-COMPILER-QUEUE-ENTRY
                 :FUNCTION-SPEC FUNCTION-SPEC
                 :FUNCTION-NAME NAME-FOR-FUNCTION
                 :DEFINITION (qc-translate-function-hook function-spec EXP)
                 :DECLARATIONS LOCAL-DECLARATIONS)))
           (DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA)
           (INSIDE-QC-TRANSLATE-FUNCTION T)
           ;; If compiling to code this is (,function-spec ,fef ,fdefinition-has-already-happened)
           ;;  for fefs which are waiting for their internal functions to be compiled
           ;;  This should really be a part of the compiler-queue-entry data structure
           ;;   -- I'm just a little tired now.
           (PENDING-DEFINITION)
           THIS-FUNCTION-BARF-SPECIAL-LIST
           VARIABLES-LISTS
           )
       (do ((entries compiler-queue (cdr entries))
            entry
            (*compiling-breakoffs-p* nil t))
           ((null entries))
         (setq entry (car entries))
         (SETF (FILL-POINTER QCMP-OUTPUT) 0)
         (LET ((DEFINITION (COMPILER-QUEUE-ENTRY-DEFINITION ENTRY))
               (FUNCTION-TO-DEFINE (COMPILER-QUEUE-ENTRY-FUNCTION-SPEC ENTRY))
               (NAME-FOR-FUNCTION (COMPILER-QUEUE-ENTRY-FUNCTION-NAME ENTRY))
               (LOCAL-DECLARATIONS (COMPILER-QUEUE-ENTRY-DECLARATIONS ENTRY))
               (*OUTER-CONTEXT-VARS* (COMPILER-QUEUE-ENTRY-VARIABLES ENTRY))
               (*OUTER-CONTEXT-LOCAL-FUNCTIONS* (COMPILER-QUEUE-ENTRY-LOCAL-FUNCTIONS ENTRY))
               (*OUTER-CONTEXT-FUNCTION-ENVIRONMENT*
                 (COMPILER-QUEUE-ENTRY-FUNCTION-ENVIRONMENT ENTRY))
               (*OUTER-CONTEXT-PROGDESC-ENVIRONMENT* (COMPILER-QUEUE-ENTRY-PROGDESCS ENTRY))
               (*OUTER-CONTEXT-GOTAG-ENVIRONMENT* (COMPILER-QUEUE-ENTRY-GOTAGS ENTRY))
               THIS-FUNCTION-ARGLIST
               THIS-FUNCTION-ARGLIST-FUNCTION-NAME)
           (OBJECT-OPERATION-WITH-WARNINGS (NAME-FOR-FUNCTION)
             (CATCH-ERROR-RESTART (EH:DEBUGGER-CONDITION
                                    "Give up on compiling ~S" NAME-FOR-FUNCTION)
               (PUSH (QCOMPILE0 DEFINITION FUNCTION-TO-DEFINE
                                (or (not (eq *target-computer* 'lambda-interface))
                                    (EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE))
                                NAME-FOR-FUNCTION)
                     VARIABLES-LISTS)
               (AND PEEP-ENABLE
                    (NEQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE)
                    (compiler-target-switch (PEEP QCMP-OUTPUT FUNCTION-TO-DEFINE)))
               (COND ((NULL HOLDPROG))
                     ((EQ QC-TF-PROCESSING-MODE 'MACRO-COMPILE)
                      (let ((fef (compiler-target-switch
                                   (QLAPP (G-L-P QCMP-OUTPUT) QC-TF-OUTPUT-MODE)))
                            tem)
                        (if (eq qc-tf-output-mode 'compile-to-core)
                            (cond ((and (eq (car-safe function-to-define) ':internal)
                                        (setq tem (si:assoc-equal (cadr function-to-define)
                                                                  pending-definition)))
                                   ;; we depend on someone
                                   (compiler-target-switch
                                     (fdefine `(:internal ,(cadr tem) . ,(cddr function-to-define))
                                              fef))
                                   (push `(,function-to-define ,fef t) pending-definition))
                                  (t
                                   (push `(,function-to-define ,fef) pending-definition)))
                          (or pending-definition (setq pending-definition fef)))))
                     ((EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE)
                      (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
                        (MICRO-COMPILE-INTERNAL (G-L-P QCMP-OUTPUT) QC-TF-OUTPUT-MODE))))))))
       (DOLIST (VL VARIABLES-LISTS)
         (DOLIST (V VL)
           (COND ((OR (STRING= (VAR-NAME V) "IGNORE")
                      (STRING= (VAR-NAME V) "IGNORED"))
                  (OR (ZEROP (VAR-USE-COUNT V))
                      (WARN 'NOT-IGNORED :IMPLAUSIBLE
                        "The variable ~S is bound and not ignored." (VAR-NAME V))))
                 ((GETF (VAR-DECLARATIONS V) 'IGNORE)
                  (OR (ZEROP (VAR-USE-COUNT V))
                      (WARN 'NOT-IGNORED :IMPLAUSIBLE
                        "The variable ~S, which is declared to be ignored, was referenced"
                        (VAR-NAME V))))
                 ((NOT (GET (VAR-NAME V) 'IGNORABLE-VARIABLE))
                  (AND (ZEROP (VAR-USE-COUNT V))
                       (EQ (VAR-TYPE V) 'FEF-LOCAL)
                       (IF (GET (VAR-NAME V) 'LOCAL-FUNCTION-NAME)
                           (WARN 'NOT-USED :IMPLAUSIBLE
                             "The local function ~S is never used."
                             (GET (VAR-NAME V) 'LOCAL-FUNCTION-NAME))
                         (WARN 'NOT-USED :IMPLAUSIBLE
                           "The variable ~S is bound but never used." (VAR-NAME V))))))))
       (COND ((NEQ QC-TF-PROCESSING-MODE 'MACRO-COMPILE)
              NIL)
             ((eq qc-tf-output-mode 'compile-to-core)
              (setq pending-definition (nreverse pending-definition))
              (AND SI:*SNAP-INDEXED-FORWARDS*            ;normally NIL
                   (FBOUNDP 'SI:RELINK-FEF-EXIT-VECTOR)
                   (dolist (e pending-definition)
                     (SI:RELINK-FEF-EXIT-VECTOR (cadr e))))
              (dolist (e (cdr pending-definition))
                (unless (caddr e)
                  (fdefine (car e) (cadr e) t)))
              ;; this is the top-level function.  Must do this last.
              (compiler-target-switch
                (fdefine (caar pending-definition) (cadar pending-definition) t))
              (cadar pending-definition))
             (t
              ;; in the qfasl-output case there is still an internal-function
              ;; screw, in that the half-defined function may be called (as part of fasload,
              ;; or from another process) whilst we are still filling in the breakoffs.
              pending-definition))))))


(DEFUN COMPILE-NOW-OR-LATER (NAME LAMBDA-EXP)
  "Compile LAMBDA-EXP and define NAME, either now or on exit from the compiler.
If not within the compiler, it is done now.
Otherwise, it is done as soon as it is safe."
  (IF INSIDE-QC-TRANSLATE-FUNCTION
      (SETQ COMPILER-QUEUE
            (NCONC COMPILER-QUEUE
                   (NCONS (MAKE-COMPILER-QUEUE-ENTRY
                            :FUNCTION-SPEC NAME
                            :FUNCTION-NAME NAME
                            :DEFINITION LAMBDA-EXP))))
    (COMPILE NAME LAMBDA-EXP)))

; Alist of lexical-ref-code//name-to-be-printed-in-disassembly
(defvar *lexical-ref-code-name-alist*)

;;; Compile an internal lambda which must be passed as an argument
;;; into a separate function, which has its own name which is a list.
;;; That name is returned.
(defun breakoff (x &optional lexical &aux fname fname-to-give local-name)
  (let ((non-instance-vars)
        (sfd self-flavor-declaration)
        ;selfp
        )
    (dolist (home *vars*)
      (and (eq (var-type home) 'fef-local)
           ;; Omit shadowed bindings.
           (eq home (find (var-name home) *vars* :key #'var-name))
           (pushnew (var-name home) non-instance-vars)))
    (dolist (elt *outer-context-vars*)
      (dolist (home elt)
        (push (var-name home) non-instance-vars)))
    (multiple-value-bind (vars-needed-lexically functions-needed-lexically
                          block-names go-tags)
        (cw-top-level-lambda-expression
          x                                     ;form
          (append (if sfd (list* 'self (cddr sfd)))
                  non-instance-vars)            ;variables we're interested in
          (mapcar #'car *local-functions*)      ;functions we're interested in
          *function-environment*)
      (multiple-value-bind (nil decls nil)
          (with-list (env *function-environment*)
            (extract-declarations (if (memq (car x) '(named-subst named-lambda))
                                      (cdddr x) (cddr x))
                                  nil t env))
        (let ((downward (assq 'sys:downward-function decls))
              tem)
          (dolist (v vars-needed-lexically)
            (cond ((and (memq v (cddr sfd)) (not (memq v non-instance-vars)))
                   (unless downward
                     (warn 'instance-variable-used-in-internal-lambda :unimplemented
                           "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~"
                           (memq v (cadr sfd)) v (car sfd) 'self)))
                  ((and (eq v 'self) sfd)
                   (unless downward
                     (warn 'self-used-in-internal-lambda :unimplemented
                           "~~S is being referenced by a lexically closed-over function.~@
                        This will not, of course, work outside of the dynamic scope of ~S.~"
                           'self 'self)))
                  (t
                   ;; Note: if V is not on *VARS*, it must come from an outer lexical level.
                   ;; That is ok, and it still requires this LAMBDA to be lexical to access it.
                   (setq lexical t)
                   (setq tem (find v *vars* :key #'var-name))
                   (when tem
                     (pushnew 'fef-arg-used-in-lexical-closures (var-misc tem) :test #'eq)))))))
      (dolist (f functions-needed-lexically)
        (let ((tem (assq f *local-functions*)))
          (when tem
            (setq lexical t)
            (pushnew 'fef-arg-used-in-lexical-closures
                     (var-misc (cadr tem)) :test #'eq))))
      (dolist (b block-names)
        (let ((tem (find b *progdesc-environment* :key #'progdesc-name)))
          (when tem
            (setq lexical t)
            ;; the flag may already have been set (by P1BLOCK) to a list representing a local variable
            ;; home which will contain the throw tag.  something seems to be sorta duplicating the
            ;; some of the work done by P1BLOCK.
            ;; I admit I don't really understand what is going on here
            ;; but I think this fixes it. -- EFH 4/9/87
            (unless (progdesc-used-in-lexical-closures-flag tem)
              (setf (progdesc-used-in-lexical-closures-flag tem) t)))))
      (dolist (g go-tags)
        (let ((tem (find g *gotag-environment* :key #'gotag-prog-tag)))
          (when tem
            (setq lexical t)
            (setf (gotag-used-in-lexical-closures-flag tem) t)
            (setf (progdesc-used-in-lexical-closures-flag (gotag-progdesc tem)) t))))))
  (if (and (eq (car x) 'named-lambda)
           (not (memq (cadr x) *local-function-map*)))
      (setq local-name (cadr x))
      (setq local-name *breakoff-count*))
  (setq fname `(:internal ,function-to-be-defined ,*breakoff-count*)
        fname-to-give `(:internal ,name-to-give-function ,local-name))
  (push local-name *local-function-map*)
  (incf *breakoff-count*)
  (when lexical
    (incf *lexical-closure-count*))
;  (let ((local-decls local-declarations))
;>> this is already in there.
;    ;; Pass along the parent function's self-flavor declaration.
;    (if sfd (push `(:self-flavor . ,sfd) local-decls))
  (setq compiler-queue
        (nconc compiler-queue
               (ncons
                 (make-compiler-queue-entry
                   :function-spec fname
                   :function-name fname-to-give
                   :definition x
                   :declarations local-declarations

                   ;; The t is a flag for RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES
                   ;; *VARS* at this point is a list of the lexically visible variables
                   ;; See  RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES for the gross detail

                   :variables  (when lexical
                                 (cons (cons 't *vars*) *outer-context-vars*))  ;***
                   :local-functions (and lexical *local-functions*)
                   :progdescs (and lexical *progdesc-environment*)
                   :gotags (and lexical *gotag-environment*)
                   :function-environment (and lexical *function-environment*)
                   ))))
  (let ((tem `(breakoff-function ,fname)))
    (if lexical `(lexical-closure ,tem) tem)))


;;; The problem is this:  Broken off functions are queued for compilation without any
;;; information about the lexical visible variables.  After locating all the local
;;; variables, we put them on an alist and go down the compilation queue to inform
;;; each breakoff function where to find its variables.  Unfortunately, we locate variables
;;; by assq and find-position-in-list.  If two variables have the same name, it will find
;;; the one furthest up on the list.
#| Example:

(defvar *loser-1*)
(defvar *loser-2*)

(defun foo-loser (a b)
  (let ((a 'local-a))
    (setq *loser-1*
          #'(lambda () (format t "~%A = ~S, B = ~S" a b))))
  (let ((b 'local-b))
    (setq *loser-2*
          #'(lambda () (format t "~%A = ~S, B = ~S" a b)))))

|#
;;; After calling foo-loser, if you funcall *loser-1* it will report that a and b are
;;; 'local-a and 'local-b respectively.  The internal lambdas never even see the arguments.
;;; The most straightforward way to fix this would be to cons the *vars* to the
;;; *outer-context-vars* in breakoff.  This would work except that now, instead of finding the
;;; wrong name in the alist, we find the right name in the wrong position
;;; and make a lexical reference to a completly random spot.
;;;

;;; The solution: We tag lexical closures in the compiler queue as
;;; before, but we also include a list of lexically visible variables.
;;; When it comes time to RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES
;;; (which informs each closure on the compilation queue of the higher
;;; context), instead of giving the complete context to the closure, we
;;; replace the variables that are not visible with gensyms so that
;;; using find-position-in-list will not find an incorrect variable with
;;; the same name or a correct name in the wrong position in the list.

(DEFUN RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES ()
  (LET ((VARS-USED (LOOP FOR HOME IN *ALLVARS*
                      WHEN (MEMQ 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
                                 (VAR-MISC HOME))
                        COLLECT HOME)))
    (DOLIST (ELT COMPILER-QUEUE)
      (LET* ((TEM (CLI:MEMBER 'T (COMPILER-QUEUE-ENTRY-VARIABLES ELT) :key #'car))
             (temvars (cdar tem))
             (varlist '()))
        (when tem
          (dolist (var vars-used)
            (block search
              (dolist (seen-var temvars)
                (cond ((eq seen-var var)             ;; same var should be seen
                       (return-from search (push var varlist)))
                      ((eq (var-name seen-var) (var-name var)) ;; var of same name should be shadowed
                       (return-from search
                         (push (make-var :name (gensymbol (var-name var)))
                               varlist)))
                      (t nil)))
              ;; The compiler puts in its own gensyms for purposes of non-local
              ;; lexical exits.  We should not shadow them, so if the variable cannot
              ;; be confused with one that should be seen, we push it.
              (push var varlist)))
          (SETF (CAR TEM) (nreverse varlist)))))
    VARS-USED))

(DEFUN LEXICAL-VAR-P (VAR)
  (DO ((I 0 (1+ I))
       (E *OUTER-CONTEXT-VARS* (CDR E)))
      ((NULL E))
    (WHEN (find VAR (CAR E) :key #'var-name)
      (RETURN T))))

;;; Return a reference to VAR as a lexical variable from a higher context,
;;; or NIL if VAR is not a variable of that sort available now.
(DEFUN TRY-REF-LEXICAL-VAR (VAR &AUX HOME)
  (DO ((I 0 (1+ I))
       (E *OUTER-CONTEXT-VARS* (CDR E)))
      ((NULL E))
    (WHEN (SETQ HOME (find VAR (CAR E) :key #'var-name))
      (INCF (VAR-USE-COUNT HOME))
      (LET ((N (DPB I
                    (BYTE 12. 12.)
                    (FIND-POSITION-IN-LIST HOME (CAR E)))))
        (if (and (symbol-package var)
                 (not (assq n *lexical-ref-code-name-alist*)))
            (push (list n var) *lexical-ref-code-name-alist*))
        (RETURN `(LEXICAL-REF ,N))))))

;;; Given a vars entry HOME, return a suitable pass 1 reference to it
;;; whether it is inherited lexically or not.
(DEFUN TRY-REF-LEXICAL-HOME (HOME &OPTIONAL NAME-FOR-DEBUGGING-INFO)
  (INCF (VAR-USE-COUNT HOME))
  (DO ((I 0 (1+ I))
       (E *OUTER-CONTEXT-VARS* (CDR E)))
      ((NULL E)
       (VAR-LAP-ADDRESS HOME))
    (WHEN (MEMQ HOME (CAR E))
      (LET ((N (DPB I
                    (BYTE 12. 12.)
                    (FIND-POSITION-IN-LIST HOME (CAR E)))))
        (and name-for-debugging-info
             (not (assq n *lexical-ref-code-name-alist*))
             (push (list n name-for-debugging-info) *lexical-ref-code-name-alist*))
        (RETURN
          `(LEXICAL-REF ,N))))))

;;; The SELF-FLAVOR-DECLARATION variable looks like
;;; (flavor-name special-ivars instance-var-names...)
;;; and describes the flavor we are compiling access to instance vars of.
(DEFUN TRY-REF-SELF (VAR)
  (WHEN (MEMQ VAR (CDDR SELF-FLAVOR-DECLARATION))
    ;; If variable is explicitly declared special, use that instead.
    (COND ((LET ((BARF-SPECIAL-LIST ()))
             (SPECIALP VAR))
           (OR (MEMQ VAR (CADR SELF-FLAVOR-DECLARATION))
               (WARN 'SPECIAL-VARIABLE-IS-UNSPECIAL-INSTANCE-VARIABLE :IMPOSSIBLE
                     "The special variable ~S is an instance variable of ~S
but was not mentioned in a ~S in that flavor.
This function will not execute correctly unless the ~S is fixed."
                     VAR (CAR SELF-FLAVOR-DECLARATION)
                     :SPECIAL-INSTANCE-VARIABLES 'DEFFLAVOR))
           (MAKESPECIAL VAR)
           VAR)
          (T
           (SETQ *SELF-REFERENCES-PRESENT* T)
           `(SELF-REF ,(CAR SELF-FLAVOR-DECLARATION) ,VAR)))))

;;;; QCOMPILE0 compiles one function, producing a list of lap code in QCMP-OUTPUT.
;;; The first argument is the lambda-expression which defines the function.
;;;   It must actually be a LAMBDA or NAMED-LAMBDA.  Other things are not allowed.
;;; The second argument is the name of the function.
;;; The third won't be useful till there's a microcompiler.

;;; We expect that DEFAULT-CONS-AREA has been bound to QCOMPILE-TEMPORARY-AREA.
;;; The compiler does ALL consing in that temporary area unless it specifies otherwise.

;;;Variables defined in QCP2, bound here but aside from that used only in QCP2.
(PROCLAIM '(SPECIAL *WITHIN-CATCH* *CALL-BLOCK-PDL-LEVELS* *TAGOUT* PDLLVL MAXPDLLVL
                    *BDEST* *DROPTHRU* *WITHIN-POSSIBLE-LOOP*))

(DEFPROP COMPILER-ARGLIST T SI::DEBUG-INFO)

(DEFUN QCOMPILE0 (EXP FUNCTION-TO-BE-DEFINED GENERATING-MICRO-COMPILER-INPUT-P
                  &OPTIONAL (NAME-TO-GIVE-FUNCTION FUNCTION-TO-BE-DEFINED))
  (LET ((EXP1 EXP)
        (DEF-TO-BE-SXHASHED)
        (LVCNT)

        (MAXPDLLVL 0)                           ;deepest lvl reached by local pdl
        (PDLLVL 0)                              ;Runtine local pdllvl

        ;; p2 things
        (*CALL-BLOCK-PDL-LEVELS*)               ;used only in lambda mode.
        (*open-frames* nil)                     ;used in cross compile mode.
        (*WITHIN-CATCH*)
        (*WITHIN-POSSIBLE-LOOP*)
        (*DROPTHRU* T)                          ;Can drop in if false, flush stuff till tag or
        (*TAGOUT*)

        (ALLGOTAGS)
        (*TLEVEL* T)
        (*P1VALUE* T)                           ;Compiling for all values
        (*BINDP* NIL)                           ;%BIND not yet used in this frame

        (*VARS* ())
        (*ALLVARS* ())
        (*FREEVARS* ())
        (*LOCAL-FUNCTIONS* *OUTER-CONTEXT-LOCAL-FUNCTIONS*)
        (*FUNCTION-ENVIRONMENT* *OUTER-CONTEXT-FUNCTION-ENVIRONMENT*)
        (*PROGDESC-ENVIRONMENT* *OUTER-CONTEXT-PROGDESC-ENVIRONMENT*)
        (*GOTAG-ENVIRONMENT* *OUTER-CONTEXT-GOTAG-ENVIRONMENT*)
        (LL)
        (TLFUNINIT (not (eq *target-computer* 'lambda-interface)))  ;crosscompiling, use FEF-INI-COMP-C not fef initialization.
        (*SPECIALFLAG*)
        (MACROFLAG)
        (*LOCAL-MAP* ())                        ;names of local variables
        (*ARG-MAP* ())                          ;names of arguments
        (*BASE-STACK-SLOTS* ())                 ;aux-slots in cross-compile mode.
        (*STACK-SLOTS* ())                      ;currently existing stack-slots in cross-compile mode.
        (*LOCAL-FUNCTION-MAP* ())               ;names of local functions
        (EXPR-DEBUG-INFO)
        (*FAST-ARGS-POSSIBLE* T)
        (*BREAKOFF-COUNT* 0)                    ;no internal functions yet
        (*LEXICAL-CLOSURE-COUNT* 0)
        (*lexical-ref-code-name-alist* ())
        (MACROS-EXPANDED)                       ;List of all macros found in this function,
                                                ; for the debugging info.
        (SELF-FLAVOR-DECLARATION (cdr (assq :self-flavor local-declarations)))
        (*SELF-REFERENCES-PRESENT* NIL)         ;Bound to T if any SELF-REFs are present
        (LOCAL-DECLARATIONS LOCAL-DECLARATIONS) ;Don't mung ouside value
        (SUBST-FLAG)                            ;T if this is a SUBST being compiled.
                                                ; Always put interpreted defn in debug info.
        (INHIBIT-SPECIAL-WARNINGS INHIBIT-SPECIAL-WARNINGS)
        (*CLOBBER-NONSPECIAL-VARS-LISTS* ())
        wrapped-block-name
        (*placeholder-function-number* 0)
        (*placeholder-alist* nil)
        )
    (BEGIN-PROCESSING-FUNCTION FUNCTION-TO-BE-DEFINED)

    (WHEN (LIST-MATCH-P FUNCTION-TO-BE-DEFINED
                        `(:PROPERTY ,IGNORE :NAMED-STRUCTURE-INVOKE))
      (WARN 'OBSOLETE-PROPERTY :IMPLAUSIBLE
            "NAMED-STRUCTURE-INVOKE, the property name, should not be a keyword."))

    ;; If compiling a macro, compile its expansion function
    ;; and direct lap to construct a macro later.
    (WHEN (EQ (CAR EXP1) 'MACRO)
      (SETQ MACROFLAG T)
      (SETQ EXP1 (CDR EXP1))
      (SETQ DEF-TO-BE-SXHASHED EXP1))
    (UNLESS (MEMQ (CAR EXP1) '(LAMBDA ZL:SUBST CL:SUBST NAMED-LAMBDA NAMED-SUBST))
      (WARN 'FUNCTION-NOT-VALID :FATAL "The definition is not a function at all.")
      (RETURN-FROM QCOMPILE0 NIL))
    (IF (MEMQ (CAR EXP1) '(ZL:SUBST NAMED-SUBST CL:SUBST))
        ;;>> This is pretty bogous
        (SETQ SUBST-FLAG T INHIBIT-SPECIAL-WARNINGS T))
    ;; If a NAMED-LAMBDA, discard the name and save debug-info in special place.
    (WHEN (MEMQ (CAR EXP1) '(NAMED-LAMBDA NAMED-SUBST))
      (SETQ EXPR-DEBUG-INFO (CDR-SAFE (CADR EXP1))
            WRAPPED-BLOCK-NAME (unless (and (listp (fourth exp1))
                                            (eql (first (fourth exp1)) 'block)
                                            (eql (second (fourth exp1))
                                                 (second exp1)))
                                 ;; Not already wrapped with this block name.
                                 (second exp1))
            EXP1 `(,(IF (EQ (CAR EXP1) 'NAMED-LAMBDA) 'LAMBDA 'ZL:SUBST)
                   . ,(CDDR EXP1)))
      ;; Debug info that is equivalent to declarations
      ;; should be turned back into declarations, coming before
      ;; declarations made outside of compilation
      ;; but after anything coming from a DECLARE in the body.
;>> Does not barf at bogoid declarations.
      (DOLIST (ELT (REVERSE EXPR-DEBUG-INFO))
        (LET ((TEM (GET (CAR ELT) 'SI::DEBUG-INFO)))
          (WHEN TEM
            (IF (AND (SYMBOLP TEM) (GET TEM 'SI::DEBUG-INFO))
                (SETQ ELT (CONS TEM (CDR ELT))))
            (PUSH ELT LOCAL-DECLARATIONS)))))

    (SETQ LL (CADR EXP1))                       ;lambda list.
    (unless (cl:listp ll)
      (warn 'invalid-lambda-list :impossible
            "~S is supposed to be a lambda-list"
            ll)
      (setq ll ()
            exp1 `(,(car exp1) () . ,(cddr exp1))))

    ;; Record the function's arglist for warnings about recursive calls.
    (OR THIS-FUNCTION-ARGLIST-FUNCTION-NAME
        (SETQ THIS-FUNCTION-ARGLIST-FUNCTION-NAME NAME-TO-GIVE-FUNCTION
              THIS-FUNCTION-ARGLIST LL))

    ;; Extract documentation string and declarations from the front of the body.
    (MULTIPLE-VALUE-BIND (BODY LOCAL-DECLARATIONS DOCUMENTATION)
        (WITH-LIST (ENV *FUNCTION-ENVIRONMENT*)
          (EXTRACT-DECLARATIONS (CDDR EXP1) LOCAL-DECLARATIONS T ENV))
      (IF WRAPPED-BLOCK-NAME
          (SETQ BODY `((BLOCK ,WRAPPED-BLOCK-NAME . ,BODY))))

      (SETQ SELF-FLAVOR-DECLARATION
            (CDR (ASSQ ':SELF-FLAVOR LOCAL-DECLARATIONS)))
      ;; If the user just did (declare (:self-flavor flname)),
      ;; compute the full declaration for that flavor.
      (WHEN (AND SELF-FLAVOR-DECLARATION
                 (NULL (CDR SELF-FLAVOR-DECLARATION)))
        (SETQ SELF-FLAVOR-DECLARATION
              (CDR (SI::FLAVOR-DECLARATION (CAR SELF-FLAVOR-DECLARATION)))))
      ;; Actual DEFMETHODs must always have SELF-FLAVOR
      (WHEN (EQ (CAR-SAFE FUNCTION-TO-BE-DEFINED) ':METHOD)
        (SETQ *SELF-REFERENCES-PRESENT* T))

      ;; Process &KEY and &AUX vars, if there are any.
      (WHEN (OR (MEMQ '&KEY LL) (MEMQ '&AUX LL))
        ;; Put arglist together with body again.
        (LET ((LAMEXP `(LAMBDA ,LL (DECLARE . ,LOCAL-DECLARATIONS) . ,BODY)))
          ;; If there are keyword arguments, expand them.
          (AND (MEMQ '&KEY LL)
               (SETQ LAMEXP (EXPAND-KEYED-LAMBDA LAMEXP)))
          ;; Now turn any &AUX variables in the LAMBDA into a LET* in the body.
          (SETQ LAMEXP (P1AUX LAMEXP))
          ;; Separate lambda list and body again.
          (SETQ LL (CADR LAMEXP) BODY (CDDR LAMEXP)))
        ;; Can just pop off the declarations as we have them already from above
        (DO () ((NEQ (CAR-SAFE (CAR BODY)) 'DECLARE))
          (POP BODY)))

      ;; Create the arglist accessable through (arglist foo 'compile)
      (LET ((L ()))
        (DOLIST (X (CADR EXP1))
          (PUSH (COND ((EQ X '&AUX) (RETURN))
                      ((ATOM X) X)              ;foo, &optional, etc
                      ((CONSP (CAR X))  ;((:foo bar)), ((:foo bar) baz foop), etc
                       (IF (CADR X)
                           (LIST (CAAR X) (CADR X))
                           (CAAR X)))
                      (T                        ;(foo), (foo bar), (foo bar foop)
                       (IF (CADR X)
                           (LIST (CAR X) (CADR X))
                         (CAR X))))
                L))
        (SETQ L (NREVERSE L))
        (and (boundp '*compilation-environment*)
             *compilation-environment*
             (setf (getf (gethash THIS-FUNCTION-ARGLIST-FUNCTION-NAME
                                  (compilation-environment-plist-hashtab *compilation-environment*))
                         'compiler-arglist)
                   l))
        (UNLESS (EQUAL L LL)
          (PUSH `(COMPILER-ARGLIST . ,L) LOCAL-DECLARATIONS)))

      ;; Now process the variables in the lambda list, after the local declarations.
      (SETQ LL (P1SBIND LL 'FEF-ARG-REQ NIL NIL LOCAL-DECLARATIONS))
      (COND ((NOT (NULL (CDR BODY)))
             (SETQ EXP1 `(PROGN . ,BODY)))
            ((SETQ EXP1 (CAR BODY))))

      (SETQ EXP1 (P1 EXP1))                     ;Do pass 1 to single-expression body

      (push (cons 'placeholder-to-micro-function-table *placeholder-alist*) local-declarations)

      (SETQ LVCNT (compiler-target-switch (ASSIGN-LAP-ADDRESSES)))      ;in cross-compile mode, number of aux-stack slots.

      ;; Now that we know all the variables needed by lexical closures,
      ;; make a list of them and put them into the entries in COMPILER-QUEUE
      ;; for each of those lexical closures.
      (let ((*variables-used-in-lexical-closures* (IF (ZEROP *LEXICAL-CLOSURE-COUNT*) ()
                                                    (RECORD-VARIABLES-USED-IN-LEXICAL-CLOSURES))))
        (OUTF `(MFEF ,FUNCTION-TO-BE-DEFINED ,*SPECIALFLAG*
                     ,(ELIMINATE-DUPLICATES-AND-REVERSE *ALLVARS*)
                     ,*FREEVARS* ,NAME-TO-GIVE-FUNCTION))
        (IF MACROFLAG (OUTF `(CONSTRUCT-MACRO)))
        (OUTF `(QTAG S-V-BASE))
        (OUTF `(S-V-BLOCK))
        (IF (AND SELF-FLAVOR-DECLARATION *SELF-REFERENCES-PRESENT*)
            (OUTF `(SELF-FLAVOR . ,SELF-FLAVOR-DECLARATION)))
        (OUTF `(QTAG DESC-LIST-ORG))
        (OUTF `(PARAM LLOCBLOCK
                      ,(IF (or (ZEROP *LEXICAL-CLOSURE-COUNT*)
                               (neq *target-computer* 'lambda-interface))  ;cross compiling, this is number of stack slots.
                           LVCNT
                           ;; One extra for the lexical frame pointer.
                           ;; One extra for the unshared frame list.
                           (+ lvcnt *lexical-closure-count* 2)
;                        (+ LVCNT (* 4 *LEXICAL-CLOSURE-COUNT*) 3
;                           (LENGTH *VARIABLES-USED-IN-LEXICAL-CLOSURES*))
                         )))
        (OUTF `(A-D-L))
        (OUTF `(QTAG QUOTE-BASE))
        (OUTF `(ENDLIST))                               ;Lap will insert quote vector here
        (WHEN (NOT (ZEROP *LEXICAL-CLOSURE-COUNT*))
          (OUTF `(VARIABLES-USED-IN-LEXICAL-CLOSURES
                   . ,(REVERSE (MAPCAR (LAMBDA (HOME)
                                         (LET ((TEM (VAR-LAP-ADDRESS HOME)))
                                           (CASE (CAR TEM)
                                             (ARG (CADR TEM))
                                             (T (unless (fixnump (cadr tem))    ;|||smh 22sep88
                                                  ;; Without this test, a nonnumeric cadr here
                                                  ;; crashed the entire lambda!
                                                  (error "compiler error -- variables-used-in-lexical-closures: ~s"
                                                         tem))
                                                (%LOGDPB 1 %%Q-BOXED-SIGN-BIT (CADR TEM))))))
                                       *VARIABLES-USED-IN-LEXICAL-CLOSURES*)))))
        ;; Set up the debug info from the local declarations and other things
        (LET ((DEBUG-INFO ()) TEM)
            (AND DOCUMENTATION (PUSH `(DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
          (DOLIST (DCL LOCAL-DECLARATIONS)
            (WHEN (SYMBOLP (CAR DCL))
              (SETQ TEM (GET (CAR DCL) 'SI::DEBUG-INFO))
              (IF (AND (SYMBOLP TEM) (GET TEM 'SI::DEBUG-INFO))
                  (SETQ DCL (CONS TEM (CDR DCL))))
              (UNLESS (ASSQ (CAR DCL) DEBUG-INFO)
                (PUSH DCL DEBUG-INFO))))
          ;; Propagate any other kinds of debug info from the expr definition.
          (DOLIST (DCL EXPR-DEBUG-INFO)
            (UNLESS (ASSQ (CAR DCL) DEBUG-INFO)
              (PUSH DCL DEBUG-INFO)))
          (WHEN (PLUSP *BREAKOFF-COUNT*)                ; local functions
            (LET ((INTERNAL-OFFSETS (MAKE-LIST *BREAKOFF-COUNT*)))
              (OUTF `(BREAKOFFS ,INTERNAL-OFFSETS))
              (PUSH `(:INTERNAL-FEF-OFFSETS . ,INTERNAL-OFFSETS) DEBUG-INFO)))
          ;; Include the local and arg maps if we have them.
          ;; They were built by ASSIGN-LAP-ADDRESSES.
          (WHEN *LOCAL-MAP* (PUSH `(LOCAL-MAP ,*LOCAL-MAP*) DEBUG-INFO))
          (WHEN *ARG-MAP* (PUSH `(ARG-MAP ,*ARG-MAP*) DEBUG-INFO))
          (WHEN *LOCAL-FUNCTION-MAP* (PUSH `(LOCAL-FUNCTION-MAP ,(NREVERSE *LOCAL-FUNCTION-MAP*))
                                           DEBUG-INFO))
          (when *lexical-ref-code-name-alist*
            (push `(lexical-ref-map . , *lexical-ref-code-name-alist*) debug-info))
          ;; Include list of macros used, if any.
          (WHEN MACROS-EXPANDED
            (LET ((MACROS-AND-SXHASHES
                    (MAPCAR (LAMBDA (MACRONAME)
                              (LET ((HASH (EXPR-SXHASH MACRONAME)))
                                (IF (OR HASH (CONSP MACRONAME))
                                    (LIST MACRONAME HASH)
                                    MACRONAME)))
                            MACROS-EXPANDED)))
              (IF QC-FILE-RECORD-MACROS-EXPANDED
                  (PROGN
                    ;; If in QC-FILE, put just macro names in the function
                    ;; but put the names and sxhashes into the file's list.
                    (PUSH `(:MACROS-EXPANDED ,MACROS-EXPANDED) DEBUG-INFO)
                    (DOLIST (M MACROS-AND-SXHASHES)
                      (OR (SI:MEMBER-EQUAL M QC-FILE-MACROS-EXPANDED)
                          (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
                            (PUSH (COPY-TREE M) QC-FILE-MACROS-EXPANDED)))))
                (PUSH `(:MACROS-EXPANDED ,MACROS-AND-SXHASHES)
                      DEBUG-INFO))))
          (AND (OR (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
                   SUBST-FLAG)
               (eq *host-computer* *target-computer*)   ;||| smh 30sep88
               (PUSH `(INTERPRETED-DEFINITION ,EXP) DEBUG-INFO))
          (WHEN SUBST-FLAG
            ;; Added the next form ||| smh 30sep88
            ;; Why must the lambda treat _everything_ as a special form??
            (when (and (boundp '*compilation-environment*)      ; $$$ Fix to change by keith&smh on 7nov <08-Nov-88 smh>
                       *compilation-environment*)
              (setf (gethash function-to-be-defined
                             (compiler:compilation-environment-macro-hashtab
                               compiler:*compilation-environment*))
                    exp))
            (LET* ((ARGS-INFO (ARGS-INFO EXP))
                   (DUMMY-FORM (CONS 'FOO
                                     (MAKE-LIST (+ (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)
                                                   (IF (LDB-TEST %ARG-DESC-EVALED-REST ARGS-INFO)
                                                       1 0))
                                                :INITIAL-ELEMENT '(gensymbol "DUMMY")))))
              ;;>> this somewhat bogous. The environment should be much hairier. Or should it?
              (UNLESS (WITH-LIST (ENV *FUNCTION-ENVIRONMENT*)
                        ;;>> BULLSHIT. this cannot hope to work. sigh.
                        (EQUAL (SI::SUBST-EXPAND EXP DUMMY-FORM ENV NIL)
                               (SI::SUBST-EXPAND EXP DUMMY-FORM ENV T)))
                ;; If simple and thoughtful substitution give the same result
                ;; even with the most intractable arguments,
                ;; we need not use thoughtful substitution for this defsubst.
                ;; Otherwise, mark it as requiring thoughtful substitution.
                (PUSH '(:NO-SIMPLE-SUBSTITUTION T) DEBUG-INFO))))
          ;; Compute the sxhash now, after all displacing macros have been displaced
          (AND MACROFLAG
               (eq *host-computer* *target-computer*)   ;||| (I'm unsure about this one) - smh 30sep88
               (PUSH `(:EXPR-SXHASH ,(FUNCTION-EXPR-SXHASH DEF-TO-BE-SXHASHED)) DEBUG-INFO))
          ;; Added the next form ||| smh 30sep88
          (when (and macroflag
                     (boundp '*compilation-environment*)
                     *compilation-environment*) ; $$$ Fix to change by keith&smh on 7nov <08-Nov-88 smh>
            (cross-define-a-macro function-to-be-defined exp))
          ;; If we aren't going to mark this function as requiring a mapping
          ;; table, provide anyway some info that the user declared it wanted one.
          (AND SELF-FLAVOR-DECLARATION (NOT *SELF-REFERENCES-PRESENT*)
               (eq *host-computer* *target-computer*)   ;||| (I'm unsure about this one) - smh 30sep88
               (PUSH `(:SELF-FLAVOR ,(CAR SELF-FLAVOR-DECLARATION)) DEBUG-INFO))
          (OUTF `(DEBUG-INFO . ,DEBUG-INFO)))
        (OUTF `PROGSA)
        (compiler-target-switch                   ;for LAMBDA, just goes to P2SBIND.
          (P2SBIND-FOR-TOPLEVEL LL *VARS* NIL))   ;Can compile initializing code
        (LET ((*LEXICAL-CLOSURE-COUNT* 0)
              (*highest-lexical-closure-disconnected* 0))
          (compiler-target-switch (P2 EXP1 'D-RETURN)))                 ;Do pass 2
        (LET* ((MXPDL (1+ MAXPDLLVL))
               (APPARENT-MAXIMUM-TOTAL-PDL-FRAME-SIZE
                 (+ MXPDL (LENGTH *LOCAL-MAP*) (LENGTH *ARG-MAP*))))
          (OUTF `(PARAM MXPDL ,MXPDL))
          (WHEN (> APPARENT-MAXIMUM-TOTAL-PDL-FRAME-SIZE 225.)
            (WARN 'PDL-FRAME-TOO-LARGE :fatal
                  "PDL frame at runtime limited to 256. (225. for safety)"))))
      *ALLVARS*)))

(defun cross-define-a-macro (function-to-be-defined exp)
  (if (symbolp function-to-be-defined)
      (setf (gethash function-to-be-defined
                     (compiler:compilation-environment-macro-hashtab
                       compiler:*compilation-environment*))
            exp)
    (progn
      (when (eq (first function-to-be-defined) ':property)
        (setq function-to-be-defined (cdr function-to-be-defined)))
      (setf (getf (gethash (first function-to-be-defined)
                           (compiler:compilation-environment-plist-hashtab
                             compiler:*compilation-environment*))
                  (second function-to-be-defined))
            exp))))

(defun clear-tlevel ()
  (setq *tlevel* nil)
 ;by now, any DECLARE-FLAVOR-INSTANCE-VARIABLES had better have been seen.
  (SETQ SELF-FLAVOR-DECLARATION
        (CDR (ASSQ ':SELF-FLAVOR LOCAL-DECLARATIONS)))
  ;; If the user just did (declare (:self-flavor flname)),
  ;; compute the full declaration for that flavor.
  (WHEN (AND SELF-FLAVOR-DECLARATION
             (NULL (CDR SELF-FLAVOR-DECLARATION)))
    (SETQ SELF-FLAVOR-DECLARATION
          (CDR (SI::FLAVOR-DECLARATION (CAR SELF-FLAVOR-DECLARATION))))))


(DEFUN FUNCTION-EXPR-SXHASH (FUNCTION)
  (LET ((FUNCTION (IF (AND (CONSP FUNCTION) (EQ (CAR FUNCTION) 'MACRO))
                      (CDR FUNCTION) FUNCTION)))
    (COND ((TYPEP FUNCTION 'COMPILED-FUNCTION)
           (OR (CADR (ASSQ ':EXPR-SXHASH (DEBUGGING-INFO FUNCTION)))
               (LET ((IDEF (CADR (ASSQ 'INTERPRETED-DEFINITION (DEBUGGING-INFO FUNCTION)))))
                 (AND IDEF (FUNCTION-EXPR-SXHASH IDEF)))))
          ((NULL FUNCTION) NIL)
          ((SYMBOLP FUNCTION) (EXPR-SXHASH FUNCTION))
          ((CONSP FUNCTION)
           (SXHASH (SI::LAMBDA-EXP-ARGS-AND-BODY FUNCTION))))))

;;; This must follow FUNCTION-EXPR-SXHASH or else FASLOAD bombs out
;;; loading this file for the first time.
(DEFUN EXPR-SXHASH (FUNCTION-SPEC)
  "Return the SXHASH of the interpreted definition of FUNCTION-SPEC.
If FUNCTION-SPEC's definition is compiled, the interpreted definition
or its SXHASH may be remembered in the debugging info.
If neither is remembered, the value is NIL."
  (FUNCTION-EXPR-SXHASH (DECLARED-DEFINITION FUNCTION-SPEC)))

;;; This should be called as each function is begun to be compiled
(DEFUN BEGIN-PROCESSING-FUNCTION (NAME)
  (COMPILATION-DEFINE NAME))

;;; There can be duplicates of local vars on *allvars* because of the variable overlaping hack.
;;; Don't disturb special vars.
(DEFUN ELIMINATE-DUPLICATES-AND-REVERSE (VAR-LIST)
  (PROG (ANS)
     L  (COND ((NULL VAR-LIST) (RETURN ANS))
              ((NULL (DOLIST (V ANS)
                       (IF (AND (EQ (VAR-NAME V) (VAR-NAME (CAR VAR-LIST)))
                                (NOT (EQ (CAR (VAR-LAP-ADDRESS V)) 'SPECIAL))
                                (EQUAL (VAR-LAP-ADDRESS V) (VAR-LAP-ADDRESS (CAR VAR-LIST))))
                           (RETURN T))))        ;this a local duplicate, flush
               (SETQ ANS (CONS (CAR VAR-LIST) ANS))))
        (SETQ VAR-LIST (CDR VAR-LIST))
        (GO L)))

#|
Expand functions that want keyword arguments.
Make them take &REST args instead, and give them code to look up the keywords.

starting from this
(DEFUN FOO (X &REST Y &KEY MUMBLE (BLETCH T BLETCHP) &AUX BAZZZ)
   BODY)

We create this:
(We call with the rest arg starting with 'permutation-table <table>
 before the first keyword, if we want to memoize the keyword lookup.
 The permutation table gets filled with the index of each specified
 keyword in the list of allowed keywords, and then it is used to
 permute the args, rather than looking up the keywords again.
 The leader of the permutation table records the fef that the table
 was computed for.  If the function definition changes, the table
 is recomputed).
(DEFUN FOO (X &REST Y &AUX (MUMBLE KEYWORD-GARBAGE) (BLETCH T) BLETCHP)
  (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
                               Y '(:MUMBLE :BLETCH)
                               NIL              ;T if &ALLOW-OTHER-KEYS
                               2)               ;1st 2 keywords required.
  (AND (EQ MUMBLE KEYWORD-GARBAGE) (FERROR ...))
  ((LAMBDA (&AUX BAZZZ)
     BODY)))

|#

;;; Given a lambda which uses &KEY, return an equivalent one
;;; which does not use &KEY.  It takes a &REST arg instead
;;; (though if the original one had a rest arg, it uses that one).
;;; If there is no ARGLIST declaration for this function, we make one
;;; so that the user is still told that the function wants keyword args.
(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
        MAYBE-REST-ARG KEYCHECKS
        PSEUDO-KEYNAMES)
    (IF (EQ (CAR LAMBDA-EXP) 'LAMBDA)
        (SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP))
        (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP)))  ;named-lambda
    (MULTIPLE-VALUE-BIND (POSITIONAL-ARGS NIL AUXVARS
                          REST-ARG nil
                          KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
        (DECODE-KEYWORD-ARGLIST LAMBDA-LIST)
      (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
      (MULTIPLE-VALUE-BIND (NIL DECLS)
          (WITH-LIST (ENV *FUNCTION-ENVIRONMENT*)
            (EXTRACT-DECLARATIONS BODY NIL NIL ENV))
        ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
        ;; and check explicitly whether that has been overridden.
        ;; If the arg is optional
        ;; and the initial value is a constant, we can really init it to that.
        ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
        ;; after all keywords are decoded, we bind the intended variable, in sequence.
        ;; However a var that can shadow something (including any special var)
        ;; must always be replaced with a dummy.
        (DO ((KIS KEYINITS (CDR KIS))
             (KNS KEYNAMES (CDR KNS))
             (PKNS PSEUDO-KEYNAMES (CDR PKNS))
             (KFS KEYFLAGS (CDR KFS)))
            ((NULL KNS))
          (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
                (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
            (OR (AND (NULL KEYFLAG)
                     (CONSTANTP KEYINIT)
                     (NOT (find KEYNAME *VARS* :key #'var-name))
                     (NOT (LEXICAL-VAR-P KEYNAME))
                     (NOT (SPECIALP KEYNAME)))
                (PROGN (SETF (CAR KIS) 'SI::KEYWORD-GARBAGE)
                       (SETQ PSEUDO-KEYNAME (gensymbol keyname))
                       (SETF (CAR PKNS) PSEUDO-KEYNAME)
                       (PUSH `(,KEYNAME
                               (COND ((EQ ,PSEUDO-KEYNAME SI::KEYWORD-GARBAGE)
                                      ,KEYINIT)
                                     (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
                                        ,PSEUDO-KEYNAME)))
                             KEYCHECKS)))))
        (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
        (SETQ KEYCHECKS (NREVERSE KEYCHECKS))

        ;; If the user didn't ask for a rest arg, make one for the
        ;; outer function anyway.
        (OR REST-ARG (SETQ REST-ARG (gensymbol "REST")
                           MAYBE-REST-ARG (LIST '&REST REST-ARG)))
        `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG)
           (DECLARE . ,DECLS)
           (LET* (,@(MAPCAR (LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
                  ,@KEYFLAGS)
             (DECLARE . ,DECLS)
;            (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;                   (OR (%PERMUTE-ARGS)
;                       (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;                                (CDR ,REST-ARG)
;                                (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;                                ',KEYKEYS)
;                              (%PERMUTE-ARGS)))
;                   ;; If the function really wants the rest arg,
;                   ;; flush the permutation table and its keyword.
;                   ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;                  (T
             ,(case *target-computer*
                (k (generate-k-keyword-args-decode pseudo-keynames rest-arg keykeys allow-other-keys))
                (otherwise
                 `(WHEN ,REST-ARG
                    (SI::STORE-KEYWORD-ARG-VALUES ;; kludgey-compilation-variable-location is just like
                      ;; variable-location except that it doesn't increment
                      ;; the var-use-count of its arg
                      (KLUDGEY-COMPILATION-VARIABLE-LOCATION
                        ,(CAR PSEUDO-KEYNAMES))
                      ,REST-ARG ',KEYKEYS
                      ,ALLOW-OTHER-KEYS))))
             (LET* ,KEYCHECKS
               (DECLARE . ,DECLS)
               ((LAMBDA ,AUXVARS . ,BODY)))))))))

;; just a lobotomized (:property variable-location p1).  Yuck!
(DEFUN (:PROPERTY KLUDGEY-COMPILATION-VARIABLE-LOCATION P1) (FORM &AUX TEM TEM1)
  (SETQ FORM (CADR FORM))
  (SETQ TEM (COND ((SETQ TEM1 (find FORM *VARS* :key #'var-name))
                   (AND (EQ (VAR-KIND TEM1) 'FEF-ARG-FREE)
                        (ZEROP (VAR-USE-COUNT TEM1))
                        (PUSH (VAR-NAME TEM1) *FREEVARS*))
                   (VAR-LAP-ADDRESS TEM1))
                  ((SPECIALP FORM) FORM)
                  (T (BARF FORM "Lossage in keyed-lambda compilation"))))
  (cond ((SYMBOLP TEM)
         `(%EXTERNAL-VALUE-CELL ',TEM))
        (t (when (eq *target-computer* 'k)
                    (case (first tem)
                      (local-ref
                       (push 'variable-location (var-misc (second tem))))))
           `(VARIABLE-LOCATION ,TEM))))

;This optimization isn't in use yet, and may never be
;  if microcoding STORE-KEYWORD-ARG-VALUES is winning enough.

;;Given a permutation table for keyword args whose contents are garbage,
;;and the actual arglist with keywords,
;;compute the contents of the permutation table
;;based on calling the fef NEW-FEF.
;(DEFUN RECOMPUTE-KEYWORD-PERMUTATION-TABLE (TABLE-AND-ARGS NEW-FEF KEYWORDS)
;  (LET ((TABLE (CAR TABLE-AND-ARGS)))
;    (DO ((I 0 (1+ I))
;        (ARGS1 (CDR TABLE-AND-ARGS) (CDDR ARGS1)))
;       ((NULL ARGS1)
;        (SETF (ARRAY-LEADER TABLE 0) NEW-FEF))
;      (LET ((KEYWORD (CAR ARGS1)))
;       (DO (INDEX) (())
;         (SETQ INDEX (FIND-POSITION-IN-LIST KEYWORD KEYWORDS))
;         (AND INDEX (RETURN (SETF (AREF TABLE I) INDEX)))
;         (SETQ KEYWORD (CERROR T NIL :UNDEFINED-ARG-KEYWORD
;                               "Keyword arg keyword ~S unrecognized"
;                               KEYWORD)))))))

;; Given a form that is a call to a function which takes keyword args,
;; stick in a permutation table, if the keyword names are constant.
;; The question of how calls to such functions are detected is still open.
;(DEFUN OPTIMIZE-KEYWORD-CALL (FORM)
;  (LET ((ARGS-INFO (ARGS-INFO (CAR FORM))))
;    (LET ((KEYARGS (CDR (NTHCDR (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO) FORM))))
;      (COND ((DO ((TAIL KEYARGS (CDDR TAIL)))
;                ((NULL TAIL) T)
;              (OR (QUOTEP (CAR TAIL)) (RETURN NIL)))
;            ;; Here if every keyword name is quoted.
;            `(,@(LDIFF FORM KEYARGS)
;              'PERMUTATION-TABLE
;              ',(MAKE-ARRAY (TRUNCATE 2 (LENGTH KEYARGS))
;                            :LEADER-LENGTH 1 :TYPE ART-8B)
;              . ,KEYARGS))
;           (T FORM)))))

;; Temporary definition for what ought to be defined in the microcode.
;; Keyed functions' expansions call this, but until calls are open-coded
;; the arguments will never be such as to make the call actually happen.
;; The open-coding won't be installed until the ucode function works.
;; Meanwhile this prevents warning messages when keyed functions are compiled.
;(DEFUN %PERMUTE-ARGS () (FERROR "%PERMUTE-ARGS called"))


;;;; Pass 1.
;;; We expand all macros and perform source-optimizations
;;; according to the OPTIMIZERS properties.  Internal lambdas turn into progs.
;;; Free variables are made special and put on *FREEVARS* unless on INSTANCEVARS.
;;; PROGs are converted into an internal form which contains pointers
;;; to the *VARS* and *GOTAG-ENVIRONMENT* lists of bound variables and prog tags.
;;; All self-evaluating constants (including T and NIL) are replaced by
;;; quote of themselves.
;;; *P1VALUE* is NIL when compiling a form whose value is to be discarded.
;;; *P1VALUE* is PREDICATE when compiling for predicate value (nilness or non-nilness)
;;; *P1VALUE* is an integer n when compiling for at most n values (n  1)
;;; *P1VALUE* is T when all values are to be passed back
;;; Some macros and optimizers look at it.

(DEFUN P1V (FORM &OPTIONAL (*P1VALUE* T) DONT-OPTIMIZE)
  (P1 FORM DONT-OPTIMIZE))

(defparameter *integrate-constants?* t          ;changed to T - smh&keith 20oct88
  "If T, constants get substituted into compiled code.")

(defun integrable-constant? (var)
  (and (or *integrate-constants?*
           (and local-declarations
                (let ((decl (assq 'si:integrate-constants local-declarations)))
                  (or (null (rest decl))
                      (memq var (rest decl))))))
       ;;; ||| changed get to getdecl for cross compilation - smh&keith 20oct88
       (getdecl var 'si:system-constant)
       (boundp var)
       (let ((value (eval var)))
         (or (symbolp value)
             (numberp value)
             ;;; ||| also eqlable - smh&keith 20oct88
             (characterp value)))
       ))

(DEFUN P1 (FORM &OPTIONAL DONT-OPTIMIZE &AUX TEM)
  (unless (eq dont-optimize 'dont-rewrite)
    (setq form (compiler-optimize form dont-optimize)))
  (SETQ FORM (COND ((ATOM FORM)
                    (COND ((SELF-EVALUATING-P FORM)
                           `',FORM)
                          ((SETQ TEM (cl:find FORM *VARS* :key #'var-name))
                           (AND (EQ (VAR-KIND TEM) 'FEF-ARG-FREE)
                                (ZEROP (VAR-USE-COUNT TEM))
                                (PUSH (VAR-NAME TEM) *FREEVARS*))
                           (INCF (VAR-USE-COUNT TEM))
                           (VAR-LAP-ADDRESS TEM))
                          ((TRY-REF-SELF FORM))
                          ((and (symbolp form)          ;smh 22aug88
                                (setq tem (getdecl form 'compiler:constant-for-compilation))
                                (consp tem)             ;We expect a list of the value
                                ;;Type-check constant-value:  |||Keith 10oct88
                                (or (numberp (setq tem (car tem)))      ;EQL-equivalent objects; smh 13sep88
                                    (symbolp tem)
                                    (characterp tem)))
                           ;;Got valid constant value:
                           `(quote ,tem))
                          ((integrable-constant? form)
                           `(QUOTE ,(eval form)))
                          ((SPECIALP FORM)
                           (MAKESPECIAL FORM) FORM)
                          ((TRY-REF-LEXICAL-VAR FORM))
                          ((and (symbolp form)
                                (eq *target-computer* 'k)
                                ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
                                (get form :register))
                           form)
                          (T (MAKESPECIAL FORM) FORM)))
                   ((EQ (CAR FORM) 'QUOTE) FORM)
                   ;; Certain constructs must be checked for here
                   ;; so we can call P1 recursively without setting *TLEVEL* to NIL.
                   ((NOT (ATOM (CAR FORM)))
                    ;; Expand any lambda macros -- just returns old function if none found
                    (LET ((FCTN (CAR FORM)))
                      (OR (SYMBOLP (CAR FCTN))
                          (WARN 'BAD-FUNCTION-CALLED :IMPOSSIBLE
                                "There appears to be a call to a function whose CAR is ~S."
                                (CAR FCTN)))
                      (IF (MEMQ (CAR FCTN) '(LAMBDA NAMED-LAMBDA))
                          (P1LAMBDA FCTN (CDR FORM))
                        ;; Old Maclisp evaluated functions.
                        (WARN 'EXPRESSION-AS-FUNCTION :VERY-OBSOLETE
                              "The expression ~S is used as a function; use ~S."
                              (CAR FORM) 'FUNCALL)
                        (P1 `(FUNCALL . ,FORM)))))
                   ((NOT (SYMBOLP (CAR FORM)))
                    (WARN 'BAD-FUNCTION-CALLED :IMPOSSIBLE
                          "~S is used as a function to be called." (CAR FORM))
                    (P1 `(PROGN . ,(CDR FORM))))
                   ((SETQ TEM (ASSQ (CAR FORM) *LOCAL-FUNCTIONS*))
                    (INCF (VAR-USE-COUNT (CADR TEM)))
                    `(funcall ,(try-ref-lexical-home (cadr tem) `(function ,(car form)))
                              . ,(loop for x in (cdr form)
                                    collect (p1 x 1))))
                   ((MEMQ (CAR FORM) '(PROG PROG*))
                    (P1PROG FORM))
                   ((MEMQ (CAR FORM) '(LET LET*))
                    (P1LET FORM))
                   ((EQ (CAR FORM) 'BLOCK)
                    (P1BLOCK FORM))
                   ((EQ (CAR FORM) 'TAGBODY)
                    (P1TAGBODY FORM))
                   ((EQ (CAR FORM) '%POP)       ;P2 specially checks for this
                    FORM)
                   (T
                    (if *tlevel* (clear-tlevel))
                    ;; Check for functions with special P1 handlers.
                    (setq form (IF (SETQ TEM (or (and (neq *target-computer* 'lambda-interface)
                                                      (get (car form) 'p1cross))
                                                 (GET (CAR FORM) 'P1)))
                                   (FUNCALL TEM FORM)
                                   (P1ARGC FORM (GETARGDESC (CAR FORM)))))
                    ;;; $$$ Check now that FORM is still a cons before further processing.
                    ;;; Special handler just above may have reduced FORM to an atom,
                    ;;; e.g. transforming (COND (OK)) -> OK.  <21-Nov-88 keith>
                    (if (and (consp form) (setq tem (get (car form) 'qintcmp)))
                        (let ((len (length (cdr form))))
                          (cond ((= len tem)
                                 form)
                                ;; Have alread barfed about wrong-number-of-args
                                ;;  Just silently append nils or truncate
                                ((< len tem)
                                 (append form (make-list (- tem len)
                                                         :initial-element ''())))
                                (t
                                 (subseq form 0 (1+ tem)))))
                      form)
;                   (IF (NOT (AND ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
;                         (find (CAR FORM) *VARS* :key #'var-name)
;                                 (NULL (FUNCTION-P (CAR FORM)))))
;                       (P1ARGC FORM (GETARGDESC (CAR FORM)))
;                     (WARN 'EXPRESSION-AS-FUNCTION :VERY-OBSOLETE
;                           "The variable ~S is used in function position; use ~S."
;                           (CAR FORM) 'FUNCALL)
;                     (P1 `(FUNCALL . ,FORM)))
                    )))
  (IF (AND (ATOM FORM) (SELF-EVALUATING-P FORM))
      ;; a p1 handler may return :foo, for example
      `',FORM
      FORM))

(DEFUN FUNCTION-P (X)
  (COND ((SYMBOLP X)
         (FBOUNDP X)) ; (GETL X '(*EXPR ARGDESC))))
        ((FDEFINEDP X))
        (T
         (FUNCALL (GET (CAR X) 'FUNCTION-SPEC-HANDLER) 'SI::COMPILER-FDEFINEDP X))))

(DEFUN MSPL2 (X)
  (WHEN (LET ((BARF-SPECIAL-LIST THIS-FUNCTION-BARF-SPECIAL-LIST))
          (NOT (SPECIALP X)))
    ;; Here unless this variable was either 1) declared special, or
    ;; 2) already used free in this function.
    (UNLESS INHIBIT-SPECIAL-WARNINGS
      (WARN 'FREE-VARIABLE :MISSING-DECLARATION
            "The variable ~S is used free; assumed special." X))
    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
;>> Which is why this idea of how defsubst works is losing beyond all belief
      (UNLESS INHIBIT-SPECIAL-WARNINGS  ;Free var in a DEFSUBST shouldn't be special for whole file.
        (PUSHNEW X BARF-SPECIAL-LIST :TEST #'EQ))
      (PUSH X THIS-FUNCTION-BARF-SPECIAL-LIST))
    (WHEN (find X *ALLVARS* :key #'var-name)
      (WARN 'FREE-VARIABLE :IMPOSSIBLE
            " ~S was previously assumed local; you will lose!" X))))

(DEFUN MAKESPECIAL (X)
  (MSPL2 X)
  (PUSHNEW X *FREEVARS* :TEST #'EQ)
  T)

;;;OPTIMIZING AND STYLE CHECKING
;;; make the first cons of form a 3-hunk to flag that has been optimized
;;; what a hack! rms' idea
(defun flag-already-optimized (form &aux l)
  (if (not (consp form))
      form
    (without-interrupts
      (setq l (make-list 3 :initial-element 'already-optimized))
      (setf (car l) (car form)
            (cadr l) (cdr form))
      (%p-dpb cdr-normal %%q-cdr-code l)
      (%p-dpb-offset cdr-normal %%q-cdr-code l 1)
      l)))

(defun already-optimized-p (form)
  (without-interrupts
    (or (atom form)
        (and (eq (%p-ldb %%q-cdr-code form) cdr-normal)
             (eq (%p-ldb-offset %%q-cdr-code form 1) cdr-normal)
             (eq (%p-ldb-offset %%q-pointer form 2) (%pointer 'already-optimized))))))

(DEFUN COMPILER-OPTIMIZE-EXTERNAL (FORM)
  "Binds top level variables needed by COMPILER-OPTIMIZE."
  (LOCKING-RESOURCES-NO-QFASL
    (FILE-OPERATION-WITH-WARNINGS (T ':COMPILE)
      (COMPILER-WARNINGS-CONTEXT-BIND
        (QC-PROCESS-INITIALIZE)
        (LET ((QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
              (*FUNCTION-ENVIRONMENT* NIL)
              (LOCAL-DECLARATIONS NIL)
              (INHIBIT-STYLE-WARNINGS-SWITCH NIL)
              ;;The caller may bind this, but we'll give a default:
              (*p1value* (if (boundp '*p1value*) *p1value* t)))
          (COMPILER-OPTIMIZE FORM))))))

(defvar *trace-optimizations* nil
  "If T, report all optimizations.
If a list, only report the named optimizations.
Reports go to *TRACE-OUTPUT*")

(defvar *trace-optimizations-print-level* 4)
(defvar *trace-optimizations-print-pretty* t)

(defvar *record-optimizations* nil
  "If T, meter all optimizations.
If a list, only meter the named optimizations.")

(defvar *optimizer-usage* '())

(defstruct (optimizer-usage (:type list)
                            (:alterant nil)
                            (:conc-name opt-usage-))
  name
  count
  function-uses)

(defsubst optimizer-usage-entry (optimizer)
  (assq optimizer *optimizer-usage*))

(defun optimizer-usage-count (optimizer)
  (let ((entry (optimizer-usage-entry optimizer)))
    (if entry (opt-usage-count entry) 0)))

(defun optimizer-function-usage (optimizer)
  (let ((entry (optimizer-usage-entry optimizer)))
    (and entry (opt-usage-function-uses entry))))

(defun invoke-optimizer (optimizer form)
  (flet ((applicable-p (switch)
           (and switch (or (eq switch t) (memq optimizer switch)))))
    (let ((new (funcall optimizer form)))
      (unless (eq new form)
        (when (applicable-p *trace-optimizations*)
          (let ((*print-pretty* *trace-optimizations-print-pretty*)
                (*print-level* *trace-optimizations-print-level*))
            (format *trace-output* "~&~S ~optimized ~S~%into ~S~~%" optimizer form new)))
        (when (applicable-p *record-optimizations*)
          (let ((entry (optimizer-usage-entry optimizer)))
            (cond (entry
                   (pushnew name-to-give-function (opt-usage-function-uses entry) :test #'equal)
                   (incf (opt-usage-count entry)))
                  (t
                   (push (make-optimizer-usage :name optimizer :count 1
                                               :function-uses (list name-to-give-function))
                         *optimizer-usage*))))))
      new)))

;;;;;;STYLE CHECKING

(defvar *trace-style-checkers* nil
  "If T, report all style checkers.
If a list, only report the named style checkers.
Reports go to *TRACE-OUTPUT*")

;;;Following are used to set corresponding print control variables
;;;when tracing style checking:

(defvar *trace-style-checkers-print-pretty* T)
(defvar *trace-style-checkers-print-level* 3.)
(defvar *trace-style-checkers-print-length* 5.)

;;;Turning this on cranks up the new more verbose style checking mechanism.
;;;(Hopefully this means developers will use it more.)

(defvar *just-once-for-style-checkers-per-inner-form* T
  "If non-NIL, user only gets one style-check warning within a top-level form,
  for a given combination of style-checker function and form (function call).
The default, T, means condense for all style-checkers; can also be a list of
  style-checker functions to condense in this fashion.")

;;;Next is an Alist kept per top-level form to note that we've already seen a
;;;style violation for style-checker/form combination.  CAR's are style-checker
;;;function <names>, like COMPILER:OBSOLETE; the rest of the list contains the
;;;names of any function names for which this particular style violation has
;;;been detected.

(defvar *just-once-for-style-checkers-per-inner-form-alist* NIL)

;;;Utility functions to maintain style-checker/form Alist:

(defun already-checked-style-for-function (style-checker fcn)
  "If STYLE-CHECKER has been not been called before on FCN (in current
style-checking environment), return non-NIL.  Doesn't note invocation.
Otherwise, returns NIL."
  (memq (function-name fcn)
        (cdr (assoc (function-name style-checker)
                    *just-once-for-style-checkers-per-inner-form-alist*))))

(defun note-style-check-for-function (style-checker fcn)
  "Note that STYLE-CHECKER has been called on FCN (in current
style-checking environment)."
  (let* ((function-name (function-name fcn))
         (style-checker-name (function-name style-checker))
         (functions-already-checked
           (assoc style-checker-name
                  *just-once-for-style-checkers-per-inner-form-alist*)))
    (if (null functions-already-checked)
        (push (list style-checker-name function-name)
              *just-once-for-style-checkers-per-inner-form-alist*)
      (pushnew function-name
               (cdr (assoc style-checker-name *just-once-for-style-checkers-per-inner-form-alist*))))))

(defun first-style-check-for-function (style-checker fcn)
  "If STYLE-CHECKER has been not been called before on FCN (in current
style-checking environment), return non-NIL and note the invocation.
Otherwise, returns NIL."
  (let* ((function-name (print(function-name fcn)))
         (style-checker-name (print(function-name style-checker)))
         (functions-already-checked
           (assoc style-checker-name *just-once-for-style-checkers-per-inner-form-alist*)))
    (cond
      ((null functions-already-checked)
       (push (list style-checker-name function-name)
             *just-once-for-style-checkers-per-inner-form-alist*))
      ((memq function-name functions-already-checked)
       nil)
      (:else
       (push function-name
             (cdr (assoc style-checker-name *just-once-for-style-checkers-per-inner-form-alist*)))))))

;;;Given a function name symbol and the complete form it was found within, this
;;;invokes its style checker (if any), doing the recording and just-once
;;;warning as noted above:
;;;
;;;The new mechanism prevents redundant and irritating pathologically
;;;repetitious style warnings.  It depends heavily on the following
;;;conventions:
;;;
;;; 1) COMPILER:WARN's second return value is 'COMPILER:WARN.
;;;
;;; 2) all style-checkers call COMPILER:WARN as their last act, returning the
;;;magic value that lets the invoker know that a style violation occurred.
;;;
;;;-Keith
;;;
;;;9/88

(defun invoke-style-checker (fcn form style-checker)
  (let (style-checker-name)
    (flet ((applicable-p (switch)
            (and switch (or (eq switch t) (member style-checker-name switch)))))
      (and
        ;;Is there a style checker for function?
        (setq style-checker-name (function-name style-checker))
        (or
          ;;Either we're warning on every un-stylish call,
          ;;...Or we're only warning on the first bad call we find,
          ;;   possibly calling only selected style checkers.
          (not (applicable-p *just-once-for-style-checkers-per-inner-form*))
          (not (already-checked-style-for-function style-checker fcn))
          ;;Trace when we pass up a style check:
          (when (applicable-p *trace-style-checkers*)
            (format *trace-output* "~&{Skipping style-check ~A on ~A}"
                    style-checker-name fcn)))
        (prog (result)
              ;;Trace, if applicable.  Also see above.
              (let ((*print-pretty* *trace-style-checkers-print-pretty*)
                    (*print-level* *trace-style-checkers-print-level*)
                    (*print-length* *trace-style-checkers-print-length*))
                (when (applicable-p *trace-style-checkers*)
                  (format *trace-output*
                          "~&~A style-checking ~A: ~~S~~%"
                          style-checker-name fcn form))
                ;;Invoke style check:
                (multiple-value-bind (ignore warning?)
                    (funcall style-checker form)
                  (and (setq result (eq warning? 'warn))
                       ;;Note that a warning was issued.
                       (note-style-check-for-function style-checker fcn)))
                (when (applicable-p *trace-style-checkers*)
                  (format *trace-output*
                          "~&  {~:[Style checked OK~;Style warning issued~]}"
                          result))
                (return result)))))))

(defun maybe-invoke-style-checker (form &optional fn style-checker)
  ;;Pick up optional values only as needed!
  (and *check-style-p* (not inhibit-style-warnings-switch)      ;Are we redundant yet?
       (symbolp (or fn (setq fn (car form))))
       (or style-checker (setq style-checker (get fn 'style-checker)))
       (invoke-style-checker fn form style-checker)))


;;;This is the main style-check/optimize/macroexpand transforming function.
;;;
;;;Given a form, apply optimizations and expand macros until no more is
;;;possible (at the top level).  Also apply style-checkers as applicable, to
;;;both inputs and generated outputs.  This function is also in charge of
;;;checking for too few or too many arguments so that this happens before
;;;optimizers are applied.
;;;
;;;DONT-OPTIMIZE means not to run optimizers for the top-level form; this does
;;;not affect the mandatory calling of rewriters for the form.

(defun compiler-optimize (form &optional dont-optimize
                          &aux (*check-style-p* *check-style-p*))
  (cond ((already-optimized-p form)
         form)
        (t
    (do (tem fn local-definition rewritten-already)
        ;;Do this loop until no more expansions possible.
        ((atom form))
      (setq fn (lambda-macro-expand (car form)))
      (unless (eq fn (car form)) (setq form (cons fn (cdr form))))
      (unless rewritten-already
        ;; Check for too few or too many arguments
        (check-number-of-args form fn))
      (setq local-definition
            (and (symbolp fn) (fsymeval-in-function-environment fn *function-environment*)))
      ;; Do style checking, maybe
      (and (not rewritten-already)
           (not local-definition)
           (maybe-invoke-style-checker form fn))
      ;; Optimize args to vanilla functions
      (when (symbolp fn)
        ;; don't optimize args to macros, special forms, or functions with p1 handlers
        (unless (if local-definition
                    (eq (car local-definition) 'macro)
                  (or (get fn 'p1)
                      (macro-function fn)
                      (special-form-p fn)
                      (and (neq *target-computer* 'lambda-interface)
                           (get fn 'p1cross))))
          (setq form (cons (car form)
                           (let ((*p1value* 1)) ; Need one value from each argform
                             (mapcar #'compiler-optimize (cdr form)))))))
      (or (unless (or local-definition (not (symbolp fn)))
            (dolist (opt (get fn 'rewriters))
              (unless (eq form (setq form (funcall opt form)))
                ;; Rewriter changed something, don't do macros this pass
                (setq rewritten-already t)
                (return t))))
          (unless (or local-definition
                      (not (symbolp fn))
                      dont-optimize
                      *inhibit-optimizers*)
            (dolist (opt (get fn 'optimizers))
              (unless (eq form (setq form (invoke-optimizer opt form)))
                ;; Optimizer changed something, don't do macros this pass
                (setq rewritten-already t)
                (return t))))
          ;; No optimizer did anything => try expanding macros.
          (warn-on-errors ('macro-expansion-error "Error expanding macro ~S:" fn)
            ;; This LET returns T if we expand something.
            (or (let ((record-macros-expanded t))
                  (multiple-value-setq (form tem)
                    (compiler-macroexpand-1 form))
                  tem)                          ;non-nil if macroexpansion happened
                ;; Stop looping, no expansions apply
                (return nil)))
          ;; The body of the WARN-ON-ERRORS either does RETURN or returns T.
          ;; So if we get here, there was an error inside it.
          (return (setq form `(error-macro-expanding ',form))))
      ;; Only do style checking the first time around
      (setq *check-style-p* nil))
    ;; Result is FORM
    (cond ((and (listp form)
                (symbolp (car form))
                (fboundp (car form))
                (listp (fdefinition (car form)))
                (eq 'macro (car (fdefinition (car form)))))
           (fsignal "About to return form containing macro from compiler-optimize")))
    (flag-already-optimized form))))

(DEFUN ERROR-MACRO-EXPANDING (FORM)
  (DECLARE (DBG:ERROR-REPORTER))
  (FERROR "The form ~S which appeared at this point
was not compiled due to an error in macro expansion." FORM))

;;>> this should eventually be different from macroexpand-1, in that it supply a
;;>> *macroexpand-hook* which will not do bogus things with things we know about
;;>> specially (such as (common-lisp-only) "macros" which we really implement as
;;>> special forms, or things with p1 handlers or optimizers)
;;>> For now, macroexpand-1 doesn't hack the things with si::alternate-macro-definitions,
;;>> so this will do.
;;>> Also, we really should pass more than just the function env -- also need declarations &c.
(defun compiler-macroexpand-1 (form)
  ;; CAR of environment is local macros (and functions)
  (with-list (env *function-environment*)
    (macroexpand-1 form env)))

;;; Given a non-atomic form issue any warnings required because of wrong number of arguments.
;;; This function has some of the same knowledge as GETARGDESC but doesn't call
;;; it because GETARGDESC has to do a lot more.
;;; This function should never get an error and never warn about
;;; anything that gets warned about elsewhere.
(DEFUN CHECK-NUMBER-OF-ARGS (FORM &OPTIONAL FUNCTION)
  (IF (NULL FUNCTION) (SETQ FUNCTION (CAR FORM)))
  (LET* (TEM
         ARGLIST
         NARGS
         (MIN NIL)
         (MAX 0) (MAX1 0)
         (ARGS-INFO NIL)
         (LOCALP NIL)
         (FN FUNCTION))
    (AND (SYMBOLP FN)
         ;; If FN is a name defined lexically by FLET or LABELS, use its definition.
         (SETQ LOCALP (FSYMEVAL-IN-FUNCTION-ENVIRONMENT FN))
         (SETQ FN LOCALP))
    (FLET ((BAD-ARGUMENTS (MSG)
              (WARN 'WRONG-NUMBER-OF-ARGUMENTS :PROBABLY-ERROR
                    (IF LOCALP
                        "~S (locally defined function) called with ~A"
                        "~S called with ~A")
                    (CAR FORM) MSG)))
      (TAGBODY
       TOP
          (IF LOCALP
              (SETQ ARGLIST
                    (CASE (CAR FN)
                      ((MACRO) (RETURN-FROM CHECK-NUMBER-OF-ARGS NIL))
                      ((NAMED-LAMBDA NAMED-SUBST) (CADDR FN))
                      (T (CADR FN))))
            (SETQ FN (LAMBDA-MACRO-EXPAND FN))
            (COND ((AND (SYMBOLP FN)
                        (NOT (FBOUNDP FN)))
                   (SETQ ARGLIST NIL))
                  (T
                   (SETQ ARGLIST (IGNORE-ERRORS (ARGLIST FN 'COMPILE)))
                   (IF (EQ ARGLIST 'MACRO) (RETURN-FROM CHECK-NUMBER-OF-ARGS NIL)))))
          (COND ((OR LOCALP
                     (MEMQ (CAR-SAFE FN) '(LAMBDA NAMED-LAMBDA ZL:SUBST CL:SUBST NAMED-SUBST)))
                 (DOLIST (X ARGLIST)
                   (COND ((EQ X '&OPTIONAL) (SETQ MIN MAX))
                         ((OR (EQ X '&REST) (EQ X '&BODY) (EQ X '&KEY))
                          (UNLESS MIN (SETQ MIN MAX))
                          (SETQ MAX MOST-POSITIVE-FIXNUM)
                          (RETURN))
                         ((EQ X '&AUX) (RETURN))
                         ((MEMQ X LAMBDA-LIST-KEYWORDS))
                         (T (INCF MAX) (INCF MAX1)))))
                ((NOT (SYMBOLP FN))
                 ;; Unknown type, don't check
                 (RETURN-FROM CHECK-NUMBER-OF-ARGS NIL))
                ((SETQ TEM (GET FN 'ARGDESC))
                 (DOLIST (X TEM)
                   (COND ((MEMQ 'FEF-ARG-REQ (CADR X))
                          (INCF MAX (CAR X)) (INCF MAX1 (CAR X)))
                         ((MEMQ 'FEF-ARG-OPT (CADR X))
                          (OR MIN (SETQ MIN MAX))
                          (INCF MAX (CAR X)) (INCF MAX1 (CAR X)))
                         ((MEMQ 'FEF-ARG-REST (CADR X))
                          (OR MIN (SETQ MIN MAX))
                          (SETQ MAX MOST-POSITIVE-FIXNUM)))))
                ((SETQ TEM (GET FN 'QINTCMP))
                 (SETQ MAX TEM MAX1 TEM))
                ;>> used??
                ((SETQ TEM (GET FN 'Q-ARGS-PROP))
                 (SETQ ARGS-INFO TEM))
                ;; Take care of recursive calls to function being compiled.
                ((AND (EQ FN THIS-FUNCTION-ARGLIST-FUNCTION-NAME)
                      (NOT LOCALP))
                 (DOLIST (X THIS-FUNCTION-ARGLIST)
                   (COND ((EQ X '&OPTIONAL) (SETQ MIN MAX))
                         ((OR (EQ X '&REST) (EQ X '&BODY) (EQ X '&KEY))
                          (UNLESS MIN (SETQ MIN MAX))
                          (SETQ MAX MOST-POSITIVE-FIXNUM)
                          (RETURN))
                         ((EQ X '&AUX) (RETURN))
                         ((MEMQ X LAMBDA-LIST-KEYWORDS))
                         (T (INCF MAX) (INCF MAX1)))))
                ;;>> doesn't look at definitions earlier in the file
                ((FBOUNDP FN)
                 (SETQ TEM (SI:UNENCAPSULATE-FUNCTION-SPEC FN))
                 (UNLESS (EQ TEM FN)
                   (SETQ FN TEM)
                   (GO TOP))
                 (SETQ TEM (SYMBOL-FUNCTION FN))
                 (COND ((OR (SYMBOLP TEM) (CONSP TEM))
                        (SETQ FN TEM)
                        (GO TOP))
                       (T (SETQ ARGS-INFO (%ARGS-INFO TEM)))))
                (T ;;No information available
                 (RETURN-FROM CHECK-NUMBER-OF-ARGS NIL))))
      (WHEN ARGS-INFO
        (SETQ MIN (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
              MAX1 (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)
              MAX (IF (BIT-TEST (LOGIOR %ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST)
                                ARGS-INFO)
                      MOST-POSITIVE-FIXNUM MAX1)))
      (SETQ NARGS (LENGTH (CDR FORM)))  ;Now that we know it's not a macro
      (COND ((< NARGS (OR MIN MAX))
             (BAD-ARGUMENTS "too few arguments"))
            ((> NARGS MAX)
             (BAD-ARGUMENTS "too many arguments"))
            ((CONSP ARGLIST)
             (LET* ((KEYARGS (MEMQ '&KEY ARGLIST))
                    (KEYFORM (NTHCDR (OR MAX1 MIN) (CDR FORM))))
               (WHEN (AND KEYARGS KEYFORM)
                 (IF (ODDP (LENGTH KEYFORM))
                     (BAD-ARGUMENTS "no value supplied for some keyword argument")
                   (LET ((ALLOW-OTHER-KEYS (OR (MEMQ '&ALLOW-OTHER-KEYS ARGLIST)
                                               (GETF KEYFORM ':ALLOW-OTHER-KEYS))))
                     (LOOP FOR KEY IN KEYFORM BY 'CDDR
                        WHEN (AND (EQ (CAR-SAFE KEY) 'QUOTE)
                                  (SELF-EVALUATING-P (CADR KEY)))
                          DO (SETQ KEY (CADR KEY))
                        DOING (COND ((KEYWORDP KEY)
                                     (UNLESS
                                       (OR ALLOW-OTHER-KEYS
                                           (DOLIST (X KEYARGS)
                                             (IF (MEMQ X LAMBDA-LIST-KEYWORDS)
                                                 NIL
                                               (IF
                                                 (IF (CONSP X)
                                                     (IF (CONSP (CAR X))
                                                         ;; ((:frob foo) bar)
                                                         (EQ KEY (CAAR X))
                                                       ;; (foo bar)
                                                       (STRING= KEY (CAR X)))
                                                   ;; foo
                                                   (STRING= KEY X))
                                                 (RETURN T)))))
                                       (BAD-ARGUMENTS
                                         (FORMAT NIL "the unrecognized keyword ~S"
                                                 KEY))))
                                    ((SELF-EVALUATING-P KEY)
                                     (BAD-ARGUMENTS
                                       (FORMAT NIL "~S appearing where a keyword should" KEY))))))))))))))


;;; Pass 1 processing for a call to an ordinary function (ordinary, at least, for pass 1).
;;; FORM is the call to the function, and DESC is the GETARGDESC of the function.
;;; Processing consists of P1'ing all evaluated arguments, but not the quoted ones.
;;; DESC is used to determine which is which.
;;; In addition, &FUNCTIONAL arguments are broken off and separately compiled.
;;; We process the args by copying the arglist,
;;;  and rplaca'ing each arg by P1 of itself if needed.
(DEFUN P1ARGC (FORM DESC)
  (IF (AND (MEMQ 'FEF-ARG-REST (CADAR DESC))
           (MEMQ 'FEF-QT-QT (CADAR DESC)))
      FORM
    (DO* ((COUNT 0 (1- COUNT))
          (ARGS-LEFT (COPY-LIST (CDR FORM)) (CDR ARGS-LEFT))
          (ARG-P1-RESULTS ARGS-LEFT)
          (FCTN (CAR FORM))
          (*P1VALUE* 1)                         ;function calling uses only first value
          (DESCS-LEFT DESC)
          TOKEN-LIST)
         (())
      ;; If all arguments processed, return.
      (COND ((NULL ARGS-LEFT)
             (RETURN (CONS FCTN ARG-P1-RESULTS)))
            ((ATOM ARGS-LEFT)
             (WARN :IMPOSSIBLE 'NON-NIL-END-OF-FORM
                   "The form ~S ends in a non-NIL atomic cdr."
                   FORM)
             (IF (ATOM ARG-P1-RESULTS)
                 (RETURN (LIST FCTN))
               (SETF (CDR (LAST ARG-P1-RESULTS)) NIL)
               (RETURN (CONS FCTN ARG-P1-RESULTS)))))

      ;; Figure out what descriptor to use for the next argument.
      ;; TOKEN-LIST is the actual descriptor, and COUNT
      ;; is the number of arguments left for it to apply to.
      (WHEN (ZEROP COUNT)
        (COND ((NULL DESCS-LEFT)
               ;; Out of descriptors => treat excess args as evalled.
               (SETQ DESCS-LEFT '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL))))))
        (SETQ COUNT (CAAR DESCS-LEFT))
        (SETQ TOKEN-LIST (CADAR DESCS-LEFT))
        (SETQ DESCS-LEFT (CDR DESCS-LEFT))
        (IF (MEMQ 'FEF-ARG-REST TOKEN-LIST)
            (SETQ COUNT #o1005)))

      ;; Process the next argument according to its descriptor.
      (COND ((MEMQ 'FEF-QT-QT TOKEN-LIST))
            ((OR (MEMQ 'FEF-QT-EVAL TOKEN-LIST)
                 (MEMQ 'FEF-QT-DONTCARE TOKEN-LIST))
             (SETF (CAR ARGS-LEFT)
                   (COND ;>> People who are using "(quote (lambda ...)" -should- lose.
                         ;((AND (MEMQ 'FEF-FUNCTIONAL-ARG TOKEN-LIST)
                         ;      (NOT (ATOM (SETQ TM (COMPILER-OPTIMIZE (CAR ARGS-LEFT)))))
                         ;      (EQ (CAR TM) 'QUOTE))   ;Look for '(LAMBDA...)
                         ; (P1FUNCTION TM))
                     (T (P1 (CAR ARGS-LEFT))))))
            (T (BARF TOKEN-LIST 'BAD-EVAL-CODE))))))

;;; Return T if OBJECT is something quoted.
(DEFSUBST QUOTEP (OBJECT)
  (EQ (CAR-SAFE OBJECT) 'QUOTE))

;;; When a var is handled by P1BINDVAR which is an optional arg with a specified-flag,
;;; we push the flag name onto SPECIFIED-FLAGS so that a home will be made for the flag.
(DEFVAR SPECIFIED-FLAGS)

;;; Process a Lambda-list (X), making the variables by default of kind KIND
;;; (FEF-ARG-REQ for the top-level lambda,
;;;  FEF-ARG-AUX or FEF-ARG-INTERNAL-AUX for progs).
;;; Return a prog variable list for the same variables with their initializations if any,
;;; with P1 done on each initialization.
;;; This function gobbles down the variables and processes keywords.
;;; Each variable, with its appropeiate keyword info, is passed to P1LMB.
;;; We can do either sequential or parallel binding.
;;; Processing of variables is done in two steps:
;;; First, create the homes
;;; Second, if these are not FEF-ARG-INTERNAL-AUX vars,
;;;  put the homes on *VARS* and *ALLVARS*.
;;; Third, process all the variables' initializations.
;;; Finally, put the homes on *VARS* and *ALLVARS* if not already there.

;;; For variables whose scope is the whole function (not FEF-ARG-INTERNAL-AUX),
;;; the order is designed so that variables bound inside their initializations
;;; all come after all the variables of the original (higher) level.
;;; This is needed to make sure that (DEFUN FOO (&OPTIONAL (A (LET ((C ...)) ...)) B) ...)
;;; does not put C into *VARS* before B.

;;; For FEF-ARG-INTERNAL-AUX variables, we want the variables bound
;;; inside the initializations to come first, since they are used first.
;;; That way, our own variables overlap with them rather than vice versa.
;;; As a result, the variable with the original home is always the first one used.
;;; This is important for deciding which variables need explicit initialization.

;;; The IGNORE-NIL-P argument is used by MULTIPLE-VALUE-BIND to say
;;;  that if NIL appears as a variable, its initial value should be evaluated
;;;  and discarded.
(DEFUN P1SBIND (V KIND PARALLEL IGNORE-NIL-P THIS-FRAME-DECLARATIONS &AUX MYVARS)
  ;; First look at the var specs and make homes, pushing them on MYVARS (reversed).
  (LET ((EVALCODE 'FEF-QT-DONTCARE)
        (SPECIALNESS NIL)
        (SPECIFIED-FLAGS NIL)
        (ALREADY-REST-ARG NIL)
        (ALREADY-AUX-ARG NIL)
        (MISC-TYPES NIL)
        (*P1VALUE* 1))
    (DO ((X V (CDR X))
         TEM)
        ((NULL X))
      (COND ((SETQ TEM (ASSQ (CAR X) '((&OPTIONAL . FEF-ARG-OPT)
                                       (&REST . FEF-ARG-REST)
                                       (&AUX . FEF-ARG-AUX))))
             (IF (OR (EQ KIND 'FEF-ARG-AUX)
                     (EQ KIND 'FEF-ARG-INTERNAL-AUX))
                 (WARN 'BAD-BINDING-LIST :IMPOSSIBLE
                       "A lambda-list keyword (~S) appears in an internal binding list."
                       (CAR X))
               (SETQ KIND (CDR TEM))))
            ;; Obsoleted by getting destructuring into shape and then doing special forms right.
            ((SETQ TEM (ASSQ (CAR X) '((&EVAL . FEF-QT-EVAL)
                                       (&QUOTE . FEF-QT-QT))))
             (SETQ EVALCODE (CDR TEM)))
            ((EQ (CAR X) '&FUNCTIONAL)
             ;; eventually obsoleted hacking of downward-funarg delcaration
             (PUSH 'FEF-FUNCTIONAL-ARG MISC-TYPES))
            ((EQ (CAR X) '&SPECIAL)
             (warn 'obsolete-lambda-list-keyword :obsolete
                   "~~S in lambda-lists is obsolete and will not be supported in the future.~%  ~
                    Use the ~S declaration.~" '&special 'special)
             (SETQ SPECIALNESS T))
            ((EQ (CAR X) '&LOCAL)
             (warn 'obsolete-lambda-list-keyword :obsolete
                   "~~S in lambda-lists is obsolete and will not be supported in the future.~%  ~
                    Use the ~S declaration.~" '&local 'special)
             (SETQ SPECIALNESS NIL))
            ((EQ (CAR X) '&AUX)
             (SETQ ALREADY-AUX-ARG T))
            ((MEMQ (CAR X) LAMBDA-LIST-KEYWORDS))
            (T
             ;; Now (CAR X) should be a variable or (var init).
             (LET ((VARN (IF (ATOM (CAR X)) (CAR X) (CAAR X))))
               (IF (NOT (SYMBOLP VARN))
                   (WARN 'VARIABLE-NOT-SYMBOL :IMPOSSIBLE
                         "~S appears in a list of variables to be bound." VARN)
                 (AND (NOT (OR (STRING= VARN "IGNORE")
                               (STRING= VARN "IGNORED")
                               (NULL VARN)))
                      ;; Does this variable appear again later?
                      ;; An exception is made in that a function argument can be repeated
                      ;; after an &AUX.
                      (DOLIST (X1 (CDR X))
                        (COND ((AND (EQ X1 '&AUX)
                                    (NOT ALREADY-AUX-ARG))
                               (RETURN NIL))
                              ((OR (EQ X1 VARN)
                                   (AND (NOT (ATOM X1)) (EQ (CAR X1) VARN)))
                               (RETURN T))))
                      (WARN 'BAD-BINDING-LIST :IMPLAUSIBLE
                            "The variable ~S appears twice in one binding list."
                            VARN))
                 (AND (EQL (CHAR (SYMBOL-NAME VARN) 0) #/&)
                      (WARN 'MISSPELLED-KEYWORD :IMPLAUSIBLE
                            "~S is probably a misspelled keyword." VARN))
                 (IF ALREADY-REST-ARG
                     (WARN 'BAD-LAMBDA-LIST :IMPOSSIBLE
                           "Argument ~S comes after the &REST argument." VARN))
                 (IF (EQ KIND 'FEF-ARG-REST)
                     (SETQ ALREADY-REST-ARG T))
                 (COND ((AND IGNORE-NIL-P (NULL VARN))
                        (P1 (CADAR X)))         ;Out of order, but works in these simple cases
                       ((OR (NULL VARN) (EQ VARN T))
                        (WARN 'NIL-OR-T-SET :IMPOSSIBLE
                              "There is an attempt to bind ~S." VARN))
                       ((KEYWORDP VARN)
                        (WARN 'KEYWORD-BOUND :IMPOSSIBLE
                              "There is an attempt to bind the keyword symbol ~S." VARN))
                       (T
                        ;; Make the variable's home.
                        (IF SPECIALNESS
                            (LET ((DECL `(SPECIAL ,(COND ((SYMBOLP (CAR X)) (CAR X))
                                                         ((SYMBOLP (CAAR X)) (CAAR X))
                                                         (T (CADAAR X))))))
                              (PUSH DECL LOCAL-DECLARATIONS)
                              (PUSH DECL THIS-FRAME-DECLARATIONS)))
                        (PUSH (P1BINDVAR (CAR X) KIND EVALCODE MISC-TYPES
                                         THIS-FRAME-DECLARATIONS)
                              MYVARS)))
                 (SETQ MISC-TYPES NIL))))))

    ;; Arguments should go on *ALLVARS* now, so all args precede all boundvars.
    (OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
        (EQ KIND 'FEF-ARG-AUX)
        (SETQ *ALLVARS* (APPEND SPECIFIED-FLAGS MYVARS *ALLVARS*)))
    ;(MAPC #'VAR-COMPUTE-INIT SPECIFIED-FLAGS (CIRCULAR-LIST NIL))
    (DOLIST (V SPECIFIED-FLAGS)
      (compiler-target-switch (VAR-COMPUTE-INIT V NIL)))

    ;; Now do pass 1 on the initializations for the variables.
    (DO ((ACCUM)
         (VS (REVERSE MYVARS) (CDR VS)))
        ((NULL VS)
         ;; If parallel binding, put all var homes on *VARS*
         ;; after all the inits are through.
         (WHEN PARALLEL
           (SETQ *VARS* (APPEND MYVARS *VARS*))
           (WHEN (OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
                     (EQ KIND 'FEF-ARG-AUX))
             (MAPC #'VAR-CONSIDER-OVERLAP MYVARS)
             (SETQ *ALLVARS* (APPEND MYVARS *ALLVARS*))))
         (NREVERSE ACCUM))
      (PUSH (compiler-target-switch (VAR-COMPUTE-INIT (CAR VS) PARALLEL)) ACCUM)
      ;; For sequential binding, put each var on *VARS*
      ;; after its own init.
      (UNLESS PARALLEL
        (COND ((OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
                   (EQ KIND 'FEF-ARG-AUX))
               (VAR-CONSIDER-OVERLAP (CAR VS))
               (PUSH (CAR VS) *ALLVARS*)))
        (PUSH (CAR VS) *VARS*)
        (LET ((TEM (CDDR (VAR-INIT (CAR VS)))))
          (AND TEM (PUSH TEM *VARS*)))))))

;;; Create a home for a variable.
;;; We fill the variable's INIT slot with a list whose car is the init form
;;; and whose cadr may be the supplied-flag-name, or with nil if there is no init at all,
;;; rather than what is ultimately to go there (which gets there in VAR-COMPUTE-INIT).
(DEFUN P1BINDVAR (VARSPEC KIND EVAL-TYPE MISC-TYPES THIS-FRAME-DECLARATIONS)
  (LET (TYPE INIT-SPECS)
    (UNLESS (ATOM VARSPEC)
      (SETQ INIT-SPECS (CDR VARSPEC))
      (SETQ VARSPEC (CAR VARSPEC)))
    (IF (OR (EQ VARSPEC NIL) (EQ VARSPEC T))
        (WARN 'NIL-OR-T-SET :IMPOSSIBLE "There is an attempt to bind ~S." VARSPEC)
      ;; If this variable is an optional arg with a specified-flag,
      ;; remember to make a home for the flag as well.
      (WHEN (CADR INIT-SPECS)
        (COND ((NEQ KIND 'FEF-ARG-OPT)
               (WARN 'BAD-ARGUMENT-LIST :IMPOSSIBLE
                     "The bound variable ~S has a specified-flag but isn't an optional arg."
                     VARSPEC))
              ((NOT (SYMBOLP (CADR INIT-SPECS)))
               (WARN 'BAD-ARGUMENT-LIST :IMPOSSIBLE
                     "The bound variable ~S has a specified-flag ~S which isn't a symbol."
                     VARSPEC (CADR INIT-SPECS)))
              (T
               (PUSH (CREATE-SPECIFIED-FLAG-HOME (CADR INIT-SPECS) THIS-FRAME-DECLARATIONS)
                     SPECIFIED-FLAGS))))
      (UNLESS (SYMBOLP VARSPEC)
        (WARN 'VARIABLE-NOT-SYMBOL :IMPOSSIBLE
              "~S, not a symbol, appears as a variable to be bound."
              VARSPEC))
      (SETQ TYPE (FIND-TYPE VARSPEC THIS-FRAME-DECLARATIONS))
      (IF (MEMQ TYPE '(FEF-SPECIAL FEF-REMOTE)) (SETQ *SPECIALFLAG* T))
      (VAR-MAKE-HOME VARSPEC TYPE KIND INIT-SPECS EVAL-TYPE MISC-TYPES
                     THIS-FRAME-DECLARATIONS))))

;;; Make a home for the "specified-flag" of an optional variable
;;; (such as, FOOP in &OPTIONAL (FOO 69 FOOP)).
;;; It is marked with FEF-ARG-SPECIFIED-FLAG in the misc flags.
;;; This home is pushed on *VARS* right after the last argument, before
;;; the first actual aux variable, and also before any locals bound
;;; in initializations of optionals, and its scope is the entire function.
;;; It is of kind "aux" and initialized to the constant T
;;; regardless of the fact that TLFUNINIT is already set and so
;;; (usually) only FEF-INI-COMP-C is allowed at this point.
(DEFUN CREATE-SPECIFIED-FLAG-HOME (NAME THIS-FRAME-DECLARATIONS)
  (VAR-MAKE-HOME NAME
                 (FIND-TYPE NAME THIS-FRAME-DECLARATIONS)
                 'FEF-ARG-AUX '('T)
                 'FEF-QT-DONTCARE '(FEF-ARG-SPECIFIED-FLAG)
                 THIS-FRAME-DECLARATIONS))

;;; Since inheriting is obsolete, this switch probably should be set to nil.
;;; However, this would probably screw people in a very obscure way.  For now, leave
;;; it at T.
(defvar *inherit-special-declarations?* t
  "If nil, local SPECIAL declarations will not be inherited in inner local contexts.")

(DEFUN SPECIALP (SYMBOL)
  ;; Do this test because I am not convinced that the compiler
  ;; won't break if we *really* look at the declarations.
  (if (not *inherit-special-declarations?*)
      (eq (find-type symbol this-frame-declarations) 'fef-special)
      (DOLIST (DECL LOCAL-DECLARATIONS
                    ;; Here if no local declaration says anything.
                    ;; Try FILE-(UN)SPECIAL-LIST which reflect global decls in the file.
                    ;; Now uses *COMPILATION-ENVIRONMENT* -smh 1aug88
                    (OR (getdecl SYMBOL 'special)
                        ALL-SPECIAL-SWITCH
                        (GET SYMBOL 'SPECIAL)
                        (GET SYMBOL 'SYSTEM-CONSTANT)
                        (MEMQ SYMBOL BARF-SPECIAL-LIST)))
        (AND (MEMQ (CAR DECL) '(SPECIAL UNSPECIAL))
             (MEMQ SYMBOL (CDR DECL))
             (RETURN (EQ (CAR DECL) 'SPECIAL))))))

(DEFUN FIND-TYPE (SYMBOL THIS-FRAME-DECLARATIONS &AUX LOSE)
  (DOLIST (DECL THIS-FRAME-DECLARATIONS)
    (AND (MEMQ (CAR DECL) '(SPECIAL UNSPECIAL))
         (MEMQ SYMBOL (CDR DECL))
         (RETURN-FROM FIND-TYPE
           (IF (EQ (CAR DECL) 'SPECIAL) 'FEF-SPECIAL 'FEF-LOCAL))))
  (DOLIST (DECL LOCAL-DECLARATIONS)
    (AND (MEMQ (CAR DECL) '(SPECIAL UNSPECIAL))
         (MEMQ SYMBOL (CDR DECL))
         (IF (EQ (CAR DECL) 'SPECIAL)
             (RETURN-FROM NIL (SETQ LOSE T))
           (RETURN-FROM FIND-TYPE 'FEF-LOCAL))))
  #+never
  (IF (OR (MEMQ SYMBOL FILE-SPECIAL-LIST)
          (AND (NOT (MEMQ SYMBOL FILE-UNSPECIAL-LIST))
               (OR ALL-SPECIAL-SWITCH
                   (GET SYMBOL 'SPECIAL)
                   (GET SYMBOL 'SYSTEM-CONSTANT)
                   (MEMQ SYMBOL BARF-SPECIAL-LIST))))
      (RETURN-FROM FIND-TYPE 'FEF-SPECIAL))
  (IF (OR (getdecl SYMBOL 'special)
          ALL-SPECIAL-SWITCH
          (GET SYMBOL 'SPECIAL)
          (GET SYMBOL 'SYSTEM-CONSTANT)
          (MEMQ SYMBOL BARF-SPECIAL-LIST))
      (RETURN-FROM FIND-TYPE 'FEF-SPECIAL))
  (IF (or (not *inherit-special-declarations?*) (NOT LOSE))
      'FEF-LOCAL
    (WARN 'INHERITED-SPECIAL-DECLARATION :OBSOLETE
          "A local SPECIAL declaration for ~S is being inherited.
The declaration should be at the beginning of the construct that binds the variable.
It still works now, but fix it quickly before it stops working." SYMBOL)
    'FEF-SPECIAL))

(defun find-variable-home (variable-name)
  "Return a home for the lexical variable named VARIABLE-NAME.
Only currently visible variables are considered."
  (find variable-name (the list *vars*) :key #'var-name))

(defun get-var-declaration (var-home declaration)
  (getf (var-declarations var-home) declaration))

;;; Construct and return a variable home to go on *VARS* and *ALLVARS*.
;;; This home has, in the VAR-INIT slot, not what is supposed to be there
;;; but the actual initialization-form for the variable.
;;; Later, VAR-COMPUTE-INIT is called to fix that up.
(DEFUN VAR-MAKE-HOME (NAME TYPE KIND INIT-SPECS
                      EVAL-TYPE MISC-TYPES THIS-FRAME-DECLARATIONS &AUX HOME)
  (COND ((NULL (MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST
                            FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
         (BARF KIND 'BAD-KIND))
        ((KEYWORDP NAME)
         (WARN 'KEYWORD-BOUND :IMPOSSIBLE
               "Binding the keyword symbol ~S." NAME))
        ((CONSTANTP NAME)
         (WARN 'SYSTEM-CONSTANT-BOUND :IMPLAUSIBLE
               "Binding ~S, which is a constant." NAME))
        ((and (MEMQ NAME (CDDR SELF-FLAVOR-DECLARATION))
              (not (memq name (cadr self-flavor-declaration))))  ;if the instance variable
                        ;is special, it is all right, sort of.
         (WARN 'INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE
               "Binding ~S, which has the same name as an instance variable of flavor ~S"
               NAME (CAR SELF-FLAVOR-DECLARATION)))
        ((AND (MEMQ NAME '(SELF)) ; SELF-MAPPING-TABLE is bound in combined methods.
              SELF-FLAVOR-DECLARATION
              (EQ TYPE 'FEF-SPECIAL))
         (WARN 'SELF-BOUND :IMPLAUSIBLE
               "Rebinding ~S.  You may lose!" NAME)))
  ;; Rest args interfere with fast arg option except when there are no specials.
  ;; We need to look at this to
  ;;  decide how to process all the AUX variables and can't tell when processing
  ;;  the first one whether the next will be special.
  ;;  In any case, being wrong about this should not be able to produce
  ;;  incorrect code.
  (COND ((EQ KIND 'FEF-ARG-REST)
         (SETQ *FAST-ARGS-POSSIBLE* NIL))
        ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
         (AND INIT-SPECS (SETQ *FAST-ARGS-POSSIBLE* NIL))))
  ;; Detect vars bound to themselves which fail to be special.
  (WHEN (AND (EQ NAME (CAR INIT-SPECS))
             (NOT (find NAME *VARS* :key #'var-name))
             (not (and (eq *target-computer* 'k)
                       ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
                       (get name :register)))
             ;; If variable is already accessible lexically, it need not be special.
             (DOLIST (FRAME *OUTER-CONTEXT-VARS* T)
               (WHEN (find NAME FRAME :key #'var-name) (RETURN NIL))))
    (MSPL2 NAME)
    (SETQ TYPE 'FEF-SPECIAL))
  ;; Cons up the variable descriptor.
  ;; Note that INIT-SPECS is not the final value that will go in the INIT slot.
  (SETQ HOME (MAKE-VAR :NAME NAME :KIND KIND :TYPE TYPE
                       :INIT INIT-SPECS :EVAL EVAL-TYPE :MISC MISC-TYPES
                       :DECLARATIONS (DECLARATIONS-FOR-VARIABLE NAME THIS-FRAME-DECLARATIONS)))
  (IF (AND (EQ TYPE 'FEF-SPECIAL) (GETF (VAR-DECLARATIONS HOME) 'IGNORE))
      (WARN 'NOT-IGNORED :IMPLAUSIBLE
            "The special variable ~S was declared to be ignored" NAME))
  (SETF (VAR-LAP-ADDRESS HOME)
        ;; Not the real lap address,
        ;; but something for P1 to use for the value of the variable
        (IF (EQ TYPE 'FEF-SPECIAL) NAME `(LOCAL-REF ,HOME)))
  HOME)

(DEFUN DECLARATIONS-FOR-VARIABLE (NAME THIS-FRAME-DECLARATIONS &AUX RESULT)
  (DOLIST (X THIS-FRAME-DECLARATIONS)
    (CASE (CAR X)
      ((IGNORE)
       (IF (MEMQ NAME (CDR X)) (SETF (GETF RESULT 'IGNORE) T)))
      ((ARRAY ATOM BIGNUM BIT BIT-VECTOR CL:CHARACTER ZL:CHARACTER COMMON COMPILED-FUNCTION
        COMPLEX CONS DOUBLE-FLOAT FIXNUM FLOAT FUNCTION HASH-TABLE INTEGER KEYWORD LIST
        LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME RANDOM-STATE RATIO RATIONAL READTABLE
        SEQUENCE SHORT-FLOAT SIMPLE-ARRAY SIMPLE-BIT SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT
        STANDARD-CHAR STREAM STRING STRING-CHAR SYMBOL T VECTOR)
       (IF (MEMQ NAME (CDR X)) (SETF (GETF RESULT 'TYPE) (CAR X))))
      ((TYPE)
       (IF (MEMQ NAME (CDDR X)) (SETF (GETF RESULT 'TYPE) (CADR X))))
     ((SPECIAL UNSPECIAL                        ;already processed
       :SELF-FLAVOR SYS:FUNCTION-PARENT FTYPE   ;irrelevant
       FUNCTION INLINE NOTINLINE OPTIMIZE DECLARATION
       downward-function downward-funarg))
     (t
      (unless (get (car x) 'si::debug-info)
        ()                                      ;>>we really should barf about things
                                                ;we don't understand
        ))))
  RESULT)

(DEFUN MAKE-FREE-VAR-HOME (NAME)
  (MAKE-VAR :NAME NAME
            :KIND 'FEF-ARG-FREE
            :TYPE 'FEF-SPECIAL
            :USE-COUNT 0
            :LAP-ADDRESS NAME))

;;; For a variable whose scope is ready to begin (it's about to be put on *VARS*),
;;; look for another variable whose scope already ended, to share a slot with.
;;; If we find a suitable one, just clobber it in.
(DEFUN VAR-CONSIDER-OVERLAP (VAR)
  (AND (EQ (VAR-KIND VAR) 'FEF-ARG-INTERNAL-AUX)
       (DO ((VS *ALLVARS* (CDR VS)))
           ((NULL VS))
         ;; Look for other vars with the same name;
         ;; for a gensym, look for another gensym.
         (AND (OR (EQ (VAR-NAME VAR) (var-name (CAR VS)))
                  (AND (NULL (SYMBOL-PACKAGE (var-name (CAR VS))))
                       (NULL (SYMBOL-PACKAGE (VAR-NAME VAR)))))
              ;; But don't try to overlap a local with a special that happens to have the same
              ;; name.
              (NEQ (VAR-TYPE (CAR VS)) 'FEF-SPECIAL)
              ;; And don't overlap with arguments
              ;; (in (LAMBDA (&OPTIONAL (A (LET (B)...)) B) ...) we might otherwise try to do it)
              (EQ (VAR-KIND (CAR VS)) 'FEF-ARG-INTERNAL-AUX)
              ;; Insist on a slot that represents a canonical home (does not
              ;; map to another slot), and that is not currently in use
              (NOT (OR (VAR-OVERLAP-VAR (CAR VS))
                       (DOLIST (V *VARS*)
                         (AND (OR (EQ V (CAR VS))
                                  (EQ (VAR-OVERLAP-VAR V) (CAR VS)))
                              (RETURN T)))))
              (PROGN
                (PUSH 'FEF-ARG-OVERLAPPED (VAR-MISC (CAR VS)))
                (RETURN (SETF (VAR-OVERLAP-VAR VAR) (CAR VS))))))))


;;; After the end of pass 1, assign lap addresses to the variables.
;;; Returns the total number of local variable slots allocated.
(DEFUN ASSIGN-LAP-ADDRESSES ()
  (LET ((ARGN 0)   ;Next arg number to allocate.
        (LVCNT 0)) ;Next local block slot number to allocate.
                   ;Count rest arg, auxes, and internal-auxes if they are not special.
    (SETQ *ARG-MAP* ())                         ;We also build the arg map and local map,
    (SETQ *LOCAL-MAP* NIL)                      ;pushing things on in reverse order.
    (DOLIST (V (REVERSE *ALLVARS*))
      ;; Cons up the expression for Lap to use to refer to this variable.
      (LET ((TYPE (VAR-TYPE V))
            (KIND (VAR-KIND V))
            (NAME (VAR-NAME V))
            PERMANENT-NAME)
        (SETF (VAR-LAP-ADDRESS V)
              (COND ((EQ TYPE 'FEF-SPECIAL)
                     `(SPECIAL ,NAME))
                    ((EQ TYPE 'FEF-REMOTE)
                     `(REMOTE ,NAME))
                    ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
                     `(ARG ,ARGN))
                    ((VAR-OVERLAP-VAR V)
                     (VAR-LAP-ADDRESS (VAR-OVERLAP-VAR V)))
                    (T `(LOCAL ,LVCNT))))
        ;; If the name is in the temporary area or is uninterned, don't put it in the
        ;; arg/local map.  This is partly to avoid putting all these stupid gensyms
        ;; into the qfasl file, but the real reason is to avoid the dreaded scourge
        ;; of temporary area lossage in the error handler.
        (SETQ PERMANENT-NAME (UNLESS (= (%AREA-NUMBER NAME) QCOMPILE-TEMPORARY-AREA)
                               (WHEN (SYMBOL-PACKAGE NAME)
                                 NAME)))
        ;; Now increment one or more of the counters of variables
        ;; and maybe make an entry on *LOCAL-MAP* or *ARG-MAP*
        (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
               (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) *ARG-MAP*)
               (AND (= (SETQ ARGN (1+ ARGN)) #o101)
                    (WARN 'TOO-MANY-SLOTS :IMPLEMENTATION-LIMIT
                          "More than 64. arguments accepted by one function.")))
              ((OR (EQ TYPE 'FEF-LOCAL)
                   (NOT (MEMQ KIND '(FEF-ARG-INTERNAL FEF-ARG-INTERNAL-AUX))))
               (COND ((NOT (VAR-OVERLAP-VAR V))
                      (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) *LOCAL-MAP*)
                      (AND (= (SETQ LVCNT (1+ LVCNT)) 65.)
                           (WARN 'TOO-MANY-SLOTS :IMPLEMENTATION-LIMIT
                                 "More than 64. local variable slots required by one function.")))
                     (T (LET ((L1 (NTHCDR (- (LENGTH *LOCAL-MAP*)
                                             (CADR (VAR-LAP-ADDRESS V))
                                             1)
                                          *LOCAL-MAP*)))
                          (OR (NULL PERMANENT-NAME)
                              (MEMQ NAME (CAR L1))
                              (PUSH NAME (CAR L1))))))))))
    (DOLIST (V *ALLVARS*)                               ;Fix FIXE's put in by VAR-COMPUTE-INIT
      (AND (EQ (CAR (VAR-INIT V)) 'FEF-INI-EFF-ADR)
           (EQ (CAADR (VAR-INIT V)) 'FIXE)
           (SETF (CADADR (VAR-INIT V)) (VAR-LAP-ADDRESS (CADR (CADADR (VAR-INIT V)))))))
    (SETQ *LOCAL-MAP* (NREVERSE *LOCAL-MAP*)
          *ARG-MAP* (NREVERSE *ARG-MAP*))
    ;; Clobber all nonspecial varnames in elements of
    ;; *CLOBBER-NONSPECIAL-VARS-LISTS* with NIL.
    ;; Clobber away all-NIL tails of those lists with NIL.
    (DOLIST (L *CLOBBER-NONSPECIAL-VARS-LISTS*)
      (LET ((LAST-NON-NIL-PTR L))
        (DO ((L1 L (CDR L1)))
            ((NULL L1))
          (LET ((HOME (find (CAR L1) *ALLVARS* :key #'var-name)))
            (IF (AND HOME (EQ (VAR-TYPE HOME) 'FEF-LOCAL))
                (RPLACA L1 NIL)
                (SETQ LAST-NON-NIL-PTR L1))))
        (IF LAST-NON-NIL-PTR
            (RPLACD LAST-NON-NIL-PTR NIL))))
    LVCNT))

;;; Given a variable home, compute its VAR-INIT and install it.
;;; When we are called, the VAR-INIT contains the data for us to work on
;;; which looks like (init-form arg-supplied-flag-name).
;;; Note that for a FEF-ARG-INTERNAL-AUX variable, the init-type will
;;; always be FEF-INI-COMP-C.
;;; At time of call, *VARS* should be bound to the environment for
;;; execution of the init form for this variable.
(DEFUN VAR-COMPUTE-INIT (HOME PARALLEL)
  (LET* ((NAME (VAR-NAME HOME))
         (KIND (VAR-KIND HOME))
         (TYPE (VAR-TYPE HOME))
         (INIT-SPECS (VAR-INIT HOME))
         (INIT-FORM (CAR INIT-SPECS))
         (SPECIFIED-FLAG-NAME (CADR INIT-SPECS))
         INIT-TYPE
         INIT-DATA)
    (COND ((NULL INIT-FORM))
          ((EQ (CAR-SAFE INIT-FORM) 'QUOTE))
          ((self-evaluating-p init-form)
           (SETQ INIT-FORM `',INIT-FORM))
          ((EQUAL INIT-FORM '(UNDEFINED-VALUE))
           ;;This is simplest thing that works.
           ;; More hair is not needed for the ways these are usually generated by SETF.
           (SETQ TLFUNINIT T))
          (T
           ;; Init is not NIL, constant or self => must P1 it, and maybe set TLFUNINIT.
           (LET ((*TLEVEL* NIL))
             (SETQ INIT-FORM (P1V INIT-FORM 1)))
           (UNLESS (ADRREFP INIT-FORM)
             (SETQ TLFUNINIT T))))
    ;; Now that we have processed the init form, determine the ADL initialization field.
    ;; First, must we, or would we rather, use code to initialize the variable?
    ;; Note: specified-flags MUST be initted at entry time regardless of anything else.
    (WHEN (AND (NOT (MEMQ 'FEF-ARG-SPECIFIED-FLAG (VAR-MISC HOME)))
               (OR (EQ KIND 'FEF-ARG-INTERNAL-AUX) TLFUNINIT
                   ;; Don't spoil the fast arg option with nontrivial inits for aux's.
                   (AND (EQ KIND 'FEF-ARG-AUX)
                        *FAST-ARGS-POSSIBLE*
                        (NOT (SI:MEMBER-EQUAL INIT-FORM '(NIL 'NIL))))
                   (IF PARALLEL (NEQ TYPE 'FEF-LOCAL))))
      (SETQ INIT-TYPE 'FEF-INI-COMP-C)
      ;; Note: if we are initting by code, there is no advantage
      ;; in binding at function entry, and doing so would
      ;; make lap stupidly turn off the fast arg option!
      (AND (EQ KIND 'FEF-ARG-AUX)
           (SETF (VAR-KIND HOME) (SETQ KIND 'FEF-ARG-INTERNAL-AUX)))
      (SETQ TLFUNINIT T))
    ;; If we aren't forced already not to use an init, figure out
    ;; what type of init to use if there's no init-form: either "none" or "nil".
    (UNLESS INIT-TYPE
      (SETQ INIT-TYPE
            (IF (OR (EQ KIND 'FEF-ARG-OPT)
                    (AND (EQ KIND 'FEF-ARG-AUX)
                         (MEMQ TYPE '(FEF-SPECIAL FEF-REMOTE))))
                'FEF-INI-NIL
                'FEF-INI-NONE)))
    ;; Then, if there is an init form, gobble it.
    (WHEN (AND INIT-FORM (NEQ INIT-TYPE 'FEF-INI-COMP-C))
      (COND ((NOT (MEMQ KIND
                        '(FEF-ARG-OPT FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
             (WARN 'BAD-ARGUMENT-LIST :IMPOSSIBLE
                   "The mandatory argument ~S was given a default value."
                   NAME))
            ;; There's a hack for binding a special var to itself.
            ((AND (EQ NAME INIT-FORM)
                  (NEQ TYPE 'FEF-LOCAL))
             (SETQ INIT-TYPE 'FEF-INI-SELF))
            ((ATOM INIT-FORM)
             (SETQ INIT-TYPE 'FEF-INI-C-PNTR)
             (SETQ INIT-DATA (LIST 'LOCATIVE-TO-S-V-CELL INIT-FORM)))
            ((MEMQ (CAR INIT-FORM) '(LOCAL-REF))
             (SETQ INIT-TYPE 'FEF-INI-EFF-ADR)  ;Initted to value of local var
             (SETQ INIT-DATA (LIST 'FIXE INIT-FORM)))
            ((MEMQ (CAR INIT-FORM) '(QUOTE FUNCTION BREAKOFF-FUNCTION SELF-REF))
             (SETQ INIT-TYPE 'FEF-INI-PNTR)
             (SETQ INIT-DATA INIT-FORM))
            (T (BARF INIT-FORM "Init-form calculation confused"))))
    (COND ((AND (EQ KIND 'FEF-ARG-OPT)
                (OR TLFUNINIT SPECIFIED-FLAG-NAME))
           ;; Once an opt arg gets an alternate starting address,
           ;; all following args must be similar or else FEF-INI-COMP-C.
           (SETQ TLFUNINIT T)
           (SETQ INIT-TYPE 'FEF-INI-OPT-SA)
           (SETQ INIT-DATA (gensymbol name)))
          ;; If something not an optional arg was given a specified-flag,
          ;; discard that flag now.  There has already been an error message.
          (T (SETQ SPECIFIED-FLAG-NAME NIL)))
    (SETF (VAR-INIT HOME)
          (LIST* INIT-TYPE INIT-DATA
                 (AND SPECIFIED-FLAG-NAME
                      (DOLIST (V *ALLVARS*)
                        (AND (EQ (VAR-NAME V) SPECIFIED-FLAG-NAME)
                             (MEMQ 'FEF-ARG-SPECIFIED-FLAG (VAR-MISC V))
                             (RETURN V))))))
    (IF (NULL INIT-FORM)
        NAME
        (LIST NAME INIT-FORM))))

;;; (MULTIPLE-VALUE-BIND variable-list m-v-returning-form . body)
;;; turns into (MULTIPLE-VALUE-BIND variable-list vars-segment m-v-returning-form . body)
;;; where vars-segment is a sublist of *VARS* that should be pushed onto *VARS*
;;; while this form is being processed on pass 2.
(DEFUN (:PROPERTY MULTIPLE-VALUE-BIND P1) (FORM)
  (LET ((VARIABLES (CADR FORM))
        (*VARS* *VARS*)
        OUTER-VARS
        (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
        (M-V-FORM (CADDR FORM)))
    (MULTIPLE-VALUE-BIND (BODY THIS-FRAME-DECLARATIONS)
        (with-list (env *function-environment*)
          (EXTRACT-DECLARATIONS-RECORD-MACROS (CDDDR FORM) NIL NIL ENV))
      (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
      (SETQ OUTER-VARS *VARS*)
      (if *tlevel* (clear-tlevel))
      ;; P1 the m-v-returning-form outside the bindings we make.
      (cond ((null variables)
             ;; this never usually gets here due to optimizer
             (setq m-v-form (p1v m-v-form nil)))
            ((atom variables)
             (warn 'invalid-form :impossible "~S's first arg should be a list of variables: ~S"
                   'multiple-value-bind variables)
             (setq variables nil)
             (setq m-v-form (p1v m-v-form nil)))
            (t
             (setq m-v-form (p1v m-v-form (length variables)))))
      ;; The code should initialize each variable by popping off the stack.
      ;; The values will be in forward order so we must pop in reverse order.
      (SETQ VARIABLES (MAPCAR (LAMBDA (V) `(,V (%POP))) VARIABLES))
      (P1SBIND VARIABLES 'FEF-ARG-INTERNAL-AUX T T THIS-FRAME-DECLARATIONS)
      (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
      ;; Return something with the same sort of arguments a LET has when given to pass 2,
      ;; but with the multiple value producing form as an additional argument at the front.
      `(MULTIPLE-VALUE-BIND ,M-V-FORM ,VARIABLES ,OUTER-VARS ,*VARS*
        . ,(CDDDDR (P1 `(LET () . ,BODY)))))))

(DEFUN PROCESS-SPECIAL-DECLARATIONS (DECLS)
  (DOLIST (DECL DECLS)
    (IF (EQ (CAR DECL) 'SPECIAL)
        (DOLIST (VARNAME (CDR DECL))
          (PUSHNEW VARNAME *FREEVARS* :TEST #'EQ)
          (PUSH (MAKE-FREE-VAR-HOME VARNAME) *VARS*)))))

(DEFPROP WITH-STACK-LIST P1-WITH-STACK-LIST P1)
(DEFPROP WITH-STACK-LIST* P1-WITH-STACK-LIST P1)

(DEFUN P1-WITH-STACK-LIST (FORM)
  (ecase *target-computer*
    ;; Arguments have been made that WITRH-STACK-LIST is no longer winning
    ;; and should not be used even on the lambda.  Who knows??  We will not
    ;; implement it fresh on the K, in any case.
    (lambda-interface
     ;; What a winnning compiler!  No fewer than 4 special forms needed to compile
     ;;  with-stack-list(*)
     (P1 `(BLOCK-FOR-WITH-STACK-LIST P1-WITH-STACK-LIST
                                     (CHANGE-PDLLVL
                                       ,(LENGTH (CDADR FORM))
                                       (%PUSH (,(CASE (CAR FORM)
                                                  (WITH-STACK-LIST '%MAKE-EXPLICIT-STACK-LIST)
                                                  (WITH-STACK-LIST* '%MAKE-EXPLICIT-STACK-LIST*))
                                               ,@(CDADR FORM))))
                                     (LET ((,(CAADR FORM) (%POP-FOR-WITH-STACK-LIST)))
                                       ,@(CDDR FORM)))))
    (k
     (p1 `(let ((,(caadr form) (,(case (car form)
                                   (with-stack-list 'list)
                                   (with-stack-list* 'list*))
                                ,@(cdadr form))))
            ,@(cddr form))))))

(DEFUN (:PROPERTY CHANGE-PDLLVL P1) (FORM)
  `(,(CAR FORM) ,(CADR FORM) . ,(MAPCAR #'P1 (CDDR FORM))))

(DEFPROP %MAKE-EXPLICIT-STACK-LIST P1EVARGS P1)
(DEFPROP %MAKE-EXPLICIT-STACK-LIST* P1EVARGS P1)

; Prevent warnings no matter how many args.
;(DEFPROP LIST ((#o777777 (FEF-ARG-OPT FEF-QT-EVAL))) ARGDESC)
;(DEFPROP LIST* ((#o777777 (FEF-ARG-OPT FEF-QT-EVAL))) ARGDESC)

(DEFPROP LIST P1ASSOC P1)
(DEFPROP NCONC P1ASSOC P1)
(DEFPROP APPEND P1ASSOC P1)
(DEFPROP LIST* P1ASSOC P1)

;; Convert single calls with too many args to multiple calls.
(DEFUN P1ASSOC (FORM)
  (IF (< (LENGTH FORM) 64.)
      (P1EVARGS FORM)
    (P1 `(,(IF (EQ (CAR FORM) 'LIST)
               'LIST*
               (CAR FORM))
           ,@(FIRSTN 61. (CDR FORM))
           (,(CAR FORM)
            . ,(NTHCDR 61. (CDR FORM)))))))

(DEFUN UNDEFINED-VALUE () NIL)

;;; Analyze a LET's variable bindings and tags,
;;; and convert it to an internal form which looks like
;;; (LET* <variable list, with keywords processed and removed>
;;;       <value of *VARS* for body of this prog>
;;;       <T if %BIND used within this prog>
;;;       <*LEXICAL-CLOSURE-COUNT* at start of LET>
;;;       <*LEXICAL-CLOSURE-COUNT* at end of LET>
;;;       . <body, P1'ified>)

;;; LET* does sequential binding, and LET does parallel binding.
;;; P1LAMBDA and P1AUX generate LET or LET* as appropriate.

(DEFUN P1LET (FORM &OPTIONAL FOR-AUXVARS)
  (unless (cl:listp (cadr form))
    (warn 'invalid-form :impossible
          "The second element, ~S, of the ~S special form is not a list"
          (cadr form) (car form))
    (setq form `(,(car form) () . ,(cddr form))))
  (LET ((*VARS* *VARS*)
        OUTER-VARS
        (FN (CAR FORM))
        (*BINDP* NIL)                                   ;%bind not yet used
        (VLIST (CADR FORM))
        (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
        (ENTRY-LEXICAL-CLOSURE-COUNT *LEXICAL-CLOSURE-COUNT*))
    (IF (EQ FN 'LET-FOR-AUXVARS) (SETQ FN 'LET*))
    (dolist (var vlist)
      (cond ((symbolp var))
            ((or (atom var)
                 (not (symbolp (car var))))
             (warn 'variable-not-symbol :impossible
                   "~S appears in a list of variables to be bound."
                   (or (car-safe var) var))
             (setq vlist (remq var vlist)))
            ((not (cl:listp (cdr var)))
             (warn 'invalid-form :impossible
                   "The ~S variable spec ~S is not a proper list"
                   (car form) var)
             (setq vlist (remq var vlist)))
            ((cddr var)
             (warn 'invalid-form :implausible
                   "More than two forms in ~S variable spec ~S"
                   (car form) var)
             (setq vlist (cl:subst (list (car var) (cdr var)) var vlist)))))
    (MULTIPLE-VALUE-BIND (BODY THIS-FRAME-DECLARATIONS)
        (with-list (env *function-environment*)
          (EXTRACT-DECLARATIONS-RECORD-MACROS (CDDR FORM) NIL NIL ENV))
      (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
      (SETQ OUTER-VARS *VARS*)
      ;; Treat parallel binding as serial if it doesn't matter.
      (WHEN (OR (NULL (CDR VLIST))                      ; ie if only 1 symbol
                (AND (EQ FN 'LET)
                     (DOLIST (XX VLIST)
                       ;; or if binding each symbol to NIL, a constant, or itself.
                       (OR (ATOM XX)                    ;(let (x) ...)
                           (CONSTANTP (CADR XX))        ;(let ((x 'foo)) ...)
                           (EQ (CAR XX) (CADR XX))      ;(let ((x x)) ...)
                           (RETURN NIL)))))
        (SETQ FN 'LET*))
      ;; Flush rebinding a var to itself if it isn't special
      ;; and range of rebinding is rest of function.
      (IF *TLEVEL*
          (SETQ VLIST (SUBSET-NOT (LAMBDA (VAR)
                                    (AND (CONSP VAR)
                                         (EQ (CAR VAR) (CADR VAR))
                                         (EQ (FIND-TYPE (CAR VAR) THIS-FRAME-DECLARATIONS)
                                             'FEF-LOCAL)
                                         (EQ (VAR-TYPE (find (CAR VAR) *VARS* :key #'var-name))
                                             'FEF-LOCAL)))
                                  VLIST)))
      ;; All the local declarations should be in effect for the init forms.
      (SETQ LOCAL-DECLARATIONS (APPEND THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
      ;; &AUX vars should be allowed to inherit special declarations
      ;; since that is what it looks like when you put a DECLARE inside the body.
      (IF FOR-AUXVARS
          (SETQ THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
      (SETQ VLIST (P1SBIND VLIST
                           (IF *TLEVEL* 'FEF-ARG-AUX 'FEF-ARG-INTERNAL-AUX)
                           (EQ FN 'LET)
                           NIL
                           THIS-FRAME-DECLARATIONS))
      ;; Now convert initial SETQs to variable initializations.
      ;; We win only for SETQs of variables bound but with no initialization spec'd,
      ;; which set them to constant values, and only if later vars' inits didn't use them.
      ;; When we come to anything other than a SETQ we can win for, we stop.
      ;; For LET*, we can't win for a special variable if anyone has called a function
      ;; to do initting, since that function might have referred to the special.
      ;; Even if we don't use tha ADL to init them,
      ;; we avoid redundant settings to NIL.
      (DO ((*P1VALUE* 1)                                ;setq only wants one value
           TEM HOME)
          (())
        (COND ((EQUAL (CAR BODY) '((SETQ)))
               (POP BODY))
              ((OR (ATOM (CAR BODY))
                   ;; Don't save the optimized form here because it was optimized
                   ;; with *p1value* set incorrectly.  We cannot just fix *p1value*
                   ;; either because we don't yet know how many values (if any) we
                   ;; do want.  Of course, we really can't trust the optimizer to do
                   ;; the right thing if *p1value* is bogus, so what are we doing here?
                   ;; - JRM 1-May-87 18:03:05
                   (let ((tem (COMPILER-OPTIMIZE (car BODY))))
                     ;(push tem body)
                     (ATOM TEM))
                   (NOT (EQ (CAR TEM) 'SETQ))
                   (NOT (MEMQ (CADR TEM) VLIST))        ;we're binding it
                   (NOT (CONSTANTP (CADDR TEM)))        ;initializing to constant
                   (AND (SPECIALP (CADR TEM))
                        (OR TLFUNINIT (NOT *TLEVEL*))
                        (EQ FN 'LET*))
                   (NOT (ZEROP (VAR-USE-COUNT (SETQ HOME
                                                    (find (CADR TEM) *VARS* :key #'var-name))))))
               (RETURN NIL))
              (T (SETQ BODY (CONS `(SETQ . ,(CDDDR TEM)) (CDR BODY)))
                 (SETF (CAR (MEMQ (CADR TEM) VLIST))
                       `(,(CADR TEM) ,(P1 (CADDR TEM))))
                 ;; For a variable bound at function entry, really set up its init.
                 ;; Other vars (FEF-ARG-INTERNAL-AUX) will be initted by code,
                 ;; despite our optimization, but it will be better code.
                 (AND *TLEVEL* (EQ (VAR-KIND HOME) 'FEF-ARG-AUX)
                      (SETF (VAR-INIT HOME) `(FEF-INI-PNTR ,(P1 (CADDR TEM))))))))
      ;; Now P1 process what is left of the body.
      (WHEN (CDR BODY) (if *tlevel* (clear-tlevel)))
      (SETQ BODY (P1PROGN-1 BODY))
      `(,FN ,VLIST ,OUTER-VARS ,*VARS* ,*BINDP*
        ,ENTRY-LEXICAL-CLOSURE-COUNT ,*LEXICAL-CLOSURE-COUNT*
        . ,BODY))))

;;; MEMQ and ASSQ together.  Find the tail of VLIST
;;; whose CAR or CAAR is VARNAME.
(DEFUN P1LET-VAR-FIND (VARNAME VLIST)
  (DO ((VL VLIST (CDR VL))) ((NULL VL) NIL)
    (AND (OR (EQ VARNAME (CAR VL))
             (AND (CONSP (CAR VL))
                  (EQ VARNAME (CAAR VL))))
         (RETURN VL))))

;;; This is what &AUX vars get bound by.
(DEFUN (:PROPERTY LET-FOR-AUXVARS P1) (FORM)
  (P1LET FORM T))

;>> Huh?!
;;; This is supposed to differ from regular LET
;;; by preventing declarations in the body from applying to the variable init forms.
;;; It also must not turn SETQs into init forms, because the declarations
;;; do apply to the SETQs within the body.
(DEFUN (:PROPERTY LET-FOR-LAMBDA P1) (FORM)
  (LET ((*VARS* *VARS*)
        OUTER-VARS
        (*BINDP*)                               ;%bind not yet used
        (VLIST)
        (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
        (ENTRY-LEXICAL-CLOSURE-COUNT *LEXICAL-CLOSURE-COUNT*))
    (MULTIPLE-VALUE-BIND (BODY THIS-FRAME-DECLARATIONS)
        (with-list (env *function-environment*)
          (EXTRACT-DECLARATIONS-RECORD-MACROS (CDDR FORM) NIL NIL env))
      (SETQ VLIST (P1SBIND (CADR FORM)
                           (IF *TLEVEL* 'FEF-ARG-AUX 'FEF-ARG-INTERNAL-AUX)
                           T
                           NIL
                           THIS-FRAME-DECLARATIONS))
      (SETQ OUTER-VARS *VARS*)
      (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
      ;; Do this here so that the local declarations
      ;; do not affect the init forms.
      (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
      ;; Now P1 process what is left of the body.
      (AND (CDR BODY) (if *tlevel* (clear-tlevel)))
      (SETQ BODY (P1PROGN-1 BODY))
      `(LET-FOR-LAMBDA ,VLIST ,THIS-FRAME-DECLARATIONS ,OUTER-VARS ,*BINDP*
                       ,ENTRY-LEXICAL-CLOSURE-COUNT ,*LEXICAL-CLOSURE-COUNT*
                       . ,BODY))))

;;;; BLOCK and RETURN-FROM.
;;; These know how to turn into catches and throws
;;; when necessary for general lexical scoping.

(defun block-atom-name (blockname)
  (typecase blockname
    ((or symbol string) blockname)
    (list (case (first blockname)
            (:internal (string-append (block-atom-name (second blockname))
                                      (typecase (third blockname)
                                        ((or symbol string) (third blockname))
                                        (otherwise (format nil "~A-" (third blockname))))))
            (:property (second blockname))
            (otherwise (format nil "~A" (rest blockname)))))
    (otherwise (format nil "~A" blockname))))

(DEFPROP BLOCK P1BLOCK P1)
(DEFPROP BLOCK-FOR-WITH-STACK-LIST P1BLOCK P1)
(DEFUN P1BLOCK (FORM &OPTIONAL ALSO-BLOCK-NAMED-NIL)
  (LET* ((PROGNAME (CADR FORM)) (BODY (CDDR FORM))
         (RETTAG (gensymbol (block-atom-name progname)))
         (*GOTAG-ENVIRONMENT* *gotag-environment*)
         (*PROGDESC-ENVIRONMENT* *PROGDESC-ENVIRONMENT*))
    (WHEN (AND ALSO-BLOCK-NAMED-NIL
               (NEQ PROGNAME 'T)
               (NEQ PROGNAME 'NIL))
      (when (eql *p1value* 0)
        (fsignal "Nothing in P1BLOCK?"))
      (PUSH (MAKE-PROGDESC :NAME 'NIL
                           :RETTAG RETTAG
                           :NBINDS 0
                           ;;>> progdesc-m-v-target is used differently in p2
                           ;;>> This value is just for the benefit return-from p1 compilation
                           ;;>> p2block mungs this slot to the correct p2 destination
                           :m-v-target *p1value*
                           :ENTRY-LEXICAL-CLOSURE-COUNT *LEXICAL-CLOSURE-COUNT*)
              *PROGDESC-ENVIRONMENT*))
    (PUSH (MAKE-PROGDESC :NAME PROGNAME
                         :RETTAG RETTAG
                         :NBINDS 0
                         ;;>> See above comment
                         :m-v-target *p1value*
                         ;; :VARS *VARS*
                         :ENTRY-LEXICAL-CLOSURE-COUNT *LEXICAL-CLOSURE-COUNT*)
          *PROGDESC-ENVIRONMENT*)
    (AND (CDR BODY) (if *tlevel* (clear-tlevel)))
    (SETQ BODY (P1PROGN-1 BODY))
    ;; Push on *GOTAG-ENVIRONMENT* a description of this prog's "return tag",
    ;; a tag we generate and stick at the end of the prog.
    (PUSH (MAKE-GOTAG RETTAG RETTAG NIL (CAR *PROGDESC-ENVIRONMENT*)) *GOTAG-ENVIRONMENT*)
    (SETF (PROGDESC-EXIT-LEXICAL-CLOSURE-COUNT (CAR *PROGDESC-ENVIRONMENT*))
          *LEXICAL-CLOSURE-COUNT*)
    (LET ((BLOCK
            `(,(CAR FORM) ,*GOTAG-ENVIRONMENT* ,(CAR *PROGDESC-ENVIRONMENT*) . ,BODY)))
      (IF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG (CAR *PROGDESC-ENVIRONMENT*))
          (LET ((VARNAME (gensymbol rettag "-LEXICAL-BLOCK-TAG"))
                HOME)
            ;; For a BLOCK name used from internal lambdas,
            ;; we make a local variable to hold the catch tag
            ;; (which is a locative pointer to that variable's own slot).
            ;; The internal lambda accesses this variable via the lexical scoping mechanism.
            ;; This ensures the proper lexical scoping when there are
            ;; multiple activations of the same block.
            (SETQ HOME (VAR-MAKE-HOME VARNAME 'FEF-LOCAL 'FEF-ARG-INTERNAL-AUX
                                      NIL 'FEF-QT-EVAL '(FEF-ARG-USED-IN-LEXICAL-CLOSURES)
                                      NIL))
            (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG (CAR *PROGDESC-ENVIRONMENT*))
                  HOME)
            (PUSH HOME *ALLVARS*)
            (compiler-target-switch (VAR-COMPUTE-INIT HOME NIL))
            (INCF (VAR-USE-COUNT HOME))
            `(PROGN
               (SETQ (LOCAL-REF ,HOME) (VARIABLE-LOCATION (LOCAL-REF ,HOME)))
               ;; must be *catch
               (*CATCH (LOCAL-REF ,HOME) ,BLOCK)))
        BLOCK))))

;;; Defines a block with two names, the specified name and NIL.
(DEFUN (:PROPERTY BLOCK-FOR-PROG P1) (FORM)
  (P1BLOCK FORM T))

(DEFPROP RETURN-FROM P1RETURN-FROM P1)
;; Copied from LAD: RELEASE-3.SYS; QCP1.LISP#663 on 3-Oct-86 14:36:13
(DEFUN P1RETURN-FROM (FORM)
  (case (length (cdr form))
    (0 (warn 'wrong-number-of-arguments :impossible
             "~S called with no block-name from which to return." 'return-from)
       (return-from p1return-from nil))
    ;; common lisp says that (return)  (return-from nil nil)
    (1 (let ((progdesc (find (cadr form) *progdesc-environment* :key #'progdesc-name)))
         (cond ((null progdesc))        ;gets an error below
; This was deemed "too obnoxious for just now" by GJC and MHD.  However the case
; of 2 (3) or more args to return (return-from) was deemed worthy of warning (see below)!
;;>> Mly: this warning be in here for a while at least.
;;>>  There is old code that assumes that (return)  (return (values))
;;>>  Changing the semantics of old programs will hurt users without
;;>>  warning them that they are going to hurt.
;;>>  (I know this from personal experience -- I changed this some time ago
;;>>   without making it give a warning, and completely broke the reader.)
;;>>  Yes, this message may happen more frequently than one would like,
;;>>   and the user may have known what s/he was doing, but there is not other
;;>>   way to catch this problem in older programs so that they can be fixed.
;;>>  Note also that the warning is only given when the RETURN is returning
;;>>   from a function call -- this is less common than "(progn (do (#) ... (return) ..) ...)"
;;;> MHD: There is very little old code that makes such an assumption, and there is a
;;;> a point where it's enough just to document the change in release notes. Multiple values
;;;> are a relatively recent invention historically.  Return is not used that often in really
;;;> lispy code anyway.  Of the small fraction of old code that hasn't been updated
;;;> yet and meant to say the equivalent of (return (values)), very little of it
;;;> cares how many values are returned.  Remember that multiple-value-bind binds args
;;;> for which no value is supplied to nil, now as before.  Of course,
;;;>     (length (multiple-value-list (old-function-that-calls-return)))
;;;> may now be a different number.  (This system is not quite the ideal
;;;> for developing common lisp compatible programs on anyway.  You don't get a
;;;> warning for using plist or string-append.  So can you move your reader to the
;;;> Sun? It is really easy to find (return) by (tag) searching for it also.  Finally,
;;;> people deserve to have no warnings come out at all when they write non-obsolete
;;;> (i.e. common-lisp) code.  Return of no or 1 args is not obsolete, but return of
;;;> 2 or more args is obsolete, and that is what the warnings should reflect.
;;;>  - The End - 8/1/86 -
;              ((eq (progdesc-m-v-target progdesc) t)
;               (warn 'return-no-values :obsolete
;                     "~To return no values one should use ~S, not ~S~%   ~
;                       (The latter form returns the single value ~S)~"
;                     (if (eq (car form) 'return)
;                         '(return (values))
;                         `(return-from ,(cadr form) (values)))
;                     (if (eq (car form) 'return)
;                         '(return)
;                         `(return-from ,(cadr form)))
;                     'nil)
;               )
               )
         (setq form `(return-from ,(cadr form) nil))))
    (2)
    (t
;>> In spite of the Brand S release 6 compiler, this is too obnoxious for just now.
; OK now (see above)! - GJC and MHD
       (if *check-style-p*
           (let ((*print-length* 5) (*print-level* 2))
             (warn 'wrong-number-of-arguments :obsolete
                   "~S called with multiple value arguments~%   ~
                        use ~S instead."
                   (car form)
                   (if (eq (car form) 'return)
                       `(return (values . ,(cddr form)))
                       `(return-from ,(cadr form) (values . ,(cddr form)))))))
       (setq form `(return-from ,(cadr form) (values . ,(cddr form))))))
  (LET ((PROGDESC (find (second form) *progdesc-environment* :key #'progdesc-name)))
    (COND ((MEMQ PROGDESC *OUTER-CONTEXT-PROGDESC-ENVIRONMENT*)
           ;; Need the *
           `(*THROW ,(TRY-REF-LEXICAL-HOME (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG PROGDESC))
                    ,(p1v (third form) (progdesc-m-v-target progdesc))))
          ((memq progdesc *progdesc-environment*)
           `(return-from ,(cadr form) ,(p1v (third form) (progdesc-m-v-target progdesc))))
          (t
           (warn 'bad-block-name :impossible
                 "There is a ~S ~S not inside a ~S of that name."
                 'return-from (cadr form) 'block)
           nil))))

(defun (:property return p1) (form)
  ;; Yes, leave the car as 'return so that p1return-from can hack error messages
  (p1return-from `(return nil . ,(cdr form))))

(defun (:property return-list p1) (form)
  (p1return-from `(return-from nil (values-list ,(cadr form)))))


;(disassemble (compile (defun foo (x)
;  (block lose
;    (tagbody lose big
;      (xmapcar (lambda (a)
;        (tagbody big lossage
;          (block fred
;            (case a
;              (1 (return-from lose (values 1 2 3)))
;              (2 (return-from fred 2))
;              (3 (go lose))
;              (4 (go lossage))
;              (5 (go big))
;              (6 (go hack))
;              (t (cons a x))))))
;         x)
;       hack)))))

;; Copied from LAD: RELEASE-3.SYS; QCP1.LISP#665 on 27-Mar-87 13:16:20
(defun gensym-for-number (n)
  (cadr (or (assoc n *numeric-tag-alist*)
            (car (push `(,n ,(gensymbol "TAG-" n)) *numeric-tag-alist*)))))

;; Copied from LAD: RELEASE-3.SYS; QCP1.LISP#665 on 27-Mar-87 13:16:21
;; 11/20/86 Added support for integer tags. -rpp
(defvar *numeric-tag-alist* nil)

(DEFPROP TAGBODY P1TAGBODY P1)
;; Copied from LAD: RELEASE-3.SYS; QCP1.LISP#665 on 27-Mar-87 13:16:21
(DEFUN P1TAGBODY (FORM)
  (LET* ((BODY (copy-list (cdr FORM)))
         (MYPROGDESC (MAKE-PROGDESC :NAME '(TAGBODY)
                                    :NBINDS 0
                                    :m-v-target nil))
         (*gotag-environment* *gotag-environment*)
         (this-gotag-environment ()))
    (WHEN (CDR BODY) (if *tlevel* (clear-tlevel)))
    (loop for tail on body
          as elt = (car tail)
      do (cond ((consp elt))
               ((eq (memq elt body) tail)
                (when (fixnump elt) ;Replace numeric tag with gensym.
                  (let ((gensym-for-numeric-tag (gensym-for-number elt)))
                    (setf (car tail) gensym-for-numeric-tag)
                    (setq elt gensym-for-numeric-tag)))
                (PUSH ELT ALLGOTAGS)
                (SETQ ELT (MAKE-GOTAG ELT (IF (MEMQ ELT (CDR ALLGOTAGS))
                                              (gensymbol elt)
                                            ELT)
                                      NIL MYPROGDESC))
                (push elt *GOTAG-ENVIRONMENT*)
                (push elt this-gotag-environment))
               (T
                (WARN 'DUPLICATE-TAGBODY-TAG :IMPLAUSIBLE
                      "The ~S tag ~S appears more than once in one ~S."
                      'TAGBODY elt 'TAGBODY))))
    (SETQ BODY (loop for tail on (copy-list body)
                     as elt = (car tail)
                  when (and (atom elt)
                            (memq elt (cdr tail)))
                    ;; Replace duplicate progtags with something that
                    ;; will be ignored by pass 2, to avoid making LAP get unhappy.
                    do (setf (cdr tail) (delq elt (cdr tail)))
                  collect (if (consp elt) (p1v elt nil) elt)))
    (IF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG MYPROGDESC)
        (p1tagbody-hack-lexical-tags myprogdesc this-gotag-environment body)
      `(tagbody ,this-gotag-environment ,myprogdesc . ,body))))

;body has already been processed by P1
(defun p1tagbody-hack-lexical-tags (myprogdesc this-gotag-environment body)
  (LET* ((clauses (loop for g in this-gotag-environment
                        when (gotag-used-in-lexical-closures-flag g)
                        collect `(,(gotag-prog-tag g) (go-hack ,g))))
         (framework
           (p1 (let ((tag (gensymbol (progdesc-name myprogdesc) "-LEXICAL-GO")))
                 `(block ,tag
                    (let (,tag)
                      (setq ,tag (variable-location ,tag))
                      (case (catch ,tag
                              (return-from ,tag
        ;P1-TAGBODY-HACK returns its argument as its P1 expansion!
                                (p1-tagbody-hack (tagbody ,this-gotag-environment ,myprogdesc . ,body))))
                        . ,clauses)))))))
    (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG MYPROGDESC)
          (CAR *ALLVARS*))
    (PUSHNEW 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
             (VAR-MISC (CAR *ALLVARS*)))
 ;    (let ((si:*print-circle* t))
 ;      (with-open-stream (stream (open "ed-file:new-hack"))
 ;      (print framework stream)))
    framework))

(defun (:property p1-tagbody-hack p1) (form)
  (cadr form))

;       ;; this is really really really really really really really ugly.
;(defun p1tagbody-hack-lexical-tags (myprogdesc this-gotag-environment body)
;  (LET* ((clauses (loop for g in this-gotag-environment
;                       when (gotag-used-in-lexical-closures-flag g)
;                       collect `(,(gotag-prog-tag g) (go-hack ,g))))
;        (kludge '(()))
;        ;;>> This is really nauseating.  You don't want to know a thing about it.
;        (framework
;          (p1 (let ((tag (gensym)))
;                `(block ,tag
;                   (let (,tag)
;                     (setq ,tag (variable-location ,tag))
;                     (case (catch ,tag
;                             (return-from ,tag
;                               ;; this is magically replaced below with
;                               ;; `(tagbody ,this-gotag-environment ,myprogdesc . ,body)
;                               ,kludge))
;                       . ,clauses)))))))
;    (SETF (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG MYPROGDESC)
;         (CAR *ALLVARS*))
;    (PUSHNEW 'FEF-ARG-USED-IN-LEXICAL-CLOSURES
;            (VAR-MISC (CAR *ALLVARS*)))
;    ;; this depends imtimately on the expansion of the CASE macro,
;    ;;  amongst other things (like every random detail of 69 different p1 handlers)
;    ;; I can't imagine why anybody would say we need a new compiler
;    (let ((tem (locf
;                (caddr                         ;'(()) out of return-from
;                  (cadr                        ;return-from out of progn
;                    (caddr                     ;progn from catch
;                      (if (= (length clauses) 1)
;                          (cadr                ;catch from eq
;                            (cadr              ;eq from and
;                              (nth 8.          ;body from let (and (eq (catch ...) (quote ,(gotag-prog-tag))) ...)
;                                   (cadddr     ;let (of ,tag) from block
;                                     framework))))
;                        (cadr                  ;catch from binding
;                          (caadr               ;first binding (.select.item.) from let*
;                            (nth 8.            ;let* (of case macro stuff) from let
;                                 (cadddr       ;let (of ,tag) from block (named ,tag)
;                                   framework)))))))))))
;      ;; EQUAL instead of EQ, because P1 can copy things as it desires.
;      (if (equal (contents tem) kludge)
;         (setf (contents tem) `(tagbody ,this-gotag-environment ,myprogdesc . ,body))
;       (ferror "Incorrect body placement in complex lexical TAGBODY.")))
;    (let ((si:*print-circle* t))
;      (with-open-stream (stream (open "ed-file:old-hack"))
;       (print framework stream)))
;    framework))

;; Copied from LAD: RELEASE-3.SYS; QCP1.LISP#665 on 27-Mar-87 13:16:22
(DEFUN (:PROPERTY GO P1) (FORM)
  (let ((tag (cadr form)))
    (when (fixnump tag)
      (setq form `(,(car form) ,(gensym-for-number tag)))))
  (LET ((GOTAG (find (CADR FORM) *GOTAG-ENVIRONMENT* :key #'gotag-prog-tag)))
    (IF (MEMQ GOTAG *OUTER-CONTEXT-GOTAG-ENVIRONMENT*)
        `(*THROW ,(TRY-REF-LEXICAL-HOME
                    (PROGDESC-USED-IN-LEXICAL-CLOSURES-FLAG
                      (GOTAG-PROGDESC GOTAG)))
                 ',(CADR FORM))
      FORM)))

(DEFPROP GO-HACK IDENTITY P1)

;;; PROG is now expanded into the standard primitives.
(DEFUN P1PROG (FORM)
  (LET ((FN (CAR FORM))
        PROGNAME VLIST)
    (SETQ FORM (CDR FORM))
    ;; Extract the prog name if there is one.
    (WHEN (AND (CAR FORM)
               (SYMBOLP (CAR FORM)))
      (SETQ PROGNAME (POP FORM)))
    (SETQ VLIST (POP FORM))
    (MULTIPLE-VALUE-BIND (BODY DECLS)
        (with-list (env *function-environment*)
          (EXTRACT-DECLARATIONS-RECORD-MACROS FORM NIL NIL env))
      (P1 `(BLOCK-FOR-PROG ,PROGNAME
                           (,(IF (EQ FN 'PROG) 'LET 'LET*)
                            ,VLIST
                            ,@(IF DECLS `((DECLARE . ,DECLS)))
                            (TAGBODY . ,BODY)))))))

;;;; FLET, LABELS, MACROLET

(defun (:property lambda p1) (form)
  (breakoff form *function-environment*))

(DEFPROP FUNCTION P1FUNCTION P1)
(DEFUN P1FUNCTION (FORM &AUX (FUNCTION (CADR FORM)))
  (COND ((SYMBOLP FUNCTION)
         (LET ((TM (ASSQ FUNCTION *LOCAL-FUNCTIONS*)))
           (IF TM                               ;Ref to a local fn made by FLET or LABELS:
               (TRY-REF-LEXICAL-HOME            ;Really ref the local var that holds it.
                 (CADR TM)
                 `(FUNCTION ,function))
             FORM)))                            ;Global function definition.
        ;;>> Should barf about macros and special forms!
        ((FUNCTIONP FUNCTION T)         ;Functional constant
         (SETQ FUNCTION (LAMBDA-MACRO-EXPAND FUNCTION))
         (IF (MEMQ (CAR-SAFE FUNCTION) '(LAMBDA NAMED-LAMBDA))
             (BREAKOFF FUNCTION *function-environment*)
           `',FUNCTION))
        ((AND (CONSP FUNCTION)                  ;Function spec
              (VALIDATE-FUNCTION-SPEC FUNCTION))
         FORM)
        (T
         (WARN 'BAD-ARGUMENT :IMPOSSIBLE
               "The argument of ~S is ~S, neither a function nor the name of one."
               'FUNCTION (CADR FORM))
         ''NIL)))

;;>> the following has the bug that compiling
;;>>   (flet ((lose () random-outer-lexical-reference))
;;>>     (macrolet ((fukt () (lose)))
;;>>       (fukt)))
;;>> should lose macroexpanding fukt, but doesn't.
(defun check-flet-args (form &aux (fns (cadr form)))
  (case (length (cdr form))
    (0 (warn 'wrong-number-of-arguments :implausible "Empty ~S form" (car form)))
    (1 (warn 'wrong-number-of-arguments :implausible "~S form has no body" (car form)))
    (t
     (if (not (cl:listp (cadr form)))
         (progn (warn 'invalid-form :impossible
                      "The second element, ~S, of the ~S special form is not a list"
                      (cadr form) (car form))
                (setq fns ()))
       (dolist (fn fns)
         (cond ((or (atom fn)
                    (not (symbolp (car fn))))
                (if (atom fn)
                    (warn 'invalid-form :impossible
                          "~S appears as ~:[a~;an~] ~S function specification."
                          fn (eq (car form) 'flet) (car form))
                    (warn 'invalid-form :impossible
                          "~S functions must be symbols: ~S"
                          (car form) (car fn)))
                (setq fns (remq fn fns)))
               ((not (cl:listp (cadr fn))))     ;will get an error later
               ((and (not (eq (car form) 'macrolet))
                     (or (memq '&quote (cadr fn))
                         (memq '&eval (cadr fn))))
                (warn 'losing-fexprs-should-be-shot-with-a-gun :obsolete
                      "Losing ~S lambda-list-keyword not allowed in ~S functions -- ignored."
                      (if (memq '&quote (cadr fn)) '&quote '&eval) (car form))
                (setq fns (cl:subst `(,(car fn)
                                      ,(remq '&quote (remq '&eval (cadr fn)))
                                      . ,(cddr fn))
                                    fn fns)))))
       (if (equal fns (cadr form))
           form
         `(,(car form) ,fns . ,(cddr form)))))))

(defun (:property flet p1) (form)
  (setq form (check-flet-args form))
  (let* ((locals (mapcar (lambda (ldef) (gensymbol "#'" (first ldef)))
                         (cadr form))))
    ;; LOCALS are local variables that really hold the functions.
    (mapc (lambda (var fndef)
            (putprop var (car fndef) 'local-function-name))
          locals (cadr form))
    ;; P1 will translate any reference to a local function
    ;; into a FUNCALL to the corresponding variable.
    (p1 `(let ,(mapcar (lambda (var def)
                         `(,var #'(named-lambda . ,def)))
                       locals (cadr form))
           (flet-internal ,(mapcar (lambda (var def)
                                     (list (car def) var `(named-lambda . ,def)))
                                   locals (cadr form))
                          . ,(cddr form)))
        'dont-rewrite)))                        ;inhibit rewrites on this pass, since
                                                ; functions with optimizers may be lexically
                                                ; shadowed, and environment isn't set up until
                                                ; flet-internal's p1 handler is executed.
                                                ; We do the optimizations of the body forms in
                                                ; that handler (by way of p1progn-1)

(defun (:property flet-internal p1) (form)
  (let ((*local-functions* *local-functions*)
        (*function-environment* *function-environment*)
        frame)
    (dolist (elt (reverse (cadr form)))
      ;; Each element of *local-functions* looks like:
      ;;  (local-function-name tempvar-name definition)
      (push `(,(car elt) ,(find (cadr elt) *vars* :key #'var-name)
              ,(caddr elt)) *local-functions*)
      (setq frame `(,(locf (symbol-function (car elt))) ,(caddr elt) . ,frame)))
    (push frame *function-environment*)
    `(progn . ,(p1progn-1 (cddr form)))))

(defun (:property labels p1) (form)
  (setq form (check-flet-args form))
  (let* ((locals (mapcar (lambda (ldef) (gensymbol "#'" (first ldef)))
                         (cadr form))))
    ;; LOCALS are local variables that really hold the functions.
    (mapc (lambda (var fndef)
            (putprop var (car fndef) 'local-function-name))
          locals (cadr form))
    ;; P1 will translate any reference to a local function
    ;; into a FUNCALL to the corresponding variable.
    (p1 `(let ,locals
           (labels-internal ,(mapcar (lambda (var def)
                                       (list (car def) var `(named-lambda . ,def)))
                                     locals (cadr form))
                            ,(mapcan (lambda (var def)
                                       `(,var #'(named-lambda . ,def)))
                                     locals (cadr form))
                            . ,(cddr form)))
        'dont-rewrite)))                        ;see above note about why we do this

(defun (:property labels-internal p1) (form)
  (let ((*local-functions* *local-functions*)
        (*function-environment* *function-environment*)
        frame)
    (dolist (elt (reverse (cadr form)))
      (push `(,(car elt) ,(find (cadr elt) *vars* :key #'var-name) ,(caddr elt))
            *local-functions*)
      (setq frame `(,(locf (symbol-function (car elt))) ,(caddr elt) . ,frame)))
    (push frame *function-environment*)
    `(progn ,(p1 `(psetq . ,(caddr form))) . ,(p1progn-1 (cdddr form)))))

(defun (:property macrolet p1) (form)
  (setq form (check-flet-args form))
  (let ((*function-environment* *function-environment*)
        ;; If we define it as a local macro, that hides any local function definition.
        (*local-functions* (rem-if (lambda (elt) (assq (car elt) (cadr form)))
                                   *local-functions*))
        frame)
    (dolist (elt (reverse (cadr form)))
      (setq frame (list* (locf (symbol-function (car elt)))
                         `(macro . ,(si::expand-defmacro elt *function-environment*))
                         frame)))
    (push frame *function-environment*)
    `(progn . ,(p1progn-1 (cddr form)))))

;;; Turn an internal lambda containing &AUX variables
;;; into one containing a LET* and having no &AUX variables.
(DEFUN P1AUX (LAMBDA)
  (LET (STANDARDIZED AUXVARS AUXLIST NONAUXLIST)
    (SETQ STANDARDIZED (SI::LAMBDA-EXP-ARGS-AND-BODY LAMBDA))
    (OR (SETQ AUXLIST (MEMQ '&AUX (CAR STANDARDIZED)))
        (RETURN-FROM P1AUX LAMBDA))
    (SETQ AUXVARS (CDR AUXLIST))
    (SETQ NONAUXLIST (LDIFF (CAR STANDARDIZED) AUXLIST))
    (DO ((VARLIST NONAUXLIST (CDR VARLIST))
         SPECIAL-FLAG)
        ((NULL VARLIST)
         (IF SPECIAL-FLAG
             (PUSH '&SPECIAL AUXVARS)))
      (COND ((EQ (CAR VARLIST) '&SPECIAL)
             (SETQ SPECIAL-FLAG T))
            ((EQ (CAR VARLIST) '&LOCAL)
             (SETQ SPECIAL-FLAG NIL))))
    (multiple-value-bind (BODY DECLS)
        (with-list (env *function-environment*)
          (EXTRACT-DECLARATIONS-RECORD-MACROS (CDR STANDARDIZED) NIL NIL env))
      `(LAMBDA ,NONAUXLIST
         (DECLARE . ,DECLS)
         (LET* ,AUXVARS
           (DECLARE . ,DECLS)
           . ,BODY)))))

;;; Turn a call to an internal lambda into a LET, and return P1 of that LET.
;;; All &AUX variables in the lambda list are extracted by P1AUX.
;;; We generate a LET, since the lambda variables should all be computed and then bound.
;;; This means that &OPTIONALs don't work quite right;
;;; but they never used to work at all in internal lambdas anyway.
;;; No checking of number of args here, because it is done elsewhere.
;;; We just eval and ignore extra args and take missing ones to be NIL.
(DEFUN P1LAMBDA (LAMBDA ARGS)
  (LET (ARGLIST BODY ARGS1 OPTIONAL PROGVARS VAR QUOTEFLAG
         SPECIAL-FLAG SPECIAL-VARS UNSPECIAL-FLAG UNSPECIAL-VARS
         KEYCHECKS BORDER-VARIABLE PSEUDO-KEYNAMES)
    (SETQ LAMBDA (SI::LAMBDA-EXP-ARGS-AND-BODY (P1AUX LAMBDA)))
    (SETQ ARGLIST (CAR LAMBDA) BODY (CDR LAMBDA))
    (MULTIPLE-VALUE-BIND (NIL NIL NIL
                          REST-ARG NIL KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
        (DECODE-KEYWORD-ARGLIST ARGLIST)
      (WHEN (AND KEYNAMES (NOT REST-ARG))
        (SETQ REST-ARG (gensymbol "REST")))
      (SETQ ARGS1 ARGS)
      (DO ((ARGLIST1 ARGLIST (CDR ARGLIST1)))
          (NIL)
        (SETQ VAR (CAR ARGLIST1))
        (COND ((NULL ARGLIST1)
               (RETURN T))
              ((EQ VAR '&KEY)
               (PUSH (LIST REST-ARG `(LIST . ,ARGS1)) PROGVARS)
               (RETURN (SETQ ARGS1 NIL)))
              ((EQ VAR '&REST)
               (POP ARGLIST1)
               (PUSH (LIST (CAR ARGLIST1) `(LIST . ,ARGS1)) PROGVARS)
               (RETURN (SETQ ARGS1 NIL)))
              ((EQ VAR '&OPTIONAL)
               (SETQ OPTIONAL T))
              ;; soon to be obsolete
              ((EQ VAR '&QUOTE)
               (SETQ QUOTEFLAG T))
              ;; soon to be obsolete
              ((EQ VAR '&EVAL)
               (SETQ QUOTEFLAG NIL))
              ((EQ VAR '&SPECIAL)
               (warn 'obsolete-lambda-list-keyword :obsolete
                     "~~S in lambda-lists is obsolete and will not be supported in the future.~%  ~
                    Use the ~S declaration.~" '&special 'special)
               (SETQ SPECIAL-FLAG T UNSPECIAL-FLAG NIL))
              ((EQ VAR '&LOCAL)
               (warn 'obsolete-lambda-list-keyword :obsolete
                     "~~S in lambda-lists is obsolete and will not be supported in the future.~%  ~
                    Use the ~S declaration.~" '&special 'unspecial)
               (SETQ SPECIAL-FLAG NIL UNSPECIAL-FLAG T))
              ;; soon also to be obsolete
              ((EQ VAR '&FUNCTIONAL))
              ((MEMQ VAR LAMBDA-LIST-KEYWORDS)
               (WARN 'BAD-INTERNAL-LAMBDA-KEYWORD :IMPOSSIBLE
                     "~S is not supported in internal lambdas." VAR))
              (T (AND SPECIAL-FLAG (PUSH VAR SPECIAL-VARS))
                 (AND UNSPECIAL-FLAG (PUSH VAR UNSPECIAL-VARS))
                 (COND ((SYMBOLP VAR)
                        (PUSH (LIST VAR (IF QUOTEFLAG `',(CAR ARGS1)
                                          (CAR ARGS1)))
                              PROGVARS))
                       (T
                        (UNLESS (NOT OPTIONAL)
                          (WARN 'BAD-ARGUMENT-LIST :IMPOSSIBLE
                                "The mandatory argument ~S of an internal lambda ~
  was given a default value."
                                (CAR VAR)))
                        (PUSH (LIST (CAR VAR)
                                    (IF ARGS1 (IF QUOTEFLAG `',(CAR ARGS1) (CAR ARGS1))
                                      (CADR VAR)))
                              PROGVARS)))
                 (POP ARGS1))))
      (WHEN KEYNAMES
        (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
        ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
        ;; and check explicitly whether that has been overridden.
        ;; If the initial value is a constant, we can really init it to that.
        ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
        ;; after all keywords are decoded, we bind the intended variable, in sequence.
        ;; However a var that can shadow something (including any special var)
        ;; must always be replaced with a dummy.
        (DO ((KIS KEYINITS (CDR KIS))
             (KNS KEYNAMES (CDR KNS))
             (PKNS PSEUDO-KEYNAMES (CDR PKNS))
             (KFS KEYFLAGS (CDR KFS)))
            ((NULL KNS))
          (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
                (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
            (OR (AND (NULL KEYFLAG)
                     (CONSTANTP KEYINIT)
                     (NOT (find KEYNAME *VARS* :key #'var-name))
                     (NOT (LEXICAL-VAR-P KEYNAME))
                     (NOT (SPECIALP KEYNAME)))
                (PROGN (SETF (CAR KIS) 'SI::KEYWORD-GARBAGE)
                       (SETQ PSEUDO-KEYNAME (gensymbol keyname))
                       (SETF (CAR PKNS) PSEUDO-KEYNAME)
                       (PUSH `(,KEYNAME
                               (COND ((EQ ,PSEUDO-KEYNAME SI::KEYWORD-GARBAGE)
                                      ,KEYINIT)
                                     (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
                                        ,PSEUDO-KEYNAME)))
                             KEYCHECKS)))))
        (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
        (SETQ KEYCHECKS (NREVERSE KEYCHECKS))

        ;; BORDER-VARIABLE is a local we put in the binding list
        ;; as the easiest way of being able to get a locative to the
        ;; slot before the first of our keyword arg locals.
        (SETQ BORDER-VARIABLE (gensymbol "KEYWORDS-LOC"))
        (SETQ BODY
              `((LET* (,BORDER-VARIABLE
                       ,@(MAPCAR (LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
                       ,@KEYFLAGS)
                  (DECLARE (IGNORE ,BORDER-VARIABLE))
             ,(case *target-computer*
                (k (generate-k-keyword-args-decode pseudo-keynames rest-arg keykeys allow-other-keys))
                (otherwise
                 `(WHEN ,REST-ARG
                    (SI::STORE-KEYWORD-ARG-VALUES-INTERNAL-LAMBDA
                      (KLUDGEY-COMPILATION-VARIABLE-LOCATION ,BORDER-VARIABLE)
                      ,REST-ARG ',KEYKEYS
                      ,ALLOW-OTHER-KEYS))))
                  (LET* ,KEYCHECKS
                    . ,BODY)))))
      ;; Take all DECLAREs off the body and put them on DECLS.
      (MULTIPLE-VALUE-BIND (BODY DECLS)
          (with-list (env *function-environment*)
            (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL NIL env))
        (WHEN SPECIAL-VARS
          (PUSH `(SPECIAL . ,SPECIAL-VARS) DECLS))
        (WHEN UNSPECIAL-VARS
          (PUSH `(UNSPECIAL . ,UNSPECIAL-VARS) DECLS))
        (WHEN DECLS
          (PUSH `(DECLARE . ,DECLS) BODY))
        (P1 `(LET-FOR-LAMBDA ,(NRECONC PROGVARS (IF ARGS1 `((IGNORE (PROGN . ,ARGS1)))))
                             . ,BODY))))))

;; Yes, *throw, NOT throw
(defun (:property *throw p1) (x)
  (case (list-length (cdr x))
    (0 (warn 'wrong-number-of-arguments :impossible "~S called with no arguments" 'throw)
       nil)
    (1 (warn 'wrong-number-of-arguments :implausible
             "~S called with no value-form -- assuming ~S"
             '(throw (values)))
       `(*throw ,(p1v (cadr x) 1) ,(p1 `(values))))
    (2 `(*throw ,(p1v (cadr x) 1) ,(p1 (caddr x))))
    (t (let ((*print-length* 5) (*print-level* 3))
         (warn 'wrong-number-of-arguments :obsolete
               "~S called with multiple value value arguments~%   ~
                 --- use ~S instead."
               'throw `(throw ,(cadr x) (values . ,(cddr x)))))
       `(*throw ,(p1v (cadr x) 1) ,(p1 `(values . ,(cddr x)))))))

(defun (:property *catch p1) (form)
  ;;; Make *CATCH compile arguments other than the first and the last for
  ;;; effect rather than for value.
  (case (list-length (cdr form))
    (0 (warn 'wrong-number-of-arguments :impossible "~S called with no arguments" 'catch)
       nil)
    (1 (warn 'wrong-number-of-arguments :impossible "~S called with no body forms" 'catch)
       nil)
    (t `(*catch ,(p1v (cadr form) 1) (progn . ,(p1progn-1 (cddr form)))))))

(DEFUN (:PROPERTY COND P1) (X)
  (COND ((NULL (CDR X))
         ''NIL)
        ((ATOM (CDR X))
         (WARN 'BAD-COND :IMPOSSIBLE
               "The atom ~S appears as the body of a ~S." (CDR X) 'COND)
         ''NIL)
        ((null (cddr x))
         ;; just one clause
         (let ((clause (cadr x)))
           (cond ((atom clause)
                  (warn 'bad-cond :impossible
                        "The atom ~S appears as a clause of a ~S." (CDR X) 'COND))
                 ((= (length clause) 1)
                  (p1 (car clause)))
                 (t
                  (p1 `(and ,(car clause)
                            (progn . ,(cdr clause))))))))
        (T `(COND . ,(MAPCAR (LAMBDA (CLAUSE)
                               (COND ((ATOM CLAUSE)
                                      (WARN 'BAD-COND :IMPOSSIBLE
                                            "The atom ~S appears as a ~S-clause."
                                            CLAUSE 'COND)
                                      NIL)
                                     (T (P1COND-CLAUSE CLAUSE))))
                             (CDR X))))))

(DEFUN (:PROPERTY IF P1) (FORM)
  `(COND ,(P1COND-CLAUSE `(,(CADR FORM) ,(CADDR FORM)))
         ,(P1COND-CLAUSE `(T NIL . ,(CDDDR FORM)))))

(DEFUN P1COND-CLAUSE (CLAUSE)
  (IF (CDR CLAUSE)
      `(,(P1V (CAR CLAUSE) 'PREDICATE) . ,(P1PROGN-1 (CDR CLAUSE)))
      `(,(P1 (CAR CLAUSE)))))

(DEFPROP PROGN P1PROGN P1)
(DEFPROP LOCALLY P1PROGN P1)    ;since our PROGN can gobble declarations, LOCALLY is like it.

(DEFUN P1PROGN (FORM)
  (if *tlevel* (clear-tlevel))
  (MULTIPLE-VALUE-BIND (BODY THIS-FRAME-DECLARATIONS)
      (with-list (env *function-environment*)
        (EXTRACT-DECLARATIONS-RECORD-MACROS (CDR FORM) NIL NIL env))
    (LET ((*VARS* *VARS*)
          (LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
      (PROCESS-SPECIAL-DECLARATIONS THIS-FRAME-DECLARATIONS)
      (SETQ LOCAL-DECLARATIONS (NCONC THIS-FRAME-DECLARATIONS LOCAL-DECLARATIONS))
      `(PROGN-WITH-DECLARATIONS ,*VARS* . ,(P1PROGN-1 BODY)))))

(DEFUN P1PROGN-1 (FORMS)
  (let ((copy (copy-list forms)))
    (do ((tail copy (cdr tail)))
        ((null tail) copy)
      (setf (car tail) (p1v (car tail) (if (cdr tail) nil *p1value*))))))

(DEFUN (:PROPERTY CASEN P1) (FORMS)
  `(CASEN
     ,(P1V (CADR FORMS) 1)
     ,@(MAPCAR #'(LAMBDA (FORM) (P1 FORM *P1VALUE*)) (CDDR FORMS))))

(DEFUN (:PROPERTY IGNORE P1) (FORM)
  `(progn
     ,@(mapcar (lambda (elt) (p1v elt nil)) (cdr form))
     'nil))

(DEFUN (:PROPERTY THE P1) (FORM)
  `(THE ,(CADR FORM) ,(P1 (CADDR FORM))))

(defun (:property values p1) (form)
  (typecase *p1value*
    (number
     `(values . ,(loop for elt in (cdr form)
                       for n from 0
                    collect (p1v elt (if (< n *p1value*) 1 nil)))))
    ((member nil predicate)
     `(values ,(p1v (cadr form) *p1value*)
              . ,(mapcar (lambda (elt) (p1v elt nil)) (cddr form))))
    (t
     `(values . ,(mapcar (lambda (elt) (p1v elt 1)) (cdr form))))))

(DEFPROP MULTIPLE-VALUE P1-MULTIPLE-VALUE P1)
(DEFUN P1-MULTIPLE-VALUE (FORM)
  (IF (NULL (CDR (CADR FORM)))                  ;(multiple-value-setq (foo) (bar))
      (IF (EQ (CAR (CADR FORM)) 'NIL)           ;(multiple-value-setq (nil) (bar))
          (P1V (CADDR FORM) NIL)
        (P1V `(SETQ ,(CAR (CADR FORM)) ,(CADDR FORM)) 1))
    `(MULTIPLE-VALUE ,(MAPCAR #'P1SETVAR (CADR FORM))
       ,(P1V (CADDR FORM) (LENGTH (CADR FORM))))))

(defun (:property nth-value p1) (form)
  (cond ((null *p1value*)
         (p1 `(progn ,(cadr form) ,(caddr form))))
        ((typep (cadr form) '(integer 0 63.))
         `(nth-value ,(cadr form) ,(p1v (caddr form) (1+ (cadr form)))))
        (t
         (let ((n (p1v (cadr form) 1)))
           (if (and (eq (car-safe n) 'quote)
                    (typep (cadr n) '(integer 0 63.)))
               (if (eql (cadr n) 0)
                   (p1v (caddr form) *p1value*)
                 `(nth-value ,(cadr n) ,(p1v (caddr form) (1+ (cadr n)))))
             `(nth ,n ,(p1v `(multiple-value-list ,(caddr form)) (if *p1value* t))))))))


(DEFPROP MULTIPLE-VALUE-LIST ((1 (FEF-ARG-REQ FEF-QT-EVAL))) ARGDESC)

;;; In pass 1, pretend this isn't a special form
(DEFUN (:PROPERTY MULTIPLE-VALUE-PUSH P1) (FORM)
  (AND (CDDDR FORM)
       (WARN 'WRONG-NUMBER-OF-ARGUMENTS :IMPOSSIBLE
             "~S is used with too many arguments." 'MULTIPLE-VALUE-PUSH))
  (COND ((ZEROP (CADR FORM))
         ;; NO, it is not correct to throw away a form just because
         ;; zero of its values are wanted!!!!
         (P1V (CADDR FORM) NIL))
        ((TYPEP (CADR FORM) '(INTEGER 1 63.))
         `(MULTIPLE-VALUE-PUSH ,(CADR FORM) ,(P1V (CADDR FORM) (CADR FORM))))
        (T
         (WARN 'TOO-MANY-VALUES :IMPOSSIBLE
               "The first argument of ~S must be a fixnum between 0 and 63."
               'MULTIPLE-VALUE-PUSH)
         (P1 (CADDR FORM)))))

(defun (:property prog1 p1) (form)
  (let ((value-to-return
          (p1v (cadr form) (if (typep *p1value* '(or number (member t)))
                               1 *p1value*))))
    (if (cddr form)
        `(prog2 nil
                ,value-to-return
                . ,(mapcar (lambda (elt) (p1v elt nil)) (cddr form)))
      value-to-return)))

(defun (:property multiple-value-prog1 p1) (form)
  (if (cddr form)
      `(multiple-value-prog1 ,(p1v (cadr form) *p1value*)
                             . ,(mapcar (lambda (elt) (p1v elt nil)) (cddr form)))
    (p1 (cadr form))))

(defun (:property unwind-protect p1) (form)
  (cond ((null (cdr form))                      ;nargs checker barfs for us.
         nil)
        ((null (cadr form))
         (warn 'foo :implausible "The body of an ~S is empty" 'unwind-protect)
         (p1 `(progn . ,(cddr form))))
        ((cddr form)
         `(unwind-protect ,(p1v (cadr form) *p1value*)
                          . ,(mapcar (lambda (elt) (p1v elt nil)) (cddr form))))
        (t
         (warn 'foo :implausible "No cleanup-forms for ~S form" 'unwind-protect)
         (p1 (cadr form)))))

(DEFUN (:PROPERTY SETQ P1) (FORM)
  (LET ((DO-THE-SETS
          `(SETQ . ,(P1SETQ-1 (CDR FORM)))))
    (IF *P1VALUE*
        ;; If the last pair is of the form X X and was ignored,
        ;; but we need X as the value of the SETQ form,
        ;; put (PROGN ... X) around the actual variable setting.
        (DO ((TAIL (CDR FORM) (CDDR TAIL)))
            ((NULL TAIL)
             DO-THE-SETS)
          (AND (CDR TAIL) (NULL (CDDR TAIL))
               (EQ (CAR TAIL) (CADR TAIL))
               (RETURN `(PROGN ,DO-THE-SETS ,(P1V (CAR TAIL) 1)))))
      DO-THE-SETS)))

(DEFUN P1SETQ-1 (PAIRS)
  (COND ((NULL PAIRS) NIL)
        ((NULL (CDR PAIRS))
         (WARN 'BAD-SETQ :IMPOSSIBLE
               "~S appears with an odd number of arguments; the last one is ~S."
               'SETQ (CAR (LAST PAIRS)))
         NIL)
        ((MEMQ (CAR PAIRS) '(T NIL))
         (WARN 'NIL-OR-T-SET :IMPOSSIBLE
               "~S being ~S'd; this will be ignored." (CAR PAIRS) 'SETQ)
         (P1V (CADR PAIRS) 1)                   ;Just to get warnings on it.
         (P1SETQ-1 (CDDR PAIRS)))
        ((GET (CAR PAIRS) 'SYSTEM-CONSTANT)
         (WARN 'SYSTEM-CONSTANT-SET :IMPOSSIBLE
               "Defined constant ~S being ~S'd; this will be ignored." (CAR PAIRS) 'SETQ)
         (P1V (CADR PAIRS) 1)
         (P1SETQ-1 (CDDR PAIRS)))
        ((EQ (CAR PAIRS) (CADR PAIRS))
         ;; Generate no code for (SETQ X X) unless returned value.
         (P1SETVAR (CAR PAIRS))
         (P1SETQ-1 (CDDR PAIRS)))
        (T
         (CONS (P1SETVAR (CAR PAIRS))
               (CONS (P1V (CADR PAIRS) 1) (P1SETQ-1 (CDDR PAIRS)))))))

(DEFUN P1SETVAR (VAR)
  (COND ((NULL VAR) NIL)                        ;For multiple-value-setq
        ((NOT (SYMBOLP VAR))
         (WARN 'BAD-SETQ :IMPOSSIBLE
               "~S cannot be ~S'd." VAR 'SETQ)
         NIL)
        (T (P1 VAR))))

;;; Given an entry on *VARS*, increment the usage count.
(DEFSUBST VAR-INCREMENT-USE-COUNT (VAR)
  (INCF (VAR-USE-COUNT VAR)))

;;; COMPILER-LET must be renamed to COMPILER-LET-INTERNAL
;;; by an "optimizer" so that its normal definition as a macro is bypassed.
(DEFREWRITE COMPILER-LET-INTERNALIZE COMPILER-LET (FORM)
  `(COMPILER-LET-INTERNAL . ,(CDR FORM)))

;;; (compiler-let ((var form) (var form) ...) body...)
(DEFUN (:PROPERTY COMPILER-LET-INTERNAL P1) (FORM)
  (PROGV (MAPCAR (LAMBDA (X) (IF (ATOM X) X (CAR X))) (CADR FORM))
         (MAPCAR (LAMBDA (X) (IF (ATOM X) NIL (EVAL (CADR X)))) (CADR FORM))
    (P1 (IF (CDDDR FORM) `(PROGN . ,(CDDR FORM)) (CADDR FORM)))))

;;; DONT-OPTIMIZE is like PROGN, except that the arguments are not optimized.
;;; Actually, only the top level of the arguments are not optimized;
;;; their subexpressions are handled normally.
(DEFUN (:PROPERTY DONT-OPTIMIZE P1) (FORM &REST (FORMS (CDR FORM)))
  (DO ((FORMS-LEFT (SETQ FORMS (COPY-LIST FORMS)) (CDR FORMS-LEFT)))
      ((NULL FORMS-LEFT) `(PROGN . ,FORMS))
    (SETF (CAR FORMS-LEFT)
          (P1V (CAR FORMS-LEFT) (IF (CDR FORMS) NIL *P1VALUE*) T))))

(DEFUN (:PROPERTY INHIBIT-STYLE-WARNINGS P1) (FORM &REST (FORMS (CDR FORM)))
  (DO ((FORMS-LEFT (SETQ FORMS (COPY-LIST FORMS)) (CDR FORMS-LEFT))
       (*CHECK-STYLE-P* NIL))
      ((NULL FORMS-LEFT) `(PROGN . ,FORMS))
    (SETF (CAR FORMS-LEFT)
          (P1V (CAR FORMS-LEFT) (IF (CDR FORMS) NIL *P1VALUE*)))))

;;; Execute body with SELF's mapping table set up.
(DEFUN (:PROPERTY WITH-SELF-ACCESSIBLE P1) (FORM)
  (P1 `(LET ((SELF-MAPPING-TABLE (%GET-SELF-MAPPING-TABLE ',(CADR FORM))))
         . ,(CDDR FORM))))

;;; Execute body with all instance variables of SELF bound as specials.
(DEFUN (:PROPERTY WITH-SELF-VARIABLES-BOUND P1) (FORM)
  (P1 `(LET ()
         (%USING-BINDING-INSTANCES (SI::SELF-BINDING-INSTANCES))
         . ,(CDR FORM))))

;;; The flavor system sometimes generates SELF-REFs by hand.
;;; Just let them through on pass 1.  Pass 2 will compile like refs to instance vars.
(DEFUN (:PROPERTY SELF-REF P1) (FORM)
  FORM)

(DEFUN (:PROPERTY LEXPR-FUNCALL-WITH-MAPPING-TABLE P1) (FORM)
  (P1 `(LET ((SELF-MAPPING-TABLE SELF-MAPPING-TABLE))
         (LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL . ,(CDR FORM)))))

(DEFUN (:PROPERTY FUNCALL-WITH-MAPPING-TABLE P1) (FORM)
  (P1 `(LET ((SELF-MAPPING-TABLE SELF-MAPPING-TABLE))
         (FUNCALL-WITH-MAPPING-TABLE-INTERNAL . ,(CDR FORM)))))

;;; Make sure combined methods get marked as being methods.
(DEFUN (:PROPERTY LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL P1) (FORM)
  (SETQ *SELF-REFERENCES-PRESENT* T)
  (P1EVARGS FORM))

(DEFUN (:PROPERTY FUNCALL-WITH-MAPPING-TABLE-INTERNAL P1) (FORM)
  (SETQ *SELF-REFERENCES-PRESENT* T)
  (P1EVARGS FORM))

(DEFUN (:PROPERTY QUOTE-EVAL-AT-LOAD-TIME P1) (FORM)
  (P1 (IF (EQ QC-TF-OUTPUT-MODE 'COMPILE-TO-CORE)
          `(QUOTE ,(EVAL (CADR FORM)))
        `(QUOTE (,EVAL-AT-LOAD-TIME-MARKER . ,(CADR FORM))))))

;;; In the interpreter, this simply evals its arg.
(DEFUN QUOTE-EVAL-AT-LOAD-TIME (FORM) FORM)

;;; By special dispensation, VALUE-CELL-LOCATION of a quoted symbol
;;; gets the location of any sort of variable; but this is semi-obsolete.
;(DEFUN (:PROPERTY VALUE-CELL-LOCATION P1) (FORM)
;  (IF (EQ (CAR-SAFE (CADR FORM)) 'QUOTE)
;      (LET ((VAR (CADADR FORM)) TEM)
;       (COND ((NOT (SYMBOLP VAR))
;              (WARN 'VARIABLE-NOT-SYMBOL :IMPOSSIBLE
;                    "The argument of ~S is '~S." 'VALUE-CELL-LOCATION VAR)
;              ''NIL)
;             ((MEMQ VAR '(T NIL))
;              `(VALUE-CELL-LOCATION ',VAR))
;             (T
;              (SETQ TEM (P1V VAR 1))
;              (COND ((SYMBOLP TEM)
;                     `(VALUE-CELL-LOCATION ',TEM))
;                    (T `(VARIABLE-LOCATION ,TEM))))))
;    (P1EVARGS FORM)))

(DEFUN (:PROPERTY VARIABLE-LOCATION P1) (FORM &AUX TEM)
  (COND ((NOT (SYMBOLP (CADR FORM)))
         (WARN 'VARIABLE-NOT-SYMBOL :IMPOSSIBLE
               "The argument of ~S is ~S, which is not a symbol."
               'VARIABLE-LOCATION (CADR FORM))
         ''NIL)
        (T
         (SETQ TEM (P1V (CADR FORM) 1))
         (COND ((SYMBOLP TEM)
                `(%EXTERNAL-VALUE-CELL ',TEM))
               (T (when (eq *target-computer* 'k)
                    (case (first tem)
                      (local-ref
                       (push 'variable-location (var-misc (second tem))))))
                  `(VARIABLE-LOCATION ,TEM))))))

(DEFUN (VARIABLE-MAKUNBOUND P1) (FORM &AUX TEM)
  (IF (COND ((NOT (SYMBOLP (CADR FORM)))
             (WARN 'VARIABLE-NOT-SYMBOL :IMPOSSIBLE
                   "The argument of ~S is ~S, which is not a symbol."
                   'VARIABLE-MAKUNBOUND (CADR FORM))
             NIL)
            (T (SETQ TEM (P1V (CADR FORM) 1))
               (COND ((SYMBOLP TEM))
                     ((EQ (CAR TEM) 'SELF-REF))
                     ((EQ (CAR TEM) 'LOCAL-REF)
                      (WARN 'VARIABLE-LOCAL :IMPOSSIBLE
                            "~S is not allowed on local variables such as ~S"
                            'VARIABLE-MAKUNBOUND (CADR FORM))
                      NIL)
                     ((EQ (CAR TEM) 'LEXICAL-REF)
                      (WARN 'VARIABLE-LOCAL :IMPOSSIBLE
                            "~S is not allowed on lexical variables such as ~S"
                            'VARIABLE-MAKUNBOUND (CADR FORM))
                      NIL))))
      (P1V `(LOCATION-MAKUNBOUND (VARIABLE-LOCATION ,(CADR FORM))) 1)
    ''NIL))

(DEFUN (:PROPERTY VARIABLE-BOUNDP P1) (FORM &AUX TEM)
  (COND ((NOT (SYMBOLP (CADR FORM)))
         (WARN 'VARIABLE-NOT-SYMBOL :IMPOSSIBLE
               "The argument of ~S is ~S, which is not a symbol."
               'VARIABLE-BOUNDP (CADR FORM))
         ''NIL)
        (T (SETQ TEM (P1V (CADR FORM) 1))
           (COND ((SYMBOLP TEM)
                  `(BOUNDP ',TEM))
                 ;; slightly bogoid...
                 ((EQ (CAR TEM) 'LOCAL-REF) ''T)
                 ((EQ (CAR TEM) 'SELF-REF)
                  (P1V `(NOT (= DTP-NULL (%P-DATA-TYPE (VARIABLE-LOCATION ,(CADR FORM)))))
                       'PREDICATE))
                 ((EQ (CAR TEM) 'LEXICAL-REF) ''T)))))

;;; BOUNDP of an instance variable of SELF does special things.
;(DEFUN (:PROPERTY BOUNDP P1) (FORM)
;  (COND ((EQ (CAR-SAFE (CADR FORM)) 'QUOTE)
;        (P1V `(VARIABLE-BOUNDP ,(CADADR FORM)) 'PREDICATE))
;       (T (P1EVARGS FORM))))

(DEFPROP OR P1ANDOR P1)
(DEFPROP AND P1ANDOR P1)
(DEFUN P1ANDOR (FORM)
  `(,(CAR FORM) . ,(DO ((X (CDR FORM) (CDR X))
                        RESULT)
                       ((NULL (CDR X))
                        (PUSH (P1 (CAR X)) RESULT)
                        (NREVERSE RESULT))
                     (PUSH (P1V (CAR X) 'PREDICATE) RESULT))))

(DEFUN P1EVARGS (FORM)
  "One value from each elt of (CDR FORM), including the last."
  (LET ((*P1VALUE* 1))
    `(,(CAR FORM) . ,(MAPCAR #'P1 (CDR FORM)))))

;;; Any use of BIND must set *SPECIALFLAG*.
(DEFPROP %BIND P1BIND P1)
(DEFPROP %USING-BINDING-INSTANCES P1BIND P1)
(DEFUN P1BIND (FORM)
  (SETQ *SPECIALFLAG* T)
  (SETQ *BINDP* T)
  (P1EVARGS FORM))
(MAKE-OBSOLETE BIND "use SYS:%BIND")

;;; For (CLOSURE '(X Y Z) ...), make sure that X, Y, Z are special.
(DEFUN (:PROPERTY P1CLOSURE P1) (FORM)
  (AND (EQ (CAR-SAFE (CADR FORM)) 'QUOTE)
       (MAPC #'MSPL2 (CADADR FORM)))
  (P1EVARGS FORM))

;(DEFUN (:PROPERTY EVENP P1) (FORM)
;  (P1 `(NOT (BIT-TEST 1 ,(CADR FORM)))))

;(DEFUN (:PROPERTY ODDP P1) (FORM)
;  (P1 `(BIT-TEST 1 ,(CADR FORM))))

;(DEFPROP ARRAY ((2 (FEF-ARG-REQ FEF-QT-QT)) (#o20 (FEF-ARG-OPT FEF-QT-EVAL))) ARGDESC)

;;;Here is some stuff to do "dynamic" MISC instructions.  The incremental micro-assembler uses it,
;;; and later the micro-compiler will too.

(defun assign-placeholder (name)
  (when (null (assq name *placeholder-alist*))
    (let ((real-misc (intern-soft (format nil "PLACEHOLDER-~D" *placeholder-function-number*)
                                  'COMPILER)))
      (when (or (null real-misc)
                (null (get real-misc 'qlval)))
        (barf () "out of PLACEHOLDERs"))
      (push (list name
                  *placeholder-function-number*
                  real-misc
                  (get name 'new-micro-n-args))
            *placeholder-alist*)
      (incf *placeholder-function-number*))))

(defun placeholder-p1 (form)
  (assign-placeholder (car form))
  (p1argc form (getargdesc (CAR FORM))))

(defun placeholder-p2 (argl dest)
  (let ((info (assq *p2fn* *placeholder-alist*)))
    (when (null info)
      (barf *p2fn* "no info for function ~S"))
    (when (not (= (length argl) (cadddr info)))
      (barf *p2fn* "I can't even think about compiling a call to
a dynamic MISC with the wrong number of arguments."))
    (P2MISC (caddr info) argl dest (cadddr info))))
