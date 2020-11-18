;;; -*- Mode:LISP; Package:COMPILER; Base:10; Readtable:CL -*-

;; ||| New file -- smh 29sep88

;; This holds macro definitions for cross compiling for the Falcon.
;; It is supposed to be cross compiled and the FDEF file the
;; loaded in as part of the cross compiler.  The actual compiled file
;; might or might not want eventually to be loaded on the Falcon.

(defmacro PUSHNEW (item place &rest testandkey)
  `(setf ,place (adjoin ,item ,place . ,testandkey)))

(inherit-lambda-macro-definitions
  inherit-lambda-macro-definitions
  si::XR-BQ-CONS si::XR-BQ-LIST si::XR-BQ-LIST* si::XR-BQ-APPEND si::XR-BQ-NCONC
  si::XR-BQ-VECTOR si::XR-BQ-VECTOR*
  )

(inherit-lambda-macro-definitions
  defvar
  defmacro defdecl
  defsetf define-setf-method deflocf
  setf locf
  defsubst si::defsubst-with-parent
  inhibit-style-warnings
  )

(inherit-lambda-macro-definitions
  when unless
  dolist dotimes
  psetq
  with-list with-list*)

(inherit-lambda-macro-definitions
  incf decf
  )

(defmacro locf (accessor &environment environment)
  "Return a locative pointer to the place where ACCESSOR's value is stored.
Note that (LOCF (CDR SOMETHING)) is normally equivalent to SOMETHING,
which may be a list rather than a locative."
  (loop
    (let (fcn)
      (cond ((symbolp accessor)                 ;Special case needed.
             (return `(variable-location ,accessor)))
            ((not (symbolp (car accessor)))
             (ferror "~S non-symbolic function in ~S" (car accessor) 'locf))
            ;;>> This is OK for now, since environment only includes the lexical stuff
            ;;>>  around the current function.  However, when environment includes stuff
            ;;>>  for a whole compilation or set of compilations, we will have to getdecl
            ;;>>  again (being careful not to getdecl for lexically defined functions!)
            ((unless (fsymeval-in-environment (car accessor) environment nil)
               (cond ((eq (getdecl (car accessor) 'locf) 'si::unlocfable)
                      (nolocf accessor))
                     ((setq fcn (getdecl (car accessor) 'si::locf-method))
                      (if (symbolp fcn)
                          (return (cons fcn (cdr accessor)))
                        (progn (if (eq (cdr fcn) 'si::nolocf)
                                   (nolocf accessor))
                               (return (call (cdr fcn) nil accessor :optional environment)))))
                     ((setq fcn (getdecl (car accessor) 'si::setf-expand))
                      (setq accessor (funcall fcn accessor)))
                     ((and (fboundp (car accessor)) (arrayp (symbol-function (car accessor))))
                      ;; +++ not yet supported in runtime
                      (return `(si::aloc #',(car accessor) . ,(cdr accessor))))
                     ((and (fboundp (car accessor)) (symbolp (symbol-function (car accessor))))
                      (return `(locf (,(symbol-function (car accessor)) . ,(cdr accessor))))))))
            ((not (eq accessor (setq accessor (macroexpand-1 accessor environment)))))
            (t (ferror 'sys:unknown-locf-reference
                       "No way known to do LOCF on ~S." (car accessor)))))))

;;; ||| Added this definition to the K macros as a temporary measure
;;; ||| till we can add it to the regular lambda system. JIM 10/19/88
;;; Make a variable special and, optionally, initialize it.
;;; This is recorded as a definition by ZWEI.
(DEFMACRO DEFVAR (SYMBOL &OPTIONAL INITIAL-VALUE DOCUMENTATION)
  "Define a special variable named VARIABLE, and initialize to INITIAL-VALUE if unbound.
Normally, reevaluating the DEFVAR does not change the variable's value.
But in patch files, and if you do C-Shift-E with no region on a DEFVAR,
the variable is reinitialized.  DOCUMENTATION is available if the user
asks for the documentation of the symbol VARIABLE.
If you want your variable to be initially unbound, yet have documentation,
use :UNBOUND as the initial value."
  (DECLARE (ARGLIST SYMBOL &OPTIONAL INITIAL-VALUE DOCUMENTATION))
  (unless (symbolp symbol)
    (error "DEFVAR first subform not a symbol: ~s" symbol))
  `(PROGN (EVAL-WHEN (COMPILE)
            (PROCLAIM '(SPECIAL ,SYMBOL)))
          (EVAL-WHEN (LOAD EVAL)
            (LET* ((STRIPPED-SYMBOL ',SYMBOL)
                   ;; ||| Removed extra paren in above line JIM 10/20/88
                   (init-it ',INITIAL-VALUE)
                   (doc ',documentation))
              (WHEN (RECORD-SOURCE-FILE-NAME STRIPPED-SYMBOL 'DEFVAR)
                (SETF (GET STRIPPED-SYMBOL 'SPECIAL) (OR FDEFINE-FILE-PATHNAME T))
                (AND (NEQ INIT-IT :UNBOUND)
                     (OR FS:THIS-IS-A-PATCH-FILE (NOT (BOUNDP STRIPPED-SYMBOL)))
                     (SET STRIPPED-SYMBOL (EVAL INIT-IT)))
                (WHEN (OR DOC (DOCUMENTATION STRIPPED-SYMBOL 'VARIABLE))
                  (SETF (DOCUMENTATION STRIPPED-SYMBOL 'VARIABLE) DOC)))
              STRIPPED-SYMBOL))))
