;-*- Mode:LISP; Package:COMPILER; Readtable:ZL; Lowercase:T; Base:10 -*-

;;; Code walker for the compiler. Written by RMS.
;;; Find out all variables referenced free by a piece of code,
;;; all lexical functions that it uses free,
;;; all BLOCK names the code tries to return to but doesn't catch,
;;; all GO tags the code tries to GO to but doesn't define.

(defvar *all-block-names*)
(defvar *all-go-tags*)
(defvar *all-functions-to-check-for*)
(defvar *all-functions*)
(defvar *all-variables-to-check-for*)
(defvar *all-variables*)

(defvar *cw-function-environment*)
(defvar *cw-return-expansion-flag*)

(defun macroexpand-all (form &optional environment)
  "Expand macro calls at all levels in FORM, and return the result.
ENVIRONMENT specifies which local MACROLET macro definitions are in effect.
It is like the second argument to MACROEXPAND."
  (nth-value 4 (cw-top-level form nil nil (car environment) t)))

(defun cw-top-level (exp &optional variables-to-check-for functions-to-check-for
                     function-environment return-expansion-p)
  "Return a list of free variables, block names and go tags used by expression EXP.
FUNCTION-ENVIRONMENT has the same format as SI::*INTERPRETER-FUNCTION-ENVIRONMENT*.
 It is the CAR of an environment object, as used by either the compiler or interpreter.
 It is used to record local macros available
 and local function definitions that may be shadowing global macro definitions.
RETURN-EXPANSION-P if non-NIL says expand macros to all levels
 and construct a macro-free form, returned as the fifth value.
The first value lists the free variables,
 (but only symbols present in the argument VARIABLES-TO-CHECK-FOR are mentioned),
the second lists function symbols used free
 (but only symbols present in the argument FUNCTIONS-TO-CHECK-FOR are mentioned),
the third value lists the free block names (including possibly NIL),
the fourth lists the free go tags.
the fifth is the macroexpanded form, but only if RETURN-EXPANSION-P is non-NIL.
VARIABLES-TO-CHECK-FOR may also be T, meaning return all variables used free."
  (declare (values variables functions block-names go-tags macroexpanded-form))
  (let ((*all-variables-to-check-for* variables-to-check-for)
        (*all-functions-to-check-for* functions-to-check-for)
        (*cw-function-environment* function-environment)
        (*cw-return-expansion-flag* return-expansion-p)
        (*all-variables* nil)
        (*all-functions* nil)
        (*all-block-names* nil)
        (*all-go-tags* nil)
        exp-value)
    (setq exp-value (cw-expression exp))
    (values *all-variables* *all-functions* *all-block-names* *all-go-tags*
            exp-value)))

(defun cw-top-level-lambda-expression (exp &optional variables-to-check-for
                                                     functions-to-check-for
                                                     function-environment
                                                     return-expansion-p)
  "Return a list of free variables, block names and go tags used by lambda-expression EXP.
FUNCTION-ENVIRONMENT has the same format as SI::*INTERPRETER-FUNCTION-ENVIRONMENT*.
 It is used to record local macros available
 and local function definitions that may be shadowing global macro definitions.
RETURN-EXPANSION-P if non-NIL says expand macros to all levels
 and construct a macro-free form, returned as the fifth value.
The first value lists the free variables,
 (but only symbols present in the argument VARIABLES-TO-CHECK-FOR are mentioned),
the second lists function symbols used free
 (but only symbols present in the argument FUNCTIONS-TO-CHECK-FOR are mentioned),
the third value lists the free block names (including possibly NIL),
the fourth lists the free go tags.
the fifth is the macroexpanded form, but only if RETURN-EXPANSION-P is non-NIL.
VARIABLES-TO-CHECK-FOR may also be T, meaning return all variables used free."
  (declare (values variables functions block-names go-tags macroexpanded-lambda-exp))
  (let ((*all-variables-to-check-for* variables-to-check-for)
        (*all-functions-to-check-for* functions-to-check-for)
        (*cw-function-environment* function-environment)
        (*cw-return-expansion-flag* return-expansion-p)
        (*all-variables* nil)
        (*all-functions* nil)
        (*all-block-names* nil)
        (*all-go-tags* nil)
        exp-value)
    (setq exp-value (cw-lambda-expression exp))
    (values *all-variables* *all-functions* *all-block-names* *all-go-tags* exp-value)))

(defsubst cw-clause (clause)
  (funcall (if *cw-return-expansion-flag* #'mapcar #'mapc)
           #'cw-expression clause))

(defsubst cw-eval-args (exp)
  exp
  (if *cw-return-expansion-flag*
      `(,(car exp) . ,(mapcar #'cw-expression (cdr exp)))
      (mapc #'cw-expression (cdr exp))))

(defsubst cw-first-arg-quoted (exp)
  (if *cw-return-expansion-flag*
      `(,(car exp) ,(cadr exp) . ,(mapcar #'cw-expression (cddr exp)))
      (mapc #'cw-expression (cddr exp))))

(defun cw-expression (exp &aux tem)
  (when (and (consp exp)
             (memq (car exp) *all-functions-to-check-for*))
    (pushnew (car exp) *all-functions* :test 'eq))
  (cond ((symbolp exp)
         (when (or (eq *all-variables-to-check-for* t)
                   (memq exp *all-variables-to-check-for*))
           (pushnew exp *all-variables* :test 'eq))
         exp)
        ((atom exp) exp)
        ((consp (car exp))
         ;; Explicit lambda-expression
         (if *cw-return-expansion-flag*
             `(,(cw-lambda-expression (car exp)) . ,(mapcar #'cw-expression (cdr exp)))
           (cw-lambda-expression (car exp))
           (mapc #'cw-expression (cdr exp))))
        ((nsymbolp (car exp))
         (cw-eval-args exp))
        ((setq tem (fsymeval-in-function-environment (car exp) *cw-function-environment*))
         (if (eq (car-safe tem) 'macro)
             ;; Local definition is a macro.  Call its expander.
             (with-stack-list (si:*macroexpand-environment* *cw-function-environment*)
               ;; Was binding *CW-FUNCTION-ENVIRONMENT* to NIL, forcing walk in top level
               ;; environment, trying to avoid a non-existent environment clash.
               ;; -- RpK 28-Jan-87 15:00:32
               (cw-expression (funcall (cdr tem) exp si:*macroexpand-environment*)))
           ;; Local definition is not a macro.  Assume it evals its args.
           (cw-eval-args exp)))
        ((setq tem (get (car exp) 'cw-handler))
         ;; special form with its own way of doing this.
         (funcall tem exp))
        ;; Hack &QUOTE.
        ((SI:INTERPRETER-SPECIAL-FORM (CAR EXP))
         (SETQ TEM (ARGLIST (CAR EXP)))
         (let ((quoted))
           (flet ((frob (arg) (do ((x (pop tem) (pop tem)))
                                  ((not (memq x lambda-list-keywords))
                                   (if quoted arg (cw-expression arg)))
                                (cond ((eq x '&quote) (setq quoted t))
                                      ((eq x '&eval) (setq quoted nil))))))
             (if *cw-return-expansion-flag*
                 `(,(car exp) . ,(mapcar #'frob (cdr exp)))
                 (mapc #'frob (cdr exp))))))
        ((multiple-value-bind (v1 v2)
             (with-stack-list (env *cw-function-environment*)
               (macroexpand-1 exp env))
           (setq tem v1)
           v2)
         ;; Macro call.
         (cw-expression tem))
        (t
         (cw-eval-args exp))))

(defun cw-lambda-expression (exp)
  (cond ((memq (car exp) '(lambda subst cli:subst))
         (let (body-expansion)
           (multiple-value-bind (bound expansion)
               (cw-serial-binding-list (cadr exp) t)
             (setq *all-variables*
                   (cl:nunion *all-variables*
                              (let (*all-variables*)
                                (setq body-expansion (cw-clause (cddr exp)))
                                (dolist (bvar bound)
                                  (setq *all-variables* (delq bvar *all-variables*)))
                                *all-variables*)
                              :test #'eq))
             (if *cw-return-expansion-flag*
                 `(,(car exp) ,expansion . ,body-expansion)))))
        ((memq (car exp) '(named-lambda named-subst))
         (let (body-expansion)
           (multiple-value-bind (bound expansion)
               (cw-serial-binding-list (caddr exp) t)
             (setq *all-variables*
                   (cl:nunion *all-variables*
                              (let (*all-variables*)
                                (setq body-expansion (cw-clause (cdddr exp)))
                                (dolist (bvar bound)
                                  (setq *all-variables* (delq bvar *all-variables*)))
                                *all-variables*)
                              :test #'eq))
             (if *cw-return-expansion-flag*
                 `(,(car exp) ,(cadr exp) ,expansion . ,body-expansion)))))
        ((lambda-macro-call-p exp)
         (cw-lambda-expression
           (lambda-macro-expand exp)))
        (t
         ;; This is something invalid which will get a warning later.
         (cw-expression exp))))

(defun cw-serial-binding-list (bindlist &optional lambda-flag
                               &aux free-inside bound)
  "Return a list of variables bound by BINDLIST, while recording any variables it uses free.
This is for serial binding such as is found in LAMBDAs and PROG*'s.
LAMBDA-FLAG should be T for a LAMBDA arglist, otherwise NIL.
Second value is an expansion of the bindlist, if one is requested."
  (declare (values variables expansion))
  (when (consp bindlist)
    (when *cw-return-expansion-flag*
      (setq bindlist (mapcar #'copy-list bindlist)))
    (dolist (elt bindlist)
      (cond ((and lambda-flag (memq elt lambda-list-keywords)))
            ((or (symbolp elt)
                 (and (consp elt) (null (cdr elt)) (setq elt (car elt))))
             (push elt bound))
            ((atom elt))
            ((consp elt)
             (setq *all-variables*
                   (cl:nunion *all-variables*
                              (let ((*all-variables* ()))
                                (if *cw-return-expansion-flag*
                                    (setf (cadr elt)
                                          (cw-expression (cadr elt)))
                                  (cw-expression (cadr elt)))
                                (dolist (b bound)
                                  (setq *all-variables* (delq b *all-variables*)))
                                *all-variables*)
                              :test #'eq))
             (pushnew (if (consp (car elt)) (cadr (car elt)) (car elt))
                      bound :test #'eq)
             (and lambda-flag
                  (caddr elt)
                  (pushnew (caddr elt) bound :test #'eq))
             (unless lambda-flag
               (setq free-inside
                     (cl:nunion free-inside
                                (let (*all-variables*)
                                  (do ((tail (cddr elt) (cdr tail)))
                                      ((null tail))
                                    (if *cw-return-expansion-flag*
                                        (setf (car tail)
                                              (cw-expression (car tail)))
                                      (cw-expression (car tail))))
                                  *all-variables*)
                                :test #'eq)))))))
  (dolist (b bound)
    (setq free-inside (delq b free-inside)))
  (setq *all-variables* (cl:nunion *all-variables* free-inside :test #'eq))
  (values bound bindlist))

(defun cw-parallel-binding-list (bindlist &aux free-inside bound)
  "Return a list of variables bound by BINDLIST, while recording any variables it uses free.
This is for parallel binding such as is found in PROG and LET."
  (declare (values variables expansion))
  (when (consp bindlist)
    (when *cw-return-expansion-flag*
      (setq bindlist (mapcar #'copy-list bindlist)))
    (dolist (elt bindlist)
      (cond ((or (symbolp elt)
                 (and (consp elt) (null (cdr elt)) (setq elt (car elt))))
             (push elt bound))
            ((atom elt))
            ((consp elt)
             (if *cw-return-expansion-flag*
                 (setf (cadr elt)
                       (cw-expression (cadr elt)))
                 (cw-expression (cadr elt)))
             (pushnew (if (consp (car elt)) (cadr (car elt)) (car elt)) bound :test #'eq)
             (setq free-inside
                   (cl:nunion free-inside
                              (let ((*all-variables* ()))
                                (do ((tail (cddr elt) (cdr tail)))
                                    ((null tail))
                                  (if *cw-return-expansion-flag*
                                      (setf (car tail)
                                            (cw-expression (car tail)))
                                    (cw-expression (car tail))))
                                *all-variables*)
                              :test #'eq))))))
  (dolist (b bound)
    (setq free-inside (delq b free-inside)))
  (setq *all-variables* (cl:nunion *all-variables* free-inside :test #'eq))
  (values bound bindlist))

;;;; Variable-binding constructs which don't contain go tags.

(defprop let cw-let cw-handler)
(defun cw-let (exp)
  (multiple-value-bind (bound bindlist)
      (cw-parallel-binding-list (cadr exp))
    (let (body)
      (setq *all-variables*
            (cl:nunion *all-variables*
                       (let ((*all-variables* ()))
                         (setq body (cw-clause (cddr exp)))
                         (dolist (b bound)
                           (setq *all-variables* (delq b *all-variables*)))
                         *all-variables*)
                       :test #'eq))
      (if *cw-return-expansion-flag*
          `(,(car exp) ,bindlist . ,body)))))


(defun (:property let* cw-handler) (exp)
  (multiple-value-bind (bound bindlist)
      (cw-serial-binding-list (cadr exp))
    (let (body)
      (setq *all-variables*
            (cl:nunion *all-variables*
                       (let ((*all-variables* ()))
                         (setq body (cw-clause (cddr exp)))
                         (dolist (b bound)
                           (setq *all-variables* (delq b *all-variables*)))
                         *all-variables*)
                       :test #'eq))
      (if *cw-return-expansion-flag*
          `(let* ,bindlist . ,body)))))

(defun (:property let-if cw-handler) (exp)
  (let ((cond (cw-expression (cadr exp))))
    (multiple-value-bind (nil bindlist)
        (cw-parallel-binding-list (caddr exp))
      (let ((body (cw-clause (cdddr exp))))
        (if *cw-return-expansion-flag*
            `(let-if ,cond ,bindlist . ,body))))))

(defun (:property multiple-value-bind cw-handler) (exp)
  (let ((mvform
          (cw-expression (caddr exp)))
        body
        (bound (cadr exp)))
    (setq *all-variables*
          (cl:nunion *all-variables*
                     (let ((*all-variables* ()))
                       (setq body (cw-clause (cdddr exp)))
                       (dolist (b bound)
                         (setq *all-variables* (delq b *all-variables*)))
                       *all-variables*)
                     :test #'eq))
    (if *cw-return-expansion-flag*
        `(multiple-value-bind ,bound ,mvform . ,body))))

(defprop with-stack-list cw-with-stack-list cw-handler)
(defprop with-stack-list* cw-with-stack-list cw-handler)

(defun cw-with-stack-list (exp)
  (let ((elements
          (cw-clause (cdadr exp)))
        body)
    (setq *all-variables*
          (cl:nunion *all-variables*
                     (let ((*all-variables* ()))
                       (setq body (cw-clause (cddr exp)))
                       (delq (caadr exp) *all-variables*))
                     :test #'eq))
    (if *cw-return-expansion-flag*
        `(,(car exp) (,(caadr exp) . ,elements) . ,body))))

(defun (:property compiler-let cw-handler) (exp)
  (progw (cadr exp)
    (cw-first-arg-quoted exp)))

;;;; PROG, DO, GO, RETURN, RETURN-FROM, TAGBODY.

(defun (prog cw-handler) (exp)
  (cw-prog-form exp #'cw-parallel-binding-list))

(defun (prog* cw-handler) (exp)
  (cw-prog-form exp #'cw-serial-binding-list))

(defun cw-prog-form (exp binding-list-function)
  (let (varlist body progname block-names vars altered-body)
    (if (and (symbolp (cadr exp)) (cadr exp))
        (setq varlist (third exp) body (cdddr exp) progname (cadr exp))
        (setq varlist (second exp) body (cddr exp)))
    (multiple-value-bind (bound bindlist)
        (funcall binding-list-function varlist)
      (let (*all-variables* *all-block-names*)
        (setq altered-body (cw-prog-body body))
        (dolist (b bound)
          (setq *all-variables* (delq b *all-variables*)))
        (setq vars *all-variables*
              block-names (delq nil (delq progname *all-block-names*))))
      (setq *all-variables* (cl:nunion *all-variables* vars :test #'eq)
            *all-block-names* (cl:nunion *all-block-names* block-names :test #'eq))
      (if *cw-return-expansion-flag*
          (if progname
              `(,(car exp) ,progname ,bindlist . ,altered-body)
              `(,(car exp) ,bindlist . ,altered-body))))))

(defun cw-prog-body (body &aux go-tags altered-body)
  (let* (*all-go-tags*)
    (setq altered-body
          (funcall (if *cw-return-expansion-flag* #'mapcar #'mapc)
                   #'(lambda (statement)
                       (if (atom statement) statement (cw-expression statement)))
                   body))
    (setq go-tags *all-go-tags*))
  (dolist (statement body)
    (if (atom statement)
        (setq go-tags (delete statement go-tags))))
  (setq *all-go-tags* (cl:nunion *all-go-tags* go-tags :test #'eq))
  altered-body)

(defun (:property tagbody cw-handler) (exp)
  `(tagbody . ,(cw-prog-body (cdr exp))))

(defun (:property do cw-handler) (exp)
  (cw-do-form exp #'cw-parallel-binding-list))

(defun (:property do* cw-handler) (exp)
  (cw-do-form exp #'cw-serial-binding-list))

(defun (:property do-named cw-handler) (exp)
  `(do-named . ,(cw-do-form (cdr exp) #'cw-parallel-binding-list (cadr exp))))

(defun (:property do*-named cw-handler) (exp)
  `(do*-named . ,(cw-do-form (cdr exp) #'cw-serial-binding-list (cadr exp))))

(defun cw-do-form (exp binding-list-function &optional progname)
  (if (and (cadr exp) (symbolp (cadr exp)))
      ;; old-style DO
      (let ((var (cadr exp))
            vars block-names
            ival step test altered-body)
        (setq ival (cw-expression (third exp)))  ;initial value expression
        (let ((*all-variables* ())
              (*all-block-names* ()))
          (setq step (cw-expression (fourth exp)))              ;Step expression
          (setq test (cw-expression (fifth exp)))               ;Endtest
          (setq altered-body (cw-prog-body (nthcdr 5 exp)))
          (setq vars (delq var *all-variables*)
                block-names (delq nil *all-block-names*)))
        (setq *all-variables* (cl:nunion *all-variables* vars :test #'eq)
              *all-block-names* (cl:nunion *all-block-names* block-names :test #'eq))
        (if *cw-return-expansion-flag*
            `(,(car exp) ,var ,ival ,step ,test . ,altered-body)))
    (let ((varlist (cadr exp)) (endstuff (caddr exp)) (body (cdddr exp))
          block-names vars altered-body altered-endstuff)
      (multiple-value-bind (bound bindlist)
          (funcall binding-list-function varlist)
        (let (*all-variables* *all-block-names*)
          (setq altered-endstuff (cw-clause endstuff))
          (setq altered-body (cw-prog-body body))
          (setq vars *all-variables*
                block-names (delq nil (delq progname *all-block-names*))))
        (dolist (b bound)
          (setq vars (delq b vars)))
        (setq *all-variables* (cl:nunion *all-variables* vars :test #'eq)
              *all-block-names* (cl:nunion *all-block-names* block-names :test #'eq))
        (if *cw-return-expansion-flag*
            `(,(car exp) ,bindlist ,altered-endstuff . ,altered-body))))))

(defun (:property go cw-handler) (exp)
  (pushnew (cadr exp) *all-go-tags* :test 'eq)
  exp)

(defun (:property return-from cw-handler) (exp)
  (pushnew (cadr exp) *all-block-names* :test 'eq)
  (cw-first-arg-quoted exp))

(defun (:property return cw-handler) (exp)
  (pushnew nil *all-block-names* :test 'eq)
  (cw-eval-args exp))

(defun (:property block cw-handler) (exp &aux value)
  (setq *all-block-names*
        (cl:nunion *all-block-names*
                   (let ((*all-block-names* ()))
                     (setq value (cw-first-arg-quoted exp))
                     (delq (cadr exp) *all-block-names*))
                   :test #'eq))
  value)

(defun (:property function cw-handler) (exp)
  (if (consp (cadr exp))
      (if *cw-return-expansion-flag*
          `(function ,(cw-lambda-expression (cadr exp)))
          (cw-lambda-expression (cadr exp)))
    (when (memq (cadr exp) *all-functions-to-check-for*)
      (pushnew (cadr exp) *all-functions* :test 'eq))
    exp))

(defun (:property cond cw-handler) (exp)
  (if *cw-return-expansion-flag*
      `(cond . ,(mapcar #'cw-clause (cdr exp)))
      (mapc #'cw-clause (cdr exp))))

(defprop if cw-eval-args cw-handler)

(defprop multiple-value cw-multiple-value cw-handler)
(defprop multiple-value-setq cw-multiple-value cw-handler)
(defprop multiple-value-call cw-eval-args cw-handler)
(defprop multiple-value-prog1 cw-eval-args cw-handler)

(defun cw-multiple-value (exp)
  (when (consp (cadr exp))
    (setq *all-variables* (cl:nunion *all-variables* (cadr exp) :test #'eq)))
  (cw-first-arg-quoted exp))

(defprop with-self-accessible cw-first-arg-quoted cw-handler)

(defprop quote-eval-at-load-time identity cw-handler)

(defun cw-quoted-variable-expression (exp)
  (when (or (eq *all-variables-to-check-for* t)
            (memq (cadr exp) *all-variables-to-check-for*))
    (pushnew (cadr exp) *all-variables* :test 'eq))
  exp)
(defprop variable-boundp cw-quoted-variable-expression cw-handler)
(defprop variable-location cw-quoted-variable-expression cw-handler)
(defprop variable-makunbound cw-quoted-variable-expression cw-handler)

(defun cw-explicitly-quoted-variable-expression (exp)
  (if (and (eq (car-safe (cadr exp)) 'quote)
           (or (eq *all-variables-to-check-for* t)
               (memq (cadadr exp) *all-variables-to-check-for*)))
      (progn (pushnew (cadadr exp) *all-variables* :test 'eq)
             exp)
    (cw-eval-args exp)))
(defprop boundp cw-explicitly-quoted-variable-expression cw-handler)
(defprop value-cell-location cw-explicitly-quoted-variable-expression
         cw-handler)


(defun cw-flet-binding-list (bindlist)
  (if *cw-return-expansion-flag*
      (loop for elt in bindlist
            collect (cons (car elt)
                          (cdr (cw-lambda-expression (cons 'lambda (cdr elt))))))
    (dolist (elt bindlist)
      (cw-lambda-expression (cons 'lambda (cdr elt))))))

(defun (:property flet cw-handler) (exp &aux bindlist body)
  (setq bindlist (cw-flet-binding-list (cadr exp)))
  (let* ((*all-functions-to-check-for*
           ;; References to these locally bound functions
           ;; should not be reported as refs to the external functions to check for.
           (remove-if #'(lambda (elt) (assq elt (cadr exp)))
                      *all-functions-to-check-for*)))
    (with-stack-list* (*cw-function-environment*
                        (loop for elt in (and (consp (cadr exp)) (cadr exp))
                              nconc (list* (locf (symbol-function (car elt)))
                                           nil
                                           nil))
                        *cw-function-environment*)
      (setq body (cw-clause (cddr exp)))
      (if *cw-return-expansion-flag*
          `(flet ,bindlist . ,body)))))

(defun (:property macrolet cw-handler) (exp)
  (let ((*all-functions-to-check-for*
          ;; References to these locally bound functions
          ;; should not be reported as refs to the external functions to check for.
          (remove-if #'(lambda (elt) (assq elt (cadr exp)))
                     *all-functions-to-check-for*)))
    (with-stack-list* (*cw-function-environment*
                        (loop for elt in (and (consp (cadr exp)) (cadr exp))
                              nconc (list* (locf (symbol-function (car elt)))
                                           ;; Local macros are defined in the top level
                                           ;; environment.  -- RpK 28-Jan-87 15:01:57
                                           `(macro . ,(si:expand-defmacro elt nil))
                                           nil))        ;for cdr-next for nconc
                        *cw-function-environment*)
      (let ((body (cw-clause (cddr exp))))
        (when *cw-return-expansion-flag*
          ;; No need to have a MACROLET in the result
          ;; since there cannot be any uses of the local macros remaining after expansion.
          (if (= (length body) 1)
              (car body)
              `(progn . ,body)))))))

(defun (:property labels cw-handler) (exp &aux bindlist body)
  (let* ((*all-functions-to-check-for*
           ;; References to these locally bound functions
           ;; should not be reported as refs to the external functions to check for.
           (remove-if #'(lambda (elt) (assq elt (cadr exp)))
                      *all-functions-to-check-for*)))
    (with-stack-list* (*cw-function-environment*
                        (loop for elt in (and (consp (cadr exp)) (cadr exp))
                              nconc (list* (locf (symbol-function (car elt))) nil nil))
                        *cw-function-environment*)
      (setq bindlist (cw-flet-binding-list (cadr exp)))
      (setq body (cw-clause (cddr exp)))
      (if *cw-return-expansion-flag*
          `(labels ,bindlist . ,body)))))

(defun (:property lambda cw-handler) (exp)
  (if *cw-return-expansion-flag*
      `(function ,(cw-lambda-expression exp))
    (cw-lambda-expression exp)))

;;;; Random fexprs
(defprop catch cw-eval-args cw-handler)
(defprop and cw-eval-args cw-handler)
(defprop or cw-eval-args cw-handler)
(defprop setq cw-eval-args cw-handler)
(defprop login-setq cw-eval-args cw-handler)
(defprop progn cw-eval-args cw-handler)
(defprop locally cw-eval-args cw-handler)
(defprop progv cw-eval-args cw-handler)
(defprop progw cw-eval-args cw-handler)
(defprop unwind-protect cw-eval-args cw-handler)
(defprop dont-optimize cw-eval-args cw-handler)
(defprop eval-when cw-first-arg-quoted cw-handler)
(defprop multiple-value-list cw-eval-args cw-handler)
(defprop nth-value cw-eval-args cw-handler)
(defprop si::advise-progn cw-eval-args cw-handler)
(defprop si::advise-let cw-let cw-handler)
(defprop si::advise-setq cw-eval-args cw-handler)
(defprop si::setq-if-unbound cw-eval-args cw-handler)
(defprop si::advise-multiple-value-list cw-eval-args cw-handler)
(defprop patch-source-file cw-first-arg-quoted cw-handler)
(defprop si::defvar-1 cw-first-arg-quoted cw-handler)
(defprop si::defconst-1 cw-first-arg-quoted cw-handler)
(defun (:property si::advise-prog cw-handler) (exp)
  (cw-prog-form exp #'cw-parallel-binding-list))
(defprop si::encapsulation-let cw-let cw-handler)
; this is never seen outside the evaluator
;(defprop si::catch-for-eval cw-eval-args cw-handler)
; another losing special form bites the dust! (by way of changes to sys2;selev)
;(defun (:property si::matchcarcdr cw-handler) (exp)
;  (let ((arg (cw-expression (cadr exp)))
;       (car (cw-lambda-expression (caddr exp)))
;       (cdr (cw-lambda-expression (cadddr exp))))
;    (if *cw-return-expansion-flag* `(si::matchcarcdr ,arg ,car ,cdr))))
