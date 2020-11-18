;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Lowercase:T; Readtable:ZL; Base:10 -*-

;;;SPECIAL-FORMS.LISP

;;;Contains definitions of standard special forms (CL and ZL), including
;;;some support functions.
;;;
;;;See also DEFSPECIALKs in SYS:SYS;QFCTNS.LISP, which must be required in
;;;cold load (?).

;; Produce code to evaluate a special form body, found as the value of BODYVAR.
;; The code produced will return multiple values from the last element of the body.

(defmacro eval-body (body)
  `(if (null ,body) nil
     (do ((l ,body (cdr l)))
         ((null (cdr l))
          (eval1 (car l)))
       (eval1 (car l)))))

(defspecialk catch (tag &quote &rest body)
  "Set up a tag TAG that a THROW can throw to.
If a THROW with argument EQ to TAG is executed dynamically within FORMS,
it returns immediately from the CATCH, skipping the rest of the execution of FORMS.
The second argument of THROW is returned from the CATCH."
  (catch tag
    (eval-body body)))

(defspecialk comment (&quote &rest ignore)
  "Ignores all arguments and returns the symbol COMMENT.  It is usually
preferable to comment code using the semicolon-macro feature of the standard
input syntax.  Comments using semicolons are ignored by the Lisp reader."
  'comment)

(defspecialk declare (&quote &rest declarations)
  "The body is made up of declarations, which are in effect throughout the
construct at the head of whose body the DECLARE appears.

At top level in compiled code, either EVAL-WHEN or PROCLAIM should be used instead.
DECLARE is ignored in the interpreter."
  (declare (ignore declarations))
  'declare)

;;;Following definition assumes we are evalling.  COMPILE-DRIVER takes care
;;;of compiling and loading.

(defspecialk eval-when (&quote times &rest forms)
  "Process the FORMS only at the specified TIMES.
TIMES is a list which may include COMPILE, EVAL or LOAD.
EVAL means to eval the FORMS if the EVAL-WHEN is processed by the interpreter,
 or to compile and eval them when compiling to core.
LOAD means the compiler when compiling to a file should compile the FORMS
 if appropriate and then make them be executed when the QFASL file is loaded.
COMPILE means the compiler should execute the forms
 at compile time.
/(EVAL LOAD) is equivalent to the normal state of affairs."
  (declare (zwei:indentation 1 1))
  (unless (and (cl:listp times)
               (loop for time in times always (memq time '(eval load compile))))
    (ferror "~S is an invalid specifier for ~S;
it should be a list consisting of ~S, ~S, and//or ~S."
            times 'eval-when 'eval 'load 'compile))
    (when (memq 'eval times)
      (eval-body forms)))

(defspecialk compiler-let (&quote bindlist &rest body)
  "Perform bindings in BINDLIST at evaluation or compilation time.
In interpreted code, this is the same as LET.
When found in code being compiled, the bindings are done at compile time,
and are not done when the compiled code is run."
  (declare (dbg:uninteresting-function eval))
  (declare (zwei:indentation 1 1))
;;>> It's possible to do much better than this using the appropriate pieces of
;;>> gobble-declarations and friends, but who cares?
  (eval1 `(let ,bindlist
            (declare (special . ,(mapcar (lambda (x) (if (atom x) x (car x))) bindlist)))
            . ,body)))

(defspecialk the (&quote type value)
  "Returns the value(s) of VALUE, but declares them to be of type(s) TYPE."
  ;; run time type-checking may not be worth the pain
  (declare (ignore type))
  (eval1 value))

(defspecialk quote (&quote x)
  "QUOTE returns its argument without evaluating it.
(QUOTE X) returns X.  This is useful because X is not evaluated.
This is the same thing as 'X"
  x)

(defspecialk setq (&quote &rest symbols-and-values)
  "Given alternating variables and value expressions, sets each variable to following value.
Each variable is set before the following variable's new value is evaluated.
See also PSETQ which computes all the new values and then sets all the variables."
  (declare (dbg:uninteresting-function eval))
  (prog (val)
     l  (cond ((null symbols-and-values) (return val))
              ((null (cdr symbols-and-values))
               (ferror "Odd number of arguments to ~S" 'setq)))
        (if (eq *interpreter-function-environment* t)
            (progn
              (require-bindable-symbol (car symbols-and-values) "set")
              (set (car symbols-and-values) (setq val (eval1 (cadr symbols-and-values)))))
          (interpreter-set (car symbols-and-values)
                           (setq val (eval1 (cadr symbols-and-values)))))
        (setq symbols-and-values (cddr symbols-and-values))
        (go l)))

(defspecialk variable-boundp (&quote variable)
  "Return T if VARIABLE has a value (is not unbound)."
  (if (eq *interpreter-function-environment* t)
      (boundp variable)
      (location-boundp (interpreter-value-cell-location variable))))

(defspecialk variable-location (&quote variable)
  "Return a locative pointer to the place where the value of VARIABLE is stored."
  (if (eq *interpreter-function-environment* t)
      (%external-value-cell variable)
      (interpreter-value-cell-location variable)))

(defspecialk variable-makunbound (&quote variable)
  "Make VARIABLE unbound.  References to it will get errors."
  (if (eq *interpreter-function-environment* t)
      (location-makunbound (%external-value-cell variable))
      (location-makunbound (interpreter-value-cell-location variable))))

(defspecialk multiple-value-setq (&quote var-list exp)
  "Evaluate EXP, collecting multiple values, and set the variables in VAR-LIST to them.
Returns the first value of EXP."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (let ((val-list (multiple-value-list (eval1 exp))))
    (do ((vars var-list (cdr vars))
         (vals val-list (cdr vals)))
        ((null vars))
      (when (car vars)                          ;allow (multiple-value-setq (nil foo) ...)
        (if (eq *interpreter-function-environment* t)
            (set (car vars) (car vals))
            (interpreter-set (car vars) (car vals)))))
    (car val-list)))

;; CANT USE DEFF FOR THIS IN COLD-LOAD.
;; also must have kluding eval to fool compile/cold-fasload
(EVAL '(FSET-CAREFULLY '(:SPECIAL-FORM multiple-value) #'(:SPECIAL-FORM multiple-value-setq)))

(defspecialk nth-value (value-number &quote exp)
  "Returns the VALUE-NUMBER'th (0-based) value of EXP.
Compiles into fast code when VALUE-NUMBER is a constant."
  (declare (zwei:indentation 1 1))
  (nth value-number (multiple-value-list (eval1 exp))))

(defspecialk multiple-value-call (function &quote &rest forms)
  "Call FUNCTION like FUNCALL, but use all values returned by each of FORMS.
FUNCALL would use only the first value returned by each of them."
  ;;This conses, alas.
  (declare (dbg:uninteresting-function eval))
  ;; Sigh^n
  (let ((args (mapcan (lambda (form)
                        `(:spread ,(multiple-value-list (eval1 form))))
                      forms)))
    (apply #'call function args)))

(defspecialk multiple-value-list (&quote exp)
  "Evaluate the expression EXP and return a list of the values it returns."
  (declare (dbg:uninteresting-function eval))
  (multiple-value-list (eval1 exp)))

(defspecialk multiple-value-prog1 (&quote value-form &rest forms)
  "Evaluates VALUE-FORM followed by the FORMs, then returns ALL the values of VALUE-FORM."
  (declare (dbg:uninteresting-function eval))
  (multiple-value-prog1 (eval1 value-form)
                        (mapc #'eval1 forms)))

(defun values (&rest values)
  "Return multiple values -- as many values as we have arguments."
  (values-list values))

(defun values-list (list-of-values)
  "Return multiple values -- each element of our arg is a separate value."
  (values-list list-of-values))

(defspecialk multiple-value-bind (&quote var-list exp &rest body)
  "Evaluate EXP, collecting multiple values, and set the variables to them."
  (declare (zwei:indentation 1 3 2 1))
  (let ((val-list (multiple-value-list (eval1 exp))))
    (if (eq *interpreter-function-environment* t)
        (zl-bind-variables-spread (var-list val-list)
          (eval-body body))
      (gobble-declarations-from-body (vars-env body)
        (bind-variables-spread (var-list val-list vars-env)
          (eval-body body))))))
(defspecialk dont-optimize (&quote &rest body)
  "Prevent all optimization or open coding of the top-level forms of BODY.
Aside from that effect, it is equivalent to PROGN.
/(Note that the arguments to forms in BODY will still be optimized unless
there is another DONT-OPTIMIZE saying not to do that, and so on)"
  (declare (dbg:uninteresting-function eval))
  (eval-body body))

(defspecialk locally (&quote &rest body)
  "Common Lisp local declaration construct.
LOCALLY is like PROGN except that Common Lisp says that declarations
are allowed only in LOCALLY, not in PROGN, and because PROGN is treated
specially as a top-level form by the compiler."
  (declare (zwei:indentation 0 1))
  (declare (dbg:uninteresting-function eval))
  (gobble-declarations-from-body (vars body)
    (eval-body body)))

(defspecialk progn (&quote &rest body)
  "Evaluate all the arguments in order and return the value of the last one.
Multiple values are passed along from that argument's evaluation."
  (declare (dbg:uninteresting-function eval))
  ;;>> this is a Stallmanism --- commonlisp doesn't want progn to mung declarations.
  ;;>>  Consider progn at top-level in a file being compiled.  And then consider the
  ;;>>  kludge in qcfile.
  (gobble-declarations-from-body (vars body)
    (eval-body body)))

;;; These functions have hair to implement the correct rules for multiple values

(defun prog2 (ignored value &rest ignored)
  "Return the second argument.
Always returns exactly one value."
  value)

(defun prog1 (value &rest ignored)
  "Return the first argument.
Always returns exactly one value.  Contrast with MULTIPLE-VALUE-PROG1."
  value)

(defspecialk with-stack-list (&quote variable-and-elements &rest body)
  "Executes BODY with VARIABLE bound to a temporary list containing ELEMENTS.
In compiled code, the temporary list lives inside the stack, like a &REST argument.
It disappears when the WITH-STACK-LIST is exited.  No garbage is produced.
In interpreted code, this is equivalent to (LET ((VARIABLE (LIST . ELEMENTS))) . BODY)"
  (declare (arglist ((variable . elements) &rest body)))
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (bind-variable ((car variable-and-elements)
                  (mapcar #'eval1 (cdr variable-and-elements))
                  body)
    (eval-body body)))

(compiler:make-obsolete with-stack-list
   "most processor-dependent functions will go away in a future release")

(defspecialk with-stack-list* (&quote variable-and-elements &rest body)
  "Executes BODY with VARIABLE bound to a temporary list equal to LIST* of ELEMENTS.
When compiled, The temporary list lives inside the stack, like a &REST argument.
It disappears when the WITH-STACK-LIST* is exited.  No garbage is produced.
When interpreted, this is just the same as (LET ((VARIABLE (LIST* . ELEMENTS))) . BODY)"
  (declare (arglist ((variable . elements) &rest body)))
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (bind-variable ((car variable-and-elements)
                  (apply #'list* (mapcar #'eval1 (cdr variable-and-elements)))
                  body)
    (eval-body body)))

(compiler:make-obsolete with-stack-list*
   "most processor-dependent functions will go away in a future release")

(defspecialk and (&quote &rest expressions)
  "Evaluates the EXPRESSIONS until one returns NIL or they are all done.
Returns NIL in the first case; the values of the last expression in the second."
  (declare (dbg:uninteresting-function eval))
  (if (null expressions) t
    (do ((l expressions (cdr l)))
        ((null (cdr l))
         (eval1 (car l)))
      (or (eval1 (car l))
          (return nil)))))

(defspecialk or (&quote &rest expressions)
  "Evaluates the EXPRESSIONS until one returns non-NIL or they are all done.
Returns the value of the last expression evaluated.
If all the expressions are evaluated, then all the multiple values of the
last expression are passed along."
  (declare (dbg:uninteresting-function eval))
  (if (null expressions) nil
    (do ((l expressions (cdr l))
         (val))
        ((null (cdr l))
         (eval1 (car l)))
      (and (setq val (eval1 (car l)))
           (return val)))))

(defspecialk cond (&quote &rest clauses)
  "Looks for the first CLAUSE whose predicate is true, and executes that clause.
Each element of the body of a COND is called a CLAUSE.

The first element of each clause is a PREDICATE-EXPRESSION.
This is evaluated to see whether to execute the clause.
If the predicate's value is non-NIL, all the remaining elements of the clause
are executed, as in a PROGN, and the value(s) of the last one are returned by COND.
If the clause contains only one element, the predicate, then
the predicate's value is returned if non-NIL.
In this case, unless it is the last clause, the predicate is not
being called tail-recursively and so only its first value is returned.

If no clause's predicate evaluates non-NIL, the COND returns NIL."
  (declare (dbg:uninteresting-function eval))
  (do ((clauses clauses (cdr clauses))
       (predval) (expressions))
      ((null clauses) nil)
    (cond ((atom (car clauses))
           (ferror "The atom ~S is not a valid ~S clause." (car clauses) 'cond))
          ((and (null (cdr clauses)) (null (cdar clauses)))
           ;; If this is the last clause, then treat its predicate as part of
           ;; the body instead of as the predicate, so that multiple values
           ;; get propagated properly.
           (setq expressions (car clauses)))
          ((setq predval (eval1 (caar clauses)))
           (or (setq expressions (cdar clauses))
               (return predval)))
          (t (go nextloop)))
    ;; Predicate true
    (return (eval-body expressions))
   nextloop
    ))

(defspecialk if (&quote test then &rest elses)
  "Execute THEN if TEST comes out non-NIL; otherwise, execute the ELSES."
  (declare (zwei:indentation 2 1))
  (declare (dbg:uninteresting-function eval))
  (if (eval1 test)
      (eval1 then)
    (eval-body elses)))


(defspecialk compiler:casen (index &quote &rest clauses)
  ;; in compiler package until next cold load.
  "A special form that evaluates the INDEX's clause of CLAUSES. Defaultly last clause."
  (CHECK-TYPE INDEX FIXNUM)
  (LET ((N (LENGTH CLAUSES)))
    (EVAL1 (NTH (IF (OR (< INDEX 0) (NOT (< INDEX N)))
                    (1- N)
                  INDEX)
                CLAUSES))))

(defspecialk let (&quote varlist &rest body)
  "Binds some variables and then evaluates the BODY.
VARLIST is a list of either variables or lists (variable init-exp).
The init-exps are evaluated, and then the variables are bound.
Then the body is evaluated sequentially and the values
of the last expression in it are returned."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (if (eq *interpreter-function-environment* t)
      (zl-parallel-binding-list (varlist)
        (eval-body body))
    (gobble-declarations-from-body (vars-env body)
      (parallel-binding-list (varlist vars-env)
        (eval-body body)))))

(defspecialk let* (&quote varlist &rest body)
  "Like LET, but binds each variable before evaluating the initialization for the next.
Thus, each variable's initialization can refer to the values of the previous ones."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (if (eq *interpreter-function-environment* t)
      (zl-serial-binding-list (varlist)
        (eval-body body))
    (gobble-declarations-from-body (vars-env body)
      (serial-binding-list (varlist vars-env)
        (eval-body body)))))

(defspecialk flet (&quote function-list &rest body)
  "Execute BODY with local function definitions as per FUNCTION-LIST.
Each element of FUNCTION-LIST looks like (NAME (ARGS...) BODY...).
FLET rebinds the function definition of each NAME lexically to
 (LAMBDA (ARGS...) BODY...), closed in the environment outside the FLET.
See also LABELS."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (if (eq *interpreter-function-environment* t)
      (ferror "~S is meaningless when performing old-style non-lexical evaluation" 'flet)
    (gobble-declarations-from-body (vars body)
      (function-binding-list (function-list flet)
        (eval-body body)))))

(defspecialk macrolet (&quote macro-list &rest body)
  "Execute BODY with macro function definitions as per MACRO-LIST.
Each element of MACRO-LIST looks like (NAME (ARGS...) BODY...).
MACROLET rebinds the function definition of each NAME lexically to
 a macro like the one you would get by doing
 (DEFMACRO NAME (ARGS...) BODY...)."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (if (eq *interpreter-function-environment* t)
      (ferror "~S is meaningless when performing old-style non-lexical evaluation" 'macrolet)
    (gobble-declarations-from-body (vars body)
      (function-binding-list (macro-list macrolet)
        (eval-body body)))))

(defspecialk labels (&quote function-list &rest body)
  "Execute BODY with local function definitions as per FUNCTION-LIST.
Each element of FUNCTION-LIST looks like (NAME (ARGS...) BODY...).
LABELS rebinds the function definition of each NAME lexically to
 (LAMBDA (ARGS...) BODY...), closed in the environment inside the LABELS.
This means that the functions defined by the LABELS can refer to
themselves and to each other.  See also FLET."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (if (eq *interpreter-function-environment* t)
      (ferror "~S is meaningless when performing old-style non-lexical evaluation" 'labels)
    (gobble-declarations-from-body (vars body)
      (function-binding-list (function-list labels)
        ;; The values were not evaluated yet.
        ;; The binding frame contains the expressions.
        ;; Eval them now and store the values in their places.
        (do ((frametail (car *interpreter-function-environment*) (cddr frametail)))
            ((null frametail))
          (setf (cadr frametail) (interpreter-enclose (cadr frametail))))
        (eval-body body)))))

(defspecialk progv (vars vals &quote &rest body)
  "Bind the VARS to the VALS and then execute the BODY.
Note that the expressions you write for VARS and VALS
are evaluated on each entry to PROGV,
so the variables bound may be different each time.
The variables are always bound as specials if they are bound;
therefore, strictly speaking only variables declared special should be used."
  (declare (zwei:indentation 2 1))
  (declare (dbg:uninteresting-function eval))
  (do ((vars vars (cdr vars))
       (vals vals (cdr vals)))
      ((null vars)
       (eval-body body))
    (%bind (require-bindable-symbol (car vars)) (car vals))))

;;; (PROGW '((VAR-1 VAL-1) (VAR-2 VAL-2) ... (VAR-N VAL-N)) &BODY BODY)
;;; Binds VAR-I to VAL-I (evaluated) during execution of BODY
(defspecialk progw (vars-and-vals &quote &rest body)
  "Perform bindings from a list of variables and expressions, then execute the BODY.
VARS-AND-VALS is a list of elements like (VARIABLE VALUE-FORM).
The VALUE-FORMs are all evaluated by PROGW, even when compiled.
Note that the value of VARS-AND-VALS is computed each time,
 and always in the global environment.
The variables are always bound as specials if they are bound;
therefore, strictly speaking only variables declared special should be used."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (do ((vars-and-vals vars-and-vals (cdr vars-and-vals)))
      ((null vars-and-vals)
       (eval-body body))
    (%bind (require-bindable-symbol (caar vars-and-vals))
           ;; eval, not eval1
           (eval1 (cadar vars-and-vals)))))

;;; (LET-IF <COND> ((VAR-1 VAL-1) (VAR-2 VAL-2) ... (VAR-N VAL-N)) &BODY BODY)
;;; If <COND> is not nil, binds VAR-I to VAL-I (evaluated) during execution of BODY,
;;; otherwise just evaluates BODY.
(defspecialk let-if (cond &quote var-list &quote &rest body)
  "Perform the bindings in VAR-LIST only if COND evaluates non-NIL; the execute the BODY.
Aside from the presence of COND, LET-IF is just like LET.
The variables are always bound as specials if they are bound;
therefore, strictly speaking only variables declared special should be used."
  (declare (zwei:indentation 2 1))
  (declare (dbg:uninteresting-function eval))
  (if (not cond)
      (if (eq *interpreter-function-environment* t)
          (eval-body body)
        (gobble-declarations-from-body (vars-env body)
          (eval-body body)))
    ;; Cannot use PROGW here; it calls EVAL rather than EVAL1.
    (if (eq *interpreter-function-environment* t)
        (zl-parallel-binding-list (var-list)
          (eval-body body))
      (gobble-declarations-from-body (vars-env body)
        (parallel-binding-list (var-list vars-env)
          (eval-body body))))))

(defspecialk letf (&quote places-and-values &rest body)
  "LETF is like LET, except that it it can bind any storage cell
rather than just value cells.
PLACES-AND-VALUES is a list of lists of two elements, the car of each
 of which specifies a location to bind (this should be a form acceptable to LOCF)
 and the cadr the value to which to bind it.
The places are bound in parallel.
Then the body is evaluated sequentially and the values
of the last expression in it are returned.
/(Note that the bindings made by LETF are always /"special/")"
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  ;;>> A kludge, indeed
  (with-stack-list (kludge 'locf nil)
    (with-current-interpreter-environment (env)
      (prog ((vars-left places-and-values))
         bindloop
            (when vars-left
              ;; Am I CONSING yet?
              (setf (cadr kludge) (caar vars-left))
              ;; macro-function is bagbitingly slow
              (%push (eval1 (funcall (cdr (symbol-function 'locf)) kludge env)))
              (%push (eval1 (cadar vars-left)))
              (pop vars-left)
              (go bindloop))
            (setq vars-left places-and-values)
         bindloop1
            (when vars-left
              (%bind (%pop) (%pop))
              (pop vars-left)
              (go bindloop1))
            (return (eval-body body))))))

(defspecialk letf-if (cond &quote places-and-values &rest body)
  "LETF-IF is like LETF, except that the binding takes place only if COND-FORM evaluates non-NIL"
  (declare (zwei:indentation 2 1))
  (declare (dbg:uninteresting-function eval))
  ;;>> A kludge, indeed
  (when cond
    (with-stack-list (kludge 'locf nil)
      (with-current-interpreter-environment (env)
        (prog ((vars-left places-and-values))
           bindloop
              (when vars-left
                ;; Am I CONSING yet?
                (setf (cadr kludge) (caar vars-left))
                ;; macro-function is bagbitingly slow
                (%push (eval1 (funcall (cdr (symbol-function 'locf)) kludge env)))
                (%push (eval1 (cadar vars-left)))
                (pop vars-left)
                (go bindloop))
              (setq vars-left places-and-values)
           bindloop1
              (when vars-left
                (%bind (%pop) (%pop))
                (pop vars-left)
                (go bindloop1))))))
  (eval-body body))

(defspecialk letf* (&quote places-and-values &rest body)
  "Like LETF except that binding of PLACES-AND-VALUES is done in series."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  ;;>> A kludge, indeed
  (with-stack-list (kludge 'locf nil)
    (with-current-interpreter-environment (env)
      (prog ((vars-left places-and-values))
         bindloop
            (when vars-left
              (setf (cadr kludge) (caar vars-left))
              ;; symbol-function is bagbitingly slow
              (%bind (eval1 (funcall (cdr (symbol-function 'locf)) kludge env))
                     (eval1 (cadar vars-left)))
              (pop vars-left)
              (go bindloop))
            (return (eval-body body))))))

;;; Interpreter version of UNWIND-PROTECT
;;; (UNWIND-PROTECT risky-stuff forms-to-do-when-unwinding-this-frame...)
;;; If risky-stuff returns, we return what it returns, doing forms-to-do
;;; (just as PROG1 would do).  If risky-stuff does a throw, we let the throw
;;; function as specified, but make sure that forms-to-do get done as well.
(defspecialk unwind-protect (&quote body-form &rest cleanup-forms)
  "Execute BODY-FORM, and on completion or nonlocal exit execute the CLEANUP-FORMS."
  (declare (zwei:indentation 0 3 1 1))
  (declare (dbg:uninteresting-function eval))
  (unwind-protect (eval1 body-form)
    (dolist (form cleanup-forms)
      (eval1 form))))

(defspecialk throw (tag &quote &rest value-expression)
  "Throw the values of VALUE-EXPRESSION to TAG.
The innermost catch for TAG will return these values to its caller.
 For backwards compatibility, there may be multiple values-expressions:
 (throw 'foo bar baz) is equivalent to (throw 'foo (values bar baz))
 New code should always use the two-argument form."
  (declare (arglist tag &quote value-expression))
  (declare (dbg:uninteresting-function eval))
  (throw tag
         (if (or (null value-expression) (cdr value-expression))
             (values-list (mapcar #'eval1 value-expression))
             (eval1 (car value-expression)))))

(EVAL '(FSET-CAREFULLY '(:SPECIAL-FORM *throw) #'(:SPECIAL-FORM throw)))

(defspecialk block (&quote name &rest body)
  "Make nonlocal exit point named NAME for use with RETURN-FROM within BODY.
BODY is evaluated, and the value(s) of the last form in it are returned,
except that if RETURN-FROM is used with our NAME as its argument
during the execution of BODY, control immediately exits from this BLOCK
with values specified by the arguments to RETURN-FROM.
If NAME is NIL, RETURN can also be used to exit this block."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (check-type name symbol)
  (enter-block name
    (if (eq *interpreter-function-environment* t)
        (eval-body body)
      (gobble-declarations-from-body (vars body)
        (eval-body body)))))

(defspecialk return-from (&quote blockname &rest vals)
  "Return from a BLOCK named BLOCKNAME, or from a named PROG or DO.
The first arg (not evaluated) is the name.
If that is the only argument, zero values are returned.
With exactly one additional argument, its value(s) are returned.
With more arguments, each argument (except the first) produces
one value to be returned."
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (check-type blockname symbol)
  (let ((values (if (cdr vals)
                    (mapcar #'eval1 vals)
                    (multiple-value-list (eval1 (car vals))))))
    (do ((tail *interpreter-frame-environment* (cdr tail))
         foundp)
        ((atom tail)
         (ferror (if foundp "The ~S ~S is no longer lexically active."
                   "There is no lexically-visible ~S named ~S.")
                 'block blockname))
      (let ((frame (car tail)))
        (when (and (eq (car frame) 'block)
                   (eq blockname (car (cadr frame))))
          (if (cadr (cadr frame))
              (throw (cdr (cadr frame))
                     (values-list values))
            (setq foundp t)))))))

(defspecialk return (&quote &rest vals)
  "Return from a BLOCK named NIL, or from the innermost PROG or DO.
Exactly the same as RETURN-FROM with NIL as first argument.
BLOCKs are candidates for RETURN only if named NIL,
but any PROG or DO is a candidate regardless of its name.
With exactly one argument, its value(s) are returned.
With zero or multiple arguments, each argument produces
one value to be returned."
  (declare (dbg:uninteresting-function eval))
  (let ((values (if (cdr vals)
                    (mapcar #'eval1 vals)
                    (multiple-value-list (eval1 (car vals))))))
    (do ((tail *interpreter-frame-environment* (cdr tail))
         foundp)
        ((atom tail)
         (ferror (if foundp "The ~S ~S is no longer lexically active."
                   "There is no lexically-visible ~S named ~S.")
                 'block 'nil))
      (let ((frame (car tail)))
        (when (and (eq (car frame) 'block)
                   (eq (car (cadr frame)) 'nil))
          (if (cadr (cadr frame))
              (throw (cdr (cadr frame))
                     (values-list values))
            (setq foundp t)))))))

(defun return-list (values)
  "Return the elements of VALUES from a BLOCK named NIL, or from the innermost PROG or DO.
BLOCKs are candidates for RETURN only if named NIL,
but any PROG or DO is a candidate regardless of its name.
Each element of VALUES becomes a single returned value.
It is preferable to write (RETURN (VALUES-LIST values))."
  (declare (dbg:uninteresting-function eval))
  (do ((tail *interpreter-frame-environment* (cdr tail))
       foundp)
      ((atom tail)
       (ferror (if foundp "The ~S ~S is no longer lexically active."
                 "There is no lexically-visible ~S named ~S.")
               'block 'nil))
    (let ((frame (car tail)))
      (when (and (eq (car frame) 'block)
                 (eq (car (cadr frame)) 'nil))
        (if (cadr (cadr frame))
            (throw (cdr (cadr frame))
                   (values-list values))
          (setq foundp t))))))


(defspecialk tagbody (&quote &rest body)
  "Execute BODY, allowing GO to transfer control to go-tags in BODY.
Lists in BODY are expressions to be evaluated (/"statements/").
Symbols in BODY are tags, which are ignored when reached sequentially.
However, GO may be used within any of the statements
to transfer control to any of the tags in BODY.
After a GO, execution of the TAGBODY form will continue with
the next statement in BODY following the tag.

TAGBODY returns only when execution reaches the end.
Its value is always NIL.  A nonlocal exit of some sort
is the only way to get out with any other value."
  (declare (zwei:indentation zwei::indent-prog))
  (declare (dbg:uninteresting-function eval))
  (tagbody-internal body))

;;; Execute the body of a TAGBODY (or, a PROG).
;;; Puts a TAGBODY entry on *INTERPRETER-FRAME-ENVIRONMENT* so that GO can find
;;; which tags are available to go to, and where they are in the TAGBODY.
;;; The TAGBODY entry also contains a catch tag that GO can throw to
;;; to do a GO.  The arg thrown is the pointer to the spot in the TAGBODY
;;; where the desired tag appears.
(defun tagbody-internal (body)
  (declare (dbg:uninteresting-function eval))
  (with-stack-list (tem body nil)
    (with-stack-list (frame 'tagbody tem)
      (with-stack-list* (*interpreter-frame-environment*
                          frame *interpreter-frame-environment*)
        (do ((pc body) exp)
            ((null pc))
          (if (atom pc) (ferror "Non-~S atomic cdr, ~S, in ~S form ~S." 'nil pc 'tagbody body))
          (setq exp (pop pc))
          (if (atom exp)
              nil
            (block fred
              ;; if the environment was copied in the eval1 below last time round,
              ;;  TEM (and FRAME for that matter, though it is irrelevant)
              ;;  will be one-q-forwarded to a new value. (see unstackify-environment)
              ;; Since we reuse tem in the catch-tag every time,
              ;;  and since CATCH uses EQ, which doesn't follow
              ;;  the 1qf's, we must chase the forwarding each time around.
              ;; We don't need this is the BLOCK case since the block's CATCH
              ;;  is entered exectly once
              (setq tem (follow-cell-forwarding tem nil))
              (setq pc (cdr (catch (cdr tem)
                              ;; points to catch-tag in catch-frame
                              (let ((tem1 (%regular-pdl-index)))
                                (setf (cadr tem) tem1))
                              (return-from fred
                                (unwind-protect
                                    (eval1 exp)
                                  ;; See comment in enter-block
                                  (setf (cadr tem) nil)))))))))
        nil))))

(defspecialk go (&quote tag &aux tem)
  "Transfer control to the tag TAG in a lexically containing TAGBODY or PROG, etc.
May be used within TAGBODY, PROG, PROG*, DO, DO*, or anything expanding into them.
TAG is not evaluated.
Control transfers instantaneously; the remainder of this statement
of the TAGBODY or PROG is not completed.
See the documentation of TAGBODY for more info."
  (do ((tail *interpreter-frame-environment* (cdr tail))
       foundp)
      ((atom tail)
       (ferror (if foundp "The ~S tag ~S is no longer lexically active"
                 "Unseen ~S tag ~S.")
               'go tag))
    (let ((frame (car tail)))
      (when (and (eq (car frame) 'tagbody)
                 (setq tem (memq tag (car (cadr frame)))))
        (if (cadr frame)
            (throw (cdr (cadr frame)) tem)
          (setq foundp t))))))

(defspecialk prog (&quote &rest prog-arguments)
  "Old-fashioned form that combines a LET, a BLOCK and a TAGBODY.
Usage is (PROG name varlist body...) or (PROG varlist body...).
A non-NIL symbol is interpreted as a NAME; NIL or a cons is a VARLIST.
These two forms of usage are equivalent to
  (BLOCK name
    (BLOCK NIL
      (LET varlist
        (TAGBODY body...))))
or, in the case with no specified NAME,
  (BLOCK NIL
    (LET varlist
      (TAGBODY body...)))
BLOCK establishes the RETURN-point, LET binds the variables,
and TAGBODY executes the body and handles GO tags.
See the documentation of BLOCK, LET and TAGBODY for more information.
PROG is semi-obsolete, but too ancient to be flushed."
  (declare (arglist /[progname/] varlist &body body))
  (declare (zwei:indentation zwei::indent-prog))
  (declare (dbg:uninteresting-function eval))
  (let* ((progname (and (atom (car prog-arguments))
                        (car prog-arguments)))
         (varlist (if progname
                      (second prog-arguments)
                      (first prog-arguments)))
         (progbody (if progname
                       (cddr prog-arguments)
                       (cdr prog-arguments))))
    (check-type progname symbol)
    (block nil
      (if (eq *interpreter-function-environment* t)
          (enter-block (if (eq progname t) t nil)
            (enter-block progname
              (zl-parallel-binding-list (varlist)
                (tagbody-internal progbody))))
        (gobble-declarations-from-body (vars-env progbody)
          (parallel-binding-list (varlist vars-env)
            (enter-block (if (eq progname t) t nil)
              (enter-block progname
                (tagbody-internal progbody)))))))))

(defspecialk prog* (&quote &rest prog-arguments)
  "Old fashioned form that combines a LET*, a BLOCK and a TAGBODY.
PROG* is the same as PROG except that the variables are bound sequentially,
as in LET*, whereas PROG binds them in parallel, like LET."
  (declare (arglist /[progname/] varlist &body body))
  (declare (zwei:indentation zwei::indent-prog))
  (declare (dbg:uninteresting-function eval))
  (let* ((progname (and (atom (car prog-arguments))
                        (car prog-arguments)))
         (varlist (if progname
                      (second prog-arguments)
                      (first prog-arguments)))
         (progbody (if progname
                       (cddr prog-arguments)
                       (cdr prog-arguments))))
    (check-type progname symbol)
    (block nil
      (if (eq *interpreter-function-environment* t)
          (enter-block (if (eq progname t) t nil)
            (enter-block progname
              (zl-serial-binding-list (varlist)
                (tagbody-internal progbody))))
        (gobble-declarations-from-body (vars-env progbody)
          (serial-binding-list (varlist vars-env)
            (enter-block (if (eq progname t) t nil)
              (enter-block progname
                (tagbody-internal progbody)))))))))

;;;; Various sorts of DOs.

(defspecialk do (&quote &rest x)
  "DO provides a generalized iteration facility.
 The general usage looks like this:
  (DO ((var initialization repeat-form) ...)
      (end-test . (result-forms ...))
     . body)
 where the vars are bound in parallel (like LET).
/
 For example, to print successive CDRs of a list,
 and then print and return the original list:
/
   /(do ((sublist list (cdr sublist)))
       ((null sublist)
        (print list)
        list)
     (format t /"~&-> ~S/" sublist))
"
  (declare (zwei:indentation 2 1))
  (declare (dbg:uninteresting-function eval))
  (do-internal x nil))

(defspecialk do-named (&quote name &rest x)
  "Like DO surrounded by a block named NAME"
  (declare (zwei:indentation 3 1))
  (declare (dbg:uninteresting-function eval))
  (enter-block name
    (do-internal x name)))

(defun do-internal (x name &aux varlist endtest retvals oncep)
  (declare (dbg:uninteresting-function eval))
  (if (and (car x) (atom (car x)))              ;"OLD STYLE"
      (let ((body (cddddr x)))
        (bind-variable ((car x) (eval1 (cadr x)) body)
          (do-body nil nil (cadddr x) nil t x body)))
    (setq varlist (car x))
    (setq oncep (null (cadr x)))
    (or oncep (setq endtest (caadr x) retvals (cdadr x)))
    (if (eq *interpreter-function-environment* t)
        (zl-parallel-binding-list (varlist)
          (do-body name oncep endtest retvals nil varlist (cddr x)))
      (gobble-declarations-from-body (vars-env (cddr x))
        (parallel-binding-list (varlist vars-env)
          (do-body name oncep endtest retvals nil varlist (cddr x)))))))

(defspecialk do* (&quote &rest x)
  "Like DO, except that the variables are bound sequentially (like LET*)."
  (declare (zwei:indentation 2 1))
  (declare (dbg:uninteresting-function eval))
  (do*-internal x nil))

(defspecialk do*-named (&quote name &rest x)
  "Like DO* surrounded by a block named NAME"
  (declare (zwei:indentation 3 1))
  (declare (dbg:uninteresting-function eval))
  (enter-block name
    (do*-internal x name)))

(defun do*-internal (x name &aux varlist endtest retvals oncep)
  (declare (dbg:uninteresting-function eval))
  (if (and (car x) (atom (car x)))              ;"OLD STYLE"
      (let ((body (cddddr x)))
        (bind-variable ((car x) (eval1 (cadr x)) body)
          (do-body nil nil (cadddr x) nil  t x body)))
    (setq varlist (car x))
    (setq oncep (null (cadr x)))
    (or oncep (setq endtest (caadr x) retvals (cdadr x)))
    (if (eq *interpreter-function-environment* t)
        (zl-serial-binding-list (varlist)
          (do-body name oncep endtest retvals nil varlist (cddr x) t))
      (gobble-declarations-from-body (vars-env (cddr x))
        (serial-binding-list (varlist vars-env)
          (do-body name oncep endtest retvals nil varlist (cddr x) t))))))

(defun do-body (name oncep endtest retvals oldp varlist body &optional serial-stepping)
  (declare (dbg:uninteresting-function eval))
  (enter-block (eq name t)
    (do ()
        ((and (not oncep) (eval1 endtest))
         ;; Now evaluate the exit actions.
         ;; The last one should return its values out of the DO.
         (eval-body retvals))
      ;; Now execute the body.
      (tagbody-internal body)
      ;; Here after finishing the body to step the DO-variables.
      (and oncep (return nil))
      (cond (oldp (if (eq *interpreter-function-environment* t)
                      (set (car varlist) (eval1 (caddr varlist)))
                      (interpreter-set (car varlist) (eval1 (caddr varlist)))))
            (serial-stepping
             (dolist (elt varlist)
               (and (consp elt) (cddr elt)
                    (if (eq *interpreter-function-environment* t)
                        (set (car elt) (eval1 (caddr elt)))
                      (interpreter-set (car elt) (eval1 (caddr elt)))))))
            (t (do ((vl varlist (cdr vl))
                    (vals (do ((vl varlist (cdr vl))
                               (vals nil (cons (and (consp (car vl)) (cdar vl) (cddar vl)
                                                    (eval1 (caddar vl)))
                                               vals)))  ;******* CONS *******
                              ((null vl) (nreverse vals)))
                          (cdr vals)))
                   ((null vl))
                 (when (and (consp (car vl)) (cdar vl) (cddar vl))
                   (if (eq *interpreter-function-environment* t)
                       (set (caar vl) (car vals))
                     (interpreter-set (caar vl) (car vals))))))))))

(defspecialk function (&quote function)
  "Quotes FUNCTION for use as a function.
If FUNCTION is a symbol, its function definition in the current environment is returned.
If FUNCTION is a list (presumably starting with LAMBDA or some lambda-macro),
 the compiler will compile it; the interpreter will make it into a closure
 that records the lexical variables of the current lexical context."
  (declare (dbg:uninteresting-function eval))
  (cond ((symbolp function)
         (if (eq *interpreter-function-environment* t)
             (symbol-function function)
           (interpreter-fsymeval function)))
        ((memq (car-safe function) '(lambda named-lambda subst cl:subst named-subst
                                     curry-before curry-after))
         (if (eq *interpreter-function-environment* t)
             function
           (interpreter-enclose function)))
        ((functionp function t)
         (fdefinition function))
        ((validate-function-spec function)      ;Function spec
         (fdefinition function))
        (t (ferror "~S is neither a function nor the name of a function" function))))

(defspecialk lambda (&quote &rest cruft)
  "Same as (FUNCTION (LAMBDA <ARGLIST> . <BODY>)).
Encloses a lambda-expression in the current environment.
This returns a closure suitable for FUNCALL, APPLY, etc."
  (declare (arglist arglist &body body))
  (declare (zwei:indentation 1 1))
  (declare (dbg:uninteresting-function eval))
  (interpreter-enclose `(lambda . ,cruft)))
