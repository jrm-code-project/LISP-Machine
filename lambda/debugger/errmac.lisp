;;; -*- Mode:LISP; Package:EH; Base:10; Readtable:CL -*-

;; Condition and error related macros used by the system.
;; Used to be in SYS2; LMMAC

;; rewritten Mly 13-Aug-85 to avoid crufty catch-continuation obfuscation

;;>> Many of these guys use up FAR too much stack (regpdl) space -- esp in view
;; of the constraints the  places on stack-frame size.  There are other ways
;; of doing this stuff, which perhaps should be done later...

(defmacro condition-case (variables body-form &body clauses)
  "Execute BODY-FORM with conditions handled according to CLAUSES.
Each element of CLAUSES is a clause like those used in CASE.
It specifies one or more condition names, and what to do if they are signalled.

If any of the conditions specified in the clauses happens during BODY-FORM,
it is handled by throwing to this level and executing the matching clause.
Within the clause, the first of VARIABLES is bound to the condition-object
that was signaled.
The values of the last form in the clause are returned from CONDITION-CASE.

If none of the conditions occurs, the values of BODY-FORM are returned
from CONDITION-CASE.

If there is a clause with keyword :NO-ERROR, it is executed after BODY-FORM
if conditions are NOT signaled.  During this clause, the variables VARIABLES
are bound to the values produced by BODY-FORM.  The values of the last form
in the clause are returned from CONDITION-CASE."
  ;; Teco madness.
  (declare (zwei:indentation 1 3 2 1))
  `(condition-case-if t ,variables ,body-form . ,clauses))

(defmacro condition-case-if (&environment env cond-form variables body-form &body clauses)
  "Like CONDITION-CASE, but establishes condition handlers only if COND-FORM evaluates non-NIL.
Refer to the documentation of CONDITION-CASE for more information."
  (declare (zwei:indentation 2 3 3 1))
  (multiple-value-bind (realclauses decls)
      (extract-declarations clauses nil nil env)
    (let* ((all-conditions
             (mapcan (lambda (clause)
                       (si::macro-type-check-warning 'condition-case-if (car clause))
                       (if (eq (car clause) ':no-error) nil
                         (if (consp (car clause))
                             (copy-list (car clause))
                             (list (car clause)))))
                     realclauses))
           (var (or (car variables) (gensym)))
           (no-error-clause (assq ':no-error realclauses))
           (tag (gensym)))
      (if (null (cdr all-conditions))
          (setq all-conditions (car all-conditions)))
      (if no-error-clause
          `(block ,tag
             (let ((,var (catch ',tag
                           (multiple-value-bind ,variables
                               (condition-bind-if ,cond-form
                                                  ((,all-conditions 'condition-case-throw ',tag))
                                 ,body-form)
                             (declare . ,decls)
                             ,@variables
                             (return-from ,tag
                               (progn . ,(cdr no-error-clause)))))))
               (declare . ,decls)
               (si::select-memq (send ,var :condition-names)
                 . ,(remq no-error-clause realclauses))))
        ;; should check for the case of (cdr variables) and warrn if so.
        `(block ,tag
           (let ((,var (catch ',tag
                         (condition-bind-if ,cond-form
                                            ((,all-conditions 'condition-case-throw ',tag))
                           (return-from ,tag ,body-form)))))
             (declare . ,decls)
             (si::select-memq (send ,var :condition-names)
               . ,realclauses)))))))

(defmacro condition-call (variables body-form &body clauses)
  "Execute BODY-FORM with conditions handled according to CLAUSES.
Each element of CLAUSES is a clause like those used in COND.
This virtual COND is executed whenever a condition is signaled within BODY-FORM.
If the predicate at the start of a clause evaluates to non-NIL,
the rest of the clause is used to handle the condition.
The values of the last form in the clause are returned from CONDITION-CALL.
The predicate, and the rest of the clause, can find the condition object
that was signaled in the value of the first VARIABLE.

If no predicate evaluates to non-NIL, the condition is not handled
at this level.  Previously established handlers then get a chance.

The predicates may be evaluated more than once, and should have no side-effects.
They are evaluated within the context where the condition was signaled
and are evaluated again after throwing back to this level.
The rest of the clause is evaluated only after throwing back to this level.

The values of BODY-FORM are returned from the CONDITION-CALL if condition
handling does not cause something else to happen.  However, if there is
a :NO-ERROR clause (a clause whose first element is :NO-ERROR) then it
is executed and its values are returned from the CONDITION-CALL.
In this clause, the VARIABLES are bound to the values of the BODY-FORM."
  (declare (zwei:indentation 1 3 2 1))
  `(condition-call-if t ,variables ,body-form . ,clauses))

(defmacro condition-call-if (&environment env cond-form variables body-form &body clauses)
  "Like CONDITION-CALL, but establishes the handlers only if COND-FORM evaluates non-NIL.
See the documentation of CONDITION-CALL for more information."
  (declare (zwei:indentation 2 3 3 1))
  (multiple-value-bind (realclauses decls)
      (extract-declarations clauses nil nil env)
    (let* ((ordinary-clauses (subset (lambda (clause) (neq (car clause) ':no-error))
                                     realclauses))
           (no-error-clause (assq ':no-error realclauses))
           (predicates (mapcar #'car ordinary-clauses))
           (var (or (car variables) (gensym)))
           (tag (gensym))
           (handler `(lambda (,var &rest ignore)
                       (declare . ,decls)
                       (if (or . ,predicates)
                           (throw ',tag ,var)))))
      (if no-error-clause
          `(block ,tag
             (let ((,var (catch ',tag
                           (multiple-value-bind ,variables
                               (condition-bind-if ,cond-form ((nil ,handler))
                                 ,body-form)
                             (declare . ,decls)
                             (return-from ,tag
                               (progn . ,(cdr no-error-clause)))))))
               (declare . ,decls)
               (cond . ,ordinary-clauses)))
        `(block ,tag
           (let ((,var (catch ',tag
                         (condition-bind-if ,cond-form ((nil ,handler))
                           (return-from ,tag
                             ,body-form)))))
             (declare . ,decls)
             (cond . ,ordinary-clauses)))))))

(defmacro condition-bind-if (cond-form handlers &body body)
  "Execute BODY, with condition handlers HANDLERS in effect iff COND-FORM evals non-NIL.
If COND-FORM's value is non-NIL, this acts just like CONDITION-BIND.
Otherwise, BODY is evaluated as if it were in a PROGN, with no condition handlers."
  (let* ((vars (mapcar (lambda (ignore) (gensym)) handlers))
         (var1 (gensym))
         (inside `(with-list* (,var1 ,@vars eh:condition-handlers)
                    (let-if ,cond-form
                            ((eh:condition-handlers ,var1))
                      . ,body))))
    (do ((vs (reverse vars) (cdr vs))
         (hs (reverse handlers) (cdr hs)))
        ((null vs))
      (setq inside
            `(with-list (,(car vs) ',(car (car hs)) . ,(cdar hs))
               ,inside)))
    inside))

(defmacro condition-bind (handlers &body body)
  "Execute BODY with condition handlers HANDLERS in effect.
Each element of HANDLERS is a list of at least two elements:
 (CONDITIONS FUNCTION EXTRA-ARGUMENTS...).
CONDITIONS is not evaluated, and should be a condition name,
 a list of condition names, or NIL meaning all possible conditions.
FUNCTION is evaluated before BODY is entered to get a function to call
 to handle the condition(s); EXTRA-ARGUMENTS are evaluated then too.
When a one of the specified conditions is signaled, FUNCTION is called
 with arguments of the condition object followed by the EXTRA-ARGUMENTS.
FUNCTION should return two values.  If the first value is NIL,
the condition has not really been handled.  Otherwise, the two
values of FUNCTION will be returned from SIGNAL.
The conditions specified by CONDITIONS do not always have to be handled;
they are an initial filter that determines whether FUNCTION will be called.
Once function is called, it can then decide whether it will handle this SIGNAL."
  `(condition-bind-if t ,handlers . ,body))

(defmacro condition-bind-default-if (cond-form handlers &body body)
  "Execute BODY with default condition handlers HANDLERS in effect iff COND-FORM evals non-NIL.
Like CONDITION-BIND-IF except the condition handlers go on
the default handler list, EH:CONDITION-DEFAULT-HANDLERS, rather than
on the regular handler list.  The two lists work just the same
except that the default list is searched after the entire regular list."
  (let* ((vars (mapcar (lambda (ignore) (gensym)) handlers))
         (var1 (gensym))
         (form `(with-list* (,var1 ,@vars eh:condition-default-handlers)
                  (let-if ,cond-form
                          ((eh:condition-default-handlers ,var1))
                    . ,body))))
    (do ((vs (reverse vars) (cdr vs))
         (hs (reverse handlers) (cdr hs)))
        ((null vs))
      (setq form `(with-list (,(car vs) ',(car (car hs)) . ,(cdar hs))
                    ,form)))
    form))

(defmacro condition-bind-default (handlers &body body)
  "Execute BODY with default condition handlers HANDLERS in effect.
Like CONDITION-BIND except the condition handlers go on
the default handler list, EH:CONDITION-DEFAULT-HANDLERS, rather than
on the regular handler list.  The two lists work just the same
except that the default list is searched after the entire regular list."
  `(condition-bind-default-if t ,handlers . ,body))


(defmacro catch-error (body &optional (printflag t))
  "Execute body, trapping errors.  If no error, return the values of BODY.
If there is an error, return first value NIL, second non-NIL.
An error message is printed unless PRINTFLAG is specified and evaluates to NIL."
  (let ((tag (gensym)))
    `(block ,tag
       (catch ',tag
         (condition-bind ((error 'errset-handler ',tag ,printflag))
           (return-from ,tag
             ,body)))
       (values nil t))))

(defmacro errset (body &optional (printflag t))
  "Old maclisp loser; use CONDITION-CASE
Execute body, trapping errors.  If no error, return a 1-list of the value of BODY.
If there is an error, return NIL (or at least not a list.)
An error message is printed unless PRINTFLAG is specified and evaluates to NIL."
  (let ((tag (gensym)))
    ;; Returning the value of the catch is for the sake of ERR only.
    ;; If ERRSET-HANDLER actually runs, it throws NIL.
    `(block ,tag
       (values (catch ',tag
                 (condition-bind ((error 'errset-handler ',tag ,printflag))
                   (return-from ,tag (list ,body))))
               t))))

(defmacro err (&optional value-form flag)
  "Old Maclisp loser; use SIGNAL and CONDITION-CASE"
  (cond ((or value-form flag)
         `(let ((.value. ,value-form))
            (dolist (h eh:condition-handlers)
              (when (and (eq (car h) 'error)
                         (eq (cadr h) 'errset-handler))
                (throw (caddr h) .value.)))
            (ferror "~S" .value.)))
        (t '(progn (dolist (h eh:condition-handlers)
                     (when (and (eq (car h) 'error)
                                (eq (cadr h) 'errset-handler))
                       (throw (caddr h) nil)))
                   (error "")))))
(compiler:make-obsolete err "use SIGNAL and CONDITION-CASE")

(defmacro ignore-errors (&body body)
  "Evaluate BODY and return even if an error occurs.
If no error occurs, our first value is the first value of the last form in BODY,
 and our second value is NIL.
If an error does occur, our first value is NIL and our second value is T.
Dangerous errors such as running out of memory are not caught."
  (let ((tag (gensym)))
    `(block ,tag
       (catch ',tag
         (condition-bind ((error 'ignore-errors-handler ',tag))
           (return-from ,tag (values (progn . ,body) nil))))
       (values nil t))))

(defmacro error-restart ((condition format-string . format-args) &body body)
  "Execute BODY, with a restart for CONDITION in effect that will try BODY over.
FORMAT-STRING and FORMAT-ARGS are for the debugger to print a description
of what this restart is for, so the user can decide whether to use it.
They are all evaluated each time around the loop, before doing BODY.

If the user chooses to go to the restart we provide, it throws back to
the loop and BODY is executed again.  If BODY returns normally, the values
of the last form in BODY are returned from the ERROR-RESTART."
  (let ((tag (gensym)))
    `(block ,tag
       (tagbody
        ,tag
           (with-list (,tag ,format-string . ,format-args)
             (catch ,tag
               (with-list (,tag
                                 ',condition ,tag t ,tag
                                 'catch-error-restart-throw ,tag)
                 (with-list* (eh:condition-resume-handlers
                                     ,tag
                                     eh:condition-resume-handlers)
                   (return-from ,tag (progn . ,body)))))
             (go ,tag))))))

(defmacro error-restart-loop ((condition format-string . format-args) &body body)
  "Execute BODY over and over, with a restart for CONDITION in effect.
FORMAT-STRING and FORMAT-ARGS are for the debugger to print a description
of what this restart is for, so the user can decide whether to use it.
They are all evaluated each time around the loop, before doing BODY.

If the user chooses to go to the restart we provide, it throws back to
 the loop and loops around again.
The loop is wrapped in a block named NIL, so if body does a RETURN those values
 are returned from ERROR-RESTART-LOOP.
Otherwise (if BODY completes normally) it also loops around."
  (let ((tag (gensym)))
    `(block nil
       (tagbody
        ,tag
           (with-list (,tag ,format-string . ,format-args)
             (catch ,tag
               (with-list (,tag
                                 ',condition ,tag t ,tag
                                 'catch-error-restart-throw ,tag)
                 (with-list* (eh:condition-resume-handlers
                                     ,tag
                                     eh:condition-resume-handlers)
                   . ,body))))
           (go ,tag)))))

(defmacro catch-error-restart ((condition format-string . format-args) &body body)
  "Provide a restart for CONDITION if signaled within BODY.
FORMAT-STRING and FORMAT-ARGS are for the debugger to print a description
of what this restart is for, so the user can decide whether to use it.
They are all evaluated when the CATCH-ERROR-RESTART is entered.
If the user chooses to go to the restart we provide,
 CATCH-ERROR-RESTART returns NIL as first value and a non-NIL second value.
If CATCH-ERROR-RESTART is exited normally, it returns the values
 of the last form in BODY."
  (let ((tag (gensym)))
    `(block ,tag
       (with-list (,tag ,format-string . ,format-args)
         (catch ,tag
           (with-list (,tag
                             ',condition ,tag t ,tag
                             'catch-error-restart-throw ,tag)
             (with-list* (eh:condition-resume-handlers
                                 ,tag eh:condition-resume-handlers)
               (return-from ,tag (progn . ,body)))))
         (values nil t)))))

(defmacro catch-error-restart-if (cond-form (condition format-string . format-args) &body body)
  "Provide a restart for CONDITION if signaled within BODY, if COND-FORM evals non-NIL.
FORMAT-STRING and FORMAT-ARGS are for the debugger to print a description
of what this restart is for, so the user can decide whether to use it.
They are all evaluated when the CATCH-ERROR-RESTART-IF is entered.
If the user chooses to go to the restart we provide,
 CATCH-ERROR-RESTART-IF returns NIL as first value and a non-NIL second value.
If CATCH-ERROR-RESTART-IF is exited normally, it returns the values
 of the last form in BODY."
  (let ((tag (gensym)))
    `(block ,tag
       (with-list (,tag ,format-string . ,format-args)
         (catch ,tag
           (with-list (,tag
                             ',condition ,tag t ,tag
                             'catch-error-restart-throw ,tag)
             (with-list* (,tag
                                ,tag eh:condition-resume-handlers)
               (let-if ,cond-form
                       ((eh:condition-resume-handlers ,tag))
                 (return-from ,tag (progn . ,body))))))
         (values nil t)))))

(defmacro catch-error-restart-explicit-if (cond-form
                                           (condition proceed-type format-string . format-args)
                                           &body body)
  "Provide a PROCEED-TYPE resume handler for CONDITION if signaled within BODY,
  if COND-FORM evals non-NIL.
PROCEED-TYPE, like CONDITION, is not evaluated.
FORMAT-STRING and FORMAT-ARGS are for the debugger to print a description
of what this restart is for, so the user can decide whether to use it.
They are all evaluated when the CATCH-ERROR-RESTART-IF is entered.
If the user chooses to go to the restart we provide,
 CATCH-ERROR-RESTART-IF returns NIL as first value and a non-NIL second value.
If CATCH-ERROR-RESTART-IF is exited normally, it returns the values
 of the last form in BODY."
  (let ((tag (gensym)))
    `(block ,tag
       (with-list (,tag ,format-string . ,format-args)
         (catch ,tag
           (with-list (,tag
                             ',condition ',proceed-type t ,tag
                             'catch-error-restart-throw ,tag)
             (with-list* (,tag
                                ,tag eh:condition-resume-handlers)
               (let-if ,cond-form
                       ((eh:condition-resume-handlers ,tag))
                 (return-from ,tag (progn . ,body))))))
         (values nil t)))))

(defmacro condition-resume (handler &body body)
  "Provide a resume handler for conditions signaled within BODY.
Each resume handler applies to certain conditions, and is named by a keyword.
The error system sees which resume handlers can apply to the condition being handled,
 and includes their names (keywords) in the available proceed-types.
If a condition handler or the debugger elects to proceed with a proceed-type
 which was supplied by a resume handler, the resume handler is called.
It should always do a throw; it should not return to its caller.

HANDLER is evaluated on entry to the CONDITION-RESUME-IF.  The value should
look like this:
 (CONDITION-NAMES PROCEED-TYPE PREDICATE (FORMAT-STRING FORMAT-ARGS...) FUNCTION EXTRA-ARGS...)
CONDITION-NAMES is as for CONDITION-BIND.  It says which conditions to consider applying to.
PROCEED-TYPE is a keyword that identifies the purpose of this resume handler.
 A resume handler is only considered for use when an attempt is made to
 proceed with this PROCEED-TYPE.
PREDICATE is a function of one arg (a condition object) which decides
 whether this resume handler is really applicable.
 PREDICATE can be T if you don't want to test anything.
FORMAT-STRING and FORMAT-ARGS are for the debugger to print a description
of what this resume handler is for, so the user can decide whether to use it.
FUNCTION is the actual resume handler function.  Its arguments are
FUNCTION is the actual resume handler function.  Its arguments are
 the condition object, the EXTRA-ARGS, and any any other values supplied by caller.

CONDITION-RESUME does not do a CATCH.
It simply establishes the resume handler and executes the body."
  ;;>> should do some validity verification on handler
  `(with-stack-list* (eh:condition-resume-handlers ,handler eh:condition-resume-handlers)
     . ,body))

(defmacro condition-resume-if (cond-form handler &body body)
  "Like CONDITION-RESUME, but provide the resume handler only if COND-FORM evals non-NIL."
  (let ((tag (gensym)))
    ;;>> should do some validity verification on handler
    `(with-stack-list* (,tag ,handler eh:condition-resume-handlers)
       (let-if ,cond-form
               ((eh:condition-resume-handlers ,tag))
         . ,body))))

(defun compilation-eq-safe-p (thing)
  "Returns T of THING, appearing in two different places in code, will be EQ among
all those places after the code is compiled."
  (typep thing '(or number symbol null character)))

(defun make-proceed-clauses (clauses)
  "Make clauses that will match with proceed types which can be non-atoms.
This should be used for macros which allow non-atomic proceed-types, and use
a CASE statement for handling the proceed types with the clauses.

The proceed types are returned in order of their appearance in CLAUSES."
  (declare (values new-clauses proceed-types))
  (let ((new-clauses '())
        (proceed-types '()))
    (flet ((new-case (case)
             (if (compilation-eq-safe-p case) case (gensym))))
      (dolist (clause clauses)
        (let ((cases (car clause)))
          (typecase cases
            (cons
             (let ((new-cases (mapcar #'new-case cases)))
               (setq proceed-types (append proceed-types new-cases))
               (push (cons new-cases (cdr clause)) new-clauses)))
            (t
             (let ((new-case (new-case cases)))
               (setq proceed-types (nconc proceed-types (ncons new-case)))
               (push (cons new-case (cdr clause)) new-clauses)))))))
    (values (nreverse new-clauses) proceed-types)))

;;>> If you don't know what this macro does, if you can't read the documentation,
;;>>  if you don't have any idea of what the error system is how it works, don't go
;;>>  changing this macro.
(defmacro signal-proceed-case ((variables . signal-args) &body clauses)
  "Signal a condition and provide a CASE for proceed-types in case it is handled.
The SIGNAL-ARGS are evaluated and passed to SIGNAL.  That is how the condition is signaled.
The VARIABLES are bound to all but the first of the values returned by SIGNAL.
 The first value is used to select one of the CLAUSES with a CASE.
 The selected clause is executed and its values are returned.
SIGNAL is called with a :PROCEED-TYPES argument constructed by examining
 the cars of the CLAUSES.
If the condition is not handled, SIGNAL returns NIL.  If there is a clause
for NIL, it is run.  Otherwise, SIGNAL-PROCEED-CASE returns NIL."
  (let ((proceed-type-variable (gensym))
        (proceed-types-in-signal-args))
    (multiple-value-bind (new-clauses proceed-types)
        (make-proceed-clauses clauses)
      (do ((sa (cdr signal-args) (cddr sa)))
          ((null sa))
        (if (si:member-equal (car sa) '(':proceed-types :proceed-types))
            (setq proceed-types-in-signal-args (cdr sa))))
      `(multiple-value-bind (,proceed-type-variable . ,variables)
           (signal (make-condition ,@signal-args)
                   :proceed-types ,(if proceed-types-in-signal-args
                                       (car proceed-types-in-signal-args)
                                     `',(delq nil proceed-types)))
         ,proceed-type-variable
         ,@(if (null new-clauses) variables)
         (case ,proceed-type-variable
           . ,new-clauses)))))

;;;; ASSERT, CHECK-TYPE, CHECK-ARG

;;>> This one could deal with some improvement.
;;>>  (more particularly, the failed-assertion condition needs a :retry proceed type)
(defmacro assert (test-form &optional places (format-string "Assertion failed.") &rest args)
  "Signals an error if TEST-FORM evals to NIL.
PLACES are SETF'able things that the user should be able to change when proceeding.
Typically they are things used in TEST-FORM.
Each one becomes a proceed-type which means to set that place.
FORMAT-STRING and ARGS are passed to FORMAT to make the error message."
  (declare (arglist test-form &optional places format-string &rest args))
  (if (null places)
      `(or ,test-form
           (error 'eh::failed-assertion
                  :places ()
                  :format-string ,format-string
                  :format-args (list . ,args)))
    ;; Need this to deal with non-variable place forms (the most useful kind...)
    (multiple-value-bind (new-clauses proceed-types)
        (make-proceed-clauses (mapcar (lambda (place)
                                        `((,place) (setf ,place .value.)))
                                      places))
      `(do () (,test-form)
         (signal-proceed-case ((.value.) 'eh::failed-assertion
                                         :places ',places :proceed-types ',proceed-types
                                         :proceed-type-place-alist ',(pairlis proceed-types places)
                                         :format-string ,format-string
                                         :format-args (list . ,args))
           ,@new-clauses)))))

(defmacro check-type (place type &optional type-string)
  "Generate an error unless (TYPEP PLACE 'TYPE).
TYPE-STRING is a string to use in the error message, such as \"a list\".
If you omit it, it will be computed from TYPE."
  ;;>> This is conceivably losing in that the definition of TYPE may change after the
  ;;>>  typep call here is open-coded in some way.  The the typep exit test in
  ;;>>  check-type-internal may be oking something which is incompatible with the compiled
  ;;>>  version of type.
  `(progn (unless (typep ,place ',type)
            ;;>> Would be good to be able to instruct the compiler to put this code
            ;;>>  somewhere at the end of the compiled function so that the in the
            ;;>>  usual, non-error, case we avoid the extra branch around this.
            (setf ,place (check-type-internal
                           ,place ',place ',type ,@(and type-string `(',type-string)))))
          nil))

(defun check-type-internal (val place type &optional string)
  ;; wrong-type-value hacks  si::type-pretty-name  when reporting
  (declare (dbg:error-reporter))
  (do () ((typep val type) val)
    (setq val (signal-proceed-case ((v) 'dbg::wrong-type-value
                                        :place place :value val
                                        :type-specifier type
                                        :description string)
                (:new-value v)))))

;(defmacro check-type (arg-name type &optional type-string)
;  "Generate an error unless (TYPEP ARG-NAME 'TYPE).
;TYPE-STRING is a string to use in the error message, such as \"a list\".
;If you omit it, it will be computed from TYPE."
;  `(do () ((typep ,arg-name ',type))
;     (setq ,arg-name
;          (cerror '(:argument-value) nil 'wrong-type-argument
;                  "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
;                  ',type ,arg-name ',arg-name
;                  ,(or type-string `(si::type-pretty-name ',type))))))

;;; (CHECK-ARG <VARIABLE> <PREDICATE> <MESSAGE>), for example:
;;; (CHECK-ARG STRING STRINGP "a string") signals an error if STRING is not a string.
;;; The error signals condition :WRONG-TYPE-ARGUMENT with arguments
;;; which are STRINGP (the predicate), the value of STRING (the losing value),
;;; the name of the argument (STRING), and the string "a string".
;;; If you try to proceed and do not supply a valid string to replace it,
;;; the error happens again.
;;; The second form may be the name of a predicate function, or it may be a full
;;; predicate form, as in:
;;; (CHECK-ARG A (AND (NUMBERP A) (< A 10.) (> A 0.)) "a number from one to ten" ONE-TO-TEN)
;;; ONE-TO-TEN is a symbol for the "type" which the argument failed to be.
;;; It is used instead of the second argument (the predicate) when signalling the error,
;;; since the second argument is not a suitable symbol.
;;; The value returned by CHECK-ARG is the argument's (original or respecified) value.
;;; In general, the condition :WRONG-TYPE-ARGUMENT is signalled with arguments
;;;    (1) A symbol for the desired type (NIL if not supplied)
;;;    (2) The bad value
;;;    (3) The name of the argument
;;;    (4) A string for the desired type.
(defmacro check-arg (arg-name predicate type-string &optional error-type-name)
  "Generate error if the value of ARG-NAME doesn't satisfy PREDICATE.
PREDICATE is a function name (a symbol) or an expression to compute.
TYPE-STRING is a string to use in the error message, such as \"a list\".
ERROR-TYPE-NAME is a keyword that tells condition handlers what type was desired.
This macro is somewhat obsolete: you should probably be using CHECK-TYPE instead."
  (and (null error-type-name)
       (symbolp predicate)
       (setq error-type-name predicate))
  `(do-named t ()
      (,(if (symbolp predicate)
            `(,predicate ,arg-name)
          predicate)
       ,arg-name)
     (setq ,arg-name
           ;>> Blorge
           (cerror '(:argument-value) nil 'wrong-type-argument
                   "The argument ~2@*~A was ~1@*~S, which is not ~3@*~A."
                   ',error-type-name ,arg-name ',arg-name ',type-string))))

(deff-macro check-arg-type 'check-type)
(compiler:make-obsolete check-arg-type "the new name for this macro is CHECK-TYPE")

(defmacro check-system-parameter (parameter type-specifier reset-value)
  "Checks the variable PARAMETER to have the type specified.
If it doesn't, the user is offered to set the value to RESET-VALUE.
The value of the expression is always the (legal) value of PARAMETER."
  `(let ((.val. nil))
     (loop
       (setq .val. ,parameter)
       (if (typep .val. ',type-specifier)
           (return .val.)
         (zl:signal-proceed-case (() 'eh:bad-system-parameter
                                  :place ',parameter
                                  :value .val.
                                  :type-specifier ',type-specifier
                                  :reset-value (setq ,parameter ,reset-value))
           (:no-action))))))

(defmacro define-parameter-checker (function parameter type-specifier reset-value)
  "Defines a function which checks the variable PARAMETER to have the type specified.
If it doesn't, the user is offered to set the value to RESET-VALUE.
The functions always returns the (legal) value of PARAMETER."
  `(defun ,function ()
     (declare (dbg:error-reporter))
     (check-system-parameter ,parameter ,type-specifier ,reset-value)))
