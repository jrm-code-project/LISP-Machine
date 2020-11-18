;-*- Mode:LISP; Package:EH; Readtable:ZL; Base:8 -*-

;;>> put stuff in debugger;condition-flavors if you intend to Do It Right.

(export '(debugger-condition no-action-mixin proceed-with-value-mixin *abort-object*))

(defmacro defsignal-explicit (signal-name flavor &optional args &body init-options)
  "Define a signal name, which can be used in ERROR, SIGNAL, FERROR, CERROR.
SIGNAL-NAME is the signal name to be defined.
FLAVOR is the flavor of condition object to make,
 or a list (FLAVOR . ADDITIONAL-CONDITION-NAMES), where ADDITIONAL-CONDITION-NAMES
 is a list of condition-names to signal in addition to the flavor components.
 If you specify just a flavor name, the SIGNAL-NAME itself is used
 as the sole additional condition name.
 If you specify a one-element list (FLAVOR), there are no additional
 condition names, and you can optionally use :CONDITION-NAMES as one of
 the INIT-OPTIONS to control them dynamically.
DOCUMENTATION is a documentation string describing what this signal-name is for.
When SIGNAL-NAME is used as the first arg to MAKE-CONDITION, ERROR, SIGNAL, etc.
then you can refer to the remaining args with the arglist ARGS.
The argument names in that list may be used in the INIT-OPTIONS,
which are arguments to pass to MAKE-INSTANCE in addition to the flavor name."
  (declare (arglist signal-name flavor &optional args documentation
                    &body init-options))
  (let ((flavor (if (consp flavor) (car flavor) flavor))
        (documentation (if (stringp (car init-options)) (pop init-options)))
        (condition-names
          (or (cdr-safe flavor) (list signal-name))))
    `(progn (si:record-source-file-name ',signal-name 'defsignal)
            ,(if documentation
                 `(setf (documentation ',signal-name 'signal) ,documentation))
            (defun (:property ,signal-name make-condition-function) (ignore . ,args)
              ,documentation
              (declare (function-parent ,signal-name defsignal-explicit))
              ,(if condition-names
                   `(make-instance ',flavor ,@init-options
                                   :condition-names ',condition-names)
                 `(make-instance ',flavor ,@init-options))))))

(deff defsignal-format 'defsignal)

(defmacro defsignal (signal-name flavor args &body init-options)
  "Define a signal name, which can be used in ERROR, SIGNAL, FERROR, CERROR.
SIGNAL-NAME is the signal name to be defined.
FLAVOR is the flavor of condition object to make,
 or a list (FLAVOR . ADDITIONAL-CONDITION-NAMES), where ADDITIONAL-CONDITION-NAMES
 is a list of condition-names to signal in addition to the flavor components.
 If you specify just a flavor name, the SIGNAL-NAME itself is used
 as the sole additional condition name.
DOCUMENTATION is a documentation string describing what this signal-name is for.
When SIGNAL-NAME is used as the first arg to MAKE-CONDITION, ERROR, SIGNAL, etc.
then the remaining arguments are treated as a format string and format arguments.
In addition, the first few (not counting the string) can be given names,
which you specify as the list ARGS.
These names (moved into the keyword package) become messages
that can be sent to the condition object to get the corresponding values.
In addition, the values of the ARGS may be used in the INIT-OPTIONS,
which are additional arguments to pass to MAKE-INSTANCE."
  (declare (arglist signal-name flavor args &optional documentation &body init-options))
  (let ((flavor (if (consp flavor) (car flavor) flavor))
        (documentation (if (stringp (car init-options)) (pop init-options)))
        (condition-names
          (or (cdr-safe flavor) (list signal-name)))
        (properties (mapcar (lambda (sym)
                              `',(intern (symbol-name sym) si:pkg-keyword-package))
                            args)))
    `(progn (si:record-source-file-name ',signal-name 'defsignal)
            ,(if documentation
                 `(setf (documentation ',signal-name 'signal) ,documentation))
            (defun (:property ,signal-name make-condition-function)
                   (ignore format-string &optional ,@args &rest format-args)
              ,documentation
              (declare (function-parent ,signal-name defsignal))
              (make-instance ',flavor
                             ,@init-options
                             :property-list
                             (list . ,(mapcan 'list properties args))
                             :format-string format-string
                             :format-args
                             (list* ,@args (copy-list format-args))
                             :condition-names ',condition-names)))))

(defun make-condition (signal-name &rest args)
  "Create a condition object using flavor or signal name SIGNAL-NAME.
If SIGNAL-NAME is a signal name defined with DEFSIGNAL or DEFSIGNAL-EXPLICIT,
the ARGS are interpreted according to what was specified in the definition.

If SIGNAL-NAME is a flavor name, the ARGS are init options to pass to MAKE-INSTANCE."
  (if (typep signal-name 'instance)
      signal-name
    (apply (cond ((and (not (stringp signal-name))
                       (get signal-name 'make-condition-function)))
                 ((and (not (stringp signal-name))
                       (get signal-name 'si:flavor) #'make-instance))
                 (t #'make-condition-default))
           signal-name args)))

(defvar *make-condition-default-restrictive* NIL)

;Nothing reasonable will happen if format-string is not present, but
; at least it will not bomb out internal to the system on a wrong number of args.
(defun make-condition-default (signal-name &optional format-string &rest args)
  (declare (dbg:error-reporter))
  (COND (*make-condition-default-restrictive*
         (ferror "~S is not a known condition flavor or signal name"
                 signal-name))
        ('else
         (make-instance 'ferror
                        :condition-names (list signal-name)
                        :format-string format-string
                        :format-args (copy-list args)))))


(defvar error-identify-value (list nil)
  "This object is the first instance variable of every error object.
It is a fast way of identifying error objects.")

(defflavor condition ((condition-identify nil)
                      (si:property-list nil)
                      (condition-names nil)
                      (format-string nil)
                      (format-args nil))
           (si:property-list-mixin si:print-readably-mixin)
  (:initable-instance-variables condition-names format-string format-args)
  (:gettable-instance-variables condition-names format-string format-args)
  :ordered-instance-variables
  (:init-keywords :proceed-types)
  (:default-handler condition-data-plist-lookup)
  (:method-combination (:or :base-flavor-last
                            :default-handler
                            :debugger-command-loop)
                       (:progn :base-flavor-last
                               :initialize-special-commands)
                       (:pass-on (:base-flavor-last proceed-types)
                                 :user-proceed-types)
                       (:case :base-flavor-last
                              :proceed
                              :proceed-asking-user
                              :document-proceed-type
                              ;>> This is a kludge.  It should only be on trap,
                              ;>>  but some compile-flavor-methods lossage is getting in my
                              ;>>  way on this cold-load.  Excise it when convenient.
                              :proceed-ucode-with-args
                              ))
  (:outside-accessible-instance-variables condition-identify))

(defconst not-condition-flavors
          '(si:vanilla-flavor si:property-list-mixin si:print-readably-mixin
                              no-action-mixin proceed-with-value-mixin)
  "These condition flavors are excluded from the condition names of a condition.")

(defmethod (condition :init) (&rest ignore)
  (let ((tem (subset (lambda (symbol) (not (memq symbol not-condition-flavors)))
                     (dont-optimize (si::flavor-depends-on-all (si::%instance-flavor self))))))
    (setq condition-names
          (si::union-eq tem (if (cl:listp condition-names)
                                condition-names (list condition-names))))))

;;; The following four operations must be redefined
;;; because we do handle operations that vanilla-flavor's methods
;;; do not realize that we handle.
(defmethod (condition :which-operations) (&aux accum)
  (do ((l si:property-list (cddr l)))
      ((null l))
    (push (car l) accum))
  ;;>> blorge
  (nreconc accum (funcall #'(:method si:vanilla-flavor :which-operations)
                          :which-operations)))

(defmethod (condition :operation-handled-p) (operation)
  ;;>> blorge
  (or (si:memq-alternated operation si:property-list)
      (funcall #'(:method si:vanilla-flavor :operation-handled-p)
               :operation-handled-p operation)))

(defmethod (condition :get-handler-for) (operation)
  ;;>> blorge
  (or (funcall #'(:method si:vanilla-flavor :get-handler-for)
               :get-handler-for operation)
      (and (si:memq-alternated operation si:property-list)
           'condition-data-plist-lookup)))

(defmethod (condition :send-if-handles) (operation &rest args)
  ;;>> blorge
  (let ((handler (or (funcall #'(:method si:vanilla-flavor :get-handler-for)
                              :get-handler-for operation)
                     (and (si:memq-alternated operation si:property-list)
                          #'condition-data-plist-lookup))))
    (if handler (apply handler operation args))))

(defmethod (condition :set) (place &rest args)
  ;;>> blorge
  (let ((loc (get-location-or-nil place si:property-list)))
    (if loc
        (setf (contents loc) (car (last args)))
      (apply #'si:flavor-unclaimed-message :set place args))))

(defwrapper (condition :report) (ignore . body)
  `(let ((*print-level* error-message-prinlevel)
         (*print-length* error-message-prinlength))
     ;; If there is an error printing some object,
     ;; Just give up on that object.
     (condition-bind ((error 'condition-report-abort-printing))
       . ,body)))

(defun condition-report-abort-printing (condition)
  (when (memq ':abort-printing (send condition :proceed-types))
    :abort-printing))

(defmethod (condition :report) (stream)
  (apply #'format stream format-string (send self :format-args)))

;;; This is the default handler for format-string conditions.
;;; Any operation which is the same as the name of a property on the instance's plist
;;; just returns the value of the property.
;;; Anything else is an error, as usual.
(defun-method condition-data-plist-lookup condition (operation &rest args)
  ;;>> blorge
  (declare (error-reporter))
  (let* ((default '(()))
         (tem (getf si:property-list operation default)))
    (if (eq tem default)
        (apply #'si::flavor-unclaimed-message operation args)
      tem)))

(defprop condition (condition-identify) :dont-print-instance-variables)

(defflavor debugger-condition () (condition)
  (:documentation "A condition with this flavor mixed in will enter the debugger by default
unless it is otherwise handled."))
(defflavor error ((condition-identify error-identify-value)) (debugger-condition))

(defflavor no-action-mixin () ()
  (:required-flavors condition)
  :abstract-flavor
  (:method-combination (:case :base-flavor-last
                              :proceed
                              :proceed-asking-user
                              :document-proceed-type
                              ;>> This is a kludge.  It should only be on trap,
                              ;>>  but some compile-flavor-methods lossage is getting in my
                              ;>>  way on this cold-load.  Excise it when convenient.
                              :proceed-ucode-with-args)))
(defmethod (no-action-mixin :case :proceed-asking-user :no-action)
           (continuation ignore)
  "Simply proceed."
  (funcall continuation :no-action))

(defun errorp (object)
  "T if OBJECT is a condition-object representing an error condition.
This is equivalent to (TYPEP OBJECT 'ERROR) but faster."
  (and (instancep object)
       (location-boundp (locf (condition-condition-identify object)))
       (eq (condition-condition-identify object) error-identify-value)))

(defun condition-typep (condition-instance condition-name)
  "T if CONDITION-NAME is one of the condition names possessed by CONDITION-INSTANCE."
  (labels ((foo (names frob)
             (if (atom frob) (memq frob names)
               (case (car frob)
                 (and (dolist (c (cdr frob) t)
                        (when (not (foo names c)) (return nil))))
                 ((or cl:member zl:member) (dolist (c (cdr frob) nil)
                                             (when (foo names c) (return t))))
                 (not (not (foo names (cadr frob))))))))
    (foo (send condition-instance :condition-names) condition-name)))

(defmethod (condition :around :print-self) (continuation map args stream &rest ignore)
  (if *print-escape*
      (condition-bind ((error 'condition-report-abort-printing))
        (lexpr-funcall-with-mapping-table continuation map args))
    (send self :report stream)))

(defmethod (condition :reconstruction-init-plist) ()
  (let (accum)
    ;;>> blorge
    (with-self-variables-bound
      (dolist (var (or (get (type-of self) 'keyword-var-alist)
                       (setup-keyword-var-alist (type-of self))))
        ;; Mention certain vars only if non-NIL.
        (if (if (memq (cdr var) '(si:property-list format-string format-args))
                (symbol-value (cdr var))
              (boundp (cdr var)))
            (setq accum (list* (car var) (symbol-value (cdr var)) accum)))))
    accum))

(defun setup-keyword-var-alist (flavor-name)
  (let ((flavor (get flavor-name 'si:flavor))
        accum
        dont-mention)
    (dolist (component (dont-optimize (si:flavor-depends-on-all flavor)))
      (setq dont-mention (si::union-eq (get component ':dont-print-instance-variables)
                                       dont-mention)))
    (dolist (var (dont-optimize (si:flavor-all-instance-variables flavor)))
      (or (memq var dont-mention)
          (push (cons (intern (string var) si:pkg-keyword-package) var)
                accum)))
    (putprop flavor-name accum 'keyword-var-alist)
    accum))

(defmethod (condition :string-for-printing) ()
  (format:output nil (send self :report *standard-output*)))

(defmethod (condition :report-string) ()
  (format:output nil (send self :report *standard-output*)))

(defmethod (condition :print-error-message-prefix) (sg brief stream)
  (declare (ignore sg brief))
  (princ ">>CONDITION: " stream))

(defmethod (error :print-error-message-prefix) (sg brief stream)
  (declare (ignore sg brief))
  (princ ">>ERROR: " stream))

(defmethod (condition :maybe-clear-input) (stream)
  (send stream :clear-input))

(defmethod (condition :print-error-message) (sg brief stream)
  (send self :print-error-message-prefix sg brief stream)
  (send self :report stream)
  (terpri stream))

;;; Four values, which are used to set *ERROR-LOCUS-FRAME*, *CURRENT-FRAME*,
;;; *INNERMOST-VISIBLE-FRAME* and *INNERMOST-FRAME-IS-INTERESTING*.
(defmethod (condition :find-current-frame) (sg)
  (declare (values error-locus-frame current-frame
                   innermost-visible-frame innermost-frame-is-interesting))
  (let ((*innermost-visible-frame* (sg-ap sg))
        cf)
    (setq cf *innermost-visible-frame*)
    (do ((rp (sg-regular-pdl sg)))
        ((not (let* ((f (rp-function-word rp cf))
                     (fn (function-name f)))
                (or (and (compiled-function-p f)
                         (assq 'error-reporter (fef-debugging-info f)))
                    (and (symbolp fn)
                         (get fn ':error-reporter))))))
      (and (eq (function-name (rp-function-word rp cf))
               'signal-condition)
           (eq (function-name (rp-function-word rp (sg-next-active sg cf)))
               'fh-applier)
           (eq (function-name (rp-function-word rp (sg-previous-nth-active sg cf -2)))
               'foothold)
           (return (setq cf (sg-previous-nth-active sg cf -3))))
      (setq cf (sg-next-active sg cf)))
    (values cf
            ;;>> this loses, for example, if an interpreter function (say return)
            ;;>>  signals an error (say no such block seen)
            ;;>> I suppose that if an uninteresting (for non-debugger reasons) function
            ;;>>  signals an error, then it should be considered interesting.
            (sg-out-to-interesting-active sg cf)
            *innermost-visible-frame*
            nil)))

(defmethod (condition :debugger-command-loop) (sg &rest ignore)
  (command-loop sg self))

(defvar *bug-report-recipient-system* "LISPM" "Rebind this if you want to send a bug to a differentmailing list")

(defmethod (condition :bug-report-recipient-system) ()
  (cond ((boundp '*error-sg*)
         (multiple-value-bind (value boundp)
             (symeval-in-stack-group '*bug-report-recipient-system* *error-sg*)
           (if (and boundp
                    value)
               value
             "LISPM")))
        (t "LISPM")))

(defparameter *default-bug-report-frames* 5
  "Default number of frames to include in a bug report backtrace.")

(defmethod (condition :bug-report-description) (stream &optional n-frames)
  (send self :print-error-message *error-sg* nil stream)
  (format stream "Backtrace from the debugger:")
  (let ((*standard-output* stream)
        total-frames)
    (do ((frame *error-locus-frame* (sg-next-active *error-sg* frame))
         (i 0 (1+ i)))
        ((null frame) (setq total-frames i)))
    (do ((frame *error-locus-frame* (sg-next-active *error-sg* frame))
         (i 0 (1+ i)))
        ((null frame))
      (cond ((< i (or n-frames *default-bug-report-frames*))
             (show-frame-for-bug-message *error-sg* frame))
            ((= i (or n-frames *default-bug-report-frames*))
             (format t "~%~%Remainder of stack:~2%")
             (show-frame-briefly-for-bug-message *error-sg* frame))
            ((< i (+ 10. (or n-frames *default-bug-report-frames*)))
             (show-frame-briefly-for-bug-message *error-sg* frame))
            ((> i (- total-frames 10.))
             (show-frame-briefly-for-bug-message *error-sg* frame))
            ((= i (+ 10. (or n-frames *default-bug-report-frames*)))
             (format t "~&..."))))))

(defmethod (condition :debugging-condition-p) ()
  nil)
(defmethod (condition :dangerous-condition-p) ()
  nil)

;;;; Proceeding, or trying to.

(defvar-resettable condition-resume-handlers nil nil
  "List of active RESUME handlers.  A resume handler has a keyword to name it
and also says which conditions it is applicable to.
A condition handler, or the debugger, can say to resume using a specified keyword
and the innermost resume handler for that keyword which is applicable to the
condition being handled will be run.  It is supposed to do a throw.
Elements of this list look like
 (CONDITION-NAMES KEYWORD PREDICATE (FORMAT-STRING FORMAT-ARGS...) HANDLER EXTRA-ARGS...).
CONDITION-NAMES is a condition name or list of such, or NIL.
 This is as for CONDITION-HANDLERS.
KEYWORD is the resume-type.
PREDICATE is called with one argument, the condition,
 and if the result is non-NIL, this handler is active for this condition.
 PREDICATE can be T, meaning handler is always active if CONDITION-NAMES match.
FORMAT-STRING and FORMAT-ARGS are used to print a message saying what
 this resume handler is intended for.
HANDLER is what to do if the debugger actually says to use this resume handler.
EXTRA-ARGS are passed to HANDLER, after the condition object which is its first arg.
Any additional values returned from the condition handler or from
the :PROCEED-ASKING-USER operation (not including the proceed-type)
are also passed to HANDLER, following the EXTRA-ARGS.

An element can also be just T, which means don't look past here when
looking for a resume handler for an unhandled SIGNAL on a non-error condition.")

;;; If the user has defined a case of :PROCEED instead of :PROCEED-ASKING-USER,
;;; call it.
(defmethod (condition :proceed-asking-user) (proceed-type continuation read-object-function)
  (declare (ignore read-object-function))
  (if (send self :proceed :operation-handled-p proceed-type)
      (let ((values (multiple-value-list
                      (send self :proceed proceed-type))))
        (if (car values)
            ;; If :PROCEED handler returns NIL, don't really proceed.
            (apply continuation values)))
    (funcall continuation proceed-type)))

;;; If a handler sends a :PROCEED message, do nothing (unless user has defined a :case);
;;; however, if there are no args, call the appropriate :PROCEED-ASKING-USER method if any.
(defmethod (condition :proceed) (&rest args &aux (proceed-type (car args)))
  (if (cdr args)
      (values-list args)
    (if (send self :proceed-asking-user
              :operation-handled-p proceed-type)
        (send self :proceed-asking-user proceed-type 'values 'read-object)
      proceed-type)))

(defmethod (condition :user-proceed-types) (proceed-types)
  proceed-types)

(defmethod (condition :document-proceed-type) (proceed-type stream
                                               &optional (resume-handlers condition-resume-handlers))
  (let ((string (or (send self :proceed-asking-user :case-documentation proceed-type)
                    (send self :proceed :case-documentation proceed-type))))
    (if string (princ string stream)
      (do ((handler-list resume-handlers (cdr handler-list))
           (h))
          ((eq handler-list (cdr handler-list)))
        (setq h (car handler-list))
        (and (consp h)
             (cond ((null (car h)) t)
                   ((not (consp (car h)))
                    (memq (car h) condition-names))
                   (t (dolist (c (car h))
                        (if (memq c condition-names) (return t)))))
             (eq (cadr h) proceed-type)
             (or (eq (caddr h) t) (funcall (caddr h) self))
             (return (apply #'format stream (fourth h))))))))

;(defmethod (condition :ucode-proceed-types) () nil)

;;; Make sure we have a combined method for this,
;;; so that the :WHICH-OPERATIONS suboperation works.
(defmethod (condition :special-command) (&rest ignore) nil)

(defmethod (condition :initialize-special-commands) () nil)

(defflavor proceed-with-value-mixin () ()
  (:required-flavors condition)
  :abstract-flavor
  (:method-combination (:case :base-flavor-last
                              :proceed
                              :proceed-asking-user
                              :document-proceed-type
                              ;>> This is a kludge.  It should only be on trap,
                              ;>>  but some compile-flavor-methods lossage is getting in my
                              ;>>  way on this cold-load.  Excise it when convenient.
                              :proceed-ucode-with-args)))

(defmethod (proceed-with-value-mixin :case :proceed-asking-user :new-value)
           (continuation read-object-function)
  "Return a value; the value of an expression you type."
  (funcall continuation :new-value
           (funcall read-object-function :eval-read
                    "Form to evaluate and return: ")))


;;;; User entries to signaling.

(defun signal (signal-name-or-condition-object &rest args
               &key &optional (proceed-types nil proceed-types-p) &allow-other-keys)
  "Signal a condition, allowing handlers to proceed with the specified PROCEED-TYPES.
SIGNAL-NAME-OR-CONDITION-OBJECT may be a condition object to be signaled,
or it and ARGS may be args to give to MAKE-CONDITION to create such an object.
Whether ARGS are needed for MAKE-CONDITION or not, they are also searched
for the keyword argument :PROCEED-TYPES, whose value should be a list
of proceed-types the caller offers to handle.  If the arguments for MAKE-CONDITION
for the particular SIGNAL-NAME-OR-CONDITION-OBJECT do not fit with this,
you should call MAKE-CONDITION yourself and pass the result.
If you do not specify the :PROCEED-TYPES argument, a list of all the
proceed-types which the specific flavor of condition can handle is used."
  (declare (error-reporter))
  (let ((condition
          (if (typep signal-name-or-condition-object 'instance)
              signal-name-or-condition-object
            (apply #'make-condition signal-name-or-condition-object args))))
    (signal-condition
      condition
      (if proceed-types-p
          (if (or (consp proceed-types) (null proceed-types))
              proceed-types
            (list proceed-types))
        (si::union-eq (send condition :proceed-asking-user :which-operations)
                      (send condition :proceed :which-operations))))))

;;; (ERROR <message> &optional <object> <interrupt>)
;;; is for Maclisp compatibility.  It makes the error message
;;; out of <message> and <object>, and the condition out of <interrupt>'s
;;; CONDITION-NAME property.  The error is proceedable if
;;; <interrupt> is given.
(defun error (signal-name-or-condition-object &rest args)
  "Signal a condition, not providing any way to proceed.
If no handler throws or uses any of the resume handlers,
the debugger is always entered, even if the condition is not an error.
SIGNAL-NAME-OR-CONDITION-OBJECT may be a condition object to be signaled,
or it and ARGS may be args to give to MAKE-CONDITION to create such an object."
  (declare (error-reporter))
  (cond ((stringp signal-name-or-condition-object)
         (signal-condition
           (make-condition 'ferror :format-string signal-name-or-condition-object
                                   :format-args args)
           nil t))
        ((and (symbolp signal-name-or-condition-object)
              (not (get signal-name-or-condition-object 'si:flavor))
              (not (get signal-name-or-condition-object 'make-condition-function)))
         (signal-condition
           (make-condition 'maclisp-error
                           signal-name-or-condition-object
                           (car args)
                           (cadr args))
           (if (get (cadr args) 'condition-name)
               '(:new-value))
           t))
        (t
         (signal-condition
           (apply #'make-condition signal-name-or-condition-object args)
           nil t))))

(defflavor multiple-cerror (proceed-alist) (error)
  :inittable-instance-variables
  (:documentation
    "A type of error which just provides a number of simple ways to proceed.
Like common-lisp CERROR with more than one proceed-type.
Usually used in conjunction with the macro MULTIPLE-CERROR"))

(defmethod (multiple-cerror :around :proceed-asking-user)
           (cont mt args &optional subop arg1 arg2)
  (case subop
    (:which-operations
     (nconc (mapcar #'car proceed-alist)
            (around-method-continue cont mt args)))
    (:operation-handled-p
     (not (not (or (assq arg1 proceed-alist)
                   (around-method-continue cont mt args)))))
    (:get-handler-for
     ;; Yow! lexical closures!
     (if (assq arg1 proceed-alist)
         #'(lambda (continuation ignore) (funcall continuation arg1))
       (around-method-continue cont mt args)))
    (:case-documentation
     (let ((tem (assq arg1 proceed-alist)))
       (cond ((not tem)
              (around-method-continue cont mt args))
             ((cdr tem)
              (format t (cdr tem) format-args))
             (t "Proceeds."))))                 ;what else is there say?
    (t
     (if (assq arg1 proceed-alist)
         (funcall arg2 arg1)
       (around-method-continue cont mt args)))))

;; Copied from LAD: RELEASE-3.DEBUGGER; EHF.LISP#290 on 26-Mar-87 16:05:14
(defmacro multiple-cerror (condition-names
                           condition-properties
                           (error-format-string &rest format-args)
                           &body proceed-clauses)
  "Signal an error with condition, passing MAKE-CONDITIONS-ARGS to MAKE-CONDITION
ERROR-FORMAT-STRING and FORMAT-ARGS give a message to be printed describing the error.
PROCEED-CLAUSES are a list of different ways of proceeding from the error which will
 be offered to the user. Each is of the form (documentation-string statments ...)
 where documentation-string describes what proceeding the error in this way will do.
The values of the last statement in the clause which is used to proceed are returned."
  (let (proceed-alist
        clauses)
    (dolist (c proceed-clauses)
      (let ((gensym (gensym)))
        (push `(cons ',gensym ,(car c)) proceed-alist)
        (push (cons gensym (cdr c)) clauses)))
    (setq clauses (nreverse clauses))           ;gratuitous, but makes disassembly better
    `(signal-proceed-case (()
                           'multiple-cerror
                           :condition-names ,condition-names
                           :format-string ,error-format-string
                           :format-args (list . ,format-args)
                           :proceed-alist (list . ,proceed-alist)
                           :property-list (list . ,condition-properties))
       . ,clauses)))


(defflavor cerror (continue-format-string) (ferror)
  :gettable-instance-variables :initable-instance-variables)

(defmethod (cerror :case :proceed-asking-user :continue) (continuation ignore)
  (funcall continuation :continue))

(defmethod (cerror :case :document-proceed-type :continue) (stream &optional ignore)
  (apply #'format stream continue-format-string format-args))

(defun cerror (proceedable-flag unused &optional signal-name format-string &rest args)
  "In its common-lisp meaning, PROCEEDABLE-FLAG is a string describing the action to be
 taken if the error is proceeded, UNUSED is a format string describing the error, and
 the other arguments are used as other arguments to FORMAT.
 NIL is always returned.
Otherwise, report a simple correctable error, using FORMAT and ARGS to print the message.
SIGNAL-NAME is a signal name or condition flavor name to be signalled,
or else a condition name to include in the signal, or NIL for none in particular.
 Actually, FORMAT-STRING and ARGS are just passed along to MAKE-CONDITION
 and SIGNAL-NAME controls how they are interpreted there.
PROCEEDABLE-FLAG = :YES means allow proceed-type :NO-ACTION,
 which returns NIL from CERROR.
PROCEEDABLE-FLAG = T means allow proceed-type :NEW-VALUE,
 and the value is returned from CERROR.
Any other non-NIL value for PROCEEDABLE-FLAG is either a proceed-type
 or a list of proceed-types."
  (declare (error-reporter))
  (if (stringp proceedable-flag)
      ;; common-lisp cerror
      (progn (signal 'cerror :proceed-types '(:continue)
                     :continue-format-string proceedable-flag
                     :format-string unused
                     :format-args (list* signal-name format-string args))
             nil)
    (multiple-value-bind (nil value)
        (signal-condition
          (apply #'make-condition signal-name format-string args)
          (case proceedable-flag
            ((t) '(:new-value))
            ((nil) nil)
            (:yes '(:no-action))
            (t (if (atom proceedable-flag) (list proceedable-flag) proceedable-flag))))
      value)))


;; Brand S BD.
(defun fsignal (format-string &rest args)
  (declare (error-reporter))
  (check-type format-string string)
  (apply 'cerror "Simply proceed." format-string args))

(defun ferror (signal-name &optional format-string &rest args)
  "Report an uncorrectable error, using FORMAT-STRING and ARGS to print the message.
SIGNAL-NAME is a signal name or condition flavor name to be signalled,
or else a condition name to include in the signal, or NIL for none in particular."
  (declare (error-reporter))
  (signal-condition
    (if (stringp signal-name)
        ;; Symbolics calling sequence has no condition; 1st arg is really the format string.
        (make-condition 'ferror :format-string signal-name
                        :format-args (cons format-string args))
      (apply #'make-condition signal-name format-string args))
    nil t))

(defun (maclisp-error make-condition-function) (ignore message object interrupt)
  (make-instance 'ferror
                 :format-string (if object "~S ~A" "~*~A")
                 :format-args (list object message)
                 :condition-names (list (get interrupt 'condition-name))))

(defflavor ferror ()
           (proceed-with-value-mixin no-action-mixin error))

(defvar eh-ready nil
  "NIL until call to first level error handler after it has been preset.
Normal error processing has no chance until this is set")


;;;; Lower levels of signaling.

(defvar-resettable condition-handlers nil nil
  "List of active condition handlers.  Each element is (CONDITION-NAMES FUNCTION).
CONDITION-NAMES is either a condition name, a list of names, or NIL for all conditions.")

(defvar-resettable condition-default-handlers nil nil
  "List of active default condition handlers.  Each element is (CONDITION-NAMES FUNCTION).
CONDITION-NAMES is either a condition name, a list of names, or NIL for all conditions.
The handlers on this list are tried after all of CONDITION-HANDLERS.")

(defun invoke-handlers (condition condition-names &aux values)
  (do ((handler-list condition-handlers (cdr handler-list))
       (h))
      ((null handler-list) nil)
    (setq h (car handler-list))
    (when (cond ((null (car h)) t)
                ((not (consp (car h)))
                 (memq (car h) condition-names))
                (t (dolist (c (car h))
                     (if (memq c condition-names) (return t)))))
      (setq values
            (multiple-value-list
              (let ((condition-handlers (cdr handler-list)))
                (apply (cadr h) condition (cddr h)))))
      (if (car values) (return-from invoke-handlers values))))
  (do ((handler-list condition-default-handlers (cdr handler-list))
       (h))
      ((null handler-list) nil)
    (setq h (car handler-list))
    (when (cond ((null (car h)) t)
                ((not (consp (car h)))
                 (memq (car h) condition-names))
                (t (dolist (c (car h))
                     (if (memq c condition-names) (return t)))))
      (setq values
            (multiple-value-list
              (let ((condition-default-handlers (cdr handler-list)))
                (apply (cadr h) condition (cddr h)))))
      (if (car values) (return-from invoke-handlers values))))
  nil)

(defun condition-name-handled-p (condition-name)
  "Non-NIL if there is a handler that might handle CONDITION-NAME.
Use this to avoid signaling a condition that will not be handled;
this can often save a lot of time."
  (or (dolist (h condition-handlers)
        (if (cond ((null (car h)) t)
                  ((symbolp (car h))
                   (eq (car h) condition-name))
                  (t (memq condition-name (car h))))
            (return (if (eq (cadr h) 'condition-case-throw) t 'maybe))))
      (dolist (h condition-default-handlers)
        (if (cond ((null (car h)) t)
                  ((symbolp (car h))
                   (eq (car h) condition-name))
                  (t (memq condition-name (car h))))
            (return (if (eq (cadr h) 'condition-case-throw) t 'maybe))))
      (dolist (h condition-resume-handlers)
        (if (cond ((eq h t))
                  ((null (car h)) t)
                  ((symbolp (car h))
                   (eq (car h) condition-name))
                  (t (memq condition-name (car h))))
            (return t)))))

;;; This saves each CONDITION-CASE from having to make a new function.
(defun condition-throw (condition tag)
  (throw tag condition))

(defvar *condition-proceed-types* :unbound
  "List of proceed types specified in call to SIGNAL.
This is an argument to the function SIGNAL, saved for use by condition handlers.")

(defvar-resettable trace-conditions nil nil
  "List of condition names whose signaling should be traced, or T for all.")

(defun signal-condition (condition
                         &optional condition-proceed-types
;>> this typep is somewhat bogus.   Should have list of interactive handlers,
;>>  and ask them if they want to hack this condition, ala symbolics.
                                   (use-debugger (typep condition 'debugger-condition))
                                   ucode-error-status
                                   inhibit-resume-handlers
                         &aux tem1)
  "Signal CONDITION, running handlers, possibly enter debugger, possibly resume or proceed.
CONDITION-PROCEED-TYPES are the proceed-types the caller offers to handle.
 The value of the free variable CONDITION-RESUME-HANDLERS may provide
 additional proceed-types.
First, look for a handler or default handler that will handle CONDITION.
If none does, and USE-DEBUGGER is non-NIL, invoke the debugger.
 (USE-DEBUGGER's default is (typep CONDITION 'dbg:debugger-condition)).
If either a handler or the debugger decided to proceed, then:
 if the proceed-type is in CONDITION-PROCEED-TYPES, just return
 the proceed-type and associated arguments as multiple values.
 Otherwise, look on CONDITION-RESUME-HANDLERS for a resume handler
 for the specified proceed-type.
If the condition is not an error, and no handler handled it,
 then if the first available proceed type is nonlocal,
 proceed using it.  Otherwise, return NIL.

If INHIBIT-RESUME-HANDLERS is non-NIL, resume handlers are not run.
Any attempt to proceed simply returns to SIGNAL-CONDITION's caller.

UCODE-ERROR-STATUS is non-NIL only when this function is called
 as a result of an error detected in the microcode; it is of interest
 only to routines of conditions that the microcode can signal."
  (declare (error-reporter))
  (declare (unspecial condition-proceed-types));>> for recompilation
  (let ((condition-names (send condition :condition-names))
        (debugger-called nil)
        (*condition-proceed-types* condition-proceed-types)
        (*ucode-error-status* ucode-error-status))
    (and trace-conditions
         (or (eq trace-conditions t)
             (dolist (c condition-names)
               (if (if (symbolp trace-conditions)
                       (eq c trace-conditions)
                     (memq c trace-conditions))
                   (return t))))
         (let ((trace-conditions nil) #|errset-status|#
               (condition-handlers nil)
               (condition-default-handlers nil))
           (cerror "Proceed" "A traced condition was signaled:~%~A" condition)))
    (when condition-names
      (setq tem1
            (invoke-handlers condition condition-names)))
    (unless (car tem1)
;      (when (and errset-status
;                (not errset)
;                (errorp condition)
;                (not (send condition :dangerous-condition-p))
;                (not (send condition :debugging-condition-p)))
;       (if errset-print-msg
;           ;; Note: MUST be "brief", since some methods will lose
;           ;; if executed in this stack group and not brief.
;           (send condition :print-error-message current-stack-group t *standard-output*))
;       (throw 'errset-catch nil))
      (if use-debugger
          (setq debugger-called t
                tem1 (let ((error-depth (1+ error-depth)))
                       (invoke-debugger condition)))))
    (cond ((eq (car tem1) :asynchronous-condition-return)
           (values-list (cdr tem1)))
          ((memq (car tem1) *condition-proceed-types*)
           (values-list tem1))
          (inhibit-resume-handlers (values-list tem1))
          ((or tem1 (null *condition-proceed-types*))
           ;; If debugger is invoking a resume handler,
           ;; turn off trap-on-exit for frames out to there.
           (when debugger-called
             (debugger-prepare-for-resume-handler condition (car tem1)))
           (apply #'invoke-resume-handler condition tem1)))))

(defun debugger-prepare-for-resume-handler (condition proceed-type)
  (let* ((rh (find-resume-handler condition proceed-type))
         (to-frame
           (sg-resume-handler-frame current-stack-group rh)))
    (when to-frame
      ;; It should never be NIL, but there can be bugs.
      (do ((frame (sg-next-active current-stack-group
                                  ;; Note we don't clear the bit for THIS frame.
                                  (%pointer-difference
                                    (%stack-frame-pointer)
                                    (locf (aref (sg-regular-pdl current-stack-group) 0))))
                  (sg-next-active current-stack-group frame)))
          ((= frame to-frame))
        (setf (rp-trap-on-exit (sg-regular-pdl current-stack-group) frame) 0)))))

;;; This function's frame is returned from directly by ordinary proceeding.
;;; Therefore, it must not do anything after calling the other stack group.
(defun invoke-debugger (condition)
  "Enter the debugger for condition-object CONDITION.
If the debugger proceeds, returns a list of the values being proceeded with
/(so the CAR is the proceed-type).
Bind ERROR-DEPTH to one plus its current value before calling this."
  (declare (error-reporter))
  ;; It used to bind ERROR-DEPTH here, but I suspect that
  ;; binding specials in this function is unwise.
  (if (trapping-enabled-p)
      (funcall %error-handler-stack-group condition)
    ;; The above FUNCALL can screw up if the stack group exists
    ;; but is not initialized yet.
    (break "Attempting to invoke debugger for~%~A" condition))
  nil)

(defun invoke-restart-handlers (condition &key flavors)
  "For compatibility with Symbolics software only."
  (invoke-resume-handler (or condition
                             (and flavors
                                  (make-instance 'condition :condition-names flavors)))))

(defun invoke-resume-handler (condition &optional proceed-type &rest args)
  "Invoke a resume handler for PROCEED-TYPE on CONDITION.
Recall that each resume handler is identified by a proceed-type
and has a list of condition-names it applies to, and a predicate to actually decide.
We run the innermost resume handler for the specified PROCEED-TYPE
which applies to CONDITION, based on CONDITION's condition names
and on applying the predicate to it.
If PROCEED-TYPE is NIL, the innermost resume handler that applies
is used regardless of its proceed type; however, in this case,
a T in the list CONDITION-RESUME-HANDLERS terminates the scan."
  (declare (error-reporter))
  (let ((h (find-resume-handler condition proceed-type)))
    (when h
      (call (fifth h) nil condition :spread (nthcdr 5 h) :spread args)
      (ferror "A condition resume handler for proceed-type ~S returned to its caller."
              proceed-type)))
  (and proceed-type
       (ferror "Invalid proceed-type ~S returned by handler for ~S."
               proceed-type condition)))

(defun find-resume-handler (condition &optional proceed-type
                            (resume-handlers condition-resume-handlers)
                            &aux (condition-names
                                   (and condition (send condition :condition-names))))
  "Return the resume handler that would be run for PROCEED-TYPE on CONDITION.
This is how INVOKE-RESUME-HANDLER finds the handler to run.
RESUME-HANDLERS is the list of resume handlers to search
/(the default is the current list);
this is useful for thinking about other stack groups.
The value is an element of RESUME-HANDLERS, or NIL if no handler is found."
  (do ((handler-list resume-handlers (cdr handler-list)))
      ((eq handler-list (cdr handler-list)))
    (let ((h (car handler-list)))
      (if (eq h t)
          (if (null proceed-type) (return nil))
        (and (cond ((null (car h)) t)
                   ((null condition-names) t)
                   ((not (consp (car h)))
                    (memq (car h) condition-names))
                   (t (dolist (c (car h))
                        (if (memq c condition-names) (return t)))))
             (or (null proceed-type)
                 (eq (cadr h) proceed-type))
             (or (eq (caddr h) t)
                 (funcall (caddr h) condition))
             (return h))))))

(defmethod (condition :proceed-type-p) (proceed-type)
  (or (memq proceed-type *condition-proceed-types*)
      (do ((handler-list condition-resume-handlers (cdr handler-list))
           (h))
          ((eq handler-list (cdr handler-list)))
        (setq h (car handler-list))
        (and (consp h)
             (cond ((null (car h)) t)
                   ((not (consp (car h)))
                    (memq (car h) condition-names))
                   (t (dolist (c (car h))
                        (if (memq c condition-names) (return t)))))
             (or (eq (caddr h) t) (funcall (caddr h) self))
             (return t)))))

(defun sg-condition-proceed-types (sg condition)
  "In the debugger, return a list of CONDITION's proceed-types."
  (si::union-eq (symeval-in-stack-group '*condition-proceed-types* sg)
                (condition-resume-types condition
                                        (symeval-in-stack-group 'condition-resume-handlers sg))))

(defmethod (condition :proceed-types) ()
  (si::union-eq *condition-proceed-types*
                (condition-resume-types self)))

(defun condition-resume-types (condition &optional
                               (resume-handlers condition-resume-handlers))
  "Return a list of all resume handler keywords available for CONDITION's handlers.
These resume-types, together with the proceed-types specified in signaling,
are the possible keywords that a condition handler may return as its first value."
  (let ((condition-names (send condition :condition-names))
        types)
    (do ((handler-list resume-handlers (cdr handler-list))
         (h))
        ((eq handler-list (cdr handler-list)))
      (setq h (car handler-list))
      (and (consp h)
           (cond ((null (car h)) t)
                 ((not (consp (car h)))
                  (memq (car h) condition-names))
                 (t (dolist (c (car h))
                      (if (memq c condition-names) (return t)))))
           (or (eq (caddr h) t) (funcall (caddr h) condition))
           (not (memq (cadr h) types))
           (push (cadr h) types)))
    (setq types (nreverse types))
    ;; Put all proceed-types which are lists at the end.
    (nconc (subset #'atom types) (subset-not #'atom types))))

(defun sg-condition-handled-p (sg condition-names)
  "T if there is an active condition handler in SG that might handle one of CONDITION-NAMES."
  (do ((hh (symeval-in-stack-group 'condition-handlers sg) (cdr hh))
       (part nil))
      (())
    (if (null hh)
        (if part (return nil)
          (setq hh (symeval-in-stack-group 'condition-default-handlers sg)
                part t)))
    (let ((h (car hh)))
      (and (cond ((null (car h)) t)
                 ((atom (car h))
                  (if (consp condition-names)
                      (memq (car h) condition-names)
                    (eq (car h) condition-names)))
                 ((atom condition-names)
                  (memq condition-names (car h)))
                 (t (dolist (c condition-names)
                      (if (memq c (car h))
                          (return t)))))
           (return t)))))

;;;; Functions for finding the special pdl info associated with a stack frame.

(defmacro scan-specpdl-by-frames ((sg) (sp-var rp-var sp-start sp-end frame-var)
                                  before-frame-body after-frame-body
                                  &body body)
  `(let ((,sp-var (sg-special-pdl ,sg))
         (,rp-var (sg-regular-pdl ,sg)))
     (do ((,frame-var (if (eq ,sg current-stack-group)
                          ;; Figure out RP index of our own frame!
                          (%pointer-difference
                            (%stack-frame-pointer)
                            (locf (aref ,rp-var 0)))
                        (sg-ap ,sg))
           (sg-next-active ,sg ,frame-var))
          (ignore-frame (eq ,sg current-stack-group) nil)
          (,sp-end (if (eq ,sg current-stack-group)
                         (get-own-special-pdl-pointer ,sp-var)
                       (sg-special-pdl-pointer ,sg)))
          (,sp-start))
         ((null ,frame-var))
       (progn . ,before-frame-body)
       (cond ((and (not ignore-frame)
                   (not (zerop (rp-binding-block-pushed ,rp-var ,frame-var))))
              (do ()
                  ((= (%p-data-type (locf (aref ,sp-var ,sp-end))) dtp-locative))
                ;; Space back over a random non-binding frame
                (do ()
                    ((not (zerop (%p-ldb %%specpdl-block-start-flag
                                         (locf (aref ,sp-var ,sp-end))))))
                  (setq ,sp-end (1- ,sp-end)))
                (setq ,sp-end (1- ,sp-end)))
              ;; Make SP-START and SP-END inclusive brackets for this binding frame
              (setq ,sp-start (1- ,sp-end))
              (do ()
                  ((not (zerop (%p-ldb %%specpdl-block-start-flag
                                       (locf (aref ,sp-var ,sp-start))))))
                (setq ,sp-start (- ,sp-start 2)))
              ;; Do the body, now that SP-START and SP-END are set up.
              (progn . ,body)
              (setq ,sp-end (1- ,sp-start))))
       (progn . ,after-frame-body))))

;;; Get the special-pdl pointer for the running SG
(defun get-own-special-pdl-pointer (&optional (sp (sg-special-pdl current-stack-group)))
  "Return the current special pdl pointer of the current stack group.."
  (%pointer-difference (special-pdl-index)
                       (locf (aref sp 0))))

(defun sg-frame-special-pdl-range (sg frame &aux (rp (sg-regular-pdl sg)))
  "Return the range of indices in SG's special pdl that go with FRAME.
/(FRAME is an index in the regular pdl.)
The two values are a starting index and an ending index.
If there is no special pdl data for the frame, NIL is returned."
  (and (not (zerop (rp-binding-block-pushed rp frame)))
       (scan-specpdl-by-frames (sg) (sp rp i j frame1) nil nil
         (and (= frame1 frame) (return (values i j))))))

(defun sg-frame-special-pdl-index (sg frame)
  "Return an index in SG's special pdl corresponding to just outside frame FRAME.
The value points to the last word of data pushed outside of this frame.
It is never NIL."
  (scan-specpdl-by-frames (sg) (sp rp i j frame1)
                          nil ((and (= frame1 frame) (return j)))))

(defun sg-frame-of-special-binding (sg location value)
  "Return the frame in SG where LOCATION (a value cell) is bound to VALUE.
The innermost binding that has that value is the one found."
  ;; If SG is not running, we scan outward till we find a binding with that value.
  ;; If SG is running, we scan outward till we find such a binding,
  ;; then keep scanning till the next binding.
  (let ((return-on-next-binding
          ;; If the desired binding is the innermost one,
          ;; set the flag to return on first binding found.
          (and (eq sg current-stack-group)
               (eq (contents location) value))))
    (scan-specpdl-by-frames (sg) (sp rp i j frame1) nil nil
      (do ((idx j (- idx 2)))
          ((< idx i))
        (when (eq (aref sp idx) location)
          (if return-on-next-binding
              (return-from sg-frame-of-special-binding frame1))
          (if (eq (aref sp (1- idx)) value)
              (if (eq sg current-stack-group)
                  (setq return-on-next-binding t)
                (return-from sg-frame-of-special-binding frame1))))))))

(defun sg-resume-handler-frame (sg resume-handler)
  "Return the index of the frame in SG in which RESUME-HANDLER was established.
RESUME-HANDLER should be an element of CONDITION-RESUME-HANDLERS' value in SG.
We assume that elements are pushed onto CONDITION-RESUME-HANDLERS
always one at a time."
  (sg-frame-of-special-binding sg (locf condition-resume-handlers)
                               (memq resume-handler
                                     (symeval-in-stack-group 'condition-resume-handlers sg))))

;;;; Various non-microcode conditions.

(defsignal nil (ferror) ()
  "This is signaled for (FERROR NIL ...), etc.")

(defsignal-explicit fquery condition (options format-string &rest format-args)
  "By default, calls to FQUERY signal this."
  :property-list (list :options options)
  :format-string format-string
  :format-args (copy-list format-args))

(defsignal sys:abort condition ()
  "This is signaled to abort back to the innermost command loop.")

(defsignal sys:unknown-locf-reference error (form)
  "Means that LOCF was used on a form which it did not know how to handle.")

(defsignal sys:unknown-setf-reference error (form)
  "Means that SETF was used on a form which it did not know how to handle.")

(defsignal sys:zero-log sys:arithmetic-error (number)
  "NUMBER, which is a zero, was used as the argument to a logarithm function.")

(defsignal math:singular-matrix arithmetic-error (matrix)
  "Signaled when any matrix handling function finds a singular matrix.")

(defsignal sys:invalid-form error (form)
  "EVAL was given FORM and couldn't make sense of it.")

(defsignal sys:invalid-function-spec error (function-spec)
  "Invalid function spec passed to FDEFINITION, etc.")

(defsignal sys:invalid-lambda-list invalid-function (function)
  "The interpreted function FUNCTION's lambda list is malformatted.")

(defsignal sys:undefined-keyword-argument sys:undefined-keyword-argument
  (keyword value)
  "A function wanting keyword args to KEYWORD with VALUE, which it wasn't expecting.")

(defflavor sys:undefined-keyword-argument () (error))

(defmethod (sys:undefined-keyword-argument :case :proceed-asking-user :new-keyword)
           (continuation read-object-function)
  "Use a different keyword, which you must type in."
  (funcall continuation :new-keyword
           (funcall read-object-function :eval-read
                    "Form to evaluate to get the keyword to use instead of ~S: "
                    (send self :keyword))))

(defsignal stream-closed (error stream-closed stream-invalid) (stream)
  "I//O to STREAM, which has been closed and no longer knows how to do I//O.")

(defsignal stream-invalid error (stream)
  "I//O to STREAM, which is in a state not valid for I//O.")

(defflavor end-of-file () (error))

(defsignal sys:end-of-file-1 end-of-file (stream)
  "End of file on STREAM, not within READ.")

(defflavor parse-error (stream) (ferror) :settable-instance-variables)
(defflavor parse-ferror () (parse-error) :alias-flavor)

(defsignal parse-error-1 parse-error ()
  "Error in parsing input; rubout handler should handle it.")

(defun parse-ferror (format-string &rest args)
  (apply #'cerror :no-action nil 'parse-error-1 format-string args))

(defmethod (parse-error :after :init) (ignore)
  (setq stream (and (variable-boundp si:read-stream) si:read-stream)))

(defmethod (parse-error :after :print-error-message) (ignore ignore -stream-)
  (when stream
    (format -stream- "Error occurred in reading from ~S.~%" stream)))

(defmethod (parse-error :case :proceed-asking-user :no-action) (continuation ignore)
  "Continue reading, trying to ignore the problem."
  (funcall continuation :no-action))

(defflavor package-error () (error)
  (:documentation "All package errors are based on this"))

(defflavor package-not-found (package-name
                              (relative-to))
           (no-action-mixin package-error)
  :inittable-instance-variables
  :gettable-instance-variables
  ;(:documentation "")
  )

(defmethod (package-not-found :report) (stream)
  (format stream "No package named /"~A/"~@[ relative to ~A~]"
          package-name relative-to))

(defmethod (package-not-found :case :proceed-asking-user :retry)
           (proceed-function ignore)
  "Looks for the package again.  Use this if you create it by hand"
  (funcall proceed-function :retry))

(defmethod (package-not-found :case :proceed-asking-user :create-package)
           (proceed-function ignore)
  "Creates the package (with default characteristics) and proceeds."
  (format t "Creating package ~A." (send self :package-name))
  (funcall proceed-function :create-package))

(defmethod (package-not-found :case :proceed-asking-user :new-name)
           (proceed-function prompt-and-read-function)
  "Proceeds, asking for a name of a package to use instead."
  (funcall proceed-function :new-name
           (funcall prompt-and-read-function :string
                    "Name (not local nickname) of package to use instead: ")))

(defsignal-explicit read-package-not-found (package-not-found parse-error) (string name)
  "Signaled when READ finds a package prefix for a nonexistent package"
  :format-string string
  :format-args (list name)
  :package-name name)


(defflavor package-name-conflict (in-package operation conflicts)
           (package-error)
  :gettable-instance-variables
  :inittable-instance-variables
  (:default-init-plist :condition-names '(name-conflict))       ;make alias name work right
  (:documentation "Base flavor for all error dealing with name conflicts"))
;;; what slime call it.
(defflavor name-conflict () (package-name-conflict) :alias-flavor)

;(defmethod (package-name-conflict :case :document-proceed-type :punt) (stream ignore)
;  (format stream "Returns without doing the ~A." (send self :operation)))

;(defmethod (package-name-conflict :case :document-proceed-type :shadow) (stream ignore)
;  (format stream "Leaves the symbols already in package ~A where they are."
;         (send self :package)))

;(defmethod (package-name-conflict :case :document-proceed-type :export) (stream ignore)
;  (format stream "Puts the newly inheritable symbols into the package."))

;(defmethod (package-name-conflict :case :document-proceed-type :unintern) (stream ignore)
;  (format stream "Uninterns the conflicting symbol~:[ ~S~;s~]."
;         (cdr (send self :losing-symbols))
;         (send self :losing-symbols)))

;(defmethod (package-name-conflict :case :proceed-asking-user :shadowing-import)
;          (proceed-function prompt-and-read-function)
;  (funcall proceed-function :shadowing-import
;          (funcall prompt-and-read-function :string
;                   "Package name of the symbol you want to prefer.")))

;(defmethod (package-name-conflict :case :document-proceed-type :choose) (stream ignore)
;  (format stream "Pops up window to specify what to do in more detail."))

;(defmethod (package-name-conflict :case :document-proceed-type :share) (stream ignore)
;  (format stream "Forwards all the symbols together like GLOBALIZE."))

(defflavor use-package-name-conflict (in-package using-packages
                                      (local-conflicts) (inherited-conflicts)
                                      (external-conflicts))
           (package-name-conflict)
  (:default-init-plist :operation 'use-package)
  :gettable-instance-variables
  :inittable-instance-variables
  ;(:documentation "")
  )

;(defmethod (use-package-name-conflict :after :init) (ignore)
;  (setq local-conflicts (sortcar local-conflicts #'string<)
;       inherited-conflicts (cons (car inherited-conflicts)
;                                 (sortcar (cdr inherited-conflicts) #'string<))
;       external-conflicts (sortcar external-conflicts #'string<)))

(defmethod (use-package-name-conflict :report) (stream)
  (format stream "Calling USE-PACKAGE from package ~A on package~:[ ~{~A~^~}~;s
~{~#[~; and ~A~; ~A~:; ~A,~]~}~] caused the following name-conflict~P:"
          in-package (cdr using-packages) using-packages (+ (length local-conflicts)
                                                            (length inherited-conflicts)
                                                            (length external-conflicts)))
  (dolist (c local-conflicts)
    (format stream "~&Package ~A already contains symbol /"~A/";
  it would inherit ~:[a conflicting symbol from package~;conflicting symbols from packages~]"
            in-package (caar c) (cdr c))
    (dolist (loser c)
      (format stream " ~A" (cdr loser))))
  (dolist (c inherited-conflicts)
    (format stream "~&Package ~A already inherits symbol /"~A/" (from package ~A);
  it would inherit ~:[a conflicting symbol from package~;conflicting symbols from packages~]"
            in-package (caar c) (cdar c) (cddr c))
    (dolist (loser (cdr c))
      (format stream " ~A" (cdr loser))))
  (dolist (c external-conflicts)
    (format stream "~&Package ~A would inherit a symbol named /"~A/" from multiple packages:"
            in-package (caar c))
    (dolist (loser c)
      (format stream " ~A" (cdr loser)))))


;(defun choose-use-package-loss-avoidance (&aux tem (default '(())) (*package* in-package))
;  (declare (:self-flavor use-package-name-conflict))
;  (let ((title (format nil "(USE-PACKAGE '(~{~A~#[~:; ~A~]~}) '~A)"
;                      using-packages in-package))
;       (vars nil))
;    (when local-conflicts
;      (push vars `(nil ,(format nil "Symbols already present in ~A" in-package) nil))
;      (dolist (c local-conflicts)
;       (push vars `(shadow "Shadow old")
;       (dolist (loser c)
;         (push vars `((use ,(cdr loser))
;                      ,(format nil "Unintern, USE ~A" (cdr loser)))))


;;;These are various end-of-file conditions for reader.
;;;The :NO-ACTION <assume> that reader attempts to recover as described;
;;;at this time, the actions match our words.  -Keith 7/88

(defflavor read-end-of-file () (end-of-file parse-error))

(defmethod (read-end-of-file :case :proceed-asking-user :no-action) (continuation ignore)
  "Handle unterminated input."
  (funcall continuation :no-action))

(defsignal read-end-of-file (read-end-of-file read-error) (stream)
  "End of file within READ on STREAM.
SYS:READ-LIST-END-OF-FILE, SYS:READ-STRING-END-OF-FILE, or SYS:READ-SYMBOL-END-OF-FILE
should be used if they apply more specifically.")

;;;EOF in lists

(defflavor read-list-end-of-file () (read-end-of-file))

(defmethod (read-list-end-of-file :case :proceed-asking-user :no-action) (continuation ignore)
  "Close off unfinished lists."
  (funcall continuation :no-action))

(defsignal read-list-end-of-file (read-list-end-of-file read-error read-end-of-file)
           (stream list)
  "End of file within READ constructing a list, on STREAM.
LIST is the list constructed so far.")

;;;EOF in strings

(defflavor read-string-end-of-file () (read-end-of-file))

(defmethod (read-string-end-of-file :case :proceed-asking-user :no-action) (continuation ignore)
  "Return null string."
  (funcall continuation :no-action))

(defsignal read-string-end-of-file (read-string-end-of-file read-error read-end-of-file)
           (stream string)
  "End of file within READ constructing a string, on STREAM.
STRING is the string read so far.")

;;;EOF in symbols

(defflavor read-symbol-end-of-file () (read-end-of-file))

(defmethod (read-symbol-end-of-file :case :proceed-asking-user :no-action) (continuation ignore)
  "Treat unterminated symbol as NIL."
  (funcall continuation :no-action))

(defsignal read-symbol-end-of-file (read-symbol-end-of-file read-end-of-file read-error)
           (stream string)
  "End of file within READ constructing a symbol, on STREAM.
Occurs, e.g., on EOF after package prefix - reading something like 'foo:'.
STRING is the string read so far.")

(defsignal read-error-1 (parse-error read-error) ()
  "Error other than end of file, within READ.")

(defsignal missing-closeparen (parse-error read-error missing-closeparen) ()
  "Error of open paren found in column 0 in middle of defun.")

(defsignal print-not-readable ferror (object)
  "Printing OBJECT, which cannot be printed so it can be read back.")

(defflavor disk-error () (error))

;;; It's best to encourage handlers to let the user see these!
(defmethod (disk-error :dangerous-condition-p) () t)

(defsignal sys:disk-error disk-error ()
 "A fatal disk error happened.")

(defmethod (disk-error :case :proceed-asking-user :retry-disk-operation)
           (continuation read-object-function)
  "Try the disk operation again."
  (if (funcall read-object-function '(:fquery)
               "Retry the disk operation? ")
      (funcall continuation :retry-disk-operation)))

(defsignal sys:redefinition redefinition
  (definition-type name new-pathname old-pathname)
  "NAME's definition of type DEFINITION-TYPE was redefined in a different file.
The old definition was in OLD-PATHNAME and the new one is in NEW-PATHNAME.
Both of the last two are generic pathnames or NIL.")

(defflavor network-error () (error))

(defflavor local-network-error () (network-error))

(defflavor remote-network-error (connection foreign-host) (network-error)
  :gettable-instance-variables
  :inittable-instance-variables)

(defmethod (remote-network-error :after :init) (ignore)
  (setq connection (getf si:property-list ':connection))
  (setq foreign-host (or (getf si:property-list ':foreign-host)
                         (and connection
                              (si:get-host-from-address
                                (chaos:foreign-address connection) ':chaos)))))

(defsignal sys:network-resources-exhausted local-network-error ()
  "The connection table was full, or something else has run out.")

(defsignal sys:unknown-address local-network-error (address)
  "ADDRESS was an /"address/" argument to CHAOS:CONNECT or some such.")

(defsignal sys:unknown-host-name local-network-error (name)
  "NAME was specified as a host name and not recognized.")

(defflavor bad-connection-state () (remote-network-error))

(defsignal sys:bad-connection-state-1 (bad-connection-state) (connection)
  "CONNECTION was in a bad state and some operation couldn't be done.
Use a more specified signal-name if one applies:
SYS:CONNECTION-NO-MORE-DATA, SYS:HOST-STOPPED-RESPONDING,
SYS:CONNECTION-CLOSED, or SYS:CONNECTION-LOST.")

(defflavor connection-error () (remote-network-error))

(defsignal sys:connection-error-1 (connection-error) (connection)
  "An error in making a connection.  CONNECTION is the connection-object.
Use a more specified signal-name if one applies:
SYS:HOST-NOT-RESPONDING-DURING-CONNECTION or SYS:CONNECTION-REFUSED.")

(defsignal sys:host-not-responding-during-connection
           (connection-error
             sys:host-not-responding-during-connection host-not-responding)
  (connection)
  "The foreign host did not respond when asked to make a connection.
CONNECTION is the connection object we used while trying.")

(defsignal sys:no-server-up connection-error ()
  "No server was available for some protocol this machine wanted to use.")

(defsignal sys:host-stopped-responding
           (bad-connection-state
             sys:host-stopped-responding host-not-responding)
  (connection)
  "The foreign host stopped responding while we were connected.
CONNECTION is the connection object.")

(defsignal sys:connection-refused connection-error
  (connection foreign-host reason)
  "FOREIGN-HOST refused a connection, giving REASON.  REASON is NIL
if none was given.  CONNECTION is the connection object.")

(defsignal sys:connection-closed bad-connection-state (connection reason)
  "Foreign host refused a connection, giving REASON.  REASON is NIL
if none was given.  CONNECTION is the connection object.")


(defsignal sys:connection-lost bad-connection-state (connection reason)
  "CONNECTION was broken for REASON.  REASON is NIL
if none was given.  CONNECTION is the connection object.")


(defsignal sys:connection-no-more-data bad-connection-state (connection foreign-host)
  "Attempt to read past all data received on closed connection CONNECTION.")


(defsignal-explicit wrong-type-argument wrong-type-argument-error
  (format-string &rest format-args)
  "Wrong type argument from old losing version of CHECK-TYPE."
  :format-string format-string
  :format-args (copy-list format-args)
  :property-list `(:description ,(first format-args)
                                :old-value ,(second format-args)
                                :arg-name ,(third format-args)
                                :function
                                ,(let ((rp (sg-regular-pdl current-stack-group)))
                                   (rp-function-word
                                     rp
                                     (sg-previous-nth-active current-stack-group
                                                             (%pointer-difference
                                                               (%stack-frame-pointer)
                                                               (locf (aref rp 0)))
                                                             -3)))))

(defflavor wrong-type-argument-error () (error))

;>> somebody should sort out which of :argument-value and :new-value should really be here
(defmethod (wrong-type-argument-error :case :proceed-asking-user :argument-value)
           (continuation read-object-function)
  "Use a different argument.  You type an expression for the new value."
  (funcall continuation :argument-value
           (funcall read-object-function :eval-read
             (format nil "Form to be evaluated and used as replacement value for ~A:~%"
                     (send self :arg-name)))))

(defmethod (wrong-type-argument-error :case :proceed-asking-user :new-value)
           (continuation read-object-function)
  "Use a different argument.  You type an expression for the new value."
  (funcall continuation :new-value
           (funcall read-object-function :eval-read
             (format nil "Form to be evaluated and used as replacement value for ~A:~%"
                     (send self :arg-name)))))

(defflavor failed-assertion (places proceed-type-place-alist) (error)
  :inittable-instance-variables
  :gettable-instance-variables)

;;; >>> Needed for system versions up to and including 121
(defmethod (failed-assertion :after :init) (plist)
  (declare (ignore plist))
  (unless (variable-boundp proceed-type-place-alist)
    (setq proceed-type-place-alist (pairlis places places))))

(defmethod (failed-assertion :or :document-proceed-type)
           (proceed-type stream ignore)
  (let ((entry (assq proceed-type proceed-type-place-alist)))
    (when entry
      (format stream "Try again, setting ~S.  You type an expression for it." (cdr entry))
      t)))

(defmethod (failed-assertion :or :proceed-asking-user)
           (proceed-type continuation read-object-function)
  (let ((entry (assq proceed-type proceed-type-place-alist)))
    (when entry
      (funcall continuation proceed-type
               (funcall read-object-function :eval-read
                        "Form to be evaluated and used as replacement value for ~S:~%"
                        (cdr entry)))
      t)))

(defun current-function-name (&aux dummy)
  "Returns the name of the function from which this one is called.
If called from the interpreter, it looks for a NAMED-LAMBDA on the stack."
  (let* ((rp (sg-regular-pdl current-stack-group))
         (frame (%pointer-difference (locf dummy) (locf (aref rp 1)))))
    (do ((f frame (sg-next-active current-stack-group f))
         tem)
        ((null f))
      (and (consp (rp-function-word rp f))
           (neq (setq tem (function-name (rp-function-word rp f)))
                (rp-function-word rp f))
           (return tem)))))

;(defun (:property current-function-name compiler::p1) (ignore)
;  `',compiler::name-to-give-function)

(compile-flavor-methods
 condition
 debugger-condition
 error
 no-action-mixin
 proceed-with-value-mixin
 multiple-cerror
 cerror
 ferror
 multiple-cerror
 undefined-keyword-argument
 parse-error
 end-of-file
 read-end-of-file
 read-list-end-of-file
 read-string-end-of-file
 read-symbol-end-of-file
 package-not-found
 package-name-conflict
 disk-error
 network-error
 local-network-error
 remote-network-error
 bad-connection-state
 connection-error
 wrong-type-argument-error
 failed-assertion
 )

(defparameter *abort-object* (make-condition 'sys:abort "Abort.")
  "A condition-object for condition SYS:ABORT, used every time we want to abort.")
(defvar abort-object)
(forward-value-cell 'abort-object '*abort-object*)
