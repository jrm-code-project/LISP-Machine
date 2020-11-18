;-*- Mode:LISP; Package:EH; Readtable:CL; Base:10 -*-

; let's do errors the right way

(defflavor warning () (no-action-mixin condition))
(defmethod (warning :print-error-message-prefix) (ignore ignore stream)
  (princ ">>WARNING: " stream))

(export 'format-condition-mixin)
(defflavor format-condition-mixin (format-string (format-args nil)) ()
  :abstract-flavor
  :gettable-instance-variables
  :initable-instance-variables
  (:required-flavors condition)
  (:documentation "Defines a :REPORT method of applying FORMAT"))

(defmethod (format-condition-mixin :report) (stream)
  (apply #'format stream format-string format-args))

;; this is what ferror should be except for brain-death.
(export 'format-error)
(defflavor format-error () (format-condition-mixin error)
  (:documentation "Random error.  Prints by applying FORMAT to :FORMAT-STRING and :FORMAT-ARGS"))

(export 'dangerous-condition)
(defflavor dangerous-condition () ()
  (:included-flavors condition))
(defmethod (dangerous-condition :dangerous-condition-p) ()
  t)

(export 'debugging-condition)
(defflavor debugging-condition () (debugger-condition))
(defmethod (debugging-condition :debugging-condition-p) ()
  t)


;;; used by check-type

(export 'wrong-type-value)
(defflavor wrong-type-value
           (value
            place
            (type-specifier nil)
            (description nil))
           (proceed-with-value-mixin error)
  :gettable-instance-variables
  :initable-instance-variables
  (:documentation "WRONG-TYPE-VALUE means an incorrect value was found someplace.
The interesting components are:
 :VALUE -- the value (of the incorrect type).
 :PLACE -- the place where it was found (a SETF-style place usually)
        This is only used for information purposes.
 :TYPE-SPECIFIER -- a type specifier.  Defaults to NIL, which means we
        don't know.
 :DESCRIPTION -- a description of the type, for when this will be
        substantially more informative than the type specifier."))

;;; This should really be an :after :init method, but we really don't want to
;;; compute this information unless we have to report it.  So we just do
;;; a check prior to any :report method.
(defmethod (wrong-type-value :before :report) (ignore)
  (cond (description)
        ((null type-specifier))
        (t (setq description (si::type-pretty-name type-specifier)))))

(defmethod (wrong-type-value :report) (stream)
  (format stream
          "~~:[The value ~S~;The value of ~:*~S, ~S,~] is ~:[of the wrong type.~;~:*not ~A.~]~"
          place value description))

(defmethod (wrong-type-value :case :document-proceed-type :new-value) (stream &optional ignore)
  (format stream "Set ~S to a new value; the value of an expression you type." place))

(defmethod (wrong-type-value :case :proceed-asking-user :new-value)
           (continuation read-object-function)
  (funcall continuation ':new-value
           (funcall read-object-function ':eval-read
                    "Set ~S to result of evaluating: " place)))

(export 'bad-system-parameter)
(defflavor bad-system-parameter (reset-value)
           (no-action-mixin wrong-type-value)
  (:documentation "Signaled when some variable or memory location
 which is needed by the system to function is found to be invalid.
In addition to the instance variables provided by the WRONG-TYPE-VALUE error,
 we provide :RESET-VALUE, which gives the value to which the place was set
 in order to be able to be able to even safely signal the error.")
  :gettable-instance-variables :initable-instance-variables
  (:required-init-keywords :reset-value))

(defmethod (bad-system-parameter :report) (stream)
  (format stream
    "~The ~:[system-used location~:;system parameter~] ~S had a bad value ~S~
        ~:[;~%it~:;~:*,~%which was not ~A.~%It~] has been reset to ~S.~@[~%~A.~]~"
    (symbolp place) place value description reset-value))


;; This is used for the M-Break and C-M-Break keys, as well as by  the (dbg) function.
(defflavor break () (format-condition-mixin no-action-mixin
                     debugger-condition))

(defmethod (break :case :proceed-asking-user :no-action) (continuation ignore)
  "Continue from break."
  (format t " Continue from break.~%")
  (funcall continuation :no-action))

(defmethod (break :print-error-message-prefix) (sg brief stream)
  (declare (ignore sg brief))
  (princ ">> " stream))

;;;; Warn

(export '*signal-on-warnings*)

(defvar-resettable *signal-on-warnings* nil nil
  "If this is not NIL, then WARN will signal a DBG:COMMON-LISP-WARNING
condition.  See the WARN function for more information.")

(defvar-resettable *break-on-warnings* nil nil
  "If this is not NIL, then the WARN function behaves like the BREAK function.
More specifically, in terms of the NIL error system, if *BREAK-ON-WARNINGS*
is not NIL, then the default interactive handler will handle conditions
of the type DBG:WARNING by entering the debugger -- normally, it ignores
them.")

(export 'common-lisp-warning)

(defflavor common-lisp-warning () (format-condition-mixin warning ferror))

(defmethod (common-lisp-warning :case :proceed-asking-user :no-action) (continuation ignore)
  "Simply proceed from warning."
  (funcall continuation :no-action))

(compile-flavor-methods common-lisp-warning)

(defun warn (format-string &rest format-args)
  "This is the Common Lisp WARN function.  FORMAT-STRING and FORMAT-ARGS are used
to construct a warning message which is output to *ERROR-OUTPUT*. WARN always returns NIL.
Normally, WARN simply produces this output and returns NIL, without using the
condition-handling system.  However, there are two variables which can be used to modify
its behavior.
 *BREAK-ON-WARNINGS*, if not NIL, makes WARN behave like enter a debugger breakpoint.
A message will be printed, and the debugger is entered.  This can give one a chance to
inspect the state of a computation at the point when a warning is generated.
 EH:*SIGNAL-ON-WARNINGS*, if not NIL (as it is by default), causes WARN to signal an
EH:COMMON-LISP-WARNING condition.  This is to permit a sophisticated system to produce its
own warnings, or simply to log them.  If this condition is not handled, then WARN behaves
normally (it prints the error message itself);  otherwise, it simply returns NIL.
 If *BREAK-ON-WARNINGS* is not NIL, then the condition IS signaled --
it is the default interactive handler (which is what decides when to call
the Lisp debugger) which decides whether to break, by looking at both
the type of the condition and *BREAK-ON-WARNINGS*."
  (check-type format-string string)
  (unless (and (or *signal-on-warnings* *break-on-warnings*)
               (signal-condition
                 (make-condition 'common-lisp-warning
                   :format-string format-string :format-args format-args
                   :proceed-types '(:no-action))
                 '(:no-action)))
    (format *error-output* "~&>>WARNING: ~?~&" format-string format-args))
  nil)

(defflavor redefinition () (warning))

(defmethod (redefinition :case :proceed-asking-user :proceed) (continuation ignore)
  "Perform this and all further redefinitions of that file by this file."
  (funcall continuation :proceed))

(defmethod (redefinition :case :proceed-asking-user :inhibit-definition) (continuation ignore)
  "Continue execution but skip this redefinition."
  (funcall continuation :inhibit-definition))


(defflavor invalid-function (function) (error)
  :gettable-instance-variables :inittable-instance-variables)

(defmethod (invalid-function :report) (stream)
  (format stream (if (symbolp function)
                     "The symbol ~S has an invalid function definition"
                     "The object ~S is not a valid function")
          function))

;; conceivably, this could also have a return-values command like
;;  com-return-a-value (c-r) in the debugger
(defmethod (invalid-function :case :proceed-asking-user :new-function)
           (continuation read-object-function)
  "Use a different function.  You type an expression for one."
  (funcall continuation :new-function
           (funcall read-object-function :eval-read
                    "Form to evaluate to get function to use instead:~%")))


;;;; used by both the interpreter and by ucode trap-out

;; function-entry-trap, in TRAP, is based on this.
(defflavor function-entry-error
         (function argument-list nargs)
         (error)
  (:required-init-keywords :function :argument-list)
  :abstract-flavor
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (function-entry-error :after :init) (ignore)
  (or (variable-boundp nargs) (setq nargs (length argument-list))))

(defmethod (function-entry-error :user-proceed-types) (proceed-types)
  (let ((tem (copy-list proceed-types))
        first)
    (dolist (p tem)
      (if (memq p '(:additional-arguments :fewer-arguments :new-argument-list))
          (return (setq first p))))
    ;; Get rid of all occurrences of those three, except the first one.
    (if first
        (setf (cdr (memq first tem))
              (delq ':additional-arguments
                    (delq ':fewer-arguments
                          (delq ':new-argument-list
                                (cdr (memq first tem)))))))
    tem))

(defmethod (function-entry-error :case :proceed-asking-user :additional-arguments)
           (continuation read-object-function)
  "Try again with additional arguments.  You type expressions for them."
  (send self :proceed-asking-user :new-argument-list continuation read-object-function))

(defmethod (function-entry-error :case :proceed-asking-user :fewer-arguments)
           (continuation read-object-function)
  "Try again, dropping some of the arguments.  You must confirm."
  (send self :proceed-asking-user :new-argument-list continuation read-object-function))

(defmethod (function-entry-error :case :proceed-asking-user :new-argument-list)
           (continuation read-object-function)
  (let* ((-function- (send self :function))
         (-argument-list- (send self :argument-list))
         (-nargs- (send self :nargs))
         (form (cons -function- -argument-list-))
         (args-info (args-info -function-))
         (args-wanted (ldb %%arg-desc-min-args args-info))
         (rest-flag (ldb-test %%arg-desc-any-rest args-info))
         (max-args (ldb %%arg-desc-max-args args-info))
         new-args)
    ;; Function may have been redefined to take the supplied number of arguments
    ;; so don't look at the original error, but check everything again.
    (cond ((< -nargs- args-wanted)
           (do ((i -nargs- (1+ i)))
               ((unless rest-flag (eq i max-args)))
             (multiple-value-bind (value flag)
                 (funcall read-object-function
                          (if ( i args-wanted)
                              :eval-read-or-end :eval-read)
                          (if ( i args-wanted)
                              "~&Arg ~D~A, or ~C: " "~&Arg ~D~A: ")
                          i
                          (format:output nil (display-arg-name " (~A)" -function- i))
                          #\End)
               (if flag (return (values)))
               (setq new-args
                     (nconc new-args
                            (ncons value)))))
           (funcall continuation :new-argument-list
                    (append (cdr form) new-args)))
          ((or ( -nargs- max-args)
               (ldb-test %%arg-desc-any-rest args-info))
           (if (funcall read-object-function '(:fquery) "Try ~S again? " form)
               (funcall continuation :new-argument-list (cdr form))))
          ((funcall read-object-function '(:fquery)
                    "Call again with the last ~[~1;~:;~:*~D ~]argument~:P dropped? "
                    (- -nargs- max-args))
           (funcall continuation :new-argument-list
                    (firstn max-args (cdr form)))))))

;>> multiple values?
(defmethod (function-entry-error :case :proceed-asking-user :return-value)
           (continuation read-object-function)
  "Pretend the function ran; you type an expression for the value it (supposedly) returns."
  (funcall continuation :return-value
           (funcall read-object-function :eval-read
                    "Form to evaluate and return from ~S: "
                    (function-name (send self :function)))))

(defflavor sys:too-few-arguments () (function-entry-error)
  (:documentation "FUNCTION was called with only NARGS args, which were ARGUMENT-LIST."))

(defmethod (too-few-arguments :report) (stream)
  (format stream "Function ~S called with only ~D argument~1@*~P."
          (function-name function) nargs))

(defflavor sys:too-many-arguments () (function-entry-error)
  (:documentation "FUNCTION was called with too NARGS args, which were ARGUMENT-LIST."))

(defmethod (too-many-arguments :report) (stream)
  (format stream "Function ~S called with too many arguments (~D)."
          (function-name function) nargs))

;;; used by the interpreter (can't use defflavor in the cold-load)

(defflavor si::free-variable-reference (attempt symbol must-be-bound-p)
           (proceed-with-value-mixin error)
  :inittable-instance-variables
  (:gettable-instance-variables symbol attempt))

(defmethod (si::free-variable-reference :report) (stream)
  (format stream "~Free reference made to symbol ~S (attempting to ~A)~@[~%~
                        (It has since been declared special)~]~"
          symbol attempt (getl symbol '(special system-constant))))

(defmethod (si::free-variable-reference :case :proceed-asking-user :use-dynamic-value)
           (cont ignore)
  (funcall cont :use-dynamic-value))
(defmethod (si::free-variable-reference :case :document-proceed-type :use-dynamic-value)
           (stream &optional ignore)
  (format stream "Use the dynamic (special) value of ~S. ~@[(Which is unbound!!)~]"
          symbol (and must-be-bound-p (not (boundp symbol)))))

(defmethod (si::free-variable-reference :case :proceed-asking-user :make-special) (cont ignore)
  (funcall cont :make-special))
(defmethod (si::free-variable-reference :case :document-proceed-type :make-special)
           (stream &optional ignore)
  (format stream "Make ~S special and use it. ~@[(Note that its dynamic value is unbound!)~]"
          symbol (and must-be-bound-p (not (boundp symbol)))))

;(defmethod (si::free-variable-reference :case :proceed-asking-user :add-lexical-binding)
;           (cont ignore)
;  (funcall cont :add-lexical-binding))
;(defmethod (si::free-variable-reference :case :document-proceeed-type :add-lexical-binding)
;           (stream)
;  (format stream "Create a lexical binding for ~S and use it." symbol))


(defflavor unclaimed-message (object message arguments) (error)
  :inittable-instance-variables :gettable-instance-variables)

(defmethod (unclaimed-message :report) (stream)
  (format stream "The object ~S received a ~S message, which went unclaimed.~% ~
                The rest of the message was ~:S."
          object message arguments))

(defmethod (unclaimed-message :case :proceed-asking-user :new-operation)
           (proceed-function read-argument-function)
  "Use another operation instead.  You specify the operation."
  (funcall proceed-function :new-operation
           (funcall read-argument-function :eval-read
                    "Form to evaluate to get operation to perform instead:~%")))

(defflavor arithmetic-error ((operands)) (error)
  :gettable-instance-variables
  :inittable-instance-variables)


(defflavor illegal-expt (base-number exponent) (arithmetic-error)
  (:inittable-instance-variables exponent)
  (:gettable-instance-variables exponent)
  (:init-keywords :base))
(defmethod (illegal-expt :after :init) (plist)
  (setq base-number (get plist ':base)))
(defmethod (illegal-expt :base) ()
  base-number)

(defmethod (illegal-expt :report) (stream)
  (format stream
          (if (eql exponent 0)
              "An attempt was made to raise zero to the power of a non-integer zero ~D"
              "An attempt was made to raise zero to power ~D, whose real part is not > 0")
          exponent))


;;; used by ecase, ccase, etypecase, ctypecase

(defflavor si::no-case-found (function) (proceed-with-value-mixin wrong-type-value)
  :gettable-instance-variables :initable-instance-variables)

(defmethod (si::no-case-found :report) (stream)
  (format stream
          "~~S failure; the value of ~S, ~S,~%is not ~A.~"
          function place value description))

(defmethod (si::no-case-found :case :document-proceed-type :new-value) (stream ignore)
  (format stream "Supply a new value for ~S and retry." place))


(compile-flavor-methods format-error warning
                        wrong-type-value bad-system-parameter
                        break
                        common-lisp-warning
                        redefinition
                        invalid-function
                        function-entry-error
                        too-few-arguments too-many-arguments
                        si::free-variable-reference
                        unclaimed-message
                        arithmetic-error
                        illegal-expt
                        si::no-case-found)
