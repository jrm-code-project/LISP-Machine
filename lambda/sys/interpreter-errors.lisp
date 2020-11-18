;;; -*- Mode:LISP; Package:EH; Base:10; Readtable:ZL -*-
;;;
;;;
;;; INTERPRETER ERRORS
;;;
;;;
;;;


;; WRONG NUMBER OF ARGUMENTS TO A FUNCTION
;;
;; Raised when the number of arguments passed to a function is less than the number
;; required or more than the number allowed.  The value returned is a list of arguments
;; that has an allowable length.
;;
;; Instance variables:
;;    function
;;       A lexical closure or a FEF, the function being applied.
;;    minimum
;;       The minimum number of arguments required by the function.
;;    maximum
;;       The maximum number of arguments allowed by the function.
;;    arglist
;;       List of arguments to the function.
;;
;; Proceed types:
;;    :truncate-argument-list [TOO-MANY-ARGUMENTS-ERROR only]
;;       Use only the first n arguments of arglist, where n = maximum.  Returns the new
;;       argument list.
;;    :new-argument-list >>NOT YET IMPLEMENTED
;;       Use a new argument list composed by evaluating forms typed by the user.  Returns
;;       the new argument list.
;;    :new-value
;;       From proceed-with-value-mixin.  Prompts the user for an expression to evaluate
;;       in place of the function application, and returns the expression.

(defflavor INTERPRETER::GET-NEW-ARG-LIST-MIXIN () ()
  (:required-flavors error)
  :abstract-flavor
  (:method-combination (:case :base-flavor-last
                         :proceed
                         :proceed-asking-user
                         :document-proceed-type)))

(defmethod (interpreter::get-new-arg-list-mixin :case :proceed-asking-user :new-argument-list)
           (continuation read-object-function)
  (declare (ignore continuation read-object-function))
  (error "whoops"))

(defmethod (interpreter::get-new-arg-list-mixin :case :document-proceed-type :new-argument-list)
           (stream &optional ignore)
  (format stream
          "Try again with new arguments.  You type expressions for them."))


(defflavor INTERPRETER::TOO-FEW-ARGUMENTS-ERROR
           (function minimum maximum arglist)
           (new-proceed-with-value-mixin error)
  :inittable-instance-variables
  :gettable-instance-variables)

(defmethod (interpreter::too-few-arguments-error :report) (stream)
  (format stream
          "Function ~S called with too few arguments; ~S provided, ~S required."
          (interpreter::name-of-closure function)
          (length arglist)
          minimum))


(defflavor INTERPRETER::TOO-MANY-ARGUMENTS-ERROR
           (function minimum maximum arglist)
           (new-proceed-with-value-mixin error)
  :inittable-instance-variables
  :gettable-instance-variables)

(defmethod (interpreter::too-many-arguments-error :report) (stream)
  (format stream
          "Function ~S called with too many arguments; ~S allowed, ~S provided."
          (interpreter::name-of-closure function)
          maximum
          (length arglist)))

(defmethod (interpreter::too-many-arguments-error
             :case :proceed-asking-user :truncate-argument-list)
           (continuation ignore)
  (send continuation
        :truncate-argument-list (ldiff arglist (nthcdr maximum arglist))))

(defmethod (interpreter::too-many-arguments-error
             :case :document-proceed-type :truncate-argument-list)
           (stream &optional ignore)
  (format stream
          "Apply the function again, dropping all but the first ~S argument~:P."
          maximum))



;; PROCEED-WITH-VALUE MIXIN
;;
;; Proceed types:
;;    :new-value
;;        Prompts the user for an expression to evaluate, then returns it.

(defflavor new-proceed-with-value-mixin () ()
  (:required-flavors error)
  :abstract-flavor
  (:method-combination (:case :base-flavor-last
                         :proceed
                         :proceed-asking-user
                         :document-proceed-type)))

(defmethod (new-proceed-with-value-mixin :case :proceed-asking-user :new-value)
           (continuation read-object-function)
  ; this is superseded by the :document-proceed-type
  ; "Returns a value, the value of an expression typed."
  ;
  ; The :eval-read should become a :read with an explicit eval to allow
  ; for the lexical environment
  (send continuation
        :new-value (send read-object-function
                         :eval-read "Form (NEW!) to evaluate and use: ")))

(defmethod (new-proceed-with-value-mixin :case :document-proceed-type :new-value)
           (stream &optional ignore)
  (format stream
          "Evaluate an expression you type, and use its value."))


;; EVALUATING A FREE VARIABLE
;;
;; Raised when an attempt is made to evaluate a free variable which has not been declared
;; special.  The value returned is used as the value of the free variable.
;;
;; Instance variables:
;;    symbol
;;       The symbol for the free variable.
;;
;; Proceed types:
;;    :new-value
;;       From proceed-with-value-mixin.  Prompts the user for a value, and returns it.
;;    :use-dynamic-value
;;       Use the dynamic value of the free variable.  A warning is made in advance if the
;;       dynamic value is unbound.  Returns the dynamic value.
;;    :make-special
;;       Proclaim the variable to be special and returns its dynamic value.  A warning
;;       is made in advance if the dynamic value is unbound.

(defflavor INTERPRETER::EVAL-FREE-VARIABLE-ERROR (symbol attempt)
           (new-proceed-with-value-mixin error)
  :inittable-instance-variables
  :gettable-instance-variables)

(defmethod (interpreter::eval-free-variable-error :report)
           (stream)
  (format stream
          "Attempt to evaluate the free variable ~S."
          symbol)
  (format stream
          "~&~S has no lexical binding and has not been declared special."
          symbol))

(defmethod (interpreter::eval-free-variable-error :case :proceed-asking-user :use-dynamic-value)
           (continuation ignore)
  "FOO"
  (funcall continuation :use-dynamic-value))

(defmethod (interpreter::eval-free-variable-error :case :document-proceed-type :use-dynamic-value)
           (stream &optional ignore)
  (format stream
          "Use the dynamic (special) value of ~S.~@[  (Which is unbound!!)~]"
          symbol
          (not (boundp symbol))))

(defmethod (interpreter::eval-free-variable-error :case :proceed-asking-user :make-special)
           (continuation ignore)
  (funcall continuation :make-special))

(defmethod (interpreter::eval-free-variable-error :case :document-proceed-type :make-special)
           (stream &optional ignore)
  (format stream
          "Proclaim ~S to be special, and use its dynamic (special) value.~@[  (Which is unbound!!)~]"
          symbol
          (not (boundp symbol))))
