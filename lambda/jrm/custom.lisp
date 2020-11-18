;;; -*- Mode:LISP; Readtable:CL; Base:10 -*-

;;; Dependancies: ()

;;; ENVIRONMENTAL IMPACT STATEMENT
;;;
;;; This program was written on recycled memory.
;;; No cons cells were created.
;;;

;;; JRM's customization.

;;; DEFSYNONYM

(defmacro defsynonym (new-name old-name)
  "New-name is a subst for old-name.  Uses rest arg so be careful."
  `(DEFSUBST ,new-name (&REST ARGUMENTS)
     (APPLY (FUNCTION ,old-name) ARGUMENTS))
  )

;;; P REMOVAL
(defsynonym array-in-bounds? array-in-bounds-p)
(defsynonym bound?  boundp)
(defsynonym character? characterp)
(defsynonym eq?     eq)
(defsynonym equal?  equal)
(defsynonym list?   listp)
(defsynonym minus?  minusp)
(defsynonym null?   null)
(defsynonym number? numberp)
(defsynonym pair?   consp)
(defsynonym string? stringp)
(defsynonym symbol? symbolp)
(defsynonym zero?   zerop)

;;; DEFEQ-TEST
(defmacro defeq-test (name symbol1)
  "EXPANDS (NAME SOMETHING) into (EQ? symbol1 SOMETHING)."
  `(DEFSUBST ,name (SYMBOL2)
     (EQ? SYMBOL2 ,symbol1)))

;;; RANDOMNESS
(defun logic-has-fallen? ()
  "T if and only if false."
  (null? t))

;;; VECTOR MAP FUNCTIONS
(defun for-indicies-in-vector (vector proc)
  "(FUNCALL PROC INDEX) for each index in order."
  (dotimes (index (length vector))
    (funcall proc index)))

(defun for-elements-in-vector (vector proc)
  "(FUNCALL PROC INDEX (ELT VECTOR INDEX)) for each element in the vector."
  (for-indicies-in-vector
    vector
    #'(lambda (index)
        (funcall proc index (elt vector index)))))

;;; SEQUENCES
(defun sequence->list (sequence)
  "Coerce a sequence into a list."
  (map 'list #'identity sequence))

;;; ALIKE
;;; A hack version of equal with a little winnage.

(defvar *alike-data-base* '()
  "Holds a mapping of type-predicates to alike-predicates")

(defmacro defalike (predicate alike-test)
  "Define a mapping between a predicate and an alikeness-test for ALIKE?
Note that alike assumes a flat, disjoint typespace so you will lose if
you are not careful."
  `(PUSH (cons ,predicate ,alike-test)
         *ALIKE-DATA-BASE*))

(defun alike? (object1 &rest objects)
  "Compare some objects using predicates from the *ALIKE-DATA-BASE*."
  (let ((predicate-test-pair (assoc object1 *ALIKE-DATA-BASE*
                                    :test #'(lambda (item pred) (funcall pred item)))))
    (if (null? predicate-test-pair)
        (ferror nil "No known alike test for object ~S" object1))
    (let ((predicate (car predicate-test-pair))
          (alike-test (cdr predicate-test-pair)))
      (dolist (other-object objects)
        (unless (and (funcall predicate other-object)
                     (funcall alike-test object1 other-object))
          (return-from alike? nil)))
      t)))

(defalike #'symbol? #'eq?)

(defun alike-pair? (pair1 pair2)
  (and (alike? (car pair1) (car pair2))
       (alike? (cdr pair1) (cdr pair2))))

(defalike #'pair?   #'alike-pair?)
(defalike #'number? #'=)
(defalike #'string? #'string=)

;;; U-NEQ
;;; Makes an object eq? to nothing but itself.

(defsynonym symbol->string symbol-name)
(defsubst string->symbol (string)
  (make-symbol string t))

(defmacro u-neq (tag-name &optional (documentation "A unique object."))
  "Create a unique object and DEFCONSTANT TAG-NAME to it."
  `(DEFCONSTANT ,tag-name (STRING->SYMBOL (SYMBOL->STRING (QUOTE ,tag-name))) ,documentation))

;;; DEFCONSTANT-NOW
;;; Like DEFCONSTANT, but the variable is bound at compile time.

(defmacro defconstant-now (name value &optional documentation)
  "Like DEFCONSTANT, but value is bound at compile time."
  `(PROGN (EVAL-WHEN (COMPILE) (PROCLAIM (QUOTE (SPECIAL ,name))))
          (EVAL-WHEN (COMPILE LOAD EVAL) (DEFPROP ,name T SYSTEM:SYSTEM-CONSTANT))
          (EVAL-WHEN (COMPILE LOAD EVAL) (SI::DEFCONST-1 ,name ,value ,@(if documentation (list documentation) nil)))))

;;; BINARY-FUNCTION->LEFT-ASSOCIATING-LEXPR

(defun left-associating-reduce (function base-case argument-list)
  (do ((answer  base-case     (funcall function answer (first sublist)))
       (sublist argument-list (rest sublist)))
      ((null? sublist) answer)))

(defun binary-function->left-associating-reducer (binary-function &optional (unary-function ':none))
  #'(lambda (base-case &rest arguments)
      (cond ((not (null? arguments))
               (left-associating-reduce binary-function base-case (copylist arguments)))
            ((eq? unary-function ':none)
               (signal 'sys:too-few-arguments :function binary-function :argument-list (list base-case)))
            ('else (funcall unary-function base-case)))))

(defun binary-function->left-associating-lexpr (binary-function base-case)
  #'(lambda (&rest arguments)
      (left-associating-reduce binary-function base-case (copylist arguments))))

;;; COMPOSE

(defun multiple-value-compose-1 (receiver transmitter)
  "Call receiver on values of transmitter."
  #'(lambda (&rest args)
      (setq args (copylist args))
      (multiple-value-call
        receiver
        (apply transmitter args))))

(defun compose (&rest functions)
  (cond ((null? functions)       #'values)
        ((null? (cdr functions)) (car functions))
        ('else (left-associating-reduce #'multiple-value-compose-1 (car functions) (copylist (rest functions))))))

;;; VALUES-VALUES
(defmacro values-values (&rest forms)
  "Returns multiple values from FORMS returning multiple values."
  `(MULTIPLE-VALUE-CALL
     #'VALUES
     ,@forms))

;;; FORMAT-UNREADABLE

(defun format-unreadable-object (stream thunk)
  "Wrap a #< > around what the thunk prints."
  (si:printing-random-object (nil stream :no-pointer)
    (funcall thunk)))

(defun print-random-object (object stream ignore)
  (si:printing-random-object (object stream :type)))

;;; CHECK-SPLIT-LIST

(defun check-split-list (l if-empty if-not)
  "Funcall if empty if the list is nil,
funcall if-not on the car and cdr if it is not empty."
  (if (null? l)
      (funcall if-empty)
      (funcall if-not (first l) (rest l))))

;;; Defstruct standard.

(defmacro define-standard-structure (name &rest elements)
  (declare (zwei:indentation 1 2))
  `(defstruct (,name (:callable-accessors t)
                     (:conc-name ,(string->symbol (string-append (symbol->string name) "-")))
                     (:print-function print-random-object))
     ,@elements))

;;; TIMING
(defun measuring-elapsed-time (thunk-to-time receiver)
  (let ((begin-time (time)))
    (multiple-value-prog1
      (funcall thunk-to-time)
      (funcall receiver (time-difference (time) begin-time)))))

;;; End of random customization.
