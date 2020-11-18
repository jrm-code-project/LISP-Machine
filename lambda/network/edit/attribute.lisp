;;; -*- Mode:LISP; Package:SITE-DATA-EDIT; Base:10; Readtable: CL -*-
;;; Copyright (c) Lisp Machine Inc., 1986.

;;; Used by the :PRINT-ITEM-LINE method of the edit windows; this puts out one line, letting the
;;; line itself take care of most of the hair.
(defun print-data-line (item -arg- window item-number)
  (declare (ignore -arg- item-number)) ; we don't use the PRINT-FUNCTION-ARG instance variable
  (send item :output window))

;;; Note that there's not neccessarily a one-to-one mapping between flavors and classes of
;;; attributes.  The flavors are mostly for sorting out the data typing cruft.
(defflavor attribute ((key nil) ; can be used for anything you want...
                      (name "Attribute") ; string for printing
                      (ok-if-void-p nil)
                      (prompt-string nil)
                      (new-p nil)
                      (delete-action nil) ; If something can be deleted, and this is non-NIL
                                          ; the value is SENT to attribute, with the editing
                                          ; window as an argument, instead of deleting
                                          ; the attribute from the list.  This is used to keep at
                                          ; at least one multiple attribute around for editting.
                      old-value)
           ()
  (:required-methods
    :output-value ; (WINDOW) output value onto the editor window, being mousable
    :cue-string ; a string which documents the format of the attribute
    :value      ; returns the actual top-level value of the attribute
    :set-value  ; sets the value
    :edit       ; frob the data, guiding the user (CASE method combination)
    )
  (:inittable-instance-variables key name ok-if-void-p prompt-string new-p
                                 delete-action old-value)
  (:settable-instance-variables delete-action)
  :gettable-instance-variables
  :abstract-flavor
  (:required-init-keywords :value)
  (:method-combination (:case :base-flavor-last :edit :documentation-string)
                       (:and :base-flavor-first :verify)
                       (:append :base-flavor-first :copy-init-plist))
  (:documentation "The basic attribute flavor.
It sets up the right method combination, and provides useful instance variables and default
methods."))

;;; Note that OLD-VALUE is canonicalized by the time anyone sees it.
(defmethod (attribute :after :init) (plist)
  (declare (ignore plist))
  (setq old-value (if new-p nil (copy-tree (send self :value)))))

(defmethod (attribute :print-self) (stream print-depth escape-p)
  (declare (ignore print-depth))
  (format stream "#<")
  (write (type-of self) :stream stream :escape escape-p)
  (format stream " Attribute [~A], " name)
  (send self :print-value stream escape-p)
  (write-string ">" stream))

(defmethod (attribute :print-value) (stream escape-p)
  (write-string "value " stream)
  (write (send self :value) :stream stream :escape escape-p))

(defmethod (attribute :void-p) ()
  "If non-NIL true, this attribute can be considered empty."
  (not (send self :value)))

;;; :multiple-p ; can this type appear more than once ?
;;; :verify     ; returns non-NIL if the line is semantically OK (AND method combination)
(defmethod (attribute :verify) () t)

;;; The :EDIT method is used is follows: the command loop gets a blip of the form
;;; (item-type item); if the item is an instance, (send item :edit item-type) happens;
;;; otherwise (lexpr-send (car item) :edit item-type (cdr item)) happens.
;;; This is how multiple-choice (like the CVV :ASSOC type) frobs are done.

;;; * Is the item well-formed ? (if possible for user only to complete part)
(defmethod (attribute :complete-p) () (not (send self :void-p)))

(defmethod (attribute :output-attribute) (window)
  (send window :set-current-font attribute-font)
  (write-string name window))

(defmethod (attribute :default-string) ()
  (if (send self :multiple-p) "New" "Default"))

(defmethod (attribute :output-empty-attribute) (window)
  (send self :output-void-item window))

;;; Output one mouseable thing.
(defmethod (attribute :output-void-item) (window)
  (send window :set-current-font unhighlighted-choice-font)
  (send window :item1 self 'void
        #'(lambda (ignore window default-string)
            (write-string default-string window))
        (if ok-if-void-p (send self :default-string) "Fill in")))

(defun write-cue-string (string window)
  "Write a string in the cue font.  Does NOT restore the current font."
  (send window :set-current-font cue-font)
  (write-string string window))

;;; The method called by the window's top-level item output function.
(defmethod (attribute :output) (window)
  (send window :item1 self 'attribute #'(lambda (x window) (send x :output-attribute window)))
  (write-string ": " window)
  (write-cue-string (send self :cue-string) window)
  (write-string " " window)
  (send window :clear-rest-of-line) ; Grumble, :REDISPLAY on the window doesn't seem to do this.
  (if (send self :void-p)
      (send self :output-empty-attribute window)
    (progn
      (send window :set-current-font default-font)
      (send self :output-value window))))

(defmethod (attribute :documentation-string) (type)
  (declare (ignore type))
  "Click left to edit the value")

(defmethod (attribute :case :documentation-string attribute) ()
  (if (send self :multiple-p)
      "Click left to add a line, middle to delete it, right to revert it"
    "Click right to revert the value"))

(defmethod (attribute :case :edit attribute) ()
  (mousecase
    (:l (send self :edit-new *window*))
    (:m (send self :edit-delete *window*))
    (:r (send self :edit-revert *window*))))

(defmethod (attribute :case :documentation-string void) ()
  "Click to start a new one")

(defmethod (attribute :prompt-string) ()
  (or prompt-string (send self :name)))

(defmethod (attribute :editing-type) () (type-of self))

(defmethod (attribute :case :edit void) ()
  (send self :edit (send self :editing-type)))

(defmethod (attribute :multiple-p) ()
  nil)

;;; This should return either NIL or a string to be edited
(defmethod (attribute :value-string) ()
  ())

;;; The following three methods make good values for DELETE-ACTION.  :DELETE-FROM-WINDOW is the
;;; default.  The method should return an attribute to redisplay (usually self) or NIL.
;;; If the second value is non-NIL, then mark the attribute as changed.  It is thus possible
;;; for a changed attribute to be void, if it held a real value when it was created.
(defmethod (attribute :reset-value) (ignore)
  (send self :set-value nil)
  (values self
          (not new-p)))

(defmethod (attribute :do-not-delete) (ignore)
  ())

(defmethod (attribute :delete-from-window) (window)
  (send window :delete-item (send window :number-of-item self))
  (send window :note-deleted-attribute self)
  ())

(defmethod (attribute :edit-delete) (window)
  (when ok-if-void-p
    (send self (or delete-action :delete-from-window) window)))

(defmethod (attribute :copy-init-plist) ()
  '())

(defmethod (attribute :edit-new) (window)
  (when (send self :multiple-p)
    (let ((new (apply #'make-instance (type-of self)
                                      :value nil :name name :ok-if-void-p t :key key :new-p t
                      (send self :copy-init-plist))))
      (let ((index (send window :number-of-item self)))
        (if (= index (+ 1 (send window :number-of-items)))
            (send window :append-item new)
          (send window :insert-item (+ index 1) new)))
      (values new nil))))

(defmethod (attribute :after :edit) (&rest ignore)
  (send self :note-edit *window*))

;;; This should do anything out of the ordinary to update the display, other objects, etc.
;;; It is called by the :AFTER :EDIT method of attributes.
(defmethod (attribute :note-edit) (window)
  (declare (ignore window))
  nil)

(defmethod (attribute :edit-revert) (window)
  (unless new-p
    (send self :set-value (copy-tree old-value))
    (send self :note-edit window)
    (send window :note-reverted-attribute self)
    (values self ())))

(defflavor value-variable-mixin ((value nil)) ()
  (:required-flavors attribute)
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  :abstract-flavor
  (:documentation "Mix this in if you want the instance variable VALUE"))

(defmethod (value-variable-mixin :void-p) ()
  (null value))

(defflavor value-method-mixin () ()
  (:required-flavors attribute)
  (:required-methods :value :set-value)
  (:init-keywords :value)
  :abstract-flavor
  (:documentation
    "Use this, and define :VALUE and :SET-VALUE; initialization is taken care of by :SET-VALUE"))

(defmethod (value-method-mixin :before :init) (plist)
  (send self :set-value (get plist :value)))

;;; Pretty names: a few words on one line, case preserved
(defflavor one-line-string-attribute () (value-variable-mixin attribute)
  :abstract-flavor
  (:method-combination (:pass-on (:base-flavor-last string) :verify-string))
  (:required-methods :legal-character-bit-vector))

(defmethod (one-line-string-attribute :cue-string) () "Token")

(defun make-initial-character-bit-vector (lo upto)
  "Make a bit map that allows all characters except LO and UPTO
The character codes serve as the indices in the vector; non-zero elements mark the legal
characters."
  (let ((s (zl:make-array char-code-limit :element-type '(mod 2) :area permanent-storage-area)))
    (loop for i from (char-code lo) below (char-code upto)
          do (setf (aref s i) 1))
    s))

(defun verify-string (string vector)
  "Non-NIL if STRING does not contain any characters deemed illegal by the bit vector."
  (dotimes (i (string-length string) t)
    (when (zerop (aref vector (zl:aref string i)))
      (return nil))))

(defmethod (one-line-string-attribute :and :verify) ()
  (or (null value)
      (verify-string value (send self :legal-character-bit-vector))))

(defmethod (one-line-string-attribute :value-string) ()
  (send self :value))

;;; The edit method is passed the typeout item type as the first argument.
(defmethod (one-line-string-attribute :case :edit name) ()
  (if (send self :operation-handled-p :choose)
      (send self :choose)
    (send self :line-edit :initial-input (send self :value-string))))

(defmethod (one-line-string-attribute :verify-string) (string) string)

;;; This should either return a string or abort out with the ABORT-EDIT function.
(defmethod (one-line-string-attribute :pass-on :verify-string) (string)
  (if (verify-string string (send self :legal-character-bit-vector))
      string
    (abort-edit)))

(defmethod (one-line-string-attribute :line-edit) (&rest args)
  "The standard way of editing a string attribute."
  (setq value (send self :verify-string (lexpr-send self :line-edit-string args))))

(defmethod (one-line-string-attribute :line-edit-string) (&key initial-input)
  "The standard way of editing a string attribute."
  (let ((me self)) ;> FMH !
    (condition-case (result)
        (with-input-editing (*query-io* `((:initial-input ,initial-input)))
          (prompt-and-read :string-or-nil "~&~A: " (send me :prompt-string))) ;> FMH !
      ((sys:abort) (abort-edit))
      (:no-error (or result (abort-edit))))))

(defmethod (one-line-string-attribute :output-value) (window)
  (send window :item1 self 'name #'(lambda (x window) (send x :write-string window))))

(defmethod (one-line-string-attribute :write-string) (window)
  (send window :string-out value))

(defflavor pretty-name () (one-line-string-attribute))

(defparameter pretty-name-bit-vector
              (make-initial-character-bit-vector #\Center-Dot (make-char #o200)))

(defmethod (pretty-name :legal-character-bit-vector) ()
  pretty-name-bit-vector)

(defmethod (pretty-name :cue-string) () "String")

(defflavor ascii-string () (one-line-string-attribute)
  (:documentation "Good defense against braindamaged network software"))

(defparameter ascii-string-bit-vector (make-initial-character-bit-vector #\Space #\Integral))

(defmethod (ascii-string :legal-character-bit-vector) ()
  ascii-string-bit-vector)

(defmethod (ascii-string :cue-string) () "String")

(defflavor atomic-name () (one-line-string-attribute)
  (:default-init-plist :name "Name")
  (:documentation
    "For an `atomic' string of alphanumerics, and some other characters like - and _ and ."))

(defmethod (atomic-name :cue-string) () "Token")

(defmethod (atomic-name :pass-on :verify-string) (string)
  (string-upcase string))

(defun make-atomic-name-bit-vector ()
  (let ((v (make-initial-character-bit-vector #\- #\`))) ; get upper case and some punctuation
    ;; Nuke other random characters in that range
    (map () #'(lambda (c) (setf (aref v (char-code c)) 0)) "/:;<>=@?")
    v))

(defparameter atomic-name-bit-vector (make-atomic-name-bit-vector))

(defmethod (atomic-name :legal-character-bit-vector) ()
  atomic-name-bit-vector)

;;; For usually restricted choices, but still allowing other choices
(defflavor choice-or-any (parse-function) (value-variable-mixin attribute)
  :inittable-instance-variables
  :gettable-instance-variables
  :abstract-flavor
  (:required-methods :choices :output-choice)
  (:documentation "Use this to choose from a set of objects, or from the keyboard.
If the keyboard is used, the input is parsed (from a string) by PARSE-FUNCTION."))

(defmethod (choice-or-any :append :copy-init-plist) ()
  (list :parse-function parse-function))

;;; This really ought to be in the system.
(defwindow-resource temporary-menu ()
  :make-window (temporary-menu)
  :initial-copies 1
  :reusable-when :deexposed)

(defun choose-or-read (choices parse-function prompt-string)
  (using-resource (menu temporary-menu)
    (send menu :set-item-list
          (append choices '(("" :no-select t)
                            ("From the keyboard" :value .read. :font :menu-standout)
                            ("Abort" :value .abort. :font :menu-standout))))
    (send menu :set-label prompt-string)
    (unwind-protect
        (progn
          (tv:expose-window-near menu '(:mouse))
          (let ((result (send menu :choose)))
            (case result
              (.read.
               (let ((string (zwei:typein-line-readline-near-window
                               menu "~40A" (string-append prompt-string #\:))))
                 (when string
                   (funcall parse-function string))))
              (.abort. (abort-edit))
              (otherwise result))))
      (send menu :deactivate))))

(defmethod (choice-or-any :editing-type) () 'choice-or-any)

(defmethod (choice-or-any :output-value) (window)
  (send window :item1 self (send self :editing-type)
        #'(lambda (x window) (send x :output-choice window))))

(defmethod (choice-or-any :case :documentation-string choice-or-any) ()
  "Click left for a menu, or to use the keyboard")

(defmethod (choice-or-any :case :edit choice-or-any) ()
  (setq value (choose-or-read (send self :choices) parse-function (send self :prompt-string))))

(defun string-to-keyword (string)
  (let ((*package* (find-package 'keyword)))
    (read-from-string string)))

(defvar *restriction-alist* ())

(defun get-restriction (class)
  (cdr (assoc class *restriction-alist*)))

(defun add-restrictions (class &rest suggestions)
  "Associates CLASS (a symbol) with other keywords that are good values for this class."
  (let ((entry (assoc class *restriction-alist*)))
    (if entry
        (setf (cdr entry) (union suggestions (cdr entry)))
      (push (cons class (copy-list suggestions)) *restriction-alist*))))

(defflavor restricted-keyword (restriction) (choice-or-any)
  (:default-init-plist :parse-function 'string-to-keyword)
  :inittable-instance-variables
  (:documentation "Allows choices from a menu of a keyword, but the user can still type one in.
The RESTRICTION is a symbol that is the name of the class; see the function ADD-RESTRICTIONS."))

(defmethod (restricted-keyword :append :copy-init-plist) ()
  (list :restriction restriction))

(defmethod (restricted-keyword :choices) ()
  (get-restriction restriction))

(defmethod (restricted-keyword :cue-string) () "Keyword")

(defmethod (restricted-keyword :void-p) () (not value))

(defmethod (restricted-keyword :output-choice) (window)
  (send window :string-out (symbol-name value)))

;;; CHOICE is alist of strings and values.
(defflavor choice (cached-choices) (value-variable-mixin attribute)
  (:init-keywords :choices)
  (:documentation "This is much like the :ASSOC type of choose-variable.
The CHOICES are an alist of strings and objects to choose."))

(defmethod (choice :cue-string) () "Choice")

(defmethod (choice :append :copy-init-plist) ()
  (list :choices (mapcar #'second cached-choices)))

(defmethod (choice :before :init) (plist)
  (setq cached-choices (mapcar #'(lambda (c) (list self c)) (get plist :choices))))

(defmethod (choice :output-value) (window)
  (do* ((choices cached-choices (cdr choices))
        (c (first choices) (first choices)))
       ((null choices))
    (send window :set-current-font (if (eql (cdr (second c)) value)
                                       highlighted-choice-font
                                     unhighlighted-choice-font))
    (send window :item1 c 'choice #'(lambda (x window)
                                         (send window :string-out (car (second x)))))
    (when (rest choices)
      (send window :tyo #\Space))))

(defmethod (choice :case :edit choice) (new-item)
  (setq value (cdr new-item)))

(defmethod (choice :case :documentation-string choice) ()
  "Click here to select this value")

(defflavor boolean () (choice)
  (:default-init-plist :choices '(("Yes" . t) ("No" . nil))))

(defmethod (boolean :void-p) () nil)

;;; Random tuple data-type -- Not complete yet.
(defflavor n-tuple (n cue-strings item-types) (value-variable-mixin attribute)
  :inittable-instance-variables
  :gettable-instance-variables
  (:required-init-keywords :n :cue-strings :item-types)
  (:method-combination (:case :base-flavor-last :output-element)))

(defmethod (n-tuple :before :init) (ignore)
  (check-type n (integer 1))
  (unless (= n (length cue-strings) (length item-types))
    (error "Length of element descriptors are mismatched: ~S and ~S" cue-strings item-types)))

(defmethod (n-tuple :after :init) (ignore)
  (unless value
    (setq value (make-list n :initial-element nil))))

(defmethod (n-tuple :cue-string) () "Items")

(defmethod (n-tuple :void-p) ()
  (some #'identity value))

(defmethod (n-tuple :complete-p) ()
  (every #'identity value))

(defmethod (n-tuple :output-element) (type element window)
  (declare (ignore type))
  (princ element window))

(defmethod (n-tuple :output-value) (window)
  window)

(defflavor user-property () (value-variable-mixin attribute)
  (:default-init-plist :ok-if-void-p t :name "Property")
  (:documentation "Represents an arbitrary property.
This consists of a keyword (stored in the KEY) instance variable and a value, which can be
any Lisp object."))

(defmethod (user-property :cue-string) () "Pair")

(defmethod (user-property :multiple-p) () t)

(defmethod (user-property :void-p) () (null key))

(defmethod (user-property :output-value) (window)
  (send window :set-current-font cue-font)
  (write-string "Indicator " window)
  (send window :set-current-font default-font)
  (send window :item1 self 'property-indicator #'(lambda (ignore w i) (princ i w)) key)
  (write-char #\Space window)
  (send window :set-current-font cue-font)
  (write-string "Value " window)
  (send window :set-current-font default-font)
  (send window :item1 self 'property-value #'(lambda (ignore w v) (prin1 v w)) value))

(defmethod (user-property :property) () key)

(defun call-with-standard-lisp-io-parameters (thunk)
  (let ((*print-level* nil) (*print-length* nil) (*print-base* 10) (*read-base* 10)
        (*print-pretty* t) (*print-array* t) (*print-circle* t) (*print-case* :upcase))
    (funcall thunk)))

(defmacro with-standard-lisp-io-parameters (&body body)
  `(call-with-standard-lisp-io-parameters #'(lambda () (declare (sys:downward-function)) ,@body)))

(defun read-lisp-object (prompt &key (type t) default)
  (fresh-line *query-io*)
  (with-standard-lisp-io-parameters
    (let ((rh-options (when default `((:initial-input ,(prin1-to-string default))))))
      (condition-case (value)
          (flet ((input ()
                        (with-input-editing (*query-io* rh-options)
                          (prompt-and-read :read "~A: " prompt))))
            (do ((x (input) (input)))
                ((and (typep x type)
                      (let ((*print-length* 5) (*print-level* 3))
                        (y-or-n-p "The object is ~S, OK ?" x)))
                 x)
              (format *query-io* "~&Please supply ~A.~%" (si::type-pretty-name type))))
        ((sys:abort) (abort-edit))))))

(defmethod (user-property :case :documentation-string property-indicator) ()
  "Click to edit the property name")

(defmethod (user-property :case :edit property-indicator) ()
  (setf key (intern (symbol-name (read-lisp-object "Indicator" :type 'symbol :default key))
                    'keyword)))

(defmethod (user-property :case :edit property-value) ()
  (setf value (read-lisp-object "Value (a Lisp object)" :default value)))

(defmethod (user-property :case :documentation-string property-indicator) ()
  "Click to edit the name of the property")

(defmethod (user-property :case :edit user-property) ()
  (send self :edit 'property-indicator)
  (send self :edit 'property-value))

(defun make-list-of-attributes (flavor values &rest init-options)
  "This makes a list of attribute lines of FLAVOR, for each VALUE.
INIT-OPTIONS can also be passed along.  If VALUES is NIL, one new attribute is made."
  (if values
      (let ((list (mapcar #'(lambda (value)
                              (apply #'make-instance flavor :value value init-options))
                          values)))
        (send (car list) :set-delete-action :reset-value)
        list)
    (ncons (apply #'make-instance flavor :value nil :ok-if-void-p t :new-p t
                  :delete-action :reset-value init-options))))

(compile-flavor-methods pretty-name ascii-string atomic-name restricted-keyword choice boolean
                        user-property)
