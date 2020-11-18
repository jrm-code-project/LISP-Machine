;;; -*- Mode:LISP; Package:SITE-DATA-EDIT; Base:10; Readtable: CL -*-
;;; Copyright (c) Lisp Machine Inc., 1986.

;;; Mouse buttons stuff so you don't have to remember the byte constants.
(defvar *mouse-button* :unbound
  "The mouse key that invoked this command: one of :L, :M, :R")

(defconstant mouse-button-symbols #(:l :m :r) "Maps the byte field into the key")

(defun mouse-key-keyword-p (thing)
  (and (keywordp thing)
       (find thing mouse-button-symbols)))

(deftype mouse-key-keyword () '(satisfies mouse-key-keyword-p))

(defvar *mouse-n-clicks* :unbound
  "The number of mouse clicks that invoked this command: a number between one and three")

(deftype mouse-click-number () '(integer 1 3))

(defmacro mousecase (&rest clauses)
  "The element of each clause is a condition, which consists of either of a symbol (:L, :M :R)
representing the mouse button, a number representing the number of clicks, or a pair of the above,
\(in which case both conditions apply) or, finally and optionally, the symbol OTHERWISE (or T)."
  (cons 'cond
        (mapcar #'(lambda (clause)
                    (let ((thing (car clause)) (conditions '()))
                      (flet ((parse-condition (x)
                               (push (etypecase x
                                       (mouse-key-keyword `(eq *mouse-button* ,x))
                                       (mouse-click-number `(eq *mouse-n-clicks* ,x)))
                                     conditions)))
                        (cond ((member thing '(t otherwise)) (push 't conditions))
                              ((atom thing) (parse-condition thing))
                              ((and (consp thing) (= (list-length thing) 2))
                               (parse-condition (first thing))
                               (parse-condition (second thing)))
                              (t (error "Bad condition for ~S: ~S" 'mousecase thing))))
                      `((and ,@conditions) . ,(cdr clause))))
                clauses)))

(defmacro with-mousecase ((mouse-character) &body body)
  `(let ((*mouse-button* (svref mouse-button-symbols
                                (ldb tv::%%kbd-mouse-button ,mouse-character)))
         (*mouse-n-clicks* (+ 1 (ldb tv::%%kbd-mouse-n-clicks ,mouse-character)))) ; !!!
     ,@body))

;;; These might become instance variables of the window someday.

(defparameter default-font :default "Font for most data")

;;; I'd like to use tv:set-standard-font here, but it seems to be impossible to name new
;;; purposes after a screen is created !
(defparameter cue-font 'fonts:tr10i "Font for data format documentation")
(defparameter attribute-font 'fonts:tr12 "Font for printing the attribute name (not value)")
(defparameter unhighlighted-choice-font 'fonts:hl12
  "Font for an unchosen item of a multiple-choice item")
(defparameter highlighted-choice-font 'fonts:hl12b
  "Font for a chosen item of a multiple-choice item")

(defun object-editor-font-map ()
  (list default-font ;; this should be first, others are fairly arbritrary
        cue-font attribute-font unhighlighted-choice-font highlighted-choice-font))

(defflavor object-editor-window
           ((object nil)
            (deleted-attributes nil)
            (changed-attributes nil))  ; Also contains new attributes
           (tv:function-text-scroll-window
            tv:mouse-sensitive-text-scroll-window
;           tv:text-scroll-window-typeout-mixin
            tv:text-scroll-window
            tv:borders-mixin
            tv:top-label-mixin
            tv:flashy-scrolling-mixin
            tv:basic-scroll-bar
            tv:margin-scroll-mixin
            tv:margin-region-mixin
            tv:scroll-stuff-on-off-mixin
            tv:dont-select-with-mouse-mixin
            tv:window)
  (:default-init-plist
    :label '(:string "Empty" :centered)
    :blinker-p t
    :blinker-deselected-visibility :off
    :blinker-flavor 'tv:rectangular-blinker
    :deexposed-typeout-action :permit
    :flashy-scrolling-region '((20 0.30 0.70) (20 0.30 0.70))
    :margin-scroll-regions '((:top) (:bottom))
    :scroll-bar-always-displayed t
    :font-map (object-editor-font-map)
    :print-function 'print-data-line) ; see ATTRIBUTE
  (:gettable-instance-variables object)
  (:documentation "A flavor of window designed to edit attributes.
Incorporates everything needed for scroll items."))

(defmethod (object-editor-window :set-object) (new-object)
  "Set the objected edited to NEW-OBJECT, resetting the attribute edit history.
Any changes must be checked beforehand by the program."
  (setq object new-object changed-attributes () deleted-attributes ())
  (if object
      (send self :set-items (send object :editor-attributes))
    (send self :delete-all-items))
  (send self :update-edit-label))

(defmethod (object-editor-window :set-edit-label) (string)
  (send self :set-label `(:centered :string ,string)))

(defmethod (object-editor-window :update-edit-label) ()
  (send self :set-edit-label
        (if object
            (string-append "Editing " (send object :name-for-editor))
          "Empty")))

(defmethod (object-editor-window :adjustable-size-p) ()
  nil)

(defmethod (object-editor-window :enable-scrolling-p) ()
  tv::scroll-bar-always-displayed)

(defmethod (object-editor-window :delete-all-items) (&optional (index 0))
  (do ((max (array-leader items 0) (1- max)))
      ((< max index) nil)
    (send self :delete-item max)))

;;; There should be a standard function/message to get the item the mouse is pointing at.
;;; I had to look at the CHOOSE-VARIABLE-VALUES stuff to get this code.
(defmethod (object-editor-window :who-line-documentation-string) ()
  (multiple-value-bind (window-x-offset window-y-offset)
      (sheet-calculate-offsets self mouse-sheet)
    (let ((x (- tv::mouse-x window-x-offset))
          (y (- tv::mouse-y window-y-offset)))
      (multiple-value-bind (value type) (send self :mouse-sensitive-item x y)
        (when type
          (typecase value ; don't signal error here !
            (cons (send (car value) :documentation-string type))
            (instance (send value :documentation-string type))
            (t "Click left to change to a new value")))))))

(defvar *window* :unbound "The window in which the object is being edited.")

(defmacro catching-aborted-edit (&body body)
  "Catch all calls to ABORT-EDIT within BODY"
  `(catch 'abort-edit ,@body))

(defun abort-edit ()
  "Call this to abort an edit."
  (throw 'abort-edit nil))

;;; This is the main message.  Command loops should send edit blips here.
(defmethod (object-editor-window :edit-object) (type item mouse-character)
  (catch 'abort-edit
    (let ((*window* self))
      (with-mousecase (mouse-character)
        (multiple-value-bind (item note-p)
            (if (eq type 'attribute) ; returns different item than forms below
                (send item :edit 'attribute) ; will return correct value
              (values
                (etypecase item
                  (instance ; Just the attribute; :EDIT it.
                   (send item :edit type)
                   item)
                  (cons ; The real attribute and some other information as arguments
                   (lexpr-send (car item) :edit type (cdr item))
                   (car item)))
                t)) ; Normal edit, so note item as changed
          (when item
            (send self :edit-redisplay item)
            (when note-p
              (send self :note-changed-attribute item))))))))

(defmethod (object-editor-window :edit-redisplay) (thing)
  "Redisplay THING in the window."
  (let ((screen-index (- (if (numberp thing) thing (send self :number-of-item thing))
                         top-item)))
   (send self :redisplay screen-index (+ screen-index 1))))

(defun last-deleted-multiple-attribute-p (a)
  (and (send a :multiple-p) (send a :void-p)))

(defmethod (object-editor-window :changed-attributes) ()
  ;; The really changed ones don't include multiple-type attributes that set void by a delete
  ;; command, but then had to remain displayed because it was at least one attribute of that
  ;; type had to remain in the edit window.
  (subset-not #'last-deleted-multiple-attribute-p changed-attributes))

(defmethod (object-editor-window :deleted-attributes) ()
  (union deleted-attributes
         ;; Get the attributes that are deleted, but need to be in the changed list.
         (subset #'last-deleted-multiple-attribute-p changed-attributes)))

(defmethod (object-editor-window :note-changed-attribute) (attribute)
  (pushnew attribute changed-attributes))

(defmethod (object-editor-window :note-deleted-attribute) (attribute)
  (unless (send attribute :new-p)
    (pushnew attribute deleted-attributes)) ; don't need to note unless old
  (setq changed-attributes (delq attribute changed-attributes)))

(defmethod (object-editor-window :note-reverted-attribute) (attribute)
  ;; Can't revert new attributes, so we don't need to check.
  (setq changed-attributes (delq attribute changed-attributes)))

(defun editor-window-changed-object (object-editor-window)
  (declare (values changed-object))
  (let ((changed-object (send object-editor-window :object)))
    (and changed-object
         (or (send object-editor-window :deleted-attributes) (send object-editor-window :changed-attributes))
         changed-object)))

(defvar *auto-test-refresh* t)

;;; A simple test method.  Only understands attribute-editing blips.
(defmethod (object-editor-window :test-blip) ()
  (let ((thing (send self :list-tyi)))
    (send (third thing) :edit-object (first thing) (second thing) (fourth thing))
    (when *auto-test-refresh* (send self :refresh))))

;;; A useful test loop.
(defmethod (object-editor-window :loop) ()
  (with-selection-substitute (*terminal-io* self)
    (loop (send self :test-blip))))

(compile-flavor-methods object-editor-window)
