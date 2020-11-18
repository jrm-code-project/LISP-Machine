;-*-Mode: Lisp; Package: SI; Base: 8; Lowercase: T-*-
;These things were written by RMS.  You can use them,
;if you return all improvements for redistribution.

;;; Record warnings about objects processed by file-transducers, etc.
;;; (primarily the compiler).

;To perform an operation on a file and report warnings on "objects" in it,
;do something like this:
;(FILE-OPERATION-WITH-WARNINGS (generic-pathname operation-name whole-file-p)
;   ... loop over the objects
;   (OBJECT-OPERATION-WITH-WARNINGS (object-name location-funcion)
;     ... do the operation, and maybe issue a warning with
;     (RECORD-WARNING type severity location-info format-string args...)
;     ...)
;   ...)

;Operation names include :COMPILE and :EVAL.
;Location-function and location-info are features not really used yet;
;just use nil for now.
;Severity is a keyword; the meanings of severities are not yet defined.
;Whole-file-p should eval to T if you are processing everything in the file.
;It tells the warnings system to assume that any objects you don't mention
;should have their warnings forgotten.


;Warnings about files are on the :warnings property of a generic pathname;
;all the warnings about all objects not in files
;go in the variable non-file-warnings-operation-alist.
(defvar non-file-warnings-operation-alist nil
  "Warnings datum for objects not in files.")

(defvar warnings-pathnames '(t)
  "All generic pathnames that have warnings, plus T for non-file objects.")

;These are used in printing out objects mentioned in warnings
(defconst warnings-prinlevel 4)
(defconst warnings-prinlength 4)

(defprop :compile "compilation" name-as-action)
(defprop :compile "compiling" name-as-present-participle)
(defprop :compile "compiled" name-as-past-participle)
(defprop :compile "compiler" name-as-agent)

(defprop :eval "evaluation" name-as-action)
(defprop :eval "evaluating" name-as-present-participle)
(defprop :eval "evaluated" name-as-past-participle)
(defprop :eval "evaluator" name-as-agent)

;Wherever found, the file-warnings-operation-alist is a list of file-warnings-datums,
;each recording the information about one kind of operation
;(eg, :COMPILE for compilation).
(defstruct (file-warnings-datum :list* (:conc-name file-warnings-)
                                (:constructor make-file-warnings (operation))
                                (:alterant nil))
  (operation nil :documentation "The file operation (such as :COMPILE) this is about")
  (editor-buffer nil :documentation "The editor buffer these warnings have been printed into")
  (object-alist nil :documentation "The alist of objects in the file and their warnings"))

;The object-alist is the warnings about that operation (such as, compilation)
;on objects in that file.  It is a list of object-warnings-datums.

;This data type records the warnings on one object (eg, one function) in a file
;(or maybe, not in a file).
(defstruct (object-warnings-datum :list*
                                  (:conc-name object-warnings-)
                                  (:constructor make-object-warnings
                                                (name location-function))
                                  (:alterant nil))
  (name nil :documentation "The name of the object this is about")
  (location-function nil :documentation
 "Information for finding this object's definition.
If NIL, use Edit Definition on the object name.
If any other symbol, use its :location-function property to visit the warning site(s)")
  (plist nil :documentation "Random other info, perhaps provided for the editor to use")
  (warnings nil :documentation "The warnings for this object"))

;This data type contains one warning.
;The type SI:PREMATURE-WARNINGS-MARKER
;(with severity NIL) is a marker that follows any premature warnings
;(for unnamed data before this object).
(defstruct (warning-datum :list* (:conc-name warning-)
                          (:constructor make-warning
                                    (type severity location-info format-string format-args))
                          (:alterant nil))
  (type nil :documentation "A keyword saying what the warning is about")
  (severity nil :documentation "A keyword giving the severity level of this warning")
  location-info
  ;; The next two are used for printing the warning.
  format-string
  format-args)

;Given a generic pathname, or t or nil for non-file objects,
;return the file-warnings-operation-alist for it.
(defun file-warnings-operation-alist (generic-pathname)
  "Returns the list of file-warnings-datums for the specified file.
There is a file-warnings-datum in the value for each operation
for which this file has any warnings.  You can SETF this."
  (if (memq generic-pathname '(t nil))
      non-file-warnings-operation-alist
    (send generic-pathname ':get ':warnings)))

(defdecl file-warnings-operation-alist setf
  ((file-warnings-operation-alist pn) . (set-file-warnings-operation-alist pn value)))

(defun set-file-warnings-operation-alist (generic-pathname new-alist)
  (and new-alist (not (memq generic-pathname warnings-pathnames))
       (push generic-pathname warnings-pathnames))
  (if (memq generic-pathname '(t nil))
      (setq non-file-warnings-operation-alist new-alist)
    (send generic-pathname ':putprop new-alist ':warnings)))

(defun examine-file-warnings (generic-pathname operation)
  "Return the file-warnings-datum for the specified file and operation, or NIL.
T or NIL as the pathname refers to non-file objects.
The operation is a keyword such as :COMPILE."
  (assq operation (file-warnings-operation-alist generic-pathname)))

(defun file-warnings-operations (generic-pathname)
  "Returns a list of all operations for which warnings are recorded for the specified file.
An operation is a keyword such as :COMPILE.
T or NIL used as an argument refers to non-file objects."
  (loop for oper in (file-warnings-operation-alist generic-pathname)
        when (file-warnings-object-alist oper)
        collect (car oper)))

(defun warnings-pathnames ()
  "Returns a list of all generic pathnames that have warnings recorded for them.
T or NIL as an element of the value refers to non-file objects."
  (subset #'file-has-warnings-p warnings-pathnames))

(defun file-has-warnings-p (generic-pathname)
  "Returns T if the specified file has any warnings recorded for it.
T or NIL as an argument refers to non-file objects."
  (loop for oper in (file-warnings-operation-alist generic-pathname)
        when (file-warnings-object-alist oper)
        return t))

;Copies the fixed part of an object-warnings-datum.
;This is the only part that is modified destructively.
;The warnings list itself is only pushed onto or changed wholesale.
(defun copy-object-warnings (object-warnings)
  (list* (first object-warnings) (second object-warnings)
         (copylist (third object-warnings))
         (object-warnings-warnings object-warnings)))

;; Macros for use by things that record warnings.


;This is the file-warnings-datum we are currently recording warnings in.
(defvar file-warnings-datum nil)

;This is the generic pathname we are recording warnings for a file operation on,
;or T if we are recording warnings for an object not associated with any file.
;or NIL if we are not set up for recording warnings about anything.
(defvar file-warnings-pathname nil)

;This is the link in the object-alist after which we are adding new objects.
;Everything up to here is the "front half" of the object alist.
;Everything after is the "back half".
;The back half is thrown away at the end of a whole-file operation.
(defvar file-warnings-pushing-location nil)

;This is a list of warnings recorded when there was no object set up to warn about.
;They are put here, and the next time an object is started, they are attached to it.
(defvar premature-warnings nil)

;This is a list of warnings recorded when there was no object set up,
;but which apply directly to the next object to be set up
;rather than to something anonymous that preceded it.
(defvar premature-warnings-this-object nil)

;Macros FILE-OPERATION-WITH-WARNINGS and NON-FILE-OPERATION-WITH-WARNINGS
;are in LMMAC.

;At the beginning of an operation on a file,
;make an object for warnings on this file and operation if there isn't one,
;and also initialize the list of objects we have had warnings on this time thru.
;Specify T or NIL as the pathname for a non-file-associated operation.
(defun begin-file-operation (generic-pathname operation-type
                             &aux (default-cons-area background-cons-area))
  (or generic-pathname (setq generic-pathname t))
  (or (eq file-warnings-pathname generic-pathname)
      (let ((file-warnings-operation-alist (file-warnings-operation-alist generic-pathname)))
        (or (memq generic-pathname warnings-pathnames)
            (push generic-pathname warnings-pathnames))
        (or (assq operation-type file-warnings-operation-alist)
            (progn
              (push (make-file-warnings operation-type) file-warnings-operation-alist)
              (set-file-warnings-operation-alist generic-pathname
                                                 file-warnings-operation-alist)))
        (setq file-warnings-datum (assq operation-type file-warnings-operation-alist))
        (setq file-warnings-pushing-location
              (locf (file-warnings-object-alist file-warnings-datum)))
        (setq file-warnings-pathname generic-pathname)
        (setq premature-warnings nil)
        (setq premature-warnings-this-object nil)
        t)))

;At the end of an operation on a file,
;flush the info on objects that didn't get warnings this time.
(defun end-file-operation ()
  (setf (cdr file-warnings-pushing-location) nil))

;Macro OBJECT-OPERATION-WITH-WARNINGS is in LMMAC.

;This is the object-name of the object we are currently recording warnings on,
;or NIL if we are not set up to record warnings on an object.
(defvar object-warnings-object-name nil)

;This is the location-info for the object we are going to record warnings on.
(defvar object-warnings-location-function nil)

;This is the object-warnings-datum in which we are recording warnings.
(defvar object-warnings-datum nil)

(defvar object-warnings-pushing-location nil)

;At the beginning of an operation on an object,
;see if there is already an object-warnings-datum for this object,
;left around from previous operations on it.
;Also, initialize that we have had no warnings yet this time.
(defun begin-object-operation (object-name location-function
                               &aux (default-cons-area background-cons-area))
  (if (or (equal object-warnings-object-name object-name)
          (null file-warnings-datum))
      nil
    (setq object-name (copytree object-name))  ;Avoid temp area lossage.
    (setq object-warnings-datum
          (assoc object-name (file-warnings-object-alist file-warnings-datum)))
    (if object-warnings-datum
        (setq object-warnings-pushing-location
              (locf (object-warnings-warnings object-warnings-datum)))
      (setq object-warnings-pushing-location nil))
    (setq object-warnings-object-name object-name)
    (setq object-warnings-location-function location-function)
    (cond (premature-warnings
           (print-object-warnings-header
             *standard-output* object-name
             (file-warnings-operation file-warnings-datum))
           (dolist (w (reverse premature-warnings))
             (apply 'record-warning w))
           (record-and-print-warning
             'premature-warning-marker nil nil
             (if (stringp object-name)
                 "The problems described above were encountered processing ~A."
               "The problems described above were in data preceding the definition of ~S.")
             object-name)))
    (cond (premature-warnings-this-object
           (unless premature-warnings
             (print-object-warnings-header
               *standard-output* object-name
               (file-warnings-operation file-warnings-datum)))
           (dolist (w (reverse premature-warnings-this-object))
             (apply 'record-warning w))
           ;; This need not be a warning at all,
           ;; since printing the warnings from the data base
           ;; will look just right with nothing here.
           (format t
                   (if (stringp object-name)
                       "~% The problems described above were encountered processing ~A."
                     (if premature-warnings
                         "~% Some of the problems apply to the definition of ~S."
                       "~% The problems described above apply to the definition of ~S."))
                   object-name)))
    (setq premature-warnings nil premature-warnings-this-object nil)
    t))

(defun print-object-warnings-header (stream object operation)
  (if (and (not (stringp object))
           (send stream ':operation-handled-p ':item))
      (progn (terpri stream)
             (send stream ':item 'zwei:function-name object
                   "<< While ~A ~S >>"
                   (get operation 'name-as-present-participle)
                   object))
    (format stream
            (if (stringp object)
                "~%<< While ~A ~A >>"
              "~%<< While ~A ~S >>")
            (get operation 'name-as-present-participle)
            object)))

(defun dispose-of-warnings-after-last-object ()
  (if (or premature-warnings premature-warnings-this-object)
      (object-operation-with-warnings
        ((string-append "the end of the data") 'zwei:go-to-end-of-file-possibility))))

;At the end of an object operation, get rid of any warnings
;that were left over from previous operations on this object.
;Furthermore, if there are now no warnings for this object,
;delete the object from the list for this file.
;In that case we must update file-warnings-pushing-location,
;since chances are it is the link that was deleted from the list.
(defun end-object-operation ()
  (cond (object-warnings-datum
         (if object-warnings-pushing-location
             (setf (cdr object-warnings-pushing-location) nil))
         (or (object-warnings-warnings object-warnings-datum)
             (progn
               (setf (file-warnings-object-alist file-warnings-datum)
                     (delq object-warnings-datum
                           (file-warnings-object-alist file-warnings-datum)))
               (if (eq (car file-warnings-pushing-location) object-warnings-datum)
                   (do ((l (locf (file-warnings-object-alist file-warnings-datum)) (cdr l)))
                       ((eq (cadr l) (cadr file-warnings-pushing-location))
                        (setq file-warnings-pushing-location l))))))))
  ;; Flush any warnings about INTERNALs of this object
  ;; that were not seen during this run.
  (dolist (objw (cdr file-warnings-pushing-location))
    (and (internal-object-of (car objw) object-warnings-object-name)
         (setf (cdr file-warnings-pushing-location)
               (delq objw (cdr file-warnings-pushing-location))))))

(defun internal-object-of (maybe-internal maybe-contains-it)
  (and (consp maybe-internal)
       (eq (car maybe-internal) ':internal)
       (or (equal (cadr maybe-internal) maybe-contains-it)
           (internal-object-of (cadr maybe-internal) maybe-contains-it))))

;Record a warning and print it too.  For an object's first warning,
;print the object's name as well.
(defun record-and-print-warning (type severity location-info format-string &rest format-args)
  "Enter a warning in the warnings data base, and print the warning too.
See RECORD-WARNING for calling information."
  (let ((default-cons-area working-storage-area))       ;Stream may cons
    (or (null object-warnings-object-name)
        (neq object-warnings-pushing-location
             (locf (object-warnings-warnings object-warnings-datum)))
        (print-object-warnings-header *standard-output*
                                      object-warnings-object-name
                                      (file-warnings-operation file-warnings-datum)))
    (terpri)
    (tyo #/space)
    (let ((prinlevel warnings-prinlevel)
          (prinlength warnings-prinlength))
      (apply 'format t format-string format-args))
    (apply 'record-warning type severity location-info format-string format-args)))

(defun maybe-print-object-warnings-header ()
  "If there is an object to record warnings on but no warnings yet, print <<While mumbling FOO>>."
  (or (null object-warnings-object-name)
      (neq object-warnings-pushing-location
           (locf (object-warnings-warnings object-warnings-datum)))
      (print-object-warnings-header *standard-output*
                                    object-warnings-object-name
                                    (file-warnings-operation file-warnings-datum))))

;Record a warning on the current object in the current file.
(defun record-warning (type severity location-info format-string &rest format-args
                       &aux (default-cons-area background-cons-area))
  "Enter a warning in the warnings data base.
The file and object should have been specified by using the macros
FILE-OPERATION-WITH-WARNINGS and OBJECT-OPERATION-WITH-WARNINGS.
TYPE and SEVERITY are keywords with no standard meanings.
FORMAT-STRING and FORMAT-ARGS are suitable for handing to FORMAT to print the warning."
  (if (null object-warnings-object-name)
      (push (make-warning type severity location-info format-string
                          (copylist format-args))
            premature-warnings)
    ;; Make sure we have ab object-warnings-datum for this object.
    (or object-warnings-datum
        (setq object-warnings-datum
              (make-object-warnings object-warnings-object-name
                                    object-warnings-location-function)
              object-warnings-pushing-location
              (locf (object-warnings-warnings object-warnings-datum))))
    ;; The first time we push a warning on an object,
    ;; make sure this object is in the front half of the file's object alist
    ;; (the half that will be kept after this file operation).
    (or (neq object-warnings-pushing-location
             (locf (object-warnings-warnings object-warnings-datum)))
        (progn
          ;; Delete it from the second half if it is there.
          (setf (cdr file-warnings-pushing-location)
                (delq object-warnings-datum (cdr file-warnings-pushing-location)))
          ;; If not present now, add to end of front half.
          (or (memq object-warnings-datum (file-warnings-object-alist file-warnings-datum))
              (progn
                (push object-warnings-datum (cdr file-warnings-pushing-location))
                (pop file-warnings-pushing-location)))))
    ;; Now push on this warning.
    (let ((warning (make-warning type severity location-info format-string
                                 (copylist format-args))))
      (push warning (cdr object-warnings-pushing-location))
      (pop object-warnings-pushing-location))))

;Filter all the warnings for a particular file, each according to the predicate
;associated with the operation.  Thus, :compile warnings are filtered by
;the definition of (:property :compile warnings-filtering-predicate).
(defun filter-warnings (generic-pathname)
  "Discard obsolete warnings for specified file from the data base."
  (dolist (op (file-warnings-operations generic-pathname))
    (let ((pred (get op 'warnings-filtering-predicate)))
      (if pred (filter-operation-warnings generic-pathname op pred)))))

;Discard any warnings for specified pathname and operation that do not match the predicate.
(defun filter-operation-warnings (generic-pathname operation predicate)
  (let ((file-warnings-datum (examine-file-warnings generic-pathname operation)))
    (dolist (objw (file-warnings-object-alist file-warnings-datum))
      ;; Any warnings which are about previously undefined functions that are now defined,
      ;; delete from the list of warnings about this object.
      (dolist (warn (object-warnings-warnings objw))
        (or (funcall predicate warn)
            (setf (object-warnings-warnings objw)
                  (delq warn (object-warnings-warnings objw)))))
      ;; If this object now has no warnings, flush it from the file.
      (or (object-warnings-warnings objw)
          (setf (file-warnings-object-alist file-warnings-datum)
                (delq objw (file-warnings-object-alist file-warnings-datum)))))))

;This predicate rejects warnings about formerly undefined functions
; which are no longer undefined.
(defun (:property :compile warnings-filtering-predicate) (warn)
  (not (and (eq (warning-type warn) 'compiler:undefined-function-used)
            (compiler:compilation-definedp (car (warning-format-args warn))))))

(defun print-warnings (pathnames stream)
  (dolist (file (or pathnames (warnings-pathnames)))
    (print-file-warnings file stream)))

(defun print-file-warnings (pathname &optional (stream *standard-output*))
  "Output warnings data base for one file to a stream, in machine-readable form."
  (if (stringp pathname)
      (setq pathname (fs:merge-pathname-defaults pathname)))
  (format stream "~&;-*-Mode: Lisp; Package: User; Base: 10. -*-")
  (format stream "~%(SI:RELOAD-FILE-WARNINGS~%  '~S~%  '(" pathname)
  (let ((generic-pathname (if (symbolp pathname) pathname (send pathname ':generic-pathname)))
        (*package* pkg-user-package)
        (*print-base* 10.) (*read-base* 10.)
        (*readtable* initial-readtable)
        file-vars
        file-vals
        (first-operation t))  ;T for the first operation in the operation-alist.
    ;; Get the file's property bindings, but use them only
    ;; when we construct the string which is the text of the warning.
    (multiple-value (file-vars file-vals)
      (and (not (symbolp generic-pathname))
           (fs:file-attribute-bindings generic-pathname)))
    (filter-warnings generic-pathname)
    (dolist (alist-elt (file-warnings-operation-alist generic-pathname))
      (if first-operation
          (setq first-operation nil)
        (format stream "~%    "))
      (format stream "(~S NIL" (car alist-elt))
      (dolist (objw (file-warnings-object-alist alist-elt))
        (apply 'format stream "~%     (~S ~S ~S" objw)
        (dolist (w (object-warnings-warnings objw))
          (multiple-value-bind (nil errorp)
              (catch-error
                (let ((print-readably t))
                  (print w 'si:null-stream)))
            (if errorp
                (format stream "~%      (~S ~S ~S /"~~A/" ~S)"
                        (first w) (second w) (third w)
                        ;; Instead of outputting the warning's format-string and args,
                        ;; run them through format now.  Avoid problems if there is an
                        ;; object in the args that can't print readably.
                        (progv file-vars file-vals
                               (apply 'format nil (fourth w) (nthcdr 4 w))))
              ;; If we can print the list itself so it will read back, do so.
              (format stream "~%      ~S" w))))
        (tyo #/) stream))
      (tyo #/) stream)))
  (format stream "))~%"))

(defun reload-file-warnings (pathname operation-alist)
  (set-file-warnings-operation-alist
    (if (symbolp pathname)
        pathname
      (send pathname ':generic-pathname))
    operation-alist))

(defun dump-warnings (output-file-pathname &rest warning-file-pathnames)
  "Write warnings data base to a file.  Read the file back with LOAD."
  (with-open-file (stream output-file-pathname ':direction ':output)
    (print-warnings warning-file-pathnames stream)
    (close stream)))
