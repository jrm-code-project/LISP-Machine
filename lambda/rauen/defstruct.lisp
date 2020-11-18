;;; -*- Mode:LISP; Package:DEFSTRUCT; Base:10; Readtable:CL -*-
;;;
;;;
;;; DEFSTRUCT.LISP
;;;
;;;
;;; The DEFSTRUCT macro expands into a PROGN form which contains a whole bunch
;;; of forms (herein called "tasks") that do the dirty work of the DEFSTRUCT.
;;; The macro itself calls several parsers to parse the DEFSTRUCT body, creates
;;; a procedure to incrementally build up a list of tasks, calls MAKE-DEFSTRUCT-
;;; TASKS to make the tasks, and constructs the PROGN form.  Most of the gory
;;; work is done by MAKE-DEFSTRUCT-TASKS and the functions it calls.

;;; Problems:
;;;   Print functions are still somewhat broken.  DEFTYPE in expansion raises an
;;; error if something different has been DEFTYPEd with the same name.  This
;;; might be the right thing, though.

(shadowing-import 'k-lisp:defstruct)
;(shadowing-import 'k-lisp:structure)


(defvar *defined-structures* NIL
  "Table of all structure types defined by DEFSTRUCT")

(defmacro defstruct (name-and-options &body documentation-and-slot-descriptions)
  (let ((tasks NIL))
    (flet ((push-task (task)
             (setq tasks (cons task tasks))))
      (multiple-value-bind (name options doc-string slots)
          (parse-defstruct name-and-options documentation-and-slot-descriptions)
        (push-task `(SETF (DOCUMENTATION ',name :STRUCTURE) ,doc-string))
        (let ((parsed-slots (mapcar #'parse-slot slots)))
          (make-defstruct-tasks name options parsed-slots #'push-task))
        `(PROGN
           ,@(reverse tasks)
           ',name)))))


(defun parse-defstruct (name-and-options documentation-and-slot-descriptions)
  (let (name options doc-string slots)
    (etypecase name-and-options
      (symbol (setq name name-and-options)
              (setq options NIL))
      (list   (setq name (first name-and-options))
              (setq options (rest name-and-options))))
    (if (stringp (car documentation-and-slot-descriptions))
        (progn (setq doc-string (car documentation-and-slot-descriptions))
               (setq slots      (cdr documentation-and-slot-descriptions)))
        (progn (setq doc-string NIL)
               (setq slots      documentation-and-slot-descriptions)))
    (values name options doc-string slots)))


(defun make-defstruct-tasks (name options slots push-task-proc)
  (flet ((push-task (task)
           (funcall push-task-proc task)))
    (let (
          (conc-name        (concatenate 'string (symbol-name name) "-"))
          (constructor      (intern (concatenate 'string "MAKE-" (symbol-name name)) *package*))
          (copier           (intern (concatenate 'string "COPY-" (symbol-name name)) *package*))
          (predicate        (intern (concatenate 'string (symbol-name name) "-P")    *package*))
          (BOA-constructors NIL)
          (inclusions       NIL)
          (print-function   NIL)        ;default?
          (type             NIL)
          (namedp           :IMPLIED)
          (predicatep       :IMPLIED)
          (initial-offset-p NIL)
          (initial-offset   0)
          (net-offset       NIL)
          )
      (dolist (option options)
        (let (option-type option-args first-arg)
          (etypecase option
            (symbol (setq option-type option)
                    (setq option-args NIL)
                    (setq first-arg   NIL))
            (list   (setq option-type (first option))
                    (setq option-args (rest option))
                    (setq first-arg   (car option-args))))
          (case option-type

            (:conc-name
             (if first-arg
                 (setq conc-name (symbol-name first-arg))
                 (setq conc-name "")))

            (:constructor
             (case (length option-args)
               (1         (if (symbolp first-arg)
                              (setq constructor first-arg)
                              (ferror "~S is not a valid name for a constructor function." first-arg)))
               (2         (push option-args BOA-constructors))
               (otherwise (ferror ":CONSTRUCTOR must have 1 or 2 arguments."))))

            (:copier
             (when option-args
               (if first-arg
                   (setq copier first-arg)
                   (setq copier NIL))))

            (:predicate
             (when option-args
               (if first-arg
                   (progn (setq predicate first-arg)
                          (setq predicatep T))
                   (progn (setq predicate NIL)
                          (setq predicatep NIL)))))

            (:include
             (if inclusions
                 (ferror "More than 1 :INCLUDE argument in DEFSTRUCT.")
                 (setq inclusions option-args)))

            (:print-function
             (setq print-function first-arg))

            (:type
             (if (or (eq first-arg 'VECTOR)
                     (eq first-arg 'LIST)
                     (and (listp first-arg) (= (length first-arg) 2) (eq (car first-arg) 'VECTOR)))
                 (setq type first-arg)
                 (ferror "Don't recognize :TYPE ~S." first-arg)))

            (:named
             (setq namedp T))

            (:initial-offset
             (unless (and (numberp first-arg) (>= first-arg 0))
               (ferror "Initial-offset can't be ~S." first-arg))
             (setq initial-offset first-arg)
             (setq initial-offset-p T))
            )))

      ;; Decide, once and for all, if the structure is named or not.
      (when (eq namedp :IMPLIED)
        (if type
            (setq namedp NIL)
            (setq namedp T)))

      ;; Augment the slots with :INCLUDEd slots, :INITIAL-OFFSET nils, and the name, where appropriate.
      (let ((addenda (append (when inclusions
                               (include (car inclusions) (cdr inclusions) type))
                             (make-list initial-offset)
                             (when namedp
                               (list name)))))
        (setq net-offset (length addenda))
        (setq slots (append addenda slots)))

      ;; Make sure all the rules are being followed
      (when (and predicatep type (not namedp))
        (cond
          ((eq predicatep T)
           (ferror "Predicate cannot be defined for unnamed structure type ~S." type))
          ((eq predicatep :IMPLIED)
           (setq predicatep NIL))))
      (when (and initial-offset-p (not type))
        (ferror ":TYPE must be specified for :INITIAL-OFFSET."))

      ;; Make the constructors
      (when (and constructor (not BOA-constructors))
        (push-task (make-defun-constructor constructor NIL type slots)))
      (dolist (BOA-constructor BOA-constructors)
        (let ((constructor-name (first BOA-constructor))
              (lambda-list      (second BOA-constructor)))
          (push-task (make-defun-constructor constructor-name lambda-list type slots))))

      ;; Make the copier
      (when copier
        (push-task (make-defun-copier copier name type)))

      ;; Make the predicate.  Note that if (:PREDICATE NIL) is explicitly requested, but
      ;; there is no type, then a predicate will be generated anyway.  This is because
      ;; the predicate is needed by the following DEFTYPE.
      (when (or predicatep (not type))
        (push-task (make-defun-predicate predicate name type net-offset)))

      ;; Define the type
      (unless type
        (push-task `(DEFTYPE ,name () `(SATISFIES ,',predicate))))

      ;; Define the print function
      (if type
          (when print-function
            (ferror ":PRINT-FUNCTION may not be specified for type ~S." type))
          (when print-function
            (push-task `(SET-STRUCTURE-OBJECT-PRINT-FUNCTION ',name ',print-function))))

      ;; Make the slot accessors and SETF methods
      (let ((index 0))
        (dolist (slot slots)
          (when (actual-slot-record-p slot)
            (push-task (make-defun-accessor conc-name slot name type index))
            (unless (cadr (member :READ-ONLY (slot-record-plist slot)))
              (push-task (make-defsetf-accessor conc-name slot name type index))))
          (incf index)))

      ;; Remember for posterity's sake
      (push-task `(RECORD-NEW-DEFSTRUCT ',name
                                        :TYPE ',type
                                        :SLOT-RECORDS ',slots
                                        :CONSTRUCTOR ',constructor)))))


;;; Task Generators
;;;
;;; These functions generate the myriad DEFUN, DEFSUBST, and DEFSETF tasks that appear
;;; in the expansion of DEFSTRUCT.

(defun make-defun-constructor (constructor-name lambda-list type slots)
  (flet (
         (value-for-slot (slot)
           (cond ((null slot) NIL)
                 ((symbolp slot) (list 'QUOTE slot))
                 ((listp slot) (slot-record-name slot))))
         )
    (let (
          (lambda-list (make-constructor-lambda-list (extract-actual-slot-records slots) lambda-list))
          (slot-names  (mapcar #'value-for-slot slots))
          )
      (let* ((rest-arg (cadr (member '&REST lambda-list)))
             (make-it-safe (if rest-arg
                               `((SETQ ,rest-arg (COPY-LIST ,rest-arg)))
                               NIL)))
        (case (internal-type-representation type)
          (:DEFAULT `(DEFUN ,constructor-name ,lambda-list
                       ,@make-it-safe
                       (MAKE-STRUCTURE-OBJECT ,@slot-names)))
          (:VECTOR  `(DEFUN ,constructor-name ,lambda-list
                       ,@make-it-safe
                       (VECTOR ,@slot-names)))
          (:LIST    `(DEFUN ,constructor-name ,lambda-list
                       ,@make-it-safe
                       (LIST ,@slot-names))))))))

(defun make-defun-copier (copier-name defstruct-name type)
  (case (internal-type-representation type)
    (:DEFAULT `(DEFUN ,copier-name (,defstruct-name)
                 (COPY-STRUCTURE-OBJECT ,defstruct-name)))
    (:VECTOR  `(DEFUN ,copier-name (,defstruct-name)
                 (COPY-SEQ ,defstruct-name)))
    (:LIST    `(DEFUN ,copier-name (,defstruct-name)
                 (COPY-LIST ,defstruct-name)))))

(defun make-defun-predicate (predicate-name defstruct-name type net-offset)
  (case (internal-type-representation type)
    (:DEFAULT `(DEFUN ,predicate-name (THING)
                 (AND (STRUCTURE-OBJECT-P THING)
                      (>= (STRUCTURE-OBJECT-LENGTH THING) ,net-offset)
                      (EQ (STRUCTURE-OBJECT-REF THING ,(1- net-offset)) ',defstruct-name))))
    (:VECTOR  `(DEFUN ,predicate-name (THING)
                 (AND (VECTORP THING)
                      (>= (LENGTH THING) ,net-offset)
                      (EQ (AREF THING ,(1- net-offset)) ',defstruct-name))))
    (:LIST    `(DEFUN ,predicate-name (THING)
                 (AND (LISTP THING)
                      (>= (LENGTH THING) ,net-offset)
                      (EQ (NTH ,(1- net-offset) THING) ',defstruct-name))))))
; we can't require length because of subtypes.

(defun make-defun-accessor (conc-name slot defstruct-name type index)
  (let ((accessor-name (concatenate-and-intern conc-name (symbol-name (slot-record-name slot)))))
    (case (internal-type-representation type)
      (:DEFAULT `(DEFUN ,accessor-name (,defstruct-name)
                   (STRUCTURE-OBJECT-REF ,defstruct-name ,index)))
      (:VECTOR  `(DEFUN ,accessor-name (,defstruct-name)
                   (AREF ,defstruct-name ,index)))
      (:LIST    `(DEFUN ,accessor-name (,defstruct-name)
                   (NTH ,index ,defstruct-name))))))

(defun make-defsetf-accessor (conc-name slot defstruct-name type index)
  (let ((accessor-name (concatenate-and-intern conc-name (symbol-name (slot-record-name slot))))
        (value-temp    (gensym)))
    (case (internal-type-representation type)
      (:DEFAULT `(DEFSETF ,accessor-name (,defstruct-name) (,value-temp)
                   `(SETF (STRUCTURE-OBJECT-REF ,,defstruct-name ,,index) ,,value-temp)))
      (:VECTOR  `(DEFSETF ,accessor-name (,defstruct-name) (,value-temp)
                   `(SETF (AREF ,,defstruct-name ,,index) ,,value-temp)))
      (:LIST    `(DEFSETF ,accessor-name (,defstruct-name) (,value-temp)
                   `(SETF (NTH ,,index ,,defstruct-name) ,,value-temp))))))

(defun concatenate-and-intern (string1 string2)
  "Concatenate STRING1 and STRING2, and intern the resulting string in the
current package."
  (intern (concatenate 'string string1 string2) *package*))

(defun internal-type-representation (type-spec)
  (cond ((eq type-spec NIL)
         :DEFAULT)
        ((or (eq type-spec 'VECTOR)
             (and (listp type-spec) (eq (car type-spec) 'VECTOR)))
         :VECTOR)
        ((eq type-spec 'LIST)
         :LIST)
        (t
         (ferror "Internal: unrecognized type ~S" type-spec))))


;;; Incredible Hair ("Cheveux Incroyable")
;;;
;;; The following atrocity takes care of lambda lists for :CONSTRUCTOR functions.
;;; SLOTS is a list only of actual-slot-records.

(defun make-constructor-lambda-list (slots supplied-lambda-list)
  (if supplied-lambda-list

      ;; Parse the lambda list into its parameters
      (multiple-value-bind (required optional rest aux)
          (USER::PARSE-LAMBDA-LIST supplied-lambda-list '(:REQUIRED :OPTIONAL :REST :AUX))

        (let ((lambda-list-variables
                (append required
                        (mapcar #'USER::PARSE-OPTIONAL-PARAMETER optional)
                        (if rest (list rest))
                        (mapcar #'USER::PARSE-AUX-PARAMETER aux)))
              (redefaulted-optionals NIL)
              (redefaulted-auxs      NIL)
              (unmentioned-slots     NIL))

          ;; Add &OPTIONAL supplied-p variables to the list of things we know are bound.  (Ugh!)
          (dolist (opt-parameter optional)
            (multiple-value-bind (ignore ignore svar)
                (USER::PARSE-OPTIONAL-PARAMETER opt-parameter)
              (if svar (push svar lambda-list-variables))))

          ;; Require each parameter to name a slot.
          (mapcar #'(lambda (llvar)
                      (unless (find-slot llvar slots)
                        (ferror "~S does not name a slot." llvar)))
                  lambda-list-variables)

          ;; Use the defstruct's defaults (rather than NIL) for the &OPTIONAL and &AUX parameters.
          ;; Arbitrarily forbid supplied-p variables.
          (dolist (parameter optional)
            (multiple-value-bind (variable initform svar)
                (USER::PARSE-OPTIONAL-PARAMETER parameter)
              (if (and (listp parameter)
                       (> (length parameter) 1))
                  (push (list* variable initform (when svar (list svar)))
                        redefaulted-optionals)
                  (push (list* variable
                               (slot-record-default (find-slot variable slots))
                               (when svar (list svar)))
                        redefaulted-optionals))))
          (setq redefaulted-optionals (reverse redefaulted-optionals))
          (dolist (parameter aux)
            (multiple-value-bind (variable value)
                (USER::PARSE-AUX-PARAMETER parameter)
              (if (and (listp parameter)
                       (> (length parameter) 1))
                  (push (list variable value)
                        redefaulted-auxs)
                  (push (list variable (slot-record-default (find-slot variable slots)))
                        redefaulted-auxs))))
          (setq redefaulted-auxs (reverse redefaulted-auxs))

          ;; Add an &AUX parameter for each unmentioned slot.
          (mapcar #'(lambda (slot)
                      (unless (member (slot-record-name slot) lambda-list-variables)
                        (push `(,(slot-record-name slot) ,(slot-record-default slot))
                              unmentioned-slots)))
                  slots)
          (setq unmentioned-slots (reverse unmentioned-slots))

          ;; Reassemble the lambda list.
          `(,@required
            ,@(when redefaulted-optionals (cons '&OPTIONAL redefaulted-optionals))
            ,@(when rest                  (list '&REST rest))
            &AUX
            ,@redefaulted-auxs
            ,@unmentioned-slots)))

      ;; If no lambda list was supplied, construct one with &KEY parameters.
      `(&KEY ,@(mapcar #'(lambda (slot)
                           `(,(slot-record-name slot) ,(slot-record-default slot)))
                       slots))))


;;; Inheritance
;;;
;;; Returns a list of slot-records to include

(defun include (parent new-slot-descriptions new-type)
  (let ((parent-plist (lookup-defstruct parent)))
    (when (eq parent-plist :UNDEFINED)
      (ferror "Attempt to include structure ~S, which hasn't been defined." parent))
    (let ((parent-type    (getf parent-plist :TYPE))
          (included-slots (getf parent-plist :SLOTS)))
      (unless (equal parent-type new-type)
        (ferror "Included structure ~S has :TYPE ~S, which does not match type ~S."
                parent parent-type new-type))
      (dolist (foo new-slot-descriptions)
        (let* ((new-slot  (parse-slot foo))
               (slot-name (slot-record-name new-slot))
               (new-plist (slot-record-plist new-slot))
               (old-slot  (find-slot slot-name included-slots))
               (old-plist (slot-record-plist old-slot)))
          (unless old-slot
            (ferror "Slot ~S is not defined in the included structure ~S" slot-name parent))
          (when (and (getf old-plist :read-only)
                     (not (getf new-plist :read-only)))
            (ferror "Slot ~S is :READ-ONLY in ~S; it must also be :READ-ONLY when included."
                    slot-name parent))
          (let ((old-type (getf old-plist :type))
                (new-type (getf new-plist :type)))
            (when (and new-type old-type (not (subtypep new-type old-type)))
              (ferror ":TYPE of slot ~S must be a subtype of included type ~S."
                      slot-name old-type)))
          ;; maybe an error if old-type & ~new-type
          (setq included-slots
                (substitute-if new-slot
                               #'(lambda (u)
                                   (and (actual-slot-record-p u) (eq (slot-record-name u) slot-name)))
                               included-slots))))
      included-slots)))


;;; Slots, Slot-Descriptions, and Slot-Records
;;;
;;; SLOT-DESCRIPTIONS make up the body of DEFSTRUCT forms.  In Common Lisp,
;;; the syntax of a SLOT-DESCRIPTION is
;;;    slot-name | (slot-name [default-init . slot-plist])
;;; If no default-init is provided, then :UNDEFINED is used.
;;;
;;; SLOT-RECORDS are the elements used to represent a structure.  If a SLOT-RECORD
;;; represents an actual slot, its syntax is
;;;    (slot-name default-init . slot-plist),
;;; which is also valid syntax for an equivalent SLOT-DESCRIPTION.  If a SLOT-RECORD
;;; represents a place to contain the name of a structure, its syntax is
;;;    structure-name,
;;; a symbol.  If a SLOT-RECORD represents an empty slot being reserved by a
;;; :INITIAL-OFFSET, its syntax is
;;;    NIL.
;;;
;;; The function PARSE-SLOT-DESCRIPTION translates a SLOT-DESCRIPTION into the
;;; first kind of SLOT-RECORD.  If a default-init is not given, :UNDEFINED is used.
;;; If a slot-plist is not given, NIL is used.
;;;
;;; SLOT-RECORDS of the first kind may be neatly constructed and dissected with
;;; the MAKE-SLOT-RECORD constructor and the SLOT-RECORD-NAME, SLOT-RECORD-DEFAULT,
;;; and SLOT-RECORD-PLIST selectors.
;;;
;;; The predicate ACTUAL-SLOT-RECORD-P returns T if a slot-record is of the first kind,
;;; and NIL otherwise.  The function EXTRACT-ACTUAL-SLOT-RECORDS takes a list of
;;; slot-records and returns a list of all the actual slot-records in that list.
;;; The function FIND-SLOT finds an actual slot-record in a list of slot-records with
;;; a given name.


(defun make-slot-record (slot-name default-value plist)
  (list* slot-name default-value plist))

(defun slot-record-name (slot-record)
  (car slot-record))

(defun slot-record-default (slot-record)
  (cadr slot-record))

(defun slot-record-plist (slot-record)
  (cddr slot-record))

(defun actual-slot-record-p (slot-record)
  (consp slot-record))

(defun parse-slot (slot-description)
  (etypecase slot-description
    (symbol (make-slot-record slot-description :unbound NIL))
    (list   (let ((slot-name (car slot-description))
                  (default   (if (< (length slot-description) 2)
                                 :UNBOUND
                                 (second slot-description)))
                  (plist     (cddr slot-description)))
              (if (symbolp slot-name)
                  (make-slot-record slot-name default plist)
                  (ferror "Slot name ~S must be a symbol." slot-name))))))

(defun find-slot (name slot-records)
  "Return the first actual slot-record in the list SLOT-RECORDS with name NAME,
or NIL if there is none."
  (cond ((null slot-records)
         NIL)
        ((and (actual-slot-record-p (first slot-records))
              (eq name (slot-record-name (first slot-records))))
         (first slot-records))
        (t
         (find-slot name (rest slot-records)))))

(defun extract-actual-slot-records (slot-records)
  (cond ((null slot-records)
         NIL)
        ((actual-slot-record-p (car slot-records))
         (cons (car slot-records) (extract-actual-slot-records (cdr slot-records))))
        (t
         (extract-actual-slot-records (cdr slot-records)))))


;;; Bookkeeping
;;;
;;; The global variable *defined-structures* keeps a record of each defstruct
;;; form.  This information is needed for :INCLUDE options in subsequent defstructs.
;;; The procedures RECORD-NEW-DEFSTRUCT and LOOKUP-DEFSTRUCT make up the interface
;;; to this variable.
;;;
;;; *defined-structures* is a table that associates a symbol, the name of a
;;; defstruct, with a plist.  Items on this plist include:
;;;    :TYPE  - The defstruct's type, which may be LIST, VECTOR, (VECTOR foo), or NIL.
;;;    :SLOTS - A list of slot-records.
;;;    :CONSTRUCTOR - The name of the defstruct's constructor.  Needed by the reader
;;;                   and printer for #S expressions.
;;;
;;; *defined-structures* is currently being maintained as an a-list; sooner or later
;;; it should become a hash table.

(defun record-new-defstruct (name &rest properties)
  (setq *defined-structures*
        (cons (cons name (copy-list properties))
              *defined-structures*)))

(defun lookup-defstruct (name)
  "Return the plist associated with structure NAME, or :UNDEFINED."
  (let ((lookup (assoc name *defined-structures*)))
    (if lookup
        (cdr lookup)
        :UNDEFINED)))

(defun constructor-function-name (name)
  "Return the constructor function for structures with type NAME.  If no such
structure type exists, return :NO-SUCH-TYPE.  If no constructor function exists for
that structure type, return :NO-SUCH-CONSTRUCTOR."
  (let ((plist (lookup-defstruct name)))
    (if (eq plist :UNDEFINED)
        :NO-SUCH-TYPE
        (getf plist :CONSTRUCTOR :NO-SUCH-CONSTRUCTOR))))

;;; Structure objects (the default type for representing defstructs)
;;;

(defun make-structure-object (name &rest elements)
  (SI::MAKE-ARRAY-INTO-NAMED-STRUCTURE (apply #'vector name elements)))

(defun structure-object-ref (structure-object slot-number)
  (aref structure-object slot-number))

(defsetf structure-object-ref (structure-object slot-number) (x)
  `(SETF (AREF ,structure-object ,slot-number) ,x))

(defun structure-object-length (structure-object)
  (length structure-object))

(defun structure-object-p (thing)
  (named-structure-p thing))

(defun copy-structure-object (structure-object)
  (SI::MAKE-ARRAY-INTO-NAMED-STRUCTURE (copy-seq structure-object)))

(defun set-structure-object-print-function (name print-function)
  (declare (ignore name print-function))
  NIL)
