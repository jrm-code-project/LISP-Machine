;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-
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


(defmacro defstruct (name-and-options &body documentation-and-slot-descriptions)
  (multiple-value-bind (name options doc-string slot-records)
      (parse-defstruct name-and-options documentation-and-slot-descriptions)
    `(PROGN
;       (SETF (DOCUMENTATION ',name :STRUCTURE) ,doc-string)
       ,@(make-defstruct-tasks name options slot-records)
       ',name)))



;;; DEFSTRUCT-DESCRIPTOR
;;;
;;; A defstruct-descriptor holds information about a defstruct.
;;; This information is needed for :INCLUDE options in subsequent defstructs and
;;; other things.

(defmacro defstruct-descriptor-name (descriptor)
  `(svref ,descriptor 0))

(defmacro defstruct-descriptor-constructor (descriptor)
  `(svref ,descriptor 1))

(defmacro defstruct-descriptor-includes (descriptor)
  `(svref ,descriptor 2))

(defmacro defstruct-descriptor-type (descriptor)
  "The defstruct's type, which may be LIST, VECTOR, (VECTOR foo), or NIL."
  `(svref ,descriptor 3))

(defmacro defstruct-descriptor-slots (descriptor)
  `(svref ,descriptor 4))

(defun make-defstruct-descriptor (name constructor includes type slots)
  (let ((desc (array:make-vector 5)))
    (setf (defstruct-descriptor-name desc) name)
    (setf (defstruct-descriptor-constructor desc) constructor)
    (setf (defstruct-descriptor-includes desc) includes)
    (setf (defstruct-descriptor-type desc) type)
    (setf (defstruct-descriptor-slots desc) slots)
    desc))

(defmacro get-defstruct-descriptor (defstruct-name)
  `(nc:get-declaration ,defstruct-name 'defstruct-descriptor))

;(defun constructor-function-name (name)
;  "Return the constructor function for structures with type NAME.  If no such
;structure type exists, return :NO-SUCH-TYPE.  If no constructor function exists for
;that structure type, return :NO-SUCH-CONSTRUCTOR."
;  (let ((desc (get-defstruct-descriptor name)))
;    (defstruct-descriptor-constructor name)))


;;; Slots, Slot-Descriptions, and Slot-Records
;;;
;;; SLOT-DESCRIPTIONS make up the body of DEFSTRUCT forms.  In Common Lisp,
;;; the syntax of a SLOT-DESCRIPTION is
;;;    slot-name | (slot-name [default-init . slot-plist])
;;; If no default-init is provided, then the contents of the slot are undefined.
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
;;; first kind of SLOT-RECORD.  If a default-init is not given, UNDEFINED is used.
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
  (and (consp slot-record) (not (eq 'structure-included-name-list (car slot-record)))))

(defun parse-slot (slot-description)
  (etypecase slot-description
    (symbol (make-slot-record slot-description ''UNDEFINED NIL))
    (cons   (let ((slot-name (car slot-description))
                  (default   (if (< (length slot-description) 2)
                                 ''UNDEFINED
                                 (second slot-description)))
                  (plist     (cddr slot-description)))
              (if (symbolp slot-name)
                  (make-slot-record slot-name default plist)
                  (error "Slot name ~S must be a symbol." slot-name))))))

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


;;; Structure objects (the default type for representing defstructs)
;;;

;;; Structure objects are defined in the cold load

(defun copy-structure-object (structure-object)
  (SI::MAKE-ARRAY-INTO-NAMED-STRUCTURE (copy-seq structure-object)))

(defun set-structure-object-print-function (name print-function)
  (declare (ignore name print-function))
  NIL)


;;; Parse Defstruct

(defun parse-defstruct (name-and-options documentation-and-slot-descriptions)
  (let (name options doc-string slots)
    (etypecase name-and-options
      (symbol (setq name name-and-options)
              (setq options NIL))
      (cons   (setq name (first name-and-options))
              (setq options (rest name-and-options))))
    (if (stringp (car documentation-and-slot-descriptions))
        (progn (setq doc-string (car documentation-and-slot-descriptions))
               (setq slots      (cdr documentation-and-slot-descriptions)))
        (progn (setq doc-string NIL)
               (setq slots      documentation-and-slot-descriptions)))
    (values name options doc-string (mapcar #'parse-slot slots))))


;;;; Task Generation


(defun make-defstruct-tasks (name options slots)
 (let ((tasks '()))
  (flet ((push-task (task)
           (setq tasks (cons task tasks))))
    (let (
          (conc-name        (concatenate 'lisp:string (symbol-name name) "-"))
          (constructor      (intern (concatenate 'lisp:string "MAKE-" (symbol-name name)) lisp:*package*))
          (copier           (intern (concatenate 'lisp:string "COPY-" (symbol-name name)) lisp:*package*))
          (predicate        (intern (concatenate 'lisp:string (symbol-name name) "-P")    lisp:*package*))
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
            (cons   (setq option-type (first option))
                    (setq option-args (rest option))
                    (setq first-arg   (car option-args))))
          (case option-type

            (:conc-name
             (if first-arg
                 (setq conc-name (string first-arg))
                 (setq conc-name "")))

            (:constructor
             (case (length option-args)
               (1         (if (symbolp first-arg)
                              (setq constructor first-arg)
                              (error "~S is not a valid name for a constructor function." first-arg)))
               (2         (push option-args BOA-constructors))
               (otherwise (error ":CONSTRUCTOR must have 1 or 2 arguments."))))

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
             (if (or inclusions (> (length option-args) 1))
                 (error "Broken :INCLUDE argument in DEFSTRUCT.")
                 (setq inclusions option-args)))

            (:print-function
             (setq print-function first-arg))

            (:type
             (if (or (eq first-arg 'VECTOR)
                     (eq first-arg 'LIST)
                     (and (listp first-arg) (= (length first-arg) 2) (eq (car first-arg) 'VECTOR)))
                 (setq type first-arg)
                 (error "Don't recognize :TYPE ~S." first-arg)))

            (:named
             (setq namedp T))

            (:initial-offset
             (unless (and (numberp first-arg) (>= first-arg 0))
               (error "Initial-offset can't be ~S." first-arg))
             (setq initial-offset first-arg)
             (setq initial-offset-p T))
            )))

      ;; Decide, once and for all, if the structure is named or not.
      (when (and (eq namedp :IMPLIED)
                 type)
        (setq namedp NIL))

      ;; Augment the slots with :INCLUDEd slots, :INITIAL-OFFSET nils, and the name, where appropriate.
      (let ((addenda (append (when (eq namedp :implied)
                               (list
                                 (do ((name-list name
                                                 (if (consp name-list)
                                                     (lisp:nconc name-list include)
                                                   (list name-list include)))
                                      (include (car inclusions)
                                               (defstruct-descriptor-includes (get-defstruct-descriptor include))))
                                     ((null include)
                                      (if (consp name-list)
                                          (list 'structure-included-name-list name-list)
                                        name-list)))))
                             (when inclusions
                               (include (car inclusions) (cdr inclusions) type))
                             (make-list initial-offset)
                             (when (eq namedp t)
                               (list name))
                             )))
        (setq net-offset (length addenda))
        (setq slots (append addenda slots)))

      ;; Make sure all the rules are being followed
      (when (and predicatep type (not namedp))
        (cond
          ((eq predicatep T)
           (error "Predicate cannot be defined for unnamed structure type ~S." type))
          ((eq predicatep :IMPLIED)
           (setq predicatep NIL))))
      (when (and initial-offset-p (not type))
        (error ":TYPE must be specified for :INITIAL-OFFSET."))

      ;; Make the constructors
      (when (and constructor (not BOA-constructors))
        (push-task (make-defun-constructor constructor NIL nil type slots)))
      (dolist (BOA-constructor BOA-constructors)
        (let ((constructor-name (first BOA-constructor))
              (lambda-list      (second BOA-constructor)))
          (push-task (make-defun-constructor constructor-name lambda-list t type slots))))

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
            (error ":PRINT-FUNCTION may not be specified for type ~S." type))
          (when print-function
            (push-task `(SET-STRUCTURE-OBJECT-PRINT-FUNCTION ',name ',print-function))))

      ;; Make the slot accessors and SETF methods
      (let ((index 0))
        (dolist (slot slots)
          (when (actual-slot-record-p slot)
            (push-task (make-define-accessor conc-name slot name type index))
            (unless (cadr (member :READ-ONLY (slot-record-plist slot)))
              (push-task (make-defsetf-accessor conc-name slot name type index))))
          (incf index)))

      ;; Remember for posterity's sake
      (push-task `(NC:DEF-DECLARATION ,name DEFSTRUCT-DESCRIPTOR
                    ',(make-defstruct-descriptor name constructor (car inclusions) type slots)))))
  tasks))


;;; Task Generators
;;;
;;; These functions generate the myriad DEFUN, DEFSUBST, and DEFSETF tasks that appear
;;; in the expansion of DEFSTRUCT.

(defun make-defun-constructor (constructor-name lambda-list BOA-p type slots)
  (flet ((value-for-slot (slot)
           (cond ((null slot) NIL)
                 ((symbolp slot) (list 'QUOTE slot))
                 ((and (consp slot) (eq (first slot) 'structure-included-name-list)) (list 'quote (second slot)))
                 ((consp slot) (slot-record-name slot)))))
    (let ((lambda-list (make-constructor-lambda-list (extract-actual-slot-records slots)
                                                     lambda-list BOA-p))
          (slot-names  (mapcar #'value-for-slot slots)))
      (let* ((rest-arg (cadr (member '&REST lambda-list)))
;            (make-it-safe (if rest-arg
;                              `((SETQ ,rest-arg (COPY-LIST ,rest-arg)))
;                              NIL))
             )
        (case (internal-type-representation type)
          (:DEFAULT `(DEFUN ,constructor-name ,lambda-list
;                      ,@make-it-safe
                       (MAKE-STRUCTURE-OBJECT ,@slot-names)))
          (:VECTOR  `(DEFUN ,constructor-name ,lambda-list
;                      ,@make-it-safe
                       (VECTOR ,@slot-names)))
          (:LIST    `(DEFUN ,constructor-name ,lambda-list
;                      ,@make-it-safe
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
    (:DEFAULT `(DEFSUBST ,predicate-name (THING)
                 (TYPEP-STRUCTURE THING ',defstruct-name)))
    (:VECTOR  `(DEFUN ,predicate-name (THING)
                 (AND (VECTORP THING)
                      (>= (LENGTH THING) ,net-offset)
                      (EQ (AREF THING ,(1- net-offset)) ',defstruct-name))))
    (:LIST    `(DEFUN ,predicate-name (THING)
                 (AND (LISTP THING)
                      (>= (LENGTH THING) ,net-offset)
                      (EQ (NTH ,(1- net-offset) THING) ',defstruct-name))))))
; we can't require length because of subtypes.

(defun make-define-accessor (conc-name slot defstruct-name type index)
  (let ((accessor-name (concatenate-and-intern conc-name (symbol-name (slot-record-name slot)))))
    (case (internal-type-representation type)
      (:DEFAULT `(DEFSUBST ,accessor-name (,defstruct-name)
                   (STRUCTURE-REF ',defstruct-name ,index ,defstruct-name)))
      (:VECTOR  `(DEFSUBST ,accessor-name (,defstruct-name)
                   (AREF ,defstruct-name ,index)))
      (:LIST    `(DEFSUBST ,accessor-name (,defstruct-name)
                   (NTH ,index ,defstruct-name))))))

(defun make-defsetf-accessor (conc-name slot defstruct-name type index)
  (let ((accessor-name (concatenate-and-intern conc-name (symbol-name (slot-record-name slot))))
        (value-temp    (gensym)))
    (case (internal-type-representation type)
      (:DEFAULT `(DEFSETF ,accessor-name (,defstruct-name) (,value-temp)
                   `(STRUCTURE-SET ',',defstruct-name ,,index ,,defstruct-name ,,value-temp)))
      (:VECTOR  `(DEFSETF ,accessor-name (,defstruct-name) (,value-temp)
                   `(SETF (AREF ,,defstruct-name ,,index) ,,value-temp)))
      (:LIST    `(DEFSETF ,accessor-name (,defstruct-name) (,value-temp)
                   `(SETF (NTH ,,index ,,defstruct-name) ,,value-temp))))))

(defun concatenate-and-intern (string1 string2)
  "Concatenate STRING1 and STRING2, and intern the resulting string in the
current package."
  (intern (concatenate 'lisp:string string1 string2) lisp:*package*))

(defun internal-type-representation (type-spec)
  (cond ((eq type-spec NIL)
         :DEFAULT)
        ((or (eq type-spec 'VECTOR)
             (and (listp type-spec) (eq (car type-spec) 'VECTOR)))
         :VECTOR)
        ((eq type-spec 'LIST)
         :LIST)
        (t
         (error "Internal: unrecognized type ~S" type-spec))))


;;; Incredible Hair ("Cheveux Incroyable")
;;;
;;; The following atrocity takes care of lambda lists for :CONSTRUCTOR functions.
;;; SLOTS is a list only of actual-slot-records.

(defun make-constructor-lambda-list (slots supplied-lambda-list BOA-p)
  (if BOA-p

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
                        (error "~S does not name a slot." llvar)))
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
  (let ((parent-desc (get-defstruct-descriptor parent)))
    (unless parent-desc
      (error "Attempt to include structure ~S, which hasn't been defined." parent))
    (let ((parent-type    (defstruct-descriptor-type parent-desc))
          (included-slots (defstruct-descriptor-slots parent-desc)))
      (unless (equal parent-type new-type)
        (error "Included structure ~S has :TYPE ~S, which does not match type ~S."
                parent parent-type new-type))
      (dolist (foo new-slot-descriptions)
        (let* ((new-slot  (parse-slot foo))
               (slot-name (slot-record-name new-slot))
               (new-plist (slot-record-plist new-slot))
               (old-slot  (find-slot slot-name included-slots))
               (old-plist (slot-record-plist old-slot)))
          (unless old-slot
            (error "Slot ~S is not defined in the included structure ~S" slot-name parent))
          (when (and (getf old-plist :read-only)
                     (not (getf new-plist :read-only)))
            (error "Slot ~S is :READ-ONLY in ~S; it must also be :READ-ONLY when included."
                    slot-name parent))
          (let ((old-type (getf old-plist :type))
                (new-type (getf new-plist :type)))
            (when (and new-type old-type (not (subtypep new-type old-type)))
              (error ":TYPE of slot ~S must be a subtype of included type ~S."
                      slot-name old-type)))
          ;; maybe an error if old-type & ~new-type
          (setq included-slots
                (substitute-if new-slot
                               #'(lambda (u)
                                   (and (actual-slot-record-p u) (eq (slot-record-name u) slot-name)))
                               included-slots))))
      (if (and (symbolp (car included-slots))
               (not (null (car included-slots))))
          (cdr included-slots)
        included-slots))))



