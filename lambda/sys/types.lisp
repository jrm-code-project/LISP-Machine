;-*- Mode:LISP; Package:SI; Lowercase:T; Cold-Load:T; Base:10; Readtable:CL -*-

;; in the cold-load because is defines a bunch of useful predicates; also since
;;  calls to typep are not guaranteed to be open-coded

;;; Each defined type keyword can have any of these three properties:
;;; TYPE-PREDICATE - value is a function to test an object for membership in the type.
;;;   It gets the object as first arg, and any elements of the type specifier
;;;   except for the keyword itself as additional args.
;;; TYPE-OPTIMIZER - value is an optimizer function for compiling calls to TYPEP.
;;;   Its first argument is the expression which calls TYPEP.
;;;   Its remaining args are the elements of the type specifier, except the first.
;;;   It can return the original call to TYPEP if it has nothing better to optimize to.
;;; TYPE-EXPANDER - value is an expander function to compute a new type specifier.
;;;   It gets one argument, the type specifier, and returns a new type specifier.
;;; TYPE-ALIAS-FOR - what it sounds like.

;;; Interpreted calls to TYPEP use TYPE-PREDICATE and TYPE-EXPANDER props.
;;; Compilation uses TYPE-OPTIMIZER and TYPE-EXPANDER props.
;;; Compilation can also use the TYPE-PREDICATE prop--
;;;  compiling a call to that function rather than to TYPEP,
;;;  but only if the property is a symbol.

;;; * CAUTION: you cannot simply define any new type with a TYPE-PREDICATE
;;; * because it needs to be wired into the SUBTYPEP data structures.
;;; * Defining types with TYPE-EXPANDERs (ie, use of DEFTYPE) is ok
;;; * because they will get expanded by SUBTYPEP, so they don't really
;;; * pose a new problem.

;;; These properties are also used:
;;; TYPE-NAME - value is a string, including a definite article, which
;;;  is used as the name of this type when it appears as an atom.
;;; TYPE-NAME-FUNCTION - value is a function to compute the name
;;;  of types which are lists starting with this symbol.

(defvar *standard-system-type-specifiers* nil)
;; initialized by define-system-type below
;; contents in 99:
;;  and or not cl:member member satisfies
;;  array atom bignum bit bit-vector cl:character zl:character common compiled-function
;;  complex cons double-float entity si::fat-char fix fixnum float flonum function
;;  hash-table instance integer keyword list locative long-float microcode-function
;;  mod named-structure nil non-complex-number null number package pathname
;;  random-state ratio rational readtable real select si::select-method sequence
;;  short-float signed-byte simple-array simple-bit-vector simple-string simple-vector
;;  single-float small-float small-flonum standard-char stream string string-char
;;  structure symbol t unsigned-byte values vector
;;  :array :atom :bignum :character :closure :compiled-function :complex :cons
;;  :entity :fat-char :fix :fixnum :float :flonum :integer :list :locative
;;  :microcode-function :named-structure :null :number :ratio :rational :select
;;  :select-method :small-flonum :string :structure :symbol

(defmacro deftype (name arglist &body body)
  "Defines NAME as a data type name for use in TYPEP, etc.
A list starting with NAME, used as a type specifier,
expands by binding the args in ARGLIST and then evaluating the BODY.
The value of BODY should be another type specifier.
Any optional arguments in ARGLIST which do not have default values specified
will be bound to * by default, rather than NIL."
  (check-type name symbol)
  (cond ((memq name *standard-system-type-specifiers*)
         (ferror "~~S is the name of a standard type specifier used by the system.
Redefining it would probably break the world.~" name))
        ((or (getdecl name 'defstruct-description)
             (getdecl name 'si::flavors)
             #+never
             (let ((tem (assq 'si::flavors file-local-declarations)))
               (and tem (get tem name)))
             (get name 'si::flavor))
         (cerror "Yes, please. I want to lose. ~S ~S anyway"
                 "~*~S is already the name of a ~:[flavor~;structure~]
  ~\(~S ~S ...) will cause (~S foo '~S) not to recognize existing
~:[instances of that flavor~;structures of that type~] in new code, ~
but not affect (~S foo '~S)~%in existing compiled code. You may lose!~"
                 'deftype name (getdecl name 'defstruct-description) 'deftype name 'typep name
                 (getdecl name 'defstruct-description) 'typep name)))
  (let ((argcopy (copy-list arglist))
        optionalf doc)
    (if (stringp (car body)) (setq doc (car body)))
    (do ((tail argcopy (cdr tail))) ((null tail))
      (cond ((eq (car tail) '&optional)
             (setq optionalf t))
            ((memq (car tail) '(&key &rest &aux))
             (return))
            ((and optionalf
                  (atom (car tail))
                  (not (memq (car tail) lambda-list-keywords)))
             (setf (car tail)
                   `(,(car tail) '*)))))
    `(progn
       (eval-when (load eval)
         (si:record-source-file-name ',name 'deftype)
         (clear-cached-subtype-info ',name)
         (defun (:property ,name type-expander) ,argcopy
           . ,body)
         (remprop ',name 'type-alias-for)
         (setf (documentation ',name 'type) ',doc))
       (eval-when (compile)
         (putdecl ',name (function (lambda ,argcopy . ,body)) 'type-expander))
       ',name)))

(defsignal-explicit invalid-type-specifier (ferror) (string typespec)
  :property-list (list :type-specifier typespec)
  :format-string string
  :format-args (list typespec))

(defun commonp (object)
  "T if OBJECT is a kind of object which Common Lisp defines.
This is everything except locatives, stack groups, selects,
closures, entities, compiled and microcode functions,
and flavor instances (except for a few flavors which implement Common Lisp types)."
  (typecase object
    (instance
     (or (pathnamep object) (streamp object) (hash-table-p object)))
    (compiled-function
     (streamp object))
    (t
     (not (memq (%data-type object)
                '(#.dtp-locative #.dtp-stack-group #.dtp-select-method
                  #.dtp-closure #.dtp-entity #.dtp-u-entry))))))

;;;; TYPE-OF

(defconst type-of-alist
          '((#.dtp-symbol . symbol)
            (#.dtp-character . cl:character)
            (#.dtp-character . zl:character)
            (#.dtp-list . cons)
            (#.dtp-fix . fixnum)
            (#.dtp-locative . locative)
            (#.dtp-fef-pointer . compiled-function)
            (#.dtp-closure . closure)
            (#.dtp-entity . entity)
            (#.dtp-instance . instance)
            (#.dtp-u-entry . microcode-function)
            (#.dtp-select-method . select)
            (#.dtp-small-flonum . short-float)
            (#.dtp-stack-group . stack-group)))

(defun type-of (object &aux (dtp (%data-type object)))
  "Returns a type-specifier describing the type OBJECT belongs to.
For example, (TYPE-OF 5) is FIXNUM"
  (cond ((eq dtp dtp-instance)
         (%p-contents-offset
           (instance-flavor object)
           %instance-descriptor-typename))
        ((eq dtp dtp-array-pointer)
         (cond ((named-structure-p object))
               ((stringp object) 'string)
               (t 'array)))
;       ((eq dtp dtp-entity)
;        (class-symbol object))
        ((eq dtp dtp-extended-number)
         (case (%p-ldb-offset %%header-type-field object 0)
           (#.%header-type-flonum 'single-float)
           (#.%header-type-bignum 'bignum)
           (#.%header-type-rational 'ratio)
           (#.%header-type-complex 'complex)
           (t t)))
        ((cdr (assq dtp type-of-alist)))
        (t t)))

;;; TYPEP top-level

(defconst typep-one-arg-alist
          '((#.dtp-symbol . :symbol)
            (#.dtp-character . :character)
            (#.dtp-list . :cons)
            (#.dtp-fix . :fixnum)
            (#.dtp-locative . :locative)
            (#.dtp-fef-pointer . :compiled-function)
            (#.dtp-closure . :closure)
            (#.dtp-entity . :entity)
            (#.dtp-instance . :instance)
            (#.dtp-u-entry . :microcode-function)
            (#.dtp-select-method . :select)
            (#.dtp-small-flonum . :small-flonum)
            (#.dtp-stack-group . :stack-group)))

(defun typep (object &optional (type nil type-specified-p))
  "T if OBJECT fits the data type specifier TYPE."
;;An obsolete mode of use is with one argument;
;;then the value is a type specifier describing OBJECT.
  (declare (arglist object type))
  (let (tem structure-desc dtp
        (type1 (if (consp type) (car type) type)))
    (cond ((not type-specified-p)
           (setq dtp (%data-type object))
           ;; Cannot use TYPE-OF, since we must
           ;; for back-compatibility return keywords.
           (cond ((eq dtp dtp-instance)
                  (%p-contents-offset
                    (instance-flavor object)
                    %instance-descriptor-typename))
                 ((eq dtp dtp-array-pointer)
                  (cond ((named-structure-p object))
                        ((stringp object) :string)
                        (t :array)))
;                ((eq dtp dtp-entity)
;                 (class-symbol object))
                 ((eq dtp dtp-extended-number)
                  (select (%p-ldb-offset %%header-type-field object 0)
                    (%header-type-flonum :flonum)
                    (%header-type-bignum :bignum)
                    (%header-type-rational :rational)
                    (%header-type-complex :complex)
                    (otherwise :random)))
                 ((cdr (assq dtp typep-one-arg-alist)))
                 (t :random)))
          ((setq dtp (or (rassq type type-of-alist) (rassq type typep-one-arg-alist)))
           (eq (%data-type object) (car dtp)))
          ;;>> Doesn't check wna to predicate function
          ((setq tem (get type1 'type-predicate))
           (if (atom type)
               (funcall tem object)
             (apply tem object (cdr type))))
          ((setq tem (get type1 'type-alias-for))
           (if (atom type)
               (typep object tem)
             (with-list* (tem tem (cdr type))
               (typep object tem))))
          ;;>> Doesn't check wna to expander function
          ((setq tem (get type1 'type-expander))
           (typep object (apply tem (if (atom type) nil (cdr type)))))
          ((get type1 'si:flavor)
           (typep-structure-or-flavor
             object
             (dont-optimize (flavor-name (get-flavor-tracing-aliases type1)))))
          ((or (and (setq structure-desc (get type1 'si::defstruct-description))
                    (defstruct-description-named-p structure-desc))
               (get type1 'defstruct-named-p))
           (typep-structure-or-flavor object type1))
;         ((and (symbolp type1) (fboundp 'class-symbolp) (class-symbolp type1))
;          (and (entityp object)
;               (subclass-of-class-symbol-p (class object) type1)))
          (t (typep object (cerror t nil 'invalid-type-specifier
                                   "~S is not a valid type specifier" type))))))

;;; As of system 98, this is used only by old compiled expansions of TYPEP.
(defun typep-structure (x type &aux xname d)
  (cond ((setq xname (named-structure-p x))
         (do () ((eq xname type) t)
           (or (and (setq d (get xname 'defstruct-description))
                    (defstruct-description-named-p d)
                    (setq xname (car (defstruct-description-include d))))
               (return nil))))
        ((and (setq d (get type 'defstruct-description))
              (defstruct-description-named-p d))
         nil)
        (t (typep x type))))                    ;Optimization turned out to be wrong

;;;; Hairy slege-hammeroid TYPE-CANONICALIZE

(defun type-canonicalize (typespec record-dependencies dependencies &aux string)
  "Returns a typespec equivalent in meaning to TYPESPEC, but possibly simpler."
  (declare (values canonicalized-type dependencies))
  (do-forever
    (setq string (condition-case ()
                     (catch 'invalid-type-specifier
                       (return-from type-canonicalize
                         (type-canonicalize-1 typespec record-dependencies dependencies)))
                   ;;>> this is the wrong thing
                   (sys:too-many-arguments "Type specifier ~S has too many \"arguments\"")
                   (sys:too-few-arguments "Type specifier ~S has too few \"arguments\"")))
    (if compiler::qc-file-in-progress
        (progn (compiler::warn 'compiler::bad-type-specification :implausible string typespec)
               (return (values typespec nil)))
      (setq typespec (cerror t nil 'invalid-type-specifier string typespec)))))

;; this is only separate from the above to make the above easier to read
(defun type-canonicalize-1 (typespec record-dependencies dependencies &aux tem)
  (macrolet ((record-dependency (x)
               `(if record-dependencies (pushnew ,x dependencies :test #'eq)))
             (type-canonicalize-2 (x)
                `(multiple-value-setq (nil dependencies)
                   (type-canonicalize ,x record-dependencies dependencies))))
    (flet ((find-tail-of-same-type (y list &aux x)
             (setq x (if (consp y) (car y) y))
             (unless (memq x '(and or not cl:member zl:member satisfies))
               (do ((z list (cdr z)))
                   ((null z))
                 (when (or (eq (car z) x) (eq (caar-safe z) x))
                   (return z))))))
      (values
        (block canon
          (cond ((symbolp typespec)
                 (cond ((memq typespec *standard-system-type-specifiers*)
                        (cond ((get typespec 'type-alias-for))
                              ((setq tem (get typespec 'type-expander))
                               (funcall tem))
                              (t typespec)))
                       ((setq tem (get typespec 'type-alias-for))
                        ;;(record-dependency typespec)
                        (type-canonicalize-2 tem))
                       ((setq tem (getdecl typespec 'type-expander))
                        (record-dependency typespec)
                        (type-canonicalize-2 (funcall tem)))
                       ;;>> trace aliases?
                       ((getdecl typespec 'si:flavor) typespec)
                       ((getdecl typespec 'defstruct-description) typespec)
                       ((get typespec 'si:flavor) typespec)
                       (t (throw 'invalid-type-specifier
                                 "~S is not a known type specifier"))))
                ((and (consp typespec) (symbolp (car typespec)))
                 (do-forever
                   (unless (setq tem (get (car typespec) 'type-alias-for))
                     (return))
                   ;;(record-dependency tem)
                   (setq typespec `(,tem . ,(cdr typespec))))
                 (case (car typespec)
                   (or (do ((tail (cdr typespec) (cdr tail))
                            elt (frobs nil))
                           ((null tail)
                            (cond ((cdr frobs)
                                   `(or . ,(nreverse frobs)))
                                  ;; (or foo) => foo
                                  (frobs (car frobs))
                                  ;; (or) => t
                                  (t t)))
                         (setq elt (type-canonicalize-2 (car tail)))
                         (case (if (consp elt) (car elt) elt)
                           (or (setq tail (append elt (cdr tail))))
                           ((t) (setq dependencies nil)
                                (return-from canon t))
                           ((nil))              ;splice out NIL's
                           (t
                            (if (setq tem (find-tail-of-same-type elt frobs))
                                (cond ((atom (car tem)))
                                      ;; (or (foo bar baz) foo) => foo
                                      ((atom elt) (setf (car tem) elt))
                                      (t (push elt frobs)))
                              (push elt frobs))))))
                   (and (do ((tail (cdr typespec) (cdr tail))
                             elt (frobs nil))
                            ((null tail)
                             (cond ((cdr frobs)
                                    `(and . ,(nreverse frobs)))
                                   (t (car frobs))))
                          (setq elt (type-canonicalize-2 (car tail)))
                          (case (if (consp elt) (car elt) elt)
                            (and (setq tail (append elt (cdr tail))))
                            ((nil) (setq dependencies nil)
                                   (return-from canon nil))
                            ((t))
                            (t
                             (if (setq tem (find-tail-of-same-type elt frobs))
                                 (cond ((atom (car tem)) (setf (car tem) elt))
                                       ((atom elt))
                                       (t (push elt frobs)))
                               (push elt frobs))))))
                   (not (let ((z (type-canonicalize-2 (cadr typespec))))
                          (if (eq (car-safe z) 'not)
                              (cadr z)
                            `(not ,z))))
                   (t
                    (cond ((dolist (elt (cdr typespec) t)
                             (unless (eq elt '*)
                               (return nil)))
                           ;; (foo * * *) => foo
                           (type-canonicalize-2 (car typespec)))
                          ((setq tem (getdecl (car typespec) 'type-expander))
                           (record-dependency (car typespec))
                           (apply tem (cdr typespec)))
                          ((setq tem (get (car typespec) 'type-canonicalizer))
                           (multiple-value-setq (nil dependencies)
                             (apply tem record-dependencies dependencies
                                    typespec (cdr typespec))))
                          ((memq (car typespec) *standard-system-type-specifiers*)
                           (throw 'invalid-type-specifier
                                    ;; Yow! Can I reimplement lisp primitives using FORMAT?
                                  "The type specifier ~1{~S~:} may not be used with \"arguments\""))
                          (t (throw 'invalid-type-specifier
                                    "~1{~S~:} is not a known type specifier"))))))
                (t (throw 'invalid-type-specifier "~S cannot be a type specifier!"))))
        dependencies))))

;;;Predicate for determining whether a form is a valid type specifier:
(defun type-specifier-p (typespec)
  "Returns a canonicalized form of TYPESPEC, if it is a valid
type specifier.  Otherwise, it returns NIL."
  (condition-case (condition)
      (type-canonicalize typespec nil nil)
    (invalid-type-specifier nil)))

;; used by array simple-array
(defun canonicalize-array-type-specifier (record-dependencies dependencies typespec
                                          &optional (element-type '*) (dimensions '*)
                                          &aux (type element-type))
  (or (memq element-type '(t *))
      (multiple-value-setq (type dependencies)
        (type-canonicalize-1 element-type record-dependencies dependencies)))
  (values (cond ((and (equal type element-type) (null (cddr typespec)))
                 typespec)
                ((eq dimensions '*)
                 `(,(car typespec) ,type))
                (t
                 `(,(car typespec) ,type ,dimensions)))
          dependencies))

;; used by non-complex-number real rational integer float short-float single-float
(defun canonicalize-real-type-specifier (record-dependencies dependencies typespec
                                          &optional (low '*) (high '*))
  (declare (ignore record-dependencies))
  (flet ((check (x)
           (or (non-complex-number-p x)
               (eq x '*)
               (and (consp x)
                    (null (cdr x))
                    (non-complex-number-p (car x))))))
    (or (check low)
        (throw 'invalid-type-specifier "Invalid lower bound in ~S"))
    (or (check high)
        (throw 'invalid-type-specifier "Invalid lower bound in ~S"))
    (values typespec dependencies)))


;;;; SUBTYPEP

(defstruct (subtypep-hash-table-element (:type :list*)
                                        (:alterant nil) (:callable-constructors nil)
                                        (:conc-name subtypep-hash-table-element-))
  (subtypep nil :documentation "First value for (SUBTYPEP (CAR key) (CDR key))")
  (knownp nil :documentation "Second value for (SUBTYPEP (CAR key) (CDR key))")
  (dependencies nil :documentation "What types we expanded in making our decision."))

(defvar *subtypep-hash-table* :unbound
  "An EQUAL hash table containing cached values of (subtypep foo bar), keyed by (cons foo bar)")

(defvar *use-subtypep-cache-p*)
(defun subtypep (x y)
  "T if any object of type X must be of type Y.
The second value is T if the first value is accurate:
if the second value is T and the first is NIL,
then there are objects of type X that are not of type Y.
If the second value is NIL, it is not known whether X is really a subtype of Y."
  (declare (values known-to-be-subtype known-whether-is-subtype))
  (unless (variable-boundp *subtypep-hash-table*)
    (let ((default-cons-area background-cons-area))
      (setq *subtypep-hash-table* (make-hash-table :test #'equal :size 400.))))
  (multiple-value-bind (known-to-be-subtype known-whether-is-subtype)
      (let ((*use-subtypep-cache-p* t))
        (subtypep-1 x y ()))
    (values known-to-be-subtype known-whether-is-subtype)))

(defun compilation-subtypep (x y)
  (declare (values known-to-be-subtype known-whether-is-subtype))
  (multiple-value-bind (known-to-be-subtype known-whether-is-subtype)
      (let ((*use-subtypep-cache-p* nil))
        (subtypep-1 x y ()))
    (values known-to-be-subtype known-whether-is-subtype)))

(defmacro subtypep-2 (x y)
  (declare (values known-to-be-subtype known-whether-is-subtype))
  `(let (a)
     (multiple-value-setq (a nil dependencies)
       (subtypep-1 ,x ,y dependencies))
     a))

(defmacro subtypep-3 ((known-to-be-var known-whether-var) x y &body body)
  `(multiple-value-bind (,known-to-be-var ,known-whether-var .deps.)
       (subtypep-1 ,x ,y dependencies)
     (setq dependencies .deps.)
     . ,body))

(defun subtypep-1 (x y dependencies
                   &aux (known-to-be-subtype nil) (known-whether-is-subtype nil) tem elt
                        (cachep *use-subtypep-cache-p*))
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (cond ((and (symbolp y) (setq tem (get y 'subtypes)))
         (if (memq (if (atom x) x (car x)) tem)
             (return-from subtypep-1 (values t t dependencies))))
        ((memq y '(t nil)) (return-from subtypep-1 (values y t dependencies)))
        ((and cachep
              (setq tem (gethash (cons x y) *subtypep-hash-table*))
              (return-from subtypep-1
                (values (subtypep-hash-table-element-subtypep tem)
                        (subtypep-hash-table-element-knownp tem)
                        (subtypep-hash-table-element-dependencies tem))))))
  (macrolet ((record-dependency (x)
               `(and cachep
                     (not (memq ,x *standard-system-type-specifiers*))
                     (if (cl:listp ,x)
                         (dolist (x ,x) (pushnew x dependencies :test #'eq))
                       (pushnew ,x dependencies :test #'eq)))))
   (labels ((record-atomic-dependency (x)
              (and cachep
                   (if (atom x)
                       (unless (memq x *standard-system-type-specifiers*)
                         (pushnew x dependencies :test #'eq))
                     (case (car x)
                       ((and or not)
                        (dolist (y (cdr x)) (record-atomic-dependency y)))
                       ((satisfies cl:member zl:member))
                       ((not (memq x *standard-system-type-specifiers*))
                        (pushnew (car x) dependencies :test #'eq)))))))
    (let ((x x) (y y))
      (multiple-value-setq (x dependencies) (type-canonicalize x cachep dependencies))
      (multiple-value-setq (y dependencies) (type-canonicalize y cachep dependencies))
      (cond ((or (null x) (eq y t) (equal x y))
             (setq known-to-be-subtype t
                   known-whether-is-subtype t))
            ((eq (car-safe y) 'or)              ;(subtypep foo '(or ...))
             (setq ;; known-to-be-subtype nil
                   known-whether-is-subtype t)
             (dolist (y (cdr y))
               (subtypep-3 (t1 t2) x y
                 (if t1 (return (setq known-to-be-subtype t
                                      known-whether-is-subtype t))
                   (setq known-whether-is-subtype (and known-whether-is-subtype t2))))))
            ((eq (car-safe y) 'and)             ;(subtypep foo '(and ...))
             (setq known-to-be-subtype t
                   known-whether-is-subtype t)
             (dolist (y (cdr y))
               (subtypep-3 (t1 t2) x y
                 (if t2
                     (setq known-to-be-subtype (and known-to-be-subtype t1))
                   (return (setq known-to-be-subtype nil
                                 known-whether-is-subtype nil))))))
            ((eq (car-safe y) 'not)             ;(subtypep foo '(not ...))
             (multiple-value-bind (t1 t2 tem) (disjoint-typep x (cadr y) dependencies)
               (setq dependencies tem)
               (setq known-to-be-subtype t1
                     known-whether-is-subtype (or t2
                                                  (subtypep-2 x (cadr y))
                                                  (subtypep-2 (cadr y) x)))))
            ((eq (car-safe x) 'cl:member)       ;(subtypep '(member ...) bar)
             (setq known-to-be-subtype (loop for z in (cdr x) always (typep z y))
                   known-whether-is-subtype t))
            ((eq (car-safe x) 'and)             ;(subtypep '(and ...) bar)
             (let ((knownp t))
               (dolist (x (cdr x))
                 (subtypep-3 (t1 t2) x y
                   (when t1
                     (setq known-to-be-subtype t
                           known-whether-is-subtype t)
                     (return nil))
                   (setq knownp (and knownp t2))))
               (setq known-whether-is-subtype knownp)))
            ((eq (car-safe x) 'or)              ;(subtypep '(or ...) bar)
             (let ((val t))
               (dolist (x (cdr x))
                 (subtypep-3 (t1 t2) x y
                   (unless t2
                     (return nil))
                   (setq val (and val t1))))
               (setq known-to-be-subtype val
                     known-whether-is-subtype t)))
            ((eq (car-safe x) 'not)             ;(subtypep '(not ...) bar)
             (multiple-value-bind (nil t2 tem) (disjoint-typep (cadr x) y dependencies)
               (setq dependencies tem)
               (setq known-whether-is-subtype (or t2
                                                  (subtypep-2 (cadr x) y)
                                                  (subtypep-2 y (cadr x))))))
            ((eq (car-safe y) 'cl:member))      ;(subtypep foo '(member ...))
            ((eq (car-safe y) 'satisfies))      ;(subtypep foo '(satisfies ...))
            ((eq (car-safe x) 'satisfies))      ;(subtypep '(satisfies ...) bar)
            ((atom y)
             (setq known-to-be-subtype (atom-subtypep (if (atom x) x (car x)) y)
                   known-whether-is-subtype t))
            ((atom x)
             (setq known-whether-is-subtype t))
            (t
              (unless (setq tem (atom-subtypep (car x) (car y)))
                (setq known-whether-is-subtype t))
              (if (and tem (setq tem (get (car y) 'subtypep-predicate)))
                  (multiple-value-setq (known-to-be-subtype known-whether-is-subtype dependencies)
                    (funcall tem x y dependencies)))))
      (setq known-whether-is-subtype (not (not known-whether-is-subtype)))
      (setq known-to-be-subtype (not (not known-to-be-subtype)))
      (when cachep
        (setq elt (let ((default-cons-area background-cons-area))
                    (make-subtypep-hash-table-element :subtypep known-to-be-subtype
                                                      :knownp known-whether-is-subtype
                                                      :dependencies (copylist dependencies))))
        (setf (gethash (cons x y) *subtypep-hash-table*) elt))))
    (when cachep (setf (gethash (cons x y) *subtypep-hash-table*) elt)))
  (values known-to-be-subtype known-whether-is-subtype dependencies))

;;; T if atomic type X is a subtype of atomic type Y.
;;; It is never impossible to tell, so only one value is returned.
(defun atom-subtypep (x y &aux t1 t2
                      (f1 (get-flavor-tracing-aliases x))
                      (f2 (get-flavor-tracing-aliases y)))
  (cond ((eq x y) t)
        (f1
         (or (eq y 'atom)
             (and (eq y 'common)
                  (subtypep-1 x '(or pathname hash-table) nil))
             (and f2
                  (memq (dont-optimize (flavor-name f2))
                        (dont-optimize (flavor-depends-on-all f1)))
                  t)))
        (f2 nil)
;       ((class-symbolp x)
;        (or (memq y '(atom entity))
;            (and (class-symbolp y)
;                 (subclass-of-class-symbol-p x y))))
;       ((class-symbolp y) nil)
        ((or (and (setq t1 (getdecl x 'defstruct-description))
                  (defstruct-description-named-p t1))
             (get x 'defstruct-named-p))
         (if (memq x '(structure atom array common))
             t
           (and (or (and (setq t2 (getdecl y 'defstruct-description))
                         (defstruct-description-named-p t2))
                    (get y 'defstruct-named-p))
                (do ((symbol x
                             (and (setq t1 (getdecl symbol 'defstruct-description))
                                  (car (defstruct-description-include t1)))))
                    ((null symbol) nil)
                  (and (eq y symbol) (return t))))))
        (t (not (not (memq x (get y 'subtypes)))))))

;;;; Comparing canonicalized types

;; used by array simple-array
(defun array-subtypep (type1 type2 dependencies &aux known tem)
  (unless (or (null (cdr type2))                ;.              (array)
              (eq (cadr type2) '*)              ;.              (array * ...)
              (and (cdr type1)
                   (neq (cadr type1) '*)        ;(array x ...)
                   (progn
                     (multiple-value-setq (tem known dependencies)
                       (subtypep-1 (cadr type1) (cadr type2) dependencies))
                     (setq tem (and tem (subtypep-2 (cadr type1) (cadr type2))))
                     (if known tem t))))
    (return-from array-subtypep (values nil t dependencies)))
  (if (or (null (cddr type2))                   ;.              (array x)
          (eq (caddr type2) '*)                 ;.              (array x *)
          (and (cddr type1)
               (neq (caddr type1) '*)           ;(array x y ...)
               (= (if (numberp (caddr type1)) (caddr type1)
                    (length (caddr type1)))
                  (if (numberp (caddr type2)) (caddr type2)
                    (length (caddr type2))))
               (cond
                 ((not (consp (caddr type1)))
                  (= (caddr type1) (if (consp (caddr type2)) (length type2) type2)))
                 ((not (consp (caddr type2)))
                  (= (caddr type2) (length type1)))
                 (t (do ((1tail (caddr type1) (cdr 1tail))
                         (2tail (caddr type2) (cdr 2tail)))
                        ((null 1tail) t)
                      (unless (or (eq (car 2tail) '*)
                                  (eql (car 1tail) (car 2tail)))
                        (return nil)))))))
      (if known (values t t dependencies) (values nil nil dependencies))
    nil))

;; used by non-complex-number rational real float short-float single-float
(defun dense-arithmetic-subtypep (type1 type2 dependencies)
  (values
    (and (or (memq (cadr type2) '(nil *))
             (and (not (memq (cadr type1) '(nil *)))
                  (if (and (consp (cadr type2)) (not (consp (cadr type1))))
                      (> (cadr type1) (caadr type2))
                    ( (if (consp (cadr type1))
                           (caadr type1) (cadr type1))
                       (if (consp (cadr type2))
                           (caadr type2) (cadr type2))))))
         (or (memq (caddr type2) '(nil *))
             (and (not (memq (caddr type1) '(nil *)))
                  (if (and (consp (caddr type2)) (not (consp (caddr type1))))
                      (< (caddr type1) (caaddr type2))
                    ( (if (consp (caddr type1))
                           (caaddr type1) (caddr type1))
                       (if (consp (caddr type2))
                           (caaddr type2) (caddr type2)))))))
    t
    dependencies))

;;;; disjoint-typep

;;; this variable really isn't as much use any more,
;;;  due to rampant type-canonicalization. Sigh.
(defconst *subtypep-pairwise-disjoint-sets*
  '((integer fixnum bignum)
    (rational ratio integer)
    (number rational float complex)
    (number non-complex-number complex)
    (list cons null)
    (sequence list sequence)
    (t cons symbol array number cl:character zl:character entity locative instance
       closure stack-group select compiled-function microcode-function)
    (t list number hash-table readtable package pathname stream random-state)))

(defun disjoint-typep (x y dependencies &aux t1 t2)
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (multiple-value-bind (x dependencies)
      (type-canonicalize x *use-subtypep-cache-p* dependencies)
    (multiple-value-bind (y dependencies)
        (type-canonicalize y *use-subtypep-cache-p* dependencies)
      (cond ((subtypep-2 y x)
             (return-from disjoint-typep (values nil t dependencies)))
            ((subtypep-2 x y)
             (return-from disjoint-typep (values nil t dependencies)))
            ((consp x)
             (case (car x)
               (or
                (loop with val = t
                      for x in (cdr x)
                   do (multiple-value-setq (t1 t2 dependencies)
                        (disjoint-typep x y dependencies))
                   when (not t2) (return-from disjoint-typep (values nil nil dependencies))
                     do (setq val (and val t1))
                   finally (return-from disjoint-typep (values val t dependencies))))
               (and
                (loop with val = t
                      for x in (cdr x)
                   do (multiple-value-setq (t1 t2 dependencies)
                        (disjoint-typep x y dependencies))
                   when t1 (return-from disjoint-typep (values t t dependencies))
                     do (setq val (and val t2))
                   finally (return-from disjoint-typep (values nil val dependencies))))
               (not
                (subtypep-1 y (cadr x) dependencies))
               (cl:member
                (loop for x in (cdr x)
                   do (multiple-value-setq (t1 t2 dependencies)
                        (subtypep-1 x y dependencies))
                   when (null t2) return (values nil nil dependencies)
                   when t1 return (values nil t dependencies)
                   finally (return (values t t dependencies))))
               (satisfies nil)
               (t
                (cond ((multiple-value-setq (nil nil dependencies)
                         (disjoint-typep (car x) y dependencies))
                       (values t t dependencies))
                      ((atom y) (values nil t dependencies))
                      ((setq t1 (get (car x) 'disjoint-typep-predicate))
                       (funcall t1 x y dependencies))
                      (t (values nil nil dependencies))))))
            ((not (atom y)) (disjoint-typep y x dependencies))
            (t (loop for (a . b) in *subtypep-pairwise-disjoint-sets*
                  when (and (subtypep-2 x a) (subtypep-2 y a))
                    do (let ((p (loop for tt in b
                                      when (subtypep-2 x tt) return tt))
                             (q (loop for tt in b
                                      when (subtypep-2 y tt) return tt)))
                         (when (and p q) (return (values (not (eq p q)) t dependencies))))
                  finally (return (values nil nil dependencies))))))))

;; used by array simple-array
(defun disjoint-array-typep (x y dependencies &aux (knownp t))
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (if (or (and (cddr x) (not (eq (caddr x) '*))
               (cddr y) (not (eq (caddr y) '*))
               (not (cond ((numberp (caddr x))
                           (if (numberp (caddr y))
                               (= x y)
                             (and (= (length y) x)
                                  (loop for z in y always (eq z '*)))))
                          ((numberp (caddr y))
                           (and (= (length x) y)
                                (loop for z in x always (eq z '*))))
                          (t (and (= (length x) (length y))
                                  (loop for z in x
                                        for w in y
                                      always (or (eq z '*) (eq w '*) (eq z w))))))))
          (and (not (eq (cadr x) '*))
               (not (eq (cadr y) '*))
               (multiple-value-setq (nil knownp dependencies)
                 (disjoint-typep (multiple-value-setq (nil dependencies)
                                   (type-canonicalize (cadr x)
                                                      *use-subtypep-cache-p* dependencies))
                                 (multiple-value-setq (nil dependencies)
                                   (type-canonicalize (cadr x)
                                                      *use-subtypep-cache-p* dependencies))
                                 dependencies))))
      (values t t dependencies)
      (values nil knownp dependencies)))

;; used by integer rational non-complex-number float short-float single-float real
(defun real-disjoint-typep (x y dependencies)
  (declare (values known-to-be-subtype known-whether-is-subtype dependencies))
  (let ((low1 (if (cdr x) (cadr x) '*))
        (low2 (if (cdr y) (cadr y) '*))
        (high1 (if (cddr x) (caddr x) '*))
        (high2 (if (cddr y) (caddr y) '*)))
    (tagbody
     retry
        (return-from real-disjoint-typep
          (if (cond ((eq low1 '*)
                     (and (neq high1 '*)
                          (neq low2 '*)
                          (cond ((consp high1)
                                 ( (car high1) (if (consp low2) (car low2) low2)))
                                ((consp low2)
                                 ( high1 (car low2)))
                                (t (< high1 low2)))))
                    ((eq low2 '*) (go swap))
                    ((eq high1 '*)
                     (and (neq high2 '*)
                          (cond ((consp low1)
                                 ( (if (consp high2) (car high2) high2) (car low1)))
                                ((consp high2)
                                 ( (car high2) low1))
                                (t (< high2 low1)))))
                    ((eq high2 '*) (go swap))
                    (t
                     (let ((t1 #'<) (t2 #'<))
                       (if (consp low1) (setq low1 (car low1) t1 #'))
                       (if (consp high1) (setq high1 (car high1) t2 #'))
                       (or (funcall t1 (if (consp high2) (car high2) high2) low1)
                           (funcall t2 high1 (if (consp low2) (car low2) low2))))))
              (values t t dependencies)
            (values nil nil dependencies)))
     swap
        (psetq low1 low2 low2 low1 high1 high2 high2 high1)
        (go retry))))


(defvar *array-element-type-hash-table* :unbound
  "Hash table of element-type for MAKE-ARRAY keyed by the type.")

(defun array-type-from-element-type (element-type)
  "Returns a symbol, such as ART-4B"
  (let ((default-cons-area background-cons-area))
    (unless (variable-boundp *array-element-type-hash-table*)
      (setq *array-element-type-hash-table* (make-hash-table :test #'equal :size 100.)))
    (unless (variable-boundp *subtypep-hash-table*)
      (setq *subtypep-hash-table* (make-hash-table :test #'equal :size 400.))))
  (let ((*use-subtypep-cache-p* t))
    (array-type-from-element-type-1 element-type)))

(defun compilation-array-type-from-element-type (element-type)
  (let ((*use-subtypep-cache-p* nil))
    (array-type-from-element-type-1 element-type)))

(defun array-type-from-element-type-1 (element-type)
  (cond ((cdr (assoc-equal element-type array-element-type-alist)))
        ((and *use-subtypep-cache-p*
              (car (gethash element-type *array-element-type-hash-table*))))
        (t
         (multiple-value-bind (canon dependencies)
             (type-canonicalize element-type *use-subtypep-cache-p* nil)
           (let ((value (or (cdr (assoc-equal canon array-element-type-alist))
                            (cond ((subtypep-1 canon 'fixnum dependencies)
                                   (cond ((subtypep-1 canon 'bit dependencies)
                                          'art-1b)      ;common case
                                         ((subtypep-1 canon '(mod #o10) dependencies)
                                          (if (subtypep-1 canon '(mod 4) dependencies)
                                              'art-2b 'art-4b))
                                         ((subtypep-1 canon '(mod #o200000) dependencies)
                                          (if (subtypep-1 canon '(mod #o400) dependencies)
                                              'art-8b 'art-16b))
                                         ((subtypep-1 canon '(signed-byte #o20) dependencies)
                                          'art-half-fix)
                                         (t 'art-q)))
                                  ((subtypep-1 canon 'cl:character dependencies)
                                   (cond ((subtypep-1 canon 'string-char dependencies)
                                          'art-string)
                                         ((subtypep-1 canon 'fat-char dependencies)
                                          'art-fat-string)
                                         (t 'art-q)))
                                  ((subtypep-1 canon 'float dependencies) 'art-float)
                                  ((subtypep-1 canon 'complex dependencies)
                                   (if (subtypep-1 canon '(complex float) dependencies)
                                       'art-complex-float 'art-complex))
                                  (t 'art-q)))))
             (prog1 value
                    (when *use-subtypep-cache-p*
                      (setq value (cons-in-area value dependencies background-cons-area))
                      (setf (gethash canon *array-element-type-hash-table*) value)
                      (setf (gethash element-type *array-element-type-hash-table*) value))))))))

(defun clear-cached-subtype-info (type)
  (when (variable-boundp *subtypep-hash-table*)
    (if type
        (maphash #'(lambda (key entry)
                     (when (memq type (subtypep-hash-table-element-dependencies entry))
                       (remhash key *subtypep-hash-table*)))
                 *subtypep-hash-table*)
      (clrhash *subtypep-hash-table*)))
  (when (variable-boundp *array-element-type-hash-table*)
    (if type
        (maphash #'(lambda (key entry)
                     (when (memq type (cdr entry))
                       (remhash key *array-element-type-hash-table*)))
                 *array-element-type-hash-table*)
      (clrhash *array-element-type-hash-table*))))

;;;; Open coding of TYPEP.

;In QCOPT:
;(defptimizer typep typep-two-args)
(defun typep-two-args (form &aux type)
  (cond ((and (= (length form) 3)
              (self-evaluating-p (caddr form)))
         (condition-case (error)
             (progn
               (setq type (if (consp (caddr form))
                              (cadr (caddr form))       ;(typep foo ':bar)
                              (caddr form)))            ;(typep foo :bar)
               (flet ((frob (type &aux tem)
                        (if (and (symbolp type)
                                 (setq tem (get type 'type-optimizer)))
                            (funcall tem form)
                          (if (and (symbolp type)
                                   (setq tem (get type 'type-alias-for)))
                              `(typep ,(cadr form) ',tem)
                            (cond ((symbolp type)
                                   (cond ((setq tem (get type 'type-optimizer))
                                          (funcall tem form))
                                         ((and (setq tem (get type 'type-predicate))
                                               (symbolp tem))
                                          `(,tem ,(cadr form)))
                                         ((setq tem (or (rassq type type-of-alist)
                                                        (rassq type typep-one-arg-alist)))
                                          `(eq (%data-type ,(cadr form)) ,(car tem)))
                                         ((getdecl type 'si::defstruct-description)
                                          `(typep-structure-or-flavor . ,(cdr form)))
                                         ;; defflavor is so nauseating...
                                         ((setq tem (or (getdecl type 'si:flavor)
                                                        (get type 'si:flavor)))
                                          ;; this is from get-flavor-tracing-aliases
                                          ;; let's hear it for modularity...
                                          (if (flavor-get tem :alias-flavor)
                                              (setq type (car (dont-optimize
                                                                (flavor-depends-on tem))))
                                            (setq type tem))
                                          `(typep-structure-or-flavor
                                             ,(cadr form)
                                             ',(dont-optimize (flavor-name type))))
;                                        ((class-symbolp type)
;                                         `(subinstance-of-class-symbol-p ,(cadr form)
;                                                                         ',type))
                                         (t form)))
                                  (t
                                   (let ((typecar (get (car type) 'type-alias-for
                                                       (car type))))
                                     (cond ((setq tem (get typecar 'type-optimizer))
                                            (apply tem form (cdr type)))
                                           ((and (setq tem (get typecar 'type-predicate))
                                                 (symbolp tem))
                                            `(,tem ,(cadr form)))
                                           (t form)))))))))
                  (let ((tem (frob type)))
                    (cond ((not (equal tem form))
                           tem)
                          (t
                           (setq form (frob (type-canonicalize type nil nil)))
                           tem)))))
           (error (compiler::warn 'compiler::bad-type-specification :implausible
                                  "Error expanding type specification ~S for ~S:~%   ~A"
                                  (caddr form) 'typep error)
                   form)))
        ((cdddr form)
         (compiler::warn 'compiler::bad-type-specification :implausuble
                         "~S is a malformed type-specification" (cddr form))
         form)
        (t form)))

;;;; Coerce

(defun coerce (object result-type &aux canon)
  "Coerce OBJECT to an object of type RESULT-TYPE.  Only certain coercions are allowed.
Any sequence can be coerced to any sequence type if the elements are legal.
Strings, symbols and integers can be coerced to type CHARACTER.
Any number can be coerced to type COMPLEX.
Any real number can be coerced to any floating point number type."
  (setq canon (type-canonicalize result-type nil nil))
  (if (typep object canon)
      object
    (block nil
      (case (if (atom canon) canon (car canon))
        (list
         (return (coerce-to-list object)))
        (short-float
         (when (realp object) (return (small-float (realpart object)))))
        (single-float
         (when (realp object) (return (float (realpart object)))))
        (float
         (when (realp object) (return (if (small-floatp object)
                                          object
                                        (float (realpart object))))))
        (complex
         (when (typep object 'number)
           (return (if (memq (cadr-safe canon) '(nil *))
                       (if (complexp object) object (complex object))
                     (if (complexp object)
                         (complex (coerce (%complex-real-part object) (cadr canon))
                                  (coerce (%complex-imag-part object) (cadr canon)))
                       (complex (coerce object (cadr canon))))))))
        (cl:character
         (return (cl:character object)))
        ((array simple-array)
         (when (and (typep object 'sequence)
                    (or (atom canon) (null (caddr canon)) (eq (list-length (caddr canon)) 1)))
           (return (coerce-to-vector object
                                     (if (or (atom canon)
                                             (eq (cadr canon) '*))
                                         'art-q
                                       (array-type-from-element-type (cadr canon)))
                                     (eq (if (atom canon) canon (car canon))
                                         'simple-array))))))
      ;; If it did not already RETURN, this coercion is not allowed.
      (ferror "~S cannot be coerced to type ~S" object result-type))))

(defun cl:character (x)
  "Convert X to a character if possible."
  (cond ((characterp x) x)
        ((numberp x)
         (int-char x))
        ((and (stringp x) (= (length x) 1))
         (char x 0))
        ((and (symbolp x) (= (length (symbol-name x)) 1))
         (char (symbol-name x) 0))
        (t (ferror "Cannot coerce ~S into a character" x))))

(defun coerce-to-vector (object array-type &optional simplep &aux vector length)
  (etypecase object
    (vector
     (cond ((and (= (symbol-value (array-type object))
                    (if (symbolp array-type) (symbol-value array-type) array-type))
                 (or (not simplep) (simple-vector-p object)))
            object)
           (t
            (setq length (length object))
            (setq vector (zl:make-array length :type array-type))
            (dotimes (i length)
              (setf (cl:aref vector i) (cl:aref object i)))
            vector)))
    (list
     (setq length (length object))
     (setq vector (zl:make-array length :type array-type))
     (do ((i 0 (1+ i))
          (l object (cdr l)))
         ((null l))
       (setf (cl:aref vector i) (car l)))
     vector)))

(deff coerce-to-array-optimized 'coerce-to-vector)  ;Still used by old compiled code.

(defun coerce-to-list (vector)
  (cond ((cl:listp vector) vector)
        (t
         (check-type vector vector "a sequence")
         (let* ((length (length vector))
                (list (make-list length))
                (l list))
           (dotimes (x length)
             (setf (car l) (cl:aref vector x))
             (setq l (cdr l)))
           list))))

(defun coerce-optimizer (form)
  (let (frob type canon)
    (cond ((not (list-match-p form `(coerce ,frob ',type)))
           form)
          (t
           (setq canon (type-canonicalize type nil nil))
           (case (if (atom canon) canon (car canon))
             (list
              (once-only (frob)
                `(if (consp ,frob) ,frob (coerce-to-list ,frob))))
             (short-float `(small-float ,frob)) ;not strictly correct, since works on complex
             (single-float `(float ,frob))      ;ditto
             (float
              (once-only (frob)                 ;ditto
                `(if (small-floatp ,frob) ,frob (float ,frob))))
             ((t) frob)
             (cl:character `(cl:character ,frob))
             (complex
              (if (memq (cadr-safe canon) '(nil *))
                  (once-only (frob)
                    `(if (complexp ,frob) ,frob
                       (complex frob)))
                (once-only (frob)
                  `(if (complexp ,frob)
                       (%complex-cons (coerce (%complex-real-part ,frob) ',(cadr canon))
                                      (coerce (%complex-imag-part ,frob) ',(cadr canon)))
                     (complex (coerce ,frob ',(cadr canon)))))))
             ;;; $$$ Added, mostly to avoid stupid compiler warnings <21-Nov-88 keith>
             (rational
              (if (consp canon) form `(rational ,frob)))
             (array
              `(coerce-to-vector
                 ,frob
                 ',(array-type-from-element-type
                     (if (atom canon) t (cadr canon)))
                 nil))
             (simple-array
              `(coerce-to-vector
                 ,frob
                 ',(array-type-from-element-type
                     (if (atom canon) t (cadr canon)))
                 t))
             (t (compiler::warn 'compiler::bad-coerce :improbable
                                "Do not know how to coerce to type ~S" type)
                form))))))

;;;; Pretty names for types (used in CHECK-TYPE, ETYPECASE, etc)

(defvar *type-pretty-name-hash-table* :unbound
  "A hash table containing cached pretty names for types")

(defun type-pretty-name (type)
  "Return a string containing a noun phrase describing objects of type TYPE."
  (unless (variable-boundp *type-pretty-name-hash-table*)
    (let ((default-cons-area background-cons-area))
      (setq *type-pretty-name-hash-table* (make-hash-table :test #'equal :size 400.))))
  (or (gethash type *type-pretty-name-hash-table*)
      (let (pretty-name)
        ;; Prevent lossage if TYPE was consed in a temporary area.
        (setq type (copytree type background-cons-area)
              pretty-name
                   (cond ((symbolp type)
                          (or (get type 'type-name)
                              (string-append-a-or-an
                                (string-subst-char #\space #\-
                                                   (string-downcase (format nil "~A" type))
                                                   nil))))
                         ((and (consp type)
                          (funcall (get (car type) 'type-name-function #'ignore) type)))
                         (t (string-append (format nil "an object of type ~S" type)))))
        (let ((default-cons-area background-cons-area))
          (setq pretty-name (copy-seq pretty-name)))
        (setf (gethash type
                       *type-pretty-name-hash-table*) pretty-name
;             (gethash (type-canonicalize type nil nil)
;                      *type-pretty-name-hash-table*) pretty-name)
              ))))

(defprop select "a select-method" type-name)
(defprop fix "an integer" type-name)
(defprop float "a floating-point number" type-name)
(defprop real "a real number" type-name)
(defprop null "NIL" type-name)
(defprop complex "a complex number" type-name)
(defprop non-complex-number "a non-complex number" type-name)

(defun (:property or type-name-function) (type)
  (setq type (block or
               (mapcan #'(lambda (x)
                           (cond ((eq (car-safe x) 'or)
                                  (cdr x))
                                 ((eq x t)
                                  (return-from or t))
                                 ((eq x nil)
                                  ())
                                 (t (list x))))
                       (cdr type))))
  (if (cdr type)
      (string-append
        (format:output nil
          (do ((tail type (cdr tail)))
              ((null tail))
            (unless (cdr tail)
              (princ "or "))
            (princ (type-pretty-name (car tail)))
            (when (cdr tail)
              (if (cddr tail)
                  (princ ", ")
                (tyo #\space))))))
    (type-pretty-name (car type))))


(defun (:property and type-name-function) (type)
  (setq type (block and
               (mapcan #'(lambda (x)
                           (cond ((eq (car-safe x) 'and)
                                  (cdr x))
                                 ((eq x nil)
                                  (return-from and nil))
                                 ((eq x t)
                                  ())
                                 (t (list x))))
                       (cdr type))))
  (if (cdr type)
      (string-append
        (format:output nil
          (princ (type-pretty-name (car type)))
          " which is also "
          (do ((tail (cdr type) (cdr tail)))
              ((null tail))
            (unless (or (cdr tail) (not (cddr type)))
              (princ "and "))
            (princ (type-pretty-name (car tail)))
            (when (cdr tail)
              (if (cddr tail)
                  (princ ", ")
                (tyo #\space))))))
    (type-pretty-name (car type))))

(defun (:property not type-name-function) (type)
  (string-append "not " (type-pretty-name (cadr type))))

(defprop zl:member member-type-name-function type-name-function)
(defprop cl:member member-type-name-function type-name-function)
(defun member-type-name-function (type &aux (len (length (cdr type))))
  (case len
    (0 nil)
    (1 (string-append (format nil "~S ~S" 'eql (cadr type))))
    (2 (string-append (format nil "~S either ~S or ~S" 'eql (cadr type) (caddr type))))
    (t (string-append
         (format:output nil
           (princ 'eql)
           (princ " one of ")
           (do ((tail (cdr type) (cdr tail)))
               ((null tail))
             (unless (cdr tail)
               (princ "or "))
             (prin1 (car tail))
             (when (cdr tail)
               (if (cddr tail)
                   (princ ", ")
                 (tyo #\space)))))))))

(defun (:property fixnum type-name-function) (type)
  (integer-type-name type "fixnum" "a "))
(defun (:property integer type-name-function) (type)
  (integer-type-name type "integer" "an "))
(defun integer-type-name (type noun article &aux low low-inclusive high high-inclusive)
  (setq low (cond ((null (cdr type)) '*)
                  ((consp (cadr type)) (floor (1+ (car (cadr type)))))
                  ((integerp (cadr type))
                   (setq low-inclusive t)
                   (cadr type))
                  (t (cadr type))))
  (setq high (cond ((null (cddr type)) '*)
                   ((consp (caddr type)) (ceiling (1- (car (caddr type)))))
                   ((integerp (caddr type))
                    (setq high-inclusive t)
                    (caddr type))
                   (t (caddr type))))
  (cond ((and (eq low '*) (eq high '*))
         (string-append article noun))
        ((and (eq low 0) (eq high '*))
         (string-append "a non-negative " noun))
;         ((and (eq high 0) (eq low '*))
;          (string-append "a non-positive " noun))
        ((eq high '*)
         (cond ((not (integerp low)))
               (low-inclusive
                (format nil "~A~A greater than or equal to ~D" article noun low))
               (t
                (format nil "~A~A greater than ~D" article noun (1- low)))))
        ((eq low '*)
         (cond ((not (integerp high)))
               (high-inclusive
                (format nil "~A~A less than or equal to ~D" article noun high))
               (t
                (format nil "~A~A less than ~D" article noun (1+ high)))))
        ((not (and (integerp low) (integerp high)))
         nil)
        ((= low high)
         (format nil "the ~A ~D" noun low))
        ((= high (1+ low))
         (format nil "either ~D or ~D" low high))
        ((< high low)
         nil)
        (t
         (format nil "~A~A between ~D and ~D (incl)" article noun low high))))

(defun (:property real type-name-function) (type)
  (real-type-name-function "real number" "number" type))
(defun (:property float type-name-function) (type)
  (real-type-name-function "float" "float" type))
(defun (:property short-float type-name-function) (type)
  (real-type-name-function "short float" "short-float" type))
(defun (:property non-complex-number type-name-function) (type)
  (real-type-name-function "non-complex number" "number" type))
(defun real-type-name-function (string short-string type)
  (let ((low (if (null (cdr type)) '* (cadr type)))
        (high (if (null (cddr type)) '* (caddr type)))
        lowex highex)
    (if (consp low) (setq low (car low) lowex t))
    (if (consp high) (setq high (car high) highex t))
    (cond ((and (eq low '*) (eq high '*))
           (string-append "a " string))
          ((and (eq low 0) (eq high '*))
           (if lowex
               (string-append "a positive " string)
             (string-append "a non-negative " string)))
          ((and (eq high 0) (eq low '*))
           (if highex
               (string-append "a negative " string)
             (string-append "a non-positive " string)))
          ((eq high '*)
           (format nil "a ~A ~:[~;>~] ~D" string lowex low))
          ((eq low '*)
           (format nil "a ~A ~:[~;<~] ~D" string highex high))
          (t (format nil "a ~A satisfying ~D ~:[~;<~] ~A ~:[~;<~] ~D"
                     string low lowex short-string highex high)))))

(defun (:property complex type-name-function) (type)
  (case (cadr type)
    ((nil * non-complex-number) "a complex number")
    (rational "a rational complex number")
    (short-float "a complex number with short-float components")
    (single-float "a complex number with single-float components")
    (long-float "a complex number with long-float components")
    (double-float "a complex number with double-float components")
    (float "a complex number with floating-point components")
    (t nil)))


;;; ugly hairy crocks
;;; can you think of a better way?
(defun array-type-subtypep (array-type1 array-type2)
  "Returns an array-type code, such as #.art-q"
  (if (fixnump array-type1)
      (if (zerop (ldb %%array-type-field array-type1))
          (setq array-type1 (dpb array-type1 %%array-type-field 0))
        (setq array-type1 (mask-field %%array-type-field array-type1)))
    (setq array-type1 (mask-field %%array-type-field (symeval array-type1))))
  (if (fixnump array-type2)
      (if (zerop (ldb %%array-type-field array-type2))
          (setq array-type2 (dpb array-type2 %%array-type-field 0))
        (setq array-type2 (mask-field %%array-type-field array-type2)))
    (setq array-type2 (mask-field %%array-type-field (symeval array-type2))))
  (case array-type2
    (#.art-q
     t)
    (#.art-q-list
     (not (eq array-type1 #.art-q)))
    ((#.art-stack-group-head #.art-special-pdl #.art-reg-pdl)
     (not (memq array-type1 '(#.art-q #.art-q-list))))
    (#.art-string
     (eq array-type1 #.art-string))
    (#.art-fat-string
     (memq array-type1 '(#.art-string #.art-fat-string)))
    (#.art-32b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b #.art-16b #.art-32b)))
    (#.art-16b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b #.art-16b)))
    (#.art-half-fix
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b #.art-half-fix)))
    (#.art-8b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b #.art-8b)))
    (#.art-4b
     (memq array-type1 '(#.art-1b #.art-2b #.art-4b)))
    (#.art-2b
     (memq array-type1 '(#.art-1b #.art-2b)))
    (#.art-1b
     (memq array-type1 '(#.art-1b)))
    (#.art-complex
     (memq array-type1 '(#.art-complex #.art-complex-float #.art-complex-fps-float)))
    (#.art-complex-float
     (memq array-type1 '(#.art-complex-float #.art-complex-fps-float)))
    (#.art-float
     (memq array-type1 '(#.art-float #.art-fps-float)))
    (#.art-fps-float
     (memq array-type1 '(#.art-fps-float)))
    (t nil)))

;;; used by defstruct.
(defun array-type-supertype (array-type1 array-type2 &aux type)
  "Returns a symbol, such as ART-Q."
  (if (fixnump array-type1)
      (if (zerop (ldb %%array-type-field array-type1))
          (setq array-type1 (dpb array-type1 %%array-type-field 0))
        (setq array-type1 (mask-field %%array-type-field array-type1)))
    (setq array-type1 (mask-field %%array-type-field (symeval array-type1))))
  (if (fixnump array-type2)
      (if (zerop (ldb %%array-type-field array-type2))
          (setq array-type2 (dpb array-type2 %%array-type-field 0))
        (setq array-type2 (mask-field %%array-type-field array-type2)))
    (setq array-type2 (mask-field %%array-type-field (symeval array-type2))))
  (setq array-type1
        (or (selector nil (lambda (ignore x)
                            (setq type (cond ((eq x array-type1) array-type2)
                                             ((eq x array-type2) array-type1))))
              (#.art-error type)
              (#.art-q-list #.art-q-list)
              (#.art-q #.art-q)
              (#.art-stack-group-head #.art-stack-group-head)
              (#.art-special-pdl #.art-special-pdl)
              (#.art-reg-pdl #.art-reg-pdl)
              (#.art-fat-string
               (if (memq type '(#.art-string #.art-fat-string)) #.art-fat-string))
              (#.art-string
               (if (eq type #.art-string) #.art-string))
              (#.art-complex-fps-float
               (case type
                 (#.art-complex-fps-float #.art-complex-fps-float)
                 (#.art-complex-float #.art-complex-float)
                 (#.art-complex #.art-complex)))
              (#.art-complex-float
               (case type
                 ((#.art-complex-float #.art-float) #.art-complex-float)
                 (#.art-complex #.art-complex)))
              (#.art-complex
               (case type
                 ((#.art-complex #.art-float) #.art-complex)))
              (#.art-fps-float
               (case type
                 ((#.art-fps-float #.art-float) art-fps-float)))
              (#.art-float
               (case type
                 (#.art-float art-float)))
              (#.art-half-fix
               (case type
                 ((#.art-half-fix #.art-1b #.art-2b #.art-4b #.art-8b) #.art-half-fix)))
              (#.art-32b #.art-32b)
              (#.art-16b #.art-16b)
              (#.art-8b #.art-8b)
              (#.art-4b #.art-4b)
              (#.art-2b #.art-2b)
              (#.art-1b #.art-1b))
            #.art-q))                           ;rampaging dotulism
  (nth (ldb %%array-type-field array-type1) array-types))


;;;; predefined (non-user-changable) types

(eval-when (compile eval)
(defmacro define-system-type (name &body cruft)
  (declare (zwei:indentation 1 1))
  ;; pushnew may not be loaded yet
  (let ((returns (list `(or (memq ',name *standard-system-type-specifiers*)
                            (setq *standard-system-type-specifiers*
                                  (cons ',name *standard-system-type-specifiers*))))))
    (cond ((null cruft))                        ;just defining it
          ((and (null (cdr cruft))
                (symbolp (car cruft)))
           (push `(putprop ',name ',(car cruft) 'type-alias-for) returns))
          (t
           (dolist (c cruft)
             (flet ((frob (y)
                      (push (if (not (cl:listp (cadr c)))
                                `(putprop ',name ',(cadr c) ',y)
                              `(defun (:property ,name ,y) . ,(cdr c)))
                            returns)))

               (ecase (car c)
                 (canonicalizer (frob 'type-canonicalizer))
                 (expander (frob 'type-expander))
                 (optimizer (frob 'type-optimizer))
                 (predicate (frob 'type-predicate))
                 (subtypep-predicate (frob 'subtypep-predicate))
                 (disjoint-typep-predicate (frob 'disjoint-typep-predicate))
                 (subtypes (push `(putprop ',name ',(cdr c) 'subtypes) returns)))))))
    `(local-declare ((function-parent ,name define-system-type))
       (record-source-file-name ',name 'define-system-type)
       . ,(nreverse returns))))
) ;eval-when

(define-system-type function
  (expander (&rest ignore)
            (ferror "~S type-specifiers are not meaningful for testing objects against."
                    'function)))

(define-system-type values
  (expander (&rest ignore)
            (ferror "~S type-specifiers are not meaningful for testing objects against."
                    'values)))

(define-system-type satisfies
  (predicate (object predicate)
             (funcall predicate object))
  (optimizer (expression predicate)
             `(,predicate ,(cadr expression)))
  (canonicalizer (ignore dependencies typespec ignore)
                 (values typespec dependencies)))

(define-system-type or
  (predicate (object &rest types)
             (dolist (disjunct types)
               (when (typep object disjunct)
                 (return t))))
  (optimizer (expression &rest types)
             (let ((object (cadr expression)))
               (once-only (object)
                 `(or . ,(mapcar #'(lambda (type) `(typep ,object ',type))
                                 types)))))
  ;; canonicalizer done specially
  )

(define-system-type and
  (predicate (object &rest types)
             (dolist (conjunct types t)
               (unless (typep object conjunct)
                 (return nil))))
  (optimizer (expression &rest types)
             (let ((object (cadr expression)))
               (once-only (object)
                 `(and . ,(mapcar #'(lambda (type) `(typep ,object ',type))
                                  types)))))
  ;; canonicalizer done specially
  )

(define-system-type not
  (predicate (object type)
             (not (typep object type)))
  (optimizer (expression type)
             `(not (typep ,(cadr expression) ',type)))
  ;; canonicalizer done specially
  )

(define-system-type cl:member
  (predicate (object &rest members)
             (not (not (member-eql object members))))
  (optimizer (expression &rest members)
             `(member-eql ,(cadr expression) ',(copy-list members)))
  (expander (&rest members)
            (let ((length (length members)))
              (cond ((= length 0) nil)
                    ;; are we gratuitous yet?
                    ((and (= length 1)
                          (typep (car members) 'non-complex-number))
                     `(,(typecase (setq members (car members))
                          (integer 'integer)
                          (ratio 'rational)
                          (short-float 'short-float)
                          (single-float 'single-float))
                       ,members ,members))
                    ;; are we even more gratuitous?
                    ((loop for x on members always (and (integerp (car x))
                                                        (not (memq (car x) (cdr x))))
                        maximize (car x) into max
                        minimize (car x) into min
                        finally (if (= length (- max min -1))
                                    (return `(integer ,min ,max))
                                  (return nil))))
                    (t `(cl:member . ,(copy-list members))))))
  ;; canonicalizer done specially
  )
(define-system-type zl:member cl:member)

(define-system-type common
  (predicate commonp)
  (subtypes array simple-array vector string bit-vector
            simple-vector simple-bit-vector simple-string
            standard-char
            list symbol cons null
            number rational integer bignum fixnum ratio complex real non-complex-number
            float short-float single-float
            hash-table readtable package pathname stream random-state
            structure))

(define-system-type atom
  (predicate atom)
  (subtypes array simple-array vector string bit-vector
            simple-vector simple-bit-vector simple-string
            standard-char
            symbol null
            number rational integer bignum fixnum ratio complex real non-complex-number
            float short-float single-float
            hash-table readtable package pathname stream random-state
            structure
            closure entity instance stack-group select locative
            compiled-function microcode-function))

(define-system-type list
  (predicate cl:listp)
  (subtypes cons null))

(define-system-type symbol
  (predicate symbolp)
  (subtypes null keyword))

(define-system-type null
  (predicate null))

(define-system-type cons
  (predicate consp))

(define-system-type keyword
  (predicate keywordp))

(define-system-type sequence
  (predicate (object)
             (or (cl:listp object) (vectorp object)))
  (expander () '(or list vector))
  (optimizer (expression)
             (let ((object (cadr expression)))
               (once-only (object)
                 `(or (cl:listp ,object)
                      (vectorp ,object)))))
  (subtypes list cons null vector bit-vector string
            simple-vector simple-bit-vector simple-string))


(define-system-type nil
  (predicate (ignore) (false))
  (optimizer (expression)
             `(progn ,(cadr expression) nil)))

(define-system-type t
  (predicate (ignore) (true))
  (optimizer (expression)
             `(progn ,(cadr expression) t))
  ;; subtypes done specially
  )

(define-system-type stream
  (predicate streamp))


;; hacked by type-of-alist
(define-system-type compiled-function)
(define-system-type entity)
(define-system-type locative)
(define-system-type instance)
(define-system-type microcode-function)
(define-system-type select)
(define-system-type select-method select)

;; hacked by flavor or structure inheritance
(define-system-type hash-table basic-hash-table)
(define-system-type package)
(define-system-type pathname)
(define-system-type random-state)
(define-system-type readtable)

(define-system-type array
  (predicate (object &optional (element-type '*) (dimensions '*) &aux array-element-type)
             (and (arrayp object)
                  (or (eq dimensions '*)
                      (if (numberp dimensions)
                          (= dimensions (array-rank object))
                        (and (= (length dimensions) (array-rank object))
                             (dotimes (i (array-rank object) t)
                               (unless
                                 (or (eq (nth i dimensions) '*)
                                     (= (nth i dimensions) (array-dimension object i))
                                     (return nil)))))))
                  (or (eq element-type '*)
                      (equal element-type
                             (setq array-element-type (array-element-type object)))
                      ;; this is because of the declarative/descriminative type specification
                      ;; dichotomy which means that
                      ;;  (subtypep '(array string-char) '(array t)) => nil
                      (and (subtypep element-type array-element-type)
                           (subtypep array-element-type element-type)))))
  (optimizer optimize-array-typep)
  (subtypep-predicate array-subtypep)
  (disjoint-typep-predicate disjoint-array-typep)
  (canonicalizer canonicalize-array-type-specifier)
  (subtypes simple-array vector string bit-vector
            simple-vector simple-bit-vector simple-string))

(define-system-type simple-array
  (predicate (object &optional (element-type '*) (dimensions '*) &aux array-element-type)
             (and (simple-array-p object)
                  (or (eq element-type '*)
                      (equal element-type
                             (setq array-element-type (array-element-type object)))
                      (and (subtypep element-type array-element-type)
                           (subtypep array-element-type element-type)))
                  (or (eq dimensions '*)
                      (if (numberp dimensions)
                          (= dimensions (array-rank object))
                        (and (= (length dimensions) (array-rank object))
                             (dotimes (i (array-rank object) t)
                               (unless
                                 (or (eq (nth i dimensions) '*)
                                     (= (nth i dimensions) (array-dimension object i))
                                     (return nil)))))))))
  (optimizer optimize-array-typep)
  (subtypep-predicate array-subtypep)
  (disjoint-typep-predicate disjoint-array-typep)
  (canonicalizer canonicalize-array-type-specifier)
  (subtypes simple-vector simple-bit-vector simple-string))

;; used by array and simple-array
(defun optimize-array-typep (expression &rest args &aux (obj (cadr expression)))
  (if (atom (cadr (caddr expression)))
      (if (eq (cadr (caddr expression)) 'simple-array)
          `(simple-array-p ,obj)
          `(arrayp ,obj))
    (let ((pred (if (eq (car (cadr (caddr expression))) 'simple-array) 'simple-array-p 'arrayp)))
      (cond ((member-equal args '(() (*) (* *)))
             `(,pred ,obj))
            ((and (equal (car args) '*)
                  (and (consp (cadr args))
                       (loop for x in (cadr args)
                          always (or (eq x '*) (typep x '(fixnum 0))))))
             (once-only (obj)
               `(and (,pred ,obj)
                     (= (array-rank ,obj) ,(length (cadr args)))
                     ,@(loop for x in (cadr args)
                             for i from 0
                          when (fixnump x)
                            collect `(= (array-dimension ,obj ,i) ,x)))))
            (t expression)))))

(define-system-type vector
  (predicate (object &optional (element-type '*) (size '*) &aux array-element-type)
             (and (vectorp object)
                  (or (eq element-type '*)
                      (equal element-type
                             (setq array-element-type (array-element-type object)))
                      (and (subtypep element-type array-element-type)
                           (subtypep array-element-type element-type)))
                  (or (eq size '*)
                      (= size (array-length object)))))
  (optimizer (expression &rest args)
             (cond ((member-equal args '(() (*) (* *)))
                    `(vectorp ,(cadr expression)))
                   (t expression)))
  (expander (&optional (element-type '*) (size '*))
            `(array ,element-type (,size)))
  (subtypes string bit-vector simple-vector simple-bit-vector simple-string))

(define-system-type simple-vector
  (predicate (object &optional (size '*))
             (and (simple-vector-p object)
                  (or (eq size '*)
                      (= size (array-length object)))))
  (optimizer (expression &optional (size '*))
             (cond ((eq size '*)
                    `(simple-vector-p ,(cadr expression)))
                   (t
                    (setq expression (cadr expression))
                    (once-only (expression)
                      `(and (simple-vector-p ,expression)
                            (= (array-total-size ,expression) ,size))))))
  (expander (&optional (size '*))
            `(simple-array t (,size))))

(define-system-type string
  (predicate (object &optional (size '*))
             (and (stringp object)
                  (or (eq size '*)
                      (= size (array-length object)))))
  (optimizer (expression &rest args)
             (cond ((member-equal args '(() (*)))
                    `(stringp ,(cadr expression)))
                   (t
                    (setq expression (cadr expression))
                    (once-only (expression)
                      `(and (stringp ,expression)
                            (= (array-total-size ,expression) ,(car args)))))))
  (expander (&optional (size '*))
            `(array string-char (,size)))
  (subtypes simple-string))

(define-system-type simple-string
  (predicate (object &optional (size '*))
             (and (simple-string-p object)
                  (or (eq size '*)
                      (= size (array-length object)))))
  (optimizer (expression &rest args)
             (cond ((member-equal args '(() (*)))
                    `(simple-string-p ,(cadr expression)))
                   (t
                    (setq expression (cadr expression))
                    (once-only (expression)
                      `(and (simple-string-p ,expression)
                            (= (array-total-size ,expression) ,(car args)))))))
  (expander (&optional (size '*))
            `(simple-array string-char (,size))))

(define-system-type bit-vector
  (predicate (object &optional (size '*))
             (and (bit-vector-p object)
                  (or (eq size '*)
                      (= size (array-length object)))))
  (optimizer (expression &rest args)
             (cond ((member-equal args '(() (*)))
                    `(bit-vector-p ,(cadr expression)))
                   (t
                    (setq expression (cadr expression))
                    (once-only (expression)
                      `(and (bit-vector-p ,expression)
                            (= (array-total-size ,expression) ,(car args)))))))
  (expander (&optional (size '*))
            `(array bit (,size)))
  (subtypes simple-bit-vector))

(define-system-type simple-bit-vector
  (predicate (object &optional (size '*))
             (and (simple-bit-vector-p object)
                  (or (eq size '*)
                      (= size (array-length object)))))
  (optimizer (expression &rest args)
             (cond ((member-equal args '(() (*)))
                    `(simple-bit-vector-p ,(cadr expression)))
                   (t
                    (setq expression (cadr expression))
                    (once-only (expression)
                      `(and (simple-bit-vector-p ,expression)
                            (= (array-total-size ,expression) ,(car args)))))))
  (expander (&optional (size '*))
            `(simple-array bit (,size))))

(define-system-type structure
  (predicate (object) (not (null (named-structure-p object))))
  (optimizer (expression)
             `(not (null (named-structure-p ,(cadr expression))))))
(define-system-type named-structure structure)

(define-system-type cl:character
  (predicate characterp)
  (subtypes standard-char string-char fat-char))
(define-system-type zl:character cl:character)

(define-system-type string-char
  (predicate (object)
             (and (characterp object) (string-char-p object)))
  (expander ()
            `(and cl:character (satisfies string-char-p)))
  (optimizer (expression)
             (let ((object (cadr expression)))
               (once-only (object)
                 `(and (characterp ,object)
                       (string-char-p ,object)))))
  (subtypes standard-char))

(define-system-type fat-char
  (predicate (object)
             (and (characterp object)
                  ( object 0)
                  (< object (lsh 1 #o20))))
  (optimizer (expression)
             (setq expression (cadr expression))
             (once-only (expression)
               `(and ( ,expression 0)
                     (< ,expression (lsh 1 #o20)))))
  (subtypes string-char standard-char))

(define-system-type standard-char
  (predicate (object)
             (and (characterp object) (standard-char-p object)))
  (optimizer (expression)
             (let ((object (cadr expression)))
               (once-only (object)
                 `(and (characterp ,object)
                       (standard-char-p ,object))))))


(define-system-type number
  (predicate numberp)
  (subtypes rational integer fixnum bignum ratio complex real non-complex-number
            float short-float single-float))

(define-system-type complex
  (predicate (object &optional (type '*))
             (and (complexp object)
                  (or (memq type '(* non-complex-number))
                      (and (typep (%complex-real-part object) type)
                           (or (memq type '(float single-float short-float
                                                  double-float long-float))
                               (typep (%complex-imag-part object) type))))))
  (optimizer (expression &optional (type '*))
             (let ((object (cadr expression)))
               (if (memq type '(* non-complex-number))
                   `(complexp ,object)
                 (once-only (object)
                   `(and (complexp ,object)
                         ,(if (memq type
                                    '(float short-float single-float
                                            double-float long-float))
                              `(typep (%complex-real-part ,object) ',type)
                            `(and (typep (%complex-real-part ,object) ',type)
                                  (typep (%complex-imag-part ,object) ',type))))))))
  (subtypep-predicate (type1 type2 dependencies)
                      (multiple-value-bind (tem tem1 dependencies)
                          (subtypep-1 (cadr type1) (cadr type2) dependencies)
                        (values tem tem1 dependencies)))
  (disjoint-typep-predicate (x y dependencies &aux (knownp t))
                            (if (and (neq (cadr x) '*)
                                     (neq (cadr y) '*)
                                     (multiple-value-setq (nil knownp dependencies)
                                       (disjoint-typep
                                         (multiple-value-setq (nil dependencies)
                                           (type-canonicalize
                                             (cadr x) *use-subtypep-cache-p* dependencies))
                                         (multiple-value-setq (nil dependencies)
                                           (type-canonicalize
                                             (cadr x) *use-subtypep-cache-p* dependencies))
                                         dependencies)))
                                (values t t dependencies)
                              (values nil knownp dependencies)))
  (canonicalizer (record-dependencies dependencies typespec &optional (subtype '*) &aux type)
                 (values (cond ((memq subtype '(t * non-complex-number))
                                'complex)
                               (t
                                (multiple-value-setq (type dependencies)
                                  (type-canonicalize-1 subtype record-dependencies dependencies))
                                (if (equal type subtype)
                                    typespec
                                  `(complex ,type))))
                         dependencies)))


(defsubst non-complex-number-p (object)
  (and (numberp object) (not (complexp object))))

(defun non-complex-number-in-range-p (object low high type)
  (and (cond ((eq low '*) t)
             ((numberp low) ( low object))
             ((consp low) (< (car low) object))
             (t (ferror "Invalid lower limit ~S in ~A type specifier." low type)))
       (cond ((eq high '*) t)
             ((numberp high) ( high object))
             ((consp high) (> (car high) object))
             (t (ferror "Invalid upper limit ~S in ~A type specifier." high type)))))

(define-system-type non-complex-number
  (predicate (object &optional (low '*) (high '*))
             (and (non-complex-number-p object)
                  (non-complex-number-in-range-p object low high 'non-complex-number)))
  (optimizer (expression &optional (low '*) (high '*))
             (optimize-numeric-type-test 'non-complex-number-p expression low high))
  (subtypep-predicate dense-arithmetic-subtypep)
  (disjoint-typep-predicate real-disjoint-typep)
  (canonicalizer canonicalize-real-type-specifier)
  (subtypes rational integer fixnum ratio bignum float short-float single-float))

(define-system-type real
  (predicate (object &optional (low '*) (high '*))
             (and (realp object)
                  (setq object (realpart object))
                  (non-complex-number-in-range-p object low high 'real)))
  (optimizer (expression &optional (low '*) (high '*))
             (if (and (eq low '*) (eq high '*))
                 `(realp ,(cadr expression))
               (let ((object (cadr expression))
                     (o (gensym)))
                 `(let ((,o ,object))
                    (block real
                      (and (setq ,o (typecase ,o
                                      (complex (%complex-real-part ,o))
                                      (number ,o)
                                      (t (return-from real nil))))
                           ,(cond ((eq low '*)
                                   t)
                                  ((numberp low)
                                   `( ,o ,low))
                                  ((consp low)
                                   `(> ,o ,(car low))))
                           ,(cond ((eq high '*)
                                   t)
                                  ((numberp high)
                                   `( ,o ,high))
                                  ((consp high)
                                   `(< ,o ,(car high))))))))))
  (subtypep-predicate dense-arithmetic-subtypep)
  (disjoint-typep-predicate real-disjoint-typep)
  (canonicalizer canonicalize-real-type-specifier)
  (subtypes rational integer fixnum ratio bignum
            float short-float single-float non-complex-number))

(define-system-type rational
  (predicate (object &optional (low '*) (high '*))
             (and (rationalp object)
                  (non-complex-number-in-range-p object low high 'rational)))
  (optimizer (expression &optional (low '*) (high '*))
             (optimize-numeric-type-test 'rationalp expression low high))
  (subtypep-predicate dense-arithmetic-subtypep)
  (disjoint-typep-predicate real-disjoint-typep)
  (canonicalizer canonicalize-real-type-specifier)
  (subtypes integer ratio bignum fixnum))

(define-system-type ratio
  (predicate ratiop))

(define-system-type integer
  (predicate (object &optional (low '*) (high '*))
             (and (integerp object)
                  (non-complex-number-in-range-p object low high 'integer)))
  (optimizer (expression &optional (low '*) (high '*))
             (if (and (neq low '*)
                      (neq high '*)
                      ;;>> Doesn't barf on invalid bounds
                      (< (- (if (consp high) (1- (car high)) high)
                            (if (consp low) (1+ (car low)) low))
                         4))
                 (let ((object (cadr expression)))
                   `(memq ,object
                          ',(loop for i from (if (consp low) (1+ (car low)) low)
                                        upto (if (consp high) (1- (car high)) high)
                               collect i)))
               (optimize-numeric-type-test 'integerp expression low high)))
  (subtypep-predicate (type1 type2 dependencies)
                      (values
                        (and (or (memq (cadr type2) '(nil *))
                                 (and (not (memq (cadr type1) '(nil *)))
                                      ( (if (consp (cadr type1))
                                             (1+ (caadr type1)) (cadr type1))
                                         (if (consp (cadr type2))
                                             (1+ (caadr type2)) (cadr type2)))))
                             (or (memq (caddr type2) '(nil *))
                                 (and (not (memq (caddr type1) '(nil *)))
                                      ( (if (consp (caddr type1))
                                             (1- (caaddr type1)) (caddr type1))
                                         (if (consp (caddr type2))
                                             (1- (caaddr type2)) (caddr type2))))))
                        t
                        dependencies))
  (disjoint-typep-predicate real-disjoint-typep)
  (canonicalizer canonicalize-real-type-specifier)
  (subtypes bignum fixnum))
;>>??
(define-system-type fix integer)

(define-system-type bignum
  (predicate bigp)
  (expander ()
            '(or (integer * (#.most-negative-fixnum)) (integer (#.most-positive-fixnum) *))))

(define-system-type fixnum
  (predicate (object &optional (low '*) (high '*))
             (and (fixnump object)
                  (non-complex-number-in-range-p object low high 'fixnum)))
  (expander (&optional (low '*) (high '*))
            (if (and (eq low '*) (eq high '*))
                '(integer #.most-negative-fixnum #.most-positive-fixnum)
              `(integer ,(if (eq low '*) most-negative-fixnum low)
                        ,(if (eq high '*) most-positive-fixnum high))))
  (optimizer (expression &optional (low '*) (high '*))
             (if (and (neq low '*)
                      (neq high '*)
                      (< (- (if (consp high) (1- (car high)) high)
                            (if (consp low) (1+ (car low)) low))
                         4))
                 (let ((object (cadr expression)))
                   `(memq ,object
                          ',(loop for i from (if (consp low) (1+ (car low)) low)
                                        upto (if (consp high) (1- (car high)) high)
                               collect i)))
               (optimize-numeric-type-test 'fixnump expression low high))))

(defun optimize-numeric-type-test (predicate expression low high)
  (flet ((check-bound (n name)
           (cond ((or (non-complex-number-p n)
                      (and (consp n)
                           (null (cdr n))
                           (non-complex-number-p (car n)))
                      (eq n '*))
                  n)
                 (t
                  (compiler::warn 'compiler::bad-type-specification :impossible
                                  "Invalid ~A bound ~S in numeric range type specifier."
                                  name n)
                  '*))))
    (setq low (check-bound low "lower"))
    (setq high (check-bound high "upper"))
    (let ((object (cadr expression)))
      (cond ((and (eql low '*)
                  (eql high '*))
             `(,predicate ,object))
            ((and (numberp low)
                  (equal low high))
             (once-only (object)
               `(and (,predicate ,object)
                     (= ,object ,low))))
            (t
             (once-only (object)
               `(and (,predicate ,object)
                     ,(cond ((eq low '*)
                             t)
                            ((numberp low)
                             `( ,object ,low))
                            ((consp low)
                             `(> ,object ,(car low))))
                     ,(cond ((eq high '*)
                             t)
                            ((numberp high)
                             `( ,object ,high))
                            ((consp high)
                             `(< ,object ,(car high)))))))))))

(define-system-type mod
  (predicate (object &optional (limit '*))
             (and (integerp object)
                  (not (minusp object))
                  (typecase limit
                    ((member *) t)
                    ((integer 0) (> limit object))
                    (t (ferror "Invalid upper limit ~S in ~S type specifier."
                               limit 'mod)))))
  (expander (&optional (high '*))
            (if (eq high '*)
                `(integer 0)
              `(integer 0 ,(1- high))))
  (optimizer (expression &optional (limit '*))
             (optimize-numeric-type-test 'integerp expression 0 limit)))

(define-system-type bit
  (predicate (object)
             (memq object '(0 1)))
  (expander ()
            '(integer 0 1))
  (optimizer (expression)
             `(memq ,(cadr expression) '(0 1))))

(define-system-type unsigned-byte
  (predicate (object &optional (byte-size '*))
             (and (integerp object)
                  (not (minusp object))
                  (typecase byte-size
                    ((member *) t)
                    ((integer 0) (> (ash 1 byte-size) object))
                    (t (ferror "Invalid byte size ~S in ~S type specifier."
                               byte-size 'unsigned-byte)))))
  (expander (&optional (byte-size '*))
            (if (eq byte-size '*)
                '(integer 0)
              `(integer 0 ,(1- (ash 1 byte-size)))))
  (optimizer (expression &optional (byte-size '*))
             (typecase byte-size
               ((member *)
                (optimize-numeric-type-test 'integerp expression 0 '*))
               ((integer 0)
                (optimize-numeric-type-test 'integerp expression 0 (1- (ash 1 byte-size))))
               (t
                (compiler::warn 'compiler::bad-type-specification :impossible
                                "Invalid byte size ~S in ~S type specifier."
                                byte-size 'unsigned-byte)))))

(define-system-type signed-byte
  (predicate (object &optional (byte-size '*))
             (and (integerp object)
                  (typecase byte-size
                    ((member *) t)
                    ((integer 0)
                     (and (< object (ash 1 (1- byte-size)))
                          ( object (- (ash 1 (1- byte-size))))))
                    (t (ferror "Invalid byte size ~S in ~S type specifier."
                               byte-size 'signed-byte)))))
  (expander (&optional (byte-size '*))
            (if (eq byte-size '*)
                `integer
              `(integer ,(- (ash 1 (1- byte-size))) ,(1- (ash 1 (1- byte-size))))))
  (optimizer (expression &optional (byte-size '*))
             (typecase byte-size
               ((member *)
                `(integerp ,(cadr expression)))
               ((integer 0)
                (optimize-numeric-type-test 'integerp expression
                                            (- (ash 1 (1- byte-size)))
                                            (1- (ash 1 (1- byte-size)))))
               (t
                (compiler::warn 'compiler::bad-type-specification :impossible
                                "Invalid byte size ~S in ~S type specifier."
                                byte-size 'signed-byte)))))

(define-system-type float
  (predicate (object &optional (low '*) (high '*))
             (and (floatp object)
                  (non-complex-number-in-range-p object low high 'float)))
  (optimizer (expression &optional (low '*) (high '*))
             (optimize-numeric-type-test 'floatp expression low high))
  (subtypep-predicate dense-arithmetic-subtypep)
  (disjoint-typep-predicate real-disjoint-typep)
  (canonicalizer canonicalize-real-type-specifier)
  (subtypes short-float single-float))


(define-system-type short-float
  (predicate (object &optional (low '*) (high '*))
             (and (small-floatp object)
                  (non-complex-number-in-range-p object low high 'short-float)))
  (subtypep-predicate dense-arithmetic-subtypep)
  (disjoint-typep-predicate real-disjoint-typep)
  (canonicalizer canonicalize-real-type-specifier)
  (optimizer (expression &optional (low '*) (high '*))
             (optimize-numeric-type-test 'small-floatp expression low high)))
(define-system-type small-flonum short-float)
(define-system-type small-float short-float)

(define-system-type single-float
  (predicate (object &optional (low '*) (high '*))
             (and (floatp object)
                  (not (small-floatp object))
                  (non-complex-number-in-range-p object low high 'single-float)))
  (subtypep-predicate dense-arithmetic-subtypep)
  (disjoint-typep-predicate real-disjoint-typep)
  (canonicalizer canonicalize-real-type-specifier)
  (optimizer (expression &optional (low '*) (high '*))
             (optimize-numeric-type-test 'flonump expression low high)))
(define-system-type flonum single-float)
(define-system-type double-float single-float)
(define-system-type long-float single-float)

(define-system-type :array array)
(define-system-type :atom atom)
(define-system-type :bignum bignum)
(define-system-type :character cl:character)
(define-system-type :closure closure)
(define-system-type :compiled-function compiled-function)
(define-system-type :complex complex)
(define-system-type :cons cons)
(define-system-type :double-float double-float)
(define-system-type :entity entity)
(define-system-type :fix integer)
(define-system-type :fixnum fixnum)
(define-system-type :flonum single-float)
(define-system-type :float float)
(define-system-type :instance instance)
(define-system-type :integer integer)
(define-system-type :locative locative)
(define-system-type :long-float long-float)
(define-system-type :microcode-function microcode-function)
(define-system-type :named-structure structure)
(define-system-type :null null)
(define-system-type :number number)
(define-system-type :ratio ratio)
(define-system-type :rational rational)
(define-system-type :select select)
(define-system-type :select-method select)
(define-system-type :short-float short-float)
(define-system-type :single-float single-float)
(define-system-type :small-flonum short-float)
(define-system-type :string string)
(define-system-type :symbol symbol)
(define-system-type :list cons)
