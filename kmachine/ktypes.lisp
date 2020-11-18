;-*- Mode:LISP; Package:LISP-INTERNALS; Lowercase:T; Base:10; Readtable:CL -*-

;;; Types defined with DEFTYPE have a type-expander declaration,
;;; the value of which is an expander function to compute a new type specifier.
;;; It gets one argument, the type specifier.

;;; System types have a type descriptor
(defstruct (type-descriptor (:conc-name td-))
  EXPANDER
   ;; Expander function for a system type.
  PREDICATE
   ;; A function to test an object for membership in the type.
   ;; It gets the object as first arg, and any elements of the type specifier
   ;; except for the keyword itself as additional args.
  OPTIMIZER
   ;; An optimizer function for compiling calls to TYPEP.
   ;; Its first argument is the expression which calls TYPEP.
   ;; Its remaining args are the elements of the type specifier, except the first.
   ;; It can return the original call to TYPEP if it has nothing better to optimize to.
  NAME
   ;; A string, including a definite article, which
   ;; is used as the name of this type when it appears as an atom.
  NAME-FUNCTION
   ;; A function to compute the name of types which are lists
   ;; starting with this type.
  CANONICALIZER
  SUBTYPES
  )

;;; Interpreted calls to TYPEP use TYPE-PREDICATE and TYPE-EXPANDER
;;; Compilation uses TYPE-OPTIMIZER and TYPE-EXPANDER props.
;;; Compilation can also use the TYPE-PREDICATE
;;;  compiling a call to that function rather than to TYPEP,
;;;  but only if the predicate is a symbol.

;;; * CAUTION: you cannot simply define any new type with a TYPE-PREDICATE
;;; * because it needs to be wired into the SUBTYPEP data structures.
;;; * Defining types with TYPE-EXPANDERs (ie, use of DEFTYPE) is ok
;;; * because they will get expanded by SUBTYPEP, so they don't really
;;; * pose a new problem.


(defvar *type-descriptor-table* (make-hash-table))

(defmacro type-descriptor (type)
  `(gethash ,type *type-descriptor-table*))

(defmacro deftype (name arglist &body body)
  "Defines NAME as a data type name for use in TYPEP, etc.
A list starting with NAME, used as a type specifier,
expands by binding the args in ARGLIST and then evaluating the BODY.
The value of BODY should be another type specifier.
Any optional arguments in ARGLIST which do not have default values specified
will be bound to * by default, rather than NIL."
  (check-type name symbol)
  (cond ((type-descriptor name)
         (error "~~S is the name of a standard type specifier used by the system.
Redefining it would probably break the world.~" name))
;       ((or (getdecl name 'defstruct-description)
;            (let ((tem (assq 'si::flavors file-local-declarations)))
;              (and tem (get tem name)))
;            (get name 'si::flavor))
;        (cerror "Yes, please. I want to lose. ~S ~S anyway"
;                "~*~S is already the name of a ~:[flavor~;structure~]
;  ~\(~S ~S ...) will cause (~S foo '~S) not to recognize existing
;~:[instances of that flavor~;structures of that type~] in new code, ~
;but not affect (~S foo '~S)~%in existing compiled code. You may lose!~"
;                'deftype name (getdecl name 'defstruct-description) 'deftype name 'typep name
;                (getdecl name 'defstruct-description) 'typep name))
        )
  (let ((argcopy (let ((optional-p nil))
                   (mapcar #'(lambda (elt)
                               (if (symbolp elt)
                                   (cond (optional-p
                                          (cond ((member elt lambda-list-keywords)
                                                 (setq optional-p nil)
                                                 elt)
                                                (t `(,elt '*))))
                                         ((eq elt '&optional)
                                          (setq optional-p t)
                                          elt)
                                         (t elt))
                                 elt))
                           arglist)))
        (doc (when (stringp (car body)) (car body)))
        (expander-name (make-symbol (concatenate 'lisp:string
                                                 (symbol-name name)
                                                 "-TYPE-EXPANDER"))))
    `(progn
       (eval-when (load eval)
         ;; this is bad, top level anonymous lambdas
         ;; should get compiled
         (defun ,expander-name ,argcopy
           . ,body)
         (nc:put-global-declaration ',name
                                    'type-expander
                                    #',expander-name))
       (eval-when (compile)
         (nc:put-local-declaration ',name
                                   'type-expander
                                   #'(lambda ,argcopy . ,body)))
       ',name)))




;;;; TYPE-OF

(defconstant non-complex-numeric-types
             '(integer rational ratio fixnum
                       bignum float short-float
                       single-float double-float
                       long-float bit))

(defconstant simple-numeric-types
             '(integer rational float short-float single-float double-float long-float))

(defconstant type-of-alist
          '((#.vinc:$$dtp-nil               . null)
            (#.vinc:$$dtp-symbol            . symbol)
            (#.vinc:$$dtp-character         . character)
            (#.vinc:$$dtp-cons              . cons)
            (#.vinc:$$dtp-fixnum            . fixnum)
            (#.vinc:$$dtp-bignum            . bignum)
            (#.vinc:$$dtp-short-float       . short-float)
            (#.vinc:$$dtp-single-float      . single-float)
            (#.vinc:$$dtp-double-float      . double-float)
            (#.vinc:$$dtp-rational          . rational)
            (#.vinc:$$dtp-complex           . complex)
            (#.vinc:$$dtp-compiled-function . compiled-function)
            ))

(defun type-of (object)
  "Returns a type-specifier describing the type OBJECT belongs to.
For example, (TYPE-OF 5) is FIXNUM"
  (let ((dtp (vinc:data-type object)))
    (cond
      ((eq dtp vinc:$$dtp-array)
       (cond                                    ;((named-structure-p object))
         ((stringp object) 'string)
         (t 'array)))
      ((cdr (assoc dtp type-of-alist :test #'=)))
      (t t))))


;;; TYPEP top-level

(defconstant typep-alist
          '((#.vinc:$$dtp-symbol            . symbol)
            (#.vinc:$$dtp-nil               . null)
            (#.vinc:$$dtp-character         . character)
            (#.vinc:$$dtp-cons              . cons)
            (#.vinc:$$dtp-fixnum            . fixnum)
            (#.vinc:$$dtp-bignum            . bignum)
            (#.vinc:$$dtp-rational          . ratio)
;           (#.vinc:$$dtp-locative          . locative)
            (#.vinc:$$dtp-compiled-function . compiled-function)
            (#.vinc:$$dtp-lexical-closure   . lexical-closure)
            (#.vinc:$$dtp-short-float       . short-float)
            (#.vinc:$$dtp-single-float      . single-float)
            (#.vinc:$$dtp-double-float      . double-float)))


(defun typep (object type)
  "T if OBJECT is of type TYPE"
  (let (tem structure-desc)
    (if (setq tem (or (rassoc type type-of-alist)
                      (rassoc type typep-alist)
                    ))
        (= (vinc:data-type object) (car tem))
      (let ((typespec (if (consp type) (car type) type))
            (specializations (unless (atom type) (cdr type))))
        (let ((type-desc (type-descriptor typespec)))
          (cond (type-desc
                 (cond
                   ;;>> Doesn't check wna to predicate function
                   ((setq tem (td-predicate type-desc))
                    (apply tem object specializations))
                   ;;>> Doesn't check wna to expander function
                   ((setq tem (td-expander type-desc))
                    (typep object (apply tem specializations)))
                   (t (typep object (cerror t nil 'invalid-type-specifier
                                            "~S is not a valid type specifier" type)))))
                ((setq tem (type-expander typespec))
                 (typep object (apply tem specializations)))
                (t (typep object (cerror t nil 'invalid-type-specifier
                                       "~S is not a valid type specifier" type)))))))))


;;;; Open coding of TYPEP.

(defun constant-value (constant)
  (cond
    ((and (consp constant)
          (eq (car constant) 'quote))
     (second constant))
    ((symbolp constant)
     (symbol-value constant))
    (t constant)))

(defun rewrite-typep (form)
  (if (and (cddr form)
           (constantp (third form)))
      (let ((type (constant-value (third form))))
        (let ((object (second form)))
          (let ((tem (try-to-rewrite-typep object type form)))
            (if (equal tem form)
                (try-to-rewrite-typep object (type-canonicalize type) form)
              tem))))
    form))


(defun try-to-rewrite-typep (object type form &aux tem)
  (if (symbolp type)
      (let ((type-desc (type-descriptor type)))
        (cond (type-desc
               (cond
                 ((setq tem (td-optimizer type-desc))
                  (funcall tem form))
                 ((and (setq tem (td-predicate type-desc))
                       (symbolp tem))
                  `(,tem ,object))))
              (t form)))
    (let ((td (type-descriptor (car type))))
      (cond ((null td) form)
            ((setq tem (td-optimizer td))
             (apply tem form (cdr type)))
            ((and (setq tem (td-predicate td))
                  (symbolp tem))
             ;; this doesn't seem quite right
             `(,tem ,object))
            (t form)))))



(defun type-canonicalize (typespec &aux tem)
  "Returns a typespec equivalent in meaning to TYPESPEC, but possibly simpler."
  (cond ((symbolp typespec)
         (let ((type-desc (type-descriptor typespec)))
           (cond
             (type-desc
              (if (setq tem (td-expander type-desc))
                  (funcall tem)
                typespec))
             (t (error "~S is not a known type specifier" typespec)))))
        ((and (consp typespec) (symbolp (car typespec)))
         (let ((type (car typespec))
               (specializations (cdr typespec)))
           (case type
             (or  (type-canonicalize-or  specializations))
             (and (type-canonicalize-and specializations))
             (not (type-canonicalize-not specializations))
             (t
              (if (every #'(lambda (elt) (eq elt '*)) specializations)
                  (type-canonicalize type) ;; (foo * * *) => foo
                (let ((type-desc (type-descriptor type)))
                  (cond
                    ((null type-desc)
                     (error "~S is not a known type specifier" typespec))
                    ((setq tem (td-expander type-desc))
                     (apply tem specializations))
                    ((setq tem (td-canonicalizer type-desc))
                     (apply tem typespec specializations))
                    (t
                     (error "The type specifier ~S may not be specialized." typespec)))))))))
        (t (error "~S cannot be a type specifier!" typespec))))


(defun typespec-type (s)
  (if (consp s)
      (car s)
    s))

(defun typespec-numeric-range (s)
  (if (consp s)
      (let ((lo (second s))
            (hi (third  s)))
        (unless lo (setq lo '*))
        (unless hi (setq hi '*))
        (values lo hi))
    (values '* '*)))

(defun typespec-ge (x y)
  (if (eq x '*)
      t                                 ;  *  any
    (if (eq y '*)
        nil                             ; any  *
      (if (consp x)
          (if (consp y)
              (>= (car x) (car y))      ; (x) (y)
            (> (car x) y))              ; (x)  y
        (if (consp y)
            (> x (car y))               ;  x  (y)
          (>= x y))))))                 ;  x   y

(defun typespec-gt (x y)
  (if (eq x '*)
      t                                 ;  *  any
    (if (eq y '*)
        nil                             ; any  *
      (if (consp x)
          (if (consp y)
              (> (car x) (car y))       ; (x) (y)
            (> (car x) y))              ; (x)  y
        (if (consp y)
            (> x (car y))               ;  x  (y)
          (> x y))))))

(defun typespec-le (x y)
  (if (eq y '*)
      t                                 ; any  *
    (if (eq x '*)
        nil                             ; *   any
      (if (consp x)
          (if (consp y)
              (<= (car x) (car y))      ; (x) (y)
            (< (car x) y))              ; (x)  y
        (if (consp y)
            (< x (car y))               ;  x  (y)
          (<= x y))))))

(defun typespec-lt (x y)
  (if (eq y '*)
      t                                 ; any  *
    (if (eq x '*)
        nil                             ; *   any
      (if (consp x)
          (if (consp y)
              (< (car x) (car y))       ; (x) (y)
            (< (car x) y))              ; (x)  y
        (if (consp y)
            (< x (car y))               ;  x  (y)
          (< x y))))))


(defun typespec-numeric-range-subset-p (s1 s2)
  "True if s1 range is a subset of s2 range"
  (multiple-value-bind (lo1 hi1) (typespec-numeric-range s1)
    (multiple-value-bind (lo2 hi2) (typespec-numeric-range s2)
      (and (typespec-ge lo1 lo2)
           (typespec-le hi1 hi2)))))

(defun typespec-numeric-subset-p (s1 s2)
  "True is s1 is a numeric subset of s1"
  (let* ((type1 (typespec-type s1))
         (type2 (typespec-type s2))
         (type-desc2 (type-descriptor type2))
         (simp1 (member type1 non-complex-numeric-types))
         (simp2 (member type2 non-complex-numeric-types)))
    (cond
      ((and simp1 (eq type2 'complex)) t)
      ((and simp1 (eq type2 'number)) t)
      ((not (and simp1 simp2)) nil)
      (t
       (and (member type1 (td-subtypes type-desc2))
            (typespec-numeric-range-subset-p s1 s2))))))

(defun typespec-contiguous-or-overlapping-numeric-pair (s1 s2)
  (let ((type (typespec-type s1)))
    (when (and (eq type (typespec-type s2))
               (member type simple-numeric-types))
      (multiple-value-bind (lo1 hi1) (typespec-numeric-range s1)
        (multiple-value-bind (lo2 hi2) (typespec-numeric-range s2)
          (when (typespec-gt lo1 lo2)
            (rotatef lo1 lo2)
            (rotatef hi1 hi2))
          (and (if (typespec-le hi1 hi2)
                   (if (eq hi1 '*)
                       t
                     (if (eq lo2 '*)
                         t
                       (if (eq type 'integer)
                           (progn
                             (when (consp hi1) (setq hi1 (1- (car hi1))))
                             (when (consp lo2) (setq lo2 (1+ (car lo2))))
                             (or (>= hi1 lo2)
                                 (= (1+ hi1) lo2)))
                         (if (consp hi1)
                             (if (consp lo2)
                                 (> (car hi1) (car lo2))
                               (> (car hi1) lo2))
                           (if (consp lo2)
                               (> hi1 (car lo2))
                             (>= hi1 lo2)))))))
               (type-canonicalize (list type lo1 hi2))))))))

(defun typespec-intersecting-numeric-pair (s1 s2)
  (let ((type (typespec-type s1)))
    (when (and (eq type (typespec-type s2))
               (member type simple-numeric-types))
      (multiple-value-bind (lo1 hi1) (typespec-numeric-range s1)
        (multiple-value-bind (lo2 hi2) (typespec-numeric-range s2)
          (when (typespec-gt lo1 lo2)
            (rotatef lo1 lo2)
            (rotatef hi1 hi2))
          (and (if (typespec-ge hi1 lo2)
                   (if (eq hi1 '*)
                       t
                     (if (eq lo2 '*)
                         t
                       (if (eq type 'integer)
                           (progn
                             (when (consp hi1) (setq hi1 (1- hi1)))
                             (when (consp lo2) (setq lo2 (1+ lo2)))
                             (>= hi1 lo2))
                         (if (consp hi1)
                             (if (consp lo2)
                                 (> (car hi1) (car lo2))
                               (> (car hi1) lo2))
                           (if (consp lo2)
                               (> hi1 (car lo2))
                             (>= hi1 lo2))))))
                 (list type hi2 lo1))))))))

(defun typespec-member-and-reduce (s1 s2)
  (let ((temp (remove-duplicates (intersection (cdr s1) (cdr s2)))))
    (if temp
        (cons 'member temp)
      nil)))

(defun typespec-member-or-reduce (s1 s2)
  (cons 'member (remove-duplicates (union (cdr s1) (cdr s2)))))

(defun typespec-array-p (s)
  (let ((type (typespec-type s)))
    (or (eq type 'array) (eq type 'simple-array))))

(defun typespec-both-array-p (s1 s2)
  (and (typespec-array-p s1)
       (typespec-array-p s2)))

(defun typespec-array-info (s)
  (if (consp s)
      (let* ((kind (car s))
             (info (cdr s))
             (type (car info))
             (dims (cadr info)))
        (if dims
            (values kind type dims)
          (values kind type '*)))
    (values s '* '*)))


(defun typespec-array-type-subset (s1 s2)
  (multiple-value-bind (kind1 type1 dims1) (typespec-array-info s1)
    (multiple-value-bind (kind2 type2 dims2) (typespec-array-info s2)
      (cond
        ((and (eq kind1 'array) (eq kind2 'simple-array)) nil)
        ((eq type2 '*) (values kind1 type1))
        ((equal type1 type2) (values kind1 type1))
        (t nil)))))

(defun typespec-array-dims-subset (s1 s2)
  (multiple-value-bind (kind1 type1 dims1) (typespec-array-info s1)
    (multiple-value-bind (kind2 type2 dims2) (typespec-array-info s2)
      (cond
        ((eq dims2 '*) dims1)
        ((eq dims1 '*) nil)
        ((not (= (length dims1) (length dims2))) nil)
        (t
         (let ((temp (mapcar #'(lambda (d1 d2)
                                 (if (or (eq d2 '*)
                                         (=  d1 d2))
                                     d1 nil))
                        dims1 dims2)))
           (if (member nil temp)
               nil
             temp)))))))

(defun typespec-array-subset (s1 s2)
  (multiple-value-bind (kind type) (typespec-array-type-subset s1 s2)
    (if kind
        (let ((dims (typespec-array-dims-subset s1 s2)))
          (if dims
              (list kind type dims)
            nil))
      nil)))

(defun typespec-array-and-reduce (s1 s2)
  (let ((temp (typespec-array-subset s1 s2)))
    (or temp
         (typespec-array-subset s2 s1))))

(defun typespec-array-or-reduce (s1 s2)
  (if (typespec-array-subset s1 s2)
      s2
    (if (typespec-array-subset s2 s1)
        s1
      nil)))


(defconstant non-and-reducible-types '(satisfies function values member))

(defun typespec-and-reductions (s1 s2)
  (let* ((type1 (typespec-type s1))
         (type2 (typespec-type s2))
         (subtypes1 (td-subtypes (type-descriptor type1)))
         (subtypes2 (td-subtypes (type-descriptor type2))))
    (cond
      ((equal s1 s2) s1)
      ((and (atom s1) (member type2 subtypes1)) s2)
      ((and (atom s2) (member type1 subtypes2)) s1)
      ((and (eq type1 'member) (eq type2 'member)) (typespec-member-and-reduce s1 s2))
      ((typespec-both-array-p type1 type2) (typespec-array-and-reduce s1 s2))
      ((member type1 non-and-reducible-types) 'non-reducible)
      ((member type2 non-and-reducible-types) 'non-reducible)
      ((typespec-numeric-subset-p s1 s2) s1)
      ((typespec-numeric-subset-p s2 s1) s2)
      (t
       (typespec-intersecting-numeric-pair s1 s2)))))


(defun typespec-or-reductions (s1 s2)
  (let* ((type1 (typespec-type s1))
         (type2 (typespec-type s2))
         (subtypes1 (td-subtypes (type-descriptor type1)))
         (subtypes2 (td-subtypes (type-descriptor type2))))
    (cond
      ((equal s1 s2) s1)
      ((and (atom s1) (member type2 subtypes1)) s1)
      ((and (atom s2) (member type1 subtypes2)) s2)
      ((and (eq type1 'member) (eq type2 'member)) (typespec-member-or-reduce s1 s2))
      ((typespec-both-array-p type1 type2) (typespec-array-or-reduce s1 s2))
      ((typespec-numeric-subset-p s1 s2) s2)
      ((typespec-numeric-subset-p s2 s1) s1)
      (t
       (typespec-contiguous-or-overlapping-numeric-pair s1 s2)))))


(defun type-canonicalize-not (not-term)
  (setq not-term (type-canonicalize not-term))
  (cond
    ((and (consp not-term) (eq (car not-term) 'not)) (cdr not-term))
    ((null not-term) t)
    ((eq not-term t) nil)
    (t (cons 'not not-term))))

(defun type-canonicalize-or (or-terms)
  (setq or-terms (mapcar #'type-canonicalize or-terms))
  (cond
    ((null or-terms) nil)
    ((null (cdr or-terms)) (car or-terms))
    (t
     (do ((changed t)
          temp)
         ((not changed)
          (if (cdr or-terms)
              (cons 'or or-terms)
            (car or-terms)))
       (setq changed nil)
       (do ((t1 or-terms (cdr t1)))
           ((null (cdr t1)))
         (do ((pre-t2 t1 (cdr pre-t2))
              (t2 (cdr t1) (cdr t2)))
             ((null t2))
           (when (setq temp (typespec-or-reductions (car t1) (car t2)))
             (rplaca t1 temp)
             (rplacd pre-t2 (cdr t2))
             (setq changed t))))))))

(defun type-canonicalize-and (and-terms)
  (setq and-terms (mapcar #'type-canonicalize and-terms))
  (cond
    ((null and-terms) nil)
    ((null (cdr and-terms)) (car and-terms))
    (t
     (do ((changed t)
          temp)
         ((not changed)
          (if (cdr and-terms)
              (cons 'and and-terms)
            (car and-terms)))
       (setq changed nil)
       (do ((t1 and-terms (cdr t1)))
           ((null (cdr t1)))
         (do ((pre-t2 t1 (cdr pre-t2))
              (t2 (cdr t1) (cdr t2)))
             ((null t2))
           (when (not (eq 'non-reducible (setq temp (typespec-and-reductions (car t1) (car t2)))))
             (if temp
                 (progn
                   (rplaca t1 temp)
                   (rplacd pre-t2 (cdr t2))
                   (setq changed t))
               (progn
                 (rplacd pre-t2 (cdr t2))
                 (if (eq t1 and-terms)
                     (setq and-terms (cdr and-terms))
                   (rplacd t1 (cdr t1))))))))))))

;;; used by array simple-array
(defun canonicalize-array-type-specifier (typespec &optional (element-type '*) (dimensions '*)
                                          &aux (type element-type))
  (or (member element-type '(t *))
      (setq type (type-canonicalize element-type)))
  (cond ((and (equal type element-type) (null (cddr typespec)))
         typespec)
        ((eq dimensions '*)
         `(,(car typespec) ,type))
        (t
         `(,(car typespec) ,type ,dimensions))))

;; used by non-complex-number real rational integer float short-float single-float
(defun canonicalize-real-type-specifier (typespec &optional (low '*) (high '*))
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
    typespec))


(defun array-type-from-element-type (element-type)
  "Return-from array-type-from-element-types a fixnum corresponding to the array types, eg ART-Q, ART-1B, etc."
  (let* ((canon (type-canonicalize element-type))
         (type  (typespec-type canon)
    (if (eq type 'integer))
        (multiple-value-bind (low hi) (typespec-numeric-range canon)
          (when (consp low) (setq low (1+ (car low))))
          (when (consp hi)  (setq hi  (1- (car hi))))
          (cond
            ((or (eq low '*) (eq hi '*))                       array:art-q)
            ((minusp low)
             (cond
               ((and (>= low -2.)          (< hi 2.))          array:art-2bs)
               ((and (>= low -8.)          (< hi 8.))          array:art-4bs)
               ((and (>= low -128.)        (< hi 128.))        array:art-8bs)
               ((and (>= low -32768.)      (< hi 32768.))      array:art-16bs)
               ((and (>= low -2147483648.) (< hi 2147483648.)) array:art-32bs)
               (t                                              array:art-q)))
            (t
             (cond
               ((< hi 2.)                                      array:art-1b)
               ((< hi 4.)                                      array:art-2b)
               ((< hi 16.)                                     array:art-4b)
               ((< hi 256.)                                    array:art-8b)
               ((< hi 65536.)                                  array:art-16b)
               ((< hi 4294967296.)                             array:art-32b)
               (t                                              array:art-q)))))
        (case type
          ((string-char standard-char)                         array:art-string)
;         ((character graphic-char)                            array:art-fat-string)
          (single-float                                        array:art-single-float)
          ((double-float long-float)                           array:art-double-float)
          (t                                                   array:art-q)))
      )))

;;;; Coerce

(defun coerce (object result-type &aux canon)
  "Coerce OBJECT to an object of type RESULT-TYPE.  Only certain coercions are allowed.
Any sequence can be coerced to any sequence type if the elements are legal.
Strings, symbols and integers can be coerced to type CHARACTER.
Any number can be coerced to type COMPLEX.
Any real number can be coerced to any floating point number type."
  (setq canon (type-canonicalize result-type))
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
        (character
         (return (character object)))
        ((array simple-array)
         (when (and (typep object 'sequence)
                    (or (atom canon) (null (caddr canon)) (eq (list-length (caddr canon)) 1)))
           (return (coerce-to-vector object
                                     (if (or (atom canon)
                                             (eq (cadr canon) '*))
                                         t
                                       (cadr canon))
                                     (eq (if (atom canon) canon (car canon))
                                         'simple-array))))))
      ;; If it did not already RETURN, this coercion is not allowed.
      (error "~S cannot be coerced to type ~S" object result-type))))

(defun coerce-to-vector (object element-type &optional simplep &aux vector length)
  (etypecase object
    (vector
     (cond (;(and (= (symbol-value (array-type object))
            ;       (if (symbolp array-type) (symbol-value array-type) array-type))
            ;    (or (not simplep) (simple-vector-p object)))
            nil
            (typep object (list 'VECTOR element-type))

            object)
           (t
            (setq length (length object))
            (setq vector (make-array length :element-type element-type))
            (dotimes (i length)
              (setf (aref vector i) (aref object i)))
            vector)))
    (list
     (setq length (length object))
     (setq vector (make-array length :element-type element-type))
     (do ((i 0 (1+ i))
          (l object (cdr l)))
         ((null l))
       (setf (aref vector i) (car l)))
     vector)))

;(deff coerce-to-array-optimized 'coerce-to-vector)  ;Still used by old compiled code.

(defun coerce-to-list (vector)
  (cond ((listp vector) vector)
        (t
         (check-type vector vector "a sequence")
         (let* ((length (length vector))
                (list (make-list length))
                (l list))
           (dotimes (x length)
             (setf (car l) (aref vector x))
             (setq l (cdr l)))
           list))))

(defun coerce-optimizer (form)
  (let (frob type canon)
    (cond ((not (list-match-p form `(coerce ,frob ',type)))
           form)
          (t
           (setq canon (type-canonicalize type))
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
             (character `(character ,frob))
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

;;;; predefined (non-user-changable) types

(defmacro define-type (name &body stuff)
  (declare (zwei:indentation 1 1))
  `(SETF (TYPE-DESCRIPTOR ',name)
         ,(if (and (null (cdr stuff))
                   (symbolp (car stuff)))
              `(TYPE-DESCRIPTOR ',(car stuff))
            `(MAKE-TYPE-DESCRIPTOR
               .
               ,(mapcan #'(lambda (c)
                            (list (car c)
                                  (cond ((eq (car c) :subtypes)
                                         `',(cdr c))
                                        ((listp (cadr c))
                                         `#'(LAMBDA . ,(cdr c)))
                                        (t `',(cadr c)))))
                        stuff)))))

(define-type function
  (:expander (&rest ignore)
             (error "~S type-specifiers are not meaningful for testing objects against."
                    'function)))

(define-type values
  (:expander (&rest ignore)
             (error "~S type-specifiers are not meaningful for testing objects against."
                    'values)))

(define-type satisfies
  (:predicate (object predicate)
              (funcall predicate object))
  (:optimizer (expression predicate)
             `(,predicate ,(cadr expression)))
  (:canonicalizer (rec-dep typespec spec)
                  (values typespec)))


(define-type or
  (:predicate (object &rest types)
              (dolist (disjunct types)
                (when (typep object disjunct)
                  (return t))))
  (:optimizer (expression &rest types)
              `(LET ((OBJECT ,(cadr expression)))
                 (OR . ,(mapcar #'(lambda (type) `(TYPEP OBJECT ',type))
                                types))))
  ;; canonicalizer done specially
  )

(define-type and
  (:predicate (object &rest types)
              (dolist (conjunct types t)
                (unless (typep object conjunct)
                 (return nil))))
  (:optimizer (expression &rest types)
              `(LET ((OBJECT ,(cadr expression)))
                 (AND . ,(mapcar #'(lambda (type) `(TYPEP OBJECT ',type))
                                 types))))
  ;; canonicalizer done specially
  )

(define-type not
  (:predicate (object type)
              (not (typep object type)))
  (:optimizer (expression type)
              `(NOT (TYPEP ,(cadr expression) ',type)))
  ;; canonicalizer done specially
  )

(defun non-complex-number-p (object)
  (and (numberp object) (not (complexp object))))


(define-type member
  (:predicate (object &rest members)
              (member object members))
  (:optimizer (expression &rest members)
              `(MEMBER ,(cadr expression) ',(copy-list members)))
  (:expander (&rest members)
             (member-expand members))
  )


(defun member-expand (mlist)
  (setq mlist (remove-duplicates mlist))
  (do* ((misc-list (let ((l (remove-if
                              #'(lambda (n) (and (numberp n) (not (complexp n))))
                              mlist)))
                     (when l (setq l (cons (cons 'member l) nil)))
                     (dolist (item mlist)
                       (cond
                         ((ratiop item)         (setq l (cons (list 'rational     item) l)))
                         ((short-float-p item)  (setq l (cons (list 'short-float  item) l)))
                         ((single-float-p item) (setq l (cons (list 'single-float item) l)))
                         ((double-float-p item) (setq l (cons (list 'double-float item) l)))))
                     l))
        (ilist (sort (remove-if-not #'integerp mlist) #'<) (cdr ilist))
        (i (car ilist) (car ilist))
        (min nil)
        (max nil))
       ((null ilist)
         (progn
           (when min (setq misc-list (cons (list 'integer min max) misc-list)))
           (if (cdr misc-list)
               (cons 'or misc-list)
             (car misc-list))))
    (cond
      ((null min)
       (setq min i)
       (setq max i))
      ((= i (1+ max))
       (setq max i))
      (t
        (setq misc-list (cons (list 'integer min max) misc-list))
        (setq min i)
        (setq max i)))))


(define-type common
  (:predicate commonp)
  (:subtypes array simple-array vector string bit-vector
             simple-vector simple-bit-vector simple-string
             standard-char
             list symbol cons null
             number rational integer bignum fixnum ratio complex real non-complex-number
             float short-float single-float double-float long-float
             hash-table readtable package pathname stream random-state
             structure))

(define-type atom
  (:predicate atom)
  (:subtypes array simple-array vector string bit-vector
             simple-vector simple-bit-vector simple-string
             standard-char
             symbol null
             number rational integer bignum fixnum ratio complex real non-complex-number
             float short-float single-float double-float long-float
             hash-table readtable package pathname stream random-state
             structure
             closure entity instance stack-group select locative
             compiled-function))

(define-type list
  (:predicate listp)
  (:subtypes cons null))

(define-type symbol
  (:predicate symbolp)
  (:subtypes null keyword))

(define-type null
  (:predicate null))

(define-type cons
  (:predicate consp))

(define-type keyword
  (:predicate keywordp))

(define-type sequence
  (:predicate (object)
              (or (listp object) (vectorp object)))
  (:expander () '(or list vector))
  (:optimizer (expression)
              `(LET ((OBJECT ,(cadr expression)))
                 (OR (LISTP OBJECT)
                     (VECTORP OBJECT))))
  (:subtypes list cons null vector bit-vector string
             simple-vector simple-bit-vector simple-string))


(define-type nil
  (:predicate (ignore) nil)
  (:optimizer (expression)
              `(PROGN ,(cadr expression) NIL)))

(define-type t
  (:predicate (ignore) t)
  (:optimizer (expression)
      `(PROGN ,(cadr expression) T))
  ;; subtypes done specially
  )

(define-type stream
  (:predicate streamp))

(define-type array
  (:predicate (object &optional (element-type '*) (dimensions '*) &aux array-element-type)
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
  (:optimizer optimize-array-typep)
  (:canonicalizer canonicalize-array-type-specifier)
  (:subtypes simple-array vector string bit-vector
             simple-vector simple-bit-vector simple-string))

(define-type simple-array
  (:predicate (object &optional (element-type '*) (dimensions '*) &aux array-element-type)
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
  (:optimizer optimize-array-typep)
  (:canonicalizer canonicalize-array-type-specifier)
  (:subtypes simple-vector simple-bit-vector simple-string))

;; used by array and simple-array
(defun optimize-array-typep (expression &optional (element-type '*) (dimensions '*))
  (let ((obj (second expression))
        (typespec (second (third expression))))
  (if (atom typespec)
      (if (eq typespec 'simple-array)
          `(SIMPLE-ARRAY-P ,obj)
          `(ARRAYP ,obj))
    (let ((pred (if (eq (car typespec) 'simple-array) 'simple-array-p 'arrayp)))
      (cond ((eq element-type '*)
             (cond
               ((eq dimensions '*)
               `(,pred ,obj))
               ((integerp dimensions)
                `(let ((OBJ ,obj))
                   (AND (,pred OBJ)
                     (= (ARRAY-RANK ,obj) ,dimensions))))
               ((listp dimensions)
                `(let ((OBJ ,obj))
                   (AND (,pred OBJ)
                     (= (ARRAY-RANK ,obj) ,(length dimensions))
                     ,@(do ((dims dimensions (cdr dims))
                            (i 0 (1+ i))
                            (forms '()))
                           ((null dims) forms)
                         (unless (eq (car dims) '*)
                           (push `(= (ARRAY-DIMENSION OBJ ,i) ,(car dims))
                                 forms))))))
               (t expression)))
            (t expression))))))

(define-type vector
  (:predicate (object &optional (element-type '*) (size '*) &aux array-element-type)
      (and (vectorp object)
           (or (eq element-type '*)
               (equal element-type
                      (setq array-element-type (array-element-type object)))
               (and (subtypep element-type array-element-type)
                    (subtypep array-element-type element-type)))
           (or (eq size '*)
               (= size (length object)))))
  (:optimizer (expression &optional (element-type '*) (dimensions '*))
       (cond ((and (eq element-type '*)
                   (eq dimensions '*))
              `(VECTORP ,(cadr expression)))
             (t expression)))
  (:expander (&optional (element-type '*) (size '*))
       `(ARRAY ,element-type (,size)))
  (:subtypes string bit-vector simple-vector simple-bit-vector simple-string))

(defmacro define-vector-subtype (name predicate exp-type &optional subtypes)
  `(define-type ,name
     (:predicate (object &optional (size '*))
         (and (,predicate object)
              (or (eq size '*)
                  (= size (length object)))))
     (:optimizer (expression &optional (size '*))
         (cond ((eq size '*)
                `(,',predicate ,(cadr expression)))
               (t
                `(LET ((OBJ ,(cadr expression)))
                   (AND (,',predicate OBJ)
                        (= (ARRAY-TOTAL-SIZE OBJ) ,size))))))
     (:expander (&optional (size '*))
                (append ',exp-type (list (list size))))
     (:subtypes . ,subtypes)))

(define-vector-subtype simple-vector simple-vector-p (simple-array t))
(define-vector-subtype string        stringp         (array string-char) (simple-string))
(define-vector-subtype simple-string simple-string-p (simple-array string-char))
(define-vector-subtype bit-vector    bit-vector-p    (array bit) (simple-bit-vector))
(define-vector-subtype simple-bit-vector simple-bit-vector-p (simple-array bit))

(define-type character
  (:predicate characterp)
  (:subtypes standard-char string-char))

(define-type string-char
  (:predicate (object)
      (and (characterp object)
           (string-char-p object)))
  (:expander ()
      `(and character (satisfies string-char-p)))
  (:optimizer (expression)
      `(LET ((OBJECT ,(cadr expression)))
         (AND (CHARACTERP OBJECT)
              (STRING-CHAR-P OBJECT))))
  (:subtypes standard-char))

(define-type standard-char
  (:predicate (object)
      (and (characterp object)
           (standard-char-p object)))
  (:optimizer (expression)
      `(LET ((OBJECT ,(cadr expression)))
         (AND (CHARACTERP OBJECT)
              (STANDARD-CHAR-P OBJECT)))))


(define-type number
  (:predicate numberp)
  (:subtypes rational integer fixnum bignum ratio complex real non-complex-number
             float short-float single-float double-float long-float))

(define-type complex
  (:predicate (object &optional (type '*))
      (and (complexp object)
           (or (member type '(t * non-complex-number))
               (and (typep (realpart object) type)
                    (or (member type '(float single-float short-float
                                             double-float long-float))
                        (typep (imagpart object) type))))))
  (:optimizer (expression &optional (type '*))
      (let ((object (cadr expression)))
        (if (member type '(t * non-complex-number))
            `(COMPLEXP ,object)
          `(LET ((OBJECT ,object))
             (AND (COMPLEXP ,object)
                  ,(if (member type
                         '(float short-float single-float
                                 double-float long-float))
                       `(TYPEP (REALPART OBJECT) ',type)
                     `(AND (TYPEP (REALPART ,object) ',type)
                           (TYPEP (IMAGPART ,object) ',type))))))))
  (:canonicalizer (typespec &optional (subtype '*) &aux type)
       ((cond ((member subtype '(t * non-complex-number))
                      'complex)
                     (t
                      (setq type (type-canonicalize subtype))
                      (if (equal type subtype)
                          typespec
                        `(complex ,type))))
               )))

(defun non-complex-number-in-range-p (object low high type)
  (and (cond ((eq low '*) t)
             ((numberp low) (<= low object))
             ((consp low) (< (car low) object))
             (t (error "Invalid lower limit ~S in ~A type specifier." low type)))
       (cond ((eq high '*) t)
             ((numberp high) (>= high object))
             ((consp high) (> (car high) object))
             (t (error "Invalid upper limit ~S in ~A type specifier." high type)))))

(define-type rational
  (:predicate (object &optional (low '*) (high '*))
     (and (rationalp object)
          (non-complex-number-in-range-p object low high 'rational)))
  (:optimizer (expression &optional (low '*) (high '*))
     (optimize-numeric-type-test 'rationalp expression low high))
  (:canonicalizer canonicalize-real-type-specifier)
  (:subtypes integer ratio bignum fixnum))

(define-type ratio
  (:predicate vinc:ratiop))

(define-type integer
  (:predicate (object &optional (low '*) (high '*))
      (and (integerp object)
           (non-complex-number-in-range-p object low high 'integer)))
  (:optimizer (expression &optional (low '*) (high '*))
      (let ((object (second expression)))
        (if (and (not (eq low '*))
                 (not (eq high '*)))
            (let ((hi (if (consp high) (1- (car high)) high))
                  (lo (if (consp low) (1+ (car low)) low)))
              (cond ((< hi lo)
                     (error "Invalid limits in type specifier: ~s" (second (third expression))))
                    ((= hi lo)
                     `(EQL ,object ,lo))
                    ((= (- hi lo) 1)
                     `(LET ((OBJECT ,object))
                        (OR (EQL OBJECT ,lo)
                            (EQL OBJECT ,hi))))
                    (t (optimize-numeric-type-test 'integerp expression low high))))
          (optimize-numeric-type-test 'integerp expression low high))))
  (:canonicalizer canonicalize-real-type-specifier)
  (:subtypes bignum fixnum))


(define-type bignum
  (:predicate vinc:bignump)
  (:expander ()
      '(or (integer * #.(1- most-negative-fixnum))
           (integer #.(1+ most-positive-fixnum) *))))

(define-type fixnum
  (:predicate (object &optional (low '*) (high '*))
             (and (vinc:fixnump object)
                  (non-complex-number-in-range-p object low high 'fixnum)))
  (:expander (&optional (low '*) (high '*))
            (if (and (eq low '*) (eq high '*))
                '(integer #.most-negative-fixnum #.most-positive-fixnum)
              `(integer ,(if (eq low '*) most-negative-fixnum low)
                        ,(if (eq high '*) most-positive-fixnum high))))
  (:optimizer (expression &optional (low '*) (high '*))
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
               (optimize-numeric-type-test 'vinc:fixnump expression low high))))

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
             `(LET ((OBJECT ,object))
               (AND (,predicate OBJECT)
                    (= OBJECT ,low))))
            (t
             `(LET ((OBJECT ,object))
               (AND (,predicate OBJECT)
                    ,(cond ((eq low '*)
                            T)
                           ((numberp low)
                            `(>= OBJECT ,low))
                           ((consp low)
                            `(> OBJECT ,(car low))))
                    ,(cond ((eq high '*)
                            T)
                           ((numberp high)
                            `(<= OBJECT ,high))
                           ((consp high)
                            `(< OBJECT ,(car high)))))))))))

(define-type mod
  (:predicate (object &optional (limit '*))
      (and (integerp object)
           (not (minusp object))
           (or (eq limit '*)
               (if (and (integerp limit)
                        (plusp limit))
                   (> limit object)
                 (error "Invalid upper limit ~S in ~S type specifier."
                        limit 'mod)))))
  (:expander (&optional (limit '*))
      (if (eq limit '*)
          `(INTEGER 0)
        `(INTEGER 0 ,(1- limit))))
  (:optimizer (expression &optional (limit '*))
      (optimize-numeric-type-test 'integerp expression 0 (1- limit))))

(define-type bit
  (:predicate (object)
      (or (eql object 0) (eql object 1)))
  (:expander ()
      '(integer 0 1))
  (:optimizer (expression)
      `(LET ((OBJECT ,(second expression)))
         (OR (EQL OBJECT 0)
             (EQL OBJECT 1)))))

(define-type unsigned-byte
  (:predicate (object &optional (byte-size '*))
      (and (integerp object)
           (not (minusp object))
           (or (eq byte-size *)
               (if (and (integerp byte-size)
                        (plusp byte-size))
                   (> (ash 1 byte-size) object)
                 (error "Invalid byte size ~S in ~S type specifier."
                        byte-size 'unsigned-byte)))))
  (:expander (&optional (byte-size '*))
      (if (eq byte-size '*)
          '(INTEGER 0)
        `(INTEGER 0 ,(1- (ash 1 byte-size)))))
  (:optimizer (expression &optional (byte-size '*))
      (typecase byte-size
        ((member *)
         (optimize-numeric-type-test 'integerp expression 0 '*))
        ((integer 0)
         (optimize-numeric-type-test 'integerp expression 0 (1- (ash 1 byte-size))))
        (t
         (compiler::warn 'compiler::bad-type-specification :impossible
                         "Invalid byte size ~S in ~S type specifier."
                         byte-size 'unsigned-byte)))))

(define-type signed-byte
  (:predicate (object &optional (byte-size '*))
      (and (integerp object)
           (typecase byte-size
             ((member *) t)
             ((integer 0)
              (and (< object (ash 1 (1- byte-size)))
                   ( object (- (ash 1 (1- byte-size))))))
             (t (error "Invalid byte size ~S in ~S type specifier."
                        byte-size 'signed-byte)))))
  (:expander (&optional (byte-size '*))
       (if (eq byte-size '*)
           `INTEGER
         `(INTEGER ,(- (ash 1 (1- byte-size))) ,(1- (ash 1 (1- byte-size))))))
  (:optimizer (expression &optional (byte-size '*))
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

(define-type float
  (:predicate (object &optional (low '*) (high '*))
      (and (floatp object)
           (non-complex-number-in-range-p object low high 'float)))
  (:optimizer (expression &optional (low '*) (high '*))
      (optimize-numeric-type-test 'floatp expression low high))
  (:canonicalizer canonicalize-real-type-specifier)
  (:subtypes short-float single-float double-float long-float))


(define-type short-float
  (:predicate (object &optional (low '*) (high '*))
      (and (vinc:short-float-p object)
           (non-complex-number-in-range-p object low high 'short-float)))
  (:canonicalizer canonicalize-real-type-specifier)
  (:optimizer (expression &optional (low '*) (high '*))
       (optimize-numeric-type-test 'vinc:short-float-p expression low high)))

(define-type single-float
  (:predicate (object &optional (low '*) (high '*))
      (and (vinc:single-floatp object)
           (non-complex-number-in-range-p object low high 'single-float)))
  (:canonicalizer canonicalize-real-type-specifier)
  (:optimizer (expression &optional (low '*) (high '*))
       (optimize-numeric-type-test 'vinc:single-float-p expression low high)))

(define-type double-float
  (:predicate (object &optional (low '*) (high '*))
      (and (vinc:double-float-p object)
           (non-complex-number-in-range-p object low high 'single-float)))
  (:canonicalizer canonicalize-real-type-specifier)
  (:optimizer (expression &optional (low '*) (high '*))
       (optimize-numeric-type-test 'vinc:double-float-p expression low high)))

(define-type long-float double-float)
