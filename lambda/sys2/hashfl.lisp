;;; -*- Mode:LISP; Package:SI; Readtable:CL; Base:10; Lowercase:T -*-

;;; hash table flavors.

;;; The actual hash table is an array called the hash-array.
;;; The flavor instance serves only to point to that.
;;; Hash arrays are defined in the file HASH.

(defflavor basic-hash-table (hash-array) ()
  :abstract-flavor
  (:required-methods :get-hash :put-hash :rem-hash)
  (:init-keywords :size :area :rehash-function :rehash-size
                  :number-of-values :actual-size :rehash-threshold :rehash-before-cold)
  ;(:documentation "")
  )

(defflavor pointer-independent-hash-table-mixin () ()
  :abstract-flavor
  (:required-flavors basic-hash-table)
  (:documentation "A flavor whose hash-codes are independent of the %POINTER's of its keys,
and hence does not require rehashing after a GC."))

(defflavor without-interrupts-hash-table-mixin () ()
  (:required-flavors hash-table)
  (:documentation "Hashing operations on this hash table happen uninterruptably"))

(defwrapper (without-interrupts-hash-table-mixin :get-hash) (ignore . body)
  `(without-interrupts
     . ,body))
(defwrapper (without-interrupts-hash-table-mixin :put-hash) (ignore . body)
  `(without-interrupts
     . ,body))
(defwrapper (without-interrupts-hash-table-mixin :clear-hash) (ignore . body)
  `(without-interrupts
     . ,body))
(defwrapper (without-interrupts-hash-table-mixin :modify-hash) (ignore . body)
  `(without-interrupts
     . ,body))
(defwrapper (without-interrupts-hash-table-mixin :swap-hash) (ignore . body)
  `(without-interrupts
     . ,body))



(defvar *hash-tables-rehash-before-cold* nil
  "List of hash tables to rehash when DISK-SAVE is done.")

(add-initialization 'rehash-hash-tables
                    '(dolist (h *hash-tables-rehash-before-cold*)
                       (send h :get-hash nil))
                    :before-cold)

(defmethod (basic-hash-table :before :init) (plist)
  (let ((rehash-before-cold (get plist ':rehash-before-cold)))
    (remprop plist ':rehash-before-cold)
    (unless (variable-boundp hash-array)
      (setq hash-array (apply #'make-hash-array (contents plist))))
    (setf (hash-array-hash-table-instance hash-array) self)
    (if rehash-before-cold (push self *hash-tables-rehash-before-cold*))))

(defmethod (basic-hash-table :print-self) (stream &rest ignore)
  (printing-random-object (self stream :type)
    (print-hash-array hash-array stream t nil)))

(defmethod (basic-hash-table :fasd-form) ()
  (let ((harry (zl:make-array (array-length hash-array)
                              :element-type t
                              :leader-length (array-leader-length hash-array)
                              :displaced-to hash-array)))
    (%blt-typed (%find-structure-leader hash-array)
                (%find-structure-leader harry)
                (1+ (array-leader-length harry))
                1)
    ;; Get rid of circularity.
    (setf (hash-array-hash-table-instance harry) nil)
    (make-array-into-named-structure harry 'hash-array)
    `(make-instance ',(type-of self) :hash-array ',harry)))

(defmethod (basic-hash-table :describe) ()
  (format t "~&~S is a hash-table with ~D entries out of a possible ~D (~D%).~%"
          self (hash-array-fullness hash-array) (hash-array-modulus hash-array)
          (truncate (* (hash-array-fullness hash-array) 100.)
                    (hash-array-modulus hash-array)))
  (if (hash-array-lock hash-array)
      (format t "Locked by ~S~%" (hash-array-lock hash-array)))
  (if (hash-array-funcallable-p hash-array)
      (format t "~Sing it hashes on the first argument to get a function to call.~%" 'funcall))
  (format t "There are ~D formerly used entries now deleted~%"
          (hash-array-number-of-deleted-entries hash-array))
  (format t "Rehash if table gets more than ~:[~*~D elements~;~S full (~D elements)~]~%"
          (floatp (hash-array-rehash-threshold hash-array))
          (hash-array-rehash-threshold hash-array)
          (hash-array-maximal-fullness hash-array))
  (if ( 1 (- (hash-array-block-length hash-array) 1
              (if (hash-array-hash-function hash-array) 1 0)))
      (format t "Each key has ~D values associated.~%"
              (- (hash-array-block-length hash-array) 1
                 (if (hash-array-hash-function hash-array) 1 0))))
  (when (hash-array-gc-rehash-necessary-p hash-array)
    (format t " rehash is required due to GC.~%"))
  (format t " The rehash function is ~S with increase parameter ~D.~%"
          (hash-array-rehash-function hash-array) (hash-array-rehash-size hash-array))
  (and (not (zerop (hash-array-fullness hash-array)))
       (y-or-n-p "Do you want to see the contents of the hash table? ")
       (if (not (y-or-n-p "Do you want it sorted? "))
           (send self :map-hash (lambda (key &rest values)
                                  (format t "~& ~S -> ~S~%" key values)))
         (let ((l ()))
           (send self :map-hash (lambda (key &rest values)
                                  (push (list key (copy-list values)) l)))
           (setq l (sort l #'alphalessp :key #'car))
           (format t "~&~:{ ~S -> ~S~%~}" l)))))

(defmethod (basic-hash-table :size) ()
  (hash-array-modulus hash-array))

(defmethod (basic-hash-table :filled-entries) ()
  (hash-array-fullness hash-array))

(defmethod (basic-hash-table :clear-hash) (&optional ignore)
  "Clear out a hash table; leave it with no entries."
  (with-lock ((hash-array-lock hash-array) #|:norecursive t|# :whostate "Clear hash table")
    (clear-hash-array hash-array)
    self))

(defmethod (basic-hash-table :next-element) (state)
  (quick-fix-hash-table-next-element self state))

(defun quick-fix-hash-table-next-element (table state)
  (declare (values (new-state key value)))
  (cond ((null state)
         (let ((l (maphash-return #'list table)))
           (cond ((null l)
                  (values nil nil nil))
                 ('else
                  (let ((new-state (list (cdr l))))
                    (values new-state (caar l) (cadar l)))))))
        ((null (car state))
         (values nil nil nil))
        ('else
         (let ((x (pop (car state))))
           (values state (car x) (cadr x))))))


(defmacro define-get-hash-method (flavor
                                  additional-bindings
                                  hash-code-form
                                  compare-form
                                  rehash-if-lose-form)
  `(defmethod (,flavor :get-hash) (key &optional default-value
                                   &aux (harry hash-array)
                                        ,@(append additional-bindings
                                                  (if hash-code-form `((hash-code ,hash-code-form)))))
     (declare (values value key-found-p entry-pointer)
              (function-parent ,flavor define-get-hash-method))
     (do ((p (hash-block-pointer harry ,(if hash-code-form `hash-code `key))
             (%make-pointer-offset #.dtp-locative p blen))
          (blen (hash-array-block-length harry))
          (alen (array-length harry)))
         (())
       ;; Make P wrap around at end of table.
       ;; > is used because the pointer-difference, when time to wrap,
       ;; is actually 1 or 2 more than the array length (because it includes the header);
       ;; if BLEN is 2, we could wrap too soon if  were used.
       (if (> (%pointer-difference p harry) alen)
           (setq p (%make-pointer-offset #.dtp-locative p (- alen))))
       (cond ((eq (%p-data-type p) #.dtp-null)          ;deleted or empty
              ,(if rehash-if-lose-form
                   ;; If we find a slot that has never been used, this key is not present.
                   ;; We assume that not all slots are used!
                   `(when (and (zerop (%p-pointer p))   ;empty
                               ,rehash-if-lose-form)
                      (setq hash-array (rehash-hash-array hash-array nil))
                      (return (send self :get-hash key))))
              (return (values default-value             ;Not found
                              nil
                              nil)))
             (,compare-form
              ,@(if hash-code-form
                    '((return (values (%p-contents-offset p 2)
                                      t
                                      (%make-pointer-offset #.dtp-list p 1))))
                    '((return (values (%p-contents-offset p 1)
                                      t
                                      (%make-pointer #.dtp-list p))))))))))

(defmacro define-put-hash-method (flavor
                                  additional-bindings
                                  hash-code-form
                                  compare-form
                                  rehash-if-lose-form
                                  pre-store-form)
  `(defmethod (,flavor :put-hash)
              (key &rest values)
     (declare (values value old-value key-found-p entry-pointer)
              (function-parent ,flavor define-put-hash-method))
     ;; Barf. Copy out stack list.  This is really not the right place to be doing this...
     (when (and (%pointerp key) (= (%area-number key) pdl-area))
       (setq key (copy-list key background-cons-area)))
     (let* ((harry hash-array)
            (values-left values)
            ,@(append additional-bindings
                      (if hash-code-form `((hash-code ,hash-code-form)))))
       (with-lock ((hash-array-lock harry) #|:norecursive t|# :whostate "Put hash")
         (do ((p (hash-block-pointer harry ,(if hash-code-form `hash-code `key))
                 (%make-pointer-offset #.dtp-locative p blen))
              (blen (hash-array-block-length harry))
              (alen (array-length harry))
              (old-value)
              (emptyp nil))
             (())
           ;; Make P wrap around at end of table.
           ;; > is used because the pointer-difference, when time to wrap,
           ;; is actually 1 or 2 more than the array length (because it includes the header);
           ;; if BLEN is 2, we could wrap too soon if  were used.
           (if (> (%pointer-difference p harry) alen)
               (setq p (%make-pointer-offset #.dtp-locative p (- alen))))
           (cond ((eq (%p-data-type p) #.dtp-null)      ;deleted or empty
                  (or emptyp (setq emptyp p))
                  (when (zerop (%p-pointer p))
                    (cond ,@(if rehash-if-lose-form
                                `((,rehash-if-lose-form
                                   (setq hash-array (rehash-hash-array harry nil))
                                   (return (lexpr-send self :put-hash key values)))))
                          ;; Also, if we are nearly full, rehash in a larger array.
                          ;; Don't allow the hash table to become full.
                          (( (+ (hash-array-fullness harry)
                                 (hash-array-number-of-deleted-entries harry))
                              (hash-array-maximal-fullness harry))
                           (setq hash-array (rehash-hash-array harry
                                                               (hash-array-rehash-size harry)))
                           (return (lexpr-send self :put-hash key values)))
                          (t                            ;Add to table using empty slot found
                           (without-interrupts
                             ,(if hash-code-form
                                  `(progn (%p-store-contents emptyp hash-code)
                                          (%p-store-contents-offset key emptyp 1))
                                `(%p-store-contents emptyp key))
                             (do ((i ,(if hash-code-form 2 1) (1+ i))) ((= i blen))
                               (%p-store-contents-offset (pop values-left) emptyp i))
                             (incf (hash-array-fullness harry))
                             ;; If reusing a deleted slot, decrement number of them slots.
                             (or (eq emptyp p)
                                 (decf (hash-array-number-of-deleted-entries harry)))
                             ,pre-store-form
                             (return (car values)))))))
                 (,compare-form                         ;found existing entry
                  (without-interrupts
                    ,(if hash-code-form
                         `(%p-store-contents-offset key p 1)
                         `(%p-store-contents p key))
                    (setq old-value (%p-contents-offset p ,(if hash-code-form 2 1)))
                    (do ((i ,(if hash-code-form 2 1) (1+ i))) ((= i blen))
                      (%p-store-contents-offset (pop values-left) p i)))
                  (return (values (car values)
                                  old-value
                                  t
                                  ,(if hash-code-form
                                       `(%make-pointer-offset #.dtp-list p 1)
                                       `(%make-pointer #.dtp-list p)))))))))))

(defmacro define-rem-hash-method (flavor
                                  additional-bindings
                                  hash-code-form
                                  compare-form
                                  rehash-if-lose-form)
  `(defmethod (,flavor :rem-hash)
              (key &aux (harry hash-array)
                        ,@(append additional-bindings
                                  (if hash-code-form `((hash-code ,hash-code-form)))))
     "Delete any entry for KEY in HASH-TABLE.  Return T if there was one."
     (declare (function-parent ,flavor define-rem-hash-method))
     (with-lock ((hash-array-lock harry) :whostate "Remove hash")
       ;;         can't use :norecursive, since in common lisp lets you rem-hash during map-hash
       (do ((p (hash-block-pointer harry ,(if hash-code-form `hash-code `key))
               (%make-pointer-offset #.dtp-locative p blen))
            (blen (hash-array-block-length harry))
            (alen (array-length harry)))
           (())
         ;; Make P wrap around at end of table.
         ;; > is used because the pointer-difference, when time to wrap,
         ;; is actually 1 or 2 more than the array length (because it includes the header);
         ;; if BLEN is 2, we could wrap too soon if  were used.
         (if (> (%pointer-difference p harry) alen)
             (setq p (%make-pointer-offset #.dtp-locative p (- alen))))
         (cond ((eq (%p-data-type p) #.dtp-null)        ;deleted or empty
                (cond ((not (zerop (%p-pointer p))))    ;empty
                      ,@(if rehash-if-lose-form
                            `((,rehash-if-lose-form
                               (setq hash-array (rehash-hash-array hash-array nil))
                               (return (send self :rem-hash key)))))
                      (t (return nil))))
               (,compare-form                           ;Found existing entry
                (without-interrupts
                  (do ((i 1 (1+ i))) ((= i blen))
                    (%p-store-contents-offset nil p i)) ;Wipe out old values
                  (%p-store-data-type p #.dtp-null)
                  (%p-store-pointer p 1)                ;Remove entry
                  (decf (hash-array-fullness harry))
                  (incf (hash-array-number-of-deleted-entries harry)))
                (return t)))))))                        ;really not found

(defmacro define-hash-table-methods (flavor
                                     &key additional-bindings
                                          hash-code-form
                                          compare-form
                                          rehash-if-lose-form
                                          pre-store-form)
  (declare (zwei:indentation 1 1))
  ;; barf. locally doesn't hack top-levelness right.
  `(local-declare ((function-parent ,flavor define-hash-table-methods))
     (record-source-file-name ',flavor 'define-hash-table-methods)
     (define-get-hash-method ,flavor ,additional-bindings ,hash-code-form
                             ,compare-form ,rehash-if-lose-form)
     (define-put-hash-method ,flavor ,additional-bindings ,hash-code-form
                             ,compare-form ,rehash-if-lose-form ,pre-store-form)
     (define-rem-hash-method ,flavor ,additional-bindings ,hash-code-form
                             ,compare-form ,rehash-if-lose-form)
     ',flavor))


(defflavor generic-pointer-dependent-hash-table () (basic-hash-table)
  (:documentation "Hash table with arbitrary :hash-function and :compare-function
HASH-FUNCTION should return two values: a hash code and the maximum volatility
of aby %POINTER depended on in the computation of that hash code.")
  (:init-keywords :hash-function :compare-function)
  (:required-init-keywords :hash-function :compare-function))
(define-hash-table-methods generic-pointer-dependent-hash-table
  :additional-bindings ((compare-function (hash-array-compare-function harry))
                        volatility)
  :hash-code-form (multiple-value-setq (nil volatility)
                    (funcall (hash-array-hash-function harry) key))
  :compare-form (and (eq (contents p) hash-code)
                     (funcall compare-function key (%p-contents-offset p 1)))
  :rehash-if-lose-form (hash-array-gc-rehash-necessary-p harry)
  :pre-store-form (when (> volatility
                           (hash-array-maximum-key-volatility harry))
                    (setf (hash-array-maximum-key-volatility harry) volatility)))
(defmethod (generic-pointer-dependent-hash-table :print-self) (stream &rest ignore)
  (printing-random-object (self stream :type)
    (print-hash-array hash-array stream t t)))

(defflavor generic-pointer-independent-hash-table ()
           (pointer-independent-hash-table-mixin basic-hash-table)
  (:documentation "Hash table with arbitrary :hash-function and :compare-function")
  (:init-keywords :hash-function :compare-function)
  (:required-init-keywords :hash-function :compare-function))
(define-hash-table-methods generic-pointer-independent-hash-table
  :additional-bindings ((compare-function (hash-array-compare-function harry)))
  :hash-code-form (funcall (hash-array-hash-function harry) key)
  :compare-form (and (eq (contents p) hash-code)
                     (funcall compare-function key (%p-contents-offset p 1))))
(defmethod (generic-pointer-independent-hash-table :print-self) (stream &rest ignore)
  (printing-random-object (self stream :type)
    (print-hash-array hash-array stream t t)))

;;; for compatabilty only
(defflavor hash-table () (generic-pointer-independent-hash-table)
  (:default-init-plist :compare-function #'eq :hash-function nil))

(defflavor eq-hash-table () (basic-hash-table)
  (:documentation "Hashing is done with :test eq"))
(defmethod (eq-hash-table :before :init) (plist)
  (setf (get plist :hash-function) nil
        (get plist :compare-function) #'eq))
(define-hash-table-methods eq-hash-table
  :compare-form (eq (contents p) key)
  :rehash-if-lose-form (and (%pointerp key) (hash-array-gc-rehash-necessary-p harry))
  :pre-store-form (when (%pointerp key)
                    (let ((volatility (%pointer-volatility key)))
                      (when (> volatility (hash-array-maximum-key-volatility harry))
                        (setf (hash-array-maximum-key-volatility harry) volatility)))))
;; make sure that %make-eq-hash-table below is compatible with this.

(defflavor eql-hash-table () (basic-hash-table)
  (:documentation "Hashing is done with :test eql"))
(defmethod (eql-hash-table :before :init) (plist)
  (setf (get plist :hash-function) #'eql-hash
        (get plist :compare-function) #'eql))
(define-hash-table-methods eql-hash-table
  :hash-code-form (eql-hash key)
  :compare-form (and (eq (contents p) hash-code)
                     (eql key (%p-contents-offset p 1)))
  :rehash-if-lose-form (and (%pointerp key) (hash-array-gc-rehash-necessary-p harry))
  :pre-store-form (when (%pointerp key)
                    (let ((volatility (%pointer-volatility key)))
                      (when (> volatility (hash-array-maximum-key-volatility harry))
                        (setf (hash-array-maximum-key-volatility harry) volatility)))))


(defflavor equal-hash-table () (pointer-independent-hash-table-mixin basic-hash-table)
  (:documentation "Hashing is done with :test equal"))
(defmethod (equal-hash-table :before :init) (plist)
  (setf (get plist :hash-function) #'sxhash
        (get plist :compare-function) #'equal))
(define-hash-table-methods equal-hash-table
  :hash-code-form (sxhash key)
  :compare-form (and (eq (contents p) hash-code)
                     (equal key (%p-contents-offset p 1))))

(defflavor string=-hash-table () (pointer-independent-hash-table-mixin basic-hash-table)
  (:documentation "Hashing is done with :test string="))
(defmethod (string=-hash-table :before :init) (plist)
  (setf (get plist :hash-function)
          (lambda (x) (%sxhash-string (if (stringp x) x (string x)) #o377))
        ;; Bletch! See (define-hash-table-methods string=-hash-table ...)
        (get plist :compare-function)
          'equal))
(define-hash-table-methods string=-hash-table
  :additional-bindings ((key (if (stringp key) key (string key))))
  :hash-code-form (%sxhash-string key #o377)
  ;; Bletch!  This seems the fastest way to do this!
  ;;  (better than (let ((alphabetic-case-affects-string-comparison t))
  ;;                  (%string-equal ...))
  :compare-form (and (eq (contents p) hash-code)
                     (equal key (%p-contents-offset p 1))))

(defflavor string-equal-hash-table () (pointer-independent-hash-table-mixin basic-hash-table)
  (:documentation "Hashing is done with :test string-equal"))
(defmethod (string-equal-hash-table :before :init) (plist)
  (setf (get plist :hash-function)
         (lambda (x) (%sxhash-string (if (stringp x) x (string x)) #o337))
        ;; Bletch! See (define-hash-table-methods string-equal-hash-table ...)
        (get plist :compare-function)
          #'equalp))
(define-hash-table-methods string-equal-hash-table
  :additional-bindings ((key (if (stringp key) key (string key))))
  :hash-code-form (%sxhash-string key #o337)
  ;; Bletch!  This seems the fastest way to do this!
  ;;  (better than (let ((alphabetic-case-affects-string-comparison nil))
  ;;                  (%string-equal ...))
  :compare-form (and (eq (contents p) hash-code)
                     (equalp key (%p-contents-offset p 1))))

(defflavor =-hash-table () (pointer-independent-hash-table-mixin basic-hash-table)
  (:documentation "Hashing is done with :test ="))
(defmethod (=-hash-table :before :init) (plist)
  (setf (get plist :hash-function) #'identity
        (get plist :compare-function) #'=))
(define-hash-table-methods =-hash-table
  :hash-code-form key
  :compare-form   (= key (contents p)))



;;; note that SETF cannot hope to give get-hash multiple VALUEs
(defmethod (basic-hash-table :case :set :get-hash) (key &rest values)
  (declare (arglist (key value)))
  ;; use car last is to ignore optional default eg from "(push zap (send foo :get-hash bar))"
  (lexpr-send self :put-hash key (car (last values))))

(defmethod (basic-hash-table :modify-hash) (key function &rest additional-args)
  ;; this should perhaps be open-coded per-flavor
  (let (value key-found-p values-list)
    (block nil
      (with-lock ((hash-array-lock hash-array) :whostate "Modify hash")
        (multiple-value-setq (value key-found-p values-list) (send self :get-hash key))
        (setq value (apply function key value key-found-p additional-args))
        (when key-found-p
          (setf (cadr values-list) value)
          (return value)))
      (send self :put-hash key value))))

(defmethod (basic-hash-table :swap-hash) (key &rest values)
  (declare (values old-value old-value-p location))
  (multiple-value-bind (nil old-value old-value-p location)
      (lexpr-send self :put-hash key values)
    (values old-value old-value-p location)))

(defmethod (basic-hash-table :map-hash) (function &rest extra-args &aux (harry hash-array))
  (gc:without-flipping  ;>> unfortunate, and a really good way to lose.  Seems necessary, though
    (when (hash-array-gc-rehash-necessary-p harry)
    ;; Some %POINTER's may have changed, try rehashing
      (LET ((NEW (rehash-hash-array harry nil)))
        (SET-IN-INSTANCE (hash-array-hash-table-instance HARRY)
                         'HASH-ARRAY
                         NEW)
        (SETQ HARRY NEW)))
    (do ((blen (hash-array-block-length harry))
         (block-offset (if (hash-array-hash-function harry) 1 0))
         (i 0 (+ i blen))
         (alen (array-length harry)))
        (( i alen))
      (unless (eq (%p-data-type (locf (aref harry i))) #.dtp-null)
        (%open-call-block function 0 0)
        (dolist (i (%make-pointer-offset #.dtp-list (locf (aref harry i)) block-offset))
          (%push i))
        (dolist (i extra-args)
          (%push i))
        (%activate-open-call-block))))
  nil)

(defmethod (pointer-independent-hash-table-mixin :map-hash)
           (function &rest extra-args &aux (harry hash-array))
  (do ((blen (hash-array-block-length harry))
       (block-offset (if (hash-array-hash-function harry) 1 0))
       (i 0 (+ i blen))
       (alen (array-length harry)))
      (( i alen))
    (unless (eq (%p-data-type (locf (aref harry i))) #.dtp-null)
      (%open-call-block function 0 0)
      (dolist (i (%make-pointer-offset #.dtp-list (locf (aref harry i)) block-offset))
        (%push i))
      (dolist (i extra-args)
        (%push i))
      (%activate-open-call-block)))
  nil)

(defmethod (basic-hash-table :map-hash-return) (function &optional (return-function 'list)
                                                &aux (harry hash-array) values)
  (gc:without-flipping  ;>> unfortunate, and a really good way to lose.  Seems necessary, though
    (when (hash-array-gc-rehash-necessary-p harry)
    ;; Some %POINTER's may have changed, try rehashing
      (LET ((NEW (rehash-hash-array harry nil)))
        (SET-IN-INSTANCE (hash-array-hash-table-instance HARRY)
                         'HASH-ARRAY
                         NEW)
        (SETQ HARRY NEW)))
    (do ((blen (hash-array-block-length harry))
         (block-offset (if (hash-array-hash-function harry) 1 0))
         (i 0 (+ i blen))
         (alen (array-length harry))
         (nconc (or (eq return-function 'nconc)
                    (eq return-function #'nconc))))
        (( i alen)
         (if (or nconc
                 (eq return-function 'list)
                 (eq return-function 'nconc))
             values
           (apply return-function values)))
      (unless (eq (%p-data-type (locf (aref harry i))) #.dtp-null)
        (let ((value (apply function
                            (%make-pointer-offset #.dtp-list (locf (aref harry i))
                                                  block-offset))))
          (if nconc
              (setq values (nconc value values))
            (push value values)))))))

(defmethod (pointer-independent-hash-table-mixin :map-hash-return)
           (function &optional (return-function 'list)
            &aux (harry hash-array) values)
  (do ((blen (hash-array-block-length harry))
       (block-offset (if (hash-array-hash-function harry) 1 0))
       (i 0 (+ i blen))
       (alen (array-length harry))
       (nconc (or (eq return-function 'nconc)
                  (eq return-function #'nconc))))
      (( i alen)
       (if (or nconc
               (eq return-function 'list)
               (eq return-function 'nconc))
           values
         (apply return-function values)))
    (unless (eq (%p-data-type (locf (aref harry i))) #.dtp-null)
      (let ((value (apply function
                          (%make-pointer-offset #.dtp-list (locf (aref harry i))
                                                block-offset))))
        (if nconc
            (setq values (nconc value values))
          (push value values))))))

(defmethod (pointer-independent-hash-table-mixin :next-element) (state)
  (quick-fix-hash-table-next-element self state))

;; don't laugh!! A generic-hash-table might want this. (esp for volatility value)
(defun eq-hash (key)
  (values key (if (%pointerp key) (%pointer-volatility key) most-negative-fixnum)))

(defun eql-hash (key)
  ;;Only fixnums are comparable with eq -- which hash-keys must be.  Do special work for
  ;;various kinds of extended numbers.  Code stolen from sxhash.....
  (values (cond ((fixnump key)
                 key)
                ((or (integerp key) (characterp key))
                 (if (minusp key)
                     (logxor (ldb (byte 23. 0) key) 1)
                   (ldb (byte 23. 0) key)))
                ((typep key 'single-float)
                 (logxor (%p-ldb-offset (byte 23. 0) key 1)
                         (%p-ldb-offset (byte 1 23.) key 1)
                         (%p-ldb (byte 18. 0) key)))
                ((typep key 'short-float)
                 (setq key (%pointer key))
                 (let ((y (logxor (ldb (- %%q-pointer 24.) key)
                                  (lsh key (- 24. %%q-pointer)))))
                   (logand #o37777777
                           (if (minusp key) (logxor y 1) y))))
                ((numberp key)
                 ;;rational, complex, ... punt
                 0)
                (t
                 key))
          most-negative-fixnum))

;; not used by the system
(defun equal-hash (key)
  (values (sxhash key)
          most-negative-fixnum))

(defun equalp-hash (key)
  (values
    (typecase key
      ;; ignore case, font
      (character
       (char-upcase (char-code key)))
      ;; prevent special hacking of strings, since (equalp #(#\a #\b) "ab")
      (vector
       (length key))
      (array
       (array-rank key))
      ((not number)
       (sxhash key))
      (fixnum
       (coerce key 'short-float))
      (short-float
       (%short-float-mantissa key))
      (t                                        ;%pointerp (bignum, flonum, ratio, complexnum)
       (if (complexp key)
           (setq key (%complex-real-part key)))
       (typecase key
         ((or bignum ratio)
          (cond (( #.most-negative-short-float key #.most-positive-short-float)
                 (%pointer (coerce key 'short-float)))
                (( #.most-negative-single-float key #.most-positive-single-float)
                 (%single-float-mantissa (coerce key 'single-float)))
                (t
                 259.)))
         (short-float
          key)
         (single-float
          (if (< #.most-negative-short-float key #.most-positive-single-float)
              (%pointer (coerce key 'short-float))
            (%single-float-mantissa key)))
         (t                                     ;??
          259.))))
    #.most-negative-fixnum))

;;;; make-hash-table

(defparameter *hash-table-test-alist* ()
  "Alist (test-name ht-flavor ht-compare-function ht-hash-function")

(defmacro define-hash-table-test (test flavor &optional (compare-function nil cfp) hash-function)
  (check-type test symbol)
  (check-type flavor symbol)
  `(without-interrupts
     (let ((l ,(if cfp `(list ',test ',flavor ,compare-function ,hash-function)
                       `(list ',test ',flavor)))
           (a (assq ',test *hash-table-test-alist*)))
       ;; can't use IF interpreted at this point in cold load!!!
     (cond (a (setf (cdr a) (cdr l)))
       ;; can't use push at top level at this point in cold load.
           (t (setq *hash-table-test-alist* (cons l *hash-table-test-alist*))))
     ',test)))

(define-hash-table-test eq eq-hash-table)
(define-hash-table-test eql eql-hash-table)
(define-hash-table-test equal equal-hash-table)
(define-hash-table-test equalp generic-pointer-independent-hash-table #'equalp 'equalp-hash)
(define-hash-table-test string= string=-hash-table)
(define-hash-table-test string-equal string-equal-hash-table)
(define-hash-table-test = =-hash-table)

(defun make-hash-table (&rest options
                        &key (test #'eql testp)
                             (compare-function nil cfp) (hash-function nil hfp)
                             (size 128.) (number-of-values 1) area
                             (rehash-threshold 0.7s0) (rehash-size 1.3s0) rehash-function
                             rehash-before-cold
                             actual-size
                        &aux (flavor 'generic-pointer-dependent-hash-table))
  "Create a hash table.  Keyword args are as follows:
TEST: Common Lisp way to specify the hashing and comparison functions.
 It must be EQ, EQL (the default) or EQUAL.
Instead of specifying TEST, one way specify the comparison and hasing functions
 explicitly by way of the COMPARE-FUNCTION and HASH-FUNCTION options.
 See the code for details on what these functions should do.
AREA: area to cons the table in.
SIZE: lower bound for number of entries (this may be rounded up).
 Note that the table cannot actually hold that many keys; this value merely serves
 as an approximation of the expected number of keys.
ACTUAL-SIZE: precise number of entries worth of size to use.
NUMBER-OF-VALUES: number of values to associate with each key (default 1).
 Each PUTHASH can set all the values, and GETHASH retrieves them all.
 Note that SETF of GETHASH can only set one value.
REHASH-FUNCTION: a function which accepts a hash table
 and returns a larger one.
REHASH-THRESHOLD: determines what \"fullness\" will make a growth of the hashtable
 and corresponding rehash necessary.
 Either a flonum between 0 and 1 (default 0.7), meaning that rehash occurs
 if more than that fraction full, or a fixnum, meaning rehash if more than that
 number of slots are filled. If a fixnum, it is automatically proportionally
 increased when the hashtable grows.
REHASH-SIZE: the ratio by which the default REHASH-FUNCTION
 will increase the size of the table.  By default, 1.3.
 This may also be a fixnum, in which case it determines the number of extra slots
 which are added to the hastable's size when it grows.
REHASH-BEFORE-COLD: rehash the hash-table if necessary before disk-saving"
  (check-type size (integer 1))
  (check-type number-of-values (integer 1))
  (check-type rehash-size (or (integer 1) (float (1.0))))
  (check-type rehash-threshold (or (integer 1) (float (0.0) (1.0))))
;; some sort of check is needed here
;  (assert (if (integerp rehash-threshold) (> rehash-threshold rehash-size size)
;       (ferror "rehash-threshold, ~D, is greater than rehash-size (~D)"
;              rehash-threshold rehash-size))
  (cond (testp
         (if (or cfp hfp)
             (ferror "Both ~S and ~S supplied" :test (if cfp :compare-function :hash-function)))
         (if (typep test '(or compiled-function microcode-function))
             (setq test (function-name test)))
         (setq test (or (assq test *hash-table-test-alist*)
                        (ferror "Test ~S is not valid: should be one of ~{~S~^, ~}"
                                test (mapcar #'car *hash-table-test-alist*))))
         (setq flavor (cadr test)
               compare-function (caddr test)
               hash-function (cadddr test)
               cfp (cddr test)
               test (car test)))
        (cfp
         (if (null compare-function)
             (ferror "~S is ~S" :compare-function nil))
         (if (null hfp)
             (ferror "~S supplied without ~S" :compare-function :hash-function)))
        (hfp
         (if (null hash-function)
             (ferror "~S is ~S" :hash-function nil))
         (if (null cfp)
             (ferror "~S supplied without ~S" :hash-function :compare-function)))
        ((and size actual-size)
         (ferror "Both ~S and ~S supplied" :size :actual-size))
        (t
         (setq flavor 'eql-hash-table)))
  (if cfp
      (make-instance flavor :compare-function compare-function :hash-function hash-function
                            :size size :number-of-values number-of-values :area area
                            :rehash-threshold rehash-threshold :rehash-size rehash-size
                            :rehash-function rehash-function :actual-size actual-size
                            :rehash-before-cold rehash-before-cold)
      (make-instance flavor :size size :number-of-values number-of-values :area area
                            :rehash-threshold rehash-threshold :rehash-size rehash-size
                            :rehash-function rehash-function :actual-size actual-size
                            :rehash-before-cold rehash-before-cold)))

(defsubst make-equal-hash-table (&rest cruft)
  "(make-hash-table :test #'equal . cruft)"
  (apply #'make-hash-table :test #'equal cruft))


;;;; Compatibility functions.

(defsubst hash-table-count (hash-table)
  "Returns the number of associations currently stored in HASH-TABLE."
  (send hash-table :filled-entries))

(defsubst clrhash (hash-table &optional ignore)
  "Clear out a hash table; leave it with no entries.  Returns the hash table."
  (send hash-table :clear-hash))

;; Returns third value, which common-lisp doesn't want
(defsubst zl:gethash (key hash-table &optional default-value)
  "Read the values associated with KEY in HASH-TABLE.
Returns:
 1) The primary value associated with KEY (DEFAULT-VALUE if KEY is not found),
 2) a flag which is T if KEY was found,
 3) a pointer to the list (inside the hash table)
    which holds the key and the associated values
    (NIL if KEY is not found)."
  (declare (values value key-found-flag entry-pointer))
  (send hash-table :get-hash key default-value))

(defsetf zl:gethash (object hash-table &optional (default nil defaultp)) (value)
  (let ((tem (if defaultp `(prog1 ,hash-table ,default) hash-table)))
    `(sethash ,object ,tem ,value)))

(defun cli:gethash (key hash-table &optional default-value)
  "Read the values associated with KEY in HASH-TABLE.
Returns:
 1) The primary value associated with KEY (DEFAULT-VALUE if KEY is not found),
 2) a flag which is T if KEY was found"
  (declare (values value key-found-flag))
  (multiple-value-bind (value key-found-flag)
      (send hash-table :get-hash key default-value)
    (values value key-found-flag)))

(defsetf cli:gethash (object hash-table &optional (default nil defaultp)) (value)
  (let ((tem (if defaultp `(prog1 ,hash-table ,default) hash-table)))
    `(sethash ,object ,tem ,value)))

(defsubst sethash (key hash-table value)
  (values (send hash-table :put-hash key value)))

(defsubst puthash (key value hash-table &rest additional-values)
  "Set the values associated with KEY in HASH-TABLE.
The first value is set from VALUE.  If the hash table associates more
than one value with each key, the remaining values are set from ADDITIONAL-VALUES.
Returns: 1) VALUE, 2) the previous value (or NIL),
 3) T if KEY already had an entry in the table,
 4) a pointer to the list (inside the hash table)
    which holds the key and the associated values."
  (declare (values value old-value key-found-flag entry-pointer))
  (lexpr-send hash-table :put-hash key value additional-values))

(defsubst swaphash (key value hash-table &rest additional-values)
  "Set the values associated with KEY in HASH-TABLE, returning the previous values.
The first value is set to VALUE.  If the hash table holds more than one
value per entry, the additional values are set from ADDITIONAL-VALUES.
The values returned by SWAPHASH are the same as those of GETHASH."
  (declare (values old-value old-value-p location))
  (lexpr-send hash-table :swap-hash key value additional-values))

(defsubst remhash (key hash-table)
  "Delete any entry for KEY in HASH-TABLE.  Return T if there was one."
  (send hash-table :rem-hash key))

(defsubst maphash (function hash-table &rest extra-args)
  "Apply FUNCTION to each item in HASH-TABLE; ignore values.
FUNCTION's arguments are the key followed by the values associated with it,
 followed by the EXTRA-ARGS.  Returns NIL."
  (lexpr-send hash-table :map-hash function extra-args))

(defsubst maphash-return (function hash-table &optional (return-function 'list))
  "Apply FUNCTION to each item in HASH-TABLE; apply RETURN-FUNCTION to list of results.
FUNCTION's arguments are the key followed by the values associated with it.
The values returned by FUNCTION are put in a list.
At the end, RETURN-FUNCTION is applied to that list to get the final value."
  (send hash-table :map-hash-return function return-function))


;;; done in LOOP

;(add-initialization 'add-loop-hash-elements-path '(add-hash-elements-path) :before-cold)

;(defvar hash-elements-path-added nil
;  "T if the HASH-ELEMENTS LOOP path has been added.")

;;; Cannot just add it now because this file is loaded too early.
;(defun add-hash-elements-path ()
;  (unless hash-elements-path-added
;    (setq hash-elements-path-added t)
;    (define-loop-path hash-elements hash-elements-path-function (with-key of))))

;(defun hash-elements-path-function (ignore variable ignore prep-phrases inclusive? ignore ignore)
;  (if inclusive?
;      (ferror "Inclusive stepping not supported in HASH-ELEMENTS path for ~S."
;             variable))
;  (unless (loop-tassoc 'of prep-phrases)
;    (ferror "No OF phrase in HASH-ELEMENTS path for ~S." variable))
;  (let (bindings prologue steps post-endtest pseudo-steps
;       (blen-var (gensym))
;       (ha-var (gensym))
;       (i-var (gensym))
;       (len-var (gensym))
;       (tem (gensym))
;       (key-var (or (cadr (loop-tassoc 'with-key prep-phrases)) (gensym)))
;       (offset-var (gensym)))
;    (setq bindings `((,ha-var (send ,(cadr (loop-tassoc 'of prep-phrases)) :hash-array))
;                    (,blen-var nil) (,offset-var nil) (,variable nil)
;                    (,i-var nil) (,key-var nil) (,len-var nil))
;         prologue `((setq ,blen-var
;                          (hash-array-block-length ,ha-var))
;                    (setq ,i-var (- ,blen-var))
;                    (setq ,offset-var (if (hash-array-hash-function ,ha-var) 1 0))
;                    (setq ,len-var (array-length ,ha-var)))
;         steps `(,i-var
;                 (do ((,tem (+ ,blen-var ,i-var) (+ ,blen-var ,tem)))
;                     ((or ( ,tem ,len-var)
;                          ( (%p-data-type (locf (aref ,ha-var ,tem))) dtp-null))
;                      ,tem)))
;         post-endtest `( ,i-var ,len-var)
;         pseudo-steps `(,key-var (aref ,ha-var (+ ,i-var ,offset-var))
;                        ,variable (aref ,ha-var (+ ,i-var ,offset-var 1))))
;    (list bindings prologue nil steps post-endtest pseudo-steps)))

(compile-flavor-methods basic-hash-table pointer-independent-hash-table-mixin
                        generic-pointer-dependent-hash-table
                        generic-pointer-independent-hash-table
                        hash-table
                        eq-hash-table eql-hash-table equal-hash-table
                        string=-hash-table string-equal-hash-table
                        =-hash-table
                        )

;;;; Bootstrapping of flavor hash tables.
;; must be after above compile-flavor-methods

(defun %make-eq-hash-table (harry area)
  (let* ((fl (get 'eq-hash-table 'si::flavor))
         (ht (%make-structure dtp-instance dtp-instance-header
               fl nil area
               (flavor-instance-size fl) (flavor-instance-size fl))))
    (if (si::flavor-all-instance-variables fl)
        ;; for robustness' sake
        (set-in-instance ht 'hash-array harry)
      (setf (%p-contents-offset ht 1) harry))
    ht))

;;; When this file is first loaded, various flavors have been given hash arrays
;;; that have no hash instances associated with them.
;;; Find all those hash arrays and give them instances.
(defun hash-flavor-install ()
  (dolist (fn *all-flavor-names*)
    (let ((fl (compilation-flavor fn)))
      (when (typep fl 'flavor)
        (let ((harry (flavor-method-hash-array fl)))
          (when (and (arrayp harry)
                     (null (hash-array-hash-table-instance harry)))
            (setf (hash-array-hash-table-instance harry)
                  (%make-eq-hash-table harry permanent-storage-area))))))))

(add-initialization 'hash-flavor-install '(hash-flavor-install) :once)
;;; This is what the flavor system calls to puthash into a hash array
;;;  HASH sets this to PUTHASH-BOOTSTRAP so FLAVOR can load.
(defun puthash-array (key value harry &rest additional-values)
  (lexpr-send (hash-array-hash-table-instance harry) :put-hash key value additional-values))

;;; Create a hash table suitable for a flavor's method hash table
;;;  and return its hash array.
;;;  Before this file is loaded, another definition (make-flavor-hash-array-internal)
;;;  in the file HASH is used which just makes a hash array with no instance.
(defun make-flavor-hash-array (area size)
  (let ((harry (make-flavor-hash-array-internal area size)))
    (values harry
            (setf (hash-array-hash-table-instance harry)
                  (%make-eq-hash-table harry area)))))
