;;; -*- Mode:LISP; Package:SIM; Readtable:CL; Base:10 -*-

(defconst *free-pointer* 10.)
(defvar *dont-cons* nil)

(defflavor sim-pointer
         (pointer type)
         ()
  :settable-instance-variables)

(defmethod (sim-pointer :print-self) (stream ignore ignore)
  (si:printing-random-object (self stream :no-pointer :type)
    (format stream "~a ~o" (nth type q-data-types) pointer)))

(defun >convert-to-pointer (bignum)
  (make-instance 'sim-pointer
                 :type (ldb %%q-data-type bignum)
                 :pointer (si:%make-pointer-unsigned bignum)))

(defun >make-pointer (data-type pointer)
  (make-instance 'sim-pointer
                 :type data-type
                 :pointer pointer))

(defun >new-type (type pointer)
  (make-instance 'sim-pointer
                 :type type
                 :pointer (send pointer :pointer)))

(defun >allocate-cells (n-cells &optional (alignment 0))
  (when *dont-cons*
    (ferror nil "*dont-cons* flag is on"))
  (>invalidate-cons-cache)
  (let ((ptr (send *proc* :read-main-memory *free-pointer*)))
    (do ((alignment-mask (1- (ash 1 alignment))))
        ((zerop (logand ptr alignment-mask)))
      (send *proc* :write-main-memory ptr
            (+ (dpb dtp-array-header %%q-data-type 0)
               art-32b
               (dpb 1 si:%%array-number-dimensions 0)
               (dpb 1 si:%%array-index-length-if-short 0)))
      (incf ptr))
    (send *proc* :write-main-memory *free-pointer* (+ ptr n-cells))
    (dotimes (i n-cells)
      (send *proc* :write-main-memory (+ ptr i) 0))
    (>make-pointer 0 ptr)))

(defun >pointer (ptr)
  (>make-pointer dtp-trap (send ptr :pointer)))

(defun >pointer-as-integer (ptr)
  (send ptr :pointer))

(defun >data-type (ptr)
  (send ptr :type))

(defun >whole-q (ptr)
  (dpb (>data-type ptr) %%q-data-type (>pointer-as-integer ptr)))

(defun >p-ldb-offset (byte-spec adr offset)
  (let ((q (send *proc* :read-main-memory (+ (>pointer-as-integer adr) offset))))
    (lam:ldb-big byte-spec q)))

(defun >p-ldb (byte-spec adr)
  (>p-ldb-offset byte-spec adr 0))

(defun >p-dpb-offset (value byte-spec adr offset)
  (let ((q (send *proc* :read-main-memory (+ (>pointer-as-integer adr) offset))))
    (send *proc* :write-main-memory (+ (>pointer-as-integer adr) offset) (lam:dpb-big value byte-spec q))
    value))

(defun >p-dpb (value byte-spec adr)
  (>p-dpb-offset value byte-spec adr 0))

(defsetf >p-ldb-offset (byte-spec adr offset) (val)
  `(>p-dpb-offset ,val ,byte-spec ,adr ,offset))

(defsetf >p-ldb (byte-spec adr) (val)
  `(>p-dpb ,val ,byte-spec ,adr))

(defun >p-mask-field-offset (byte-spec adr offset)
  (lam:dpb-big (>p-ldb-offset byte-spec adr offset) byte-spec 0))

(defmacro >p-mask-field (byte-spec adr)
  `(>p-mask-field-offset ,byte-spec ,adr 0))

(defun >p-deposit-field-offset (value byte-spec adr offset)
  (let ((q (send *proc* :read-main-memory (+ (>pointer-as-integer adr) offset))))
    (send *proc* :write-main-memory
          (lam:dpb-big (lam:ldb-big byte-spec value) byte-spec q))
    value))

(defmacro >p-deposit-field (value byte-spec adr)
  `(>p-deposit-field-offset ,value ,byte-spec ,adr 0))

(defsetf >p-mask-field-offset (byte-spec adr offset) (val)
  `(>p-deposit-field-offset ,val ,byte-spec ,adr ,offset))

(defmacro >p-data-type-offset (adr offset)
  `(>p-ldb-offset %%q-data-type ,adr ,offset))

(defmacro >p-data-type (adr)
  `(>p-data-type-offset ,adr 0))

(defun >p-pointer-offset (adr offset)
  (>make-pointer dtp-trap (>p-ldb-offset %%q-pointer adr offset)))

(defsetf >p-pointer-offset (adr offset) (val)
  `(>p-dpb-offset (>pointer-as-integer ,val) %%q-pointer ,adr ,offset))

(defmacro >p-pointer (adr)
  `(>p-pointer-offset ,adr 0))


;;these are for writing brand new cells -- they will never transport
(defun >p-write-cell-offset (val adr offset)
  (send *proc* :write-main-memory (+ (>pointer-as-integer adr) offset) (>whole-q val)))

(defun >p-write-cell (val adr)
  (>p-write-cell-offset val adr 0))

;;these will someday do transport-header, etc
;;therefore, they can't be used on uninitialized or unboxed storage
(defun >p-contents-offset (adr offset)
  (let ((q (send *proc* :read-main-memory (+ (>pointer-as-integer adr) offset))))
    (>make-pointer (ldb %%q-data-type q) (logand #.(1- (ash 1 25.)) q))))

(defun >p-store-contents-offset (value adr offset)
  (send *proc* :write-main-memory
        (+ (>pointer-as-integer adr) offset)
        (>whole-q value)))

(defun >p-store-contents-offset-setf (adr offset val)
  (send *proc* :write-main-memory
        (+ (>pointer-as-integer adr) offset)
        (>whole-q val)))

(defsetf >p-contents-offset >p-store-contents-offset-setf)

(defun >p-contents (adr)
  (>p-contents-offset adr 0))

(defun >p-store-contents (adr val)
  (>p-store-contents-offset val adr 0))

(defsetf >p-contents >p-store-contents)

(defun >pointer-plus (ptr &rest offsets)
  (>make-pointer dtp-trap (apply #'+ (>pointer-as-integer ptr) offsets)))


(defvar *NIL*)

(defun >cons (car cdr)
  (let ((new-cell (>allocate-cells 2 1)))
    (>p-write-cell car new-cell)
    (>p-write-cell-offset cdr new-cell 1)
    (>new-type dtp-list new-cell)))

(defun >eq (ptr1 ptr2)
  (and (= (>pointer-as-integer ptr1) (>pointer-as-integer ptr2))
       (= (>data-type ptr1) (>data-type ptr2))))

(defun >car (cons)
  (let ((dt (>data-type cons)))
    (cond ((= dt dtp-list)
           (>p-contents cons))
          ((and (= dt dtp-symbol)
                (>eq cons *NIL*))
           *NIL*)
          (t
           (ferror nil "can't do car on ~s" cons)))))

(defun >cdr (cons)
  (let ((dt (>data-type cons)))
    (cond ((= dt dtp-list)
           (>p-contents-offset cons 1))
          ((and (= dt dtp-symbol)
                (>eq cons *NIL*))
           *NIL*)
          (t
           (ferror nil "can't do cdr on ~s" cons)))))

(defun >print (ptr)
  (format t "~%")
  (>prin1 ptr))

(defun >prin1 (ptr)
  (select (>data-type ptr)
    (dtp-list (>print-list ptr))
    (dtp-symbol (>print-symbol ptr))
    (dtp-symbol-header (>print-symbol-header ptr))
    (dtp-array-pointer (>print-array ptr))
    (t
     (>print-random-object ptr)
     )))

(defun >print-random-object (ptr)
  (format t "#<~s ~s>" (nth (>data-type ptr) q-data-types) (>pointer-as-integer ptr)))

(defun >array-length (array)
  (when (not (= (>data-type array) dtp-array-pointer))
    (ferror nil "not an array"))
  (when (not (= (>p-data-type array) dtp-array-header))
    (ferror nil "doesn't point to a header"))
  (cond ((zerop (>p-ldb si:%%array-long-length-flag array))
         (>p-ldb si:%%array-index-length-if-short array))
        (t
         (>p-contents-offset array 1))))

(defun >print-array (array &optional just-the-bytes)
  (when (not (= (>p-data-type array) dtp-array-header))
    (ferror nil "first word of array is not header"))
  (cond ((or (not (= (>p-ldb si:%%array-number-dimensions array) 1))
             (not (= (>p-ldb si:%%array-long-length-flag array) 0))
             (not (= (>p-mask-field si:%%array-type-field array) art-string)))
         (>print-random-object array))
        (t
         (when (null just-the-bytes)
           (format t "#<remote \""))
         (dotimes (i (>array-length array))
           (format t "~c" (>aref array i)))
         (when (null just-the-bytes)
           (format t "\">")))))

(defun >print-symbol-header (ptr)
  (when (not (= (>data-type ptr) dtp-symbol-header))
    (ferror nil "~s is not a symbol header" ptr))
  (let ((string (>new-type dtp-array-pointer (>pointer ptr))))
    (format t "#<DTP-SYMBOL-HEADER ")
    (>print-array string t)
    (format t " ~o>" (>pointer-as-integer ptr))))



(defun >print-symbol (ptr)
  (when (not (= (>data-type ptr) dtp-symbol))
    (ferror nil "~s is not a symbol" ptr))
  (when (not (= (>p-data-type ptr) dtp-symbol-header))
    (ferror nil "this symbol pointer does not point to a symbol header"))
  (let ((string (>new-type dtp-array-pointer (>p-pointer ptr))))
    (>print-array string t)))

(defun >null (ptr)
  (>eq ptr *NIL*))

(defun >consp (ptr)
  (= (>data-type ptr) dtp-list))

(defun >atom (ptr)
  (not (>consp ptr)))

(defun >print-list (ptr)
  (labels ((>print-list-1 (ptr)
             (cond ((>null ptr))
                   ((>atom ptr)
                    (>prin1 ptr))
                   (t
                    (>print-list (>car ptr))
                    (cond ((>consp (>cdr ptr))
                           (format t " "))
                          ((not (>null (>cdr ptr)))
                           (format t " . ")))
                    (>print-list-1 (>cdr ptr))))))
    (cond ((>consp ptr)
           (format t "(")
           (>print-list-1 ptr)
           (format t ")"))
          (t
           (>prin1 ptr)))))

(defun >aref (array &rest subscripts &aux element-number)
  (when (not (= (length subscripts) 1))
    (ferror nil "only one dimensional"))
  (setq element-number (car subscripts))
  (when (not (= (>data-type array) dtp-array-pointer))
    (ferror nil "~s is not an array-pointer" array))
  (when (not (= (>p-data-type array) dtp-array-header))
    (ferror nil "~s does not point to an array header"))
  (when (not (= (>p-ldb si:%%array-long-length-flag array) 0))
    (ferror nil "can't hack long arrays"))
  (when (or (< element-number 0)
            (>= element-number (>p-ldb si:%%array-index-length-if-short array)))
    (ferror nil "out of bounds"))
  (let ((elements-per-word (select (>p-mask-field si:%%array-type-field array)
                             (art-string 4)
                             (t (ferror nil "can't hack this type of array"))))
        byte-spec)
    (setq byte-spec (byte (floor 32. elements-per-word) (* (floor 32. elements-per-word)
                                                           (remainder element-number 4))))
    (>p-ldb-offset byte-spec array (1+ (floor element-number elements-per-word)))))

(defun >aset (val array &rest subscripts &aux element-number)
  (when (not (= (length subscripts) 1))
    (ferror nil "only one dimensional"))
  (setq element-number (car subscripts))
  (when (not (= (>data-type array) dtp-array-pointer))
    (ferror nil "~s is not an array-pointer" array))
  (when (not (= (>p-data-type array) dtp-array-header))
    (ferror nil "~s does not point to an array header"))
  (when (not (= (>p-ldb si:%%array-long-length-flag array) 0))
    (ferror nil "can't hack long arrays"))
  (when (or (< element-number 0)
            (>= element-number (>p-ldb si:%%array-index-length-if-short array)))
    (ferror nil "out of bounds"))
  (let ((elements-per-word (select (>p-mask-field si:%%array-type-field array)
                             (art-string 4)
                             (t (ferror nil "can't hack this type of array"))))
        byte-spec)
    (setq byte-spec (byte (floor 32. elements-per-word) (* (floor 32. elements-per-word)
                                                           (remainder element-number 4))))
    (>p-dpb-offset val byte-spec array (1+ (floor element-number elements-per-word)))))

(defsetf >aref (array index) (val)
  `(>aset ,val ,array  ,index))

(defun >make-string (length)
  (let ((words (ceiling length 4)))
    (when (> words si:%array-max-short-index-length)
      (ferror nil "can't hack long array"))
    (let ((array (>allocate-cells (1+ words))))
      (setf (>p-ldb %%q-data-type array) dtp-array-header)
      (setf (>p-ldb si:%%array-type-field array) (ldb si:%%array-type-field art-string))
      (setf (>p-ldb si:%%array-number-dimensions array) 1)
      (setf (>p-ldb si:%%array-index-length-if-short array) length)
      (>new-type dtp-array-pointer array))))

(defun copy-array-to-remote (from to)
  (dotimes (i (array-length from))
    (>aset (aref from i) to i)))

(defun make-remote-string (string)
  (let ((s (>make-string (string-length string))))
    (copy-array-to-remote string s)
    s))


(defvar *symbol-cache* 0)

(defpackage sim-sym
  :use nil)

(defun clear-symbol-cache ()
  (incf *symbol-cache*)
  (setq *NIL* (>intern "NIL"))
  (setf (>p-contents-offset *NIL* 1) *NIL*)
  )

(defun >make-symbol (pname &optional adr)
  (let ((symbol (if adr (>make-pointer dtp-trap adr) (>allocate-cells 5))))
    ;;pname
    (setf (>p-data-type symbol) dtp-symbol-header)
    (setf (>p-pointer symbol) (>pointer (make-remote-string pname)))
    ;;value
    (setf (>p-data-type-offset symbol 1) dtp-null)
    (setf (>p-pointer-offset symbol 1) (>pointer symbol))
    ;;function
    (setf (>p-data-type-offset symbol 2) dtp-null)
    (setf (>p-pointer-offset symbol 2) (>pointer symbol))
    ;;plist
    (setf (>p-contents-offset symbol 3) *NIL*)
    ;;package
    (setf (>p-contents-offset symbol 4) *NIL*)
    (>new-type dtp-symbol symbol)))

(defun >intern (string)
  (let ((local-symbol (intern string 'sim-sym)))
    (cond ((not (eq (get local-symbol 'sim-symbol-cache) *symbol-cache*))
           (let ((remote-symbol (>make-symbol string
                                              (cond ((string-equal string "NIL") 0)
                                                    (t nil)))))
             (putprop local-symbol remote-symbol 'sim-remote-symbol)
             (putprop local-symbol *symbol-cache* 'sim-symbol-cache)
             remote-symbol))
          (t
           (get local-symbol 'sim-remote-symbol)))))

(defun (:property format:>S format:format-ctl-one-arg) (thing ignore)
  (>prin1 thing)
  )

(defun >length (list)
  (do ((len 0 (1+ len))
       (l list (>cdr l)))
      ((>atom l)
       (cond ((>null l) len)
             (t (ferror nil "list ends in ~\\>s\\" l))))))
