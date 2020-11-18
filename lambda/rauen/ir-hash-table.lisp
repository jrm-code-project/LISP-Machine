;;; -*- Mode:LISP; Package:TABLE; Base:10; Readtable:CL -*-
;;;
;;;
;;; IR-HASH-TABLE.LISP
;;;
;;;
;;; Contents:
;;;
;;;    INDEXED-RING structure.  A circular array structure.  There is a continuous mapping
;;;        from the integers onto an INDEXED-RING.  INDEXED-RINGs are manipulated by means
;;;        of INDEXED-RING-LOCATIVES.
;;;
;;;    INDEXED-RING-LOCATIVE structure.  Effectively, a pointer into an INDEXED-RING.
;;;        Provides functions to read and write the INDEXED-RING at that point, generate
;;;        a new INDEXED-RING-LOCATIVE for an adjacent point on the INDEXED-RING, and
;;;        return a unique identifier for the point.
;;;
;;;    HASH-TABLE constructors.  A hash table is represented using a DEFSTRUCT; the field
;;;        containing its elements is an INDEXED-RING.  The MAKE-HASH-TABLE function is
;;;        defined here.
;;;
;;;    HASH-TABLE ring walker.  This procedure walks around the elements of a hash table's
;;;        INDEXED-RING, calling various procedures depending on what it finds at each
;;;        place.
;;;
;;;    HASH TABLE operations.  GETHASH, PUTHASH, REMHASH, MAPHASH, CLRHASH are implemented
;;;        using the ring walker.  Other functions here include HASH-TABLE-P and
;;;        HASH-TABLE-COUNT.  (gethash's setf method?)
;;;
;;;    Speed checks.  Compares the speed of hash tables against a-lists.


(shadow
  '(:make-hash-table
    :hash-table-p
    :gethash
    :puthash
    :remhash
    :maphash
    :clrhash
    :hash-table-count))

(export
  '(make-hash-table
    hash-table-p
    gethash
    puthash
    remhash
    maphash
    clrhash
    hash-table-count))



;;;--------------------------------------------------------------------------------
;;; INDEXED-RING
;;;--------------------------------------------------------------------------------

(defstruct (indexed-ring
             (:conc-name "INDEXED-RING-")
             (:print-function print-indexed-ring)
             (:constructor make-indexed-ring-internal (size array)))
  size
  array)

(defun print-indexed-ring (ring stream depth)
  depth
  (si::printing-random-object (ring stream)
    (format stream "Indexed Ring")))

(defun make-indexed-ring (size)
  (make-indexed-ring-internal size (make-array size)))



;;;--------------------------------------------------------------------------------
;;; INDEXED-RING-LOCATIVE
;;;--------------------------------------------------------------------------------

(defstruct (indexed-ring-locative
             (:conc-name "INDEXED-RING-LOCATIVE-")
             (:print-function print-indexed-ring-locative)
             (:constructor make-indexed-ring-locative-internal (read store advance identifier)))
  read
  store
  advance
  identifier)

(defun print-indexed-ring-locative (irloc stream depth)
  depth
  (si::printing-random-object (irloc stream)
    (format stream "Indexed Ring Locative")))


(defun read-irloc (irloc)
  (funcall (indexed-ring-locative-read irloc)))

(defun store-irloc (irloc value)
  (funcall (indexed-ring-locative-store irloc) value))

(defun advance-irloc (irloc)
  (funcall (indexed-ring-locative-advance irloc)))

(defun compare-irloc (irloc1 irloc2)
  (let ((id1 (funcall (indexed-ring-locative-identifier irloc1)))
        (id2 (funcall (indexed-ring-locative-identifier irloc2))))
    (and (eq (car id1) (car id2))
         (=  (cdr id1) (cdr id2)))))

(defun make-irloc (indexed-ring offset)
  (let* ((size  (indexed-ring-size indexed-ring))
         (array (indexed-ring-array indexed-ring)))
    (labels ((make-irloc-internal (my-offset)
               (labels ((read-function ()
                          (aref array my-offset))
                        (store-function (value)
                          (setf (aref array my-offset) value))
                        (advance-function ()
                          (make-irloc-internal (mod (1+ my-offset) size)))
                        (identity-generator ()
                          (cons array my-offset)))
                 (make-indexed-ring-locative-internal
                   #'read-function
                   #'store-function
                   #'advance-function
                   #'identity-generator))))
      (make-irloc-internal (mod offset size)))))



;;;--------------------------------------------------------------------------------
;;; HASH TABLE constructors
;;;--------------------------------------------------------------------------------

(defstruct (hash-table-rep
             (:print-function print-hash-table-rep)
             (:constructor    make-hash-table-rep
                              (
                               lock
                               empty
                               pothole
                               hashing-function
                               compare-tester
                               rehash-size
                               rehash-threshold
                               member-table-p
                               number-of-elements-used
                               indexed-ring
                               )))

  lock
  empty
  pothole
  hashing-function
  compare-tester
  rehash-size
  rehash-threshold
  member-table-p
  number-of-elements-used
  indexed-ring
  )

(defun print-hash-table-rep (table stream depth)
  depth
  (si::printing-random-object (table stream )
    (format stream "Simple Member Hash Table")))


(defparameter *hash-table-default-size* 23.)

(defparameter *hash-table-default-rehash-size* 1.3)

(defparameter *hash-table-default-rehash-threshold* 0.7)


(defun make-hash-table (&key (test             'EQL)
                                 (size             *hash-table-default-size*)
                                 (rehash-size      *hash-table-default-rehash-size*)
                                 (rehash-threshold *hash-table-default-rehash-threshold*)
                                 (member-table-p   NIL))

  "Create a hash table.  These are the valid keyword arguments:
TEST:  A function indicating how to compare keys.  It must be one of
  EQ, EQL (the default), EQUAL, #'EQ, #'EQL, or #'EQUAL.
SIZE:  Lower bound for initial size of the hash table.  This may
  be rounded up.  Note that the table cannot actually hold that many
  keys; this value merely serves as an approximation of the expected
  number of keys.
REHASH-SIZE:  Indicates how much the rehash function will increase
  the size of the table.  If it is a positive integer, the size will
  increase by that many entries.  If it is a floating-point number
  greater than 1, the size will be multiplied by that amount.
REHASH-THRESHOLD:  Determines how full the hash table must be before
  rehashing.  If it is an integer, the table will be rehashed
  when its number of elements exceeds that amount.  (The REHASH-
  THRESHOLD will also be scaled accordingly.)  If it is a
  floating-point number between 0 and 1, the table will be rehashed
  when the ratio of its number of elements to its size exceeds
  that amount."

  (let ((empty   (generate-unique-object))
        (pothole (generate-unique-object))
        (ring    (make-indexed-ring size)))
    (labels ((flush-ring (irloc)
               (unless (eq (read-irloc irloc) empty)
                 (store-irloc irloc empty)
                 (flush-ring (advance-irloc irloc)))))
      (flush-ring (make-irloc ring 0)))
    (make-hash-table-rep
      nil
      empty
      pothole
      #'sxhash
      test
      rehash-size
      rehash-threshold
      member-table-p
      0
      ring)))


;;; Paranoia
;;;

(defmacro locking-hash-table (table &body body)
  `(WITH-LOCK ((LOCF (HASH-TABLE-REP-LOCK ,table)))
     ,@body))



;;;--------------------------------------------------------------------------------
;;; HASH TABLE RING WALKER
;;;--------------------------------------------------------------------------------
;;;
;;; Walks around the hash table ring, one element at a time.
;;;
;;; TABLE is the hash table being walked.
;;;
;;; START is an integer indicating where in the table's indexed-ring to begin the
;;;   walk.
;;;
;;; CONSIDER is a function which the walker calls when it hits an element in the ring.
;;; IF-POTHOLE is a function which the walker calls when it hits a pothole.
;;; IF-EMPTY is a function which the walker calls when it hits a hole.
;;;   Each step of the way, the walker will call one of these three functions.  The
;;;   walker hands the function two arguments:  the irloc pointing to the object
;;;   under consideration, and a continuation to call to resume the walk.
;;;
;;; IF-CHECKED-ALL is a function which the walker calls if it loops all the way around the
;;;   ring.  It has no arguments.
;;;--------------------------------------------------------------------------------

(defun walk-hash-table (table start consider if-pothole if-empty if-checked-all)
  (let ((ring    (hash-table-rep-indexed-ring table))
        (pothole (hash-table-rep-pothole      table))
        (empty   (hash-table-rep-empty        table)))
    (let ((beginning-irloc (make-irloc ring start)))
      (labels ((search-element (irloc)
                 (let ((resume
                         #'(lambda () (let ((next-irloc (advance-irloc irloc)))
                                        (if (compare-irloc next-irloc beginning-irloc)
                                            (funcall if-checked-all)
                                            (search-element next-irloc)))))
                       (element (read-irloc irloc)))
                   (cond ((eq element pothole) (funcall if-pothole irloc resume))
                         ((eq element empty)   (funcall if-empty   irloc resume))
                         (t                    (funcall consider   irloc resume))))))
        (search-element beginning-irloc)))))



;;;--------------------------------------------------------------------------------
;;; HASH TABLE OPERATIONS
;;;--------------------------------------------------------------------------------

(defun gethash (key table &optional default)
  "Look up KEY in TABLE.  Returns two values:
1) The value associated with KEY, if it was found; or DEFAULT if it wasn't."
2) T if KEY was found; NIL if it wasn't."
  (walk-hash-table
    table
    (funcall (hash-table-rep-hashing-function table) key)
    #'(lambda (irloc continue)
        (if (funcall (hash-table-rep-compare-tester table)
                     (car (read-irloc irloc))
                     key)
            (values (cdr (read-irloc irloc)) T)
            (funcall continue)))
    #'(lambda (irloc continue) irloc (funcall continue))
    #'(lambda (irloc continue) irloc continue (values default NIL))
    #'(lambda () (ferror nil "Looped all the way."))))

(defun puthash (key value table)
  (walk-hash-table
    table
    (funcall (hash-table-rep-hashing-function table) key)
    #'(lambda (irloc continue)
        (if (funcall (hash-table-rep-compare-tester table)
                     (car (read-irloc irloc))
                     key)
            (store-irloc irloc (cons key value))
            (funcall continue)))
    #'(lambda (irloc continue)
        continue
        (store-irloc irloc (cons key value))
        (incf (hash-table-rep-number-of-elements-used table))
        value)
    #'(lambda (irloc continue)
        continue
        (store-irloc irloc (cons key value))
        (incf (hash-table-rep-number-of-elements-used table))
        value)
    #'(lambda () (ferror nil "Table full.")))
  (rehash-if-necessary table))

(defun remhash (key table)
  (walk-hash-table
    table
    (funcall (hash-table-rep-hashing-function table) key)
    #'(lambda (irloc continue)
        (if (funcall (hash-table-rep-compare-tester table)
                     (car (read-irloc irloc))
                     key)
            (progn (store-irloc irloc (hash-table-rep-pothole table))
                   (decf (hash-table-rep-number-of-elements-used table))
                   T)
            (funcall continue)))
    #'(lambda (irloc continue) irloc (funcall continue))
    #'(lambda (irloc continue) irloc continue NIL)
    #'(lambda () (ferror nil "Looped all the way."))))

(defun maphash (function table)
  (walk-hash-table
    table
    0
    #'(lambda (irloc continue)
        (let ((key-value (read-irloc irloc)))
          (funcall function (car key-value) (cdr key-value))
          (funcall continue)))
    #'(lambda (irloc continue) irloc (funcall continue))
    #'(lambda (irloc continue) irloc (funcall continue))
    #'(lambda () NIL)))

(defun clrhash (table)
  (let ((empty (hash-table-rep-empty table)))
    (walk-hash-table
      table
      0
      #'(lambda (irloc continue) (store-irloc irloc empty) (funcall continue))
      #'(lambda (irloc continue) (store-irloc irloc empty) (funcall continue))
      #'(lambda (irloc continue) (store-irloc irloc empty) (funcall continue))
      #'(lambda () table)))
  (setf (hash-table-rep-number-of-elements-used table) 0))

(defun hash-table-count (table)
  (hash-table-rep-number-of-elements-used table))



;;;--------------------------------------------------------------------------------
;;; REHASHING
;;;--------------------------------------------------------------------------------

(defun rehash-if-necessary (table)
  (let ((rehash-size      (hash-table-rep-rehash-size             table))
        (rehash-threshold (hash-table-rep-rehash-threshold        table))
        (elements         (hash-table-rep-number-of-elements-used table))
        (capacity         (indexed-ring-size (hash-table-rep-indexed-ring table))))
    (if (typep rehash-threshold 'INTEGER)
        (when (>= elements rehash-threshold)
          (rehash-table table (+ capacity rehash-size) (+ elements rehash-size)))
        (when (>= elements (* rehash-threshold capacity))
          (rehash-table table (ceiling (* capacity rehash-size)) rehash-threshold)))))


(defun rehash-table (table new-capacity new-threshold)
  (let ((new-ring  (make-indexed-ring new-capacity))
        (new-empty (generate-unique-object))
        (entries   NIL))
    (maphash #'(lambda (key value)
                     (push (cons key value) entries))
                 table)
    (labels ((flush-ring (irloc)
               (unless (eq (read-irloc irloc) new-empty)
                 (store-irloc irloc new-empty)
                 (flush-ring (advance-irloc irloc)))))
      (flush-ring (make-irloc new-ring 0)))
    (setf (hash-table-rep-indexed-ring            table) new-ring)
    (setf (hash-table-rep-empty                   table) new-empty)
    (setf (hash-table-rep-rehash-threshold        table) new-threshold)
    (setf (hash-table-rep-number-of-elements-used table) 0)
    (dolist (entry entries)
      (puthash (car entry) (cdr entry) table))))


;(defun store-in-member-hash-table (element table)
;  (walk-hash-table
;    table
;    (funcall (hash-table-rep-hashing-function table) element)
;    #'(lambda (irloc continue)
;       (unless (eq (read-irloc irloc) element)
;         (funcall continue)))
;    #'(lambda (irloc continue) continue (store-irloc irloc element))
;    #'(lambda (irloc continue) continue (store-irloc irloc element))
;    #'(lambda () (ferror nil "Table full."))))

(defun print-member-hash-table (table &optional (stream *trace-output*))
  (format stream "~&Elements >> ")
  (walk-hash-table
    table
    0
    #'(lambda (irloc continue)
        (format stream "~S " (read-irloc irloc))
        (funcall continue))
    #'(lambda (irloc continue)
        irloc
        (format stream "POTHOLE ")
        (funcall continue))
    #'(lambda (irloc continue)
        irloc
        (format stream "EMPTY ")
        (funcall continue))
    #'(lambda () NIL))
  (format stream "<<"))


(defun generate-unique-object ()
  "Generate an object that isn't EQ to anything else."
  (ncons nil))



;;;--------------------------------------------------------------------------------
;;; Speed checks
;;;--------------------------------------------------------------------------------

(defvar *speed-check-hash-table*)
(defvar *speed-check-a-list*)
(defvar *speed-check-old-hash-table*)

(defun lookup-in-hash-table () (dotimes (j 1000.) (gethash 'frob *speed-check-hash-table*)))
(defun lookup-in-old-hash-table () (dotimes (j 1000.) (global:gethash 'frob *speed-check-hash-table*)))
(defun lookup-in-a-list () (dotimes (j 1000.) (assoc 'frob *speed-check-a-list*)))

(defun setup-hash-table ()
  (setq *speed-check-hash-table* (make-hash-table))
  (puthash 'frob 77 *speed-check-hash-table*))

(defun setup-old-hash-table ()
  (setq *speed-check-old-hash-table* (global:make-hash-table))
  (global:puthash 'frob 77 *speed-check-hash-table*))

(defun setup-a-list (half-size)
  (setq *speed-check-a-list*
        (append (make-list half-size :initial-element '(bar . 33))
                (list '(frob . 77))
                (make-list half-size :initial-element '(bar . 33)))))
