; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:NIL; Readtable:CL; Base:10 -*-

;;; A hash table that the user sees is a flavor instance.
;;; The guts of it is a named-structure array defined below.

;;; The contents of the array is divided into n-word blocks.
;;; Each block corresponds to one hash key.  The first word of the block is the key.
;;; The remaining words are associated data.  Normally there is only one of them.
;;; Each key and its associated data words form a cdr-next list.
;;; The third value of GETHASH points to this list.
;;; Extra args to PUTHASH can be used to set data words past the first.

;;; A DTP-NULL is used in the table in the key position to mark an empty slot.
;;; DTP-NULL with nonzero pointer means a slot that was "deleted" and
;;; which search must continue past.

;;; A flavor's method hash table is actually the array, not the flavor instance.
;;; The array points to the flavor instance it belongs to, so that the
;;; flavor code can find the instance to send messages to it.
;;; This also enables flavors to be bootstrapped.

;;; This file defines only the low-level hash arrays.
;;; The file HASHFL, loaded after flavors are loaded,
;;; defines the hash table flavors and the user-level functions that work on them.

;;;; This page deals with defining and creating the hash arrays.

;;; NOTE: the microcode knows the index of HASH-ARRAY-MODULUS
;;;  as well as the organization of the entries and how to hash them,
;;;  in the special case that the modulus is a power of 2.
;;;  See label CALL-INSTANCE-ARRAY in the microcode.
;; accessors here are called close-compiled by sys2;flavor
(zl:defstruct (hash-array :named :array-leader (:conc-name hash-array-)
                          (:constructor make-hash-array-internal) (:alterant nil)
                          (:callable-constructors nil))
  (rehash-function 'hash-array-default-rehash :documentation
    "A function when rehash is required.  First argument is the hash-array,
second is NIL to just rehash or the rehash-size to grow it first.
The function must return the hash-array (which may have been moved by ADJUST-ARRAY-SIZE)")
  (rehash-size 1.3s0 :documentation
    "How much to grow by when the time comes.  A flonum is the ratio to increase by,
a fixnum is the number of entries to add.
These will get rounded up to the next appropriate size.")
  (gc-generation-number gc:%gc-generation-number :documentation
    "Value of %gc-generation-number when we last rehashed")
  (modulus nil :documentation
    "The number of blocks. Used for remainder to get hash code.")
  (fullness 0 :documentation
    "The number of valid entries currently in the array.")
  (block-length 2 :documentation
    nil)
  (rehash-threshold 0.7s0 :documentation
    "Rehash if we get more than this fraction full.")
  (number-of-deleted-entries 0 :documentation
    "Number of \"deleted\" entries (entries no longer valid but which you must keep
searching past). Used together with the FULLNESS to make sure there is always one
slot that has never been used.")
  (lock nil :documentation
    "Used to make :get-hash slow!")
  (hash-function nil :documentation
    "This function computes a numeric key from an object. NIL means use the object.")
  (compare-function 'eq :documentation
    "This function compares an object key with a key in the table.")
  (hash-table-instance nil :documentation
    "This is the instance whose HASH-ARRAY this hash array is.")
  (maximum-key-volatility most-negative-fixnum :documentation
    "Maximum volatility of any pointer used in the computation of a hash code"))

(defsubst hash-array-funcallable-p (harry)
  (and (array-has-leader-p harry)
       (not (zerop (%p-ldb-offset %%array-leader-funcall-as-hash-table harry -1)))))

; gc:*gc-flip-generations* is initialized in ltop, so we need not fear referencing it in the cold-load
(defsubst hash-array-gc-rehash-necessary-p (harry)
  ;; Some %POINTER's may have changed, try rehashing
  (let ((tem (hash-array-maximum-key-volatility harry)))
    (and ( tem 0)
         (< (hash-array-gc-generation-number harry) (elt gc::*gc-flip-generations* tem)))))

(defsubst rehash-hash-array (hash-array grow)
  (funcall (hash-array-rehash-function hash-array) hash-array grow))



(defun make-hash-array (&key (size 128.) area
                        ((:rehash-function rhf) 'hash-array-default-rehash)
                        ((:rehash-size rhs) 1.3s0)
                        (number-of-values 1)
                        actual-size
                        (rehash-threshold 0.7s0)
                        hash-function
                        (compare-function 'eq)
                        &aux harry blen)
  (setq size (or actual-size (hash-array-good-size size)))
  (if (integerp rehash-threshold)
      (setq rehash-threshold (cl:/ (float rehash-threshold) size)))
  (or rhf (setq rhf 'hash-array-default-rehash))
  (setq blen (+ 1 number-of-values (if hash-function 1 0)))
  (setq harry (make-hash-array-internal
                :make-array (:length (* size blen) :area area :type art-q-list)
                :modulus size
                :block-length blen
                :rehash-threshold rehash-threshold
                :rehash-function rhf
                :rehash-size rhs
                :hash-function hash-function
                :compare-function compare-function))
  (clear-hash-array harry)
  harry)

;;; Convert SIZE (a number of array elements) to a more-or-less prime.
(defun hash-array-good-size (size)
  (cond ((lessp size 20.)
         19.)
        (t
         (unless (oddp size) (incf size))       ;Find next higher more-or-less prime
         (do () ((and (not (zerop (cl:rem size 3)))
                      (not (zerop (cl:rem size 5)))
                      (not (zerop (cl:rem size 7)))
                      (not (zerop (cl:rem size 11.)))
                      (not (zerop (cl:rem size 13.)))
                      (not (zerop (cl:rem size 17.))))) ;Magic number
           (incf size 2))
         size)))

(defun make-flavor-hash-array-internal (area size &aux harry)
  ;; Funcallable hash arrays are looked at by the microcode
  ;; and require that the modulus be a power of 2.
  (setq size (lsh 1 (integer-length (1- size))))
  (setq harry (make-hash-array-internal
                :make-array (:length (* size 3) :area area :type art-q-list)
                :modulus size
                :block-length 3
                :rehash-threshold *flavor-hash-array-rehash-threshold*
                :rehash-function 'hash-array-double-size-rehash
                :hash-function nil
                :compare-function 'eq))
  (%p-dpb-offset 1 %%array-leader-funcall-as-hash-table harry -1)
  (clear-hash-array harry)
  (values harry nil))

;;; This is a separate function from the :CLEAR-HASH operation
;;; for the sake of bootstrapping flavors.
(defun clear-hash-array (harry)
  ;; caller is expected to have locked us
  (without-interrupts
    (let ((elt-0 (locf (aref harry 0)))
          (blen (hash-array-block-length harry)))
      (when (> blen 2)
        ;; Set all of hash array to NIL with cdr-next,
        ;;  in the cases where the code below isn't enough
        (setf (contents elt-0) nil)
        (%p-store-cdr-code (locf (aref harry 0)) cdr-next)
        (%blt-typed (locf (aref harry 0))
                    (locf (aref harry 1))
                    (1- (array-length harry))
                    1))
      ;; Set first word of each group to DTP-NULL, 0.
      (%p-store-pointer elt-0 0)
      (%p-store-data-type elt-0 dtp-null)
      (%blt-typed elt-0
                  (%make-pointer-offset dtp-locative elt-0 blen)
                  (1- (truncate (array-length harry) blen))
                  blen)
      ;; Set last word of each group to CDR-NIL.
      (setf (aref harry (+ blen -1)) nil)
      (%p-store-cdr-code (locf (aref harry (+ blen -1))) cdr-nil)
      (%blt-typed (locf (aref harry (+ blen -1)))
                  (locf (aref harry (+ blen blen -1)))
                  (1- (truncate (array-length harry) blen))
                  blen))
    (setf (hash-array-fullness harry) 0
          (hash-array-number-of-deleted-entries harry) 0
          (hash-array-gc-generation-number harry) gc:%gc-generation-number
          (hash-array-maximum-key-volatility harry) most-negative-fixnum)
    harry))

(defsubst hash-array-maximal-fullness (harry)
  (values (floor (* (if (floatp (hash-array-rehash-threshold harry))
                        (hash-array-rehash-threshold harry)
                      ;; never a fixnum, as make-hash-array coerces fixnums to floats,
                      ;;  so non-float presumably means the value is NIL
                      0.7s0)
                    (- (hash-array-modulus harry) 2)))))

(defselect ((:property hash-array named-structure-invoke))
  (:fasload-fixup (self)
    ;; Force rehash as if due to gc, because hash codes are all wrong now.
    ;; Also fix up cdr codes.
    (setf (hash-array-gc-generation-number self) most-negative-fixnum
          (hash-array-maximum-key-volatility self) most-negative-fixnum)
    (do ((i 0 (+ i blen))
         (blen (hash-array-block-length self))
         (length (array-length self)))
        (( i length))
      (%p-store-cdr-code (locf (aref self (+ i blen -1))) cdr-nil)))
  (:describe (self)
    (describe-defstruct self)
    (format t "~&   Maximum fullness before rehash: ~S" (hash-array-maximal-fullness self)))
  (:print-self (self stream &optional ignore ignore)
    (printing-random-object (self stream :type)
      (print-hash-array self stream nil t))))

(defun print-hash-array (harry stream as-hash-table-p testp)
  (let ((funcallable (and (not as-hash-table-p) (hash-array-funcallable-p harry))))
    (format stream "~S~:[+~S~;~*~]/~S~@[ :test ~S~]~@[ ~D values~]~@[ (Funcallable)~]"
            (hash-array-fullness harry)
            (zerop (hash-array-number-of-deleted-entries harry))
            (hash-array-number-of-deleted-entries harry)
            (hash-array-maximal-fullness harry)
            (and testp (function-name (hash-array-compare-function harry)))
            (and (not funcallable)
                 ( 1 (- (hash-array-block-length harry) 1
                         (if (hash-array-hash-function harry) 1 0)))
                 (- (hash-array-block-length harry) 1
                    (if (hash-array-hash-function harry) 1 0)))
            funcallable)))

;;;; Rehashing of hash arrays.

;;; Add a new entry to a hash array being constructed for rehashing an old one.
;;; CONTENTS is a pointer to the first word of the entry in the old hash array,
;;; so its CAR is the hash code.
;;; There is no need to lock the hash array since nobody else knows about it yet.
(defun rehash-put (harry contents &optional for-gc)
  (multiple-value-bind (hash-code volatility)
      ;; Use the same hash code as before, to avoid swapping,
      ;; unless this is rehashing due to GC.
      (if (and for-gc (hash-array-hash-function harry))
          (funcall (hash-array-hash-function harry)
                   (%p-contents-offset contents 1))
        (values (contents contents)
                (if (%pointerp (contents contents))
                    (%region-volatility (%region-number (contents contents)))
                  most-negative-fixnum)))
    (or volatility (setq volatility 3))
    (do ((p (hash-block-pointer harry hash-code)
            (%make-pointer-offset dtp-locative p blen))
         (blen (hash-array-block-length harry))
         (alen (array-length harry)))
        (())
      ;; Make P wrap around at end of table.
      ;; > is used because the pointer-difference, when time to wrap,
      ;; is actually 1 or 2 more than the array length (because it includes the header);
      ;; if BLEN is 2, we could wrap too soon if >= were used.
      (if (> (%pointer-difference p harry) alen)
          (setq p (%make-pointer-offset dtp-locative p (- alen))))
      ;; Install this key in the first empty slot.
      ;; We know the key cannot already be present, and there are no deleted slots,
      ;; and we don't need to rehash because that's what we are doing.
      (when (eq (%p-data-type p) dtp-null)
        (%blt-typed contents p blen 1)
        (setf (contents p) hash-code)
        (when (> volatility
                 (hash-array-maximum-key-volatility harry))
          (setf (hash-array-maximum-key-volatility harry) volatility))
        (return nil)))))

;;; Standard rehash function.  Returns new hash array (possibly the same one).
;;; GROW is either the hash array's rehash-size or NIL meaning use same size array.
;;; ACTUAL-SIZE is so that this can be used as a subroutine of another
;;; rehash function which differs only in how to compute the new size.
(defun hash-array-default-rehash (harry grow &optional actual-size)
  (setq harry (follow-structure-forwarding harry))
  (with-lock ((hash-array-lock harry))
  ;gc:without-flipping used to be wrapped here.  It is not necessary, and has extraordinary
  ; potential for causing deadly embraces and problems in the scheduler (any flavor operation
  ; can get here..).
  ; If a flip occurs, we will just have to rehash again, which is what was going to
  ; happen anyway.
      (let* ((new-size (if (null grow)
                           (hash-array-modulus harry)
                         (hash-array-good-size
                           (if (floatp grow)
                               (floor (* (hash-array-modulus harry) grow))
                             (+ (hash-array-modulus harry) grow)))))
             (new-hash-array (make-hash-array
                                :size new-size
                                :area (if grow
                                          (sys:%area-number harry)
                                        ;; this is to be copied and return-array'ed, so fret not!
                                        background-cons-area)
                                :rehash-function (hash-array-rehash-function harry)
                                :rehash-size (hash-array-rehash-size harry)
                                :hash-function (hash-array-hash-function harry)
                                :compare-function (hash-array-compare-function harry)
                                :actual-size (if grow actual-size (hash-array-modulus harry))
                                :number-of-values (- (hash-array-block-length harry)
                                                     1
                                                     (if (hash-array-hash-function harry) 1 0))
                                :rehash-threshold (hash-array-rehash-threshold harry)
                                )))
        (if (hash-array-funcallable-p harry)
            (%p-dpb-offset 1 %%array-leader-funcall-as-hash-table new-hash-array -1))
        ;; Scan the old hash array and find all nonempty entries.
        (do ((p (%make-pointer-offset dtp-locative harry
                                      (1+ (%p-ldb %%array-long-length-flag harry)))
                (%make-pointer-offset dtp-locative p blen))
             (blen (hash-array-block-length harry))
             (i 0 (+ i blen))
             (alen (array-length harry)))
            (( i alen))
          (unless (eq (%p-data-type p) dtp-null)
            ;; And store each one in the new hash array.
            (rehash-put new-hash-array p (null grow))))
        (setf (hash-array-fullness new-hash-array) (hash-array-fullness harry)
              (hash-array-hash-table-instance new-hash-array)
              (hash-array-hash-table-instance harry))
        (cond ((null grow)
               (setq harry (follow-structure-forwarding harry))
               (setf (hash-array-lock new-hash-array) (hash-array-lock harry))
               (let ((base (%find-structure-leader harry)))
                 (declare (unspecial base))     ;gak
                 (%blt-typed (%find-structure-leader new-hash-array)
                             base
                             (%structure-total-size base)
                             1))
               harry)
              (t
               new-hash-array)))))

;;; Rehash a hash array to be exactly double the size.
;;; Use this as a rehash function for a hash array.
;;; It ignores the :REHASH-SIZE parameter.
(defun hash-array-double-size-rehash (harry grow)
  (hash-array-default-rehash harry grow (lsh (hash-array-modulus harry) 1)))

;;; The flavor system needs to be able to do PUTHASH before hash table flavors can be used.

;;; Subroutine of hashing.
;;; Given a hash-array and a key, return a locative to the start
;;; of the block in the array which may contain an association from that key.
;;; Cannot use ALOC because it gets an error if there is a DTP-NULL in the array.
(defun hash-block-pointer (harry key)
  (%make-pointer-offset dtp-locative harry
                        (+ (* (cl:rem (ldb (byte #o23 0)
                                           (rot (%pointer key)
                                                (if (hash-array-funcallable-p harry) 0 3)))
                                      (hash-array-modulus harry))
                              (hash-array-block-length harry))
                           (%p-ldb %%array-long-length-flag harry)
                           1)))

(defun puthash-bootstrap (key value harry &rest additional-values
                          &aux
                          (values-left (cons value additional-values)))
  (declare (values value old-value key-found-flag entry-pointer))
  ;(with-lock ((hash-array-lock harry)) >>) no need, as there is nobody else against whom
  ;                                     >>  to lock when this guy is called.
  (setq harry (follow-structure-forwarding harry))
  ;; Only allow eq-hash-tables in cold load
  (if (hash-array-hash-function harry) (ferror "not eq ht"))
  (do ((p (hash-block-pointer harry key)
          (%make-pointer-offset dtp-locative p blen))
       (blen (hash-array-block-length harry))
       (old-value)
       (emptyp nil))
      (())
      ;; Make P wrap around at end of table.
      ;; > is used because the pointer-difference, when time to wrap,
      ;; is actually 1 or 2 more than the array length (because it includes the header);
      ;; if BLEN is 2, we could wrap too soon if  were used.
      (if (> (%pointer-difference p harry)
             (array-length harry))
          (setq p (%make-pointer-offset dtp-locative p (- (array-length harry)))))
      (cond ((eq (%p-data-type p) dtp-null)
             (or emptyp (setq emptyp p))
             (when (zerop (%p-pointer p))
               ;; Hash arrays are not supposed to need rehash before HASHFL is loaded.
               ;; It wouldn't work, since the hash table instance is not there
               ;; for FLAVOR to use to find the new hash array.
               (cond ((hash-array-gc-rehash-necessary-p harry)
                      (ferror "gc" harry))
                     (( (+ (hash-array-fullness harry)
                            (hash-array-number-of-deleted-entries harry))
                         (hash-array-maximal-fullness harry))
                      (ferror "too full." harry))
                     (t                         ;Add to table using empty slot found
                      (%p-store-contents emptyp key)
                      (do ((i 1 (1+ i))) ((= i blen))
                        (%p-store-contents-offset (pop values-left) emptyp i))
                      (incf (hash-array-fullness harry))
                      ;; If reusing a deleted slot, decrement number of them slots.
                      (or (eq emptyp p)
                          (decf (hash-array-number-of-deleted-entries harry)))
                      (if (%pointerp key)
                          (let ((volatility (%pointer-volatility key)))
                            (if (> volatility (hash-array-maximum-key-volatility harry))
                                (setf (hash-array-maximum-key-volatility harry) volatility))))
                      (return value)))))
            ((eq (contents p) key)              ;Found existing entry
             (setq old-value (%p-contents-offset p 1))
             (do ((i 1 (1+ i))) ((= i blen))
               (%p-store-contents-offset (pop values-left) p i))
             (return (values value
                             old-value
                             t
                             (%make-pointer dtp-list p)))))))

(unless (fboundp 'puthash-array)
  (fset 'puthash-array 'puthash-bootstrap))

(unless (fboundp 'make-flavor-hash-array)
  (fset 'make-flavor-hash-array 'make-flavor-hash-array-internal))

;;; Like MAPHASH but wants a hash-array rather than a hash table instance.
;;; So it can be used before HASHFL has been fully loaded and installed
;;; (such as, for composing the hash table flavors).
;;; Also, we always lock, unlike vanilla :map-hash which must not do so.
(defun maphash-array (function harry &rest extra-args)
  (let ((instance (hash-array-hash-table-instance harry)))
    (if instance
        (send instance :map-hash function extra-args)
      (with-lock ((hash-array-lock harry))
        (%assure-pdl-room (+ (hash-array-block-length harry) (length extra-args) 7))
        (do ((blen (hash-array-block-length harry))
             (block-offset (if (hash-array-hash-function harry) 1 0))
             (i 0 (+ i blen))
             (alen (array-length harry)))
            (( i alen))
          (unless (eq (%p-data-type (locf (aref harry i))) dtp-null)
            (%open-call-block function 0 0)
            (dolist (i (%make-pointer-offset dtp-list (locf (aref harry i)) block-offset))
              (%push i))
            (dolist (i extra-args)
              (%push i))
            (%activate-open-call-block))))))
  nil)
