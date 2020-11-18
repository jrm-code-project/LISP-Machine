;;; -*- Mode:LISP; Package:SI; Readtable:CL; Base:10; Lowercase:T -*-

;>> In general these are abysmally slow.
;>>  Many many many many special-cases need to be written.
;>>  This is all very depressing.


;;; Primitives for fetching elements sequentially from either lists or arrays.
;;; You use an index variable which contains an array index if the
;;; sequence is an array, or a tail if the sequence is a list.

(defprop check-sequence-args t :error-reporter)
;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:29
(defun check-sequence-args (sequence start end
                            &optional (q 'sequence) (s 'start) (e 'end))
  (let ((tem (typecase sequence
               (list
                (or (list-length sequence)
                    end
                    (ferror "The sequence ~S is circular" sequence)))
               (vector
                (length sequence))
               (t
                (ferror "The ~S argument, ~S, is not a sequence (a list or a vector)"
                        q sequence)))))
    (or end (setq end tem))
    (check-type start (integer 0))
    (check-type end (integer 0))
    (unless ( 0 start tem)
      (ferror "The ~S argument, ~S, is out of range. (0 - ~S)" s start tem))
    (unless ( 0 end tem)
      (ferror "The ~S argument, ~S, is out of range. (0 - ~S)" e end tem))
    (unless ( start end)
      (ferror "The ~S argument, ~S, is greater than ~S (~S)" s start e end))
    (values tem
            ;; seq-start
            (if (cl:listp sequence) (setq tem (nthcdr start sequence)) start)
            ;; seq-end
            (if (cl:listp sequence) (nthcdr (- end start) tem) end))))


(defmacro seq-inc (indexvar)
  `(if (fixnump ,indexvar)
       (incf ,indexvar)
       (setq ,indexvar (cdr ,indexvar))))

(defmacro seq-fetch (sequence indexvar)
  `(if (fixnump ,indexvar)
       (cl:aref ,sequence ,indexvar)
       (car ,indexvar)))

(defmacro seq-fetch-inc (sequence indexvar)
  `(if (fixnump ,indexvar)
       (cl:aref ,sequence (prog1 ,indexvar (incf ,indexvar)))
       (pop ,indexvar)))

;;>>  (Wouldn't it be nice to have a machine in which (identity x) were fast???)
(defmacro key-fetch (key sequence indexvar)
  `(let ((tem (if (fixnump ,indexvar)
                  (cl:aref ,sequence ,indexvar)
                  (car ,indexvar))))
     (if ,key (funcall ,key tem) tem)))

(defmacro key-fetch-inc (key sequence indexvar)
  `(let ((tem (if (fixnump ,indexvar)
                  (cl:aref ,sequence (prog1 ,indexvar (incf ,indexvar)))
                  (pop ,indexvar))))
     (if ,key (funcall ,key tem) tem)))

(defmacro seq-store (sequence indexvar value)
  `(if (fixnump ,indexvar)
       (setf (cl:aref ,sequence ,indexvar) ,value)
       (setf (car ,indexvar) ,value)))

;;; This returns an index variable value that is ready to fetch the
;;; first element of the sequence.

(defmacro seq-start (sequence &optional numeric-index)
  (if numeric-index
      `(if (arrayp ,sequence)
           ,numeric-index
           (nthcdr ,numeric-index ,sequence))
      `(if (arrayp ,sequence) 0 ,sequence)))

;;; This returns a value for use in an end-test.
;;; Compare the index var against this value with EQ to see if you are at the end.
(defmacro seq-end (sequence &optional numeric-index)
  `(if (arrayp ,sequence)
       (or ,numeric-index (length ,sequence))
       (and ,numeric-index (nthcdr ,numeric-index ,sequence))))


;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:30
(defun make-sequence (type size &key (initial-element nil initp))
  "Returns a sequence of SIZE elements, of type TYPE.
Each element is set to INITIAL-ELEMENT.
TYPE must be equivalent to either LIST or some sort of ARRAY.
If the value is a list, it is completely cdr-coded."
  (check-type size (fixnum 0))
  (cond ((eq type 'list)
         (make-list size :initial-element initial-element))
        ((or (memq type '(array simple-array vector simple-vector))
             (and (memq (car-safe type) '(array simple-array vector simple-vector))
                  (equal (cadr type) '(t))))
         (if initp
             (make-array size :initial-element initial-element)
             (make-array size)))
        ((memq type '(string simple-string))
         (if initp
             (make-string size :initial-element initial-element)
             (make-string size)))
        (t
         (let ((xtype (type-canonicalize type nil nil)))
           (cond ((eq xtype 'list)
                  (make-list size :initial-element initial-element))
                 ((or (memq xtype '(array simple-array))
                      (and (memq (car-safe xtype) '(array simple-array))
                           (equal (cdr xtype) '(t))))
                  (if initp
                      (make-array size :initial-element initial-element)
                      (make-array size)))
                 ((and (memq (car-safe xtype) '(array simple-array))
                       (null (cddr xtype)))
                  (if initp
                      (make-array size :element-type (cadr xtype)
                                       :initial-element initial-element)
                      (make-array size :element-type (cadr xtype))))
                 ;;***********************************************************
                 ;; code added to let (make-sequence '(vector atom) 128) work. bpm
                 ;;***********************************************************
                 ((and (listp xtype)
                            (memq (car-safe xtype) '(array simple-array)))
                       (if initp
                           (make-array size :element-type (cadr xtype)
                                            :initial-element initial-element)
                           (make-array size :element-type (cadr xtype))))
                 ;;***********************************************************
                 (t
                  (ferror "Invalid sequence type ~S." type)))))))

(defvar *temp-vector* nil)
(defmacro get-temp-vector (size)
  `(or (do (old)
           ((%store-conditional (locf *temp-vector*)
                                (setq old *temp-vector*)
                                nil)
            (if ( (length old) ,size)
                old)))
       (make-array ,size :fill-pointer 0)))

(defun merge (result-type sequence1 sequence2 predicate &key key)
  "Return a single sequence containing the elements of SEQUENCE1 and SEQUENCE2 interleaved.
The interleaving is done by taking the next element of SEQUENCE1 unless
the next element of SEQUENCE2 is \"less\" than it according to PREDICATE.
KEY, if non-NIL, is applied to each element to get the object to
pass to PREDICATE, rather than the element itself.
RESULT-TYPE specifies the type of sequence returned."
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (let ((index-or-tail-1 (seq-start sequence1))
        (index-or-tail-2 (seq-start sequence2))
        (end1 (seq-end sequence1))
        (end2 (seq-end sequence2)))
    (do ((result (make-sequence result-type (+ (length sequence1) (length sequence2))))
         (store-index 0 (1+ store-index))
         val)
        (())
      (setq val (cond ((eq index-or-tail-1 end1)
                       (if (eq index-or-tail-2 end2)
                           (return result)
                         (seq-fetch-inc sequence2 index-or-tail-2)))
                      ((eq index-or-tail-2 end2)
                       (seq-fetch-inc sequence1 index-or-tail-1))
                      (t (let ((e1 (seq-fetch sequence1 index-or-tail-1))
                               (e2 (seq-fetch sequence2 index-or-tail-2)))
                           (cond ((apply-predicate-and-key predicate key e2 e1)
                                  (seq-inc index-or-tail-2)
                                  e2)
                                 (t
                                  (seq-inc index-or-tail-1)
                                  e1))))))
      (if (arrayp result)
          (setf (cl:aref result store-index) val)
        ;; since a list constructed with make-list will be contiguous
        (%p-store-contents-offset val result store-index)))))

(defun subseq (sequence start &optional end)
  "Return a subsequence of SEQUENCE; a new sequence containing some of SEQUENCE's elements.
If SEQUENCE is a list, the value is also a list;
if it is an array, the value is an array of the same type.
START and END are the indices in SEQUENCE of the desired subsequence.
If END is NIL, it means the end of SEQUENCE."
  (ctypecase sequence
    (vector
     (let ((len (length sequence))
           vallen)
       (or (and (or (null end) ( end start))
                ( start 0))
           (ferror "Args ~S and ~S out of range for ~S" start end sequence))
       (if end
           (setq vallen (- end start)
                 start (min len start)
                 end (min len end))
         (setq start (min len start)
               end len
               vallen (- end start)))
       (let ((res (zl:make-array vallen :type (%p-ldb-offset %%array-type-field sequence 0))))
         (copy-array-portion sequence start end res 0 vallen)
         res)))
    (list
     (if end
         (firstn (- end start) (nthcdr start sequence))
         (nthcdr start sequence)))))

#|
;;; this does the wrong thing in the case when the new subsequence is a different length
;;; from the old one.  What is the right way to treat this case?  See below.
(defun subseq-setf (sequence start end new-subsequence &optional (sub-start 0) sub-end)
  (if (and (vectorp sequence) (vectorp new-subsequence))
      (copy-array-portion new-subsequence sub-start (or sub-end (length new-subsequence))
                          sequence start (or end (length sequence)))
    (let ((store-index (if (vectorp sequence) start (nthcdr start sequence)))
          (store-end (if end
                         (if (arrayp sequence) end (nthcdr sequence end))
                         (seq-end sequence)))
          (fetch-end (seq-end new-subsequence sub-end))
          (fetch-index (seq-start new-subsequence sub-start)))
      (do ()
          ((or (eq store-index store-end)
               (eq fetch-index fetch-end)))
        (seq-store sequence store-index
                   (seq-fetch-inc new-subsequence fetch-index))
        (seq-inc store-index))))
  sequence)

(defsetf subseq (sequence start &optional end) (new)
  `(si:subseq-setf ,sequence ,start ,end ,new))
|#

#|
;;; Naha prefers:
(defsetf subseq (sequence start &optional (end nil end-is-here)) (new)
  `(let ((new-seq (concatenate (if (listp ,sequence) 'list 'vector)
                               (subseq ,sequence 0 ,start)
                               ,new
                               ,@(if end-is-here
                                     `((subseq ,sequence ,end))
                                   nil))))
     (structure-forward ,sequence new-seq)      ;fake "Destructively modifying" the sequence.  Pace suggested this.
     ,new))
;;; but this has the "bug" that is is not destructive.  Common LISP specifies that (setf (subseq . . .) . . .)
;;; should destructively modify the sequence.  The above will not modify it but rather, create a new one.
;;; Adding the STRUCTURE-FORWARD makes it look like the origional sequence was modified.
|#


;;; What this does when the new subsequence doesn't fit exactly in the START/END range:
;;; If the new subsequence is too short, simply copy all of in and
;;;  leave the extra stuff in the old sequence alone.
;;; If the new subsequence is too long, simply copy what was specified by
;;;  the START and END arguments, ignoring the rest of the new subsequence.
;;; >>I think the optional arguments are for possible optimisation from calls to
;;; >>REPLACE and maybe (SETF (SUBSEQ s0 x0 y0) (SUBSEQ s1 x1 y1)), otherwise there isn't
;;; >>really any reason to have them.  (RpK)
(defun subseq-setf (sequence start end new-subsequence &optional (sub-start 0) sub-end)
  (if (and (vectorp sequence) (vectorp new-subsequence))
      (let ((store-end-idx (or end (length sequence)))
            (new-subseq-length (or sub-end (length new-subsequence))))
        (copy-array-portion new-subsequence sub-start new-subseq-length
                            sequence start (min store-end-idx (+ start new-subseq-length))))
    (let ((store-index (if (vectorp sequence) start (nthcdr start sequence)))
          (store-end (if end
                         (if (arrayp sequence) end (nthcdr end sequence))
                         (seq-end sequence)))
          (fetch-end (seq-end new-subsequence sub-end))
          (fetch-index (seq-start new-subsequence sub-start)))
      (do ()
          ((or (eq store-index store-end)
               (eq fetch-index fetch-end)))
        (seq-store sequence store-index
                   (seq-fetch-inc new-subsequence fetch-index))
        (seq-inc store-index))))
  sequence)

(defsetf subseq (sequence start &optional end) (new)
  `(si:subseq-setf ,sequence ,start ,end ,new))



(defun replace (into-sequence-1 from-sequence-2 &key (start1 0) end1 (start2 0) end2)
  "Copy all or part of FROM-SEQUENCE-2 into INTO-SEQUENCE-1.
A sequence is either a list or a vector.
START1 and END1 specify the part of FROM-SEQUENCE-2 to be copied.
 They default to 0 and NIL (which means the end of the sequence).
START2 and END2 specify the part of INTO-SEQUENCE-1 to be copied into.
If the subsequence to be copied into is longer than the one to be copied,
 the extra elements of the to-subsequence are left unchanged.
If the two sequences are the same, the data is first copied to a
 intermediate location and then copied back in.
The value is INTO-SEQUENCE-1."
  (check-type into-sequence-1 sequence)
  (check-type from-sequence-2 sequence)
  (replace-1 into-sequence-1 from-sequence-2 start1 end1 start2 end2))

(defun replace-1 (into-sequence-1 from-sequence-2 &optional (start1 0) end1 (start2 0) end2)
  (or end1 (setq end1 (length into-sequence-1)))
  (or end2 (setq end2 (length from-sequence-2)))
  (if (eq into-sequence-1 from-sequence-2)
      (let* ((n-copy (min (- end2 start2) (- end1 start1)))
             (temp (get-temp-vector n-copy)))
        (replace-1 temp from-sequence-2 0 n-copy start2 end2)
        (replace-1 into-sequence-1 temp start1 end1 0 n-copy)
        (setf (fill-pointer temp) 0)
        (setq *temp-vector* temp))
    (if (and (vectorp into-sequence-1) (vectorp from-sequence-2))
        (let ((n-copy (min (- end2 start2) (- end1 start1))))
          (copy-array-portion from-sequence-2 start2 (+ start2 n-copy)
                              into-sequence-1 start1 (+ start1 n-copy)))
      (let ((store-index (if (arrayp into-sequence-1) start1 (nthcdr start1 into-sequence-1)))
            (store-end (if (arrayp into-sequence-1) end1 (nthcdr end1 into-sequence-1)))
            (fetch-end (if (arrayp from-sequence-2) end2 (nthcdr end2 from-sequence-2)))
            (fetch-index (if (arrayp from-sequence-2)
                             start2 (nthcdr start2  from-sequence-2))))
        (do ()
            ((or (eq store-index store-end)
                 (eq fetch-index fetch-end)))
          (seq-store into-sequence-1 store-index
                     (seq-fetch-inc from-sequence-2 fetch-index))
          (seq-inc store-index)))))
  into-sequence-1)

(defun reduce (function sequence &key from-end (start 0) end
               (initial-value nil initp) &aux tem)
  "Combine the elements of SEQUENCE using FUNCTION, a function of two args.
FUNCTION is applied to the first two elements; then to that result and the third element;
 then to that result and the fourth element; and so on.
START and END restrict the action to a part of SEQUENCE,
 as if the rest of SEQUENCE were not there.  They default to 0 and NIL
 (NIL for END means to the end of SEQUENCE).
If FROM-END is non-NIL, FUNCTION is applied to the last two elements;
 then to the previous element and that result; then to the previous
 element and that result; and so on.
If INITIAL-VALUE is specified, it acts like an extra element of SEQUENCE
 at the end (if FROM-END is non-NIL) or the beginning, in addition to
 the actual elements of the specified part of SEQUENCE.  Then there is
 effectively one more element to be processed.  The INITIAL-VALUE is
 used in the first call to FUNCTION.
If there is only one element to be processed,
 that element is returned and FUNCTION is not called.
If there are no elements (SEQUENCE is of length zero and no INITIAL-VALUE),
 FUNCTION is called with no arguments and its value is returned."
  (declare (downward-funarg function))
  (multiple-value-bind (length seq-start seq-end)
      (check-sequence-args sequence start end)
    (cond ((eql length 0)
           (if initp initial-value (funcall function)))
          ((eql length 1)
           (setq tem (elt sequence 0))
           (if initp
               (if from-end
                   (funcall function tem initial-value)
                   (funcall function initial-value tem))
             tem))
          (from-end
           (cond ((vectorp sequence)
                  (let ((accum (cond (initial-value
                                      (funcall function (cl:aref sequence (decf seq-end))
                                               initial-value))
                                     (t
                                      (decf seq-end 2)
                                      (funcall function (cl:aref sequence seq-end)
                                               (cl:aref sequence (1+ seq-end)))))))
                  (do ((index (1- seq-end) (1- index)))
                      ((< index start)
                       accum)
                      (setq accum (funcall function (cl:aref sequence index) accum)))))
                 (t
                  (labels
                    ((reduce-list-backwards (function list length initial initp)
                           (if (null (cdr list))
                               (if initp (funcall function (car list) initial) (car list))
                             (funcall function
                                      (car list)
                                      (reduce-list-backwards function
                                                             (cdr list)
                                                             (1- length)
                                                             initial initp)))))
                    (reduce-list-backwards function seq-start
                                           length initial-value initp)))))
          (t
           (let ((accum (if initial-value
                            (funcall function initial-value (seq-fetch-inc sequence seq-start))
                          (funcall function (seq-fetch-inc sequence seq-start)
                                            (seq-fetch-inc sequence seq-start)))))
             (do ((index seq-start))
                 ((eq index seq-end)
                  accum)
               (setq accum (funcall function accum (seq-fetch-inc sequence index)))))))))



(defun sequence-length (s)
  (etypecase s
    (list (or (list-length s) (ferror "~S is circular" s)))
    (vector (length s))))

(defun concatenate (result-type &rest sequences)
  "Return a sequence of type RESULT-TYPE concatenating the contents of the SEQUENCES.
Each sequence argument may be a list or an array.
RESULT-TYPE must be a valid sequence type such as LIST or VECTOR."
  (let ((rlen 0))
    (dolist (s sequences)
      (incf rlen (sequence-length s)))
    (let ((result (make-sequence result-type rlen)))
      (let ((store-index (seq-start result)))
        (dolist (s sequences)
          (if (and (arrayp result) (arrayp s))
              (let ((len (length s)))
                (copy-array-portion s 0 len
                                    result store-index (+ store-index len))
                (incf store-index len))
            (do ((fetch-index (seq-start s))
                 (fetch-end (seq-end s)))
                ((eq fetch-index fetch-end))
              (seq-store result store-index
                         (seq-fetch-inc s fetch-index))
              (seq-inc store-index)))))
      result)))

(defun fill (sequence item &key (start 0) end)
  "Set all the elements of SEQUENCE (or some subsequence of it) to ITEM.
START and END specify the subsequence; they default to 0 and NIL
\(NIL for END means to the end of SEQUENCE)."
  (multiple-value-bind (ignore start end)
      (check-sequence-args sequence start end)
    (if (arrayp sequence)
        (array-initialize sequence item start end)
      (do ((tail start (cdr tail)))
          ((eq tail end))
        (setf (car tail) item))))
  sequence)

(defun fill-array-from-sequences (array sequence dimension array-index)
  (if (= 0 (array-rank array))
      (setf (cl:aref array) sequence)
    (do ((index (seq-start sequence))
         (i 0 (1+ i))
         (last-dim-flag (= (1+ dimension) (array-rank array)))
         (stop-i (array-dimension array dimension)))
        ((= i stop-i))
      (if last-dim-flag
          ;; Cut off one level of recursion - eliminates most of the function calls.
          (setf (ar-1-force array (+ (* array-index stop-i) i))
                (seq-fetch-inc sequence index))
        (fill-array-from-sequences array (seq-fetch-inc sequence index) (1+ dimension)
                                   (+ (* array-index stop-i) i))))))

(defun cl:map (result-type fcn &rest sequences)
  "Maps over successive elements of each SEQUENCE, returns a sequence of the results.
FCN is called first on the 0'th elements of all the sequences,
then on the 1st elements of all, and so on until some argument sequence is exhausted.
The values returned by FCN are put into a result sequence which is returned by MAP.
RESULT-TYPE is a sequence type; the result is of that type.
Or RESULT-TYPE can be NIL, meaning call FCN for effect only,
throw away the values, and return NIL."
  (declare (downward-funarg fcn))
  (block map
    (let (result-length)
      ;; Figure out the obvious things about the number of iterations
      ;; regardless of whether we are making a result sequence.
      ;; It makes the end tests faster anyway.
      (dolist (s sequences)
        (etypecase s
          (vector
           (setq result-length
                 (if result-length (min result-length (length s)) (length s))))
          (null
           (setq result-length 0)
           (return))
          (cons)))                              ;done below
      ;; If some arg is length 0, return fast.
      (and (eql 0 result-length)
           (return-from map
             (and result-type
                  (make-sequence result-type 0))))
      (if result-type
          (prog ((index 0) val s-tail result store-index)
                (cond (result-length)
                      ;; If making a list and all args are lists,
                      ;; MAPCAR is suitable, and faster than figuring out
                      ;; the lengths of everything in advance.
                      ((eq result-type 'list)
                       (return-from map (apply #'mapcar fcn sequences)))
                      ;; Otherwise, must find the length of the shortest arg
                      ;; without wasting too much time on any circular lists.
                      (t
                       (dolist (s sequences)
                         (if result-length
                             (if (null (nthcdr result-length s))
                                 (setq result-length (length s)))
                           (setq result-length (list-length s))))
                       (if (null result-length)
                           (ferror "All sequences supplied to ~S were circular" 'cl:map))))
                (setq result (make-sequence result-type result-length))
                (setq store-index (seq-start result))

                ;; Now we are ready to do the actual mapping.
                (%assure-pdl-room (+ (length sequences) 4))     ;Make sure %PUSH's don't lose
             nextcall  ;; Here to compute the next element of the result.
                (when (= index result-length)
                  (return result))
                (%open-call-block fcn 0 1)      ;Destination is stack
                (setq s-tail sequences)
             nextarg  ;; Here to push the next arg for FCN.
                (when s-tail
                  (%push (if (arrayp (car s-tail))
                             (cl:aref (car s-tail) index)
                           (pop (car s-tail))))
                  (pop s-tail)
                  (go nextarg))
                ;; Now all the args are pushed.
                (%activate-open-call-block)
                (setq val (%pop))
                (seq-store result store-index val)
                (seq-inc store-index)
                (incf index)
                (go nextcall))
        ;; result-type nil: do the mapping, but return nothing
        (prog ((index 0) s-tail)
              (%assure-pdl-room (+ (length sequences) 4))       ;Make sure %PUSH's don't lose
           nextcall
              (when (eql index result-length)
                (return nil))
              (%open-call-block fcn 0 0)        ;Destination is ignore
              (setq s-tail sequences)
           nextarg
              (when s-tail
                (%push (cond ((arrayp (car s-tail))
                              (cl:aref (car s-tail) index))
                             (t
                              (unless (car s-tail) (return nil))
                              (pop (car s-tail)))))
                (pop s-tail)
                (go nextarg))
              ;; Now all the args are pushed.
              (%activate-open-call-block)
              (incf index)
              (go nextcall))))))

(defun cl:some (predicate &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCES; if it returns non-NIL, so does SOME.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns non-NIL, the value it returns is returned by SOME.
If one of the sequences is exhausted, SOME returns NIL."
  (declare (downwarg-funarg predicate))
  (block some
    (prog ((index 0) s-tail value)
          (%assure-pdl-room (+ (length sequences) 4))   ;Make sure %PUSH's don't lose
       nextcall
          (%open-call-block predicate 0 1)              ;Destination is stack
          (setq s-tail sequences)
       nextarg
          (%push (etypecase (car s-tail)
                   (vector
                    (if (= index (length (car s-tail))) (return nil)
                      (cl:aref (car s-tail) index)))
                   (cons
                    (pop (car s-tail)))
                   (null
                    (return nil))))
          (pop s-tail)
          (when s-tail (go nextarg))
          ;; Now all the args are pushed.
          (%activate-open-call-block)
          (cond ((setq value (%pop))
                 (return value))
                (t
                 (incf index)
                 (go nextcall))))))

(defun notevery (predicate &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCES; T if PREDICATE ever returns NIL.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns NIL, then NOTEVERY returns T.
If one of the sequences is exhausted, NOTEVERY returns NIL."
  (declare (downward-funarg predicate))
  (prog ((index 0) s-tail)
        (%assure-pdl-room (+ (length sequences) 4))     ;Make sure %PUSH's don't lose
     nextcall
        (%open-call-block predicate 0 1)                ;Destination is stack
        (setq s-tail sequences)
     nextarg
        (%push (etypecase (car s-tail)
                 (vector
                  (if (= index (length (car s-tail))) (return nil)
                    (cl:aref (car s-tail) index)))
                 (cons
                  (pop (car s-tail)))
                 (null
                  (return nil))))
        (pop s-tail)
        (when s-tail (go nextarg))
        ;; Now all the args are pushed.
        (%activate-open-call-block)
        (cond ((null (%pop))
               (return t))
              (t
               (incf index)
               (go nextcall)))))

(defun notany (predicate &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCES; T if PREDICATE always returns NIL.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns non-NIL, then NOTANY returns NIL.
If one of the sequences is exhausted, NOTANY returns T."
  (declare (downward-funarg predicate))
  (prog ((index 0) s-tail)
        (%assure-pdl-room (+ (length sequences) 4))     ;Make sure %PUSH's don't lose
     nextcall
        (%open-call-block predicate 0 1)                ;Destination is stack
        (setq s-tail sequences)
     nextarg
        (%push (etypecase (car s-tail)
                 (vector
                  (if (= index (length (car s-tail))) (return t)
                    (cl:aref (car s-tail) index)))
                 (cons
                  (pop (car s-tail)))
                 (null
                  (return t))))
        (pop s-tail)
        (when s-tail (go nextarg))
        ;; Now all the args are pushed.
        (%activate-open-call-block)
        (when (%pop) (return nil))
        (incf index)
        (go nextcall)))

(defun cl:every (predicate &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCES; T if PREDICATE always returns T.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns NIL, then EVERY returns NIL.
If one of the sequences is exhausted, EVERY returns T."
  (block every
    (prog ((index 0) s-tail)
          (%assure-pdl-room (+ (length sequences) 4))   ;Make sure %PUSH's don't lose
       nextcall
          (%open-call-block predicate 0 1)              ;Destination is stack
          (setq s-tail sequences)
       nextarg
          (%push (etypecase (car s-tail)
                   (vector
                    (if (= index (length (car s-tail))) (return t)
                      (cl:aref (car s-tail) index)))
                   (cons
                    (pop (car s-tail)))
                   (null
                    (return t))))
          (pop s-tail)
          (when s-tail (go nextarg))
          ;; Now all the args are pushed.
          (%activate-open-call-block)
          (unless (%pop) (return nil))
          (incf index)
          (go nextcall))))

(defun remove-if (predicate sequence &key (start 0) end count key from-end)
  "Return SEQUENCE, partially copied so that elements satisfying PREDICATE are omitted.
START and END specify a subsequence to consider; elements outside
 that subsequence will not be removed even if they would satisfy PREDICATE.
 They default to 0 and NIL (which means the end of the sequence).
KEY, if non-NIL, is a function to be applied to each element
 to get the object to pass to the PREDICATE.  If KEY is NIL,
 the element itself is passed to the PREDICATE.
COUNT can be used to specify how many elements to remove (at maximum);
 after that many have been found, the rest are left alone.
FROM-END, if non-NIL, means that the last COUNT matching elements
 should be removed, rather than the first COUNT many."
  (funcall (etypecase sequence
             (list #'remove-from-list)
             (vector #'remove-from-array))
           nil sequence start end count nil nil key from-end predicate))

(defun remove-if-not (predicate sequence &key (start 0) end count key from-end)
  "Like REMOVE-IF but removes elements which do not satisfy PREDICATE."
  (funcall (etypecase sequence
             (list #'remove-from-list)
             (vector #'remove-from-array))
           nil sequence start end count nil t key from-end predicate))

(defun cl:remove (item sequence &key test test-not (start 0) end count key from-end)
  "Return SEQUENCE, partially copied so that elements matching ITEM are omitted.
START and END specify a subsequence to consider; elements outside
 that subsequence will not be removed even if they would match.
 They default to 0 and NIL (which means the end of the sequence).
KEY, if non-NIL, is a function to be applied to each element
 to get the object to match against ITEM.  If KEY is NIL,
 the element itself is matched.
TEST is a function of two args to use to compare ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
COUNT can be used to specify how many elements to remove (at maximum);
 after that many have been found, the rest are left alone.
FROM-END, if non-NIL, means that the last COUNT matching elements
 should be removed, rather than the first COUNT many."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (funcall (etypecase sequence
             (list #'remove-from-list)
             (vector #'remove-from-array))
           item sequence start end count (or test-not test) (not (null test-not))
           key from-end nil))

(defun delete-if (predicate sequence &key (start 0) end count key from-end)
  "Return SEQUENCE, modified so that elements satisfying PREDICATE are omitted.
The value may be SEQUENCE itself destructively modified or it may be a copy.
START and END specify a subsequence to consider; elements outside
 that subsequence will not be removed even if they would satisfy PREDICATE.
 They default to 0 and NIL (which means the end of the sequence).
KEY, if non-NIL, is a function to be applied to each element
 to get the object to pass to the PREDICATE.  If KEY is NIL,
 the element itself is passed to the PREDICATE.
COUNT can be used to specify how many elements to remove (at maximum);
 after that many have been found, the rest are left alone.
FROM-END, if non-NIL, means that the last COUNT matching elements
 should be removed, rather than the first COUNT many."
  (funcall (etypecase sequence
             (list #'delete-from-list)
             (vector #'delete-from-array))
           nil sequence start end count nil nil key from-end predicate))

(defun delete-if-not (predicate sequence &key (start 0) end count key from-end)
  "Like DELETE-IF but deletes elements which do not satisfy PREDICATE."
  (funcall (etypecase sequence
             (list #'delete-from-list)
             (vector #'delete-from-array))
           nil sequence start end count nil t key from-end predicate))

(defun cl:delete (item sequence &key test test-not (start 0) end count key from-end)
  "Return SEQUENCE, partially copied so that elements matching ITEM are omitted.
START and END specify a subsequence to consider; elements outside
 that subsequence will not be removed even if they would match.
 They default to 0 and NIL (which means the end of the sequence).
KEY, if non-NIL, is a function to be applied to each element
 to get the object to match against ITEM.  If KEY is NIL,
 the element itself is matched.
TEST is a function of two args to use to compare ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
COUNT can be used to specify how many elements to remove (at maximum);
 after that many have been found, the rest are left alone.
FROM-END, if non-NIL, means that the last COUNT matching elements
 should be removed, rather than the first COUNT many."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (funcall (etypecase sequence
             (list #'delete-from-list)
             (vector #'delete-from-array))
           item sequence start end count (or test-not test) (not (null test-not))
           key from-end nil))

(defun remove-from-array (item vector start end count test invertp key from-end one-arg-predicate)
  (or end (setq end (length vector)))
  (or count (setq count (length vector)))
  (let ((temp (get-temp-vector (min count (- end start))))
        elt)
    (setf (fill-pointer temp) 0)
    ;; Find the indices of all the things we want to remove.
    (if from-end
        (do ((i (1- end) (1- i)))
            ((or (< i start)
                 ( (fill-pointer temp) count))
             (nreverse temp))
          (setq elt (if key (funcall key (cl:aref vector i)) (cl:aref vector i)))
          (if (eq invertp (not (cond (one-arg-predicate
                                      (funcall one-arg-predicate elt))
                                     (test
                                      (funcall test item elt))
                                     (t
                                      (eql item elt)))))
            (vector-push i temp)))
      (do ((i start (1+ i)))
          ((or ( i end) ( (fill-pointer temp) count)))
        (setq elt (if key (funcall key (cl:aref vector i)) (cl:aref vector i)))
        (if (eq invertp (not (cond (one-arg-predicate
                                    (funcall one-arg-predicate elt))
                                   (test
                                    (funcall test item elt))
                                   (t
                                    (eql item elt)))))
            (vector-push i temp))))
    ;; Now TEMP contains the indices of the elements to be removed, in ascending order.
    (cond ((zerop (fill-pointer temp))
           (setq *temp-vector* temp)
           vector)
          (t
           (let ((result (if (array-has-fill-pointer-p vector)
                             (zl:make-array (array-dimension vector 0) :type (array-type vector)
                                         :fill-pointer (- (fill-pointer vector) (fill-pointer temp)))
                           (zl:make-array (- (length vector) (fill-pointer temp))
                                          :type (array-type vector)))))
             (copy-array-portion vector 0 (cl:aref temp 0) result 0 (cl:aref temp 0))
             (do ((i 1 (1+ i))
                  (stop (fill-pointer temp)))
                 (( i stop)
                  (let ((x (1+ (cl:aref temp (1- i)))))
                    (copy-array-portion vector  x (length vector)
                                        result (- x i) (length result))))
               (let ((x (1+ (cl:aref temp (1- i))))
                     (y (cl:aref temp i)))
                 (copy-array-portion vector x y result (- x i) (- y i))))
             (setf (fill-pointer temp) 0)
             (setq *temp-vector* temp)
             result)))))
(deff delete-from-array 'remove-from-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:35
(defun remove-from-list (item list
                         start end count test invertp key from-end one-arg-predicate
                         &aux (skip-count 0))
  (or count (setq count most-positive-fixnum))
  (or end (setq end (length list)))
  (when from-end
    (unless (and end ( count (- end start)))
      (setq skip-count
            (max 0 (- (count-1 item list start end test invertp key one-arg-predicate) count)))))
  (if (and (plusp count)
           (or (null end) (> end start)))
      (let ((head (variable-location list))
            elt)
        (loop for start-tail = (nthcdr start list) then (cdr tail)
              with end-tail = (and end (nthcdr (- end start) start-tail))
              as tail = (do ((l start-tail (cdr l)))
                            ((eq l end-tail) l)
                          (setq elt (if key (funcall key (car l)) (car l)))
                          (if (eq invertp (not (cond (one-arg-predicate
                                                      (funcall one-arg-predicate elt))
                                                     (test
                                                      (funcall test item elt))
                                                     (t
                                                      (eql item elt)))))
                              (return l)))
              until (eq tail end-tail)
          when (minusp (decf skip-count))
            do (loop until (eq (cdr head) tail)
                     do (setf (cdr head) (setq head (cons (cadr head) (cddr head)))))
               (setf (cdr head) (cdr tail))
               (when (zerop (decf count))
                 (return)))))
  list)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:36
(defun delete-from-list (item list
                         start end count test invertp key from-end one-arg-predicate
                         &aux (skip-count 0))
  (or count (setq count most-positive-fixnum))
  (or end (setq end (length list)))
  (when from-end
    (unless (and end ( count (- end start)))
      (setq skip-count (max 0 (- (count-1 item list start end test invertp key one-arg-predicate) count)))))
  (if (and (plusp count)
           (or (null end) (> end start)))
      (let* ((tail (nthcdr start (variable-location list)))
             (end-tail (and end (nthcdr (- end start) (cdr tail)))))
        (do (elt)
            ((eq (cdr tail) end-tail))
          (setq elt (if key (funcall key (cadr tail)) (cadr tail)))
          (cond ((and (eq invertp (not (cond (one-arg-predicate
                                              (funcall one-arg-predicate elt))
                                             (test
                                              (funcall test item elt))
                                             (t
                                              (eql item elt)))))
                      (minusp (decf skip-count)))
                 (setf (cdr tail) (cddr tail))
                 (when (zerop (decf count))
                   (return)))
                (t (setq tail (cdr tail)))))))
  list)

(defun remove-duplicates (sequence &key (start 0) end key from-end test test-not)
  "Returns SEQUENCE, partially copied if necessary, omitting duplicate elements.
Elements are compared using TEST, a function of two arguments.
 Elements match if TEST returns non-NIL.  Alternatively, specify TEST-NOT;
 then elements match if TEST-NOT returns NIL.
If KEY is non-NIL, then it is a function of one arg, which is applied
 to each element to get the \"key\" which is passed to TEST or TEST-NOT.
START and END are indices specifying the part of SUBSEQUENCE considered.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside this subsequence are not looked at.
Duplicate elements are not necessarily identical.
Normally the last duplicate element is the one retained.
If FROM-END is non-NIL, the first one is retained."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (funcall (etypecase sequence
             (list #'remove-duplicates-from-list)
             (vector #'remove-duplicates-from-array))
           sequence start end (or test-not test) (not (null test-not)) key from-end))

(defun delete-duplicates (sequence &key (start 0) end key from-end test test-not)
  "Like REMOVE-DUPLICATES except that SEQUENCE may be destructively modified.
\(If it is an array, it will probably be copied anyway.)
Returns SEQUENCE, sans any duplicate elements.
Elements are compared using TEST, a function of two arguments.
 Elements match if TEST returns non-NIL.  Alternatively, specify TEST-NOT;
 then elements match if TEST-NOT returns NIL.
If KEY is non-NIL, then it is a function of one arg, which is applied
 to each element to get the \"key\" which is passed to TEST or TEST-NOT.
START and END are indices specifying the part of SUBSEQUENCE considered.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside this subsequence are not looked at.
Duplicate elements are not necessarily identical.
Normally the last duplicate element is the one retained.
If FROM-END is non-NIL, the first one is retained."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (funcall (etypecase sequence
             (list #'delete-duplicates-from-list)
             (vector #'delete-duplicates-from-array))
           sequence start end (or test-not test) (not (null test-not)) key from-end))

(defun remove-duplicates-from-array (sequence start end test invertp key from-end)
  (or end (setq end (length sequence)))
  (let ((temp (get-temp-vector (- end start))))
    (setf (fill-pointer temp) 0)
    ;; Find the indices of all the things we want to remove.
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start)
             (nreverse temp))
          (when (nth-value 1 (find-1 (if key (funcall key (cl:aref sequence i))
                                       (cl:aref sequence i))
                                     sequence start i test invertp key))
            (vector-push i temp)))
        (do ((i start (1+ i)))
            (( i end))
          (when (nth-value 1 (find-1 (if key (funcall key (cl:aref sequence i))
                                       (cl:aref sequence i))
                                     sequence (1+ i) end test invertp key))
            (vector-push i temp))))
    ;; Now TEMP contains the indices of the elements to be removed, in ascending order.
    (cond ((zerop (fill-pointer temp))
           (setq *temp-vector* temp)
           sequence)
          (t
           (let ((result (if (array-has-fill-pointer-p sequence)
                             (zl:make-array (array-dimension sequence 0) :type (array-type sequence)
                                            :fill-pointer (- (fill-pointer sequence) (fill-pointer temp)))
                           (zl:make-array (- (length sequence) (fill-pointer temp))
                                          :type (array-type sequence)))))
             (copy-array-portion sequence 0 (cl:aref temp 0) result 0 (cl:aref temp 0))
             (do ((i 1 (1+ i))
                  (stop (fill-pointer temp)))
                 (( i stop)
                  (let ((x (1+ (cl:aref temp (1- i)))))
                    (copy-array-portion sequence  x (length sequence)
                                        result (- x i) (length result))))
               (let ((x (1+ (cl:aref temp (1- i))))
                     (y (cl:aref temp i)))
                 (copy-array-portion sequence x y
                                     result (- x i) (- y i))))
             (setf (fill-pointer temp) 0)
             (setq *temp-vector* temp)
             result)))))
(deff delete-duplicates-from-array 'remove-duplicates-from-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:38
(defun remove-duplicates-from-list (list start end test invertp key from-end)
  (if (or (null end) (> end start))
      (loop with head = (variable-location list)
            with start-list = (nthcdr start list)
            for start-tail = start-list then (cdr head)
            with end-tail = (and end (nthcdr (- end start) start-tail))
            as tail = (do ((l start-tail (cdr l))
                           elt)
                          ((eq l end-tail) l)
                        (setq elt (if key (funcall key (car l)) (car l)))
                        (when (nth-value 1
                                (if from-end
                                    (find-1 elt start-list 0 l test invertp key)
                                    (find-1 elt (cdr l) 0 end-tail test invertp key)))
                            (return l)))
            until (eq tail end-tail)
         do (loop until (eq (cdr head) tail)
                  do (setf (cdr head) (setq head (cons (cadr head) (cddr head)))))
            (setf (cdr head) (cdr tail))))
  list)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:39
(defun delete-duplicates-from-list (list start end test invertp key from-end)
  (if (or (null end) (> end start))
      (let* ((tail (nthcdr start (variable-location list)))
             (start-list (cdr tail))
             (end-tail (and end (nthcdr (- end start) (cdr tail)))))
        (do (elt)
            ((eq (cdr tail) end-tail))
          (setq elt (if key (funcall key (cadr tail)) (cadr tail)))
          (if (nth-value 1
                (if from-end
                    (find-1 elt start-list 0 (cdr tail) test invertp key)
                    (find-1 elt (cddr tail) 0 end-tail test invertp key)))
              (setf (cdr tail) (cddr tail))
              (setq tail (cdr tail))))))
  list)

(defun substitute-if (newitem predicate sequence &key (start 0) end count key from-end)
  "Return SEQUENCE copied as necessary so that NEWITEM replaces any elements
satisfying PREDICATE.
SEQUENCE can be a list or an array.  A list may be copied partially.
If COUNT is non-NIL, it is the number of such elements to replace.
The first COUNT-many suitable elements are replaced, or,
 if FROM-END is non-NIL, the last COUNT-many are replaced.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to PREDICATE.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are never substituted for."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           t newitem nil sequence start end count nil nil key from-end predicate))

(defun substitute-if-not (newitem predicate sequence &key (start 0) end count key from-end)
  "Like SUBSTITUTE-IF except the elements replaced are those for which PREDICATE returns NIL."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           t newitem nil sequence start end count nil t key from-end predicate))

(defun substitute (newitem olditem sequence
                   &key test test-not (start 0) end count key from-end)
  "Return SEQUENCE copied if necessary so that NEWITEM replaces any elements matching OLDITEM.
SEQUENCE can be a list or an array.  A list may be copied partially.
If COUNT is non-NIL, it is the number of such elements to replace.
The first COUNT-many suitable elements are replaced, or,
 if FROM-END is non-NIL, the last COUNT-many are replaced.
TEST is a function of two args to use to compare OLDITEM against an element (or key).
 An element matches when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST or TEST-NOT.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are never substituted for."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           t newitem olditem sequence start end count (or test-not test) (not (null test-not))
           key from-end nil))

(defun nsubstitute-if (newitem predicate sequence &key (start 0) end count key from-end)
  "Like SUBSTITUTE-IF except that SEQUENCE may be destructively modified rather than copied."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           nil newitem nil sequence start end count nil nil key from-end predicate))

(defun nsubstitute-if-not (newitem predicate sequence &key (start 0) end count key from-end)
  "Like SUBSTITUTE-IF-NOT except that SEQUENCE may be destructively modified
rather than copied."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           nil newitem nil sequence start end count nil t key from-end predicate))

(defun nsubstitute (newitem olditem sequence
                    &key test test-not (start 0) end count key from-end)
  "Like SUBSTITUTE except that SEQUENCE may be destructively modified rather than copied."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           nil newitem olditem sequence start end count
           (or test-not test) (not (null test-not)) key from-end nil))

(defun substitute-in-array (copyflag newitem olditem sequence
                            start end count test invertp key from-end one-arg-predicate
                            &aux result)
  (or count (setq count (length sequence)))
  (or end (setq end (length sequence)))
  (or copyflag (setq result sequence))
  (do ((i (if from-end (1- end) start)
          (+ i inc))
       (inc (if from-end -1 1))
       (num-replaced 0)
       elt)
      ((or (if from-end (< i start) ( i end))
           ( num-replaced count)))
    (setq elt (if key (funcall key (cl:aref sequence i)) (cl:aref sequence i)))
    (when (eq invertp (not (cond (one-arg-predicate
                                  (funcall one-arg-predicate elt))
                                 (test
                                  (funcall test olditem elt))
                                 (t
                                  (eql olditem elt)))))
      (unless result
        (setq result (if (array-has-fill-pointer-p sequence)
                         (zl:make-array (array-dimension sequence 0) :type (array-type sequence)
                                        :fill-pointer (fill-pointer sequence))
                       (zl:make-array (array-dimension sequence 0) :type (array-type sequence))))
        (copy-array-contents sequence result))
      (setf (cl:aref result i) newitem)
      (incf num-replaced)))
  (or result sequence))

(defun substitute-in-list (copyflag newitem olditem list
                           start end count test invertp key from-end one-arg-predicate
                           &aux (skip-count 0))
  (or count (setq count most-positive-fixnum))
  (when from-end
    (unless (and end ( count (- end start)))
      (setq skip-count (max 0 (- (count-1 olditem list start end test invertp
                                          key one-arg-predicate)
                                 count)))))
  (if (and (plusp count)
           (or (null end) (> end start)))
      (loop with head = (variable-location list)
            for start-tail = (nthcdr start list) then (cdr head)
            with end-tail = (and end (nthcdr (- end start) start-tail))
            as tail = (do ((l start-tail (cdr l))
                           elt)
                          ((eq l end-tail) l)
                        (setq elt (if key (funcall key (car l)) (car l)))
                        (when (eq invertp (not (cond (one-arg-predicate
                                                      (funcall one-arg-predicate elt))
                                                     (test
                                                      (funcall test olditem elt))
                                                     (t
                                                      (eql olditem elt)))))
                          (return l)))
            until (eq tail end-tail)
         when (minusp (decf skip-count))
           do (cond ((not copyflag)
                     (setf (car tail) newitem))
                    (t
                     (loop until (eq (cdr head) (cdr tail))
                           do (setf (cdr head) (setq head (cons (cadr head) (cddr head)))))
                     (setf (car head) newitem)))
              (when (zerop (setq count (1- count)))
                (return))))
  list)

(defun position-if (predicate sequence &key from-end (start 0) end key)
  "Return index in SEQUENCE of first element that satisfies PREDICATE.
Value is NIL if no element satisfies PREDICATE.  SEQUENCE can be a list or an array.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to PREDICATE.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not tested.
 The value is the index in SEQUENCE, not in the subsequence which was searched.
If FROM-END is non-NIL, the value describes the LAST element in SEQUENCE
 (or specified subsequence) that satisfies the predicate."
  (check-type sequence sequence)
  (nth-value 1 (find-1 nil sequence start end nil nil key from-end predicate)))

(defun position-if-not (predicate sequence &key from-end (start 0) end key)
  "Like POSITION-IF but looks for an element which does NOT satisfy PREDICATE."
  (check-type sequence sequence)
  (nth-value 1 (find-1 nil sequence start end nil t key from-end predicate)))

(defun position (item sequence &key from-end test test-not (start 0) end key)
  "Return index in SEQUENCE of first element that matches ITEM.
Value is NIL if no element matches.  SEQUENCE can be a list or an array.
TEST is a function of two args to use to compare ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST or TEST-NOT.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not tested.
 The value is the index in SEQUENCE, not in the subsequence which was searched.
If FROM-END is non-NIL, the value describes the LAST element in SEQUENCE
 (or specified subsequence) that matches."
  (check-type sequence sequence)
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (if (and (cl:listp sequence) (null from-end) (eq start 0) (null end) (null key)
           (or (eq test 'eq) (eq test #'eql)))
      (find-position-in-list item sequence)
    (nth-value 1 (find-1 item sequence start end (or test-not test) (not (null test-not))
                         key from-end nil))))

(defun find-if (predicate sequence &key from-end (start 0) end key)
  "Return the first element of SEQUENCE that satisfies PREDICATE.
Value is NIL if no element satisfies PREDICATE.  SEQUENCE can be a list or an array.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to PREDICATE.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not tested.
If FROM-END is non-NIL, the value is the LAST element in SEQUENCE
 (or specified subsequence) that satisfies the predicate."
  (check-type sequence sequence)
  (values (find-1 nil sequence start end nil nil key from-end predicate)))

(defun find-if-not (predicate sequence &key from-end (start 0) end key)
  "Like FIND-IF but looks for an element which does NOT satisfy PREDICATE."
  (check-type sequence sequence)
  (values (find-1 nil sequence start end nil t key from-end predicate)))

(defun find (item sequence &key from-end test test-not (start 0) end key)
  "Return first element of SEQUENCE that matches ITEM.
Value is NIL if no element matches.  SEQUENCE can be a list or an array.
TEST is a function of two args to use to compare ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST or TEST-NOT.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not tested.
If FROM-END is non-NIL, the value is the LAST element in SEQUENCE
 (or specified subsequence) that matches."
  (check-type sequence sequence)
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (values (find-1 item sequence start end (or test-not test) (not (null test-not))
                  key from-end nil)))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:42
(defun find-1 (item sequence &optional (start 0) end
                                       test invertp key from-end one-arg-predicate)
  (declare (values item index))
  (if (and from-end (arrayp sequence))
      (do ((index (1- (or end (length sequence))) (1- index))
           elt)
          ((< index start)
           nil)
        (setq elt (if key (funcall key (cl:aref sequence index))
                    (cl:aref sequence index)))
        (when (eq invertp (not (cond (one-arg-predicate
                                      (funcall one-arg-predicate elt))
                                     (test
                                      (funcall test item elt))
                                     (t
                                      (eql item elt)))))
          (return (values (cl:aref sequence index) index))))
      (do ((index (seq-start sequence start))
           (i start (1+ i))
           (stop-index (if (consp end) end (seq-end sequence end)))
           last-pos elt)
          ((eq index stop-index)
           (if last-pos (values (elt sequence last-pos) last-pos)))
        (setq elt (key-fetch-inc key sequence index))
        (when (eq invertp (not (cond (one-arg-predicate
                                      (funcall one-arg-predicate elt))
                                     (test
                                      (funcall test item elt))
                                     (t
                                      (eql item elt)))))
          (if from-end
              (setq last-pos i)
            (return (values (elt sequence i) i)))))))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:43
(defun count-if (predicate sequence &key from-end (start 0) end key)
  "Return number of elements of SEQUENCE (a list or array) that satisfy PREDICATE.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to PREDICATE.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not counted."
  from-end ; purely for Steele, p. 257
  (check-type sequence sequence)
  (count-1 nil sequence start end nil nil key predicate))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:43
(defun count-if-not (predicate sequence &key from-end (start 0) end key)
  "Like COUNT-IF but counts elements that do NOT satisfy PREDICATE."
  from-end ; purely for Steele, p. 257
  (check-type sequence sequence)
  (count-1 nil sequence start end nil t key predicate))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:43
(defun count (item sequence &key from-end test test-not (start 0) end key)
  "Return number of elements of SEQUENCE (a list or vector) that match ITEM.
TEST is a function of two args to use to compare ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST or TEST-NOT.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not counted."
  from-end ; purely for Steele, p. 257
  (check-type sequence sequence)
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (count-1 item sequence start end (or test-not test) (not (null test-not)) key nil))

(defun count-1 (item sequence &optional (start 0) end test invertp key one-arg-predicate)
  (do ((index (seq-start sequence start))
       (count 0)
       (stop-index (seq-end sequence end))
       elt)
      ((eq index stop-index)
       count)
    (setq elt (key-fetch-inc key sequence index))
    (when (eq invertp (not (cond (one-arg-predicate
                                  (funcall one-arg-predicate elt))
                                 (test
                                  (funcall test item elt))
                                 (t
                                  (eql item elt)))))
      (incf count))))

(defun mismatch (sequence1 sequence2 &key from-end test test-not key
                                          (start1 0) end1 (start2 0) end2)
  "Return index in SEQUENCE1 of first mismatch between it and SEQUENCE2.
Elements are compared one by one, starting with elements at indexes START1 and START2
 and stopping when index 1 reaches END1 or index 2 reaches END2.
If sequences match, value is NIL.  If they match until one is exhausted but not both,
 the value is the index in SEQUENCE1 at which one sequence is exhausted.
TEST is a function of two args to use to compare two elements.
 The elements match when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST or TEST-NOT.  If KEY is NIL, the element itself is used.
FROM-END non-NIL means comparison aligns right ends of the specified
 subsequences and returns one plus the index of the rightmost mismatch."
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (mismatch-1 sequence1 sequence2 start1 end1 start2 end2
              (or test-not test) (not (null test-not)) key from-end))

(defun mismatch-1 (sequence1 sequence2
                   &optional (start1 0) end1 (start2 0) end2 test invertp key from-end)
  (or end1 (setq end1 (length sequence1)))
  (or end2 (setq end2 (length sequence2)))
  (cond (from-end
         (funcall (if (and (arrayp sequence1) (arrayp sequence2))
                      #'mismatch-arrays-from-end
                    #'mismatch-sequences-from-end)
                  sequence1 sequence2 start1 end1 start2 end2 test invertp key))
        (t
         (or test (setq test #'eql))
         (do ((index1 (seq-start sequence1 start1))
              (index2 (seq-start sequence2 start2))
              (i start1 (1+ i))
              (stop1 (seq-end sequence1 end1))
              (stop2 (seq-end sequence2 end2)))
             ((or (eq index1 stop1) (eq index2 stop2))
              (unless (and (eq index1 stop1) (eq index2 stop2))
                i))
           (unless (eq invertp (not (funcall test (key-fetch-inc key sequence1 index1)
                                             (key-fetch-inc key sequence2 index2))))
             (return i))))))

(defun mismatch-arrays-from-end (sequence1 sequence2 start1 end1 start2 end2
                                 test invertp key)
  (or test (setq test #'eql))
  (do ((index1 (1- end1) (1- index1))
       (index2 (1- end2) (1- index2)))
      ((or (< index1 start1) (< index2 start2))
       (unless (and (< index1 start1) (< index2 start2))
         (1+ index1)))
    (unless (eq invertp (not (if key (funcall test (funcall key (cl:aref sequence1 index1))
                                                   (funcall key (cl:aref sequence2 index2)))
                               (funcall test (cl:aref sequence1 index1)
                                             (cl:aref sequence2 index2)))))
      (return (1+ index1)))))

(defun mismatch-sequences-from-end (sequence1 sequence2 start1 end1 start2 end2
                                    test invertp key)
  (or test (setq test #'eql))
  (let ((compare-length (min (- end1 start1) (- end2 start2))))
    (do ((index1 (seq-start sequence1 (- end1 compare-length)))
         (index2 (seq-start sequence2 (- end2 compare-length)))
         (i (- end1 compare-length) (1+ i))
         (last-mismatch-index1 (unless (= (- end1 start1) (- end2 start2))
                                 (- end1 compare-length))))
        ((= i end1)
         last-mismatch-index1)
      (unless (eq invertp (not (funcall test (key-fetch-inc key sequence1 index1)
                                             (key-fetch-inc key sequence2 index2))))
        (setq last-mismatch-index1 (1+ i))))))


(defun search (for-sequence-1 in-sequence-2 &key (start1 0) end1 (start2 0) end2
                                                 from-end test test-not key)
  "Return index in IN-SEQUENCE-2 of first subsequence that matches FOR-SEQUENCE-1.
If no occurrence is found, the value is NIL.
MISMATCH is used to do the matching, with TEST, TEST-NOT and KEY passed along.
START1 and END1 are indices specifying a subsequence of FOR-SEQUENCE-1 to search for.
 The rest of FOR-SEQUENCE-1 might as well not be there.
START2 and END2 are indices specifying a subsequence of IN-SEQUENCE-2 to search through.
 However, the value returned is an index into the entire IN-SEQUENCE-2.
If FROM-END is non-NIL, the value is the index of the LAST subsequence that
 matches FOR-SEQUENCE-1 or the specified part of it.
In either case, the value returned is the index of the beginning of
 the subsequence (of IN-SEQUENCE-2) that matches."
  (check-type for-sequence-1 sequence)
  (check-type in-sequence-2 sequence)
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (let* ((length1 (- (or end1 (length for-sequence-1)) start1))
         (really-backwards (and from-end (arrayp in-sequence-2)))
         (pretend-backwards (and from-end (not (arrayp in-sequence-2))))
         (real-end2 (- (or end2 (length in-sequence-2)) length1 -1))
         (test (or test-not test #'eql))
         (invertp (not (null test-not))))
    (unless (< real-end2 0)
      ;; If REALLY-BACKWARDS, we actually step backwards through IN-SEQUENCE-2.
      ;; If PRETEND-BACKWARDS, we step forwards but remember the last thing we found.
      (do ((index (seq-start in-sequence-2 start2))
           (inc (if really-backwards -1 1))
           (i (if really-backwards (1- real-end2) start2)
              (+ i inc))
           last-index-if-from-end
           (stop-index (if really-backwards (1- start2) (seq-end in-sequence-2 real-end2)))
           (START-KEY-1 (FUNCALL (OR KEY #'IDENTITY)
                                 (ELT FOR-SEQUENCE-1 START1))))
          ((IF REALLY-BACKWARDS
               (OR (< I 0)
                   (EQ INDEX STOP-INDEX))
             (EQ INDEX STOP-INDEX))
           LAST-INDEX-IF-FROM-END)
        (if really-backwards (setq index i))
        (and (eq invertp (not (funcall test start-key-1
                                       (key-fetch-inc key in-sequence-2 index))))
             (not (mismatch-1 for-sequence-1 in-sequence-2 start1 end1 i (+ i length1)
                              test invertp key nil))
             (if pretend-backwards
                 (setq last-index-if-from-end i)
               (return i)))))))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:45
(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise AND of all the two bit arrays,
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-and bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:46
(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise OR of all the two bit arrays,
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-ior bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:47
(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise XOR of all the two bit arrays,
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-xor bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:48
(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise EQV of all the two bit arrays,
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-eqv bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:49
(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise NAND of all the two bit arrays,
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-nand bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:50
(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise NOR of all the two bit arrays,
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-nor bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:50
(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise AND of BIT-ARRAY2 with the complement of BIT-ARRAY1.
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-andc1 bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:51
(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise AND of BIT-ARRAY1 with the complement of BIT-ARRAY2.
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-andc2 bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:52
(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise OR of BIT-ARRAY2 with the complement of BIT-ARRAY1.
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-orc1 bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:53
(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Returns the bitwise OR of BIT-ARRAY1 with the complement of BIT-ARRAY2.
The result is stored into RESULT-BIT-ARRAY, or returned as a new bit array
if RESULT-BIT-ARRAY is NIL.  If it is T, BIT-ARRAY1 is used for the result."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array1))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array1))))
  (bit-array-logical-op boole-orc2 bit-array1 bit-array2 result-bit-array)
  result-bit-array)

;Could be microcoded and made far faster, if anyone ever cares.
(defun bit-array-logical-op (alu-function bv1 bv2 bv-out)
  (cond ((not (equal (array-dimensions bv1) (array-dimensions bv2)))
         (ferror "Dimensions of input bit-arrays do not agree"))
        ((not (equal (array-dimensions bv1) (array-dimensions bv-out)))
         (ferror "Dimensions of input arrays do not agree with that of the output array")))
  (dotimes (i (array-length bv-out))
    (setf (ar-1-force bv-out i)
          (boole alu-function (ar-1-force bv1 i) (ar-1-force bv2 i))))
  bv-out)

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:54
(defun bit-not (bit-array &optional result-bit-array)
  "Returns a bit array containing the complements of the elements of BIT-ARRAY."
  (cond ((eq result-bit-array t) (setq result-bit-array bit-array))
        ((eq result-bit-array nil) (setq result-bit-array (copy-object bit-array))))
  (dotimes (i (array-length bit-array))
    (setf (ar-1-force result-bit-array i) (lognot (ar-1-force bit-array i))))
  result-bit-array)

(defun cl:subst (new old tree &key test test-not key)
  "Replace with NEW every atom or subtree in TREE which matches OLD.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used.
TEST is a function passed OLD and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (block subst
    (when (and (null test-not) (null key))
      (cond ((or (null test) (eq test 'eql) (eq test #'eql))
             (return-from subst (subst-eql new old tree)))
            ((or (eq test #'eq) (eq test 'eq))
             (return-from subst (subst-eq new old tree)))))
    (subst-1 new old tree (or test-not test) (not (null test-not)) key nil)))

(defun subst-if (new predicate tree &key key)
  "Replace with NEW every atom or subtree in TREE which satisfies PREDICATE.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (subst-1 new nil tree nil nil key predicate))

(defun subst-if-not (new predicate tree &key key)
  "Replace with NEW every atom or subtree in TREE which doesn't satisfy PREDICATE.
List structure is copied as necessary so that the original TREE is not modified.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (subst-1 new nil tree nil t key predicate))

(defun subst-1 (new old tree &optional test invertp key one-arg-predicate)
  (cond ((let ((elt (if key (funcall key tree) tree)))
           (eq invertp (not (cond (one-arg-predicate (funcall one-arg-predicate elt))
                                  (test (funcall test old elt))
                                  (t (eql old elt))))))
         new)
        ((atom tree)
         tree)
        (t
         (let ((newcar (subst-1 new old (car tree) test invertp key one-arg-predicate))
               (newcdr (subst-1 new old (cdr tree) test invertp key one-arg-predicate)))
           (if (and (eql newcar (car tree))
                    (eql newcdr (cdr tree)))
               tree
             (cons newcar newcdr))))))

(defun subst-eql (new old tree)
  (cond ((eql old tree)
         new)
        ((atom tree)
         tree)
        (t
         (let ((newcar (subst-eql new old (car tree)))
               (newcdr (subst-eql new old (cdr tree))))
           (if (and (eql newcar (car tree))
                    (eql newcdr (cdr tree)))
               tree
             (cons newcar newcdr))))))

(defun subst-eq (new old tree)
  (cond ((eq old tree)
         new)
        ((atom tree)
         tree)
        (t
         (let ((newcar (subst-eq new old (car tree)))
               (newcdr (subst-eq new old (cdr tree))))
           (if (and (eql newcar (car tree))
                    (eql newcdr (cdr tree)))
               tree
             (cons newcar newcdr))))))

(defun nsubst (new old tree &key test test-not key)
  "Destructively replace with NEW every atom or subtree in TREE which matches OLD.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used.
TEST is a function passed OLD and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (when (and (null test-not) (null key))
    (cond ((or (null test) (eq test #'eql) (eq test 'eql))
           (return-from nsubst (nsubst-eql new old tree)))
          ((or (eq test #'eq) (eq test 'eq))
           (return-from nsubst (nsubst-eq new old tree)))))
  (nsubst-1 new old tree (or test-not test) (not (null test-not)) key nil))

(defun nsubst-if (new predicate tree &key key)
  "Destructively replace with NEW every atom or subtree in TREE which satisfies PREDICATE.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (nsubst-1 new nil tree nil nil key predicate))

(defun nsubst-if-not (new predicate tree &key key)
  "Destructively replace with NEW every atom or subtree in TREE not satisfying PREDICATE.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used."
  (nsubst-1 new nil tree nil t key predicate))

(defun nsubst-eql (new old s-exp)
  (cond ((eql old s-exp) new)
        ((atom s-exp) s-exp)
        (t (do ((s s-exp (cdr s))
                (prev nil s))
               ((atom s)
                (when (eql old s)
                  (setf (cdr prev) new)))
             (if (atom (car s))
                 (when (eql old (car s))
                   (setf (car s) new))
               (setf (car s) (nsubst-eql new old (car s)))))
           s-exp)))

(defun nsubst-eq (new old s-exp)
  (cond ((eq old s-exp) new)
        ((atom s-exp) s-exp)
        (t (do ((s s-exp (cdr s))
                (prev nil s))
               ((atom s)
                (when (eq old s)
                  (setf (cdr prev) new)))
             (if (atom (car s))
                 (when (eq old (car s))
                   (setf (car s) new))
               (setf (car s) (nsubst-eq new old (car s)))))
           s-exp)))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:56
; After subst-1 (see Steele, p. 274).
(defun nsubst-1 (new old tree &optional test invertp key one-arg-predicate)
  (cond ((let ((elt (if key (funcall key tree) tree)))
           (eq invertp (not (cond (one-arg-predicate (funcall one-arg-predicate elt))
                                  (test (funcall test old elt))
                                  (t (eql old elt))))))
         new)
        ((atom tree)
         tree)
        (t
         (let ((newcar (nsubst-1 new old (car tree) test invertp key one-arg-predicate))
               (newcdr (nsubst-1 new old (cdr tree) test invertp key one-arg-predicate)))
           (unless (eql newcar (car tree))
             (setf (car tree) newcar))
           (unless (eql newcdr (cdr tree))
             (setf (cdr tree) newcdr))
           tree))))

(defun cl:member (item list &rest stuff &key test test-not key)
  "Return a tail of LIST whose car is the first element of LIST that matches ITEM.
KEY, if non-NIL, is a function applied to each element to get the
 object to match against.  If KEY is NIL, the element itself is used.
TEST is a function passed ITEM and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match."
  (cond ((null stuff)
         (member-eql item list))
        ((and test test-not)
         (ferror "Both ~S and ~S specified" :test :test-not))
        (t
         (member-1 item list test test-not key))))

(defun member-1 (item list &optional test test-not key)
  (if (null key)
      (cond ((or (eq test 'eq) (eq test #'eq))
             (memq item list))
            ((or (and (null test) (null test-not)) (eq test 'eql) (eq test #'eql))
             (member-eql item list))
            (t
             (do ((tail list (cdr tail)))
                 ((null tail))
               (if (if test
                       (funcall test item (car tail))
                       (not (funcall test-not item (car tail))))
                   (return tail)))))
    (do ((tail list (cdr tail)))
        ((null tail))
      (if (cond (test (funcall test item (funcall key (car tail))))
                (test-not (not (funcall test-not item (funcall key (car tail)))))
                (t (eql item (funcall key (car tail)))))
          (return tail)))))

(defun member-if (predicate list &key key)
  "Return a tail of LIST whose car is the first element of LIST that satisfies PREDICATE."
  (do ((tail list (cdr tail)))
      ((null tail))
    (if (funcall predicate (if key (funcall key (car tail)) (car tail)))
        (return tail))))

(defun member-if-not (predicate list &key key)
  "Return a tail of LIST whose car is the first element that doesn't satisfy PREDICATE."
  (do ((tail list (cdr tail)))
      ((null tail))
    (unless (funcall predicate (if key (funcall key (car tail)) (car tail)))
      (return tail))))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:57
(defun cl:assoc (item list &key test test-not key)
  "Returns the first element of LIST whose car matches ITEM, or NIL if none.
TEST is a function used to compare ITEM with each car;
 they match if it returns non-NIL.  TEST defaults to EQL.
Alternatively, specify TEST-NOT, a function which returns NIL for a match."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (assoc-1 item list test test-not key))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:21:58
(defun assoc-1 (item list &optional test test-not key)
  (cond ((AND (NULL KEY) (or (eq test 'eq) (eq test #'eq)))
         (assq item list))                      ;NO KEY, test is EQ
        ((or (and (NULL KEY) (null test) (null test-not) (typep item '(or fixnum short-float (not number))))
             (eq test 'eql) (eq test #'eql))    ;NO key, test is EQL, and item is a number
         (assq item list))
        ((NULL KEY)
           (do ((tail list (cdr tail)))
               ((null tail))
             (and (car tail)
                  (COND (TEST (funcall test item (caar tail)))
                        (TEST-NOT (not (funcall test-not item (caar tail))))
                        (T (eql item (caar tail))))
                  (return (car tail)))))
        (t
         (do ((tail list (cdr tail)))
             ((null tail))
             (and (car tail)
                  (COND (TEST (funcall test item (funcall key (caar tail))))
                        (TEST-NOT (not (funcall test-not item (funcall key (caar tail)))))
                        (T (eql item (funcall key (caar tail)))))
                  (return (car tail)))))))

(defun assoc-if (predicate list)
  "Returns the first element of LIST whose car satisfies PREDICATE, or NIL if none."
  (do ((tail list (cdr tail)))
      ((null tail))
    (and (car tail)
         (funcall predicate (caar tail))
         (return (car tail)))))

(defun assoc-if-not (predicate list)
  "Returns the first element of LIST whose car does not satisfy PREDICATE, or NIL if none."
  (do ((tail list (cdr tail)))
      ((null tail))
    (and (car tail)
         (not (funcall predicate (caar tail)))
         (return (car tail)))))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:22:00
(defun cl:rassoc (item list &key test test-not key)
  "Returns the first element of LIST whose cdr matches ITEM, or NIL if none.
TEST is a function used to compare ITEM with each cdr;
 they match if it returns non-NIL.  TEST defaults to EQL.
Alternatively, specify TEST-NOT, a function which returns NIL for a match."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (rassoc-1 item list test test-not key))

;; Copied from LAD: RELEASE-3.SYS; GENRIC.LISP#60 on 27-Mar-87 12:22:00
(defun rassoc-1 (item list &optional test test-not key)
  (cond ((and (null key) (or (eq test 'eq) (eq test #'eq)))
         (rassq item list))                     ;NO KEY, test is EQ
        ((or (and (NULL KEY) (null test) (null test-not) (typep item '(or fixnum short-float (not number))))
             (eq test 'eql) (eq test #'eql))    ;NO key, test is EQL, and item is a number
         (rassq item list))
        ((NULL KEY)
           (do ((tail list (cdr tail)))
               ((null tail))
             (and (car tail)
                  (COND (TEST (funcall test item (cdar tail)))
                        (TEST-NOT (not (funcall test-not item (cdar tail))))
                        (T (eql item (cdar tail))))
                  (return (car tail)))))
        (t
         (do ((tail list (cdr tail)))
             ((null tail))
             (and (car tail)
                  (COND (TEST (funcall test item (funcall key (cdar tail))))
                        (TEST-NOT (not (funcall test-not item (funcall key (cdar tail)))))
                        (T (eql item (funcall key (cdar tail)))))
                  (return (car tail)))))))

(defun rassoc-if (predicate list)
  "Returns the first element of LIST whose cdr satisfies PREDICATE, or NIL if none."
  (do ((tail list (cdr tail)))
      ((null tail))
    (and (car tail)
         (funcall predicate (cdar tail))
         (return (car tail)))))

(defun rassoc-if-not (predicate list)
  "Returns the first element of LIST whose cdr does not satisfy PREDICATE, or NIL if none."
  (do ((tail list (cdr tail)))
      ((null tail))
    (and (car tail)
         (not (funcall predicate (cdar tail)))
         (return (car tail)))))

(defun adjoin (item list &rest stuff &key test test-not key area)
  "Return either LIST or (CONS ITEM LIST); the latter if no element of LIST matches ITEM.
KEY, if non-NIL, is a function applied ITEM and to each element of LIST
 to get the objects to match.  If KEY is NIL, ITEM and the element itself are used.
TEST is a function passed ITEM (or its key) and the element (or its key).
 There is a match if TEST returns non-NIL.  TEST defaults to EQL.
Alternatively, pass TEST-NOT, a function to return NIL when there is a match.
AREA is the area in which to make the cons, if an item is adjoined."
  (cond ((null stuff)
         (if (member-eql item list)
             list
           (cons item list)))
        ((and test test-not)
         (ferror "Both ~S and ~S specified" :test :test-not))
        (t
         (if (member-1 (if key (funcall key item) item) list test test-not key)
             list
           (if area (cons-in-area item list area) (cons item list))))))

(defun adjoin-1 (item list &optional area test test-not key)
  (if (member-1 (if key (funcall key item) item) list test test-not key)
      list
    (if area (cons-in-area item list area) (cons item list))))

(defun cl:union (list1 list2 &rest stuff &key test test-not key)
  "Return the union of LIST1 and LIST2, regarded as sets.
The result is LIST1 plus any elements of LIST2 that match no element of LIST1.
If the first argument has no duplicate elements, neither does the value.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (cond ((null stuff)
         (dolist (x list1)
           (unless (member-eql x list2)
             (push x list2)))
         list2)
        ((and (null test-not)
              (null key)
              (or (eq test #'eq) (eq test 'eq)))
         (dolist (x list1)
           (unless (memq x list2)
             (push x list2)))
         list2)
        ((and test test-not)
         (ferror "Both ~S and ~S specified" :test :test-not))
        (t
         (let ((result list2))
           (dolist (x list1)
             (unless (member-1 (if key (funcall key x) x) list2 test test-not key)
               (push x result)))
           result))))

(defun cl:nunion (list1 list2 &rest stuff &key test test-not key)
  "Destructively modify LIST1 to be the union of LIST1 and LIST2, regarded as sets.
Any element of LIST that matches no element of LIST1 is NCONC'd at the end of LIST1.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (block nunion
    (let ((tail (or (last list1) (variable-location list1))))
      (cond ((null stuff)
             (dolist (elt list2)
               (unless (member-eql elt list1)
                 (setf (cdr tail) (setq tail (ncons elt))))))
            ((and (null test-not)
                  (null key)
                  (or (eq test #'eq) (eq test 'eq)))
             (dolist (elt list2)
               (unless (memq elt list1)
                 (setf (cdr tail) (setq tail (ncons elt))))))
            ((and test test-not)
             (ferror "Both ~S and ~S specified" :test :test-not))
            (t
             (dolist (elt list2)
               (unless (member-1 (if key (funcall key elt) elt) list1 test test-not key)
                 (setf (cdr tail) (setq tail (ncons elt))))))))
    list1))

(defun cl:intersection (list1 list2 &rest stuff &key test test-not key &aux result)
  "Return the intersection of LIST1 and LIST2, regarded as sets.
Any element of LIST1 which matches some element of LIST2 is included.
If the first argument has no duplicate elements, neither does the value.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (block intersection
    (cond ((null stuff)
           (dolist (x list1)
             (when (member-eql x list2)
               (push x result))))
          ((and (null test-not)
                (null key)
                (or (eq test #'eq) (eq test 'eq)))
           (dolist (x list1)
             (when (memq x list2)
               (push x result))))
          ((and test test-not)
           (ferror "Both ~S and ~S specified" :test :test-not))
          (t
           (dolist (x list1)
             (when (member-1 (if key (funcall key x) x) list2 test test-not key)
               (push x result)))))
    result))

(defun cl:nintersection (list1 list2 &rest stuff &key test test-not key)
  "Destructively modify LIST1 to be the intersection of LIST1 and LIST2, regarded as sets.
Any element of LIST1 which fails to match some element of LIST2 is deleted.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (block nintersection
    (do ((list list1 (cdr list))
         (result)
         (old))
        ((null list) result)
      (cond ((if (null stuff)
                 (member-eql (car list) list2)
               (member-1 (if key (funcall key (car list)) (car list)) list2 test test-not key))
             (or result (setq result list))
             (setq old list))
            (old
             (setf (cdr old) (cdr list)))))))

(defun set-difference (list1 list2 &rest stuff &key test test-not key &aux result)
  "Return the difference of LIST1 minus LIST2, regarded as sets.
Any element of LIST1 which matches no element of LIST2 is included.
If the first argument has no duplicate elements, neither does the value.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (cond ((null stuff)
         (dolist (x list1)
           (unless (member-eql x list2)
             (push x result))))
        ((and (null test-not)
              (null key)
              (or (eq test #'eq) (eq test 'eq)))
         (dolist (x list1)
           (unless (memq x list2)
             (push x result))))
        ((and test test-not)
         (ferror "Both ~S and ~S specified" :test :test-not))
        (t
         (dolist (x list1)
           (unless (member-1 (if key (funcall key x) x) list2 test test-not key)
             (push x result)))))
  result)


(defun nset-difference (list1 list2 &rest stuff &key test test-not key)
  "Destructively modify LIST1 to be the LIST1 minus LIST2, regarded as sets.
Any element of LIST1 which matches an element of LIST2 is deleted.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (do ((list list1 (cdr list))
       (result)
       (old))
      ((null list) result)
    (cond ((not (if (null stuff)
                    (member-eql (car list) list2)
                  (member-1 (if key (funcall key (car list)) (car list))
                            list2 test test-not key)))
           (or result (setq result list))
           (setq old list))
          (old
           (setf (cdr old) (cdr list))))))

(defun set-exclusive-or (list1 list2 &rest stuff &key test test-not key &aux result)
  "Return the differences between LIST1 and LIST2, regarded as sets.
Any element of either list which matches nothing in the other list is in the result.
If neither argument has duplicate elements, neither does the value.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (cond ((null stuff)
         (dolist (x list1)
           (unless (member-eql x list2)
             (push x result)))
         (dolist (x list2)
           (unless (member-eql x list1)
             (push x result))))
        ((and (null test-not)
              (null key)
              (or (eq test #'eq) (eq test 'eq)))
         (dolist (x list1)
           (unless (memq x list2)
             (push x result)))
         (dolist (x list2)
           (unless (memq x list1)
             (push x result))))
        ((and test test-not)
         (ferror "Both ~S and ~S specified" :test :test-not))
        (t
         (dolist (x list1)
           (unless (member-1 (if key (funcall key x) x) list2 test test-not key)
             (push x result)))
         (dolist (x list2)
           (unless (member-1 (if key (funcall key x) x) list1 test test-not key)
             (push x result)))))
  result)

(defun nset-exclusive-or (list1 list2 &rest stuff &key test test-not key)
  "Destructively return the differences between LIST1 and LIST2, regarded as sets.
Any element of either list which matches nothing in the other list is in the result.
If neither argument has duplicate elements, neither does the value.
Both arguments can be chewed up in producing the result.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (when (and test test-not)
    (ferror "Both ~S and ~S specified" :test :test-not))
  (do* ((list list1 savecdr)
        (result)
        removed
        (savecdr (cdr list) (cdr list))
        (old))
       ((null list)
        (nconc result (apply #'nset-difference list2 removed stuff)))
    (cond ((not (if (null stuff)
                    (member-eql (car list) list2)
                  (member-1 (if key (funcall key (car list)) (car list))
                            list2 test test-not key)))
           (or result (setq result list))
           (setq old list))
          (t
           (if old (setf (cdr old) (cdr list)))
           (setf (cdr list) removed)
           (setq removed list)))))

(defun subsetp (list1 list2 &rest stuff &key test test-not key)
  "T if every element of LIST1 matches some element of LIST2.
TEST is used to compare two elements; it returns T if they match.
 It defaults to EQL.
 Alternately, specify TEST-NOT, a function to return NIL if they match.
If KEY is non-NIL, it is a function to apply to each element
 to get a key which is then passed to TEST or TEST-NOT instead of the element."
  (cond ((null stuff)
         (dolist (x list1 t)
           (unless (member-eql x list2)
             (return nil))))
        ((and (null test-not)
              (null key)
              (or (eq test #'eq) (eq test 'eq)))
         (dolist (x list1 t)
           (unless (memq x list2)
             (return nil))))
        ((and test test-not) (ferror "Both ~S and ~S specified" :test :test-not))
        (t
         (dolist (x list1 t)
           (unless (member-1 (if key (funcall key x) x)
                             list2 test test-not key)
             (return nil))))))

(defsubst acons (key datum alist)
  "(CONS (CONS KEY DATUM) ALIST). I don't know why this deserves a function of its own."
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional starting-alist)
  "(NCONC (MAPCAR #'CONS KEYS DATA) STARTING-ALIST)"
  (nconc (mapcar #'cons keys data) starting-alist))

;;;; Random commonlisp stuff

(defun lisp-implementation-type ()
  "Return the generic name of this Common Lisp implementation."
  "Zetalisp")                   ;Any lispmachine lisp is zetalisp.  If not, we're in trouble.

(defun lisp-implementation-version ()
  "Return a string that identifies the version of this particular implementation of Lisp."
  (with-output-to-string (version)
    (do ((sys patch-systems-list (cdr sys)))
        ((null sys))
      (let ((system (car sys)))
        (format version "~A ~D.~D"
                (patch-name system)
                (patch-version system)
                (version-number (first (patch-version-list system)))))
      (when (cdr sys) (send version :string-out ", ")))))
(deff software-version 'lisp-implementation-version)

(defun machine-type ()
  "Return the generic name for the hardware that we are running on, as a string.
For example, \"LAMBDA\"."
  (select-processor
    (:cadr "CADR")
    (:lambda "LAMBDA")
    (:explorer "EXPLORER")
    (:falcon "FALCON")))

(defun machine-version ()
  "Return a string that identifies which hardware and special microcode we are using."
  (format nil "~A, Microcode ~D" (machine-type) %microcode-version-number))

(defun machine-instance ()
  "Return a string that identifies which particular machine this implementation is."
  disk-pack-name)                               ;this is pretty much it

(defun software-type ()
  "Return the generic name of the host software, as a string."
  "Zetalisp")

(defun short-site-name ()
  "Return the abbreviated name for this site as a string, or NIL if we don't know it."
  (get-site-option :short-site-name))

(defun long-site-name ()
  "Return the long name for this site as a string, or NIL if we don't know it."
  (or (get-site-option :long-site-name) (short-site-name)))
