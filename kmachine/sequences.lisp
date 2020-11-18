;;; -*- Mode:LISP; Package:seq; Readtable:CL; Base:10; Lowercase:T -*-
;;;
;;; Edited by Youcef Bennour.
;;;
;;; Functions
;;; CONCATENATE COPY-SEQ COUNT COUNT-IF COUNT-IF-NOT DELETE DELETE-DUPLICATES
;;; DELETE-IF DELETE-IF-NOT ELT EVERY FILL FIND FIND-IF FIND-IF-NOT LENGTH
;;; MAKE-SEQUENCE MAP MERGE MISMATCH NOTANY NOTEVERY NREVERSE NSUBSTITUTE
;;; NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT POSITION POSITION-IF POSITION-IF-NOT
;;; REDUCE REMOVE REMOVE-DUPLICATES REMOVE-IF REMOVE-IF-NOT REPLACE REVERSE
;;; SEARCH SOME SORT STABLE-SORT SUBSEQ SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT
;;;
;;;
;;; Primitives for fetching elements sequentially from either lists or arrays.
;;; You use an index variable which contains an array index if the
;;; sequence is an array, or a tail if the sequence is a list.

(defprop check-sequence-args t :error-reporter)
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
       (aref-1-dim ,sequence ,indexvar)
     (car ,indexvar)))

(defmacro seq-fetch-inc (sequence indexvar)
  `(if (fixnump ,indexvar)
       (aref-1-dim ,sequence (prog1 ,indexvar (incf ,indexvar)))
     (pop ,indexvar)))

;;>>  (Wouldn't it be nice to have a machine in which (identity x) were fast???)
(defmacro key-fetch (key sequence indexvar)
  `(let ((tem (if (fixnump ,indexvar)
                  (aref-1-dim ,sequence ,indexvar)
                (car ,indexvar))))
     (if ,key (funcall ,key tem) tem)))

(defmacro key-fetch-inc (key sequence indexvar)
  `(let ((tem (if (fixnump ,indexvar)
                  (aref-1-dim ,sequence (prog1 ,indexvar (incf ,indexvar)))
                (pop ,indexvar))))
     (if ,key (funcall ,key tem) tem)))

(defmacro seq-store (sequence indexvar value)
  `(if (fixnump ,indexvar)
       (setf (aref-1-dim ,sequence ,indexvar) ,value)
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

;;; Macro used by merge and I am sure some other functions
;;;

(defmacro apply-predicate-and-key (lpred keyfun arg1 arg2)
  `(cond ((null ,keyfun) (funcall ,lpred ,arg1 ,arg2))
         ((eq ,keyfun #'car) (funcall ,lpred (car ,arg1) (car ,arg2)))
         (t (funcall ,lpred (funcall ,keyfun ,arg1)
                     (funcall ,keyfun ,arg2)))))


(defun zl:make-sequence (type size &key (initial-element nil initp))
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
                      (make-array size :initial-value initial-element)
                      (make-array size)))
                 ((and (memq (car-safe xtype) '(array simple-array))
                       (null (cddr xtype)))
                  (if initp
                      (make-array size :element-type (cadr xtype)
                                       :initial-element initial-element)
                      (make-array size :element-type (cadr xtype))))
                 (t
                  (ferror "Invalid sequence type ~S." type)))))))

(defun @make-sequence (type size &key initial-element)
  "Returns a sequence of SIZE elements, of type TYPE.
Each element is set to INITIAL-ELEMENT.
TYPE must be equivalent to either LIST or some sort of ARRAY.
If the value is a list, it is completely cdr-coded."
;  (@check-data-type size FIXNUM)
  (if (>= size 0) NIL (error nil "~S is not a positive FIXNUM" size))
  (cond ((eq type 'list)
         (make-list size :initial-element initial-element))
        ((or (memq type '(array simple-array vector simple-vector))
             (and (consp type)
                  (memq (car type) '(array simple-array vector simple-vector))
                  (equal (cadr type) '(t))))
         (make-array size :initial-element initial-element))
        ((memq type '(string simple-string))
         (make-string size :initial-element initial-element))
        (t
         (let ((xtype (type-canonicalize type nil nil)))
           (cond ((eq xtype 'list)
                  (make-list size :initial-element initial-element))
                 ((or (memq xtype '(array simple-array))
                      (and (consp xtype) (memq (car xtype) '(array simple-array))
                           (equal (cdr xtype) '(t))))
                  (make-array size :initial-value initial-element))
                 ((and (consp xtype)
                       (memq (car xtype) '(array simple-array))
                       (null (cddr xtype)))
                      (make-array size :element-type (cadr xtype)
                                       :initial-element initial-element))
                 (t
                  (ferror "Invalid sequence type ~S." type)))))))


(defun elt (sequence index)
  "Returns element at position index in sequence. Sequence must be a vector or a list."
  (etypecase sequence
    (vector (aref sequence index))
    (list (nth index sequence))
    )
  )


;; map function for sequences

(defun mapping-with-result (result-type function sequence &rest sequences)
  "Does the same thing as map except that it returns a sequence of type
RESULT-TYPE of all the results that are returned by applying FUNCTION to the
first element of SEQUENCE and first elements of all the sequence in SEQUENCES,
then to the second element and so on...."
  (let ((any-array-seq? nil)
        length-of-result
        result)
    ;; initialize length-of-result.
    (setq length-of-result (if (arrayp sequence)
                               (prog1 (array-total-size sequence)
                                      (setq any-array-seq? T))
                             (list-length sequence)))
    ;; compute the length of result.
    (dolist (seq sequences)
      (if length-of-result
          (if (arrayp seq)
              (setq length-of-result (min (array-total-size seq) length-of-result)
                    any-array-seq? T)
            (if (nthcdr length-of-result seq)
                ;; present seq which is a list is longer that some of the previously
                ;; seen.
                NIL
              ;; otherwise it is shorter. Take it as the length of the sequence to return
              (setq length-of-result (length seq))))
        (setq length-of-result (if (arrayp sequence)
                                   (prog1 (array-total-size sequence)
                                          (setq any-array-seq? T))
                                 (list-length sequence))))
      )
    ;; If length-of-result is NIL, we will caught in an infinite loop.
    ;; override result-type and just make it a list.
    (or length-of-result (setq result-type 'LIST))
    (cond (any-array-seq?
           ;; we have seen some one-dimensional arrays used as sequences.
           ;; length-of-result is finite. Result can be a list or an array.
           (do* ((index 0 (1+ index))
                 (result-sequence (make-sequence result-type length-of-result))
                 (temp (and (listp result-sequence) result-sequence))
                 (arg-for-function (make-list (1+ (length sequences))))
                 (other-args sequences))
                ((= index length-of-result) result-sequence)
             (setf (car arg-for-function)
                   (if (arrayp sequence)
                       (aref-1-dim sequence index)
                     (prog1 (car sequence) (setq sequence (cdr sequence)))))
             ;; get the other args for the next function call.
             (do ((next-arg other-args (cdr next-arg))
                  (next-function-arg (cdr arg-for-function) (cdr next-function-arg))
                  element)
                 ((null next-arg))
               (setf (car next-function-arg)
                     (if (arrayp (setq element (car next-arg)))
                         (aref-1-dim element index)
                       (prog1 (car element) (setf (car next-arg) (cdr element))))))
             ;; all arguments have been collected.
             ;; now compute and save partial result in result-sequence.
             (if temp
                 (progn (setf (car temp) (apply function arg-for-function))
                        (setq temp (cdr temp)))
               ;; result is a one dimensional array, use index for next element.
               (aset-1-dim (apply function arg-for-function) result-sequence index)))
           )
          ((eq result-type 'LIST)
           ;; call MAPCAR since all the sequence are list and the result to return is
           ;; a list.
           (if (null sequences)
               (funcall #'MAPCAR function sequence)
             (apply #'MAPCAR function sequence sequences)))
          (t
           ;; we have not seen an array to be as a sequence, but the result is to be
           ;; an array sequence.
           (setq result (if (null sequences)
                            (funcall #'MAPCAR function sequence)
                          (apply #'MAPCAR function sequence sequences)))
           ;; if we returned from MAPCAR, it means that everything was ok. and the result is
           ;; of finite length
           (do ((index 0 (1+ index))
                (seq result (cdr seq))
                (array-sequence (make-sequence result-type length-of-result)))
               ((null seq) array-sequence)
             (aset-1-dim (car seq) array-sequence index))))
    )
  )

(defun map (result-type function sequence &rest sequences)
  "Maps over successive elements of each SEQUENCE, returns a sequence of the results.
FCN is called first on the 0'th elements of all the sequences,
then on the 1st elements of all, and so on until some argument sequence is exhausted.
The values returned by FCN are put into a result sequence which is returned by MAP.
RESULT-TYPE is a sequence type; the result is of that type.
Or RESULT-TYPE can be NIL, meaning call FCN for effect only,
throw away the values, and return NIL."
  ;; First are we to return anything?
  (if result-type
      (apply #'mapping-with-result result-type function sequence sequences)
    ;; otherwise no result is to be returned.
    ;; The function is used for its side effects.
    (cond ((null sequences)
           ;; only one argument passed.
           (if (listp sequence)
               (funcall #'mapc function sequence)
             ;; the sequence is a one dimensional array.
             (do ((index 0 (1+ index))
                  (number-of-times (array-total-size sequence)))
                 ((= index number-of-times))
               (funcall function (aref-1-dim sequence index)))))
          (t
           ;; more than one argument is passed to function.
           ;; Circular list are not checked for. So watch for that users!!!!
           ;; More hair since sequences can be lists and 1-dim-arrays.
           (let ((shortest-length (if (arrayp sequence)
                                      (array-total-size sequence)
                                    (list-length sequence))))
             (dolist (seq sequences)
               (if (arrayp seq)
                   (setq shortest-length
                         (if shortest-length
                             (min shortest-length (array-total-size seq))
                           (array-total-size seq)))
                 ;; otherwise the sequence is a list.
                 (if shortest-length
                     (if (nthcdr shortest-length seq)
                         ;; seq is longer than some previously seen sequence. Ignore it
                         NIL
                       ;; otherwise, the present seq is shorter than all those seen previously.
                       ;; set the shortest-length to it.
                       (setq shortest-length (length seq)))
                   ;; shortest length is NIL, what out for circular list here.
                   (setq shortest-length (list-length seq)))))
             ;; At this point, If shortest-length is still NIL, all sequences are circular lists
             ;; Same hack, call mapc
             (if (not shortest-length)
                 (apply #'mapc function sequence sequences)
               ;; We have a shortest-length.
               (do ((arg (make-list (1+ (length sequences))))
                    (arrayp-first-arg (arrayp sequence))
                    (index 0 (1+ index))
                    (other-args sequences))
                   ((= index shortest-length))
                 ;; collect argument to function
                 (setf (car arg)
                       (if arrayp-first-arg
                           (aref-1-dim sequence index)
                         (prog1
                           (car sequence)
                           (setq sequence (cdr sequence)))))
                 ;; now the other arguments
                 (do ((next-args other-args (cdr next-args))
                      (arg-for-function (cdr arg) (cdr arg-for-function))
                      temp)
                     ((null next-args))
                   (setf (car arg-for-function)
                         (if (arrayp (setq temp (car next-args)))
                             (aref temp index)
                           (prog1 (car temp)
                                  (setf (car next-args) (cdr temp))))))
                 ;; apply function here on arg list
                 (apply function arg)))))
          )
    )
  )

(defun zl:map (result-type function &rest sequences)
  "Maps over successive elements of each SEQUENCE, returns a sequence of the results.
FCN is called first on the 0'th elements of all the sequences,
then on the 1st elements of all, and so on until some argument sequence is exhausted.
The values returned by FCN are put into a result sequence which is returned by MAP.
RESULT-TYPE is a sequence type; the result is of that type.
Or RESULT-TYPE can be NIL, meaning call FCN for effect only,
throw away the values, and return NIL."
  (apply #'map result-type function sequences)
  )

(defun merge (result-type sequence1 sequence2 predicate &key key)
  "Return a single sequence containing the elements of SEQUENCE1 and SEQUENCE2 interleaved.
The interleaving is done by taking the next element of SEQUENCE1 unless
the next element of SEQUENCE2 is \"less\" than it according to PREDICATE.
KEY, if non-NIL, is applied to each element to get the object to
pass to PREDICATE, rather than the element itself.
RESULT-TYPE specifies the type of sequence returned."
  (@check-data-type sequence1 sequence)
  (@check-data-type sequence2 sequence)
  (let ((index-or-tail-1 (seq-start sequence1))
        (index-or-tail-2 (seq-start sequence2))
        (end1 (seq-end sequence1))
        (end2 (seq-end sequence2))
        (result-length (+ (length sequence1) (length sequence2))))
    (do* ((result (make-sequence result-type result-length))
          (store-index 0 (1+ store-index))
          (temp (if (listp result) result))
          val)
        ((= store-index result-length)
         result)
      (setq val (cond ((eq index-or-tail-1 end1)
                       (seq-fetch-inc sequence2 index-or-tail-2))
                      ((eq index-or-tail-2 end2)
                       (seq-fetch-inc sequence1 index-or-tail-1))
                      (t
                       (let ((e1 (seq-fetch sequence1 index-or-tail-1))
                             (e2 (seq-fetch sequence2 index-or-tail-2)))
                         (cond ((apply-predicate-and-key predicate key e2 e1)
                                (seq-inc index-or-tail-2) e2)
                               (t
                                (seq-inc index-or-tail-1) e1))))))
      (if temp
          (progn
            (setf (car temp) val)
            (setq temp (cdr temp)))
          (aset val result store-index))))
  )


(defun subseq (sequence start &optional end)
  "Return a subsequence of SEQUENCE; a new sequence containing some of SEQUENCE's elements.
If SEQUENCE is a list, the value is also a list;
if it is an array, the value is an array of the same type.
START and END are the indices in SEQUENCE of the desired subsequence.
If END is NIL, it means the end of SEQUENCE."
  (ctypecase sequence
    (vector
     (let ((len (length sequence))
           vallen result-sequence)
       (or (and (or (null end) ( end start))
                ( start 0))
           (ferror "Args ~S and ~S out of range for ~S" start end sequence))
       (setq start (min len start)
             end (min (or end len) len)
             vallen (- end start)
             result-sequence (make-array vallen :element-type (array-element-type sequence)))
       (copy-array-portion sequence start end result-sequence 0 vallen)
       result-sequence))
    (list
     (if end
         (firstn (- end start) (nthcdr start sequence))
         (nthcdr start sequence))))
  )

(defun copy-seq (sequence)
  "Return a new sequence with the same elements as SEQUENCE, and of the same type.
SEQUENCE may be a list or an array."
  (etypecase sequence
    (list (copy-list sequence))
    (vector
     (let* ((len (length sequence))
            (new (make-array len :element-type (array-element-type sequence))))
       (copy-array-portion sequence 0 len new 0 len)
       new))))


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

(defun replace-1 (into-sequence-1 from-sequence-2 &optional (start1 0) end1 (start2 0) end2)
  (or end1 (setq end1 (length into-sequence-1)))
  (or end2 (setq end2 (length from-sequence-2)))
  (if (eq into-sequence-1 from-sequence-2)
      (let* ((n-copy (min (- end2 start2) (- end1 start1)))
             (temp (make-list n-copy)))
        (replace-1 temp from-sequence-2 0 n-copy start2 end2)
        (replace-1 into-sequence-1 temp start1 end1 0 n-copy))
    (if (and (vectorp into-sequence-1) (vectorp from-sequence-2))
        (let ((n-copy (min (- end2 start2) (- end1 start1))))
          (copy-array-portion from-sequence-2 start2 (+ start2 n-copy)
                              into-sequence-1 start1 (+ start1 n-copy)))
      (let ((store-index (if (arrayp into-sequence-1) start1 (nthcdr start1 into-sequence-1)))
            (store-end (if (arrayp into-sequence-1) end1 (nthcdr end1 into-sequence-1)))
            (fetch-end (if (arrayp from-sequence-2) end2 (nthcdr end2 from-sequence-2)))
            (fetch-index (if (arrayp from-sequence-2) start2 (nthcdr start2  from-sequence-2))))
        (do ()
            ((or (eq store-index store-end)
                 (eq fetch-index fetch-end)))
          (seq-store into-sequence-1 store-index (seq-fetch-inc from-sequence-2 fetch-index))
          (seq-inc store-index)))))
  into-sequence-1
  )

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
  (@check-data-type into-sequence-1 sequence)
  (@check-data-type from-sequence-2 sequence)
  (replace-1 into-sequence-1 from-sequence-2 start1 end1 start2 end2)
  )




(defun zl:reduce (function sequence &key from-end (start 0) end
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
                  (let ((accum (if initial-value
                                   (funcall function (cl:aref sequence (decf seq-end))
                                                     initial-value)
                                 (decf seq-end 2)
                                 (funcall function (cl:aref sequence seq-end)
                                                   (cl:aref sequence (1+ seq-end))))))
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

;;;
;;; replace cl:aref by aref-1-dim
;;;

(defun reduce (function sequence &key from-end (start 0) end (initial-value nil initp) &aux tem)
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
                  (let ((accum (if initial-value
                                   (funcall function (aref sequence (decf seq-end))
                                                     initial-value)
                                 (decf seq-end 2)
                                 (funcall function (aref sequence seq-end)
                                                   (aref sequence (1+ seq-end))))))
                  (do ((index (1- seq-end) (1- index)))
                      ((< index start)
                       accum)
                      (setq accum (funcall function (aref sequence index) accum)))))
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
               (setq accum (funcall function accum (seq-fetch-inc sequence index))))))))
  )



(defun sequence-length (s)
  (etypecase s
    (list (or (list-length s) (ferror "~S is circular" s)))
    (vector (length s)))
  )

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
      (setf (aref array) sequence)
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
                                   (+ (* array-index stop-i) i)))))
  )



(defun *-internal (predicate test-for-nil? invert-result? sequence &rest sequences &aux test)
  "Is called by some, notevery, notany and every. Is here for the purpose
to not use too much memory space since all the callers share a large portion
of code."
  (setq test (if test-for-nil? #'(lambda (&rest args) (not (apply predicate args))) predicate))
  (if (null sequences)
      ;; only one arguments for predicate.
      (etypecase sequence
        (vector
         (do ((index 0 (1+ index))
              (max-size (length sequence))
              (temp))
             ((or (= max-size index)
                  (setq temp (funcall test (aref sequence index))))
              (if invert-result? (not temp) temp))))
        (cons
         (do ((seq sequence (cdr seq))
              (temp))
             ((or (null sequence)
                  (setq temp (funcall test (car seq))))
              (if invert-result? (not temp) temp))))
        (null NIL))
    ;; otherwise more than one argument is passed to test
    (do ((arg (make-list (1+ (length sequences))))
         (index 0 (1+ index))
         (first-arg sequence)
         (other-args sequences)
         (done-flag? NIL)
         (done-flag-delayed? NIL)
         (first-time T nil)
         (temp))
        ((or done-flag? done-flag-delayed? (if first-time NIL temp))
         (if invert-result? (not temp) temp))
      ;; collect the args for the next call.
      (setf (car arg)
            (if (arrayp sequence)
                (if (= (length sequence) index)
                    (setq done-flag? T)
                  (aref first-arg index))
              (prog1
                (car first-arg)
                (setq first-arg (cdr first-arg)
                      done-flag-delayed? (not first-arg)))))
      ;; now other arguments
      (do ((other-arg-tail other-args (cdr other-arg-tail))
           (next-arg-for-call (cdr arg) (cdr next-arg-for-call))
           next-arg)
          ((null other-arg-tail))
        (setf (car next-arg-for-call)
              (if (arrayp (setq next-arg (car other-arg-tail)))
                  (if (= (array-total-size next-arg) index)
                      (setq done-flag? T)
                    (aref next-arg index))
                (prog1
                  (car next-arg)
                  (setf (car other-arg-tail) (cdr next-arg))
                  (or done-flag-delayed? (setq done-flag-delayed? (not (cdr next-arg))))))))
      ;; now apply test on list of arguments if done-flag? is not set.
      (or done-flag?
          (setq temp (apply test arg)))))
  )

(defun some (predicate sequence &rest sequences)
  "Applies PREDICATE to successive elements of of SEQUENCE and SEQUENCES;
if it returns non-NIL, so does SOME. PREDICATE gets one argument from each sequence;
first element 0 of each sequence, then element 1, and so on.
If PREDICATE returns non-NIL, the value it returns is returned by SOME.
If one of the sequences is exhausted, SOME returns NIL."
  (apply #'*-internal predicate nil nil sequence sequences)
  )

(defun zl:some (predicate &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCES; if it returns non-NIL, so does SOME.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns non-NIL, the value it returns is returned by SOME.
If one of the sequences is exhausted, SOME returns NIL."
  (apply #'*-internal predicate nil nil (car sequences) (cdr sequences))
  )



(defun notevery (predicate sequence &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCE and SEQUENCES;
T if PREDICATE ever returns NIL. PREDICATE gets one argument from each sequence;
first element 0 of each sequence, then element 1, and so on.
If PREDICATE returns NIL, then NOTEVERY returns T.
If one of the sequences is exhausted, NOTEVERY returns NIL."
  (apply #'*-internal predicate T NIL sequence sequences)
  )

(defun zl:notevery (predicate &rest sequences)
    "Applies PREDICATE to successive elements of SEQUENCES; T if PREDICATE ever returns NIL.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns NIL, then NOTEVERY returns T.
If one of the sequences is exhausted, NOTEVERY returns NIL."
  (apply #'*-internal predicate T NIL (car sequences) (cdr sequences))
  )


(defun notany (predicate sequence &rest sequences)
  "Applies PREDICATE to successive elements of sequence and SEQUENCES;
T if PREDICATE always returns NIL. PREDICATE gets one argument from each sequence;
first element 0 of each sequence, then element 1, and so on.
If PREDICATE returns non-NIL, then NOTANY returns NIL.
If one of the sequences is exhausted, NOTANY returns T."
  (apply #'*-internal predicate NIL T sequence sequences)
  )

(defun zl:notany (predicate &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCES; T if PREDICATE always returns NIL.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns non-NIL, then NOTANY returns NIL.
If one of the sequences is exhausted, NOTANY returns T."
  (apply #'*-internal predicate NIL T (car sequences) (cdr sequences))
  )


(defun every (predicate sequence &rest sequences)
  "Applies PREDICATE to successive elements of sequence and SEQUENCES;
T if PREDICATE always returns T. PREDICATE gets one argument from each sequence;
first element 0 of each sequence, then element 1, and so on.
If PREDICATE returns NIL, then EVERY returns NIL.
If one of the sequences is exhausted, EVERY returns T."
  (apply #'*-internal predicate T T sequence sequences)
  )

(defun zl:every (predicate &rest sequences)
  "Applies PREDICATE to successive elements of SEQUENCES; T if PREDICATE always returns T.
PREDICATE gets one argument from each sequence; first element 0
of each sequence, then element 1, and so on.
If PREDICATE returns NIL, then EVERY returns NIL.
If one of the sequences is exhausted, EVERY returns T."
  (apply #'*-internal predicate T T (car sequences) (cdr sequences))
  )



(defun get-tail (item start-tail key test end-tail one-arg-predicate &aux elt (last-cell nil))
  (do ((LIST start-tail (cdr LIST)))
       ((eq LIST end-tail) NIL)
    (setq elt (if key (funcall key (car LIST)) (car LIST)))
    (if (cond (one-arg-predicate
               (funcall one-arg-predicate elt))
              (test
               (funcall test item elt))
              (t (eql item elt)))
        (return (or last-cell LIST))
      (setq last-cell LIST)))
  )

(defun count-1 (item sequence &optional (start 0) end test
                invertp key one-arg-predicate &aux tst)
  (setq tst (if invertp #'(lambda (&rest args) (not (apply test args))) test))
  (do ((index start (1+ index))
       (count 0)
       (stop-index (or end (length sequence)))
       (List (and (listp sequence) sequence))
       elt)
      ((= index stop-index) count)
    (setq elt (cond (LIST
                     (prog1
                       (car LIST)
                       (setq LIST (cdr LIST))))
                    (t
                     ;; it is an array
                     (aref sequence index))))
    (and key (setq elt (funcall key elt)))
    (and (cond (one-arg-predicate
                (funcall one-arg-predicate elt))
               (tst
                (funcall tst item elt))
               (t (eql item elt)))
         (incf count)))
  )

(defun remove-from-array (item vector start end count test invertp key from-end one-arg-predicate &rest ignore)
  (or end (setq end (length vector)))
  (or count (setq count (length vector)))
  ;; collect all the indices of element to be removed.
  (let ((temp nil)
        (start-index (if from-end (1- end) start))
        (stepping-index (if from-end #'1- #'1+))
        (partial-stopping-test-function (if from-end #'< #'>=))
        (second-arg (if from-end start end))
        (number-of-elements-collected 0)
        result elt)
    (do ((index start-index (funcall stepping-index index)))
        ((or (funcall partial-stopping-test-function index second-arg)
             (>= number-of-elements-collected count))
         (or from-end (setq temp (nreverse temp))))
      (setq elt (if key (funcall key (aref vector index)) (aref vector index)))
      (when (eq invertp (not (cond (one-arg-predicate
                                    (funcall one-arg-predicate elt))
                                   (test (funcall test elt item))
                                   (t (eql elt item)))))
        (setq temp (cons index temp))
        (incf number-of-elements-collected)))
    ;; if temp is nil then nothing has been found and just return the same array.
    (if (null temp)
        vector
      ;; some elements have been found, we must then delete them from the sequence.
      ;; Now TEMP contains the indices of the elements to be removed, in ascending order.
      (setq result (make-array (- (length vector) number-of-elements-collected)
                                     :type (array-type vector)))
      (do* ((start-from 0 (1+ (car tmp)))
            (tmp temp (cdr tmp))
            (to-from (car tmp) (or (car tmp) (length vector)))
            (start-dest 0 (+ start-dest number-of-elements))
            (number-of-elements (- to-from start-from) (- to-from start-from)))
           ((null tmp)
            ;; remainding elements in vector have to be copied over to result
            (copy-array-portion vector start-from to-from result start-dest (length result)))
        (copy-array-portion vector start-from to-from result start-dest (+ start-dest number-of-elements))))
      result)
  )

(defun remove-from-list (item LIST start end count test invertp
                         key from-end one-arg-predicate
                         &optional destructivep &aux (skip-count 0) tst)
  (or count (setq count most-positive-fixnum))
  (or end (setq end (length list)))
  (setq tst (if invertp #'(lambda (&rest args) (not (apply test args))) test))
  (when from-end
    (unless (>= count (- end start))
      (setq skip-count (max 0 (- (count-1 item list start end test invertp key) count)))))
  (if (and (plusp count)
           (or (null end) (> end start)))
      (do* ((result (if destructivep LIST (copy-list LIST)))
            (end-tail (nthcdr (- end start) result))
            (start-tail (get-tail item (nthcdr start result) key tst end-tail one-arg-predicate)
                        (get-tail item start-tail key tst end-tail one-arg-predicate))
            )
          ((or (eq start-tail end-tail)
               (null start-tail)) RESULT)
        ;; start-tail has the car which is the element to remove.
        ;; make sure, you don't delete any until the skip-count is negative
        (when (minusp (decf skip-count))
          ;; ok delete it now.
          ;; we need a pointer to the last cell before the one to delete.
          (if (eq result start-tail)
              (setq result (cdr result)
                    start-tail result)
            (setf (cdr start-tail) (cddr start-tail))))
        )
    LIST)
  )


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
           nil sequence start end count nil nil key from-end predicate)
  )

(defun remove-if-not (predicate sequence &key (start 0) end count key from-end)
  "Like REMOVE-IF but removes elements which do not satisfy PREDICATE."
  (funcall (etypecase sequence
             (list #'remove-from-list)
             (vector #'remove-from-array))
           nil sequence start end count nil t key from-end predicate)
  )

(defun remove (item sequence &key test test-not (start 0) end count key from-end)
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
  (funcall (etypecase sequence
             (list #'remove-from-list)
             (vector #'remove-from-array))
           item sequence start end count (or test-not test) (not (null test-not))
           key from-end nil)
  )

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
           nil sequence start end count nil nil key from-end predicate t)
  )

(defun delete-if-not (predicate sequence &key (start 0) end count key from-end)
  "Like DELETE-IF but deletes elements which do not satisfy PREDICATE."
  (funcall (etypecase sequence
             (list #'delete-from-list)
             (vector #'delete-from-array))
           nil sequence start end count nil t key from-end predicate t)
  )

(defun delete (item sequence &key test test-not (start 0) end count key from-end)
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
  (funcall (etypecase sequence
             (list #'delete-from-list)
             (vector #'delete-from-array))
           item sequence start end count (or test-not test) (not (null test-not))
           key from-end nil t)
  )

(deff delete-from-array 'remove-from-array)


(defun find-1 (item sequence &optional (start 0) end test invertp key from-end one-arg-predicate)
  (or end (setq end (length sequence)))
  (let ((starting-from (if from-end (1- end) start))
        (stepping-function (if from-end #'1- #'1+))
        (second-arg (if from-end start end))
        (stop-function (if from-end #'< #'>=)))
    (do ((index starting-from (funcall stepping-function index))
         elt-to-return)
        ((funcall stop-function index second-arg) NIL)
      (setq elt-to-return (elt sequence index))
      (when (eq invertp (not (cond (one-arg-predicate
                                    (funcall one-arg-predicate
                                             (if key (funcall key elt-to-return)
                                               elt-to-return)))
                                   (test
                                    (funcall test (if key (funcall key elt-to-return)
                                                    elt-to-return)
                                             item))
                                   (t (eql (if key (funcall key elt-to-return)
                                               elt-to-return)
                                           item)))))
        (return (values elt-to-return index)))))
  )

(defun get-new-tail (list1 list2)
  ;; Returns a tail of list1 whose cdr is list2. If list1 and list2 are
  ;; eq then it returns list1.
  (if (eq list1 list2)
      list1
    (do ((list list1 (cdr list)))
        ((or (null list)
             (eq (cdr list) list2))
         LIST))
    )
  )

(defun get-previous-tail (starting-tail ending-tail elt)
  ;; returns a tail of starting-tail whose second element is elt.
  ;; if starting-tail is eq to ending-tail, it returns starting-tail.
  ;; if first element of starting tail is eql to elt, it returns starting-tail.
  (if (or (eq starting-tail ending-tail)
          (eql (car starting-tail) elt))
      starting-tail
    (do ((LIST starting-tail (cdr LIST)))
        ((or (eql (cadr list) elt)
             (eq list ending-tail))
         LIST)
      ))
  )

(defun remove-duplicates-from-array (vector start end test invertp key from-end &rest ignore)
  (or end (setq end (length vector)))
  ;; collect all the indices of element to be removed.
  (let ((temp nil)
        (start-index (if from-end (1- end) start))
        (stepping-index (if from-end #'1- #'1+))
        (partial-stopping-test-function (if from-end #'< #'>=))
        (second-arg (if from-end start end))
        (number-of-elements-collected 0)
        result elt ind)
    (do ((index start-index (funcall stepping-index index)))
        ((funcall partial-stopping-test-function index second-arg)
         (or from-end (setq temp (nreverse temp))))
      (setq elt (if key (funcall key (aref vector index)) (aref vector index)))
      (multiple-value-setq (nil ind)
        (if from-end
            (find-1 elt vector start index test invertp key)
          (find-1 elt vector (1+ index) end test invertp key)))
      (when ind
        (setq temp (cons index temp))
        (incf number-of-elements-collected)))
    ;; if temp is nil then nothing has been found and just return the same array.
    (if (null temp)
        vector
      ;; some elements have been found, we must then delete them from the sequence.
      ;; Now TEMP contains the indices of the elements to be removed, in ascending order.
      (setq result (make-array (- (length vector) number-of-elements-collected)
                                     :type (array-type vector)))
      (do* ((start-from 0 (1+ (car tmp)))
            (tmp temp (cdr tmp))
            (to-from (car tmp) (or (car tmp) (length vector)))
            (start-dest 0 (+ start-dest number-of-elements))
            (number-of-elements (- to-from start-from) (- to-from start-from)))
           ((null tmp)
            ;; remainding elements in vector have to be copied over to result
            (copy-array-portion vector start-from to-from result start-dest (length result)))
        (copy-array-portion vector start-from to-from result start-dest (+ start-dest number-of-elements))))
      result)
  )

;(defun delete-duplicates-from-list (list start end test invertp key from-end)
;  (if (or (null end) (> end start))
;      (let* ((tail (nthcdr start (variable-location list)))
;            (end-tail (and end (nthcdr (- end start) (cdr tail)))))
;       (do (elt index)
;           ((eq (cdr tail) end-tail))
;         (setq elt (if key (funcall key (cadr tail)) (cadr tail)))
;         (multiple-value-setq (nil index)
;           (if from-end
;                   (find-1 elt list 0 (cdr tail) test invertp key)
;                   (find-1 elt (cddr tail) 0 end test invertp key)))
;         (if index
;             (setf (cdr tail) (cddr tail))
;           (setq tail (cdr tail))))))
;  list)

;(defun remove-duplicates-from-list (list start end test invertp key from-end)
;  (let ((list-copy (copy-list (if end (firstn end list) list))))
;    (and end (setf (cdr (last list-copy)) (nthcdr end list)))
;    (setq list-copy (delete-duplicates-from-list list-copy start end test invertp key from-end))
;    (if (equal list-copy list) LIST list-copy))
;  )

(defun remove-duplicates-from-list (list start end test invertp key from-end &optional destructivep)
  (or end (setq end (length LIST)))
  (or test (setq test #'eql))
  (and invertp (setq test #'(lambda (&rest args) (not (apply test args)))))
  (if (> end start)
      (let* ((result (if destructivep LIST (copy-list (if end (firstn end LIST) LIST))))
             (starting-tail (nthcdr start result))
             (ending-tail (nthcdr (- end start) starting-tail)))
        (or destructivep (null end) (setf (cdr (last result)) (nthcdr end LIST)))
        (do* ((tail starting-tail (if (eql elt (car tail)) (cdr tail) tail))
              (previous-tail starting-tail (get-previous-tail
                                             (if (zerop start)
                                                 result
                                               starting-tail)
                                             ending-tail
                                             (car tail)))
              (elt (car tail) (car tail)))
             ((eq tail ending-tail))
          (and key (setq elt (funcall key elt)))
          (do ((rest-tail (cdr tail))
               (new-tail (get-tail elt (cdr tail) key test ending-tail nil)
                         (get-tail elt rest-tail key test ending-tail nil)))
              ((or (null new-tail)
                   (eq rest-tail ending-tail)))
            (if from-end
                ;; delete the element just found. It is either the first or second
                ;; element in new-tail.
                (progn
                  (if (eql (car new-tail) elt)
                      ;; it is the first one.
                      ;; use tail to delete it. It can never
                      ;; the first element of result since it has to be kept.
                      (progn
                        (setf (cdr tail) (cddr tail))
                        (setq rest-tail (cdr tail)))
                    (setf (cdr new-tail) (cddr new-tail))
                    (setq rest-tail new-tail)))
              ;; otherwise, delete the element found earlier in the list.
              ;; This element should be found as the first or second element
              ;; in previous-tail. If it is the first element, check to see
              ;; if previous-tail is eq to result, in which case result list
              ;; is updated to point to its cdr.
              (if (eq previous-tail result)
                  ;; check to see if previous-tail is the same as result
                  (if (eql elt (car previous-tail))
                      ;; delete the first element in result and update
                      ;; starting-tail and pevious-tail
                      (setq result (cdr result)
                            previous-tail (if (eql (car new-tail) elt)
                                              (get-new-tail result new-tail)
                                            new-tail)
                            starting-tail result
                            tail result
                            rest-tail (if (eql (car new-tail) elt) (cdr new-tail) (cddr new-tail)))
                    ;; otherwise previous-tail is pointing somewhere else
                    (setf (cdr previous-tail) (cddr previous-tail))
                    (setq previous-tail (if (eql (car new-tail) elt)
                                            (get-new-tail result new-tail)
                                          new-tail)
                          rest-tail (if (eql (car new-tail) elt) (cdr new-tail) (cddr new-tail))))
                (setf (cdr previous-tail) (cddr previous-tail))
                (setq previous-tail (if (eql (car new-tail) elt)
                                            (get-new-tail result new-tail)
                                          new-tail)
                          rest-tail (if (eql (car new-tail) elt) (cdr new-tail) (cddr new-tail)))))))
        result)
    list)
  )

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
  (funcall (etypecase sequence
             (list #'remove-duplicates-from-list)
             (vector #'remove-duplicates-from-array))
           sequence start end (or test-not test) (not (null test-not)) key from-end)
  )

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
  (funcall (etypecase sequence
             (list #'delete-duplicates-from-list)
             (vector #'delete-duplicates-from-array))
           sequence start end (or test-not test) (not (null test-not)) key from-end t)
  )

(deff delete-duplicates-from-array 'remove-duplicates-from-array)

;(defun remove-duplicates-from-list (list start end test invertp key from-end)
;  (if (or (null end) (> end start))
;      (loop with head = (variable-location list)
;           for start-tail = (nthcdr start list) then (cdr head)
;           with end-tail = (and end (nthcdr (- end start) start-tail))
;           as tail = (do ((l start-tail (cdr l))
;                          elt)
;                         ((eq l end-tail) l)
;                       (setq elt (if key (funcall key (car l)) (car l)))
;                       (when (nth-value 1
;                               (if from-end
;                                   (find-1 elt list 0 l test invertp key)
;                                   (find-1 elt (cdr l) 0 end-tail test invertp key)))
;                           (return l)))
;           until (eq tail end-tail)
;        do (loop until (eq (cdr head) tail)
;                 do (setf (cdr head) (setq head (cons (cadr head) (cddr head)))))
;           (setf (cdr head) (cdr tail))))
;  list)

;(defun remove-duplicates-from-list (list start end test invertp key from-end)
;  (if (or (null end) (> end start))
;      (do* ((head (variable-location list))
;           (start-tail (nthcdr start list) (cdr head))
;           (end-tail (and end (nthcdr (- end start) start-tail)))
;           (tail LIST))
;          ((eq tail end-tail) head)
;          (setq tail (do ((L start-tail (cdr L))
;                          elt index)
;                         ((eq L end-tail) L)
;                       (setq elt (if key (funcall key (car L)) (car L)))
;                       (multiple-value-setq (nil index)
;                         (if from-end
;                             (find-1 elt list 0 l test invertp key)
;                           (find-1 elt (cdr l) 0 end-tail test invertp key)))
;                       (when index
;                         (return L))))
;          (do ()
;              ((eq (cdr head) tail)
;               (setf (cdr head) (cdr tail)))
;            (setf (cdr head) (setq head (cons (cadr head) (cddr head)))))))
;  list
;  )

;(defun remove-duplicates-from-list (list start end test invertp key from-end)
;  (if (or (null end) (> end start))
;      (loop with head = (variable-location list)
;           for start-tail = (nthcdr start list) then (cdr head)
;           with end-tail = (and end (nthcdr (- end start) start-tail))
;           as tail = (do ((l start-tail (cdr l))
;                          elt)
;                         ((eq l end-tail) l)
;                       (setq elt (if key (funcall key (car l)) (car l)))
;                       (when (nth-value 1
;                               (if from-end
;                                   (find-1 elt list 0 l test invertp key)
;                                 (find-1 elt (cdr l) 0 end-tail test invertp key)))
;                         (return l)))
;           until (eq tail end-tail)
;           do (loop until (eq (cdr head) tail)
;                    do (setf (cdr head) (setq head (cons (cadr head) (cddr head)))))
;           (setf (cdr head) (cdr tail))))
;  list)

;(defun delete-duplicates-from-list (list start end test invertp key from-end)
;  (if (or (null end) (> end start))
;      (let* ((tail (nthcdr start (variable-location list)))
;            (end-tail (and end (nthcdr (- end start) (cdr tail)))))
;       (do (elt)
;           ((eq (cdr tail) end-tail))
;         (setq elt (if key (funcall key (cadr tail)) (cadr tail)))
;         (if (nth-value 1
;               (if from-end
;                   (find-1 elt list 0 (cdr tail) test invertp key)
;                   (find-1 elt (cddr tail) 0 end-tail test invertp key)))
;             (setf (cdr tail) (cddr tail))
;             (setq tail (cdr tail))))))
;  list)


;;;
;;; substitute in an array.
;;;

(defun substitute-in-array (copyflag newitem olditem sequence
                            start end count test invertp key from-end one-arg-predicate
                            &aux result)
  (or count (setq count (length sequence)))
  (or end (setq end (length sequence)))
  (or copyflag (setq result sequence))
  (unless result
    (setq result (make-array (length sequence) :type (array-type sequence)))
    (copy-array-contents sequence result))
  (do ((i (if from-end (1- end) start)
          (+ i inc))
       (inc (if from-end -1 1))
       (num-replaced 0)
       elt)
      ((or (if from-end (< i start) ( i end))
           ( num-replaced count)))
    (setq elt (if key (funcall key (aref sequence i)) (aref sequence i)))
    (when (eq invertp (not (cond (one-arg-predicate
                                  (funcall one-arg-predicate elt))
                                 (test
                                  (funcall test olditem elt))
                                 (t
                                  (eql olditem elt)))))

      (setf (aref result i) newitem)
      (incf num-replaced)))
  result
  )

;;;
;;; substitute in an array.
;;;

(defun substitute-in-list (copyflag newitem olditem list
                           start end count test invertp key from-end one-arg-predicate
                           &aux (skip-count 0) result)
  (or count (setq count most-positive-fixnum))
  (when from-end
    (unless (and end ( count (- end start)))
      (setq skip-count (max 0 (- (count-1 olditem list start end test invertp
                                          key one-arg-predicate)
                                 count)))))
  (if (and (plusp count)
           (or (null end) (> end start)))
      (progn
        (setq result (if copyflag
                   (copy-list (if end (firstn end list) LIST))
                 LIST))
        (or (equal LIST result) (setf (cdr (last result)) (nthcdr end list)))
        (do* ((start-tail (nthcdr start result) (cdr start-tail))
              (end-tail (and end (nthcdr (- end start) start-tail)))
              (tail (get-tail olditem start-tail key test end-tail one-arg-predicate)
                    (get-tail olditem start-tail key test end-tail one-arg-predicate)))
             ((or (null tail)
                  (eq end-tail tail)))
          (when (minusp (decf skip-count))
            (if (and (eql (car tail) olditem)
                     (eq start-tail tail))
                (setf (car tail) newitem)
              (setf (car (cdr tail)) newitem))
            (and (zerop (setq count (1- count)))
                 (return t))))
        result)
    LIST)
  )

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
           t newitem nil sequence start end count nil nil key from-end predicate)
  )

(defun substitute-if-not (newitem predicate sequence &key (start 0) end count key from-end)
  "Like SUBSTITUTE-IF except the elements replaced are those for which PREDICATE returns NIL."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           t newitem nil sequence start end count nil t key from-end predicate)
  )

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
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           t newitem olditem sequence start end count (or test-not test) (not (null test-not))
           key from-end nil)
  )

(defun nsubstitute-if (newitem predicate sequence &key (start 0) end count key from-end)
  "Like SUBSTITUTE-IF except that SEQUENCE may be destructively modified rather than copied."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           nil newitem nil sequence start end count nil nil key from-end predicate)
  )

(defun nsubstitute-if-not (newitem predicate sequence &key (start 0) end count key from-end)
  "Like SUBSTITUTE-IF-NOT except that SEQUENCE may be destructively modified
rather than copied."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           nil newitem nil sequence start end count nil t key from-end predicate)
  )

(defun nsubstitute (newitem olditem sequence
                    &key test test-not (start 0) end count key from-end)
  "Like SUBSTITUTE except that SEQUENCE may be destructively modified rather than copied."
  (funcall (etypecase sequence
             (list #'substitute-in-list)
             (vector #'substitute-in-array))
           nil newitem olditem sequence start end count
           (or test-not test) (not (null test-not)) key from-end nil)
  )


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
  (multiple-value-bind (nil position)
      (find-1 nil sequence start end nil nil key from-end predicate)
    position)
;  (nth-value 1 (find-1 nil sequence start end nil nil key from-end predicate))
  )

(defun position-if-not (predicate sequence &key from-end (start 0) end key)
  "Like POSITION-IF but looks for an element which does NOT satisfy PREDICATE."
  (check-type sequence sequence)
  (multiple-value-bind (nil position)
      (find-1 nil sequence start end nil t key from-end predicate)
    position)
;  (nth-value 1 (find-1 nil sequence start end nil t key from-end predicate))
  )

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
  (multiple-value-bind (nil position)
      (find-1 item sequence start end (or test-not test) (not (null test-not))
                         key from-end nil)
    position)
;  (if (and (listp sequence) (null from-end) (eq start 0) (null end) (null key)
;          (or (eq test 'eq) (eq test #'eql)))
;      (find-position-in-list item sequence)
;    (nth-value 1 (find-1 item sequence start end (or test-not test) (not (null test-not))
;                        key from-end nil)))
  )

;(defun find-1 (item sequence &optional (start 0) end test invertp key from-end one-arg-predicate)
;  (declare (values item index))
;  (if (and from-end (arrayp sequence))
;      (do ((index (1- (or end (length sequence))) (1- index))
;          elt elt-to-return)
;         ((< index start)
;          nil)
;       (setq elt (if key (funcall key (setq elt-to-return (aref-1-dim sequence index)))
;                   (setq elt-to-return (aref-1-dim sequence index))))
;       (when (eq invertp (not (cond (one-arg-predicate
;                                     (funcall one-arg-predicate elt))
;                                    (test
;                                     (funcall test item elt))
;                                    (t
;                                     (eql item elt)))))
;         (return (values elt-to-return index))))
;      (do ((index (seq-start sequence start))
;          (i start (1+ i))
;          (stop-index (if (consp end) end (seq-end sequence end)))
;          last-pos elt)
;         ((eq index stop-index)
;          (if last-pos (values elt last-pos)))
;       (setq elt (key-fetch-inc key sequence index))
;       (when (eq invertp (not (cond (one-arg-predicate
;                                     (funcall one-arg-predicate elt))
;                                    (test
;                                     (funcall test item elt))
;                                    (t
;                                     (eql item elt)))))
;         (if from-end
;             (setq last-pos i)
;           (return (values (elt sequence i) i))))))
;  )

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
  (@check-data-type sequence sequence)
  (values (find-1 nil sequence start end nil nil key from-end predicate))
  )

(defun find-if-not (predicate sequence &key from-end (start 0) end key)
  "Like FIND-IF but looks for an element which does NOT satisfy PREDICATE."
  (@check-data-type sequence sequence)
  (values (find-1 nil sequence start end nil t key from-end predicate))
  )

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
  (@check-data-type sequence sequence)
  (values (find-1 item sequence start end (or test-not test) (not (null test-not))
                  key from-end nil))
  )

;;; count functions have to have from-end key argument.

;(defun count-1 (item sequence &optional (start 0) end test invertp key one-arg-predicate)
;  (do ((index (seq-start sequence start))
;       (count 0)
;       (stop-index (seq-end sequence end))
;       elt)
;      ((eq index stop-index)
;       count)
;    (setq elt (key-fetch-inc key sequence index))
;    (when (eq invertp (not (cond (one-arg-predicate
;                                 (funcall one-arg-predicate elt))
;                                (test
;                                 (funcall test item elt))
;                                (t
;                                 (eql item elt)))))
;      (incf count)))
;  )

(defun count-if (predicate sequence &key (start 0) end key from-end)
  "Return number of elements of SEQUENCE (a list or array) that satisfy PREDICATE.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to PREDICATE.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not counted."
  from-end
  (check-type sequence sequence)
  (count-1 nil sequence start end nil nil key predicate)
  )

(defun count-if-not (predicate sequence &key (start 0) end key from-end)
  "Like COUNT-IF but counts elements that do NOT satisfy PREDICATE."
  from-end
  (check-type sequence sequence)
  (count-1 nil sequence nil t start end key predicate)
  )

(defun count (item sequence &key test test-not (start 0) end key from-end)
  "Return number of elements of SEQUENCE (a list or vector) that match ITEM.
TEST is a function of two args to use to compare ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL.
 Alternatively, specify as TEST-NOT a function to use which returns NIL if there is a match.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST or TEST-NOT.  If KEY is NIL, the element itself is used.
START and END are indices restricting substitution to a subsequence of SEQUENCE.
 They default to 0 and NIL (which means the end of SEQUENCE).
 Elements outside that range are not counted."
  from-end
  (check-type sequence sequence)
  (count-1 item sequence start end (or test-not test) (not (null test-not)) key nil)
  )



(defun mismatch-arrays-from-end (sequence1 sequence2 start1 end1 start2 end2
                                 test invertp key)
  (or test (setq test #'eql))
  (do ((index1 (1- end1) (1- index1))
       (index2 (1- end2) (1- index2)))
      ((or (< index1 start1) (< index2 start2))
       (unless (and (< index1 start1) (< index2 start2))
         (1+ index1)))
    (unless (eq invertp (not (if key (funcall test (funcall key (aref sequence1 index1))
                                                   (funcall key (aref sequence2 index2)))
                               (funcall test (aref sequence1 index1)
                                             (aref sequence2 index2)))))
      (return (1+ index1))))
  )

(defun mismatch-sequences-from-end (sequence1 sequence2 start1 end1 start2 end2
                                    test invertp key)
  (or test (setq test #'eql))
  (let ((compare-length (min (- end1 start1) (- end2 start2))))
    (do ((index1 (seq-start sequence1 (- end1 compare-length)))
         (index2 (seq-start sequence2 (- end2 compare-length)))
         (i (- end1 compare-length) (1+ i))
         (last-mismatch-index1 (unless (= (- end1 start1) (- end2 start2))
                                 (- end1 compare-length))))
        ((= i compare-length)
         last-mismatch-index1)
      (unless (eq invertp (not (funcall test (key-fetch-inc key sequence1 index1)
                                             (key-fetch-inc key sequence2 index2))))
        (setq last-mismatch-index1 (1+ i)))))
  )

(defun mismatch-1 (sequence1 sequence2
                   &optional (start1 0) end1 (start2 0) end2 test invertp key from-end)
  (or end1 (setq end1 (length sequence1)))
  (or end2 (setq end2 (length sequence2)))
  (if from-end
      (funcall (if (and (arrayp sequence1) (arrayp sequence2))
                   #'mismatch-arrays-from-end
                   #'mismatch-sequences-from-end)
               sequence1 sequence2 start1 end1 start2 end2 test invertp key)
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
        (return i))))
  )

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
  (mismatch-1 sequence1 sequence2 start1 end1 start2 end2
              (or test-not test) (not (null test-not)) key from-end)
  )



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
               (return i))))))
  )

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
             (cons newcar newcdr)))))
  )

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
             (cons newcar newcdr)))))
  )

(defun adjoin-1 (item list &optional area test test-not key)
  (if (member-1 (if key (funcall key item) item) list test test-not key)
      list
    (if area (cons-in-area item list area) (cons item list)))
  )


(defun length (LIST-OR-ARRAY)
  "If LIST-OR-ARRAY is a list, returns the number of elements in LIST-OR-ARRAY.
If LIST-OR-ARRAY is an array. returns the active length of LIST-OR-ARRAY, which is
the value of the fill-pointer, if any, or else the number of elements in the
array"
  (typecase list-or-array
    (DTP-CONS
     ;; a list. CDR down the list counting how many time untill we reach NIL.
     (do ((count 0 (1+ count))
          (list list-or-array (cdr list)))
         ((null list) count))
     )
    (DPT-ARRAY
     ;; it is an array. Call array-length.
     (array-total-size list-or-array)
     )
    )
  )

(DEFUN REVERSE (SEQUENCE)
  "Return a sequence whose elements are those of SEQUENCE, in reverse order.
If SEQUENCE is a list, the value is a list.
If it is an array, the value is an array of the same type."
  (ETYPECASE SEQUENCE
    (VECTOR
     (LET* ((LEN (LENGTH SEQUENCE))
            (RESULT (MAKE-ARRAY LEN :TYPE (ARRAY-TYPE SEQUENCE))))
       (DOTIMES (I LEN)
         (SETF (AREF RESULT I)
               (AREF SEQUENCE (- LEN I 1))))
       RESULT))
    (LIST
     (DO (V (S SEQUENCE (CDR S)))
         ((ATOM S) V)
       (PUSH (CAR S) V)))))

(DEFUN NREVERSE (SEQUENCE)
  "Alter SEQUENCE destructively to contain its elements in reverse order.
If SEQUENCE is a list, this works by changing cdr pointers.
If SEQUENCE is an array, this works by shuffling the elements."
  (ETYPECASE SEQUENCE
    (VECTOR
     (LET* ((LEN (LENGTH SEQUENCE))
            (HALFLEN (TRUNCATE LEN 2)))
       (DOTIMES (I HALFLEN)
         (LET ((TEM (AREF SEQUENCE I)))
           (SETF (AREF SEQUENCE I)
                 (AREF SEQUENCE (- LEN I 1)))
           (SETF (AREF SEQUENCE (- LEN I 1)) TEM)))
       SEQUENCE))
    (LIST
     (NRECONC SEQUENCE NIL))))
