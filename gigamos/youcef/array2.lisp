;;; -*- Mode:LISP; Package:ARRAY; Base:10; Readtable:CL -*-

(export '(aref
          aset
          array-element-type
          array-rank
          array-dimension
          array-dimensions
          array-total-size
          array-in-bounds-p
          array-total-size
          array-row-major-index
          adjustable-array-p
          array-has-fill-pointer-p
          make-array
          fill-pointer
          vector-pop
          vector-push
          bit-vector-p
          simple-bit-vector-p
          vector))

;;;
;;;  Most of the array accessors lock the array, i.e., sequence breaks are turned off.
;;;  Make sure that something is done to unlock it, before you leave the function, and especially if
;;;  something goes wrong and you end up in li:error routine.
;;;  You should think about this when you get a real debugger running.
;;;
;;;


(defun ARRAY-ELEMENT-TYPE (array)
  (let* ((header (read-and-lock-array-header array))
         (type (hw:ldb header %%sv-art 0))
         (loc (hw:dpb vinc:$$dtp-locative vinc::%%data-type array))
         (info nil))
    (when (= type art-hard)
      (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ -1 loc))
      (setq type (hw:ldb (hw:read-md) %%array-type 0)))
    (setq info (dispatch (byte 5. 0.) type
      (art-q t)
      (art-1b   '(unsigned-byte 1.))
      (art-2b   '(unsigned-byte 2.))
      (art-4b   '(unsigned-byte 4.))
      (art-8b   '(unsigned-byte 8.))
      (art-16b  '(unsigned-byte 16.))
      (art-32b  '(unsigned-byte 32.))
      (art-2bs  '(signed-byte 2.))
      (art-4bs  '(signed-byte 4.))
      (art-8bs  '(signed-byte 8.))
      (art-16bs '(signed-byte 16.))
      (art-32bs '(signed-byte 32.))
      (art-string 'string-char)
      (art-fat-string 'character)
      (art-single-float 'single-float)
      (art-double-float 'double-float)
      (t (li:error "Bad array type used in header" ))))
    (unlock-array)
    info))


(defun ARRAY-RANK (array)
  (let ((rank
          (if (= art-hard (hw:ldb (read-and-lock-array-header array) %%sv-art 0))
              (progn
                (hw:vma-start-read-vma-boxed-md-boxed
                  (hw:32-1- (hw:dpb vinc:$$dtp-locative vinc:%%data-type array)))
                (hw:ldb (hw:read-md) %%dimensions 0))
            1.)))
    (unlock-array)
    rank))

(defun ARRAY-DIMENSION (array dim-arg)
  (let* ((header (read-and-lock-array-header array))
         (dim
           (cond
             ((= art-hard (hw:ldb header %%sv-art 0))
              (let* ((loc (hw:dpb vinc:$$dtp-locative vinc:%%data-type array))
                     (dims (hw:ldb (progn
                                     (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- loc))
                                     (hw:read-md))
                                   %%dimensions 0)))
                (cond
                  ((or (>= dim-arg dims) (minusp dim-arg))
                   (li:error "Bad dimension argument"))
                  ((zerop dim-arg)
                   (hw:ldb header %%bounds 0))
                  (t
                   (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ (- -1 dims) array))
                   (1+ (hw:ldb (hw:read-md) %%bounds 0))))))
             ((zerop dim-arg)
              (1+ (hw:ldb header %%bounds 0)))
             (t
              (li:error "Bad dimension argument")))))
    (unlock-array)
    dim))

(defun ARRAY-DIMENSIONS (array)
  (let* ((header (read-and-lock-array-header array))
         (array-rank (if (= art-hard (hw:ldb header %%sv-art 0))
                         (hw:ldb (%VM-READ (hw:24+ -1 array)) %%dimensions 0)
                       1)))
    (prog1 (cond ((zerop array-rank)
                  nil)
                 ((= array-rank 1)
                  (cons:cons (hw:ldb header %%bounds 0) nil))
                 (t
                  (do ((rank (- array-rank) (1+ rank))
                       (dims () (cons:cons (cons:contents-offset array rank) dims)))
                      ((>= rank -1) (cons:cons (hw:ldb header %%bounds 0) dims)))
                  )
                 )
           (unlock-array))
    )
  )


(defun ARRAY-TOTAL-SIZE (array)
  (let* ((header (read-and-lock-array-header array))
         (array-rank (if (= art-hard (hw:ldb header %%sv-art 0))
                         (hw:ldb (%VM-READ (hw:24+ -1 array)) %%dimensions 0)
                       1)))
    (prog1 (cond ((zerop array-rank)
                  1)
                 ((= array-rank 1)
                  (hw:ldb header %%bounds 0))
                 (t
                  (do ((rank (- array-rank) (1+ rank))
                       (array-total-size
                         (hw:ldb header %%bounds 0)
                         (new-math:multiply-fixnum array-total-size (cons:contents-offset array rank))))
                      ((>= rank -1) array-total-size))))
           (unlock-array))
    )
  )

(defun ARRAY-LENGTH (array)
  (let* ((header (read-and-lock-array-header array))
         (array-rank (if (= art-hard (hw:ldb header %%sv-art 0))
                         (hw:ldb (%VM-READ (hw:24+ -1 array)) %%dimensions 0)
                       1)))
    (prog1 (cond ((zerop array-rank)
                  1)
                 ((= array-rank 1)
                  (hw:ldb header %%bounds 0))
                 (t
                  (do ((rank (- array-rank) (1+ rank))
                       (array-total-size
                         (hw:ldb header %%bounds 0)
                         (new-math:multiply-fixnum array-total-size (cons:contents-offset array rank))))
                      ((>= rank -1) array-total-size))))
           (unlock-array))
    )
  )

(defun ARRAY-ACTIVE-LENGTH (array)
  (if (array-has-fill-pointer-p array)
      (Fill-Pointer array)
    (array-length array))
  )

(defun array-in-bounds-p-internal (array subscripts)
  (let* ((header (read-and-lock-array-header array))
         (array-rank (if (= art-hard (hw:ldb header %%sv-art 0))
                         (hw:ldb (%VM-READ (hw:24+ -1 array)) %%dimensions 0)
                       1)))
    (prog1 (cond ((zerop array-rank)
                  (unless (null subscripts)
                    (li:error "Wrong number of subscripts passed to array-in-bounds-p"))
                  T)
                 (t
                  (do* ((tail-subscripts subscripts (cons:cdr tail-subscripts))
                        (subscript (cons:car subscripts) (cons:car tail-subscripts))
                        (n -2 (1- n))
                        (array-rank array-rank (1- array-rank))
                        (in-bounds T)
                        (check-range-bound
                          (hw:ldb header %%bounds 0)
                          use-this-bound)
                        (use-this-bound (hw:ldb (%VM-READ (hw:24+ n array)) %%bounds 0)
                                        (hw:ldb (%VM-READ (hw:24+ n array)) %%bounds 0))
                        )
                       ((or (= array-rank 1)
                            (null subscript))
                        (progn
                          ;; check number of subscripts vs array rank.
                          (when (or (null subscript)
                                    (cons:cdr tail-subscripts))
                            (li:error "Wrong number of subscripts passed to array-in-bounds-p"))
                          ;; must be a fixnum
                          (unless (vinc:%fixnump subscript)
                            (li:error "Bad subscript passed to array-in-bounds-p"))
                          ;; check for subscript error range.
                          (when (or (minusp subscript) (>= subscript check-range-bound))
                            (setq in-bounds nil))
                          in-bounds))
                    (when in-bounds
                      ;; must be a fixnum.
                      (unless (vinc:%fixnump subscript)
                        (li:error "Bad subscript passed to array-in-bounds-p"))
                      ;; check for subscript error range.
                      (when (or (minusp subscript) (>= subscript check-range-bound))
                        (setq in-bounds nil))))))
           (unlock-array))
    )
  )

(defun ARRAY-IN-BOUNDS-P (array &rest subscripts)
  (array-in-bounds-p-internal array subscripts)
  )

(defun ARRAY-ROW-MAJOR-INDEX (array &rest subscripts)
  (let* ((header (read-and-lock-array-header array))
         (array-rank (if (= art-hard (hw:ldb header %%sv-art 0))
                         (hw:ldb (%VM-READ (hw:24+ -1 array)) %%dimensions 0)
                       1)))
    (prog1
      (compute-linearized-index (cons:make-pointer vinc:$$dtp-unboxed-locative array)
                                header array-rank subscripts)
      (unlock-array))
    )
  )

(defun ADJUSTABLE-ARRAY-P (array)
  (let ((flag
          (and (= art-hard (hw:ldb (read-and-lock-array-header array) %%sv-art 0))
               (progn
                 (hw:vma-start-read-vma-boxed-md-boxed
                   (hw:32-1- (hw:dpb vinc:$$dtp-locative vinc:%%data-type array)))
                 (not (zerop (hw:ldb (hw:read-md) %%adjustable-p 0)))))))
    (unlock-array)
    flag))

(defun array-has-leader-p (array)
  (let ((flag
          (and (= art-hard (hw:ldb (read-and-lock-array-header array) %%sv-art 0))
               (progn
                 (hw:vma-start-read-vma-boxed-md-boxed
                   (hw:32-1- (hw:dpb vinc:$$dtp-locative vinc:%%data-type array)))
                 (not (zerop (hw:ldb (hw:read-md) %%leader-p 0)))))))
    (unlock-array)
    flag)
  )

(defun Array-leader (array index)
  (unless (vinc:%fixnump index)
    (li:error "~S is not a valid index" index))
  (when (< index 0)
    (li:error "~S is negative, and cannot be an index to a leader" index))
  (let* (loc
         extension-header leader-offset
         )
    ;; check to see if array has leader, and read in the header extension.
    (or (and (= art-hard (hw:ldb (read-and-lock-array-header array) %%sv-art 0))
             (progn
               (setq loc (hw:dpb vinc:$$dtp-locative vinc:%%data-type array))
               (hw:vma-start-read-vma-boxed-md-boxed
                 (hw:32-1- loc))
               (not (zerop (hw:ldb (setq extension-header (hw:read-md)) %%leader-p 0)))))
        (li:error "~S does not have a leader" array))
    (setq leader-offset (- (hw:ldb extension-header %%leader-offset 0)))
    (when (<= (progn
                (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ leader-offset loc))
                (hw:read-md))
              index)
      (li:error "~S is out of range in array-leader function" index))
    (setq leader-offset (- leader-offset index 1))
    (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ leader-offset loc))
    (prog1
      (hw:read-md)
      (unlock-array)))
    )

(defun set-array-leader (array index value)
  (unless (vinc:%fixnump index)
    (li:error "~S is not a valid index" index))
  (when (< index 0)
    (li:error "~S is negative, and cannot be an index to a leader" index))
  (let* (loc
         extension-header leader-offset
         )
    ;; check to see if array has leader, and read in the header extension.
    (or (and (= art-hard (hw:ldb (read-and-lock-array-header array) %%sv-art 0))
             (progn
               (setq loc (hw:dpb vinc:$$dtp-locative vinc:%%data-type array))
               (hw:vma-start-read-vma-boxed-md-boxed
                 (hw:32-1- loc))
               (not (zerop (hw:ldb (setq extension-header (hw:read-md)) %%leader-p 0)))))
        (li:error "~S does not have a leader" array))
    (setq leader-offset (- (hw:ldb extension-header %%leader-offset 0)))
    (when (<= (progn
                (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ leader-offset loc))
                (hw:read-md))
              index)
      (li:error "~S is out of range in array-leader function" index))
    (setq leader-offset (- leader-offset index 1))
    (hw:write-md-boxed value)
    (hw:vma-start-write-boxed (hw:24+ leader-offset loc))
    (unlock-array)
    value)
  )

(defun store-array-leader (value array index)
  (set-array-leader array index value)
  )

(defsetf array-leader set-array-leader)

(defun ARRAY-HAS-FILL-POINTER-P (array)
  (let ((flag
          (and (= art-hard (hw:ldb (read-and-lock-array-header array) %%sv-art 0))
               (progn
                 (hw:vma-start-read-vma-boxed-md-boxed
                   (hw:32-1- (hw:dpb vinc:$$dtp-locative vinc:%%data-type array)))
                 (not (zerop (hw:ldb (hw:read-md) %%fill-pointer-p 0)))))))
    (unlock-array)
    flag)
  )

(defun LOCK-ARRAY-AND-GET-FILL-POINTER-LOC (array)
  ;; Non Common Lisp.
  (if (= art-hard (hw:ldb (read-and-lock-array-header array) %%sv-art 0))
      (let ((loc (hw:dpb vinc:$$dtp-locative vinc:%%data-type array))
            extension-header)
        (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- loc))
        (setq extension-header (hw:read-md))
        ;; if no fill pointer error out.
        (unless (hw:32logbitp (byte-position %%fill-pointer-p) extension-header)
          (li:error "No fill pointer"))
        (hw:24+ (- -1 (hw:ldb extension-header %%leader-offset 0)) loc))
    (li:error "No fill pointer"))
  )

(defun INC-FILL-POINTER (array inc)
  ;; Non Common Lisp.
  (hw:vma-start-read-will-write-vma-boxed-md-boxed
    (lock-array-and-get-fill-pointer-loc array))
  (let ((new-val (+ inc (hw:read-md))))
    (when (minusp new-val)
      (li:error "Fill pointer cannot be negative."))
    (hw:md-start-write-boxed new-val)
    (unlock-array)
    new-val)
  )

(defun FILL-POINTER (array)
  (hw:vma-start-read-vma-boxed-md-boxed
    (lock-array-and-get-fill-pointer-loc array))
  (unlock-array)
  (hw:read-md))

(defun SET-FILL-POINTER (array data)
  (hw:vma-start-read-will-write-vma-boxed-md-boxed
    (lock-array-and-get-fill-pointer-loc array))
  (hw:read-md)
  (hw:md-start-write-boxed data)
  (unlock-array)
  data)

;(defsetf fill-pointer set-fill-pointer)

(defun VECTOR-POP (array)
  (svref array (inc-fill-pointer array -1))
  )

(defun VECTOR-PUSH (data array)
  (svset array (1- (inc-fill-pointer array 1)) data)
  )

;;; fix this up
(defun vector-push-extend (data array)
  (vector-push data array))

(defun named-structure-p (array)
  (let* ((header (read-and-lock-array-header array))
         (array-type (hw:ldb header %%sv-art 0))
         )
    (prog1
      (when (= art-hard array-type)
        (setq array (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:read-vma)))
        (setq header (%vm-read (hw:32-1- array)))  ;;header2
        (hw:32logbitp (byte-position %%named-structure-p) header)
        )
      (unlock-array))
    )
  )

(defun named-structure-symbol (array)
  (let* ((header (read-and-lock-array-header array))
         (array-type (hw:ldb header %%sv-art 0))
         leader-offset)
    ;; named structures (array) are hard arrays.
    ;; if the array is a hard array, then read the extension-header word at (array - 1).
    ;; check to see if the named-structure-bit is set. If it is, then subtract the leader
    ;; offset from the array pointer, and access the leader header. The length must be at
    ;; least 2. The named structure symbol is stored at (leader 1).
    (unless (= art-hard array-type)
      (li:error "~s is not a named array" array))

    (setq array (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:read-vma)))

    (setq header (%vm-read (hw:32-1- array)))  ;;header2
    (unless (hw:32logbitp (byte-position %%named-structure-p) header)
      (li:error "~s is not a named array" array))
    (setq leader-offset (- (hw:ldb header %%leader-offset 0)))
    ;; access leader header. Check to see if we have at least two words in the leader.
    (setq header (%vm-read (hw:24+ leader-offset array))) ;;leader-header
    (unless (> (hw:ldb header %%leader-length 0) 1)
      (li:error "~S has a leader with a length of only 1" array))
    (prog1
      (%vm-read (hw:24+ -2 (hw:read-vma)))
      (unlock-array))
    )
  )

;*******************************************************************************

(defun decode-displaced-array (array offset)
  (let* (array-pointer array-length array-type array-offset array-rank array-data)
    (multiple-value-setq (array-pointer array-length array-offset array-data array-rank array-type)
      (decode-array array))
    ;; hum, may be we should include some type of check to see if displaced-array fits in displaced-to array.
    (values array-data (+ array-offset offset))
    )
  )

(defun compute-Linearized-index (array header array-rank subscript-or-list)
  (cond ((vinc:%fixnump subscript-or-list)
         (if (or (>= subscript-or-list (hw:ldb header %%bounds 0))
                 (minusp subscript-or-list))
             (li:error "Subscript out of range")
           subscript-or-list))
        (t
         (do* ((subscripts subscript-or-list (cons:cdr subscripts))
               (subscript (cons:car subscript-or-list) (cons:car subscripts))
               (n -2 (1- n))
               (array-offset 0)
               (array-rank array-rank (1- array-rank))
               (check-range-bound
                 (hw:ldb header %%bounds 0)
                 use-this-bound)
               (use-this-bound (hw:ldb (%VM-READ (hw:24+ n array)) %%bounds 0)
                               (hw:ldb (%VM-READ (hw:24+ n array)) %%bounds 0))
               )
              ((or (= array-rank 1)
                   (null subscript))
               (progn
                 ;; check on number of subscripts vs array rank.
                 (when (or (null subscript)
                           (cons:cdr subscripts))
                   (li:error "Wrong number of subscripts"))
                 ;; must be a fixnum
                 (li:%trap-if-not-both-fixnum subscript subscript)
                 ;; check for subscript error range.
                 (when (or (minusp subscript) (>= subscript check-range-bound))
                   (li:error "subscript out of range"))
                 ;; compute linearized index.
                 (setq array-offset (+ array-offset subscript))))
           ;; must be a fixnum.
           (li:%trap-if-not-both-fixnum subscript subscript)
           ;; check for subscript error range.
           (when (or (minusp subscript) (>= subscript check-range-bound))
             (li:error "subscript out of range"))
           ;; compute linearized index.
           (setq array-offset (new-math:multiply-fixnum (+ array-offset subscript) use-this-bound))))
        )
  )

(defun decode-array (array)
  ;; function to decode an array. If the argument passed is not an array
  ;; a trap occurs. Returns multiple values that describe array:
  ;;        array-pointer :locative to array
  ;;        array-length : number of element in array
  ;;        array-offset : 0 or offset if array is displaced
  ;;        array-data : locative to first element of array
  ;;        array-rank : number of dimensions of the array
  ;;        array-type : array element type.
  ;;
  (let* ((header (read-and-lock-array-header array))
         (array-type (hw:ldb header %%sv-art 0))
         (header2 (hw:dpb array-type
                          %%array-type
                          (hw:unboxed-constant
                            (lisp:logior (lisp:ash 1 (byte-position %%dimensions))
                                         (lisp:ash vinc:$$dtp-array-header-extension
                                                   (byte-position vinc:%%data-type))))))
         array-pointer array-length array-offset array-data array-rank)
    (setq array-pointer (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:read-vma)))
    (when (= art-hard array-type)
      (setq header2 (%vm-read (hw:32-1- array)))
      (setq array-type (hw:ldb header2 %%array-type 0))
      (when (= array-type art-error)
        (li:error "Array is ART-ERROR")))
    (setq array-rank (hw:ldb header2 %%dimensions 0))
    ;; compute array length
    (setq array-length (cond ((zerop array-rank) 1)
                             ((= array-rank 1) (hw:ldb header %%bounds 0))
                             (t
                              (do ((rank (- array-rank) (1+ rank))
                                   (array-total-size
                                     (hw:ldb header %%bounds 0)
                                     (new-math:multiply-fixnum array-total-size (cons:contents-offset array rank))))
                                  ((>= rank -1) array-total-size)))))
    (if  (zerop (hw:ldb header2 %%displaced-p 0))
         (setq array-data (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:24+ 1 array-pointer))
               array-offset 0)
      ;; compute offset to array
      ;; compute locative pointer to data.
      (let ((displaced-to (%VM-READ (hw:24+ (- -1 array-rank) array-pointer))))
        (setq array-offset (%VM-READ (hw:24+ -1 (hw:READ-VMA))))
        (if (= (hw:ldb displaced-to vinc:%%data-type 0) vinc:$$dtp-array)
            (multiple-value-setq (array-data array-offset)
              (decode-displaced-array displaced-to array-offset))
          (setq array-data displaced-to))
        ))
    (values array-pointer array-length array-offset array-data array-rank array-type))
  )

(defun aloc-hard (array subscript-list)
  (let* ((header (read-and-lock-array-header array))
         (array-type (hw:ldb header %%sv-art 0))
         (header2 (hw:dpb array-type
                          %%array-type
                          (hw:unboxed-constant
                            (lisp:logior (lisp:ash 1 (byte-position %%dimensions))
                                         (lisp:ash vinc:$$dtp-array-header-extension
                                                   (byte-position vinc:%%data-type))))))
         (array-offset 0)
         array-data array-rank)
    (setq array (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:read-vma)))
    (when (= art-hard array-type)
      (setq header2 (%vm-read (hw:32-1- array)))
      (setq array-type (hw:ldb header2 %%array-type 0))
      (when (= array-type art-error)
        (li:error "Array is ART-ERROR")))
    (setq array-rank (hw:ldb header2 %%dimensions 0))
    (setq array-offset (if (zerop array-rank)
                           0
                         ;; compute linearized index
                         (compute-linearized-index array header array-rank subscript-list)))
    (if (not (hw:32logbitp (byte-position %%displaced-p) header2))
        (setq array-data (hw:24+ 1 (cons:make-pointer vinc:$$dtp-unboxed-locative array)))
      ;; compute locative pointer to data.
      (let ((displaced-to (%VM-READ (hw:24+ (- -1 array-rank) array))))
        (setq array-offset (+ array-offset (%VM-READ (hw:24+ -1 (hw:READ-VMA)))))
        (if (= (hw:ldb displaced-to vinc:%%data-type 0) vinc:$$dtp-array)
            (multiple-value-setq (array-data array-offset)
              (decode-displaced-array displaced-to array-offset))
          (setq array-data displaced-to))
        ;; new two-dimensional displaced-array
        ;; to be yet written.
        ))
    (values array-offset array-data array-type))
  )

(defun decode-2d-array (array s1 s0)
  (li:%trap-if-not-both-fixnum s1 s0)
  (let* ((header (read-and-lock-array-header array))
         (array-type (hw:ldb header %%sv-art 0))
         (header2)
         (array-offset)
         array-data array-rank dimension-s1 dimension-s0)
    (setq array (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:read-vma)))
    (when (not (= array-type art-hard))
        (li:error "Array is a not two dimensional"))
    (setq header2 (%vm-read (hw:32-1- array)))
    (setq array-type (hw:ldb header2 %%array-type 0))
    (and (= array-type art-error)
         (li:error "Array is ART-ERROR"))
    (setq array-rank (hw:ldb header2 %%dimensions 0))
    (or (= array-rank 2) (li:error "Array is not two-Dimensional"))
    ;; return these for bitblt purposes.
    (setq dimension-s0 (%VM-READ (hw:24+ -1 (hw:read-vma))))
    (setq dimension-s1 (hw:ldb header %%bounds 0))
    ;; check for bounds errors
    (when (or (minusp s0) (>= s0 dimension-s0))
      (li:error "Subscript range error"))
    (when (or (minusp s1) (>= s1 dimension-s1))
      (li:error "Subscript range error"))
    ;; compute linearized index here
    (setq array-offset (+ (new-math:multiply-fixnum s1 dimension-s0) s0))
    ;; now displaced-stuff
    (if (not (hw:32logbitp (byte-position %%displaced-p) header2))
        (setq array-data (hw:24+ 1 (cons:make-pointer vinc:$$dtp-unboxed-locative array)))
      ;; compute locative pointer to data.
      (let ((displaced-to (%VM-READ (hw:24+ -3 array))))
        (setq array-offset (+ array-offset (%VM-READ (hw:24+ -4 array))))
        (if (= (hw:ldb displaced-to vinc:%%data-type 0) vinc:$$dtp-array)
            (multiple-value-setq (array-data array-offset)
              (decode-1d-displaced-array displaced-to array-offset))
          (setq array-data displaced-to))
        ))
    (values array-offset array-data array-type dimension-s1 dimension-s0)
    )
  )

(defafun aref-2 (array s1 s0)
  (move o0 a0 ch-open)
  (move o1 a1)
  (call (decode-2d-array 3) a0 (o2 a2))
  (alu load-status-r nop ignore gr:*data-type* bw-16 unboxed)
  (movea r1 (svref-dispatch 1) boxed)
  (alu l+r r1 r1 gr:*return-1* boxed)
  (alu r-1 vma gr:*return-0* gr:*return-0* boxed-vma)
  (alu l-r nop gr:*zero* gr:*one* bw-24 next-pc-dispatch)
  )

(defafun ASET-2 (data array s1 s0)
  (move o0 a1 ch-open)
  (move o1 a2)
  (call (decode-2d-array 3) a1 (o2 a3))
  (alu load-status-r nop ignore gr:*data-type* bw-16 unboxed)
  (movea r1 (svset-dispatch 2) boxed)
  (alu l+r r1 r1 gr:*return-1* boxed)
  (alu r-1 vma gr:*return-0* gr:*return-0* boxed-vma)
  (alu l-r nop gr:*zero* gr:*one* bw-24 next-pc-dispatch)
 )

(defafun AREF-HARD (array subscript-list)
  (move o0 a0 ch-open)
  (call (aloc-hard 2) a0 (o1 a1))
  (alu load-status-r nop ignore gr:*data-type* bw-16 unboxed)
  (movea r1 (svref-dispatch 1) boxed)
  (alu l+r nop r1 gr:*return-1*)
  (alu r-1 vma gr:*return-0* gr:*return-0* boxed-vma)
  (alu l-r nop gr:*zero* gr:*one* bw-24 next-pc-dispatch)
 )

(defafun ASET-HARD (data array subscript-list)
  (move o0 a1 ch-open)
  (call (aloc-hard 2) a1 (o1 a2))
  (alu load-status-r nop ignore gr:*data-type* bw-16 unboxed)
  (movea r1 (svset-dispatch 2) boxed)
  (alu l+r nop r1 gr:*return-1*)
  (alu r-1 vma gr:*return-0* gr:*return-0* boxed-vma)
  (alu l-r nop gr:*zero* gr:*one* bw-24 next-pc-dispatch)
 )

(defun aref-n (array &rest subscripts)
  (aref-hard array subscripts)
  )

(defun aset-n (value array &rest subscripts)
  (aset-hard value array subscripts)
  )

(defafun aref-linear-dangerously (array index array-type)
  (alu load-status-r nop ignore gr:*data-type* bw-16)
  (move vma a0 boxed-vma)
  (movea r0 (svref-dispatch 1) boxed)
  (alu l+r r0 a2 r0 bw-24 boxed-right)
  (alu r+1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*allow-sequence-break* boxed-right)
  (move a0 a1 next-pc-dispatch)
 )

(defafun aset-linear-dangerously (array index array-type data)
  (alu load-status-r nop ignore gr:*data-type* bw-16)
  (move vma a0 boxed-vma)
  (movea r0 (svset-dispatch 2) boxed)
  (alu l+r r0 a2 r0 bw-24 boxed-right)
  (alu r+1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*allow-sequence-break* boxed-right)
  (move a0 a3 next-pc-dispatch)
 )

(defrewrite aref (&whole form array &rest subscripts)
  (lisp:case (lisp:length subscripts)
    (1 `(AREF-1  ,array . ,subscripts))
    (2 `(AREF-2  ,array . ,subscripts))
    (t form)))

(defun aref (array &rest subscripts)
  (aref-hard array subscripts)
  )

(defrewrite aset (&whole form value array &rest subscripts)
  (lisp:case (lisp:length subscripts)
    (1 `(ASET-1 ,value ,array . ,subscripts))
    (2 `(ASET-2 ,value ,array . ,subscripts))
    (t form)))

(defun aset (value array &rest subscripts)
  (aset-hard value array subscripts)
  )

(defsetf aref (array &rest subscripts) (value)
  `(aset ,value ,array . ,subscripts))

(defun bit-vector-p (x)
  (array-test x
    #'(lambda (header1)
          (or
            (hw:field=
              (hw:unboxed-constant #.(lisp:ash art-1b (byte-position %%sv-art)))
              header1 %%sv-art)
            (and
              (hw:field= (hw:unboxed-constant #.(lisp:ash $$dtp-array-header-multiple 26.)) header1 vinc:%%data-type)
              (let ((header2 (progn
                               (hw:vma-start-read-vma-unboxed-md-boxed (hw:32-1- x))
                               (hw:read-md))))
                (and
                  (= 1 (hw:ldb header2 %%dimensions 0))
                  (hw:field=
                    (hw:unboxed-constant #.(lisp:ash art-1b (byte-position %%array-type)))
                    header1 %%array-type))))))))

(defun simple-bit-vector-p (x)
  (array-test x
    #'(lambda (header1)
        (hw:field=
          (vinc:dpb-multiple-unboxed
              art-1b                    %%sv-art
              $$dtp-array-header-single vinc:%%data-type
              0)
            x (byte 11. 21.)))))


;;;; MAKE-ARRAY

;;;;
;;;;

(defun array-boxed-words-per-element (type)
  (if (= type art-q) 1 0))

(defun array-total-data-size (array-type index-length)
  (dispatch (byte 5 0) array-type
            (art-q      index-length)
            (art-1b     (hw:ldb (+ 31. index-length) (byte 19. 5.) 0))
            ((art-2b art-2bs) (hw:ldb (+ 15. index-length) (byte 20. 4.) 0))
            ((art-4b art-4bs) (hw:ldb (+ 7. index-length) (byte 21. 3.) 0))
            ((art-8b art-8bs) (hw:ldb (+ 3. index-length) (byte 22. 2.) 0))
            ((art-16b art-16bs) (hw:ldb (+ 1. index-length) (byte 23. 1.) 0))
            ((art-32b art-32bs) index-length)
            (art-string (hw:ldb (+ 3. index-length) (byte 22. 2.) 0))
            (art-fat-string (hw:ldb (+ 1. index-length) (byte 23. 1.) 0))
            ))

(eval-when (load compile eval)

(defun array-type-from-element-type (type)
  (cond
    ((li:consp type)
     (let ((first (cons:car type))
           (second (cons:cadr type)))
       (cond
         ((eq first 'li:signed-byte)
          (cond
            ((<= second 2) art-2bs)
            ((<= second 4) art-4bs)
            ((<= second 8.) art-8bs)
            ((= second 16.) art-16bs)
            ((<= second 32.) art-32bs)
            (t art-q)))
         ((eq first 'li:unsigned-byte)
          (cond
            ((<= second 1) art-1b)
            ((<= second 2) art-2b)
            ((<= second 4) art-4b)
            ((<= second 8.) art-8b)
            ((<= second 16.) art-16b)
            ((<= second 32.) art-32b)
            (t art-q)))
         (t art-q))))
    ((eq type 'li:string-char) art-string)
    ((eq type 'li:fat-string) art-fat-string)
    (t art-q))
  )

)


;;;;;
;;;;;

;;; Internal

(defun Array-dimensions-and-data-length-in-q-s (dimensions type)
  (let (numb-of-dimensions array-length-in-elements)
    ;; error checking on the dimensions. Returns the length of the data area of the array in elements
    ;; if every thing is ok.
    (cond ((and (vinc:%fixnump dimensions)
                (>= dimensions 0))
           (unless (< dimensions array-dimension-limit)
             (li:error "Array too large---- Cannot make it."))
           (setq numb-of-dimensions 1)
           (setq array-length-in-elements dimensions))
          ((li:consp dimensions)
           (setq array-length-in-elements (cons:car dimensions))
           (do ((i 1 (1+ i))
                (dims (cons:cdr dimensions) (cons:cdr dims)))
               ((null dims)
                (progn
                  (setq numb-of-dimensions i)
                  (when (>= i array-rank-limit)
                    (li:error "Arrays may have at most ~D dimensions, not ~S" i array-rank-limit))
                  (unless (vinc:%fixnump array-length-in-elements)
                    (li:error "Array too large-- Cannot make it."))))
             (let ((dim (cons:car dims)))
               (or (and (vinc:%fixnump dim)
                        (>= dim 0)
                        (< dim array-dimension-limit))
                   (li:error "~dth dimension, ~S, is not a fixnum" i dim))
               (setq array-length-in-elements (new-math:multiply-fixnum array-length-in-elements dim))
               (li:%trap-if-not-both-fixnum array-length-in-elements array-length-in-elements))))
          ((null dimensions)
           (setq numb-of-dimensions 0)
           (setq array-length-in-elements 1))
          (t (li:error "~S is not a valid array dimension specification." dimensions)))
    (values numb-of-dimensions (array-total-data-size type array-length-in-elements) array-length-in-elements)
    )
  )


(defun build-array-headers (number-of-dimensions
                            type
                            displaced-to
                            fill-pointer
                            adjustable
                            leader-length
                            index-length-in-q-s
                            header-block-length
                            named-structure-symbol
                            main-array-header
                            array-header-extension
                            array-leader-header
                            array
                            )
  ;; Compute what each header has to be and store it.
  (when array-header-extension
    (hw:write-md-boxed (cons:make-header vinc:$$dtp-array-header-extension (+ index-length-in-q-s header-block-length)))
    (hw:vma-start-write-boxed array)
    (let ((temp (1+ number-of-dimensions)))
      (setq main-array-header (hw:dpb-boxed vinc:$$dtp-array-header-multiple vinc:%%data-type main-array-header))
      (setq main-array-header (hw:dpb-boxed art-hard %%sv-art main-array-header))
      (when displaced-to
        (setq temp (+ temp 2)))
      (setq array-header-extension (hw:dpb-boxed temp %%leader-offset 0))
      (when array-leader-header
        (setq array-leader-header (hw:dpb-boxed leader-length %%leader-length 0))
;       (setq temp (1+ leader-length))
        (hw:write-md-boxed array-leader-header)
        (hw:vma-start-write-boxed (hw:24+ temp array)))
      (when fill-pointer (setq array-header-extension (hw:dpb-boxed 1 %%fill-pointer-p array-header-extension)))
      (when (and leader-length (not (zerop leader-length)))
        (setq array-header-extension (hw:dpb-boxed 1 %%leader-p array-header-extension)))
      (when displaced-to (setq array-header-extension (hw:dpb-boxed 1 %%displaced-p array-header-extension)))
      (setq array-header-extension (hw:dpb-boxed type %%array-type array-header-extension))
      (setq array-header-extension (hw:dpb-boxed number-of-dimensions %%dimensions array-header-extension))
      (when adjustable (setq array-header-extension (hw:dpb-boxed 1 %%adjustable-p array-header-extension)))
      (when named-structure-symbol (setq array-header-extension (hw:dpb-boxed 1 %%named-structure-p array-header-extension)))
      ;; now store it in right spot.
      (setq temp (- header-block-length 2))
      (hw:write-md-boxed array-header-extension)
      (hw:vma-start-write-boxed (hw:24+ temp array))
      ))
  (setq array (hw:24+ (1- header-block-length) array))
  (values array main-array-header)
  )

(defun Get-array-and-array-header (number-of-dimensions
                                   dimension-0
                                   type
                                   displaced-to
                                   fill-pointer
                                   adjustable
                                   leader-length
                                   index-length-in-q-s
                                   named-structure-symbol
                                   area)
  ;; Returns the number of boxed qs to be used in the header-block. and the values to store in headers
  (let ((main-array-header (vinc::dpb-multiple-boxed
                             dimension-0                        %%bounds
                             type                               %%sv-art
                             vinc:$$dtp-array-header-single     vinc:%%data-type
                             0))
        (array-header-extension nil)
        (array-leader-header nil)
        (header-block-length 0)
        array)
    (when (> number-of-dimensions 1) (setq header-block-length (1- number-of-dimensions)))
    (when displaced-to               (setq header-block-length (+  header-block-length 2))
          ;; silly displaced array do not have any data area.
          (setq index-length-in-q-s 0))
    (when (and leader-length (not (zerop leader-length)))
      (setq header-block-length (+ header-block-length leader-length 1))
      (setq array-leader-header t))
    (if (or (not (= number-of-dimensions 1))
            fill-pointer
            adjustable
            displaced-to)
        (progn
          (setq header-block-length (+ header-block-length 3))
          (setq array-header-extension t))
      (setq header-block-length (1+ header-block-length)))
    ;; Allocate the array block now.
    (setq array (cond ((zerop (array-boxed-words-per-element type))
                       (if area
                           (cons:allocate-structure-in-area
                             header-block-length
                             index-length-in-q-s
                             vinc:$$dtp-unboxed-locative nil
                             area)
                         (cons:allocate-structure
                           header-block-length
                           index-length-in-q-s
                           vinc:$$dtp-unboxed-locative
                           nil)))
                      (t
                       (if area
                           (cons:allocate-structure-in-area
                             (+ header-block-length
                                index-length-in-q-s)
                             0
                             vinc:$$dtp-unboxed-locative nil
                             area)
                         (cons:allocate-structure
                           (+ header-block-length
                              index-length-in-q-s)
                           0
                           vinc:$$dtp-unboxed-locative
                           nil)))))

    (build-array-headers number-of-dimensions
                         type
                         displaced-to
                         fill-pointer
                         adjustable
                         leader-length
                         index-length-in-q-s
                         header-block-length
                         named-structure-symbol
                         main-array-header
                         array-header-extension
                         array-leader-header
                         array
                         )
    )
  )

(defun check-displaced-legality (type array-length-in-elements displaced-to
                                 displaced-index-offset)
  (cond ((hw:field= (hw:dpb-unboxed vinc:$$dtp-array vinc:%%data-type 0)
                    displaced-to vinc:%%data-type)
         (if (> (+ array-length-in-elements displaced-index-offset) (array-total-size displaced-to))
             (li:error "Cannot fit in specified displaced to array")
           )
         )
        ((hw:field= (hw:dpb-unboxed vinc:$$dtp-unboxed-locative vinc:%%data-type 0)
                    displaced-to vinc:%%data-type)
         )
        (t (li:error "Not a valid thing to displaced to.")))
  )



(defun make-array-internal (dimensions type adjustable fill-pointer
                            displaced-to displaced-index-offset
                            leader-length named-structure-symbol area)
  (let (number-of-dimensions
        array-data-length-in-q-s
        main-array-header
        array temp)
    (multiple-value-setq (number-of-dimensions array-data-length-in-q-s temp)
      (array-dimensions-and-data-length-in-q-s dimensions type))
    (when displaced-to
      (check-displaced-legality type temp       ; temp has array length in elements
                                displaced-to displaced-index-offset))
    (when fill-pointer
      (and (eq fill-pointer t) (setq fill-pointer 0))
      (if (vinc:%fixnump fill-pointer)
          (unless (= number-of-dimensions 1)
            (li:error "Only one dimensional array can have fill-pointers"))
        (li:error "~S is a bad fill-pointer specification" fill-pointer))
      ;; since fill pointer is leader 0 of array do the following
      (setq leader-length (if leader-length (max leader-length 1) 1)))
    (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
    (multiple-value-setq (array main-array-header)
      (Get-array-and-array-header
        number-of-dimensions
        (cond
          ((li:consp dimensions) (cons:car dimensions))
          ((vinc:%fixnump dimensions) dimensions)
          (t 0))
        type displaced-to fill-pointer adjustable leader-length array-data-length-in-q-s named-structure-symbol area))
    (setq temp (hw:24+ -2 array))
    ;; fill in dimensions and displaced-stuff
    (when (> number-of-dimensions 1)
      (do* ((dim (cons:cdr dimensions) (cons:cdr dim)))
           ((null dim))
        (hw:write-md-boxed (cons:car dim))
        (hw:vma-start-write-boxed temp)
        (setq temp (hw:24+ -1 temp))))
    ;; temp is pointing to the next empty spot.
    ;; now displaced-suff
    (when displaced-to
      (hw:write-md-boxed displaced-to)
      (hw:vma-start-write-boxed temp)
      (hw:memory-wait)
      (hw:write-md-boxed displaced-index-offset)
      (hw:vma-start-write-boxed (hw:32-1- temp))
      (setq temp (hw:24+ -2 temp))
      )
    (when leader-length
      (hw:write-md-boxed leader-length)
      (hw:vma-start-write-boxed temp)
      (hw:memory-wait)
      (setq temp (hw:24+ -1 temp))
      ;; leave address pointing to first element of leader.
      )
    (when fill-pointer
      ;; Write fill-pointer in first leader length.
      (hw:write-md-boxed fill-pointer)
      (hw:vma-start-write-boxed temp)
      (hw:memory-wait))
    (when named-structure-symbol
      ;; skip fill-pointer word.
      (setq temp (hw:24+ -1 temp))
      ;; store named-structure-symbol in second leader length
      (hw:write-md-boxed named-structure-symbol)
      (hw:vma-start-write-boxed temp)
      (hw:memory-wait)
      )
    (hw:write-md-boxed main-array-header)
    (hw:vma-start-write-boxed array)
    (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*))
    (cons:make-pointer vinc:$$dtp-array array)
    )
  )

(defun simple-error-checks (type? element-type? initial-contents? initial-element? displaced-to?)
  (when (and element-type? type?)
    (li:error "Cannot supecify both element-type and type parameters"))
  (when (and initial-contents? initial-element?)
    (li:error "initial-contents and initial-element cannot be specified at the same time"))
  (when (or (and initial-contents? displaced-to?)
            (and initial-element? displaced-to?))
    (li:error "Initial-contents or initial-element cannot be specified when displaced-to is specified"))
  )

(defun zl-make-array (dimensions &key (element-type t element-type-p) (type art-q type-p)
                      (initial-element nil initial-element-p) (initial-contents nil initial-contents-p)
                      (named-structure-symbol nil) fill-pointer
                      (displaced-to nil displaced-to-p) (displaced-index-offset 0) adjustable
                      leader-length leader-list area)
  (simple-error-checks type-p element-type-p initial-element-p initial-contents-p displaced-to-p)
  ;; error checks and setup of length of leader.
  (cond ((null leader-length)
         (setq leader-length (if named-structure-symbol 2 0)))
        ((and (vinc:%fixnump leader-length)
              (>= leader-length 0))
         (when named-structure-symbol
           (setq leader-length (max leader-length 2))))
        (t (li:error "~S is not a valid leader-length specification" leader-length)))
  (cond ((null leader-list))
        ((li:consp leader-list)
         (setq leader-length (max (length leader-list) leader-length)))
        (t (li:error "~S is not a valid leader-list specification" leader-list)))
  (when (and (not type-p)
             (not element-type-p)
             displaced-to-p)
      (setq element-type (array-element-type displaced-to)
            element-type-p T))
  (make-array-internal
    dimensions
    (if element-type-p (array-type-from-element-type element-type) type)
    T fill-pointer displaced-to displaced-index-offset
    leader-length named-structure-symbol area)
  )

;; Commonlisp

(defun make-array (dimensions &key (element-type t element-type-p)
                   (initial-element nil initial-element-p)
                   (initial-contents nil initial-contents-p)
                   adjustable fill-pointer
                   (displaced-to nil displaced-to-p)
                   (displaced-index-offset 0)
                   )
  (simple-error-checks nil element-type-p initial-contents-p initial-element-p displaced-to-p)
  (when displaced-to-p
;    (unless (arrayp displaced-to)
;      (li:error "Supplied displaced-to ~S must be an array" displaced-to))
    ;; ask about types.
;    (when element-type-p
;      (li:error "Cannot specify a type for a displaced array"))
    )
  (make-array-internal
    dimensions (array-type-from-element-type element-type)
    adjustable fill-pointer
    displaced-to displaced-index-offset
    nil nil nil)
  )

(defun make-easy-array (dimensions &optional (type ART-Q))
  (make-array-internal dimensions type nil nil nil 0 nil nil nil))

(defun make-easy-array-with-element-type (dimensions element-type)
  (make-array-internal dimensions (array-type-from-element-type element-type) nil nil nil 0 nil nil nil))

(defrewrite make-array (&whole form dimensions &rest options &key (element-type t element-type-p))
  (cond
    ((null options)
     (cond ((numberp dimensions)
            `(MAKE-VECTOR ,dimensions))
           (t
            `(MAKE-EASY-ARRAY ,dimensions))))
    ((and element-type-p
          (null (lisp:cddr options)))
     (let ((type ;; do something clever with constantp here
             (or (eq element-type t)
                 (and (lisp:consp element-type)
                      (eq (lisp:car element-type) 'QUOTE)
                      (lisp:cadr element-type)))))
       (if type
           (let ((array-type (array-type-from-element-type type)))
             (if (numberp dimensions)
                 (cond ((= array-type art-q)
                        `(MAKE-VECTOR ,dimensions))
                       ((= array-type art-string)
                        `(MAKE-STRING ,dimensions))
                       (t
                        `(MAKE-1D-ARRAY ,dimensions ,array-type)))
               `(MAKE-EASY-ARRAY ,dimensions ,array-type)))
         `(MAKE-EASY-ARRAY-WITH-ELEMENT-TYPE ,dimensions ,element-type))))
    (t form)))



(defun vector (&rest objects)
  (let ((vector (make-vector (length objects))))
    (do ((i 0 (1+ i))
         (tail objects (cdr tail)))
        ((null tail) vector)
      (svset vector i (car tail))))
  )


;;; adjust array stuff

;(defun adjust-array (array new-dimensions &rest options)
;  ;; allowed options are the following key arguments
;  ;;               element-type
;  ;;               initial-element
;  ;;               initial-contents
;  ;;               fill-pointer
;  ;;               displaced-to
;  ;;               displaced-index-offset
;  (let (new-array array-rank
;       array-type dimensions
;       array-length leader-length displaced
;       initial-contents-p displaced-p args
;       fill-pointer)
;    (multiple-value-setq (array-type array-rank dimensions array-length leader-length fill-pointer displaced)
;      (decode-array-for-adjusting array))
;    (unless (= array-rank (length new-dimensions))
;      (li:error "New array rank ~D is different from old array rank ~D"))
;  (multiple-value-setq (args initial-contents-p displaced-p)
;    (li:apply #'build-arg-list-for-adjust-array
;             (cons:cons array                        ; Silly but must print error message with it in called function.
;                   (cons:cons new-dimensions         ; dimensions of new array.
;                   (cons:cons fill-pointer           ; for checking existence of fill pointer in old array
;                              (cons:cons array-type options))))))     ; elment type checking.
;  (setq new-array
;       (if (zerop leader-length)
;           (li:apply #'make-array args)
;         (progn
;           (li:push leader-length args)
;           (li:push :LEADER-LENGTH args)
;           (li:apply #'zl-make-array args))))
;    ;; should not interrupt this piece of code since we are going to start screwing with pointers.
;    (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
;    ;; copy elements from old array to new array.
;    (unless (or initial-contents-p displaced-p)
;      (forward-element-of-adjusted-array array new-array
;                                        (and (not displaced) (= array-type art-q)) dimensions new-dimensions))
;    (forward-header-of-adjusted-array array new-array leader-length array-rank fill-pointer displaced)
;    (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*))
;    array
;    )
;  )

;(defun forward-element-of-adjusted-array (array
;                                         new-array
;                                         write-body-forwards
;                                         dimensions
;                                         new-dimensions)
;  ;; array-length and dimensions are those of the forworded array.
;  (let ((index (li:make-list (length dimensions) :initial-element 0))
;       (pointer-to-write (cons:make-pointer vinc:$$dtp-body-forward array))
;       (base (li:mapcar #'li:min dimensions new-dimensions))
;       )
;    ;; first, copyt the elements of array into new-array. Put forward pointers if the array is of type art-q
;    (loop
;      (aset-hard (aref-hard array index) new-array index)
;      (when write-body-forwards
;       ;; put a body forward pointer in current cell of old array to corresponding cell in new array.
;       (aset-hard pointer-to-write array index))
;      ;; increment index.
;      (labels
;       ((increment-index (index base)
;                         (if (null index)
;                             (return-from forward-element-of-adjusted-array)
;                           (let* ((current-index (cons:car index))
;                                  (current-base (cons:car base))
;                                  (new-index (1+ current-index)))
;                             (if (< new-index current-base)
;                                 (cons:rplaca index new-index)
;                               (progn
;                                 (cons:rplaca index 0)
;                                 (increment-index (cons:cdr index) (cons:cdr base))))))))
;       ;; increment index
;       (increment-index index base))
;      )
;    )
;  )

;(defun forward-leader (array new-array leader-length pointer-to-write fill-pointer?)
;  (do ((i 0 (1+ i))
;       (offset-new (- -1 (hw:ldb (cons:contents-offset new-array -1) %%leader-offset 0))
;                  (1- offset-new))
;       (offset-old (- -1 (hw:ldb (cons:contents-offset array -1) %%leader-offset 0))
;                          (1- offset-old)))
;      ((= i leader-length))
;    (unless (and fill-pointer? (zerop i))
;      (cons:store-contents-offset new-array offset-new (cons:contents-offset array offset-old)))
;    (cons:store-contents-offset array offset-old pointer-to-write))
;  ;; forward header word
;  (cons:store-contents-offset array (- (hw:ldb (hw:vm-read (hw:24+ -1 array)) %%leader-offset 0)) pointer-to-write)
;  )

;(defun forward-header-of-adjusted-array (array
;                                        new-array
;                                        leader-length
;                                        array-rank
;                                        fill-pointer?
;                                        displaced-to?
;                                        &aux (pointer-to-write (cons:make-pointer vinc:$$dtp-body-forward array)))
;  ;; Copy and forward leader if it exists.
;  (unless (zerop leader-length)
;    (forward-leader array new-array leader-length pointer-to-write fill-pointer?))
;  ;; displaced-stuff
;  (when displaced-to?
;    (cons:store-contents-offset array (- -1 array-rank) pointer-to-write)
;    (cons:store-contents-offset array (- -2 array-rank) pointer-to-write)
;    )
;  ;; dimensions
;  (unless (or (zerop array-rank) (= 1 array-rank))
;    (do ((rank (- array-rank) (1+ rank)))
;       ((>= rank -1))
;      (cons:store-contents-offset array rank pointer-to-write))
;    )
;  ;; fill-pointer?
;;  (when fill-pointer?
;;    (cons:store-contents-offset array (- -1 (hw:ldb (cons:contents-offset array -1) %%leader-offset 0)) pointer-to-write)
;;    )
;  ;; the array is adjustable, then it has an extended header at least.
;  (cons:store-contents-offset array -1 pointer-to-write)
;  ;; forward header of old array to new one.
;  (cons:store-contents array (cons:make-pointer vinc:$$dtp-header-forward new-array))
;  )


;(defun decode-array-for-adjusting (array)
;  ;; should return the type, the old-dimensions, the rank, adjustable-flag, leader-length,
;  ;; total-size, displaced?, fill-pointer.
;  (let* ((header (read-and-lock-array-header array))
;        (array-type (hw:ldb header %%sv-art 0))
;        (header2 (hw:dpb array-type
;                         %%array-type
;                         (hw:unboxed-constant
;                           (lisp:logior (lisp:ash 1 (byte-position %%dimensions))
;                                        (lisp:ash vinc:$$dtp-array-header-extension
;                                                  (byte-position vinc:%%data-type))))))
;        (fill-pointer nil)
;        (dimensions nil)
;        array-length
;        array-rank
;        (leader-length 0)
;        displaced)
;    (setq array (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:read-vma)))
;    (when (= art-hard array-type)
;      (setq header2 (%vm-read (hw:32-1- array)))
;      (setq array-type (hw:ldb header2 %%array-type 0))
;      (when (= array-type art-error)
;       (li:error "Array is ART-ERROR")))
;    (unless (hw:32logbitp (byte-position %%adjustable-p) header2)
;      (li:error "~S is not an adjustable array." array))
;    (setq array-rank (hw:ldb header2 %%dimensions 0))
;    ;; compute array length and dimensions
;    (setq array-length (cond ((zerop array-rank) 1)
;                            ((= array-rank 1) (hw:ldb header %%bounds 0))
;                            (t
;                             (do ((rank (- array-rank) (1+ rank))
;                                  (array-total-size
;                                    (hw:ldb header %%bounds 0)
;                                    (new-math:multiply-fixnum array-total-size
;                                                              (cons:contents-offset array rank))))
;                                 ((>= rank -1) array-total-size)))))
;    (setq dimensions (cond ((zerop array-rank)
;                           nil)
;                          ((= array-rank 1)
;                           (cons:cons (hw:ldb header %%bounds 0) nil))
;                          (t
;                           (do ((rank (- array-rank) (1+ rank))
;                                (dims () (cons:cons (cons:contents-offset array rank) dims)))
;                               ((>= rank -1) (cons:cons (hw:ldb header %%bounds 0) dims)))
;                           )))
;    (when (= (hw:ldb header2 %%fill-pointer-p 0) 1)
;      (setq fill-pointer (%vm-read (hw:24- (- (hw:ldb header2 %%leader-offset 0)) array))))
;    (setq displaced (= 1 (hw:ldb header2 %%displaced-p 0)))
;    (when (= 1 (hw:ldb header2 %%leader-p 0))
;      (setq leader-length (hw:ldb (%vm-read (hw:24+ -2 array)) %%leader-length 0)))
;    (unlock-array)
;    (values array-type array-rank dimensions array-length leader-length fill-pointer displaced)
;    )
;  )

;(defun build-arg-list-for-adjust-array (array dimensions old-fill-pointer old-element-type
;                                       &key (element-type t element-type-p)
;                                       (initial-element nil initial-element-p)
;                                       (initial-contents nil initial-contents-p)
;                                       (fill-pointer 0 fill-pointer-p)
;                                       (displaced-to nil displaced-to-p)
;                                       (displaced-index-offset 0 displaced-index-offset-p))
;  (let ((args (li:list :adjustable T)))
;    (when element-type-p
;      (unless (= old-element-type (array-type-from-element-type element-type))
;       (li:error "Old element type ~S, and new element type ~S do not match in adjust-array"
;                 old-element-type element-type)))
;      (li:push (dispatch (byte 5. 0.) old-element-type
;                        (art-q t)
;                        (art-1b   '(unsigned-byte 1.))
;                        (art-2b   '(unsigned-byte 2.))
;                        (art-4b   '(unsigned-byte 4.))
;                        (art-8b   '(unsigned-byte 8.))
;                        (art-16b  '(unsigned-byte 16.))
;                        (art-32b  '(unsigned-byte 32.))
;                        (art-2bs  '(signed-byte 2.))
;                        (art-4bs  '(signed-byte 4.))
;                        (art-8bs  '(signed-byte 8.))
;                        (art-16bs '(signed-byte 16.))
;                        (art-32bs '(signed-byte 32.))
;                        (art-string 'string-char)
;                        (art-fat-string 'character)
;                        (art-single-float 'single-float)
;                        (art-double-float 'double-float)
;                        (t (li:error "Bad array type used in header"))) args)
;      (li:push :element-type args)
;    (when initial-element-p
;      (li:push initial-element args)
;      (li:push :initial-element args))
;    (when :initial-contents-p
;      (li:push initial-contents args)
;      (li:push :initial-contents args))
;    ;; if old array has a fill pointer and we are not supplied with a new one, copy it.
;    (when old-fill-pointer
;      (unless fill-pointer-p
;       (setq fill-pointer old-fill-pointer
;             fill-pointer-p T)))
;    (when fill-pointer-p
;      ;; check to see if old array had a fill-pointer. If not then it is an error.
;      (unless old-fill-pointer
;       (li:error "~S has no Fill pointer. Cannot specify it for its adjustment." array))
;      (li:push fill-pointer args)
;      (li:push :fill-pointer args))
;    (when displaced-to-p
;      (li:push displaced-to args)
;      (li:push :displaced-to args))
;    (when displaced-index-offset-p
;      (li:push displaced-index-offset args)
;      (li:push :displaced-index-offset args))
;    ;; put the dimensions in the argument list
;    (li:push dimensions args)
;    (values args initial-contents-p displaced-to-p)
;    )
;  )
