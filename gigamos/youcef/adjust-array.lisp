;;; -*- Mode:LISP; Package:ARRAY; Base:10; Readtable:CL -*-

;(defun adjust-array (array new-dimensions
;                    &key (element-type t element-type-p)
;                    (initial-element nil initial-element-p)
;                    (initial-contents nil initial-contents-p)
;                    (fill-pointer 0 fill-pointer-p)
;                    (displaced-to nil displaced-to-p)
;                    (displaced-index-offset 0 displaced-index-offset-p)
;                    &aux array-rank)
;;  (unless (adjustable-array-p array)
;;    (li:error "~S is not an adjustable array."))
;;  (unless (= (setq array-rank (array-rank array)) (length new-dimensions))
;;    (li:error "New array rank ~D is different from old array rank ~D"))
;;  (let ((args (li:list :adjustable T))
;;      (old-element-type (array-type-from-element-type (array-element-type array)))
;;      (leader-length (array-leader-length array))
;;      new-array
;;      )
;;    (when element-type-p
;;      (unless (= old-element-type (array-type-from-element-type element-type))
;;      (li:error "Old element type ~S, and new element type ~S do not match in adjust-array"
;;                old-element-type element-type)))
;;    ;; in any case it should be one of the arguments.
;;    (li:push old-element-type args)
;;    (li:push :element-type args)
;;    (when initial-element-p
;;      (li:push initial-element args)
;;      (li:push :initial-element args))
;;    (when :initial-contents-p
;;      (li:push initial-contents args)
;;      (li:push :initial-contents args))
;;    (when fill-pointer-p
;;      ;; check to see if old array had a fill-pointer. If not then it is an error.
;;      (unless (array-has-fill-pointer-p array)
;;      (li:error "~S has no Fill pointer. Cannot specify it for its adjustment." array))
;;      (li:push fill-pointer args)
;;      (li:push :fill-pointer args))
;;    (when displaced-to-p
;;      (li:push displaced-to args)
;;      (li:push :displaced-to args))
;;    (when displaced-index-offset-p
;;      (li:push displaced-index-offset args)
;;      (li:push :displaced-index-offset args))
;;    ;; put the dimensions in the argument list
;;    (li:push new-dimensions args)
;;    ;; builds new array with everything in header set.
;    (setq new-array
;         (if (zerop leader-length)
;             (li:apply #'make-array args)
;           (progn
;             (li:push leader-length args)
;             (li:push :LEADER-LENGTH args)
;             (li:push (array-leader-list array) args)
;             (li:push :LEADER-LIST args)
;             (li:apply #'zl-make-array args))))
;    ;; should not interrupt this piece of code since we are going to start screwing with pointers.
;    (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
;    ;; copy elements from old array to new array.
;    (unless (or initial-contents-p displaced-to-p)
;      (let ((index (li:make-list array-rank :initial-element 0))
;           (old-dimensions (array-dimensions array))
;           array-index)
;       (dotimes (i (array-total-size array))
;         (setq array-index (li:list-reverse index))
;         (when (array-in-bounds-p-internal new-array array-index)
;           (aset-hard (aref-hard array array-index) new-array array-index))
;         (when (= old-element-type art-q)
;           ;; put a body forward pointer in current cell of old array to corresponding cell in new array.
;           (aset-hard (cons:make-pointer vinc:$$dtp-body-forward array)
;                      array array-index))
;         ;; increment index.
;         (labels
;           ((increment-index (index base)
;                             (if (null index)
;                                 ;; should never see this, but does not hurt to have.
;                                 nil
;                               (let* ((current-index (cons:car index))
;                                      (current-base (cons:car base))
;                                      (new-index (1+ current-index)))
;                                 (if (< new-index current-base)
;                                     (cons:rplaca index new-index)
;                                   (progn
;                                     (cons:rplaca index 0)
;                                     (increment-index (cons:cdr index) (cons:cdr base))))))))
;           ;; increment index
;           (increment-index index old-dimensions))
;         )
;       )
;      )
;    ;; forward leader if it exists. Leader contents should have been already copied if it existed before.
;    (unless (zerop leader-length)
;      (do ((i 0 (1+ i))
;          (new-leader-address (hw:24+ (- (ldb (hw:vm-read (hw:24+ new-array -1)) %%leader-offset 0)) new-array)
;                          (hw:24+ new-leader-address -1))
;          (old-leader-address (hw:24+ (- (ldb (hw:vm-read (hw:24+ array -1)) %%leader-offset 0)) array)
;                          (hw:24+ old-leader-address -1)))
;         ((= i leader-length))
;       (%vm-write old-leader-address (cons:make-pointer vinc:$$dtp-body-forward array)))
;      )
;    ;; forward header of old array to new one.
;    (%vm-write array (cons:make-pointer vinc:$$dtp-header-forward new-array))
;    (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*))
;    new-array
;    )
;  )

(defun adjust-array (array new-dimensions &rest options)
  ;; allowed options are the following key arguments
  ;;                element-type
  ;;                initial-element
  ;;                initial-contents
  ;;                fill-pointer
  ;;                displaced-to
  ;;                displaced-index-offset
  (let (new-array array-rank
        array-type dimensions
        array-length leader-length displaced
        initial-contents-p displaced-p args
        fill-pointer)
    (multiple-value-setq (array-type array-rank dimensions array-length leader-length fill-pointer displaced)
      (decode-array-for-adjusting array))
    (unless (= array-rank (length new-dimensions))
      (li:error "New array rank ~D is different from old array rank ~D"))
  (multiple-value-setq (args initial-contents-p displaced-p)
    (li:apply #'build-arg-list-for-adjust-array
              (cons:cons array                        ; Silly but must print error message with it in called function.
                    (cons:cons new-dimensions         ; dimensions of new array.
                    (cons:cons fill-pointer           ; for checking existence of fill pointer in old array
                               (cons:cons array-type options))))))     ; elment type checking.
  (setq new-array
        (if (zerop leader-length)
            (li:apply #'make-array args)
          (progn
            (li:push leader-length args)
            (li:push :LEADER-LENGTH args)
            (li:push (array-leader-list array) args)
            (li:push :LEADER-LIST args)
            (li:apply #'zl-make-array args))))
    ;; should not interrupt this piece of code since we are going to start screwing with pointers.
    (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
    ;; copy elements from old array to new array.
    (unless (or initial-contents-p displaced-p)
      (forward-element-of-adjusted-array array new-array array-type array-rank array-length dimensions displaced))
    (forward-header-of-adjusted-array array new-array leader-length array-rank fill-pointer displaced)
    (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*))
    new-array
    )
  )

(defun forward-element-of-adjusted-array (array
                                          new-array
                                          array-type
                                          array-rank
                                          array-length
                                          dimensions
                                          displaced)
  ;; array-length and dimensions are those of the forworded array.
  (let ((index (li:make-list array-rank :initial-element 0))
        (pointer-to-write (cons:make-pointer vinc:$$dtp-body-forward array))
        array-index)
    ;; first, copyt the elements of array into new-array. Put forward pointers if the array is of type art-q
    (dotimes (i array-length)
      (setq array-index (li:list-reverse index))
      (when (array-in-bounds-p-internal new-array array-index)
        (aset-hard (aref-hard array array-index) new-array array-index))
      (when (and (not displaced) (= array-type art-q))
        ;; put a body forward pointer in current cell of old array to corresponding cell in new array.
        (aset-hard pointer-to-write array array-index))
      ;; increment index.
      (labels
        ((increment-index (index base)
                          (if (null index)
                              ;; should never see this, but does not hurt to have.
                              nil
                            (let* ((current-index (cons:car index))
                                   (current-base (cons:car base))
                                   (new-index (1+ current-index)))
                              (if (< new-index current-base)
                                  (cons:rplaca index new-index)
                                (progn
                                  (cons:rplaca index 0)
                                  (increment-index (cons:cdr index) (cons:cdr base))))))))
        ;; increment index
        (increment-index index dimensions))
      )
    )
  )

(defun forward-header-of-adjusted-array (array
                                         new-array
                                         leader-length
                                         array-rank
                                         fill-pointer?
                                         displaced-to?
                                         &aux (pointer-to-write (cons:make-pointer vinc:$$dtp-body-forward array)))
  ;; Copy and forward leader if it exists.
  (unless (zerop leader-length)
    (do ((i 0 (1+ i))
         (new-leader-address (hw:24+ (- (ldb (hw:vm-read (hw:24+ -1 new-array)) %%leader-offset 0)) new-array)
                             (hw:24+ new-leader-address -1))
         (old-leader-address (hw:24+ (- (ldb (hw:vm-read (hw:24+ -1 array)) %%leader-offset 0)) array)
                             (hw:24+ old-leader-address -1)))
        ((= i leader-length))
      (%vm-write new-leader-address (%vm-read old-leader-address))
      (%vm-write old-leader-address pointer-to-write))
    ;; forward header word
    (%vm-write (hw:24+ -2 new-array) pointer-to-write)
    )
  ;; displaced-stuff
  (when displaced-to?
    (%vm-write (hw:24+ (- -1 array-rank) array) pointer-to-write)
    (%vm-write (hw:24+ (- -2 array-rank) array) pointer-to-write)
    )
  ;; dimensions
  (unless (or (zerop array-rank) (= 1 array-rank))
    (do ((rank (- array-rank) (1+ rank)))
        ((>= rank -1))
      (%vm-write (hw:24+ rank array) pointer-to-write))
    )
  ;; fill-pointer?
  (when fill-pointer?
    (%vm-write (hw:24+ (- (ldb (hw:vm-read (hw:24+ array -1)) %%leader-offset 0)) array) pointer-to-write)
    )
  ;; the array is adjustable, then it has an extended header at least.
  (%vm-write (hw:24+ -1 array) pointer-to-write)
  ;; forward header of old array to new one.
  (%vm-write array (cons:make-pointer vinc:$$dtp-header-forward new-array))
  )


(defun decode-array-for-adjusting (array)
  ;; should return the type, the old-dimensions, the rank, adjustable-flag, leader-length,
  ;; total-size, displaced?, fill-pointer.
  (let* ((header (read-and-lock-array-header array))
         (array-type (hw:ldb header %%sv-art 0))
         (header2 (hw:dpb array-type
                          %%array-type
                          (hw:unboxed-constant
                            (lisp:logior (lisp:ash 1 (byte-position %%dimensions))
                                         (lisp:ash vinc:$$dtp-array-header-extension
                                                   (byte-position vinc:%%data-type))))))
         (fill-pointer nil)
         (dimensions nil)
         array-pointer
         array-length
         array-rank
         (leader-length 0)
         displaced)
    (setq array-pointer (cons:make-pointer vinc:$$dtp-unboxed-locative (hw:read-vma)))
    (when (= art-hard array-type)
      (setq header2 (%vm-read (hw:32-1- array-pointer)))
      (setq array-type (hw:ldb header2 %%array-type 0))
      (when (= array-type art-error)
        (li:error "Array is ART-ERROR")))
    (unless (= 1 (hw:ldb header2 %%adjustable-p 0))
      (li:error "~S is not an adjustable array."))
    (setq array-rank (hw:ldb header2 %%dimensions 0))
    ;; compute array length and dimensions
    (setq array-length (cond ((zerop array-rank) 1)
                             ((= array-rank 1) (hw:ldb header %%bounds 0))
                             (t
                              (do ((rank (- array-rank) (1+ rank))
                                   (array-total-size
                                     (hw:ldb header %%bounds 0)
                                     (new-math:multiply-fixnum array-total-size
                                                               (cons:contents-offset array-pointer rank))))
                                  ((>= rank -1) array-total-size)))))
    (setq dimensions (cond ((zerop array-rank)
                            nil)
                           ((= array-rank 1)
                            (cons:cons (hw:ldb header %%bounds 0) nil))
                           (t
                            (do ((rank (- array-rank) (1+ rank))
                                 (dims () (cons:cons (cons:contents-offset array rank) dims)))
                                ((>= rank -1) (cons:cons (hw:ldb header %%bounds 0) dims)))
                            )))
    (when (= (hw:ldb header2 %%fill-pointer-p 0) 1)
      (setq fill-pointer (%vm-read (hw:24- (- (hw:ldb header2 %%leader-offset 0)) array-pointer))))
    (setq displaced (= 1 (hw:ldb header2 %%displaced-p 0)))
    (when (= 1 (hw:ldb header2 %%leader-p 0))
      (setq leader-length (hw:ldb (%vm-read (hw:24+ -2 array-pointer)) %%leader-length 0)))
    (unlock-array)
    (values array-type array-rank dimensions array-length leader-length fill-pointer displaced)
    )
  )

(defun build-arg-list-for-adjust-array (array dimensions old-fill-pointer old-element-type
                                        &key (element-type t element-type-p)
                                        (initial-element nil initial-element-p)
                                        (initial-contents nil initial-contents-p)
                                        (fill-pointer 0 fill-pointer-p)
                                        (displaced-to nil displaced-to-p)
                                        (displaced-index-offset 0 displaced-index-offset-p))
  (let ((args (li:list :adjustable T)))
    (when element-type-p
      (unless (= old-element-type (array-type-from-element-type element-type))
        (li:error "Old element type ~S, and new element type ~S do not match in adjust-array"
                  old-element-type element-type)))
      (li:push (dispatch (byte 5. 0.) old-element-type
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
                         (t (li:error "Bad array type used in header"))) args)
      (li:push :element-type args)
    (when initial-element-p
      (li:push initial-element args)
      (li:push :initial-element args))
    (when :initial-contents-p
      (li:push initial-contents args)
      (li:push :initial-contents args))
    ;; if old array has a fill pointer and we are not supplied with a new one, copy it.
    (when old-fill-pointer
      (unless fill-pointer-p
        (setq fill-pointer old-fill-pointer
              fill-pointer-p T)))
    (when fill-pointer-p
      ;; check to see if old array had a fill-pointer. If not then it is an error.
      (unless old-fill-pointer
        (li:error "~S has no Fill pointer. Cannot specify it for its adjustment." array))
      (li:push fill-pointer args)
      (li:push :fill-pointer args))
    (when displaced-to-p
      (li:push displaced-to args)
      (li:push :displaced-to args))
    (when displaced-index-offset-p
      (li:push displaced-index-offset args)
      (li:push :displaced-index-offset args))
    ;; put the dimensions in the argument list
    (li:push dimensions args)
    (values args initial-contents-p displaced-to-p)
    )
  )

;;; I gess, I should do a version that will be used in vector-push-extend.

(defun test-adjust-array (x y k)
  (setq k 0)
  (li:boot-stack-groups)
  (setq x (make-array '(4 4) :element-type t :adjustable t))
  (dotimes (i 4)
    (dotimes (j 4)
      (setf (aref x i j) k)
      (setq k (1+ k))))
  (setq y (adjust-array x '(10 10)))
  (loop)
  )
