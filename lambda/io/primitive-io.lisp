;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable: Traditional -*-

;;; started by Pace 8/10/85

;;; This file is intended to replace DISK and NEW-DISK, and perhaps some tape stuff.

;;; todo
;;;
;;;  copy-directory
;;;

(comment *** Added to NEWDISK on 1/10/86 -dg

;;; First, here are primitives for making arrays that can be wired, and for wiring them.

(defvar wireable-structures-area)

(add-initialization "Make WIREABLE-STRUCTURES-AREA"
                    '(make-area :name 'wireable-structures-area
                                :gc :static
;                               :volatility 0
                                )
                    '(once))

(defmacro with-consing-allowed-in-wireable-structures-area (&body body)
  `(unwind-protect
       (progn
         (without-interrupts
           (setf (%area-type wireable-structures-area) %region-space-static)
           (for-every-region-in-area (r wireable-structures-area)
             (setf (%region-type r) %region-space-static))
           (%invalidate-area-mapping wireable-structures-area))
         ,@body
         )
     (without-interrupts
       (setf (%area-type wireable-structures-area) %region-space-fixed)
       (for-every-region-in-area (r wireable-structures-area)
         (setf (%region-type r) %region-space-fixed))
       (%invalidate-area-mapping wireable-structures-area))
     ))

(defun make-wireable-array (n-pages type named-structure-symbol)
  (check-type n-pages integer)
  (check-type type (member art-1b art-2b art-4b art-8b art-16b art-32b art-string))
  (check-type named-structure-symbol symbol)

  (let* ((array-length (* n-pages page-size (cdr (assq type array-elements-per-q))))
         (long-length (if (> array-length %array-max-short-index-length) 1 0)))
    (with-consing-allowed-in-wireable-structures-area
      (make-array array-length
                  :type type
                  :leader-length (- page-size
                                    1           ;for leader header
                                    1           ;for leader length q
                                    1           ;for array header
                                    long-length ;for long length q
                                    )
                  :named-structure-symbol named-structure-symbol
                  :area wireable-structures-area
                  ))))

(defun wireable-array-p (array)
  (and (arrayp array)
       (not (array-displaced-p array))
       (eq (%area-number array) wireable-structures-area)
       (zerop (ldb (byte 8 0) (%pointer (%find-structure-leader array))))
       (zerop (ldb (byte 8 0) (%structure-total-size (%find-structure-leader array))))
       (zerop (ldb (byte 8 0) (%pointer-plus array (array-data-offset array))))))

(defun wire-wireable-array (array from-index to-index set-modified dont-bother-paging-in)
  from-index to-index
  ;;someday, just do specified part
  (if (not (wireable-array-p array))
      (ferror nil "Array ~s was not made with MAKE-WIREABLE-ARRAY." array))
  (%wire-structure array set-modified dont-bother-paging-in))

(defun unwire-wireable-array (array from-index to-index)
  from-index to-index
  (if (not (wireable-array-p array))
      (ferror nil "Array ~s was not made with MAKE-WIREABLE-ARRAY." array))
  (%unwire-structure array))

;;;
;;;  DMA-BUFFERS
;;;

;when we can wire subsections of the array, make the matcher
;accept any array that is at least as large as the requested size
(defstruct (dma-buffer-leader
             (:type :array-leader)
             )
  dma-buffer-ignore                             ;would be fill pointer
  dma-buffer-named-structure-symbol
  dma-buffer-16b
  dma-buffer-8b
  dma-buffer-string
  dma-buffer-size-in-pages
  dma-buffer-data-vadr
  )

(defresource dma-buffer (n-pages)
  :constructor make-dma-buffer
  :free-list-size 50.)

(defun make-dma-buffer (ignore n-pages)
  (let ((dma-buffer (make-wireable-array n-pages
                                         'art-32b
                                         'dma-buffer)))
    (setf (dma-buffer-16b dma-buffer)
          (make-array (* n-pages page-size 2)
                      :type :art-16b
                      :displaced-to dma-buffer))
    (setf (dma-buffer-8b dma-buffer)
          (make-array (* n-pages page-size 4)
                      :type :art-8b
                      :displaced-to dma-buffer))
    (setf (dma-buffer-string dma-buffer)
          (make-array (* n-pages page-size 4)
                      :type :art-string
                      :displaced-to dma-buffer))

    (setf (dma-buffer-size-in-pages dma-buffer) n-pages)
    (setf (dma-buffer-data-vadr dma-buffer)
          (%pointer-plus dma-buffer (array-data-offset dma-buffer)))
    dma-buffer))

(defselect ((dma-buffer named-structure-invoke))
  (:print-self (array stream ignore ignore)
    (printing-random-object (array stream :typep)
      (format stream "Length ~d. page~:p" (dma-buffer-size-in-pages array))
      (when (eq (ldb %%pht1-swap-status-code (%page-status array))
                %pht-swap-status-wired)
        (format stream "; Wired, first page at #x~x"
                (vadr-to-nubus-phys (dma-buffer-data-vadr array))))))
  (:which-operations (ignore)
    '(:print-self :which-operations))
  )

(defun get-dma-buffer (n-pages)
  (let ((dma-buffer (allocate-resource 'dma-buffer n-pages)))
    ;;people are allowed to change this
    (setf (dma-buffer-named-structure-symbol dma-buffer) 'dma-buffer)
    dma-buffer))
)

(defun free-dma-buffer (dma-buffer)
  (setf (dma-buffer-named-structure-symbol dma-buffer) 'dma-buffer)
  (deallocate-resource 'dma-buffer dma-buffer))

;;these can turn into aref and setf after we have "real" art-32b's
(defmacro fake-aref-32 (array index)
  `(dpb (aref (dma-buffer-16b ,array) (1+ (* 2 ,index)))
        (byte 16. 16.)
        (aref (dma-buffer-16b ,array) (* 2 ,index))))

(defmacro fake-set-aref-32 (array index value)
  `(progn
     (aset (ldb (byte 16. 0) ,value) (dma-buffer-16b ,array) (* 2 ,index))
     (aset (ldb (byte 16. 16.) ,value) (dma-buffer-16b ,array) (1+ (* 2 ,index)))
     ,value))

(defsetf fake-aref-32 fake-set-aref-32)

;;;
;;; nupi command blocks
;;;

(defresource nupi-command-block ()
  :constructor get-nupi-command-block)

(defun get-nupi-command-block (ignore)
  (let ((command-block (get-dma-buffer 1)))
    (setf (dma-buffer-named-structure-symbol command-block) 'nupi-command-block)
    command-block))

(defun free-nupi-command-block (command-block)
  (free-dma-buffer command-block))

(defconst %nupi-command-word 0)
(defconst %nupi-status-word 1)
(defconst %nupi-scatter-list 2)
(defconst %nupi-transfer-count 3)
(defconst %nupi-logical-block 4)
(defconst %nupi-interrupt-address 5)
(defconst %nupi-reserved-a 6)
(defconst %nupi-reserved-b 7)

(defmacro nupi-command-word (command-block)
  `(fake-aref-32 ,command-block %nupi-command-word))

(defmacro nupi-status-word (command-block)
  `(fake-aref-32 ,command-block %nupi-status-word))

(defmacro nupi-scatter-list (command-block)
  `(fake-aref-32 ,command-block %nupi-scatter-list))

(defmacro nupi-transfer-count (command-block)
  `(fake-aref-32 ,command-block %nupi-transfer-count))

(defmacro nupi-logical-block (command-block)
  `(fake-aref-32 ,command-block %nupi-logical-block))

(defmacro nupi-interrupt-address (command-block)
  `(fake-aref-32 ,command-block %nupi-interrupt-address))

(defmacro nupi-reserved-a (command-block)
  `(fake-aref-32 ,command-block %nupi-reserved-a))

(defmacro nupi-reserved-b (command-block)
  `(fake-aref-32 ,command-block %nupi-reserved-b))

(defselect ((nupi-command-block named-structure-invoke))
  (:print-self (array stream ignore ignore)
    (printing-random-object (array stream :typep)
      (if (eq (ldb %%pht1-swap-status-code (%page-status array))
                   %pht-swap-status-wired)
          (format stream "Wired #x~x; "
                  (vadr-to-nubus-phys (dma-buffer-data-vadr array))))
      (format stream "Command: ~a" (or (cadr (assq (ldb (byte 8 24.) (nupi-command-word array))
                                                   nupi-commands))
                                       "Unknwon"))
      (format stream "; Unit #x~x" (ldb (byte 8 0) (nupi-command-word array)))
      (let ((status (nupi-status-word array))
            list)
        (if (ldb-test (byte 1 31.) status)
            (push "Busy" list))
        (if (ldb-test (byte 1 30.) status)
            (push "Complete" list))
        (if (ldb-test (byte 1 29.) status)
            (push "Error" list))
        (when list
          (format stream "; Status: ")
          (format:print-list stream "~a" list))
        )

      (format stream "; Count ~d." (nupi-transfer-count array))
      (format stream "; Block ~d." (nupi-logical-block array))
      ))
  (:describe (array)
    (format t "~&~S:" array)
    (let ((command-word (nupi-command-word array))
          (status (nupi-status-word array))
          (scatter-list (nupi-scatter-list array))
          (transfer-count (nupi-transfer-count array))
          (block (nupi-logical-block array))
          (interrupt-address (nupi-interrupt-address array))
          (reserved-a (nupi-reserved-a array))
          (reserved-b (nupi-reserved-b array)))

      (format t "~&Unit #x~x" (ldb (byte 8 0) command-word))
      (format t "~&Spare ~s" (ldb (byte 8 8) command-word))
      (format t "~&Options ~s" (ldb (byte 8 16.) command-word))
      (if (ldb-test (byte 1 20.) command-word) (format t " Swap-partial-completion-interrupt"))
      (if (ldb-test (byte 1 21.) command-word) (format t " device-address-is-physical"))
      (if (ldb-test (byte 1 22.) command-word) (format t " SCATTER"))
      (if (ldb-test (byte 1 23.) command-word) (format t " Interrupt-enable"))
      (format t "~&Command #x~16r ~a" (ldb (byte 8 24.) command-word)
              (cadr (assq (ldb (byte 8 24.) command-word) nupi-commands)))

      (format t "~&Status ~s" status)
      (format t "~&  Busy ~s" (ldb (byte 1 31.) status))
      (format t "~&  Complete ~s" (ldb (byte 1 30.) status))
      (format t "~&  Error ~s" (ldb (byte 1 29.) status))
      (format t "~&  Retries required ~s" (ldb (byte 1 28.) status))
      (format t "~&  Aux status available ~s" (ldb (byte 1 27.) status))
      (format t "~&  Paging partial completion ~s" (ldb (byte 1 26.) status))
      (format t "~&  spare ~s" (ldb (byte 2 24.) status))
      (let ((error (ldb (byte 8 16.) status)))
        (format t "~&  controller error ~s" error)
        (when (not (zerop error))
          (format t "  Class: /"~a/"" (nth (ldb (byte 3 21.) status) nupi-error-classes))
          (format t "  ~a" (cadr (assq error nupi-controller-errors)))
          ))

      (let ((error (ldb (byte 8 8) status)))
        (format t "~&  device error ~s" error)
        (when (not (zerop error))
          (format t "  Class /"~a/"" (nth (ldb (byte 3 13.) status) nupi-error-classes))
          (format t "  ~a " (cadr (assq error nupi-device-errors)))
          ))
      (format t "~&  spare ~s" (ldb (byte 3 5) status))
      (format t "~&  ECC applied ~s" (ldb (byte 1 4) status))
      (format t "~&  n-retries ~s" (ldb (byte 3 0) status))

      (format t "~&scatter-list #x~16r" scatter-list)

      (format t "~&Transfer count ~d." transfer-count)
      (format t "~&Device block address ~s" block)
      (format t "~&Interrupt address #x~16r" interrupt-address)
      (format t "~&Reserved ~s ~s" reserved-a reserved-b)

      (when (ldb-test (byte 1 22.) command-word)        ;scatter bit
        (format t "~&Scatter list: ")
        (if (not (= (vadr-to-nubus-phys (%pointer-plus
                                          (dma-buffer-data-vadr array)
                                          8.))
                    scatter-list))
            (format t "~&   *** warning, scatter list doesn't really point here ***"))
        (do ((scatter-index 8 (+ scatter-index 2))
             (pages-to-go (floor transfer-count 1024.) (1- pages-to-go)))
            ((zerop pages-to-go))
          (format t "~&#x~8x ~d."
                  (fake-aref-32 array scatter-index)
                  (fake-aref-32 array (1+ scatter-index)))))
      ))

  (:which-operations (ignore)
    '(:print-self :which-operations :describe))
  )


(defun fill-in-nupi-command (command-block phys-unit command byte-count disk-address
                             dma-buffer dma-buffer-offset-in-pages
                             &aux n-pages
                             )

  (if (or (not (eq (ldb %%pht1-swap-status-code (%page-status command-block))
                   %pht-swap-status-wired))
          (and dma-buffer
               (not (eq (ldb %%pht1-swap-status-code (%page-status dma-buffer))
                        %pht-swap-status-wired))))
      (ferror nil "COMMAND-BLOCK and DMA-BUFFER must be wired."))

  (if (not (zerop (ldb (byte 10. 0) byte-count)))
      (ferror nil "byte-count must be an even number of pages"))

  (setq n-pages (floor byte-count 1024.))

  (if (and dma-buffer
           (or (> (+ dma-buffer-offset-in-pages n-pages)
                  (dma-buffer-size-in-pages dma-buffer))
               (> n-pages (floor (- page-size 8) 2))))  ;number of scatter entries available
      (ferror nil "transfer request too big"))

  ;;really just need to clear first 8 words
  ;; can't use array-initialize on 32b array, since it stores DTP-FIX tags
  (array-initialize (dma-buffer-16b command-block) 0)

  (setf (nupi-command-word command-block)
        (+ phys-unit
           (dpb command (byte 8 24.) (if dma-buffer #x400000 0))))      ;scatter flag

  (setf (nupi-scatter-list command-block)
        (vadr-to-nubus-phys (%pointer-plus
                              (dma-buffer-data-vadr command-block)
                              8.)))

  (setf (nupi-transfer-count command-block) byte-count)
  (setf (nupi-logical-block command-block) disk-address)

  (when dma-buffer
    (do ((vadr (%pointer-plus (dma-buffer-data-vadr dma-buffer)
                              (* dma-buffer-offset-in-pages page-size))
               (%pointer-plus vadr page-size))
         (scatter-entry 8 (+ scatter-entry 2))
         (pages-to-go n-pages (1- pages-to-go)))
        ((zerop pages-to-go))
      (let ((padr (vadr-to-nubus-phys vadr)))
        (setf (fake-aref-32 command-block scatter-entry) padr)
        (setf (fake-aref-32 command-block (1+ scatter-entry)) 1024.)))))

(defun start-nupi-command (command-block phys-unit command byte-count disk-address
                           dma-buffer dma-buffer-offset-in-pages
                           set-modified)
  (wire-wireable-array command-block 0 nil nil nil)
  ;;could arrange to do DONT-BOTHER-PAGING-IN on all pages but first
  (when dma-buffer
    (wire-wireable-array dma-buffer 0 nil set-modified nil))
  (fill-in-nupi-command command-block phys-unit command byte-count disk-address dma-buffer dma-buffer-offset-in-pages)
  (without-interrupts
    ;;can remove this after microcode bug is fixed
    ;;basically, get maps set up...
    ;;I hope the compiler does't decide this aref isn't useful
    (aref command-block 0)
    (%io-cmd-run command-block))
  )

(defun wait-for-nupi-command (command-block)
  (process-wait "Disk Wait"
                #'(lambda (command-block)
                    (ldb-test (byte 1 30.) (nupi-status-word command-block)))
                command-block)
  (cond ((ldb-test (byte 1 29.) (nupi-status-word command-block))
         (ferror nil "nupi error"))))

(defun finish-nupi-command (command-block dma-buffer)
  (unwire-wireable-array command-block 0 nil)
  (when dma-buffer
    (unwire-wireable-array dma-buffer 0 nil)))

(defun nupi-logical-to-physical-unit (logical-unit)
  (dpb (ldb (byte 3 1) logical-unit)
       (byte 3 3)
       (ldb (byte 1 0) logical-unit)))

(defun simple-nupi-command (command-block command logical-unit disk-address byte-count
                            dma-buffer dma-buffer-offset-in-pages
                            set-modified)
  (start-nupi-command command-block
                      (nupi-logical-to-physical-unit logical-unit)
                      command
                      byte-count
                      disk-address
                      dma-buffer
                      dma-buffer-offset-in-pages
                      set-modified)
  (wait-for-nupi-command command-block)
  (finish-nupi-command command-block dma-buffer))

(defun nupi-read-from-disk (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
                      (nupi-logical-to-physical-unit logical-unit)
                      #x12
                      byte-count
                      disk-address
                      dma-buffer
                      dma-buffer-offset-in-pages
                      t)
  (wait-for-nupi-command command-block)
  (finish-nupi-command command-block dma-buffer)
  )

(defun nupi-write-to-disk (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
                      (nupi-logical-to-physical-unit logical-unit)
                      #x13
                      byte-count
                      disk-address
                      dma-buffer
                      dma-buffer-offset-in-pages
                      t)
  (wait-for-nupi-command command-block)
  (finish-nupi-command command-block dma-buffer)
  )

(defun nupi-read-from-tape (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (do ((page-offset 0 (1+ page-offset))
       (byte-offset 0 (+ byte-offset 1024.))
       (end-page-offset (floor byte-count 1024.)))
      ((= page-offset end-page-offset)
       (setf (nupi-transfer-count command-block) byte-count))
    (start-nupi-command command-block
                        (nupi-logical-to-physical-unit logical-unit)
                        #x12
                        1024.
                        disk-address
                        dma-buffer
                        (+ dma-buffer-offset-in-pages page-offset)
                        t)
    (wait-for-nupi-command command-block)
    (when (not (zerop (ldb (byte 8 8) (nupi-status-word command-block))))
      (setf (nupi-transfer-count command-block) (* page-offset 1024.))
      (return)))
  (finish-nupi-command command-block dma-buffer))

(defun nupi-write-to-tape (command-block logical-unit disk-address byte-count dma-buffer dma-buffer-offset-in-pages)
  (start-nupi-command command-block
                      (nupi-logical-to-physical-unit logical-unit)
                      #x13
                      byte-count
                      disk-address
                      dma-buffer
                      dma-buffer-offset-in-pages
                      nil)
  (wait-for-nupi-command command-block)
  (finish-nupi-command command-block dma-buffer))

(defflavor basic-block-device-interface
           (label-buffer
            label-version-number
            partition-table-start
            n-partitions
            n-words-per-partition
            )
           ()
  :settable-instance-variables
  )

(defmethod (basic-block-device-interface :assure-label-read) ()
  (when (not (variable-boundp label-buffer))
    (send self :read-label)))

(defmethod (basic-block-device-interface :read-label) ()
  (when (not (variable-boundp label-buffer))
    (setq label-buffer (get-dma-buffer 3)))
  (read-label-into-label-buffer self))

(defmethod (basic-block-device-interface :get-partition-info) (part-name)
  (declare (values part-number
                   part-name
                   part-start
                   part-size
                   part-comment))
  (find-partition-in-label-buffer part-name self))

(defun read-label-into-label-buffer (unit)
  (let ((label-buffer (send unit :label-buffer)))
    (send unit :read-blocks 0 1 label-buffer)
    (if (not (string-equal (dma-buffer-string label-buffer) "LABL" :end1 4))
        (ferror nil "Bad label checkword."))
    (ecase (fake-aref-32 label-buffer 1)
      (1 (read-v1-label-into-label-buffer unit))
      (2 (read-v2-label-into-label-buffer unit)))))

(defun read-v1-label-into-label-buffer (unit)
  unit
  (ferror nil "not implemented"))

(defun read-v2-label-into-label-buffer (unit)
  (let ((label-buffer (send unit :label-buffer)))
    (if (not (string-equal (dma-buffer-string label-buffer)
                           "PTBL"
                           :start1 (* 20. 4)
                           :end1 (* 21. 4)
                           :end2 4))
        (ferror nil "don't understand this label"))
    (send unit :read-blocks
          (fake-aref-32 label-buffer 21.) 1 label-buffer 1)
    (if (not (string-equal (dma-buffer-string label-buffer) "PRTN"
                           :start1 1024.
                           :end1 1028.))
        (ferror nil "don't understand this label"))

    (send unit :set-label-version-number 2)
    (send unit :set-partition-table-start (+ 256. 16.))
    (send unit :set-n-partitions (fake-aref-32 label-buffer (+ 256. 2)))
    (send unit :set-n-words-per-partition (fake-aref-32 label-buffer (+ 256. 3)))
    (if (> (+ 16. (* (send unit :n-words-per-partition)
                     (send unit :n-partitions)))
           page-size)
        (ferror nil "partition table doesn't fit on one page"))
    ))

(defun canonicalize-partition-name (part-name)
  (setq part-name
        (etypecase part-name
          (string part-name)
          (integer (format nil "LOD~d" part-name))
          ))
  (if (> (string-length part-name) 4)
      (ferror nil "partition-name must be less than 4 characters long"))
  part-name)

(defun get-string-from-label-buffer (label-buffer byte-offset n-bytes)
  (let ((result (make-array n-bytes
                            :type :art-string
                            :leader-list '(nil))))
    (copy-array-portion (dma-buffer-string label-buffer)
                        byte-offset
                        (+ byte-offset n-bytes)
                        result
                        0
                        n-bytes)
    (setf (fill-pointer result)
          (string-search-char 0 result))
    result))

(defun find-partition-in-label-buffer (part-name unit)
  (declare (values part-number
                   part-name
                   part-start
                   part-size
                   part-comment))
  (send unit :assure-label-read)
  (setq part-name (canonicalize-partition-name part-name))
  (let ((label-buffer-string (dma-buffer-string (send unit :label-buffer)))
        (partition-table-byte-offset (* (send unit :partition-table-start) 4))
        (n-bytes-per-partition (* 4 (send unit :n-words-per-partition)))
        )
    (dotimes (part-number (send unit :n-partitions))
      (when (string-equal label-buffer-string
                          part-name
                          :start1 (+ partition-table-byte-offset
                                     (* part-number n-bytes-per-partition))
                          :end1 (+ partition-table-byte-offset
                                   (* part-number n-bytes-per-partition)
                                   4))
        (return (get-partition-info unit part-number))))))

(defun get-partition-info (unit part-number)
  (send unit :assure-label-read)
  (ecase (send unit :label-version-number)
    (2 (get-partition-info-v2 unit part-number))))

(defun get-partition-info-v2 (unit part-number)
  (let ((label-buffer (send unit :label-buffer))
        (partition-table-start (send unit :partition-table-start))
        (n-words-per-partition (send unit :n-words-per-partition))
        partition-descriptor-start
        )
    (setq partition-descriptor-start (+ partition-table-start
                                        (* part-number n-words-per-partition)))
    (values part-number
            (get-string-from-label-buffer label-buffer
                                          (* partition-descriptor-start 4)
                                          4)
            (fake-aref-32 label-buffer (+ partition-descriptor-start 1))        ;partition-start
            (fake-aref-32 label-buffer (+ partition-descriptor-start 2))        ;partition-size
            (get-string-from-label-buffer label-buffer
                                          (* (+ partition-descriptor-start 4) 4)
                                          (* (- n-words-per-partition 4) 4)))))

(defflavor nupi-disk-interface
           ((logical-unit nil)
            (command-block nil)
            )
           (basic-block-device-interface)
  :settable-instance-variables)

(defmethod (nupi-disk-interface :after :init) (ignore)
  (check-type logical-unit integer)
  )

(defmethod (nupi-disk-interface :read-blocks) (block-number n-blocks
                                                       &optional
                                                       dma-buffer
                                                       (dma-buffer-offset-in-pages 0))
  (if (null command-block)
      (setq command-block (get-nupi-command-block nil)))
  (if (null dma-buffer)
      (setq dma-buffer (get-dma-buffer n-blocks)))
  (nupi-read-from-disk command-block logical-unit block-number (* n-blocks 1024.)
                       dma-buffer dma-buffer-offset-in-pages)
  dma-buffer)

(defmethod (nupi-disk-interface :write-blocks) (block-number n-blocks
                                                        dma-buffer
                                                        &optional
                                                        (dma-buffer-offset-in-pages 0))
  (if (null command-block)
      (setq command-block (get-nupi-command-block nil)))
  (nupi-write-to-disk command-block logical-unit block-number (* n-blocks 1024.)
                      dma-buffer dma-buffer-offset-in-pages)
  dma-buffer)

(defflavor nupi-tape-interface
           ((logical-unit nil)
            (command-block nil)
            )
           (basic-block-device-interface)
  :settable-instance-variables)

(defmethod (nupi-tape-interface :after :init) (ignore)
  (check-type logical-unit integer)
  )

(defmethod (nupi-tape-interface :read-blocks) (block-number n-blocks
                                                       &optional
                                                       dma-buffer
                                                       (dma-buffer-offset-in-pages 0))
  (if (null command-block)
      (setq command-block (get-nupi-command-block nil)))
  (if (null dma-buffer)
      (setq dma-buffer (get-dma-buffer n-blocks)))
  (nupi-read-from-tape command-block logical-unit block-number (* n-blocks 1024.)
                       dma-buffer dma-buffer-offset-in-pages)
  dma-buffer)

(defmethod (nupi-tape-interface :write-blocks) (block-number n-blocks
                                                        dma-buffer
                                                        &optional
                                                        (dma-buffer-offset-in-pages 0))
  (if (null command-block)
      (setq command-block (get-nupi-command-block nil)))
  (nupi-write-to-tape command-block logical-unit block-number (* n-blocks 1024.)
                      dma-buffer dma-buffer-offset-in-pages)
  dma-buffer)


(defmethod (nupi-tape-interface :rewind) ()
  (if (null command-block)
      (setq command-block (get-nupi-command-block nil)))
  (simple-nupi-command command-block
                       #x20
                       logical-unit
                       0
                       0
                       nil
                       nil
                       nil)
  )

(defmethod (nupi-tape-interface :write-file-mark) ()
  (if (null command-block)
      (setq command-block (get-nupi-command-block nil)))
  (simple-nupi-command command-block
                       #x25
                       logical-unit
                       0
                       0
                       nil
                       nil
                       nil))

(defun get-unit (unit-name)
  (etypecase unit-name
    (integer
     (select-processor
       (:explorer
         (cond ((= unit-name 6)
                (make-instance 'nupi-tape-interface :logical-unit unit-name))
               (t
                (make-instance 'nupi-disk-interface :logical-unit unit-name))))
       ((:cadr :lambda)
        (ferror nil "not implemented"))))
    (basic-block-device-interface
     unit-name)
    ))

;;; ----

(defflavor partition-access
           (unit
            partition-name
            start
            size
            comment
            )
           ()
  :settable-instance-variables)

(defun make-partition-access (unit partition-name)
  (make-instance 'partition-access :unit unit :partition-name partition-name))

(defmethod (partition-access :after :init) (ignore)
  (check-type unit instance)
  (setq partition-name (canonicalize-partition-name partition-name))
  (let (part-number)
    (multiple-value (part-number nil start size comment)
      (send unit :get-partition-info partition-name))
    (if (null part-number)
        (ferror nil "part ~s not found" partition-name))
    ))

(defmethod (partition-access :read-blocks-from-partition)  (block-number n-blocks
                                                            &optional
                                                            dma-buffer
                                                            (dma-buffer-offset-in-pages 0))
  (check-type block-number (integer 0))
  (check-type n-blocks (integer 0))
  (if (> (+ block-number n-blocks) size)
      (ferror nil "read request out of range"))
  (send unit :read-blocks (+ block-number start) n-blocks dma-buffer dma-buffer-offset-in-pages))

(defmethod (partition-access :write-blocks-to-partition) (block-number
                                                          n-blocks
                                                          dma-buffer
                                                          dma-buffer-offset-in-pages)
  (check-type block-number (integer 0))
  (check-type n-blocks (integer 0))
  (if (> (+ block-number n-blocks) size)
      (ferror nil "read request out of range"))
  (send unit :write-blocks (+ block-number start) n-blocks dma-buffer dma-buffer-offset-in-pages))

;;; ---

(defun make-nupi-tape-input-stream (unit)
  (make-instance 'nupi-tape-input-stream :unit (get-unit unit)))

(defflavor nupi-tape-input-stream
           (dma-buffer
            unit
            (n-blocks-per-operation 10.)
            (eof-p nil)
            (bytes-returned-since-last-eof 0)
            (total-byte-limit nil)
            (do-not-strip-nulls nil)
            STREAM-INPUT-BUFFER
            STREAM-INPUT-INDEX
            STREAM-INPUT-LIMIT
            )
           (si:buffered-input-stream)
  :settable-instance-variables
  )

(defmethod (nupi-tape-input-stream :after :init) (ignore)
  (check-type unit instance)
  (setq dma-buffer (allocate-resource 'dma-buffer n-blocks-per-operation)))

(defmethod (nupi-tape-input-stream :buffer-n-bytes) (n-bytes)
  (if stream-input-buffer
      (ferror nil "can't do this because input is already buffered"))

  (if (not (zerop (ldb (byte 10. 0) n-bytes)))
      (ferror nil "must buffer a multiple of 1024. bytes"))
  (block nil
    (if eof-p (return nil))
    (send unit :read-blocks 0 (floor n-bytes 1024.) dma-buffer)

    (let ((transfer-count (nupi-transfer-count (send unit :command-block))))
      (when (zerop transfer-count)
        (setq eof-p t)
        (return nil))
      (if (< transfer-count n-bytes)
          (setq eof-p t))
      (setq stream-input-buffer (dma-buffer-8b dma-buffer))
      (setq stream-input-index 0)
      (setq stream-input-limit transfer-count)
      transfer-count)))

(defmethod (nupi-tape-input-stream :next-input-buffer) (no-hang-p)
  no-hang-p
  (block nil
    (if eof-p (return nil))

    (send unit :read-blocks 0 n-blocks-per-operation dma-buffer)

    (let ((transfer-count (nupi-transfer-count (send unit :command-block)))
          bytes-this-time)

      (when (zerop transfer-count)
        (setq eof-p t)
        (return nil))

      (if (< transfer-count (* 1024. n-blocks-per-operation))
          (setq eof-p t))

      (cond (total-byte-limit
             (setq bytes-this-time (min (- total-byte-limit bytes-returned-since-last-eof)
                                        transfer-count))
             (incf bytes-returned-since-last-eof bytes-this-time)
             (if (= bytes-returned-since-last-eof total-byte-limit)
                 (setq eof-p t)))
            (do-not-strip-nulls
             (setq bytes-this-time transfer-count))
            (t
             (let ((last-non-null (string-reverse-search-not-char 0 (dma-buffer-8b dma-buffer) transfer-count)))
               (setq bytes-this-time (if last-non-null
                                         (1+ last-non-null)
                                       transfer-count)))))

      (values (dma-buffer-8b dma-buffer)
              0
              bytes-this-time))))

(defmethod (nupi-tape-input-stream :clear-eof) ()
  (setq eof-p nil)
  (setq bytes-returned-since-last-eof 0)
  (setq total-byte-limit nil)
  (setq do-not-strip-nulls nil))

(defmethod (nupi-tape-input-stream :discard-input-buffer) (ignore)
  nil)

;can optimize this later
(defmethod (nupi-tape-input-stream :string-in-direct) (eof external-dma-buffer)
  (declare (values next-byte-number reached-eof-p)
           (ignore eof))
  (send self :string-in nil (dma-buffer-8b external-dma-buffer)))

(defflavor nupi-tape-output-stream
           (dma-buffer
            unit
            (n-blocks-per-operation 100.)
            (done-a-write-p nil)
            )
           (si:buffered-output-stream)
  :settable-instance-variables
  (:required-init-keywords :unit)
  )

(defmethod (nupi-tape-output-stream :after :init) (ignore)
  (check-type unit instance)
  (setq dma-buffer (allocate-resource 'dma-buffer n-blocks-per-operation))
  )

(defmethod (nupi-tape-output-stream :new-output-buffer) ()
  (values (dma-buffer-8b dma-buffer) 0 (* n-blocks-per-operation 1024.)))

(defmethod (nupi-tape-output-stream :send-output-buffer) (buffer ending-index)
  (if (not (zerop (ldb (byte 10. 0) ending-index)))
      (ferror nil "can't flush the buffer except on 1K boundaries"))
  (if (not (eq buffer (dma-buffer-8b dma-buffer)))
      (ferror nil "trying to write wrong buffer"))
  (setq done-a-write-p t)
  (send unit :write-blocks 0 (floor ending-index 1024.) dma-buffer)
  )

(defmethod (nupi-tape-output-stream :discard-output-buffer) (buffer)
  buffer
  )

(defmethod (nupi-tape-output-stream :after :close) (&rest ignore)
  (if done-a-write-p
      (send unit :write-file-mark))
  (free-dma-buffer (prog1 dma-buffer (setq dma-buffer nil)))
  )

(defmethod (nupi-tape-output-stream :string-out-direct) (external-dma-buffer &optional (start 0) end)
  (if (null end) (setq end (* 1024. (dma-buffer-size-in-pages external-dma-buffer))))
  (send self :force-output)
  (setq done-a-write-p t)
  (send unit :write-blocks 0 (floor (- end start) 1024.) external-dma-buffer (floor start 1024.)))

;---

(defun make-new-partition-input-stream (&key partition-name
                                        (unit 0))
  (make-instance 'new-partition-input-stream
                 :partition-name partition-name
                 :unit (get-unit unit)))

(defflavor new-partition-input-stream
           (
            partition-access
            (n-blocks-at-a-time 1)
            dma-buffer
            (next-page-number 0)
            (measured-size nil)
            (measured-size-is-input-limit nil)
            stream-input-index                  ;from buffered-input-stream
            )
           (si:input-pointer-remembering-mixin
            si:buffered-input-stream)
  :inittable-instance-variables
  (:gettable-instance-variables partition-access)
  (:init-keywords :partition-name :unit)
  (:required-init-keywords :partition-name :unit))

(defmethod (new-partition-input-stream :after :init) (init-plist)
  (let ((u (get init-plist :unit))
        (partition-name (get init-plist :partition-name)))
    (check-type u instance)
    (setq partition-access (make-partition-access u partition-name))
    (setq dma-buffer (allocate-resource 'dma-buffer n-blocks-at-a-time))
    ))

(defmethod (new-partition-input-stream :next-input-buffer) (no-hang-p)
  (declare (values array start end)
           (ignore no-hang-p))
  (let ((size (if measured-size-is-input-limit
                  measured-size
                (send partition-access :size))))
    (cond ((= next-page-number size)
           nil)
          ((> next-page-number size)
           (ferror nil "trying to read outside partition"))
          (t
           (let ((blocks-this-time (min n-blocks-at-a-time
                                        (- size next-page-number))))
             (send partition-access :read-blocks-from-partition next-page-number blocks-this-time dma-buffer)
             (incf next-page-number blocks-this-time)
             (values (dma-buffer-8b dma-buffer) 0 (* blocks-this-time 1024.)))))))

(defmethod (new-partition-input-stream :discard-input-buffer) (ignore)
  nil)

(defmethod (new-partition-input-stream :set-buffer-pointer) (new-pointer)
  (setq next-page-number (floor new-pointer 1024.))
  (* next-page-number 1024.))

(defmethod (new-partition-input-stream :close) (&optional abortp)
  (declare (ignore abortp))
  (free-dma-buffer (prog1 dma-buffer (setq dma-buffer nil)))
  )

(defmethod (new-partition-input-stream :string-in-direct) (eof external-dma-buffer)
  (declare (values next-byte-number reached-eof-p)
           (ignore eof))
  (if (send self :stream-input-buffer)
      (ferror nil "can't do string in direct because some input is already buffered"))
  (let ((size (if measured-size-is-input-limit
                  measured-size
                (send partition-access :size))))
    (cond ((= next-page-number size)
           nil)
          ((> next-page-number size)
           (ferror nil "trying to read outside partition"))
          (t
           (let ((blocks-this-time (min (dma-buffer-size-in-pages external-dma-buffer)
                                        (- size next-page-number))))
             (send partition-access :read-blocks-from-partition next-page-number blocks-this-time external-dma-buffer)
             (incf next-page-number blocks-this-time)
             (values (* blocks-this-time 1024.) (not (= (dma-buffer-size-in-pages external-dma-buffer)
                                                        blocks-this-time))))))))

(defmethod (new-partition-input-stream :label-comment) ()
  (send partition-access :comment))

(defmethod (new-partition-input-stream :read-32b) ()
  (let* ((b0 (send self :tyi))
         (b1 (send self :tyi))
         (b2 (send self :tyi))
         (b3 (send self :tyi)))
    (+ (ash b3 24.)
       (ash b2 16.)
       (ash b1 8.)
       b0)))

(defmethod (new-partition-input-stream :measured-size) (&aux band-format)
  (cond (measured-size)
        ((string-equal (send partition-access :partition-name) "LOD" :end1 3)
         (send self :set-pointer (* 4 (+ page-size %sys-com-valid-size)))
         (setq measured-size (send self :read-32b))
         (send self :set-pointer (* 4 (+ page-size %sys-com-band-format)))
         (setq band-format (send self :read-32b))



         (if (or (not (zerop (ldb %%q-all-but-pointer measured-size)))
                 (not (= (ldb (byte 16. 0) band-format) #o1000)))
             (ferror nil "partition doesn't seem to be a LOD band"))
         (setq measured-size (ash measured-size -8))
         (send self :clear-input)
         (send self :set-buffer-pointer 0)
         (setq stream-input-index 0)
         )
        (t
         (setq measured-size (send partition-access :size))))
  measured-size)

(defmethod (new-partition-input-stream :set-input-limit-to-measured-size) ()
  (send self :measured-size)
  (setq measured-size-is-input-limit t)
  )

(defun make-new-partition-output-stream (&key partition-name
                                         (unit 0))
  (make-instance 'new-partition-output-stream
                 :partition-name partition-name
                 :unit (get-unit unit)))

(defflavor new-partition-output-stream
           (partition-access
            (n-blocks-per-operation 1)
            dma-buffer
            (next-page-number 0)
            )
           (si:buffered-output-stream)
  :settable-instance-variables
  (:init-keywords :unit :partition-name)
  (:required-init-keywords :unit :partition-name)
  )

(defmethod (new-partition-output-stream :after :init) (init-plist)
  (let ((u (get init-plist :unit))
        (part-name (get init-plist :partition-name)))
    (check-type u instance)
    (setq partition-access (make-partition-access u part-name))
    (setq dma-buffer (allocate-resource 'dma-buffer n-blocks-per-operation))
    ))

(defmethod (new-partition-output-stream :new-output-buffer) ()
  (values (dma-buffer-8b dma-buffer) 0 (* n-blocks-per-operation 1024.)))

(defmethod (new-partition-output-stream :send-output-buffer) (buffer ending-index)
  (if (not (zerop (ldb (byte 10. 0) ending-index)))
      (ferror nil "can't flush the buffer except on 1K boundaries"))
  (if (not (eq buffer (dma-buffer-8b dma-buffer)))
      (ferror nil "trying to write wrong buffer"))
  (let ((n-pages (floor ending-index 1024.)))
    (send partition-access :write-blocks-to-partition next-page-number n-pages dma-buffer 0)
    (incf next-page-number n-pages))
  )

(defmethod (new-partition-output-stream :discard-output-buffer) (buffer)
  buffer
  )

(defmethod (new-partition-output-stream :after :close) (&rest ignore)
  (free-dma-buffer (prog1 dma-buffer (setq dma-buffer nil)))
  )

(defmethod (new-partition-output-stream :string-out-direct) (external-dma-buffer &optional (start 0) end)
  (if (null end) (setq end (* 1024. (dma-buffer-size-in-pages external-dma-buffer))))
  (send self :force-output)
  (let ((n-pages (floor (- end start) 1024.)))
    (send partition-access :write-blocks-to-partition next-page-number n-pages external-dma-buffer (floor start 1024.))
    (incf next-page-number n-pages)))

;----

(defun copy-partition-to-tape (from-unit from-part tape-unit)
  (let ((partition-stream (make-new-partition-input-stream :partition-name from-part
                                                            :unit from-unit))
        (tape-stream (make-instance 'nupi-tape-output-stream :unit (get-unit tape-unit))))
    (let (plist)
      (setf (getf plist :style) :mit)
      (setf (getf plist :partition) t)
      (setf (getf plist :comment) (send partition-stream :label-comment))
      (setf (getf plist :size) (send partition-stream :measured-size))
      (setf (getf plist :byte-size) 16.)
      (setf (getf plist :author) user-id)
      (setf (getf plist :tape-header-length-in-bytes) 1024.)
      (send tape-stream :string-out "LMFL")
      (let ((base 10.)
            (ibase 10.)
            (*package* (find-package "USER"))
            (*readtable* (find-readtable-named nil))
            )
        (print plist tape-stream))
      (send tape-stream :pad-and-send-buffer 0 1024.)
      ;;stolen from STREAM-COPY-UNTIL-EOF
      (do ((buf) (offset) (limit) (total-blocks 0) (next-print 10.))
          (())
        (multiple-value (buf offset limit)
          (send partition-stream :read-input-buffer))
        (cond ((null buf) (return nil)))
        (send tape-stream :string-out buf offset limit)
        (send partition-stream :advance-input-buffer)
        (incf total-blocks (floor (- limit offset) 1024.))
        (when (> total-blocks next-print)
          (format t "~d " next-print)
          (incf next-print 10.))
        )
      (send tape-stream :close)
      (send partition-stream :close))))

(defun fast-copy-partition-to-tape (from-unit from-part tape-unit)
  (let ((partition-stream (make-new-partition-input-stream :partition-name from-part
                                                            :unit from-unit))
        (tape-stream (make-instance 'nupi-tape-output-stream :unit (get-unit tape-unit)))
        (plist))
    (send partition-stream :set-input-limit-to-measured-size)
    (using-resource (dma-buffer dma-buffer 100.)
      (setf (getf plist :style) :mit)
      (setf (getf plist :partition) t)
      (setf (getf plist :comment) (send partition-stream :label-comment))
      (setf (getf plist :size) (send partition-stream :measured-size))
      (setf (getf plist :byte-size) 16.)
      (setf (getf plist :author) user-id)
      (setf (getf plist :tape-header-length-in-bytes) 1024.)
      (send tape-stream :string-out "LMFL")
      (let ((base 10.)
            (ibase 10.)
            (*package* (find-package "USER"))
            (*readtable* (find-readtable-named nil))
            )
        (print plist tape-stream))
      (send tape-stream :pad-and-send-buffer 0 1024.)
      (do ((total-blocks 0)
           (next-print 100.))
          (())
        (let ((n-bytes (send partition-stream :string-in-direct nil dma-buffer)))
          (cond ((null n-bytes)
                 (return nil))
                (t
                 (send tape-stream :string-out-direct dma-buffer 0 n-bytes)))
          (incf total-blocks (floor n-bytes 1024.))
          (when (> total-blocks next-print)
            (format t "~d " (floor next-print 100.))
            (incf next-print 100.))))
      (send tape-stream :close)
      (send partition-stream :close))))


(defun copy-files-to-tape (tape-unit &rest files)
  (dolist (f files)
    (let ((dl (fs:directory-list f)))
      (dolist (individual-file (cdr dl))
        (with-open-file (stream (car individual-file))
          (format t "~&Writing ~a" (send stream :truename))
          (copy-stream-to-tape stream tape-unit))))))

(defun copy-files-to-tape-recursive (tape-unit &rest files &aux (nbytes 0))
  (dolist (f files)
    (let ((dl (fs:directory-list f)))
      (dolist (individual-file (cdr dl))
        (cond ((get individual-file :directory)
               (let ((pn (car individual-file))
                     (dir (send (car individual-file) :directory)))
                 (if (not (listp dir))
                     (setq dir (list dir)))
                 (copy-files-to-tape-recursive
                   tape-unit
                   (send pn :new-pathname
                         :directory (append dir (list (send pn :name)))
                         :name :wild
                         :type :wild
                         :version :newest))))
              ((not (or (eq (send (car individual-file) :canonical-type) :qfasl)
                        (string-equal (send (car individual-file) :type) "XFASL")))
               (with-open-file (stream (car individual-file))
                 (let ((size (send stream :length)))
                   (format t "~&Writing ~a, ~d bytes." (send stream :truename) size)
                   (incf nbytes size)
                   (copy-stream-to-tape stream tape-unit))))))))
  (format t "Total bytes = ~:d." nbytes))

(defun copy-stream-to-tape (input-stream tape-unit)
  (let ((tape-stream (make-instance 'nupi-tape-output-stream :unit (get-unit tape-unit)))
        plist)
    (using-resource (dma-buffer dma-buffer 100.)
      (let ((truename (send input-stream :truename)))
        (setf (getf plist :style) :mit)
        (setf (getf plist :directory) (send truename :directory))
        (setf (getf plist :name) (send truename :name))
        (setf (getf plist :type) (send truename :type))
        (setf (getf plist :version) (send truename :version))
        (if (typep (send truename :host) 'fs::lispm-host)
            (setf (getf plist :believe-size) t))
        )
      (setf (getf plist :author) (send input-stream :get :author))
      (setf (getf plist :characters) (send input-stream :characters))
      (setf (getf plist :qfaslp) (send input-stream :qfaslp))
      (setf (getf plist :length) (send input-stream :length))
      (setf (getf plist :creation-date) (send input-stream :creation-date))
      (setf (getf plist :tape-header-length-in-bytes) 1024.)
      (send tape-stream :string-out "LMFL")
      (let ((base 10.)
            (ibase 10.)
            (*package* (find-package "USER"))
            (*readtable* (find-readtable-named nil)))
        (print plist tape-stream))
      (send tape-stream :pad-and-send-buffer 0 1024.)

      ;;stolen from STREAM-COPY-UNTIL-EOF
      (do ((buf) (offset) (limit))
          (())
        (multiple-value (buf offset limit)
          (send input-stream :read-input-buffer))
        (cond ((null buf) (return nil)))
        (send tape-stream :string-out buf offset limit)
        (send input-stream :advance-input-buffer)
        (process-allow-schedule))

      (send tape-stream :pad-and-send-buffer 0 1024.)
      (send tape-stream :close)
      )))

(defun list-files-on-tape (tape-unit)
  (with-open-stream (tape-stream (make-nupi-tape-input-stream tape-unit))
    (do (
         plist)
        (())
      (setq plist (read-and-parse-tape-header tape-stream))
      (cond ((eq plist 'eof)
             (return nil))
            ((getf plist :partition)
             (format t "~&Partition ~s, ~d. blocks long, written by ~s"
                     (getf plist :comment)
                     (getf plist :size)
                     (getf plist :author)))
            (t
             (format t "~&~a, characters ~s, created ~\time\, author ~s"
                     (send (fs:make-pathname :defaults nil
                                             :host si:local-host
                                             :device nil
                                             :directory (getf plist :directory)
                                             :name (getf plist :name)
                                             :type (getf plist :type)
                                             :version (getf plist :version)
                                             )
                           :string-for-host)
                     (getf plist :characters)
                     (getf plist :creation-date)
                     (getf plist :author))))

      (send tape-stream :read-until-eof)
      (send tape-stream :clear-eof))))

(defun read-and-parse-tape-header (tape-stream &aux plist string claimed-header-length end-of-header)
  (send tape-stream :buffer-n-bytes 4096.)
  (multiple-value-bind (array start end)
      (send tape-stream :read-input-buffer)
    (if (null array)
        (return-from read-and-parse-tape-header 'eof))
    (if (< (- end start) 1024.)
        (ferror nil "header less than 1024. bytes"))

    (if (> (- end start) 4096.)
        (ferror nil "buffering code failed to limit input to 4096. bytes"))

    (setq string (make-array (array-length array) :type :art-string :displaced-to array))

    (if (not (string-equal string "LMFL" :start1 start :end1 4))
        (ferror nil "tape not positioned at a file header"))

    (let ((base 10.)
          (ibase 10.)
          (*package* (find-package "USER"))
          (*readtable* (find-readtable-named nil)))
      (setq plist (read-from-string string nil (+ start 4))))

    (do ((tail plist (cdr tail)))
        ((null tail))
      (if (and (symbolp (car tail))
               (not (keywordp (car tail)))
               (not (memq (car tail) '(t nil))))
          (setf (car tail) (intern (string (car tail)) pkg-keyword-package))))

    (setq claimed-header-length (getf plist :tape-header-length-in-bytes))

    (cond (claimed-header-length
           (cond ((= claimed-header-length 1024.)
                  (setq end-of-header (+ start 1024.)))
                 ((and (= claimed-header-length 4096.)
                       (= (- end start) 4096.))
                  (setq end-of-header end))
                 (t
                  (ferror nil ":HEADER-LENGTH-IN-BYTES property doesn't jibe with data on tape"))))
          ((< (- end start) 4096.)
           (setq end-of-header (+ start 1024.)))
          ((and (string-search-not-char 0 string (+ start 1024.) end)
                (string-search-not-char #\space string (+ start 1024.) end))
           (setq end-of-header (+ start 1024.)))
          (t
           (setq end-of-header end)))
    (send tape-stream :advance-input-buffer end-of-header)
    plist
    ))

(defun restore-tape (&key &optional
                     (unit 6)
                     (host si:local-host)
                     (query nil)
                     )
  (with-open-stream (tape-stream (make-nupi-tape-input-stream unit))
    (do (
         plist)
        (())
      (setq plist (read-and-parse-tape-header tape-stream))
      (cond ((getf plist :partition)
             (format t "~&Skipping over partition ~s, ~d. blocks long, written by ~s"
                     (getf plist :comment)
                     (getf plist :size)
                     (getf plist :author))
             (send tape-stream :read-until-eof)
             (send tape-stream :clear-eof))
            (t
             (let ((pathname (fs:make-pathname :defaults nil
                                               :host host
                                               :device nil
                                               :directory (getf plist :directory)
                                               :name (getf plist :name)
                                               :type (getf plist :type)
                                               :version (getf plist :version)
                                               ))
                   probe)
               (format t "~&~a, characters ~s, created ~\time\, author ~s"
                       pathname
                       (getf plist :characters)
                       (getf plist :creation-date)
                       (getf plist :author))
               (setq probe (open pathname :direction :probe :error nil))
               (cond ((and probe (not (errorp probe)))
                      (format t " ... a file by that name already exists, skipping.")
                      (send tape-stream :read-until-eof)
                      (send tape-stream :clear-eof))
                     ((or (null query)
                          (y-or-n-p "Ok? "))
                      (tagbody
                       retry
                          (with-open-file-case (file-stream pathname :direction :output :characters (getf plist :characters))
                            (:no-error
                             (if (getf plist :believe-size)
                                 (send tape-stream :set-total-byte-limit (getf plist :size)))
                             (send tape-stream :set-do-not-strip-nulls (or (getf plist :qfaslp)
                                                                           (null (getf plist :characters))))
                             (stream-copy-until-eof tape-stream file-stream)
                             (send tape-stream :clear-eof))
                            (fs:directory-not-found
                             (format t "~&Creating directory ~a" (send pathname :new-pathname :name nil :type nil))
                             (fs:create-directory pathname :recursive t)
                             (go retry)))))
                     (t
                      (send tape-stream :read-until-eof)
                      (send tape-stream :clear-eof)))))))))

(defun restore-partition-from-tape (tape-unit disk-unit disk-part-name &aux plist)
  (with-open-stream (partition-probe-stream (make-new-partition-input-stream :partition-name disk-part-name
                                                                             :unit disk-unit))
    (if (null (yes-or-no-p "Are you sure you want to clober ~a on ~a, ~s?"
                           disk-part-name
                           disk-unit
                           (send partition-probe-stream :label-comment)))
        (return-from restore-partition-from-tape nil)))
  (with-open-stream (tape-stream (make-nupi-tape-input-stream tape-unit))
    (setq plist (read-and-parse-tape-header tape-stream))
    (if (not (getf plist :partition))
        (ferror nil "current file is not a partition"))
    (restore-partition-from-tape-internal tape-stream plist disk-unit disk-part-name)))

(defun restore-partition-from-tape-internal (tape-stream plist disk-unit part-name)
  plist
  (let ((partition-stream (make-new-partition-output-stream :partition-name part-name
                                                            :unit disk-unit)))
    (send tape-stream :set-do-not-strip-nulls t)
    (do ((buf) (offset) (limit) (total-blocks 0) (next-print 10.))
        (())
      (multiple-value (buf offset limit)
        (send tape-stream :read-input-buffer))
      (cond ((null buf) (return nil)))
      (send partition-stream :string-out buf offset limit)
      (send tape-stream :advance-input-buffer)
      (incf total-blocks (floor (- limit offset) 1024.))
      (when (> total-blocks next-print)
        (format t "~d " next-print)
        (incf next-print 10.))
      )
    (send tape-stream :close)
    (send partition-stream :close)))
