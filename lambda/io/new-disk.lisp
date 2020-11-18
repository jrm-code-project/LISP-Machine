;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

;(compiler:defmic %io-cmd-run 1150 (io-cmd) t)

(mapcar #'(lambda (x) (putprop x t 'special)) nupi-disk-rq-halfwords)
(assign-values nupi-disk-rq-halfwords)

(compiler:make-obsolete wire-page "use %wire-page with different conventions")
(compiler:make-obsolete unwire-page "use %unwire-page with different conventions")
(compiler:make-obsolete wire-words "use %wire-words with different conventions")
(compiler:make-obsolete unwire-words "use %unwire-words with different conventions")
(compiler:make-obsolete wire-array "use %wire-structure with different conventions")
(compiler:make-obsolete unwire-array "use %unwire-structure with different conventions")
(compiler:make-obsolete wire-structure "use %wire-structure with different conventions")
(compiler:make-obsolete unwire-structure "use %unwire-structure with different conventions")
(compiler:make-obsolete wire-area "use %wire-area")
(compiler:make-obsolete unwire-area "use %unwire-area")

;;Strategy:
;;Call %CHANGE-PAGE-STATUS to set the swap status to wired.  If the page
;;is swapped out, then this will return NIL.  If that happens, we have to
;;get the page into the PAGE-HASH-TABLE somehow.  If DONT-BOTHER-PAGING-IN
;;is set, we don't want to fault it in, so allocate a physical page, and
;;use %PAGE-IN to enter it into the PAGE-HASH-TABLE.  Otherwise, just
;;touch it.
(defun %wire-page (address &optional (wire-p t) set-modified dont-bother-paging-in)
  (cond ((null wire-p)
         (%unwire-page address))
        (t
         (without-interrupts
           (loop until (%change-page-status address %pht-swap-status-wired nil)
                 finally (if set-modified
                             (%p-dpb (%p-ldb (byte 1 0) address) (byte 1 0) address))
                 when dont-bother-paging-in
                 ;;"create" an uninitialized page
                   do (let* ((vpage (lsh address -8.))
                             (pfn (if (ldb-test %%processor-switch-fast-cache-mode
                                                (%processor-switches nil))
                                      (%findcore-hexadec (logand #o17 vpage))
                                    (%findcore))))
                        (cond ((null (%page-in pfn vpage))
                               ;;it got paged in behind our back, just free the physical page
                               (%create-physical-page (lsh pfn 8)))))
                 else
                 ;;Touch page to get it in
                   do (%p-ldb (byte 1 0) (%pointer address)))))))


(defun %unwire-page (address)
  (%change-page-status address %pht-swap-status-normal nil))

(defun %wire-words (from size &optional (wire-p t) set-modified dont-bother-paging-in)
  (if (not (zerop (logand (%pointer from) (1- page-size))))
      (ferror nil "must wire from a page boundary"))
  (if (not (zerop (logand size (1- page-size))))
      (ferror nil "must wire a multiple of the page size"))

  (do ((adr (%pointer from) (%pointer-plus adr page-size))
       (pages-to-go (floor size page-size) (1- pages-to-go)))
      ((zerop pages-to-go))
    (%wire-page adr wire-p set-modified dont-bother-paging-in)))

(defun %unwire-words (from size)
  (%wire-words from size nil))

(defun %wire-structure (obj &optional set-modified dont-bother-paging-in)
  (setq obj (%find-structure-leader (follow-structure-forwarding obj)))
  (without-interrupts
    (%wire-words obj
                 (%structure-total-size obj)
                 t set-modified dont-bother-paging-in)))

(defun %unwire-structure (obj)
  (setq obj (%find-structure-leader (follow-structure-forwarding obj)))
  (without-interrupts
    (%unwire-words obj (%structure-total-size obj))))

;;; convert from explorer logical unit to physical unit
(defun convert-logical-unit-to-physical-unit (logical-unit)
  (dpb (ldb (BYTE 3. 1.) logical-unit) (BYTE 3. 3.) (ldb (BYTE 1. 0.) logical-unit)))

(defstruct (io-cmd
             (:type :array-leader)
             )
  ;;these first four are the same as RQB's
  io-cmd-n-halfwords
  io-cmd-n-pages
  io-cmd-buffer
  io-cmd-8-bit-buffer

  io-cmd-command-block
  io-cmd-first-command-block-address
  io-cmd-first-data-address
  )

;** make sure not to create an io-cmd that has too many pages for the scatter list
(defun create-io-cmd (ignore n-pages)
  (let* ((n-half-words-for-user-data (* 2 n-pages page-size))
         (sample-io-cmd (make-io-cmd))
         (io-cmd-basic-size (%structure-total-size (%find-structure-leader sample-io-cmd)))
         (n-half-words-left-on-first-page (* 2 (- page-size io-cmd-basic-size)))
         io-cmd io-cmd-leader-pointer
         )
    (cond ((> (+ n-half-words-for-user-data n-half-words-left-on-first-page)
              %array-max-short-index-length)
           (incf io-cmd-basic-size)
           (decf n-half-words-left-on-first-page 2)
           (cond ((<= (+ n-half-words-for-user-data n-half-words-left-on-first-page)
                      %array-max-short-index-length)
                  (ferror nil "pathalogical io-cmd size -- can't fit")))))
    (setq io-cmd (make-array (+ n-half-words-for-user-data
                                n-half-words-left-on-first-page)
                             :type art-16b
                             :leader-length (array-leader-length sample-io-cmd)
                             :area disk-buffer-area
                             ))
    (setq io-cmd-leader-pointer (%find-structure-leader io-cmd))

    (cond ((not (zerop (logand (1- page-size) (%pointer io-cmd-leader-pointer))))
           (ferror nil "io-cmd doesn't start at beginning of page")))
    (cond ((not (zerop (remainder (%structure-total-size io-cmd-leader-pointer) page-size)))
           (ferror nil "io-cmd not multiple of page size")))

    (setf (io-cmd-n-pages io-cmd) n-pages)
    (setf (io-cmd-buffer io-cmd)
          (make-array n-half-words-for-user-data
                      :type art-16b
                      :displaced-to io-cmd
                      :displaced-index-offset n-half-words-left-on-first-page
                      ))
    (setf (io-cmd-8-bit-buffer io-cmd)
          (make-array (* 2 n-half-words-for-user-data)
                      :type art-string
                      :displaced-to io-cmd
                      :displaced-index-offset (* 2 n-half-words-left-on-first-page)
                      ))
    (setf (io-cmd-command-block io-cmd)
          (make-array n-half-words-left-on-first-page
                      :type art-16b
                      :displaced-to io-cmd))

    (setf (io-cmd-first-command-block-address io-cmd)
          (%pointer-plus io-cmd-leader-pointer io-cmd-basic-size))
    (setf (io-cmd-first-data-address io-cmd)
          (%pointer-plus io-cmd-leader-pointer page-size))

    io-cmd
    ))

(defresource io-cmd (n-pages)
  :constructor create-io-cmd
  :free-list-size 50.)

(defun get-io-cmd (&optional (n-pages 1))
  (let ((default-cons-area working-storage-area)) ;avoid lossage on consing in
                                                  ; resource stuff. (specifically parametizer)
    (allocate-resource 'io-cmd n-pages)))

(defun return-io-cmd (io-cmd)
  (cond ((not (null io-cmd))
         (%unwire-structure io-cmd)
         (deallocate-resource 'io-cmd io-cmd)))
  nil)

(defun vadr-to-nubus-phys (vadr)
  (dpb (%nubus-physical-address (ldb (byte 22. 8) (%physical-address vadr)))
       (byte 22. 10.)
       (dpb (%pointer vadr)
            (byte 8 2)
            0)))

;;; +++ is this used? does the user want a logical of physical unit number?
;;; +++ 3/18/86 ptm
(defmacro nupi-unit (io-cmd)
  `(aref (io-cmd-command-block ,io-cmd) %nupi-disk-unit))

(defmacro nupi-command (io-cmd)
  `(aref (io-cmd-command-block ,io-cmd) %nupi-disk-command))

(defmacro nupi-disk-status (io-cmd)
  `(dpb (aref (io-cmd-command-block ,io-cmd) %nupi-disk-status-hi)
        (byte 16. 16.)
        (aref (io-cmd-command-block ,io-cmd) %nupi-disk-status-lo)))

(defmacro set-nupi-disk-status (io-cmd val)
  `(progn (aset (ldb (byte 16. 0) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-status-lo)
          (aset (ldb (byte 16. 16.) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-status-hi)))

(defsetf nupi-disk-status set-nupi-disk-status)

(defmacro nupi-list-pointer (io-cmd)
  `(dpb (aref (io-cmd-command-block ,io-cmd) %nupi-disk-list-pointer-hi)
        (byte 16. 16.)
        (aref (io-cmd-command-block ,io-cmd) %nupi-disk-list-pointer-lo)))

(defmacro set-nupi-disk-list-pointer (io-cmd val)
  `(progn (aset (ldb (byte 16. 0) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-list-pointer-lo)
          (aset (ldb (byte 16. 16.) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-list-pointer-hi)))

(defsetf nupi-disk-list-pointer set-nupi-disk-list-pointer)

(defmacro nupi-disk-transfer-count (io-cmd)
  `(dpb (aref (io-cmd-command-block ,io-cmd) %nupi-disk-transfer-count-hi)
        (byte 16. 16.)
        (aref (io-cmd-command-block ,io-cmd) %nupi-disk-transfer-count-lo)))

(defmacro set-nupi-disk-transfer-count (io-cmd val)
  `(progn (aset (ldb (byte 16. 0) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-transfer-count-lo)
          (aset (ldb (byte 16. 16.) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-transfer-count-hi)))

(defsetf nupi-disk-transfer-count set-nupi-disk-transfer-count)

(defmacro nupi-disk-logical-block (io-cmd)
  `(dpb (aref (io-cmd-command-block ,io-cmd) %nupi-disk-logical-block-hi)
        (byte 16. 16.)
        (aref (io-cmd-command-block ,io-cmd) %nupi-disk-logical-block-lo)))

(defmacro set-nupi-disk-logical-block (io-cmd val)
  `(progn (aset (ldb (byte 16. 0) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-logical-block-lo)
          (aset (ldb (byte 16. 16.) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-logical-block-hi)))

(defsetf nupi-disk-logical-block set-nupi-disk-logical-block)


(defmacro nupi-disk-interrupt-address (io-cmd)
  `(dpb (aref (io-cmd-command-block ,io-cmd) %nupi-disk-interrupt-address-hi)
        (byte 16. 16.)
        (aref (io-cmd-command-block ,io-cmd) %nupi-disk-interrupt-address-lo)))

(defmacro set-nupi-disk-interrupt-address (io-cmd val)
  `(progn (aset (ldb (byte 16. 0) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-interrupt-address-lo)
          (aset (ldb (byte 16. 16.) ,val) (io-cmd-command-block ,io-cmd) %nupi-disk-interrupt-address-hi)))

(defsetf nupi-disk-interrupt-address set-nupi-disk-interrupt-address)

;io-cmd must be wired before calling this
(defun build-nupi-command (io-cmd phys-unit command byte-count disk-address
                           &optional
                           (scatter-p t)
                           (byte-offset 0)
                           )
  (let ((page-status (%page-status io-cmd)))
    (cond ((null page-status)
           (ferror nil "io-cmd is swapped out!!"))
          ((not (= (ldb %%pht1-swap-status-code page-status) %pht-swap-status-wired))
           (ferror nil "io-cmd is not wired!!"))))

  (if (not (zerop (ldb (byte 10. 0) byte-count)))
      (ferror nil "byte-count must be an even number of pages"))
  (if (not (zerop (ldb (byte 10. 0) byte-offset)))
      (ferror nil "offset must be an even number of pages"))
  (if (> (+ byte-count byte-offset) (* 4 page-size (io-cmd-n-pages io-cmd)))
      (ferror nil "byte-count too big"))

  (array-initialize (io-cmd-command-block io-cmd) 0)

  (aset phys-unit (io-cmd-command-block io-cmd) %nupi-disk-unit)
  (cond ((null scatter-p)
         (if (not (zerop byte-count))
             (ferror nil "can't do non-scatter operations with non-zero byte-counts"))
         (aset (dpb command (byte 8 8) 0) (io-cmd-command-block io-cmd) %nupi-disk-command)
         (let ((data-phys-adr (vadr-to-nubus-phys (io-cmd-first-data-address io-cmd))))
           (aset (ldb (byte 16. 0) data-phys-adr) (io-cmd-command-block io-cmd) %nupi-disk-ccw-list-pointer-lo)
           (aset (ldb (byte 16. 16.) data-phys-adr) (io-cmd-command-block io-cmd) %nupi-disk-ccw-list-pointer-hi)))
        (t
         (aset (dpb command (byte 8 8) #o100) (io-cmd-command-block io-cmd) %nupi-disk-command)
         (let ((scatter-phys-adr (vadr-to-nubus-phys (+ (io-cmd-first-command-block-address io-cmd)
                                                        (floor %nupi-disk-ccw-list 2)))))
           (aset (ldb (byte 16. 0) scatter-phys-adr) (io-cmd-command-block io-cmd) %nupi-disk-ccw-list-pointer-lo)
           (aset (ldb (byte 16. 16.) scatter-phys-adr) (io-cmd-command-block io-cmd) %nupi-disk-ccw-list-pointer-hi))))
  (aset (ldb (byte 16. 0) byte-count) (io-cmd-command-block io-cmd) %nupi-disk-transfer-count-lo)
  (aset (ldb (byte 16. 16.) byte-count) (io-cmd-command-block io-cmd) %nupi-disk-transfer-count-hi)
  (aset (ldb (byte 16. 0) disk-address) (io-cmd-command-block io-cmd) %nupi-disk-logical-block-lo)
  (aset (ldb (byte 16. 16.) disk-address) (io-cmd-command-block io-cmd) %nupi-disk-logical-block-hi)

  (when scatter-p
    (do ((vadr (%pointer-plus (io-cmd-first-data-address io-cmd)
                              (floor byte-offset 4))
               (%pointer-plus vadr page-size))
         (scatter-entry %nupi-disk-ccw-list (+ scatter-entry 4))
         (pages-to-go (floor byte-count 1024.) (1- pages-to-go)))
        ((zerop pages-to-go))
      (let ((padr (vadr-to-nubus-phys vadr)))
        (aset (ldb (byte 16. 0) padr) (io-cmd-command-block io-cmd) scatter-entry)
        (aset (ldb (byte 16. 16.) padr) (io-cmd-command-block io-cmd) (+ scatter-entry 1))
        (aset 1024. (io-cmd-command-block io-cmd) (+ scatter-entry 2))
        (aset 0 (io-cmd-command-block io-cmd) (+ scatter-entry 3)))))

  )

(defconst nupi-commands
          '((#x00 "")
            (#x41 "formatter set up")
            (#x81 "NUPI set up")
            (#x02 "Request device status")
            (#x42 "Request Formatter Status")
            (#x82 "Request NUPI Status")
            (#x10 "Restore Device")
            (#x11 "Seek")
            (#x12 "Read")
            (#x13 "Write")
            (#x14 "Format")
            (#x15 "Read to NUPI Buffer")
            (#x16 "NUPI Buffer to Nubus Transfer")
            (#x17 "Swap NUPI Buffer and Write")
            (#x20 "Rewind (tape)")
            (#x21 "Unload (tape)")
            (#x22 "Erase (tape)")
            (#x23 "Space forward (tape)")
            (#x25 "Write Filemark (tape)")
            (#x26 "Tape retension")
            (#x27 "Space forward by filemarks")
            (#x29 "Skip forward to end of data")
            (#x2a "Load")
            (#x2b "Load/unload cmd with bit params")
            (#x30 "Device selftest")
            (#x70 "Formatter selftest")
            (#xb0 "NUPI selftest")
            (#x71 "SCSI pass through read")
            (#x72 "SCSI pass through write")
            ))

(defconst nupi-error-classes '("no error"
                               "self test error"
                               "attention error"
                               "bus related error"
                               "command error"
                               "hardware error"
                               "media error"
                               "reserved error"))

(defconst nupi-controller-errors
          '((#x61 "nubus time out")
            (#x62 "nubus bus error")
            (#x63 "scsi bus parity error")
            (#x64 "formatter busy")
            (#x65 "rate error")
            (#x66 "bus error trap")
            (#x81 "command aborted")
            (#x82 "invalid command")
            (#x83 "invalid parameter")
            (#x84 "scsi command completed without data transfer")
            (#x8c "attempted read-and-hold with one pending")
            (#x8d "attempeted write-and-swap without valid buffer")
            (#xa1 "illegal interrupt")
            (#xa2 "scsi function complete without cause")
            (#xa3 "timeout on NCR 5385 data register full wait")
            (#xa4 "scsi invalid command interrupt")
            (#xa6 "hardware error trap")
            (#xa7 "queue overflow")
            (#xa8 "address error trap")
            (#xa9 "illegal instruction error trap")
            (#xaa "nubus dma locked up")
            ))

(defconst nupi-device-errors
          '((#x41 "no selected unit")
            (#x42 "media not loaded")
            (#x43 "write protected")
            (#x44 "offline device now online or media change")
            (#x46 "temperature fault")
            (#x47 "invalid media type")
            (#x48 "status: scsi sense data available")
            (#x49 "status: tape incorrect length indicate")
            (#x4a "tape end of media")
            (#x4b "status: tape incorrect length indicate and end of media")
            (#x4c "status: tape file mark detected")
            (#x4d "status: tape incorrect length indicate and file mark detected")
            (#x4e "status: tape end of media and file mark detected")
            (#x4f "status: tape incorrect length indicate, end of media, and file mark detected")
            (#x50 "scsi bus hung needs hardware reset")
            (#x61 "scsi bus parity error")
            (#x62 "drive not ready")
            (#x63 "rate error")
            (#x64 "invalid scsi interrupt: selected")
            (#x65 "device offline")
            (#x66 "invalid scsi testability interrupt")
            (#x67 "invalid scsi disconnect")
            (#x68 "invalid mode for scsi status")
            (#x69 "invalid mode for scsi command byte request")
            (#x6a "sequence error: scsi completion address")
            (#x6b "sequence error: scsi requested data")
            (#x6c "sequence error: dma start/stop address")
            (#x70 "unknown message received from formatter")
            (#x71 "invalid mode on scsi message in")
            (#x72 "excess scsi status")
            (#x73 "excess scsi command bytes requested")
            (#x74 "expected scsi restore message but note received")
            (#x75 "reconnected to unit not waiting for it")
            (#x76 "expected scsi cmd complete msg; did not receive it")
            (#x77 "illegal scsi message for reconnected state")
            (#x78 "reselected without valid scsi id")
            (#x79 "invalid mode on scsi message out")
            (#x7a "invalid mode on scsi data transfer")
            (#x81 "command aborted")
            (#x82 "invalid command")
            (#x83 "invalid parameter")
            (#x84 "illegal block address")
            (#x85 "volume overflow")
            (#x8a "formatter failed to connect to scsi bus")
            (#x8e "unknown error code feturned from the formatter")
            (#xa1 "missing index signal")
            (#xa2 "no seek complete")
            (#xa3 "write fault")
            (#xa4 "track 0 not found")
            (#xa5 "multiple units selected")
            (#xa6 "seek error")
            (#xa7 "formatter hardware error")
            (#xc1 "id error")
            (#xc2 "uncorrectable data error")
            (#xc3 "id address mark not found")
            (#xc4 "data address mark not found")
            (#xc5 "block not found (sector address)")
            (#xc6 "bad block not found")
            (#xc7 "format error")
            (#xc8 "corrrectable data check")
            (#xc9 "interleave error")
            (#xca "media error")))


(defun print-nupi-command (io-cmd)
  (let* ((command-word (dpb (aref io-cmd 1) (byte 16. 16.) (aref io-cmd 0)))
         (status (dpb (aref io-cmd 3) (byte 16. 16.) (aref io-cmd 2)))
         (parameter-list (dpb (aref io-cmd 5) (byte 16. 16.) (aref io-cmd 4)))
         (transfer-count (dpb (aref io-cmd 7) (byte 16. 16.) (aref io-cmd 6)))
         (device-address (dpb (aref io-cmd 9.) (byte 16. 16.) (aref io-cmd 8.)))
         (interrupt-address (dpb (aref io-cmd 11.) (byte 16. 16.) (aref io-cmd 10.)))
         (reserved-1 (dpb (aref io-cmd 13.) (byte 16. 16.) (aref io-cmd 12.)))
         (reserved-2 (dpb (aref io-cmd 15.) (byte 16. 16.) (aref io-cmd 14.)))
         (scatter-p (ldb-test (byte 1 22.) command-word))
         )
    (format t "~&Physical Unit ~s" (ldb (byte 8 0) command-word))
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

    (format t "~&parameter-list #x~16r" parameter-list)

    (format t "~&Transfer count ~d." transfer-count)
    (format t "~&Device block address ~s" device-address)
    (format t "~&Interrupt address #x~16r" interrupt-address)
    (format t "~&Reserved ~s ~s" reserved-1 reserved-2)

    (when scatter-p
      (format t "~&Scatter list: ")
      (let ((cmd io-cmd))
        (do ((scatter-index %nupi-disk-ccw-list (+ scatter-index 4))
             (pages-to-go (floor transfer-count 1024.) (1- pages-to-go)))
            ((zerop pages-to-go))
          (format t "~&#x~8x ~d."
                  (dpb (aref cmd (+ scatter-index 1))
                       (byte 16. 16.)
                       (aref cmd scatter-index))
                  (dpb (aref cmd (+ scatter-index 3))
                       (byte 16. 16.)
                       (aref cmd (+ scatter-index 2)))))))))


;;; Built on top of disk-error.
(DefSignal NuPI-Error eh:disk-error nil)

(defun nupi-disk-wait (io-cmd)
  (process-wait "Disk Wait"
                #'(lambda (io-cmd)
                    (ldb-test (byte 1 14.) (aref (io-cmd-command-block io-cmd) %nupi-disk-status-hi)))
                io-cmd)
  (When (ldb-test (byte 1 13.) (aref (io-cmd-command-block io-cmd) %nupi-disk-status-hi))
    ;;(ferror nil "disk error")
    ;; need some args to describe error...
    (Ferror 'NuPI-Error "NuPI request failed."))
  )

(defun nupi-disk-command (io-cmd command phys-unit disk-address byte-count &optional (scatter-p t))
  (unwind-protect
      (progn
        ;;** someday, just wire the part we are going to use
        (%wire-structure (%find-structure-leader io-cmd) (eq command #x12))
        (build-nupi-command io-cmd phys-unit command byte-count disk-address scatter-p)
        (when scatter-p
          (verify-nupi-command io-cmd))
        (%io-cmd-run io-cmd)
        (nupi-disk-wait io-cmd)
        )
    (%unwire-structure (%find-structure-leader io-cmd)))
  io-cmd)

(defun nupi-tape-command (io-cmd command phys-unit disk-address byte-count)
  (unwind-protect
      (progn
        (%wire-structure io-cmd)
        (do ((byte-offset 0 (+ byte-offset 1024.))
             (block-number 0 (1+ block-number))
             (end-block-number (floor byte-count 1024.))
             )
            ((= block-number end-block-number)
             (setf (nupi-disk-transfer-count io-cmd) byte-count))
          (build-nupi-command io-cmd phys-unit command 1024. disk-address t byte-offset)
          (verify-nupi-command io-cmd)
          (%io-cmd-run io-cmd)
          (nupi-disk-wait io-cmd)
          (when (not (zerop (ldb (byte 8 8) (nupi-disk-status io-cmd))))
            (setf (nupi-disk-transfer-count io-cmd) (* block-number 1024.))
            (return))
          ))
    (%unwire-structure io-cmd))
  io-cmd)

(defun test-nupi-disk-command (io-cmd command phys-unit disk-address byte-count &optional (scatter-p t))
  (unwind-protect
      (progn
        (%wire-structure io-cmd)
        (build-nupi-command io-cmd phys-unit command byte-count disk-address scatter-p)
        (when scatter-p
          (verify-nupi-command io-cmd))
        )
    (%unwire-structure io-cmd))
  io-cmd)

(defun verify-nupi-command (io-cmd)
  (let* ((command-block-adr (vadr-to-nubus-phys (io-cmd-first-command-block-address io-cmd)))
         (command-block-slot (ldb (byte 8 24.) command-block-adr))
         (command-block-offset (ldb (byte 24. 0) command-block-adr))
         (parameter-list-adr (%nubus-read command-block-slot
                                          (+ command-block-offset 8)))
         (parameter-list-slot (ldb (byte 8 24.) parameter-list-adr))
         (parameter-list-offset (ldb (byte 24. 0) parameter-list-adr))
         )
    (if (not (= command-block-slot parameter-list-slot))
        (ferror nil "bad nupi command - command block and parameter list on different slots"))
    (if (not (= (+ command-block-offset (* 8. 4)) parameter-list-offset))
        (ferror nil "bad nupi command - parameter list doesn't follow command-block"))
    (do ((pages-to-go (floor (%nubus-read command-block-slot
                                          (+ command-block-offset 12.))
                             1024.)
                      (1- pages-to-go))
         (page-number 0 (1+ page-number))
         )
        ((zerop pages-to-go))
      (let ((adr (%nubus-read parameter-list-slot
                              (+ parameter-list-offset (* page-number 8))))
            (size (%nubus-read parameter-list-slot
                               (+ parameter-list-offset (+ (* page-number 8) 4)))))
        (if (not (= size 1024.))
            (ferror nil "bad nupi command: scatter list doesn't have size 1024."))
        (if (and (not (= (ldb (byte 8 24.) adr) #xf3))
                 (not (= (ldb (byte 8 24.) adr) #xf4)))
            (ferror nil "bad nupi scatter address #x~x" adr))))))

(defun nupi-disk-read (io-cmd logical-unit disk-address n-pages)
  (nupi-disk-command io-cmd #x12 (convert-logical-unit-to-physical-unit logical-unit) disk-address (* n-pages 1024.))
  )

(defun nupi-disk-write (io-cmd logical-unit disk-address n-pages)
  (nupi-disk-command io-cmd #x13 (convert-logical-unit-to-physical-unit logical-unit) disk-address (* n-pages 1024.))
  )

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
  (check-type type (member art-1b art-2b art-4b art-8b art-16b art-32b art-string art-inum))
  (check-type named-structure-symbol symbol)

  (let* ((array-length (* n-pages page-size (cdr (assq type array-elements-per-q))))
         (long-length (if (> array-length %array-max-short-index-length) 1 0)))
    (with-consing-allowed-in-wireable-structures-area
      (make-array array-length
                  :type type
                  :leader-length (max 2         ;be sure named-structure-symbol is in leader
                                      (- page-size
                                         1      ;for leader header
                                         1      ;for leader length q
                                         1      ;for array header
                                         long-length    ;for long length q
                                         ))
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
                      :displaced-to dma-buffer
                      :leader-length 2
                      :named-structure-symbol 'dma-buffer-16b))
    (setf (dma-buffer-8b dma-buffer)
          (make-array (* n-pages page-size 4)
                      :type :art-8b
                      :displaced-to dma-buffer
                      :leader-length 2
                      :named-structure-symbol 'dma-buffer-8b))
    (setf (dma-buffer-string dma-buffer)
          (make-array (* n-pages page-size 4)
                      :type :art-string
                      :displaced-to dma-buffer
                      :leader-length 2
                      :named-structure-symbol 'dma-buffer-string))
    ;;
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
    (setf (array-leader (dma-buffer-16b dma-buffer) 1) 'dma-buffer-16b)
    (setf (array-leader (dma-buffer-8b dma-buffer) 1) 'dma-buffer-8b)
    (setf (array-leader (dma-buffer-string dma-buffer) 1) 'dma-buffer-string)
    dma-buffer))

(defun free-dma-buffer (dma-buffer)
  (setf (dma-buffer-named-structure-symbol dma-buffer) 'dma-buffer)
    (setf (array-leader (dma-buffer-16b dma-buffer) 1) 'dma-buffer-16b)
    (setf (array-leader (dma-buffer-8b dma-buffer) 1) 'dma-buffer-8b)
    (setf (array-leader (dma-buffer-string dma-buffer) 1) 'dma-buffer-string)
  (deallocate-resource 'dma-buffer dma-buffer))
