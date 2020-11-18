;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:8; Lowercase:T -*-

(defstruct (interphase-iopb
             (:type :array)
             (:constructor nil)
             (:conc-name ip-iopb-)
             )
  command
  options
  status
  error
  unit
  head
  cylinder-hi
  cylinder-lo
  block-hi
  block-lo
  nblocks-hi
  nblocks-lo
  dma-count
  dma-adr-hi
  dma-adr-med
  dma-adr-lo
  io-adr-hi
  io-adr-lo
  relative-adr-hi
  relative-adr-lo
  reserved
  linked-iopb-hi
  linked-iopb-med
  linked-iopb-lo
  )

(defun build-interphase-command (iopb dma-buffer multibus-mapping-register-base unit command byte-count
                                 head cylinder sector
                                 &optional
                                 (byte-offset 0)
                                 )
  (when (or (not (arrayp iopb))
            (not (array-has-leader-p iopb))
            (not (eq (array-leader iopb 1) 'interphase-iopb)))
    (ferror nil "iopb must be an interphase-iopb"))
  (when (or (not (arrayp dma-buffer))
            (not (array-has-leader-p dma-buffer))
            (not (eq (array-leader dma-buffer 1) 'dma-buffer)))
    (ferror nil "dma-buffer must be a dma-buffer"))
  (let ((page-status (%page-status iopb)))
    (cond ((null page-status)
           (ferror nil "iopb is swapped out!!"))
          ((not (= (ldb %%pht1-swap-status-code page-status) %pht-swap-status-wired))
           (ferror nil "iopb is not wired!!"))))

  (when (not (zerop (ldb (byte 10. 0) byte-count)))
    (ferror nil "byte count must be even number of pages"))

  (when (not (zerop (ldb (byte 10. 0) byte-offset)))
    (ferror nil "byte-offset must be page aligned"))

  (when (> (+ byte-count byte-offset) (* 1024. (dma-buffer-size-in-pages dma-buffer)))
    (ferror nil "dma-buffer too small"))

  (do ((i 0 (1+ i))
       (adr (%pointer-plus (dma-buffer-data-vadr dma-buffer)
                           (floor byte-offset 1024.))
            (%pointer-plus adr #o400))
       (n-pages (floor byte-count 1024.)))
      ((= i n-pages))
    (let ((page-status (%page-status adr)))
      (cond ((null page-status)
             (ferror nil "a page in the dma-buffer is swapped out!!"))
            ((not (= (ldb %%pht1-swap-status-code page-status) %pht-swap-status-wired))
             (ferror nil "a page in the dma-buffer is not wired!!"))))
    (write-multibus-mapping-register
      (+ multibus-mapping-register-base i)
      (dpb 1 (byte 1 23.) (ldb (byte 22. 10.) (vadr-to-nubus-phys adr)))))

  (setf (ip-iopb-command iopb) command)
  (setf (ip-iopb-options iopb) #o21)
  (setf (ip-iopb-status iopb) 0)
  (setf (ip-iopb-error iopb) 0)
  (setf (ip-iopb-unit iopb) unit)

  (setf (ip-iopb-head iopb) head)
  (setf (ip-iopb-cylinder-hi iopb) (ldb (byte 8 8) cylinder))
  (setf (ip-iopb-cylinder-lo iopb) (ldb (byte 8 0) cylinder))
  (setf (ip-iopb-block-hi iopb) (ldb (byte 8 8) sector))
  (setf (ip-iopb-block-lo iopb) (ldb (byte 8 0) sector))

  (let ((blocks (truncate byte-count 1024.)))
    (setf (ip-iopb-nblocks-hi iopb) (ldb (byte 8 8) blocks))
    (setf (ip-iopb-nblocks-lo iopb) (ldb (byte 8 0) blocks)))

  (setf (ip-iopb-dma-count iopb) 64.)
  (let ((io-adr #o100))
    (setf (ip-iopb-io-adr-hi iopb) (ldb (byte 8 8) io-adr))
    (setf (ip-iopb-io-adr-lo iopb) (ldb (byte 8 0) io-adr)))

  (setf (ip-iopb-relative-adr-hi iopb) 0)
  (setf (ip-iopb-relative-adr-lo iopb) 0)
  (setf (ip-iopb-reserved iopb) 0)
  (setf (ip-iopb-linked-iopb-hi iopb) 0)
  (setf (ip-iopb-linked-iopb-med iopb) 0)
  (setf (ip-iopb-linked-iopb-lo iopb) 0)

  (let ((multibus-adr (* multibus-mapping-register-base  1024.)))
    (setf (ip-iopb-dma-adr-hi iopb) (ldb (byte 8 16.) multibus-adr))
    (setf (ip-iopb-dma-adr-med iopb) (ldb (byte 8 8) multibus-adr))
    (setf (ip-iopb-dma-adr-lo iopb) (ldb (byte 8 0) multibus-adr)))

  )


(defun interphase-disk-command (iopb dma-buffer multibus-mapping-register-base
                                command unit head cylinder sector n-pages page-offset)
  (unwind-protect
      (progn
        (%wire-structure dma-buffer)
        (build-interphase-command
          iopb
          dma-buffer
          multibus-mapping-register-base
          unit
          command
          (* n-pages 1024.)
          head
          cylinder
          sector
          (* page-offset 1024.))
        (share-disk-go iopb)
        (share-disk-wait iopb))
    (%unwire-structure dma-buffer)
    ))

(DEFCONST IP-ERROR-CODES-ALIST
  '((0 . "everything ok, or the controller hasn't looked yet")
    (#16R10 . "disk-not-ready")
    (#16R11 . "invalid disk address")
    (#16R12 . "seek error")
    (#16R13 . "ecc code error-data field")
    (#16R14 . "invalid command code")
    (#16R15 . "/"unused/"")
    (#16R16 . "invalid sector in command")
    (#16R17 . "/"spare/"")
    (#16R18 . "bus timeout")
    (#16R19 . "/"not used/"")
    (#16R1A . "disk write-proctected")
    (#16R1B . "unit not selected")
    (#16R1C . "no address mark - header field")
    (#16R1D . "/"not used/"")
    (#16R1E . "drive faulted")
    (#16R1F . "/"not used/"")
    (#16R20 . "/"not used/"")
    (#16R21 . "/"not used/"")
    (#16R22 . "/"not used/"")
    (#16R23 . "uncorrectable error")
    (#16R24 . "/"spare/"")
    (#16R25 . "/"spare/"")
    (#16R26 . "no sector pulse")
    (#16R27 . "data overrun")
    (#16R28 . "no index pulse on write format")
    (#16R29 . "sector not found")
    (#16R2A . "id field error-wrong head")
    (#16R2B . "invalid sync in data field")
    (#16R2D . "seek timeout error")
    (#16R2E . "busy timeout")
    (#16R2F . "not on cylinder")
    (#16R30 . "rtz timeout")
    (#16R31 . "format overrun on data")
    (#16R40 . "unit not initialized")
    (#16R42 . "gap specification error")
    (#16R4B . "seek error")
    (#16R4C . "mapped header error")
    (#16R50 . "sector per track error in UIB, 2190 only")
    (#16R51 . "bytes//sector speccification error")
    (#16R52 . "interleave specification error")
    (#16R53 . "invalid head address")
    (#16R54 . "invalid DMA burst count, 2190 only")))

(defun share-disk-wait (iopb)
  (process-wait "Disk Wait"
                #'(lambda (x)
                    (and (not (= (ip-iopb-status x) #x81))
                         (not (= (ip-iopb-status x) 0))))
                iopb)
  (when (not (= (ip-iopb-status iopb) #x80))
    (ferror nil "disk error: ~a"
            (cdr (assq (ip-iopb-error iopb) ip-error-codes-alist)))))

(defun print-ip-error (error-number)
  (cond ((not (zerop error-number))
         (format t "~%ip disk error  #16r~16r: " error-number)
         (let ((error-message (assoc error-number ip-error-codes-alist)))
           (cond ((null error-message)
                  (format t "not listed in manual"))
                 (t
                  (format t (cdr error-message))))))
        (t (format t "~%no errors yet"))))


;;; routines to support shared disk   -pace 3/21/84

(defun multibus-address-to-8086-ptr (adr)
  (dpb (ldb (byte 16. 4) adr)
       (byte 16. 16.)
       (ldb (byte 4. 0) adr)))

(defun 8086-ptr-to-multibus-address (ptr)
  (+ (ash (ldb (byte 16. 16.) ptr) 4)
     (ldb (byte 16. 0) ptr)
     #xff000000))


(defun sharestruct-ptr ()
  (%system-configuration-share-struct-pointer *sys-conf*))

(defun sharestruct-max-iopbs ()
  (let ((nubus-adr (+ (sharestruct-ptr) 4)))
    (let ((result (%nubus-read (ldb (byte 8 24.) nubus-adr)
                               (ldb (byte 24. 0) nubus-adr))))
      (when (or (< result 0)
                (>= result 64.))
        (ferror nil "too many iopbs"))
      result)))

(defun sharestruct-slot-valid (slot-number)
  (when (or (< slot-number 0)
            (>= slot-number (sharestruct-max-iopbs)))
    (ferror nil "bad slot number"))
  (let ((nubus-adr (+ (sharestruct-ptr) 12. (* slot-number 4))))
    (%nubus-read (ldb (byte 8 24.) nubus-adr)
                 (ldb (byte 24. 0) nubus-adr))))

(defun set-sharestruct-slot-valid (slot-number val)
  (when (or (< slot-number 0)
            (>= slot-number (sharestruct-max-iopbs)))
    (ferror nil "bad slot number"))
  (let ((nubus-adr (+ (sharestruct-ptr) 12. (* slot-number 4))))
    (%nubus-write (ldb (byte 8 24.) nubus-adr)
                  (ldb (byte 24. 0) nubus-adr)
                  val)))

(defsetf sharestruct-slot-valid set-sharestruct-slot-valid)

(defun sharestruct-slot-siopb-pointer (slot-number)
  (let ((max (sharestruct-max-iopbs)))
    (when (or (< slot-number 0)
              (>= slot-number max))
      (ferror nil "bad slot number"))
    (let ((nubus-adr (+ (sharestruct-ptr) 12. (* max 4) (* slot-number 4))))
      (read-8086-multibus-address nubus-adr))))

(defun write-sharestruct-slot-siopb-pointer (slot-number val)
  (let ((max (sharestruct-max-iopbs)))
    (when (or (< slot-number 0)
              (>= slot-number max))
      (ferror nil "bad slot number"))
    (let ((nubus-adr (+ (sharestruct-ptr) 12. (* max 4) (* slot-number 4))))
      (%nubus-write (ldb (byte 8 24.) nubus-adr)
                    (ldb (byte 24. 0) nubus-adr)
                    val))))



(defun share-iopb-ref (share-iopb-number slot)
  (let ((valid (sharestruct-slot-valid share-iopb-number))
        (siopb (sharestruct-slot-siopb-pointer share-iopb-number)))
    (when (zerop valid)
      (ferror nil "share-iopb-number ~s not valid" share-iopb-number))
    (when (or (< slot 0)
              (>= slot 5))
      (ferror nil "bad slot number ~s in share-iopb" slot))
    (%nubus-read (ldb (byte 8 24.) siopb)
                 (+ (ldb (byte 24. 0) siopb)
                    (* 4 slot)))))

(defun share-iopb-store (share-iopb-number slot val)
  (let ((valid (sharestruct-slot-valid share-iopb-number))
        (siopb (sharestruct-slot-siopb-pointer share-iopb-number)))
    (when (zerop valid)
      (ferror nil "share-iopb-number ~s not valid" share-iopb-number))
    (when (or (< slot 0)
              (>= slot 5))
      (ferror nil "bad slot number ~s in share-iopb" slot))
    (%nubus-write (ldb (byte 8 24.) siopb)
                  (+ (ldb (byte 24. 0) siopb)
                     (* 4 slot))
                  val)))

(defsetf share-iopb-ref share-iopb-store)

(defmacro share-iopb-runme (slot)
  `(share-iopb-ref ,slot 0))

(defmacro share-iopb-quad-slot (slot)
  `(share-iopb-ref ,slot 1))

(defmacro share-iopb-type (slot)
  `(share-iopb-ref ,slot 2))

(defmacro share-iopb-iopb (slot)
  `(share-iopb-ref ,slot 3))

(defmacro share-iopb-interrupt (slot)
  `(share-iopb-ref ,slot 4))




(defun read-8086-multibus-address (nubus-pointer-location)
  (let ((multibus-address
          (8086-ptr-to-multibus-address
            (cond ((zerop (ldb (byte 2 0) nubus-pointer-location))
                   (%nubus-read (ldb (byte 8 24.) nubus-pointer-location)
                                (ldb (byte 24. 0) nubus-pointer-location)))
                  (t
                   (ferror nil "must be word aligned"))))))
    (values (map-multibus-address multibus-address) multibus-address)))


(defun map-multibus-address (nubus-address)
  "return nubus-address, unless it points to the multibus, and is mapped to the nubus.
in that case, follow the mapping, and return that address"
  (cond ((not (= (ldb (byte 8 24.) nubus-address) #xff))
         nubus-address)
        (t
         (let ((map-to (read-multibus-mapping-register (ldb 1212 nubus-address))))
           (cond ((ldb-test (byte 1 23.) map-to)        ; check valid bit
                  (dpb (ldb (byte 22. 0) map-to)
                       (byte 22. 10.)
                       (ldb (byte 10. 0) nubus-address)))
                 (t
                  nubus-address))))))

(defun print-share-iopbs (&optional print-iopbs)
  (let ((sharestruct (sharestruct-ptr)))
    (format t "~&sharestruct = ~16r" sharestruct)
    (let ((maxiopbs (sharestruct-max-iopbs)))
      (format t "~&maxiopbs = ~d" maxiopbs)

      (dotimes (n maxiopbs)
        (let ((valid (sharestruct-slot-valid n))
              (siopb (sharestruct-slot-siopb-pointer n)))
          (format t "~&slot ~d: valid = #x~16r siopb = #x~16r"
                  n valid siopb)
          (cond ((not (zerop valid))
                 (print-share-iopb n print-iopbs)))
          )))))

(defun print-share-iopb (slot &optional print-iopbs)
  print-iopbs
  (format t "~&~8trunme = ~o" (share-iopb-runme slot))
  (format t "~&~8tquad-slot = #x~16r" (share-iopb-quad-slot slot))
  (format t "~&~8ttype = ~o" (share-iopb-type slot))
  (let ((iopb-address (map-multibus-address
                        (8086-ptr-to-multibus-address
                          (share-iopb-iopb slot)))))
    (format t "~&~8tiopb = ~16r" iopb-address)
    )
  (let ((interrupt (map-multibus-address
                     (8086-ptr-to-multibus-address
                       (share-iopb-interrupt slot)))))
    (format t "~&~8tinterrupt = #x~16r" interrupt)))


(defun remove-share-iopb (slot type &optional (ask-p t))
  (let ((maxiopbs (sharestruct-max-iopbs)))
    (dotimes (n maxiopbs)
      (when (not (zerop (sharestruct-slot-valid n)))
        (let ((this-slot (share-iopb-quad-slot n))
              (this-type (share-iopb-type n)))
          (cond ((and (or (= this-slot slot)
                          (= this-slot (logxor #xf0 slot)))
                      (= this-type type))
                 (setf (sharestruct-slot-valid n) 0))
                ((not (memq (ldb (byte 4 4) this-slot)
                            '(0 1 #xe #xf)))
                 (print-share-iopb n)
                 (when (or (null ask-p)
                           (y-or-n-p "Flush this IOPB "))
                   (format t "~&Flushing this IOPB ")
                   (setf (sharestruct-slot-valid n) 0)))))))))

(defun insert-share-iopb ()
  (remove-share-iopb rg-quad-slot 25.)
  (let ((share-iopb (make-wireable-array 1 'art-8b 'interphase-iopb))
        (multibus-mapping-register (allocate-multibus-mapping-registers 1)))
    (array-initialize share-iopb 0)
    (%wire-structure share-iopb)
    (let ((nubus-adr (vadr-to-nubus-phys (%pointer-plus share-iopb
                                                        (array-data-offset share-iopb)))))
      (setf (aref share-iopb (+ 128. (* 1 4))) rg-quad-slot)
      (setf (aref share-iopb (+ 128. (* 2 4))) 25.)
      (let ((mult-adr (multibus-address-to-8086-ptr
                        (dpb multibus-mapping-register (byte 22. 10.) 0))))
        (setf (aref share-iopb (+ 128. (* 3 4) 0)) (ldb (byte 8 0) mult-adr))
        (setf (aref share-iopb (+ 128. (* 3 4) 1)) (ldb (byte 8 8) mult-adr))
        (setf (aref share-iopb (+ 128. (* 3 4) 2)) (ldb (byte 8 16.) mult-adr))
        (setf (aref share-iopb (+ 128. (* 3 4) 3)) (ldb (byte 8 24.) mult-adr)))
      (write-multibus-mapping-register multibus-mapping-register
                                       (dpb 1 (byte 1 23.) (ldb (byte 22. 10.) nubus-adr))))

    (let ((max-iopbs (sharestruct-max-iopbs)))
      (dotimes (i max-iopbs (ferror nil "out of iopbs"))
        (when (zerop (sharestruct-slot-valid i))
          (write-sharestruct-slot-siopb-pointer
            i (multibus-address-to-8086-ptr
                (dpb multibus-mapping-register (byte 22. 10.) 128.)))
          (setf (sharestruct-slot-valid i) 1)
          (return nil))))
    share-iopb))

(defconst multibus-interrupt-7 #xff01c1fc)

(defun share-disk-go (iopb)
  (aset 1 iopb 128.)
  (%nubus-write #xff #x01c1fc 1))




(defun sdu-disk-format-ref (unit slot)
  (labels ((nubus-read (adr)
            (%nubus-read (ldb (byte 8 24.) adr)
                         (ldb (byte 24. 0) adr))))
    (when (or (< unit 0)
              (>= unit 8))
      (ferror nil "invalid unit number"))
    (when (zerop (aref-32 *sys-conf* (+ %system-configuration-disk-unit-0-initialized unit)))
      (ferror nil "unit not initialized by SDU"))
    (let ((nadr (+ (sharestruct-ptr)
                   12.
                   (* 8 (sharestruct-max-iopbs))
                   )))
      (let ((fmt-size (nubus-read (+ nadr 4)))
            (n-fmt (nubus-read (+ nadr 8))))
        (when (>= unit n-fmt)
          (ferror nil "not enough units described in SDU structure"))
        (when (>= slot fmt-size)
          (ferror nil "slot ~s out of range" slot))
        (nubus-read (+ nadr 12. (* unit fmt-size 4) (* 4 slot)))))))

(defmacro sdu-disk-format-type (unit)
  `(sdu-disk-format-ref ,unit 0))

(defmacro sdu-disk-format-heads (unit)
  `(sdu-disk-format-ref ,unit 1))

(defmacro sdu-disk-format-sectors (unit)
  `(sdu-disk-format-ref ,unit 2))

(defmacro sdu-disk-format-cylinders (unit)
  `(sdu-disk-format-ref ,unit 3))

(defun describe-disk-unit (unit)
  (format t "~&Type ~d, Heads ~d, Sectors ~d, Cylinders ~d."
          (sdu-disk-format-type unit)
          (sdu-disk-format-heads unit)
          (sdu-disk-format-sectors unit)
          (sdu-disk-format-cylinders unit)))



(defun convert-block-to-disk-physical (unit block)
  (declare (values sector head cylinder))
  (let ((sectors (sdu-disk-format-sectors unit))
        (heads (sdu-disk-format-heads unit))
        (cylinders (sdu-disk-format-cylinders unit))
        sector head cylinder remainder)
    (multiple-value (cylinder remainder)
      (floor block (* heads sectors)))
    (when (>= cylinder cylinders)
      (ferror nil "block ~s is off the edge of the disk" block))
    (multiple-value (head sector)
      (floor remainder sectors))
    (values sector head cylinder)))



(defvar *share-disk-access*)

(defflavor share-disk-access
         (iopb
          mapping-register-base
          number-of-mapping-registers
          )
         ()
  :settable-instance-variables)

(defparameter *number-of-mapping-registers-for-share* 10.)

(defun initialize-for-share-disk-access ()
  (let ((mapping-registers (allocate-multibus-mapping-registers
                             *number-of-mapping-registers-for-share*)))
    (setq *share-disk-access* (make-instance 'share-disk-access
                                             :iopb (insert-share-iopb)
                                             :mapping-register-base mapping-registers
                                             :number-of-mapping-registers
                                             *number-of-mapping-registers-for-share*))))


(defflavor share-disk-unit
         (unit
          )
         ()
  :settable-instance-variables)


(defmethod (share-disk-unit :read-blocks) (block-number n-blocks
                                           &optional
                                           dma-buffer
                                           )
  (when (null dma-buffer)
    (setq dma-buffer (get-dma-buffer n-blocks)))
  (when (> n-blocks (send *share-disk-access* :number-of-mapping-registers))
    (ferror nil "transfer too big"))
  (multiple-value-bind (sector head cylinder)
      (convert-block-to-disk-physical unit block-number)
    (interphase-disk-command
      (send *share-disk-access* :iopb)
      dma-buffer
      (send *share-disk-access* :mapping-register-base)
      #x81
      unit
      head
      cylinder
      sector
      n-blocks
      0))
  dma-buffer)
