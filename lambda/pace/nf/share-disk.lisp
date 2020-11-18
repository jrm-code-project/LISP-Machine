;;; -*- Mode:LISP; Package:NF; Base:8; Readtable:ZL -*-

;;; routines to support shared disk   -pace 3/21/84

(defun multibus-address-to-8086-ptr (adr)
  (dpb (ldb (byte 16. 4) adr)
       (byte 16. 16.)
       (ldb (byte 4. 0) adr)))

(defun 8086-ptr-to-multibus-address (ptr)
  (+ (ash (ldb (byte 16. 16.) ptr) 4)
     (ldb (byte 16. 0) ptr)
     #xff000000))

; first there is the share-iopb-chain, located at a well known place
; in multibus memory.  The head of the chain is a structure that looks like:
;
;     ptr-to-first-share-iopb - 4 byte 8086 address
;     lock-byte - 1 byte - set to 0 if free, 1 if someone is looking at the chain
;     debug-byte - 1 byte set to the debug level given to the share starter 8086 program

(defconst sharestruct-ptr #xff000080)
(defconst sharestruct-lock #xff000084)
(defconst sharestruct-debug-level #xff000085)

(defconst sharestruct-share-lock 0)
(defconst sharestruct-max-iopbs-offset 4)
(defconst sharestruct-current-iopb-offset 8)
(defconst sharestruct-valid-table-offset 12.)

(defconst share-iopb-runme-offset 0)
(defconst share-iopb-slot-offset 4)
(defconst share-iopb-type-offset 8)
(defconst share-iopb-iopb-offset 12.)
(defconst share-iopb-interrupt-offset 16.)

(defun %nubus-read-unalinged (nubus-adr)
  (let ((quad-slot (ldb (byte 8 24.) nubus-adr))
        (offset (ldb (byte 24. 0) nubus-adr)))
    (let ((b0 (si:%nubus-read-8 quad-slot offset))
          (b1 (si:%nubus-read-8 quad-slot (+ offset 1)))
          (b2 (si:%nubus-read-8 quad-slot (+ offset 2)))
          (b3 (si:%nubus-read-8 quad-slot (+ offset 3))))
      (dpb b3
           (byte 8 24.)
           (dpb b2
                (byte 8 16.)
                (dpb b1
                     (byte 8 8)
                     b0))))))

(defun %read-nubus-adr-8 (nubus-adr)
  (si:%nubus-read-8 (ldb (byte 8 24.) nubus-adr) (ldb (byte 24. 0) nubus-adr)))

(defun %write-nubus-adr-8 (nubus-adr data)
  (si:%nubus-write-8 (ldb (byte 8 24.) nubus-adr) (ldb (byte 24. 0) nubus-adr) data))

(defun %read-nubus-adr (nubus-adr)
  (si:%nubus-read (ldb (byte 8 24.) nubus-adr) (ldb (byte 24. 0) nubus-adr)))

(defun read-8086-multibus-address (nubus-adr)
  (let ((multibus-address
          (8086-ptr-to-multibus-address
            (%nubus-read-unalinged nubus-adr))))
    (values (map-multibus-address multibus-address) multibus-address)))


(defun map-multibus-address (nubus-address)
  "return nubus-address, unless it points to the multibus, and is mapped to the nubus.
in that case, follow the mapping, and return that address"
  (cond ((not (= (ldb (byte 8 24.) nubus-address) #xff))
         nubus-address)
        (t
         (let ((map-to (si:read-multibus-mapping-register (ldb (byte 10. 10.) nubus-address))))
           (cond ((ldb-test (byte 1 23.) map-to)        ; check valid bit
                  (dpb (ldb (byte 22. 0) map-to)
                       (byte 22. 10.)
                       (ldb (byte 10. 0) nubus-address)))
                 (t
                  nubus-address))))))

(defun print-share-iopbs (&optional print-iopbs)
  (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
    (format t "~&sharestruct = ~16r" sharestruct)
    (let ((maxiopbs (%read-nubus-adr-8 (+ sharestruct sharestruct-max-iopbs-offset)))
          (currentiopb (%read-nubus-adr-8 (+ sharestruct sharestruct-current-iopb-offset))))
      (format t "~&maxiopbs = ~d" maxiopbs)
      (format t "~&currentiopb = ~d" currentiopb)

      (dotimes (n maxiopbs)
        (let ((valid (%read-nubus-adr-8 (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
              (siopb (read-8086-multibus-address
                       (+ sharestruct sharestruct-valid-table-offset
                          (* 4 maxiopbs) (* n 4)))))
          (format t "~&slot ~d: (~16r) valid = #x~16r siopb = #x~16r"
                  n
                  (+ sharestruct sharestruct-valid-table-offset (* 4 n))
                  valid siopb)
          (when (not (zerop valid))
            (print-share-iopb siopb print-iopbs)))))))

(defun print-share-iopb (adr &optional print-iopbs)
  (format t "~&~4tshare-iopb at ~o (#x~16r)" adr adr)
  (format t "~&~8trunme = ~o"
          (%read-nubus-adr-8 (+ adr share-iopb-runme-offset)))
  (format t "~&~8tslot = ~o (#x~:*~16r)"
          (%read-nubus-adr-8 (+ adr share-iopb-slot-offset)))
  (format t "~&~8ttype = ~o"
          (%read-nubus-adr-8 (+ adr share-iopb-type-offset)))
  (let ((iopb-address (read-8086-multibus-address (+ adr share-iopb-iopb-offset))))
    (format t "~&~8tiopb = ~o ~:* ~16r" iopb-address)
;    (if print-iopbs
;       (print-iopb-at-nubus-address iopb-address))
    )
  (let ((inter-multi-loc
          (read-8086-multibus-address (+ adr share-iopb-interrupt-offset))))
    (format t "~&~8tinterrupt = ~o (= nubus ~16r)"
            inter-multi-loc (map-multibus-address inter-multi-loc))))

(defun remove-share-iopb (slot type)
  (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
    (let ((maxiopbs (%read-nubus-adr-8 (+ sharestruct sharestruct-max-iopbs-offset))))
      (dotimes (n maxiopbs)
        (let ((valid (%read-nubus-adr-8 (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
              (siopb (read-8086-multibus-address
                       (+ sharestruct sharestruct-valid-table-offset
                          (* maxiopbs 4) (* n 4)))))
          (cond ((not (zerop valid))
                 (let ((this-slot (%read-nubus-adr-8 (+ siopb share-iopb-slot-offset)))
                       (this-type (%read-nubus-adr-8 (+ siopb share-iopb-type-offset))))
                   (cond ((and (or (= this-slot slot)
                                   (= this-slot (logxor #xf0 slot)))
                               (= this-type type))
                          (%write-nubus-adr-8 (+ sharestruct sharestruct-valid-table-offset
                                                 (* n 4))
                                              0)))))))))))

(defstruct (share-iopb (:type :named-array)
                       (:constructor nil)
                       (:print "#<~S Slot #x~x Type #x~x ~S>"
                               (type-of share-iopb)
                               (share-iopb-slot share-iopb)
                               (share-iopb-type share-iopb)
                               (%pointer share-iopb)))
  share-iopb-runme
  share-iopb-slot
  share-iopb-type
  share-iopb-iopb
  share-iopb-interrupt)

(defun get-share-iopb ()
  (let ((share-iopb (allocate-resource 'si:dma-buffer 1)))
    (array-initialize share-iopb 0)
    (setf (array-leader share-iopb 1) 'share-iopb)
    share-iopb))

(defstruct (interphase-iopb (:type :named-array)
                            (:constructor nil)
                            (:print "#<~S ~S>"
                                    (type-of interphase-iopb)
                                    (%pointer interphase-iopb)))
  interphase-iopb-command
  interphase-iopb-options
  interphase-iopb-status
  interphase-iopb-errors

  interphase-iopb-unit
  interphase-iopb-head
  interphase-iopb-cylinder-hi
  interphase-iopb-cylinder-lo

  interphase-iopb-sector-hi
  interphase-iopb-sector-lo
  interphase-iopb-count-hi
  interphase-iopb-count-lo

  interphase-iopb-dma-count
  interphase-iopb-buffer-hi
  interphase-iopb-buffer-med
  interphase-iopb-buffer-lo

  interphase-iopb-base-reg-hi
  interphase-iopb-base-reg-lo
  interphase-iopb-rel-adr-hi
  interphase-iopb-rel-adr-lo

  interphase-iopb-reserved
  interphase-iopb-linked-iopb-hi
  interphase-iopb-linked-iopb-med
  interphase-iopb-linked-iopb-lo
  )

(defun get-interphase-iopb ()
  (let* ((dma-buffer (allocate-resource 'si:dma-buffer 1))
         (iopb (si:dma-buffer-8b dma-buffer)))
    (array-initialize iopb 0)
    (setf (array-leader iopb 1) 'interphase-iopb)
    iopb))


(defun insert-share-iopb (slot type)
  (remove-share-iopb slot type)
  (let* ((share-iopb (get-share-iopb))
         (phys-adr (si:vadr-to-nubus-phys (%pointer-plus share-iopb (si:array-data-offset share-iopb)))))
    (array-initialize share-iopb 0)
    (setf (share-iopb-slot share-iopb) slot)
    (setf (share-iopb-type share-iopb) type)

    ;;set up pointer from share-iopb to real iopb
    ;;like the old code, use 650 in virtual address space for iopb, and point
    ;; to it with our multibus mapping reg
    (write-multibus-mapping-register lam-multibus-mapping-register-base
                                     (+ 1_23. (cadr-page-to-nubus-page 1)))
    (send *proc* :bus-write (+ prime-memory-adr share-iopb-iopb-offset)
                  (multibus-address-to-8086-ptr
                    (+ (ash lam-multibus-mapping-register-base 10.) (* 250 4))))

    ;;no interrupts
    (send *proc* :bus-write (+ prime-memory-adr share-iopb-interrupt-offset) 0)

    (share-lock)
    (unwind-protect
        (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
          (cond ((zerop (ldb (byte 20. 0) sharestruct))
                 (ferror nil "~&sharestruct pointer not set up yet")))
          (let ((maxiopbs (send *proc* :bus-read-byte
                            (+ sharestruct sharestruct-max-iopbs-offset))))

            (dotimes (n maxiopbs (ferror nil "out of iopb slots"))
              (cond ((zerop (send *proc* :bus-read-byte
                              (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                     (send *proc* :bus-write
                       (+ sharestruct sharestruct-valid-table-offset
                          (* 4 maxiopbs) (* 4 n))
                       (multibus-address-to-8086-ptr
                         (+ (ash lam-multibus-mapping-register-base 10.) (* 140 4))))
                     (send *proc* :bus-write-byte
                       (+ sharestruct sharestruct-valid-table-offset (* 4 n)) 1)
                     (return nil))))))
      (share-unlock))))

(defconst multibus-interrupt-7 #xff01c1fc)

(defun share-go ()
  (let ((prime-memory-adr (+ (ash (cadr (car (send *proc* :memory-configuration-list))) 10.)
                             ;(dpb (SEND *PROC* :MEM-SLOT) (byte 4 24.) #xf0000000)
                             (* debug-program-share-iopb-structure 4))))
    (send *proc* :bus-write-byte (+ prime-memory-adr share-iopb-runme-offset) 1)
    (send *proc* :bus-write-byte multibus-interrupt-7 1)))
