;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;

;;;
;;; COPIED FROM DJ:L.LAMBDA-DIAG;SMD-DISK.LISP
;;;
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

(defconstant sharestruct-ptr #xff000080)
(defconstant sharestruct-lock #xff000084)
(defconstant sharestruct-debug-level #xff000085)

(defconstant sharestruct-share-lock 0)
(defconstant sharestruct-max-iopbs-offset 4)
(defconstant sharestruct-current-iopb-offset 8.)
(defconstant sharestruct-valid-table-offset 12.)

(defconstant share-iopb-runme-offset 0)
(defconstant share-iopb-slot-offset 4)
(defconstant share-iopb-type-offset 8.)
(defconstant share-iopb-iopb-offset 12.)
(defconstant share-iopb-interrupt-offset 16.)

(defun sharestruct-valid-p ()
  (not (zerop (ldb (byte 20. 0) (read-8086-multibus-address sharestruct-ptr)))))

; these are word addresses in mem-slot
(defconstant lambda-share-iopb-structure #o520) ; see qcom
(defconstant debug-program-share-iopb-structure #o540)

(defvar share-lock-prevent-accidental-recursion nil)

;(defconstant enable-locking t)

(defun share-lock ()
  (cond ((null share-lock-prevent-accidental-recursion)
         (setq share-lock-prevent-accidental-recursion t)
         ;; we don't have with-timeout yet. When we do
         ;; wrap the do loop with it
         (do ()
             ((zerop (nubus-stuff:%bus-byte-read sharestruct-lock))))
         (nubus-stuff:%bus-byte-write sharestruct-lock 1))
        (t
         (li:error nil "share-lock called while k apparently already had lock")))
  )

(defun share-unlock ()
  (setq share-lock-prevent-accidental-recursion nil)
  (nubus-stuff:%bus-byte-write sharestruct-lock 0)
  )

(defun read-8086-multibus-address (nubus-pointer-location)
  (let ((multibus-address
          (8086-ptr-to-multibus-address
            (cond ((zerop (ldb (byte 2 0) nubus-pointer-location))
                   (nubus-stuff:%bus-quad-slot-read-unsafe
                         (ldb (byte 8. 24.) nubus-pointer-location)
                         (ldb (byte 24. 0) nubus-pointer-location)))
                  (t
                   (logior (nubus-stuff:%bus-read-byte-unsafe nubus-pointer-location)
                           (ash (nubus-stuff:%bus-read-byte-unsafe
                                      (+ nubus-pointer-location 1)) 8.)
                           (ash (nubus-stuff:%bus-read-byte-unsafe
                                      (+ nubus-pointer-location 2)) 16.)
                           (ash (nubus-stuff:%bus-read-byte-unsafe
                                      (+ nubus-pointer-location 3)) 24.)))))))
    (values (nubus-stuff:map-multibus-address multibus-address) multibus-address))
  )

(defun print-share-iopbs (&optional print-iopbs)
  (format t "~&sharestruct-debug-level = ~d."
          (nubus-stuff:%bus-byte-read sharestruct-debug-level))
  (format t "~&sharestruct-lock = ~o"
          (nubus-stuff:%bus-byte-read sharestruct-lock))
  (share-lock)
  ;; we do not have unwind-protect now. put it back as soon it exists
;  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (format t "~&sharestruct = ~16r" sharestruct)
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (li:error "~&sharestruct pointer not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-byte-read
                          (+ sharestruct sharestruct-max-iopbs-offset )))
              (currentiopb (nubus-stuff:%bus-byte-read
                             (+ sharestruct sharestruct-current-iopb-offset))))
          (format t "~&maxiopbs = ~d" maxiopbs)
          (format t "~&currentiopb = ~d" currentiopb)

          (dotimes (n maxiopbs)
            (let ((valid (nubus-stuff:%bus-read-byte-unsafe
                           (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                  (siopb (read-8086-multibus-address
                           (+ sharestruct sharestruct-valid-table-offset
                              (* 4 maxiopbs) (* n 4)))))
              (format t "~&slot ~d: (~16r) valid = #x~16r siopb = #x~16r"
                      n
                      (+ sharestruct sharestruct-valid-table-offset (* 4 n))
                      valid siopb)
              (cond ((not (zerop valid))
                     (print-share-iopb siopb print-iopbs)))))))
    (share-unlock)
;    )
    )

(defun print-share-iopb (adr &optional print-iopbs)
  (format t "~&~4tshare-iopb at ~o (#x~16r)" adr adr)
  (format t "~&~8trunme = ~o"
          (nubus-stuff:%bus-read-byte-unsafe (+ adr share-iopb-runme-offset)))
  (format t "~&~8tslot = ~o (#x~:*~16r)"
          (nubus-stuff:%bus-read-byte-unsafe (+ adr share-iopb-slot-offset)))
  (format t "~&~8ttype = ~o"
          (nubus-stuff:%bus-read-byte-unsafe (+ adr share-iopb-type-offset)))
  (let ((iopb-address (read-8086-multibus-address (+ adr share-iopb-iopb-offset))))
    (format t "~&~8tiopb = ~o ~:* ~16r" iopb-address)
    (if print-iopbs
        (print-iopb-at-nubus-address iopb-address)))
  (let ((inter-multi-loc
          (read-8086-multibus-address (+ adr share-iopb-interrupt-offset))))
    (format t "~&~8tinterrupt = ~o (= nubus ~16r)"
            inter-multi-loc (nubus-stuff:map-multibus-address inter-multi-loc))))

(defconstant cadr-share-slot #o375)
;         (cond ((= si:processor-type-code si:cadr-type-code) #o377)
;               ((= si:processor-type-code si:lambda-type-code)
;                (cond ((not (boundp 'si:*my-op*)) #o376)
;                      (t (- #o375 (si:op-proc-number si:*my-op*)))))))

(defconstant cadr-share-type #o377)

(defun remove-share-iopb (&optional (slot cadr-share-slot) (type cadr-share-type) (ask-p t))
  (share-lock)
;  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (li:error "~&sharestruct pointer not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-byte-read
                          (+ sharestruct sharestruct-max-iopbs-offset))))

          (dotimes (n maxiopbs)
            (let ((valid (nubus-stuff:%bus-byte-read
                           (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                  (siopb (read-8086-multibus-address
                           (+ sharestruct sharestruct-valid-table-offset
                              (* maxiopbs 4) (* n 4)))))
              (cond ((not (zerop valid))
                     (let ((this-slot (nubus-stuff:%bus-read-byte-unsafe
                                            (+ siopb share-iopb-slot-offset)))
                           (this-type (nubus-stuff:%bus-read-byte-unsafe
                                            (+ siopb share-iopb-type-offset))))
                       (cond ((and (or (= this-slot slot)
                                       (= this-slot (logxor #xf0 slot)))
                                   (= this-type type))
                              (nubus-stuff:%bus-byte-write
                                    (+ sharestruct sharestruct-valid-table-offset
                                       (* n 4)) 0)))
                       (when (not (memq (ldb (byte 4 4) this-slot)
                                        '(0 1 #xe #xf)))
                         (print-share-iopb siopb)
;                        (if (if ask-p (y-or-n-p "Flush this IOPB ")
;                              (format t "Flushing this IOPB ")
;                              t)
                             (nubus-stuff:%bus-byte-write
                                   (+ sharestruct sharestruct-valid-table-offset
                                      (* n 4)) 0)))))))))
;      )
    (share-unlock)
;    )
    )

(defun invalidate-slot (slot-number)
  (share-lock)
  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (li:error "sharestruct not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-byte-read
                              (+ sharestruct sharestruct-max-iopbs-offset))))
          (cond ((>= slot-number maxiopbs)
                 (li:error "there are only ~d slots" maxiopbs)))
          (nubus-stuff:%bus-write (+ sharestruct sharestruct-valid-table-offset
                                     (* slot-number 4)) 0)))
    (share-unlock)))

(defun insert-share-iopb ()
  (remove-share-iopb cadr-share-slot cadr-share-type nil)
  (let ((prime-memory-adr (+ (ash (cadr (car (k-memory-configuration-list))) 14.)
                             ;;(dpb (SEND *PROC* :MEM-SLOT) (byte 4 24.) #xf0000000)
                             (* debug-program-share-iopb-structure 4))))
    (format t "~%prime-memory-adr for iopb=#x~16r" prime-memory-adr)
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-runme-offset) 0)
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-slot-offset) cadr-share-slot)
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-type-offset) cadr-share-type)

    ;;set up pointer from share-iopb to real iopb
    ;;like the old code, use 650 in virtual address space for iopb, and point
    ;; to it with our multibus mapping reg
    (nubus-stuff:write-multibus-mapping-register multibus-mapping-register-base
                                     (+ #x800000 (cadr-page-to-nubus-page 1)))
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-iopb-offset)
                  (multibus-address-to-8086-ptr
                    (+ (ash multibus-mapping-register-base 10.) (* #o250 4))))

    ;;no interrupts
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-interrupt-offset) 0)

    (share-lock)
    (unwind-protect
        (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
          (cond ((zerop (ldb (byte 20. 0) sharestruct))
                 (li:error "~&sharestruct pointer not set up yet")))
          (let ((maxiopbs (nubus-stuff:%bus-byte-read
                            (+ sharestruct sharestruct-max-iopbs-offset))))

            (dotimes (n maxiopbs (li:error "out of iopb slots"))
              (cond ((zerop (nubus-stuff:%bus-byte-read
                              (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                     (nubus-stuff:%bus-write
                       (+ sharestruct sharestruct-valid-table-offset
                          (* 4 maxiopbs) (* 4 n))
                       (multibus-address-to-8086-ptr
                         (+ (ash multibus-mapping-register-base 10.) (* #o140 4))))
                     (nubus-stuff:%bus-byte-write
                       (+ sharestruct sharestruct-valid-table-offset (* 4 n)) 1)
                     (return nil))))))
      (share-unlock))))

(defconstant multibus-interrupt-1 #xff01c1e4)
(defconstant multibus-interrupt-7 #xff01c1fc)

(defconstant share-trace nil)

(defun share-go ()
  (let ((prime-memory-adr (+ (dpb (k-memory-slot) (byte 8. 24.) 0)
                             (*  *physical-iopb-address*
                                 ;; debug-program-share-iopb-structure
                                 4))))
    (nubus-stuff:%bus-byte-write (+ prime-memory-adr share-iopb-runme-offset) 1)
    (nubus-stuff:%bus-byte-write multibus-interrupt-7 1)))

(defun share-go-slot (slot-num)
  (share-lock)
  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (format t "~&sharestruct = ~16r" sharestruct)
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (li:error "~&sharestruct pointer not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-byte-read
                          (+ sharestruct sharestruct-max-iopbs-offset )))
              (currentiopb (nubus-stuff:%bus-byte-read
                             (+ sharestruct sharestruct-current-iopb-offset))))
          (format t "~&maxiopbs = ~d" maxiopbs)
          (format t "~&currentiopb = ~d" currentiopb)

          (dotimes (n maxiopbs)
            (let ((valid (nubus-stuff:%bus-read-byte-unsafe
                           (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                  (siopb (read-8086-multibus-address
                           (+ sharestruct sharestruct-valid-table-offset
                              (* 4 maxiopbs) (* n 4)))))
              (format t "~&slot ~d: (~16r) valid = #x~16r siopb = #x~16r"
                      n
                      (+ sharestruct sharestruct-valid-table-offset (* 4 n))
                      valid siopb)
              (cond ((not (zerop valid))
                     (print-share-iopb siopb t)))
              (cond ((and (= n slot-num)
;                         (yes-or-no-p "Goose this one?")
                          t)
                     (nubus-stuff:%bus-write-byte-unsafe (+ siopb share-iopb-runme-offset) 1)))))))
    (share-unlock)))
