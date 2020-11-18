;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;

;;;
;;; COPIED FROM DJ:L.LAMBDA-DIAG;SMD-DISK.LISP
;;;


(defparameter *proc* nil)                       ; processor to debug.

(defparameter *k-disk-cylinder-offset* nil)     ;offset all debug disk reference by this
                                                ;many cylinders!! NIL means read from
                                                ;mini-label.

;use the last 6 mapping registers before the prom
;we only use offset 0 (iopb and share-iopbs) and 1 (data) for now
(defparameter k-multibus-mapping-register-base #o1672)
(defparameter k-number-of-multibus-mapping-registers 6)
(defparameter k-multibus-mapping-register-data-offset 1)

(defparameter disk-base-reg-address #o100)

(defparameter iopb-base-address 0)                      ;byte address - has to be offset by 20000
                                                ;when given to the controller - see
                                                ;multibus-read/32

;; *** note that in SDU-MODE (always), low level multibus routines automatically
;; offset addresses by 20000. ***no more****



;;available disk commands:

(DEFCONSTANT smd-read-command #o201)                    ;81 hex
(DEFCONSTANT smd-write-command #o202)           ;82 hex
(DEFCONSTANT smd-verify-command #o203)          ;83 hex
(DEFCONSTANT smd-format-track-command #o204)            ;84 hex
(DEFCONSTANT smd-map-command #o205)                     ;85 hex
(DEFCONSTANT smd-report-configuration-command #o206)    ;86 hex
(DEFCONSTANT smd-initialize-command #o207)              ;87 hex (used on power up and after reset)
(DEFCONSTANT smd-restore-command #o211)         ;89 hex
(DEFCONSTANT smd-seek-command #o212)                    ;8A hex
(DEFCONSTANT smd-zero-sector-command #o213)             ;8B hex
(DEFCONSTANT smd-reset-command #o217)           ;8F hex
(DEFCONSTANT smd-read-direct-command #o221)             ;91 hex (use with caution)
(DEFCONSTANT smd-write-direct-command #x92)     ;92 hex (use with caution)
(DEFCONSTANT smd-read-absolute #x93)            ;2190 only
(DEFCONSTANT smd-read-non-cached #x94)          ;2190 only

;; possible status codes

(DEFCONSTANT smd-op-ok #o200 "operation successful, ready for next")    ;80 hex
(DEFCONSTANT smd-busy #o201 "operation in progress")            ;81 hex
(DEFCONSTANT smd-error #o202 "error on last command")           ;82 hex

(defparameter default-unit 0)

(defparameter word-mode-option #o21)                    ;sets word mode for data

;these set up for eagle for use during formatting only.
(defparameter sectors-per-track 25.)    ;1024 byte sectors
(defparameter heads-per-cylinder 20.)
(defparameter tracks-per-drive 842.)

;;;--------------------------------------------------------------------------------------------------------------

(defun setup-for-disk ()
  (setq disk-base-reg-address #o100)
  t)

;; function returns cylinder plus offset of lisp mini label
;; this is a map of virtual LISP cylinder to physical cylinder on the disk
(defun k-offset-cylinder (cylinder)
  (if (null *k-disk-cylinder-offset*)
      (k-read-mini-label "LISP"))
  (+ cylinder *k-disk-cylinder-offset*))

;; Note: If you get a fault sometimes (verify-track 0 0) will give the smd restore
;; command which makes the drive happy again.

;; setting *verbose? to T increasing the amount of verbage
(defparameter *verbose? t)

(defun read-random-block (cylinder head sector)
  (set-iopb-pointer iopb-base-address)
  (build-iopb iopb-base-address
              smd-read-command
              default-unit
              head
              cylinder
              sector
              1                 ;one sector
              2                 ;dma-count
              #o2000)           ;buffer area address
  (disk-go-command)
  (wait-for-iopb-completion)
  (let* ((mb-word-base #o2000))
    (dotimes (x 8)
      (format t "~&")
      (if *verbose?
          (dotimes (y 16.)
            (format t "~3o " (nubus-stuff:%multibus-byte-read (+ mb-word-base (+ (* x 16.) y)) t))))
      (dotimes (y 16.)
        (format t "~c" (nubus-stuff:%multibus-byte-read (+ mb-word-base (+ (* x 16.) y)) t))))))


(defun write-random-block (cylinder head sector list-of-words)
  (let* ((mb-adr-base #o2000)
         (mb-word-base (ash mb-adr-base -2)))
    (do ((l list-of-words (cdr l))
         (word-number 0 (1+ word-number)))
        ((null l))
      (nubus-stuff:%multibus-write-32 (+ word-number mb-word-base) (car l)))
    (set-iopb-pointer iopb-base-address)
    (build-iopb iopb-base-address
                smd-write-command
                default-unit
                head
                cylinder
                sector
                1                               ;one sector
                2                               ;dma-count
                mb-adr-base)                    ;buffer area address
    (disk-go-command)
    (wait-for-iopb-completion)))

(defun decode-logical-block (block)
  (let* ((b-per-t sectors-per-track)
         (b-per-c (* b-per-t heads-per-cylinder))
         (cylinder (floor b-per-c))
         (head (floor (- block (* cylinder b-per-c))
                      b-per-t))
         (block-within-track (- block (+ (* cylinder b-per-c)
                                         (* head b-per-t)))))
    (values cylinder head block-within-track)))

(defun read-logical-block (block)
  (multiple-value-bind (cylinder head sector)
      (decode-logical-block block)
    (read-random-block cylinder head sector)))

(defun write-logical-block (block data)
  (multiple-value-bind (cylinder head sector)
      (decode-logical-block block)
    (write-random-block cylinder head sector data)))

(defun map-logical-blocks (start count &optional data)
  (do ((j 0 (1+ j)))
      ((= j count))
    (if data
        (write-logical-block (+ start j) data)
      (read-logical-block (+ start j)))))

(defun number-of-logical-blocks ()
  (* sectors-per-track
     heads-per-cylinder
     TRACKS-PER-DRIVE))

(defun random-logical-blocks (&optional data)
  (let ((n (random (number-of-logical-blocks))))
    (multiple-value-bind (cylinder head sector)
        (decode-logical-block n)
      (format t "~&; Block = ~D. CYL = ~D HEAD = ~D SECTOR = ~D~%"
              n cylinder head sector))
    (if data (write-logical-block n data))
    (read-logical-block n)))

;;;------------------------------------------------------------------------------------------------------------------------

;;;  Iopb stuff


;;status register: bit 0 : BUSY
;;                     1 : OPERATION DONE
;;                     2 : STATUS CHANGE INTERRUPT - for polled interrupt systems
;;                     3 : unused
;;                     4 : UNIT 1 READY
;;                     5 : UNIT 2 READY
;;                     6 : UNIT 3 READY
;;                     7 : UNIT 4 READY

(defun read-disk-status ()
  (cond ((null (P-disk-share-mode *proc*))
         (nubus-stuff:%multibus-io-read-8 disk-base-reg-address))
        (t
         (li:error "can't call this in share mode"))))

;;command register: bit 0 : GO
;;                      1 : CLEAR INTERRUPT
;;                    4-2 : unused
;;                      5 : BUS - word vs. byte mode for fetching I/O parameter block (IOPB)
;;                    7-6 : unused

(defun write-disk-command (value)
 (write-iopb 0 value))

(defun disk-go-command ()                       ;GO!
  (cond ((null (P-disk-share-mode *proc*))
         (nubus-stuff:%multibus-io-write-8 disk-base-reg-address #o43));retain word mode and clear interrupt
        (t
         (share-go))))

(defun set-iopb-pointer (address &optional (offset-p t))
  (cond ;((null (P-disk-share-mode *proc*))
;  (let ((physical-address (if offset-p
;                             (nubus-stuff:multibus-real-address address)
;                             address)))
;    (nubus-stuff:%multibus-io-write-8 (+ disk-base-reg-address 1)
;                                     (hw:ldb physical-address (byte 8. 16.) 0))
;    (nubus-stuff:%multibus-io-write-8 (+ disk-base-reg-address 2)
;                                     (hw:ldb physical-address (byte 8. 8.) 0))
;    (nubus-stuff:%multibus-io-write-8 (+ disk-base-reg-address 3)
;                                     (hw:ldb physical-address (byte 8. 0) 0)))
    ((= address (+ (ash k-multibus-mapping-register-base 10.) (* #o250 4))))
    (t
     (li:error "share mode can only use the prime iopb at 650")))
  )


;;build iopb -


(DEFUN WRITE-IOPB (ADDRESS-WITHIN-IOPB DATA)
  (nubus-stuff:%MULTIBUS-BYTE-WRITE (+ IOPB-BASE-ADDRESS ADDRESS-WITHIN-IOPB) DATA T))

(DEFUN READ-IOPB (ADDRESS-WITHIN-IOPB)
  (nubus-stuff:%MULTIBUS-BYTE-READ (+ IOPB-BASE-ADDRESS ADDRESS-WITHIN-IOPB) T))

(defun read-k-iopb (address-within-iopb)
  (LET ((IOPB-BASE-ADDRESS (* 4 #o640)))
    (nubus-stuff:%read-iopb-from-nubus address-within-iopb)))

(defun build-iopb (iopb-base-address
                   command
                   unit
                   head
                   cylinder
                   starting-sector
                   number-of-sectors
                   dma-count
                   buffer-memory-address
                   &optional (fctn 'write-iopb))

  (FUNCALL FCTN 0 command)
  (FUNCALL FCTN 1 word-mode-option)                             ;command options
  (FUNCALL FCTN 2 0)                                            ;reset status
  (FUNCALL FCTN 3 0)                                            ;reset errors
  (FUNCALL FCTN 4 unit)                                         ;unit select
  (FUNCALL FCTN 5 head)                                         ;head select
  (FUNCALL FCTN 6 (hw:ldb cylinder (byte 8. 8.) 0))                     ;cylinder select [high byte]
  (FUNCALL FCTN 7 (hw:ldb cylinder (byte 8. 0) 0))                      ;cylinder select [low byte]
  (FUNCALL FCTN #o10 (hw:ldb starting-sector (byte 8. 8.) 0))           ;high byte
  (FUNCALL FCTN #o11 (hw:ldb starting-sector (byte 8. 0) 0))            ;low byte
  (FUNCALL FCTN #o12 (hw:ldb number-of-sectors (byte 8. 8.) 0)) ;sector count [high byte]
  (FUNCALL FCTN #o13 (hw:ldb number-of-sectors (byte 8. 0) 0))  ;sector count [low byte]
  (FUNCALL FCTN #o14 dma-count)                 ;dma count
  (let ((buf (if (listp buffer-memory-address)
                 (car buffer-memory-address)
               (nubus-stuff:multibus-real-address buffer-memory-address))))
    (FUNCALL FCTN #o15 (hw:ldb buf (byte 8. 16.) 0))                    ;buffer [data] address
    (FUNCALL FCTN #o16 (hw:ldb buf (byte 8. 8.) 0))
    (FUNCALL FCTN #o17 (hw:ldb buf (byte 8. 0) 0)))
  (FUNCALL FCTN #o20 (hw:ldb disk-base-reg-address (byte 8. 8.) 0))     ;must be the same as that set by switch
  (FUNCALL FCTN #o21 (hw:ldb disk-base-reg-address (byte 8. 0) 0))
  (FUNCALL FCTN #o22 0)                                         ;relative address irrelevent in
  (FUNCALL FCTN #o23 0)                                         ; absolute addressing mode
  (FUNCALL FCTN #o24 0)                                         ;reserved
  (FUNCALL FCTN #o25 0)                                         ;no linked iopbs [XMB]
  (FUNCALL FCTN #o26 0)                                         ;no linked iopbs [MSB]
  (FUNCALL FCTN #o27 0))

(defun build-iopb-in-nubus-memory
       (nubus-base-address
        command
        unit
        head
        cylinder
        starting-sector
        number-of-sectors
        dma-count
        buffer-memory-address)
  (build-iopb (* 4 nubus-base-address)
              command
              unit
              head
              cylinder
              starting-sector
              number-of-sectors
              dma-count
              (list buffer-memory-address)
              'write-iopb-to-nubus))


(defun build-remap-iopb
       (iopb-base-address
        unit
        bad-head
        bad-cylinder
        new-head
        new-cylinder)
  (write-iopb 0 smd-map-command)
  (write-iopb 1 word-mode-option)               ;command options
  (write-iopb 2 0)                              ;reset status
  (write-iopb 3 0)                              ;reset errors
  (write-iopb 4 unit)                           ;unit select
  (write-iopb 5 bad-head)                       ;head select
  (write-iopb 6 (hw:ldb bad-cylinder (byte 8. 8.) 0))   ;cylinder select [high byte]
  (write-iopb 7 (hw:ldb bad-cylinder (byte 8. 0.) 0))   ;cylinder select [low byte]
  (write-iopb #o10 0)
  (write-iopb #o11 new-head)
  (write-iopb #o12 (hw:ldb new-cylinder (byte 8. 8.) 0))
  (write-iopb #o13 (hw:ldb new-cylinder (byte 8. 0.) 0))
  (write-iopb #o14 0)
  (write-iopb #o15 0)
  (write-iopb #o16 0)
  (write-iopb #o17 0)
  (write-iopb #o20 (hw:ldb disk-base-reg-address (byte 8. 8.) 0)) ;must be the same as that set by switch
  (write-iopb #o21 (hw:ldb disk-base-reg-address (byte 8. 0.) 0))
  (write-iopb #o22 0)                           ;relative address irrelevent in
  (write-iopb #o23 0)                           ; absolute addressing mode
  (write-iopb #o24 0)                           ;reserved
  (write-iopb #o25 0)                           ;no linked iopbs [XMB]
  (write-iopb #o26 0)                           ;no linked iopbs [MSB]
  (write-iopb #o27 0))                          ;no linked iopbs [LSB]

(defun read-iopb-status (&optional (fctn 'read-iopb))
  (funcall fctn 2))

(defun read-iopb-error (&optional (fctn 'read-iopb))
  (funcall fctn 3))

(defun read-iopb-cylinder (&optional (fctn 'read-iopb))
  (hw:dpb (funcall fctn 6) #o1010
          (funcall fctn 7)))

(defun read-iopb-number-of-sectors (&optional (fctn 'read-iopb))
  (hw:dpb (funcall fctn #o12) #o1010
          (funcall fctn #o13)))

(defun read-iopb-memory-address (&optional (fctn 'read-iopb))
  (hw:dpb (funcall fctn #o15) #o2010
          (hw:dpb (funcall fctn #o16) #o1010
                  (funcall fctn #o17))))

(defun read-next-iopb-address (&optional (fctn 'read-iopb))
  (hw:dpb (funcall fctn #o25) #o2010
          (hw:dpb (funcall fctn #o26) #o1010            ;
                  (funcall fctn #o27))))

(defun read-iopb-starting-sector (&optional (fctn 'read-iopb))
  (hw:dpb (funcall fctn #o10) #o1010
          (funcall fctn #o11)))

(defun read-iopb-controller-io-address (&optional (fctn 'read-iopb))
  (hw:dpb (funcall fctn #o20) #o1010
          (funcall fctn #o21)))

(defun print-k-iopb ()
  (multiple-value-bind (quad-slot rel-adr)
      (cadr-adr-to-nubus-quad-slot-and-rel-adr #o640)
    (let ((nubus-adr (hw:dpb-unboxed quad-slot (byte 8 24.) (* rel-adr 4))))
      (format t "~%IOPB at phys adr ~s(~16r)" nubus-adr nubus-adr)))
  (print-iopb (* 4 #o640) 'read-iopb-from-nubus))

(DEFUN PRINT-DEBUG-NUBUS-IOPB ()
  (PRINT-IOPB (* 4 #o650) 'READ-IOPB-FROM-NUBUS))

(DEFUN K-IOPB-NUMBER-OF-SECTORS ()
  (LET ((IOPB-BASE-ADDRESS (* 4 #o640)))
    (READ-IOPB-NUMBER-OF-SECTORS 'READ-IOPB-FROM-NUBUS)))

(defun k-iopb-memory-address ()
  (LET ((IOPB-BASE-ADDRESS (* 4 #o640)))
    (READ-IOPB-MEMORY-ADDRESS 'READ-IOPB-FROM-NUBUS)))

(DEFUN READ-IOPB-FROM-NUBUS (ADDRESS-WITHIN-IOPB)
  (LET ((WD (nubus-stuff:%local-memory-read (LSH (+ IOPB-BASE-ADDRESS ADDRESS-WITHIN-IOPB) -2))))
    (hw:LDB WD (BYTE 8 (* (LOGAND ADDRESS-WITHIN-IOPB 3) 8)) 0)))

(DEFUN WRITE-IOPB-TO-NUBUS (ADDRESS-WITHIN-IOPB DATA)
  (LET* ((WD (nubus-stuff:%local-memory-read (LSH (+ IOPB-BASE-ADDRESS ADDRESS-WITHIN-IOPB) -2))))
    (SETQ WD (hw:DPB-unboxed DATA (BYTE  8 (* (LOGAND ADDRESS-WITHIN-IOPB 3) 8)) WD))
    (nubus-stuff:%local-memory-write (LSH (+ IOPB-BASE-ADDRESS ADDRESS-WITHIN-IOPB) -2) WD))
  )

(defun print-iopb-at-nubus-address (nubus-address)
  (print-iopb nil #'(lambda (x) (hw:ldb (nubus-stuff:%bus-byte-read (+ nubus-address x)) (byte 8. 0) 0))))

(defun print-iopb (&optional
                   (iopb-base-address iopb-base-address)
                   (fctn 'read-iopb))
  (print-ip-error (funcall fctn 3))
  (format t "~%              command: ~o" (funcall fctn 0))
  (format t "  ~a" (case (funcall fctn 0)
                     (#x81 "read")
                     (#x82 "write")
                     (#x83 "verify")
                     (#x84 "format track")
                     (#x85 "map")
                     (#x87 "initialize")
                     (#x89 "restore")
                     (#x8a "seek")
                     (#x8f "reset")
                     (#x91 "direct read")
                     (#x92 "direct write")
                     (#x93 "read absolute")))
  (format t "~%      command options: ~o" (funcall fctn 1))
  (format t "~%               status: ~o" (funcall fctn 2))
  (case (funcall fctn 2)
    (0 (format t "  (controller hasn't seen it yet)"))
    (#x80 (format t "  (operation successful)"))
    (#x81 (format t "  (operation in progress)"))
    (#x82 (format t "  (error)"))
    (t (format t "  where did this number come from??")))
  (format t "~%               errors: ~o" (funcall fctn 3))
  (format t "~%                 unit: ~o" (funcall fctn 4))
  (format t "~%                 head: ~o" (funcall fctn 5))
  (format t "~%             cylinder: ~o" (read-iopb-cylinder fctn))
  (format t "~%      starting sector: ~o" (read-iopb-starting-sector fctn))
  (format t "~%    number of sectors: ~o" (read-iopb-number-of-sectors fctn))
  (format t "~%            dma count: ~o" (funcall fctn 14))
  (format t "~%buffer memory address: ~o, page ~o"
          (read-iopb-memory-address fctn)
          (ash (read-iopb-memory-address fctn) -10.))
  (print-multibus-mapping-register (ash (read-iopb-memory-address fctn) -10.))
  (format t "~%controller io address: ~o" (read-iopb-controller-io-address fctn))
  (format t "~%    next iopb address: ~o" (read-next-iopb-address fctn))
  (format t "~%"))


;;;------------------------------------------------------------------------------------------------------------------------
;;;
;;; Iop completion code.

(DEFCONSTANT interphase-timeout (* 3. 60.))

(defun time () 0)

(defun time-difference (t1 t2)
  (- t1 t2)
  )

;if no second arg, IOPB is in multbus memory.  Otherwise, it can be in NUBUS memory.
(DEFUN WAIT-FOR-IOPB-COMPLETION (&OPTIONAL (BREAK-ON-ERROR-P T) (FCTN 'READ-IOPB))
;  (prog2
;    (if share-trace (send standard-output ':tyo #/w))
    (DO ((STATUS (READ-IOPB-STATUS FCTN) (READ-IOPB-STATUS FCTN))
         (START-TIME (TIME)))                   ;initialize time
        (())
      (COND ((OR (= STATUS 0)
                 (= STATUS SMD-BUSY)) nil)              ;command in progress
            ((= STATUS SMD-OP-OK)
             (RETURN STATUS))
            ((= STATUS SMD-ERROR)
             (COND ((NOT (NULL BREAK-ON-ERROR-P))
                    (PRINT-IP-ERROR (READ-IOPB-ERROR FCTN))
                    (li:error "disk error  status ~o, type ~O" STATUS
                            (READ-IOPB-ERROR FCTN))
                    (return status))
                   (T (RETURN STATUS))))
            (T (FORMAT T "~%bad disk status read ~o: current iopb" STATUS)
               (PRINT-IOPB IOPB-BASE-ADDRESS FCTN)))
      (when (>= (TIME-DIFFERENCE (TIME) START-TIME) interphase-timeout)
        (li:error "Disk timeout"))

;       (cerror "Do ~S and try again" "Disk timeout" '(initialize-disk-control))
;       (initialize-disk-control)
;       (disk-go-command)
;       (SETQ START-TIME (TIME)))
      )

;    (if share-trace (send *standard-output* :tyo #/W))
;    )

  )


(defun wait-for-iopb-completion-nubus (nubus-address &optional (break-on-error-p t)
                                                               (fctn 'read-iopb-from-nubus))
  (let ((iopb-base-address (* 4 nubus-address)))
    (wait-for-iopb-completion break-on-error-p fctn)))

;claims to work on 2181 revision levels 2.0 or higher, and 2190.  doesnt seem to tho.
(defun report-configuration ()
  (set-iopb-pointer iopb-base-address)
  (build-iopb iopb-base-address
              smd-report-configuration-command
              0         ;unit irrelevent, but initialize IOPB
              0         ;head irrelevent, but initialize
              0         ;cylinder irrelevent, but initialize
              0         ;starting sector irrelevent
              0         ;number of sectors irrelevent
              0         ;dma-count irrelevent
              0         ;buffer address, irrelevent
      )
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION)
  (values (read-iopb 2) (read-iopb 3) (read-iopb 4) (read-iopb 5)))

;;;--------------------------------------------------------------------------------------------------------------------------

;(defun k-read-mini-label (name)
;  name
;  (cond ((null (P-disk-share-mode *proc*))  ;*use-configuration-structure*
;        (nubus-stuff:write-multibus-mapping-register k-multibus-mapping-register-base
;                                  (+ #x800000
;                                     #x440000
;                                     (LSH (P-mem-slot *PROC*) 14.)
;                                     1
;                                     ))
;        (set-iopb-pointer (+ (ash k-multibus-mapping-register-base 10.)
;                             (* #o250 4)) nil)  ;byte adr within page.
;        (build-iopb-in-nubus-memory #o650 ;iopb-base-address
;             smd-read-command
;             default-unit
;             (if (< sectors-per-track 23.) 1 0)  ;head 0
;             0                 ;cylinder 0
;             (if (< sectors-per-track 23.) (- 22. sectors-per-track) 22.) ;starting sector
;             1                 ;one sector
;             2                 ;dma-count
;             #o102000)         ;buffer area address
;        (disk-go-command)
;        (wait-for-iopb-completion-nubus #o650)
;        (cond ((or (not (= (nubus-stuff:%multibus-byte-read #o102000) #/F))
;                   (not (= (nubus-stuff:%multibus-byte-read #o102001) #/O))
;                   (not (= (nubus-stuff:%multibus-byte-read #o102002) #/O))
;                   (not (= (nubus-stuff:%multibus-byte-read #o102003) #/B)))
;               (li:error "mini-label check word doesn't check"))
;              ((or (not (= (nubus-stuff:%multibus-byte-read #o102004) #/L))
;                   (not (= (nubus-stuff:%multibus-byte-read #o102005) #/I))
;                   (not (= (nubus-stuff:%multibus-byte-read #o102006) #/S))
;                   (not (= (nubus-stuff:%multibus-byte-read #o102007) #/P)))
;               (li:error "can't find LISP mini label partition")))
;        (setq *k-disk-cylinder-offset*
;              (nubus-stuff:%bus-slot-read sdu-quad-slot (ash #o102010 -2))))
;;      ((typep *proc* 'lambda-via-local-access)
;;       ;we're really the same machine, so we can read it directly!
;;       (let* ((rqb (si:get-disk-rqb))
;;              (rqb-string (si:rqb-8-bit-buffer rqb)))
;;         (si:disk-read-physical rqb default-unit 22.)
;;         (cond ((or (not (= (aref rqb-string 0) #/F))
;;                    (not (= (aref rqb-string 1) #/O))
;;                    (not (= (aref rqb-string 2) #/O))
;;                    (not (= (aref rqb-string 3) #/B)))
;;                (li:error "mini-label check word doesn't check"))
;;               ((or (not (= (aref rqb-string 4) #/L))
;;                    (not (= (aref rqb-string 5) #/I))
;;                    (not (= (aref rqb-string 6) #/S))
;;                    (not (= (aref rqb-string 7) #/P)))
;;                (li:error "can't find LISP mini label partition")))
;;         (setq *k-disk-cylinder-offset*
;;               (+ (ash (aref rqb-string #o11) 8) (aref rqb-string #o10)))
;;         (si:return-disk-rqb rqb)))
;;      (t (let ((his-version (or *his-version*
;;                                his-system-version-override
;;                                (QF-POINTER (nubus-stuff:%local-bus-read (+ #o400 %SYS-COM-MAJOR-VERSION))))))
;;           (cond ((>= his-version 104.)
;;                  ;use wired-disk-buffer
;;                  (let ((wired-disk-buffer (qf-initial-area-origin 'wired-disk-buffer)))
;;                    (multiple-value-bind (sys-com-quad-slot sys-com-rel-adr)
;;                        (cadr-adr-to-nubus-quad-slot-and-rel-adr #o650)
;;                      (nubus-stuff:%write-multibus-mapping-register k-multibus-mapping-register-base
;;                                                       (+ #x800000
;;                                                          (LSH sys-com-quad-slot 14.)
;;                                                          (ash sys-com-rel-adr -8)))
;;                      (set-iopb-pointer (+ (ash k-multibus-mapping-register-base 10.)
;;                                           (* (logand sys-com-rel-adr #o377) 4)) nil)  ;byte adr within page.
;;                      (multiple-value-bind (disk-buffer-quad-slot disk-buffer-rel-adr)
;;                          (cadr-adr-to-nubus-quad-slot-and-rel-adr wired-disk-buffer)
;;                        (nubus-stuff:%write-multibus-mapping-register (1+ k-multibus-mapping-register-base)
;;                                                         (+ #x800000
;;                                                            (lsh disk-buffer-quad-slot 14.)
;;                                                            (ash disk-buffer-rel-adr -8)))
;;                        (build-iopb-in-nubus-memory #o650 ;iopb-base-address
;;                         smd-read-command
;;                         default-unit
;;                         (if (< sectors-per-track 23.) 1 0)  ;head 0
;;                         0                                   ;cylinder 0
;;                         (if (< sectors-per-track 23.) (- 22. sectors-per-track) 22.) ;starting sector
;;                         1                    ;one sector
;;                         2                    ;dma-count
;;                         (ASH (1+ k-multibus-mapping-register-base)
;;                              10.))           ;buffer area address
;;                        (disk-go-command)
;;                        (wait-for-iopb-completion-nubus #o650)
;;                        (cond ((or (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 0)) #/F))
;;                                   (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 1)) #/O))
;;                                   (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 2)) #/O))
;;                                   (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 3)) #/B)))
;;                               (li:error nil "mini-label check word doesn't check"))
;;                              ((or (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 4)) #/L))
;;                                   (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 5)) #/I))
;;                                   (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 6)) #/S))
;;                                   (not (= (nubus-stuff:%bus-slot-byte-read
;;                                                    disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 7)) #/P)))
;;                               (li:error nil "can't find LISP mini label partition")))
;;                        (setq *k-disk-cylinder-offset*
;;                              (nubus-stuff:%bus-slot-read disk-buffer-quad-slot
;;                                                          (+ (ash disk-buffer-rel-adr 2) #o10)))
;;                        ))))
;;                 (t
;;                  (setq *k-disk-cylinder-offset* 100.)
;;                  (format t "~%asumming mini label since to memory available to read it into!!!")
;;                  ))))
;       )       ;foo no memory to read it into
;;  (format t "~&Setting LISP disk cylinder offset to ~D." *k-disk-cylinder-offset*)
;  )
