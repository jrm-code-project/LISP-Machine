;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;

;;; This defstruct is to replace the *proc* introduced for lambda debugging.
;;;

(defstruct (processor
             (:conc-name P-)
             )
  (prom-string nil)
  (proc-type nil)
  (tv-controller-type nil)
  (tv-slot nil)
  (tv-device-subindex nil)
  (disk-controller-type nil)
  (disk-share-mode nil)
  (disk-type nil)
  (proc-variables-to-switch nil)
  (saved-opcs nil)
  (saved-micro-stack nil)
  (memory-configuration-list nil)
  (page-band-unit nil)
  (proc-conf-pointer nil)
  (mem-slot nil)
  (proc-conf-bus-address nil)
  )

(DEFUN CADR-ADR-TO-NUBUS-QUAD-SLOT-AND-REL-ADR (ADR)
  (LET ((NUBUS-PAGE (CADR-PAGE-TO-NUBUS-PAGE (hw:LDB ADR #o1020 0))))
  ;altogether, nubus address is 8 quad-slot, 14 page number, 8 within page, 2 byte.
    (VALUES (hw:LDB NUBUS-PAGE #.(byte 8 (- 24. 10.)) 0)        ;quad-slot
            (+ (LSH (hw:LDB NUBUS-PAGE #.(byte 14. 0.) 0) 8)
               (LOGAND ADR #o377)))))

(DEFUN CADR-PAGE-TO-NUBUS-PAGE (CADR-PAGE)
  (DO ((P (P-MEMORY-CONFIGURATION-LIST *PROC*) (CDR P))
       (PAGE CADR-PAGE))
      ((NULL P)
       (ERROR "UNABLE TO MAP PAGE"))
    (COND ((< PAGE (FIRST (CAR P)))
           (RETURN (+ (SECOND (CAR P)) PAGE))))
    (SETQ PAGE (- PAGE (FIRST (CAR P))))))

;;;
;;;
;;;

;;; COPIED FROM DJ:L.LAMBDA-DIAG;SMD-DISK.LISP
;;;



(defparameter *lam-disk-cylinder-offset* nil)   ;offset all debug disk reference by this
                                                ;many cylinders!! NIL means read from
                                                ;mini-label.

;use the last 8 mapping registers before the prom
;we only use offset 0 (iopb and share-iopbs) and 1 (data) for now
(defparameter lam-multibus-mapping-register-base #o1670)
(defparameter lam-number-of-multibus-mapping-registers 8)
(defparameter lam-multibus-mapping-register-data-offset 1)

;;this file contains functions for diagnostic operation of a winchester disk
;;with an INTERPHASE 2181 SMD controller on the multibus.  The first disk
;;used is a prototype 474 megabyte model MS2351A/F Eagle from Fujitsu.

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

(DEFCONSTANT word-mode-option #o21)                     ;sets word mode for data

;these set up for eagle for use during formatting only.
(DEFCONSTANT sectors-per-track 25.)     ;1024 byte sectors
(DEFCONSTANT heads-per-cylinder 20.)
(DEFCONSTANT TRACKS-PER-DRIVE 842.)

(DEFCONSTANT lambda-disk-type 'eagle) ;obsolete, send :disk-type message to *proc*.

;;;--------------------------------------------------------------------------------------------------------------

(defun setup-for-disk ()
  (setq disk-base-reg-address #o100)
  t)

;; function returns cylinder plus offset of lisp mini label
;; this is a map of virtual LISP cylinder to physical cylinder on the disk
(defun lam-offset-cylinder (cylinder)
  (if (null *lam-disk-cylinder-offset*)
      (lam-read-mini-label "LISP"))
  (+ cylinder *lam-disk-cylinder-offset*))

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
         (error "can't call this in share mode"))))

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
  (cond ((null (P-disk-share-mode *proc*))
         (let ((physical-address (if offset-p
                                     (multibus-real-address address)
                                   address)))
           (nubus-stuff:%multibus-io-write-8 (+ disk-base-reg-address 1)
                                 (hw:ldb physical-address #o2010 0))
           (nubus-stuff:%multibus-io-write-8 (+ disk-base-reg-address 2)
                                 (hw:ldb physical-address #o1010 0))
           (nubus-stuff:%multibus-io-write-8 (+ disk-base-reg-address 3)
                                 (hw:ldb physical-address #o0010 0))))
        ((= address (+ (ash lam-multibus-mapping-register-base 10.) (* #o250 4))))
        (t
         (ferror nil "share mode can only use the prime iopb at 650"))))

;;build iopb -


(DEFUN WRITE-IOPB (ADDRESS-WITHIN-IOPB DATA)
  (nubus-stuff:%MULTIBUS-BYTE-WRITE (+ IOPB-BASE-ADDRESS ADDRESS-WITHIN-IOPB) DATA T))

(DEFUN READ-IOPB (ADDRESS-WITHIN-IOPB)
  (nubus-stuff:%MULTIBUS-BYTE-READ (+ IOPB-BASE-ADDRESS ADDRESS-WITHIN-IOPB) T))

(defun read-lambda-iopb (address-within-iopb)
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
  (FUNCALL FCTN 6 (hw:ldb cylinder #o1010 0))                   ;cylinder select [high byte]
  (FUNCALL FCTN 7 (hw:ldb cylinder #o0010 0))                   ;cylinder select [low byte]
  (FUNCALL FCTN #o10 (hw:ldb starting-sector #o1010 0))         ;high byte
  (FUNCALL FCTN #o11 (hw:ldb starting-sector #o0010 0))         ;low byte
  (FUNCALL FCTN #o12 (hw:ldb number-of-sectors #o1010 0))       ;sector count [high byte]
  (FUNCALL FCTN #o13 (hw:ldb number-of-sectors #o0010 0))       ;sector count [low byte]
  (FUNCALL FCTN #o14 dma-count)                 ;dma count
  (let ((buf (if (listp buffer-memory-address)
                 (car buffer-memory-address)
               (nubus-stuff:%multibus-real-address buffer-memory-address))))
    (FUNCALL FCTN #o15 (hw:ldb buf #o2010 0))                   ;buffer [data] address
    (FUNCALL FCTN #o16 (hw:ldb buf #o1010 0))
    (FUNCALL FCTN #o17 (hw:ldb buf #o0010 0)))
  (FUNCALL FCTN #o20 (hw:ldb disk-base-reg-address #o1010 0))   ;must be the same as that set by switch
  (FUNCALL FCTN #o21 (hw:ldb disk-base-reg-address #o0010 0))
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
  (write-iopb 6 (hw:ldb bad-cylinder #o1010 0)) ;cylinder select [high byte]
  (write-iopb 7 (hw:ldb bad-cylinder #o0010 0)) ;cylinder select [low byte]
  (write-iopb #o10 0)
  (write-iopb #o11 new-head)
  (write-iopb #o12 (hw:ldb new-cylinder #o1010 0))
  (write-iopb #o13 (hw:ldb new-cylinder #o0010 0))
  (write-iopb #o14 0)
  (write-iopb #o15 0)
  (write-iopb #o16 0)
  (write-iopb #o17 0)
  (write-iopb #o20 (hw:ldb disk-base-reg-address #o1010 0)) ;must be the same as that set by switch
  (write-iopb #o21 (hw:ldb disk-base-reg-address #o0010 0))
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

(defun print-lambda-iopb ()
  (multiple-value-bind (quad-slot rel-adr)
      (cadr-adr-to-nubus-quad-slot-and-rel-adr #o640)
    (let ((nubus-adr (hw:dpb-unboxed quad-slot (byte 8 24.) (* rel-adr 4))))
      (format t "~%IOPB at phys adr ~s(~16r)" nubus-adr nubus-adr)))
  (print-iopb (* 4 #o640) 'read-iopb-from-nubus))

(DEFUN PRINT-DEBUG-NUBUS-IOPB ()
  (PRINT-IOPB (* 4 #o650) 'READ-IOPB-FROM-NUBUS))

(DEFUN LAMBDA-IOPB-NUMBER-OF-SECTORS ()
  (LET ((IOPB-BASE-ADDRESS (* 4 #o640)))
    (READ-IOPB-NUMBER-OF-SECTORS 'READ-IOPB-FROM-NUBUS)))

(defun lambda-iopb-memory-address ()
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
  (print-iopb nil #'(lambda (x) (hw:ldb (nubus-stuff:%bus-byte-read (+ nubus-address x)) #o0010 0))))

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

;;; Label stuff.



;mini-label is stored in block 22, which, with 512 byte sectors, is sector 0 of head 1,
; with 1024 byte sectors, it is sector 22. of head 0.
; cylinder 0.  Format is:
;  word 0: check   ascii FOOB
;  word 1: name    ascii LISP
;  word 2: cylinder offset
;  word 3: length in cylinders
;  word 4: -1
;(in the future, words 1-3 can be repeated).
; this uses mbus location #o2000 as a buffer
; (which is then offset by multibus offset hack)

(defun lam-read-mini-label (name)
  name
  (cond ((null (P-disk-share-mode *proc*))  ;*use-configuration-structure*
         (nubus-stuff:write-multibus-mapping-register lam-multibus-mapping-register-base
                                   (+ #x800000
                                      #x440000
                                      (LSH (P-mem-slot *PROC*) 14.)
                                      1
                                      ))
         (set-iopb-pointer (+ (ash lam-multibus-mapping-register-base 10.)
                              (* #o250 4)) nil)  ;byte adr within page.
         (build-iopb-in-nubus-memory #o650 ;iopb-base-address
              smd-read-command
              default-unit
              (if (< sectors-per-track 23.) 1 0)  ;head 0
              0                 ;cylinder 0
              (if (< sectors-per-track 23.) (- 22. sectors-per-track) 22.) ;starting sector
              1                 ;one sector
              2                 ;dma-count
              #o102000)         ;buffer area address
         (disk-go-command)
         (wait-for-iopb-completion-nubus #o650)
         (cond ((or (not (= (nubus-stuff:%multibus-byte-read #o102000) #/F))
                    (not (= (nubus-stuff:%multibus-byte-read #o102001) #/O))
                    (not (= (nubus-stuff:%multibus-byte-read #o102002) #/O))
                    (not (= (nubus-stuff:%multibus-byte-read #o102003) #/B)))
                (error "mini-label check word doesn't check"))
               ((or (not (= (nubus-stuff:%multibus-byte-read #o102004) #/L))
                    (not (= (nubus-stuff:%multibus-byte-read #o102005) #/I))
                    (not (= (nubus-stuff:%multibus-byte-read #o102006) #/S))
                    (not (= (nubus-stuff:%multibus-byte-read #o102007) #/P)))
                (error "can't find LISP mini label partition")))
         (setq *lam-disk-cylinder-offset*
               (nubus-stuff:%bus-slot-read sdu-quad-slot (ash #o102010 -2))))
;       ((typep *proc* 'lambda-via-local-access)
;        ;we're really the same machine, so we can read it directly!
;        (let* ((rqb (si:get-disk-rqb))
;               (rqb-string (si:rqb-8-bit-buffer rqb)))
;          (si:disk-read-physical rqb default-unit 22.)
;          (cond ((or (not (= (aref rqb-string 0) #/F))
;                     (not (= (aref rqb-string 1) #/O))
;                     (not (= (aref rqb-string 2) #/O))
;                     (not (= (aref rqb-string 3) #/B)))
;                 (error "mini-label check word doesn't check"))
;                ((or (not (= (aref rqb-string 4) #/L))
;                     (not (= (aref rqb-string 5) #/I))
;                     (not (= (aref rqb-string 6) #/S))
;                     (not (= (aref rqb-string 7) #/P)))
;                 (error "can't find LISP mini label partition")))
;          (setq *lam-disk-cylinder-offset*
;                (+ (ash (aref rqb-string #o11) 8) (aref rqb-string #o10)))
;          (si:return-disk-rqb rqb)))
        (t (let ((his-version (or *his-version*
                                  his-system-version-override
                                  (QF-POINTER (nubus-stuff:%local-bus-read (+ #o400 %SYS-COM-MAJOR-VERSION))))))
             (cond ((>= his-version 104.)
                    ;use wired-disk-buffer
                    (let ((wired-disk-buffer (qf-initial-area-origin 'wired-disk-buffer)))
                      (multiple-value-bind (sys-com-quad-slot sys-com-rel-adr)
                          (cadr-adr-to-nubus-quad-slot-and-rel-adr #o650)
                        (nubus-stuff:%write-multibus-mapping-register lam-multibus-mapping-register-base
                                                         (+ #x800000
                                                            (LSH sys-com-quad-slot 14.)
                                                            (ash sys-com-rel-adr -8)))
                        (set-iopb-pointer (+ (ash lam-multibus-mapping-register-base 10.)
                                             (* (logand sys-com-rel-adr #o377) 4)) nil)  ;byte adr within page.
                        (multiple-value-bind (disk-buffer-quad-slot disk-buffer-rel-adr)
                            (cadr-adr-to-nubus-quad-slot-and-rel-adr wired-disk-buffer)
                          (nubus-stuff:%write-multibus-mapping-register (1+ lam-multibus-mapping-register-base)
                                                           (+ #x800000
                                                              (lsh disk-buffer-quad-slot 14.)
                                                              (ash disk-buffer-rel-adr -8)))
                          (build-iopb-in-nubus-memory #o650 ;iopb-base-address
                           smd-read-command
                           default-unit
                           (if (< sectors-per-track 23.) 1 0)  ;head 0
                           0                                   ;cylinder 0
                           (if (< sectors-per-track 23.) (- 22. sectors-per-track) 22.) ;starting sector
                           1                    ;one sector
                           2                    ;dma-count
                           (ASH (1+ lam-multibus-mapping-register-base)
                                10.))           ;buffer area address
                          (disk-go-command)
                          (wait-for-iopb-completion-nubus #o650)
                          (cond ((or (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 0)) #/F))
                                     (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 1)) #/O))
                                     (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 2)) #/O))
                                     (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 3)) #/B)))
                                 (ferror nil "mini-label check word doesn't check"))
                                ((or (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 4)) #/L))
                                     (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 5)) #/I))
                                     (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 6)) #/S))
                                     (not (= (nubus-stuff:%bus-slot-byte-read
                                                      disk-buffer-quad-slot (+ (ash disk-buffer-rel-adr 2) 7)) #/P)))
                                 (ferror nil "can't find LISP mini label partition")))
                          (setq *lam-disk-cylinder-offset*
                                (nubus-stuff:%bus-slot-read disk-buffer-quad-slot
                                                            (+ (ash disk-buffer-rel-adr 2) #o10)))
                          ))))
                   (t
                    (setq *lam-disk-cylinder-offset* 100.)
                    (format t "~%asumming mini label since to memory available to read it into!!!")
                    )))))       ;foo no memory to read it into
  (format t "~&Setting LISP disk cylinder offset to ~D." *lam-disk-cylinder-offset*))


(defun initialize-mini-label (&optional (offset 100.))  offset
  (ferror nil "doesn't work")
  (comment
  (cond ((yes-or-no-p "are you sure you want to initialize the mini-label?")
         (let* ((mb-adr-base #o2000)
                (mb-word-base (// mb-adr-base 4)))
           (nubus-stuff:%multibus-write-32 mb-word-base (+ #/F (ASH #/O 8) (ASH #/O 16.) (ASH #/B 24.)))
           (nubus-stuff:%multibus-write-32 (1+ mb-word-base)
                              (+ #/L (ASH #/I 8) (ASH #/S 16.) (ASH #/P 24.)))
           (nubus-stuff:%multibus-write-32 (+ 2 mb-word-base) offset)
           (nubus-stuff:%multibus-write-32 (+ 3 mb-word-base) (- 842. offset))
           (nubus-stuff:%multibus-write-32 (+ 4 mb-word-base) -1)
           (set-iopb-pointer iopb-base-address)
           (build-iopb iopb-base-address
                       smd-write-command
                       default-unit
                       (if (< sectors-per-track 23.) 1 0)       ;head 0 or 1
                       0                        ;cylinder 0
                                                ;starting sector
                       (if (< sectors-per-track 23.) (- 22. sectors-per-track) 22.)
                       1                        ;one sector
                       2                        ;dma-count
                       mb-adr-base)             ;buffer area address
           (disk-go-command)
           (wait-for-iopb-completion))))))

;;;------------------------------------------------------------------------------------------------------------------------
;;;
;;;   Disk stuff


;(defprop eagle setup-for-eagle setup-disk)
(setf (getf 'eagle 'setup-disk) 'setup-for-eagle)

(defun setup-for-eagle ()
  (format t "~&Setting up for eagle")
  (setq lambda-disk-type 'eagle)
  (setq sectors-per-track 25.)
  (setq heads-per-cylinder 20.)
  (SETQ TRACKS-PER-DRIVE 842.))

;(defprop unknown setup-for-unknown setup-disk) ;

(setf (getf 'unknown 'setup-disk) 'setup-for-unknown)

(defun setup-for-unknown ()
  ;dont do it now since machine may be running.
  (format t "~&will read label to determine disk type")
  (setq n-heads nil
        n-blocks-per-track nil
        sectors-per-track nil
        heads-per-cylinder nil
        tracks-per-drive nil))

(defun really-setup-for-unknown ()
  (format t "~&Reading label to determine disk type")
  (setq sectors-per-track 25.   ;should only try to read blocks 0 and 1 using these.
        heads-per-cylinder 20.
        tracks-per-drive 842.)
  (read-label)
  (cond ((and (= n-heads 20.)
              (= n-blocks-per-track 25.))
         (format t "~%Disk is EAGLE.")
         (setup-for-eagle))
        ((and (= n-heads 19.)
              (= n-blocks-per-track 18.))
         (format t "~%Disk is T302")
         (setup-for-t302))
        ((and (= n-heads 24.)
              (= n-blocks-per-track 26.))
         (format t "~%Disk is CDC 500")
         (funcall (get 'cdc-500 'setup-disk)))
        (t
         (error "Disk type unknown.  Do setup for it and return.")))
  )

;(defprop t302 setup-for-t302 setup-disk)

(setf (getf 't302 'setup-disk) 'setup-for-t302)

(defun setup-for-t302 ()
  (format t "~&Setting up for T302")
  (setq lambda-disk-type 't302)
  (setq sectors-per-track 18.)
  (setq heads-per-cylinder 19.)
  (SETQ TRACKS-PER-DRIVE 800.))

;(defprop local setup-for-local setup-disk)

(setf (getf 'local 'setup-disk) 'setup-for-local)

(defun setup-for-local ()
  (setq lambda-disk-type 'local)
  (setq sectors-per-track (aref si:disk-sectors-per-track-array 0)
        heads-per-cylinder (aref si:disk-heads-per-cylinder-array 0)
        tracks-per-drive 842.)          ;** no check here for now.
  (format t "~&Setting up for local disk, ~D sectors per track, ~D heads." sectors-per-track heads-per-cylinder))

(DEFPARAMETER *GENERIC-DISK-PROPERTIES* ())

(DEFUN GETDISKP (PROP-NAME &OPTIONAL (DEFAULT NIL DEFAULTP))
  (LET ((CELL (GET *GENERIC-DISK-PROPERTIES* PROP-NAME)))
    (COND (CELL
           cell)
          (DEFAULTP
           DEFAULT)
          (T
           (ERROR "No ~S property specified for disk type ~S"
                   prop-name
                   (generic-disk-type))))))

(defun generic-disk-type ()
  (car *GENERIC-DISK-PROPERTIES*))

(defun generic-disk-name ()
  (getdiskp ':name
            (generic-disk-type)))

(DEFUN SETUP-FOR-GENERIC-DISK ()
  (FORMAT T "~&Setting up for ~A" (generic-disk-name))
  (setq lambda-disk-type (generic-disk-type))
  (setq sectors-per-track (getdiskp ':SECTORS-PER-TRACK))
  ;; this is sort of the number of virtual platters.
  (setq heads-per-cylinder (getdiskp ':HEADS-PER-CYLINDER))
  (SETQ TRACKS-PER-DRIVE (getdiskp ':CYLINDERS-PER-HEAD)))

(DEFUN DEFINE-GENERIC-DISK (*GENERIC-DISK-PROPERTIES*)
  (PUTPROP (CAR *GENERIC-DISK-PROPERTIES*)
           (CLOSURE '(*GENERIC-DISK-PROPERTIES*)
                    'SETUP-FOR-GENERIC-DISK)
           'SETUP-DISK)
  (PUTPROP (CAR *GENERIC-DISK-PROPERTIES*)
           (CLOSURE '(*GENERIC-DISK-PROPERTIES*)
                    'INITIALIZE-GENERIC-DISK)
           'INITIALIZE-DISK))

(DEFINE-GENERIC-DISK
  '(CDC-500 :SECTORS-PER-TRACK 26.
            :HEADS-PER-CYLINDER 24.
            :CYLINDERS-PER-HEAD 711.
            :BYTES-PER-SECTOR 1024.
            :GAP1 28.
            :GAP2 28.
            :INTERLEAVE 3.
            :RETRY-COUNT 3.
            :SPIRAL-SKEW-factor 6.
            :DUAL-PORT 0))


(DEFCONSTANT IP-ERROR-CODES-ALIST
  '((0 . "everything ok, or the controller hasn't looked yet")
    (#X10 . "disk-not-ready")
    (#X11 . "invalid disk address")
    (#X12 . "seek error")
    (#X13 . "ecc code error-data field")
    (#X14 . "invalid command code")
    (#X15 . "/"unused/"")
    (#X16 . "invalid sector in command")
    (#X17 . "/"spare/"")
    (#X18 . "bus timeout")
    (#X19 . "/"not used/"")
    (#X1A . "disk write-proctected")
    (#X1B . "unit not selected")
    (#X1C . "no address mark - header field")
    (#X1D . "/"not used/"")
    (#X1E . "drive faulted")
    (#X1F . "/"not used/"")
    (#X20 . "/"not used/"")
    (#X21 . "/"not used/"")
    (#X22 . "/"not used/"")
    (#X23 . "uncorrectable error")
    (#X24 . "/"spare/"")
    (#X25 . "/"spare/"")
    (#X26 . "no sector pulse")
    (#X27 . "data overrun")
    (#X28 . "no index pulse on write format")
    (#X29 . "sector not found")
    (#X2A . "id field error-wrong head")
    (#X2B . "invalid sync in data field")
    (#X2D . "seek timeout error")
    (#X2E . "busy timeout")
    (#X2F . "not on cylinder")
    (#X30 . "rtz timeout")
    (#X31 . "format overrun on data")
    (#X40 . "unit not initialized")
    (#X42 . "gap specification error")
    (#X4B . "seek error")
    (#X4C . "mapped header error")
    (#X50 . "sector per track error in UIB, 2190 only")
    (#X51 . "bytes//sector speccification error")
    (#X52 . "interleave specification error")
    (#X53 . "invalid head address")
    (#X54 . "invalid DMA burst count, 2190 only")))


(defun print-ip-error (error-number)
  (cond ((not (zerop error-number))
         (format t "~%ip disk error  #x~x: " error-number)
         (let ((error-message (assoc error-number ip-error-codes-alist)))
           (cond ((null error-message)
                  (format t "not listed in manual"))
                 (t
                  (format t (cdr error-message))))))
        (t (format t "~%no errors yet"))))


;;;------------------------------------------------------------------------------------------------------------------------
;;;
;;; Iop completion code.

(DEFCONSTANT interphase-timeout (* 3. 60.))

;if no second arg, IOPB is in multbus memory.  Otherwise, it can be in NUBUS memory.
(DEFUN WAIT-FOR-IOPB-COMPLETION (&OPTIONAL (BREAK-ON-ERROR-P T) (FCTN 'READ-IOPB))
  (prog2
    (if share-trace (send standard-output ':tyo #/w))
    (DO ((STATUS (READ-IOPB-STATUS FCTN) (READ-IOPB-STATUS FCTN))
         (START-TIME (TIME)))                   ;initialize time
        (())
      (COND ((OR (= STATUS 0)
                 (= STATUS SMD-BUSY)))          ;command in progress
            ((= STATUS SMD-OP-OK)
             (RETURN STATUS))
            ((= STATUS SMD-ERROR)
             (COND ((NOT (NULL BREAK-ON-ERROR-P))
                    (PRINT-IP-ERROR (READ-IOPB-ERROR FCTN))
                    (ferror 'eagle-disk-error "disk error  status ~o, type ~O" STATUS
                            (READ-IOPB-ERROR FCTN))
                    (return status))
                   (T (RETURN STATUS))))
            (T (FORMAT T "~%bad disk status read ~o: current iopb" STATUS)
               (PRINT-IOPB IOPB-BASE-ADDRESS FCTN)))
      (when ( (TIME-DIFFERENCE (TIME) START-TIME) interphase-timeout)
        (cerror "Do ~S and try again" "Disk timeout" '(initialize-disk-control))
        (initialize-disk-control)
        (disk-go-command)
        (SETQ START-TIME (TIME)))
      )
    (if share-trace (send *standard-output* :tyo #/W))))


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

;;;------------------------------------------------------------------------------------------------------------------------
;;;
;;; Formatting a drive.

;;formatting : the 2181 has a built-in format facility
;;      to format a track, you specify the cylinder select byte and then go
;;      (a "track" is a particular cylinder/head combination)
;;
;;      {byte 9 of the iopb used to be used to specify a data pattern to initialize
;;      the track in the 2180 ... again, sigh - a feature that apparently is now gone}

(defparameter *bad-block-list* nil)

(defun format-track (cylinder head &optional (read-back NIL))
 (*catch 'win
   (do ((tries 0 (1+ tries)))
       (())
     (*catch 'again
       (progn
         (set-iopb-pointer iopb-base-address)
         (build-iopb iopb-base-address
                     smd-format-track-command
                     default-unit
                     head
                     cylinder
                     0                          ;starting sector irrelevent
                     0                          ;number of sectors irrelevent
                     #o20                               ;dma-count irrelevent
                     #o100)                     ;buffer area address irrelevent
         (disk-go-command)
         (cond ((not (= (WAIT-FOR-IOPB-COMPLETION nil) #o200))
                (format t "~%lose in format, trying again")
                (*throw 'again t)))
         (cond ((not (null read-back))
                (do ((sec 0 (+ sec 1)))
                    ((= sec sectors-per-track))
                  (BUILD-IOPB IOPB-BASE-ADDRESS
                              smd-read-command
                              DEFAULT-UNIT
                              HEAD
                              CYLINDER
                              SEC
                              1         ;for 1024 byte sectors
                              2
                              #o100)
                  (disk-go-command)
                  (cond ((not (= (WAIT-FOR-IOPB-COMPLETION nil) #o200))
                         (format t "~%lose in read-check at sector ~s, trying reformat"
                                 sec)
                         (print-iopb)
                         (let ((block-number (+ (* cylinder (* heads-per-cylinder
                                                               sectors-per-track))
                                                (* head sectors-per-track)
                                                sec)))
                           (cond ((member block-number *bad-block-list*)) ;already known bad
                                 ((> tries 5.)
                                  (format t
                                        "~%giving up on cyl ~s, head ~s, sec starting ~s"
                                        cylinder head sec)
                                  (setq *bad-block-list*
                                        (cons block-number *bad-block-list*)))
                                 (t
                                  (*throw 'again t)))))))))
         (*throw 'win #o200))))))

(defun map-bad-blocks (remap-cylinder remap-head
                       &optional (*bad-block-list* *bad-block-list*))
  (dolist (block *bad-block-list*)
    (let* ((b-per-t sectors-per-track)
           (b-per-c (* b-per-t heads-per-cylinder))
           (cylinder (// block b-per-c))
           (head (// (- block (* cylinder b-per-c))
                     b-per-t))
           (block-within-track (- block (+ (* cylinder b-per-c)
                                           (* head b-per-t)))))
      (cond ((>= block-within-track b-per-t)
             (ferror nil "losey losey")))
      (format t
              "~%remapping cylinder ~s, head ~s, to cylinder ~s, head ~s because of block ~s"
              cylinder head remap-cylinder remap-head block)
      (map-bad-track cylinder head remap-cylinder remap-head)
      (setq remap-head (1+ remap-head))
      (cond ((>= remap-head heads-per-cylinder)
             (setq remap-head 0
                   remap-cylinder (1+ remap-cylinder)))))))

(defun map-bad-track (bad-cyl bad-head new-cyl new-head)
  (set-iopb-pointer iopb-base-address)
  (build-remap-iopb iopb-base-address
                    default-unit
                    bad-head
                    bad-cyl
                    new-head
                    new-cyl)
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION nil))


(defun check-smd-disk (first-cylinder n-cylinders)
  (dotimes (cylinder-delta n-cylinders)
    (let ((cylinder (+ first-cylinder cylinder-delta)))
      (format t "~%cylinder ~s " cylinder)
      (check-cylinder cylinder))))

(defun check-cylinder (cylinder)
  (dotimes (head heads-per-cylinder)
    (format t "H-~s " head)
    (check-track cylinder head)))

(DEFUN CHECK-TRACK (CYLINDER HEAD
                    &optional (sectors-worth sectors-per-track)
                              (step sectors-per-track)) ;set this = 1 for more precision
  (do ((sec 0 (+ sec step))
       (tries 0 0)
       (sec-done 0 (+ step sec-done)))
      ((>= sec-done sectors-worth))
    (cond ((= sec sectors-per-track)
           (setq sec 0 head (1+ head))))
    (cond ((= head heads-per-cylinder)
           (setq head 0 cylinder (1+ cylinder))))
  TOP
    (SETQ TRIES (1+ TRIES))
    (set-iopb-pointer iopb-base-address)  ;so its self contained for calling directly..
    (BUILD-IOPB IOPB-BASE-ADDRESS
                smd-read-command
                DEFAULT-UNIT
                HEAD
                CYLINDER
                SEC
                STEP
                2
                #o100)
    (disk-go-command)
    (cond ((not (= (WAIT-FOR-IOPB-COMPLETION nil) 200))
           (format t "~%lose at sector ~s,"
                   sec)
           (print-iopb)
           (let ((block-number (+ (* cylinder (* heads-per-cylinder
                                                 sectors-per-track))
                                  (* head sectors-per-track)
                                  sec)))
             (cond ((member block-number *bad-block-list*))     ;already known bad
                   ((> tries 5.)
                    (format t
                            "~%giving up on cyl ~s, head ~s, sec starting ~s"
                            cylinder head sec)
                    (setq *bad-block-list*
                          (cons block-number *bad-block-list*)))
                   (t
                    (GO TOP))))))))

(defun verify-track (cylinder head)
  (set-iopb-pointer iopb-base-address)
  (build-iopb iopb-base-address
              smd-restore-command
              default-unit
              head
              cylinder
              0         ;starting sector
              25.       ;# sectors
              #o20                              ;dma-count irrelevent
              #o100)                            ;buffer area address irrelevent
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION nil))       ;turn off break-on-error-p

(defun restore-smd ()
  (set-iopb-pointer iopb-base-address)
  (build-iopb iopb-base-address
              smd-restore-command
              default-unit
              0                                 ;head = 0
              0                                 ;cylinder = 0
              0                                 ;starting sector irrelevent
              0                                 ;number of sectors irrelevent
              #o20                              ;dma-count irrelevent
              #o100)                            ;buffer area address irrelevent
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION))

(defun format-and-verify-smd-cylinder (cylinder number-of-heads)
  (COND ((format-smd-cylinder cylinder number-of-heads)
         (verify-smd-cylinder cylinder number-of-heads))
        (T (FERROR NIL "failed to format cylinder ~d" CYLINDER))))

(DEFUN FORMAT-WHOLE-EAGLE NIL
  (FORMAT T "are you sure you want to clobber the whole disk??")
  (COND ((YES-OR-NO-P)
         (FORMAT-SMD-DISK TRACKS-PER-DRIVE HEADS-PER-CYLINDER))))

(defun format-smd-disk (number-of-cylinders number-of-heads &optional (first-cylinder 0))
  (setq *bad-block-list* nil)
  (do ((cylinder first-cylinder (1+ cylinder))
       (count 0 (1+ count)))
      (( count number-of-cylinders))
    (format t "~%cylinder ~O"cylinder)
    (format-and-verify-smd-cylinder cylinder number-of-heads)))

(defun format-smd-cylinder (cylinder number-of-heads)   ;give this a default value
    (do ((format-won t)
       (head 0 (1+ head)))
      (( head number-of-heads)
       format-won)
    (let ((format-result (format-track cylinder head)))
      (cond ((not (= format-result smd-op-ok))
             (format t "~%status ~o format error type ~o at head ~o~%"
                     format-result (read-iopb-error) head)
             (setq format-won nil))
            (t (format t " F-~O" head))))))

(defun verify-smd-cylinder (cylinder number-of-heads)
  (do ((verify-won t)
       (head 0 (1+ head)))
      (( head number-of-heads)
       verify-won)
    (let ((verify-result (verify-track cylinder head)))
      (cond ((not (= verify-result smd-op-ok))
             (format t "~%status ~o verify error type ~o at head ~o~%"
                     verify-result (read-iopb-error) head)
              (setq verify-won nil))
             (t (format t "~TV~O" head))))))

(DEFCONSTANT UIB-BASE-ADDRESS #o30)                     ;byte address

(defun write-uib (address-within-uib data)
  (LET* ((ADR (+ UIB-BASE-ADDRESS ADDRESS-WITHIN-UIB))
         (WD (nubus-stuff:%multibus-read-32 ADR)))
    (SETQ WD (hw:DPB-unboxed DATA (BYTE  8 (* (LOGAND ADDRESS-WITHIN-UIB 3) 8)) WD))
    (nubus-stuff:%multibus-write-32 ADR WD)))

(defun read-uib (address-within-uib)
  (LET ((WD (nubus-stuff:%multibus-read-32 (+ UIB-BASE-ADDRESS ADDRESS-WITHIN-UIB))))
    (hw:LDB WD (BYTE 8 (* (LOGAND ADDRESS-WITHIN-UIB 3) 8)) 0)))

(defun build-uib (uib-base-address
                  heads-per-unit
                  sectors-per-track
                  bytes-per-sector
                  gap1
                  gap2
                  interleave
                  retry-count
                  error-correction
                  reseek
                  bad-data
                  head-increment
                  dual-porting
                  interrupt-on-change-status
                  spiral-skew-factor
                  group-size)
  (write-uib 0 heads-per-unit)
  (write-uib 1 sectors-per-track)
  (write-uib 2 (hw:ldb bytes-per-sector 0010 0))        ;LOW byte (opposite convention of iopb)
  (write-uib 3 (hw:ldb bytes-per-sector 1010 0))        ;HIGH byte
  (write-uib 4 gap1)
  (write-uib 5 gap2)
  (write-uib 6 interleave)
  (write-uib 7 retry-count)
  (write-uib #o10 error-correction)
  (write-uib #o11 reseek)
  (write-uib #o12 bad-data)
  (write-uib #o13 head-increment)
  (write-uib #o14 dual-porting)
  (write-uib #o15 interrupt-on-change-status)
  (write-uib #o16 spiral-skew-factor)
  (write-uib #o17 group-size)
  (write-uib #o20 0)                            ;reserved
  (write-uib #o21 0)                            ;reserved
  (write-uib #o22 0))                           ;reserved

(defun read-uib-bytes-per-sector ()
  (dpb (read-uib 3) #o1010
       (logand #o377 (read-uib 2))))

(defun print-uib (&optional (uib-base-address uib-base-address))
  (format t "~%       heads per unit: ~o" (read-uib 0))
  (format t "~%    sectors per track: ~o" (read-uib 1))
  (format t "~%     bytes per sector: ~o" (read-uib-bytes-per-sector))
  (format t "~%                 gap1: ~o" (read-uib 4))
  (format t "~%                 gap2: ~o" (read-uib 5))
  (format t "~%           interleave: ~o" (read-uib 6))
  (format t "~%          retry count: ~o" (read-uib 7))
  (format t "~%     error correction: ~o" (read-uib #o10))
  (format t "~%               reseek: ~o" (read-uib #o11))
  (format t "~%             bad data: ~o" (read-uib #o12))
  (format t "~%       head increment: ~o" (read-uib #o13))
  (format t "~%         dual porting: ~o" (read-uib #o14))
  (format t "~%     status interrupt: ~o" (read-uib #o15))
  (format t "~%   spiral skew factor: ~o" (read-uib #o16))
  (format t "~%           group size: ~o" (read-uib #o17)))

(defun disk-reset-command ()
  (build-iopb    iopb-base-address
                 smd-reset-command
                 default-unit
                 0                              ;head
                 0                              ;cylinder
                 0                              ;starting address
                 0                              ;number of sectors
                 #o20                           ;dma count
                 #o100)                         ;buffer starting address
  (set-iopb-pointer iopb-base-address)
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION))


;to reset when iopb slots jam up:
; (print-share-iopbs)
; losers are "many that are similar" usually f0 (or randomness) hex in SLOT
; (invalidate-slot <iopb-slot-number>)

(DEFUN EAGLE-INITIALIZE ()      ;SHOULD NO LONGER BE CALLED BY ANY PROGRAM.
  (INITIALIZE-DISK-CONTROL))

(DEFUN INITIALIZE-DISK-CONTROL (&OPTIONAL &KEY
                                (UNIT DEFAULT-UNIT)
                                (INTERLEAVE nil)        ;nil means use default.
                                (GROUP-SIZE 0)  ;next three for 2190 only
                                (GRP 0)
                                (CE 0)
                                (PRINTOUT T)
                                (disk-type (P-disk-type *proc*))
                                )
  ce grp group-size
;  (if (= unit 0)
;      (setq lambda-disk-type disk-type))
  (cond ((null (P-disk-share-mode *proc*))
         (funcall (get disk-type 'initialize-disk)
                  unit interleave printout))
        (t
         (insert-share-iopb)
         (setup-for-disk)
         (funcall (get disk-type 'setup-disk)))))


(defun (eagle initialize-disk) (u i p)  ;somewhat of a fake for now.
  (eagle-initialize-eagle u i 0 0 0 p))

;;now converted for 1024 byte sectors.
;; Foo! with 1024 byte sectors, it wins on read with interleave of 3, but needs 4 on write!
(defun eagle-initialize-eagle (unit interleave
                               group-size grp ce   ;these are relavent to 2190 only
                               printout)
  (if (null interleave)
      (setq interleave 4))      ;default for eagle for now.
  (if printout (format t "~%initializing control for EAGLE on unit ~d" unit))
  (SETUP-FOR-DISK)
  (setup-for-eagle)
  (cond (nil ;  (access-path-lmi-serial-protocol *proc*)
         (format t "~%add something to serial protocol for (eagle-initialize)"))
        (t
         (build-uib uib-base-address
                    heads-per-cylinder          ;number of heads = 20
        ;*** see also dcheck-disk-xfer if these change ***
                    sectors-per-track           ;sectors/track = 25.
                    1024.                       ;bytes/sector = 1024
                    28.                         ;gap1 = 28.
                    (cond ((= group-size 0) 28.)        ;gap2 = 28.   2181
                          (t 31.))                      ;  2190
                    (hw:dpb grp #o0701 (hw:dpb ce #o0601 interleave))                   ;interleave
                    3                           ;retry count = 3
                    1                           ;error correction attempt allowed = 1
                    1                           ;number of reseek attempts = 1
                    0                           ;bad data allowed through = 0
                    1                           ;head-increment rather than seek = 1
                    0                           ;no dual-porting = 0
                    0                           ;don't allow interrupt-on-change-status = 0
                    12.                         ;spiral-skew-factor
                    group-size)
         (if printout (print-uib))
         (build-iopb iopb-base-address
                     smd-initialize-command
                     unit
                     0                          ;head
                     0                          ;cylinder
                     0                          ;starting address
                     0                          ;number of sectors
                     2                          ;dma count
                     (list uib-base-address))   ;uib address. Really at 30, do not offset!
  (if printout (print-iopb))
  (set-iopb-pointer iopb-base-address)
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION))))


;(defprop t302 eagle-initialize-t302 initialize-disk)

(setf (getf 't302 'initialize-disk) 'eagle-initialize-t302)

(defun eagle-initialize-t302 (unit interleave printout)
  (if (null interleave)
      (setq interleave 3))      ;default for t302.
  (SETUP-FOR-DISK)
  (if printout (format t "~%initializing control for T302 on unit ~d" unit))
  (setup-for-t302)
  (cond ((access-path-lmi-serial-protocol *proc*)
         (format t "~%add something to serial protocol for (eagle-initialize)"))
        (t
  (build-uib     uib-base-address
                 heads-per-cylinder             ;number of heads
        ;*** see also dcheck-disk-xfer if these change ***
                 sectors-per-track              ;sectors/track
                 1024.                          ;bytes/sector = 1024
                 20.                            ;gap1 = 20.
                 22.                            ;gap2 = 22.
                 interleave                     ;interleave
                 3                              ;retry count = 3
                 1                              ;error correction attempt allowed = 1
                 1                              ;number of reseek attempts = 1
                 0                              ;bad data allowed through = 0
                 1                              ;head-increment rather than seek = 1
                 0                              ;no dual-porting = 0
                 0                              ;don't allow interrupt-on-change-status = 0
                 12.                            ;spiral-skew-factor
                 0                              ;group size (2190)
                 )
  (if printout (print-uib))
  (build-iopb    iopb-base-address
                 smd-initialize-command
                 unit
                 0                              ;head
                 0                              ;cylinder
                 0                              ;starting address
                 0                              ;number of sectors
                 2                              ;dma count
                 uib-base-address)              ;uib address
  (if printout (print-iopb))
  (set-iopb-pointer iopb-base-address)
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION))))


(defun INITIALIZE-GENERIC-DISK (unit interleave printout)
  (if (null interleave)
      (setq interleave (GETDISKP ':INTERLEAVE)))
  (SETUP-FOR-DISK)
  (if printout (format t "~%initializing control for ~A on unit ~d"
                       (generic-disk-name)
                       unit))
  (FUNCALL (GET (generic-disk-type) 'SETUP-DISK))
  (cond ((access-path-lmi-serial-protocol *proc*)
         (format t "~%add something to serial protocol for (eagle-initialize)"))
        (t

  (build-uib     uib-base-address
                 heads-per-cylinder             ;number of heads
        ;*** see also dcheck-disk-xfer if these change ***
                 sectors-per-track              ;sectors/track
                 (getdiskp ':bytes-per-sector)
                 (getdiskp ':gap1)
                 (getdiskp ':gap2)
                 interleave
                 (getdiskp ':retry-count 3)
                 (getdiskp ':error-correction-attempt 1)
                 (getdiskp ':number-of-reseek-attempts 1)
                 (getdiskp ':bad-data-allowed-through 0)
                 (getdiskp ':head-increment-rather-than-seek 1)
                 (getdiskp ':dual-porting 0)
                 (getdiskp ':dont-allow-interrupt-on-change-status 0)
                 (getdiskp ':spiral-skew-factor)
                 (getdiskp ':group-size 0)
                 )
  (if printout (print-uib))
  (build-iopb    iopb-base-address
                 smd-initialize-command
                 unit
                 0                              ;head
                 0                              ;cylinder
                 0                              ;starting address
                 0                              ;number of sectors
                 2                              ;dma count
                 uib-base-address)              ;uib address
  (if printout (print-iopb))
  (set-iopb-pointer iopb-base-address)
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION))))

(comment
(defun eagle-initialize (&optional (unit default-unit))
  (build-uib     uib-base-address
                 20.                            ;number of heads = 20
                 25.                            ;sectors/track = 25
                 1024.                          ;bytes/sector = 1024
                 20.                            ;gap1 = 20
                 31.                            ;gap2 = 31
                 3                              ;interleave = 3
                 3                              ;retry count = 3
                 1                              ;error correction attempt allowed = 1
                 1                              ;number of reseek attempts = 1
                 0                              ;bad data allowed through = 0
                 1                              ;head-increment rather than seek = 1
                 0                              ;no dual-porting = 0
                 0                              ;don't allow interrupt-on-change-status = 0
                 12.                            ;spiral-skew-factor = 12
                 0)
  (build-iopb    iopb-base-address
                 smd-initialize-command
                 unit
                 0                              ;head
                 0                              ;cylinder
                 0                              ;starting address
                 0                              ;number of sectors
                 #o20                           ;dma count
                 uib-base-address)              ;uib address
  (set-iopb-pointer iopb-base-address)
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION))
);end comment

(defun SMD-SEEK (cylinder &optional (unit default-unit))
  (set-iopb-pointer iopb-base-address)
  (build-iopb iopb-base-address
              smd-SEEK-COMMAND
              unit
              0
              cylinder
              0                                 ;starting sector irrelevent
              0                                 ;number of sectors irrelevent
              #o20                              ;dma-count irrelevent
              #o100)                            ;buffer area address irrelevent
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION nil))       ;turn off break-on-error-p


(defun SMD-RECALIBRATE ()
  (set-iopb-pointer iopb-base-address)
  (build-iopb iopb-base-address
              smd-restore-command
              default-unit
              0
              0
              0                                 ;starting sector irrelevent
              0                                 ;number of sectors irrelevent
              #o20                              ;dma-count irrelevent
              #o100)                            ;buffer area address irrelevent
  (disk-go-command)
  (WAIT-FOR-IOPB-COMPLETION T))

(DEFUN SEEK-ALTERNATE (&OPTIONAL (CYL1 0) (CYL2 400) (UNIT DEFAULT-UNIT))
  (DO () (())
    (PRINT (SMD-SEEK CYL1 UNIT))
    (PRINT (SMD-SEEK CYL2 UNIT))))


;returns t if wins
; if DISK-BLOCK-NUM a list, its (track head sector).
(DEFUN LAM-DISK-XFER-VIA-MULTIBUS (FCN DISK-BLOCK-NUM MB-ADR N-BLOCKS
                      &OPTIONAL (ONE-SECTOR-TRANSFERS ()))
  (PROG (CYLINDER HEAD SECTOR ERRCNT DBN
         FINAL-ADDRESS FINAL-SECTOR FINAL-HEAD FINAL-CYLINDER)
        (SET-IOPB-POINTER IOPB-BASE-ADDRESS)
        (LET ((SECTORS-PER-CYLINDER (* sectors-per-track heads-per-cylinder)) ;BLOCKS-PER-CYLINDER
              ;(SECTORS-PER-TRACK BLOCKS-PER-TRACK)
              )
          (SETQ DBN DISK-BLOCK-NUM)
        ;  (COND ((NOT LAM-DISK-TYPE)(LAM-DISK-INIT)))
          (SETQ ERRCNT LAM-DISK-RETRY-COUNT)
          (COND ((NUMBERP DBN)
                ; (SETQ DBN (* DBN 2))          ;for 512 byte sectors
                 (SETQ CYLINDER (// DBN SECTORS-PER-CYLINDER))
                 (SETQ SECTOR (\ DBN SECTORS-PER-CYLINDER))
                 (SETQ HEAD (// SECTOR SECTORS-PER-TRACK)
                       SECTOR (\ SECTOR SECTORS-PER-TRACK))
                 (SETQ FINAL-ADDRESS (+ DBN (1- N-BLOCKS))
                       FINAL-CYLINDER (// FINAL-ADDRESS SECTORS-PER-CYLINDER)
                       FINAL-SECTOR (\ FINAL-ADDRESS SECTORS-PER-CYLINDER)
                       FINAL-HEAD (// FINAL-SECTOR SECTORS-PER-TRACK)
                       FINAL-SECTOR (\ FINAL-SECTOR SECTORS-PER-TRACK)))
                (T (SETQ CYLINDER (CAR DBN)
                         HEAD (CADR DBN)
                         SECTOR (CADDR DBN))))
          (setq cylinder (lam-offset-cylinder cylinder))
          (COND (ONE-SECTOR-TRANSFERS
                 (DOTIMES (BN N-BLOCKS)
                   (SETQ DBN (+ DISK-BLOCK-NUM BN))
                   (SETQ CYLINDER (// DBN SECTORS-PER-CYLINDER))
                   (SETQ SECTOR (\ DBN SECTORS-PER-CYLINDER))
                   (SETQ HEAD (// SECTOR SECTORS-PER-TRACK)
                         SECTOR (\ SECTOR SECTORS-PER-TRACK))
                   (BUILD-IOPB IOPB-BASE-ADDRESS
                               FCN
                               DEFAULT-UNIT
                               HEAD
                               CYLINDER
                               SECTOR
                               1                ;one sector
                               2
                               MB-ADR)
                   (DISK-GO-COMMAND)
                   (WAIT-FOR-IOPB-COMPLETION)
                   (SETQ MB-ADR (+ 2000 MB-ADR))))
                (T
                 (BUILD-IOPB IOPB-BASE-ADDRESS
                             FCN
                             DEFAULT-UNIT
                             HEAD
                             CYLINDER
                             SECTOR
                             N-BLOCKS
                             2
                             MB-ADR)
                 (DISK-GO-COMMAND)
                 (WAIT-FOR-IOPB-COMPLETION)))
          (RETURN T))))


(DEFUN LAM-DISK-XFER-VIA-NUBUS (FCN DISK-BLOCK-NUM CORE-PAGE-NUM N-BLOCKS
                                &optional (dma-count 2))
  (cond ((and (null sectors-per-track)
              (eq (send *proc* :disk-type) 'unknown))
         (really-setup-for-unknown)))
  (PROG (CYLINDER HEAD SECTOR ERRCNT DBN
         FINAL-ADDRESS FINAL-SECTOR FINAL-HEAD FINAL-CYLINDER)
        (LET ((SECTORS-PER-CYLINDER (* sectors-per-track heads-per-cylinder)) ;BLOCKS-PER-CYLINDER
              ;(SECTORS-PER-TRACK BLOCKS-PER-TRACK)
              )
          (SETQ DBN DISK-BLOCK-NUM)
  ;       (COND ((NOT LAM-DISK-TYPE)(LAM-DISK-INIT)))
          (SETQ ERRCNT LAM-DISK-RETRY-COUNT)
          (COND ((NUMBERP DBN)
                 (SETQ CYLINDER (// DBN SECTORS-PER-CYLINDER))
                 (SETQ SECTOR (\ DBN SECTORS-PER-CYLINDER))
                 (SETQ HEAD (// SECTOR SECTORS-PER-TRACK)
                       SECTOR (\ SECTOR SECTORS-PER-TRACK))
                 (SETQ FINAL-ADDRESS (+ DBN (1- N-BLOCKS))
                       FINAL-CYLINDER (// FINAL-ADDRESS SECTORS-PER-CYLINDER)
                       FINAL-SECTOR (\ FINAL-ADDRESS SECTORS-PER-CYLINDER)
                       FINAL-HEAD (// FINAL-SECTOR SECTORS-PER-TRACK)
                       FINAL-SECTOR (\ FINAL-SECTOR SECTORS-PER-TRACK)))
                (T (SETQ CYLINDER (CAR DBN)
                         HEAD (CADR DBN)
                         SECTOR (CADDR DBN))))
          (setq cylinder (lam-offset-cylinder cylinder))
          (WRITE-MULTIBUS-MAPPING-REGISTER
            lam-multibus-mapping-register-base
            (+ 1_23. (CADR-PAGE-TO-NUBUS-PAGE 1)))      ;sys-communication-area, page 1
          (SET-IOPB-POINTER (+ (ASH lam-multibus-mapping-register-base 10.)
                               (* #o250 4)) nil)        ;byte adr within page.
          (DOTIMES (C N-BLOCKS)
            (WRITE-MULTIBUS-MAPPING-REGISTER
              (+ lam-multibus-mapping-register-base
                 lam-multibus-mapping-register-data-offset
                 c)
              (+ 1_23. (CADR-PAGE-TO-NUBUS-PAGE(+ C CORE-PAGE-NUM)))))
          (BUILD-IOPB-IN-NUBUS-MEMORY
            #o650
            FCN
            DEFAULT-UNIT
            HEAD
            CYLINDER
            SECTOR
            N-BLOCKS
            dma-count
            (ASH (+ lam-multibus-mapping-register-base
                    lam-multibus-mapping-register-data-offset)
                 10.))
          (DISK-GO-COMMAND)
          (WAIT-FOR-IOPB-COMPLETION-NUBUS 650)
          (RETURN T))))

(DEFUN LAM-MAP-BLOCK-NUMBER (DISK-BLOCK-NUM)
 disk-block-num
 (ferror nil "this can't work")
 (LET ((SECTORS-PER-CYLINDER (* sectors-per-track heads-per-cylinder)) ;BLOCKS-PER-CYLINDER
      ;(SECTORS-PER-TRACK BLOCKS-PER-TRACK)
        CYLINDER HEAD SECTOR DBN)
    ;(COND ((NOT LAM-DISK-TYPE)(LAM-DISK-INIT)))
    ;(SETQ DBN (* DISK-BLOCK-NUM 2))                    ;for 512 byte sectors
    (SETQ CYLINDER (// DBN SECTORS-PER-CYLINDER))
    (SETQ SECTOR (\ DBN SECTORS-PER-CYLINDER))
    (SETQ HEAD (// SECTOR SECTORS-PER-TRACK)
          SECTOR (\ SECTOR SECTORS-PER-TRACK))
    (LIST CYLINDER HEAD SECTOR)))


;;;------------------------------------------------------------------------------------------------------------------------
;;;
;controller setup for test rig
;s1 - 1,2,3,4,5,6 up; 7,8,9,10 down
;s2 - 2 up, rest down.
;w1  - c2               onboard -5
;w4  no connect         no interrupt
;w5  c1                 -12 from p1-79, p1-80
;w6  c1
;w7  c1
;w8  c2
;w9  c2
;w10 c1
;w11 connected(!!??)  controller supplies BCLK  is this right??
;w12
;w13 wired
;w14 wired
;w15 wired
;w16 wired
;w17 wired
;w18 wired
;w19 c-1        not highest or parallel priority

;setup for NU machine
;s1 - 2 up, rest down
;s2 - all down
;w1  c1         -5v from bus
;w4  no connect
;w5  no connect (-5v is comming from bus)
;w6  c1
;w7  c1
;w8  c2
;w9  c2
;w10 c1
;w11 not connected      no BCLK from this board
;w12
;w13, w14, w15, w16, w17 , w18 all wired
;w19 c-1        not highest or parallel priority.

(defun which-cylinders (partition-name &optional (unit "lam"))
  (multiple-value-bind (beginning-block length-in-blocks ignore name)
      (si:find-disk-partition partition-name nil (si:decode-unit-argument unit "label"))
    (format t "~%~A starts at block ~O (~D.) and is ~O (~D.) blocks long."
            name beginning-block beginning-block length-in-blocks length-in-blocks)
    (format t "~%It starts at")
    (print-cylinder-track-and-sector beginning-block)
    (format t "~% and goes to")
    (print-cylinder-track-and-sector (+ beginning-block length-in-blocks -1))))

(defun format-including-partition (partition-name &optional (unit "lam"))
  (multiple-value-bind (beginning-block length-in-blocks ignore name)
      (si:find-disk-partition partition-name nil (si:decode-unit-argument unit "label"))
    (format t "~%~A starts at block ~O (~D.) and is ~O (~D.) blocks long."
            name beginning-block beginning-block length-in-blocks length-in-blocks)
    (format t "~%It starts at")
    (print-cylinder-track-and-sector beginning-block)
    (format t "~% and goes to")
    (print-cylinder-track-and-sector (+ beginning-block length-in-blocks -1))
    (multiple-value-bind (beg-cylinder ignore ignore)
        (convert-to-cylinder-track-and-sector beginning-block)
          (multiple-value-bind (end-cylinder ignore ignore)
              (convert-to-cylinder-track-and-sector (+ beginning-block length-in-blocks -1))
            (format t "~%FORMATTING CYLINDERS ~S THRU ~S INCLUSIVE. OK?"
                    beg-cylinder end-cylinder)
            (cond ((yes-or-no-p)
                   (format-smd-disk (1+ (- end-cylinder beg-cylinder))
                                    heads-per-cylinder
                                    beg-cylinder)))))))

(defun print-cylinder-track-and-sector (lispm-block)
  (multiple-value-bind (cylinder head sector)
      (convert-to-cylinder-track-and-sector lispm-block)
    (format t " cylinder ~O (~D.)," cylinder cylinder)
    (format t " head ~O (~D.)," head head)
    (format t " sector ~O (~D.)." sector sector)))


(defun convert-to-cylinder-track-and-sector (lispm-block)
  (let* ((disk-block lispm-block)
         (b-per-c (* sectors-per-track heads-per-cylinder))
         (cylinder (// disk-block b-per-c))
         (head (// (\ disk-block b-per-c) sectors-per-track))
         (sector (\ disk-block sectors-per-track)))
    (values cylinder head sector)))

(defun check-partition (partition-name)
  (which-cylinders partition-name)
  (multiple-value-bind (beginning-block length-in-blocks)
      (si:find-disk-partition partition-name nil (si:decode-unit-argument "lam" "label"))
    (let ((from-cylinder (convert-to-cylinder-track-and-sector beginning-block))
          (to-cylinder (convert-to-cylinder-track-and-sector
                         (+ beginning-block length-in-blocks -1))))
      (do ((cyl from-cylinder (1+ cyl)))
          ((> cyl to-cylinder)) ;this may get extra, but it wont get too little
        (format t "~%cylinder ~O: " cyl)
        (check-cylinder cyl)))))

;;; random sdu stuff

; rtc = real time clock

(defun read-rtc-data ()
  (send *proc* :bus-slot-read #o17 (ash #x1c120 -2)))

(defun write-rtc-data (data)
  (send *proc* :bus-slot-write #o17 (ash #x1c120 -2) data))

(defun read-rtc-adr ()
  (send *proc* :bus-slot-read #o17 (ash #x1c124 -2)))

(defun write-rtc-adr (data)
  (send *proc* :bus-slot-write #o17 (ash #x1c124 -2) data))

(defun dump-rtc-area ()
  (dotimes (i 14)
    (print (send *proc* :bus-slot-read #o17 (+ (ash #x1c120 -2) i)))))

(defun dump-rtc-regs-in-hex ()
  (dotimes (i 14.)
    (format t "~%~x" (read-rtc-reg i))))

(defun read-rtc-reg (adr)
  (write-rtc-adr adr)
  (logand #o177777 (read-rtc-data)))

(defun write-rtc-reg (adr data)
  (write-rtc-adr adr)
  (write-rtc-data data))

(defun set-up-rtc ()
  (write-rtc-reg 12. #o200)                     ;put in set mode
  (dotimes (i 14.)
    (write-rtc-reg i 0))
  (write-rtc-reg 10. #o40)                      ;reg A: set 32KHz mode
  (write-rtc-reg 11. #o207)                     ;reg B: binary mode, 24 hr time, daylight
                                                ;       savings time mode, freeze counter
  (write-rtc-reg 12. 0)                         ;reg C: random interrupt bits
  (write-rtc-reg 13. #o200)                     ;reg D: valid time bit
  (multiple-value-bind (seconds minutes hours date month year day-of-week)
      (time:decode-universal-time (time:get-universal-time))
    (write-rtc-reg 0 seconds)
    (write-rtc-reg 2 minutes)
    (write-rtc-reg 4 hours)
    (write-rtc-reg 6 day-of-week)
    (write-rtc-reg 7 date)
    (write-rtc-reg 8 month)
    (write-rtc-reg 9 year))
  (write-rtc-reg 11. 7))                        ;allow time to start counting

;;; interval timer stuff

;;; transfer rate tests

;used to be format-track
(defun read-track (&optional (n-times #o100) (dma-count 4))
  (set-iopb-pointer iopb-base-address)
  (let ((beginning-time (time:fixnum-microsecond-time))
        (head 0)
        (cylinder 0))
    (dotimes (n n-times)
      (do ((sec 0 (+ sec 1)))
          ((>= sec sectors-per-track))
        (build-iopb iopb-base-address
                    smd-read-command
                    default-unit
                    head
                    cylinder
                    sec                         ;starting sector
                    1                           ;number of sectors
                    dma-count                   ;dma-count
                    #o100)                      ;buffer area address
        (disk-go-command)
        (cond ((not (= (WAIT-FOR-IOPB-COMPLETION nil) #o200))
               (format t "~%read didn't complete")))))
    (let ((ending-time (time:fixnum-microsecond-time)))
      (format t "~&~A seconds" (// (time:time-difference ending-time beginning-time)
                                   1000000.0)))))

(defun run-disk-timing-sequence (&aux min-revs-on-read min-revs-on-write)
  (dolist (formatted-interleave '(1 2 3))
    (dolist (gs '(1 2 4 8 13. 25.))
      (format-for-timing-sequence formatted-interleave gs)
      (dolist (nblocks '(1 2 4 8 13. 25.))
        (dolist (dma-count '(#o376 #o300 #o100 #o60 #o10 2))
          (format t "~%READING, dma count ~D, formatted interleave ~D, gs ~D, nblocks ~D"
                  dma-count formatted-interleave gs nblocks)
          (setq min-revs-on-read
                (read-track-via-nubus dma-count nblocks 0 smd-read-command t))
          (format t "~%WRITING, dma count ~D, formatted interleave ~D, gs ~D, nblocks ~D"
                  dma-count formatted-interleave gs nblocks)
          (setq min-revs-on-write
                (read-track-via-nubus dma-count nblocks 0 smd-write-command t))
          (cond ((and (> min-revs-on-read 20.)
                      (> min-revs-on-write 20.))
                 (format t "~%~%Thats bletcherously slow, not testing smaller DMA counts!~%")
                 (return nil))))))))

(defun format-for-timing-sequence (interleave gs)
  (INITIALIZE-DISK-CONTROL ':interleave interleave ':group-size gs ':grp 1 ':ce 1
                           ':printout nil)
  (format-track 0 0)
  (format-track 0 1))

(defun read-track-via-nubus (&optional (dma-count 4)
                             (n-blocks-at-a-time 1)
                             (cylinder 830.)
                             (command smd-read-command)
                             no-warning-on-write
                             &aux min-revs)
  (cond ((not (< dma-count #o400))
         (ferror nil "MAX DMA COUNT IS 377")))
  (cond ((and (null no-warning-on-write)
              (not (= command smd-read-command)))
         (if (null (yes-or-no-p "are you sure you want to write?"))
             (ferror nil "ok then, I wont"))))
  (dotimes (x 3)
    (hacks:with-real-time
      (let ((total-time 0)
            (track-start (* SECTORS-PER-TRACK HEADS-PER-CYLINDER
                            (- cylinder *lam-disk-cylinder-offset*))))
        (do ((page 0 (+ page n-blocks-at-a-time)))
            ((>= page SECTORS-PER-TRACK))
          (setq total-time (+ total-time
                              (timing-lam-disk-xfer-via-nubus
                                command
                                (+ page track-start)    ;disk page
                                #o10    ; page in core
                                n-blocks-at-a-time
                                dma-count))))
        (let ((revs (* (// 3961.0 60.) total-time)))
          (format t "~&~A seconds; ~A revolutions " total-time revs)
          (cond ((or (null min-revs)
                     (< revs min-revs))
                 (setq min-revs revs)))))))
  (fix min-revs))

(DEFUN TIMING-LAM-DISK-XFER-VIA-NUBUS (FCN DISK-BLOCK-NUM CORE-PAGE-NUM N-BLOCKS
                                &optional (dma-count 2))
  (PROG (CYLINDER HEAD SECTOR ERRCNT DBN
         FINAL-ADDRESS FINAL-SECTOR FINAL-HEAD FINAL-CYLINDER)
        (LET ((SECTORS-PER-CYLINDER (* sectors-per-track heads-per-cylinder)) ;BLOCKS-PER-CYLINDER
              ;(SECTORS-PER-TRACK BLOCKS-PER-TRACK)
              )
          (SETQ DBN DISK-BLOCK-NUM)
         ;  (COND ((NOT LAM-DISK-TYPE)(LAM-DISK-INIT)))
          (SETQ ERRCNT LAM-DISK-RETRY-COUNT)
          (COND ((NUMBERP DBN)
                 (SETQ CYLINDER (// DBN SECTORS-PER-CYLINDER))
                 (SETQ SECTOR (\ DBN SECTORS-PER-CYLINDER))
                 (SETQ HEAD (// SECTOR SECTORS-PER-TRACK)
                       SECTOR (\ SECTOR SECTORS-PER-TRACK))
                 (SETQ FINAL-ADDRESS (+ DBN (1- N-BLOCKS))
                       FINAL-CYLINDER (// FINAL-ADDRESS SECTORS-PER-CYLINDER)
                       FINAL-SECTOR (\ FINAL-ADDRESS SECTORS-PER-CYLINDER)
                       FINAL-HEAD (// FINAL-SECTOR SECTORS-PER-TRACK)
                       FINAL-SECTOR (\ FINAL-SECTOR SECTORS-PER-TRACK)))
                (T (SETQ CYLINDER (CAR DBN)
                         HEAD (CADR DBN)
                         SECTOR (CADDR DBN))))
          (setq cylinder (lam-offset-cylinder cylinder))
          (WRITE-MULTIBUS-MAPPING-REGISTER
            lam-multibus-mapping-register-base
            (+ 1_23. (CADR-PAGE-TO-NUBUS-PAGE 1)))  ;sys-communication-area, page 1
          (SET-IOPB-POINTER (+ (ASH lam-multibus-mapping-register-base 10.)
                               (* #o250 4)) nil)  ;byte adr within page.
          (DOTIMES (C N-BLOCKS)
            (WRITE-MULTIBUS-MAPPING-REGISTER
              (+ lam-multibus-mapping-register-base
                 lam-multibus-mapping-register-data-offset
                 c)
              (+ 1_23. (CADR-PAGE-TO-NUBUS-PAGE (+ C CORE-PAGE-NUM)))))
          (BUILD-IOPB-IN-NUBUS-MEMORY
            #o650
            FCN
            DEFAULT-UNIT
            HEAD
            CYLINDER
            SECTOR
            N-BLOCKS
            dma-count
            (ASH lam-multibus-mapping-register-base 10.))
          (let ((beginning-time (time:fixnum-microsecond-time)))
            (DISK-GO-COMMAND)
            (WAIT-FOR-IOPB-COMPLETION-NUBUS #o650)
            (let* ((ending-time (time:fixnum-microsecond-time))
                   (elapsed-time (// (time:time-difference ending-time beginning-time)
                                     1000000.0)))
              (return elapsed-time))))))

(defparameter eagle-errors-while-copying nil)

;;; Copying a partition from one unit to another
si:
(DEFUN LAM-COPY-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART
                            &OPTIONAL (N-PAGES-AT-A-TIME 1) (DELAY NIL)
                                      (STARTING-HUNDRED 0) (WHOLE-THING-P NIL)
                            &AUX FROM-PART-BASE FROM-PART-SIZE TO-PART-BASE TO-PART-SIZE RQB
                                 PART-COMMENT)
  "Copy partition FROM-PART on FROM-UNIT to partition TO-PART on TO-UNIT.
While names of other machines can be specified as units, this
is not very fast for copying between machines.
Use SI:RECEIVE-BAND or SI:TRANSMIT-BAND for that."
  (SETQ FROM-UNIT (DECODE-UNIT-ARGUMENT FROM-UNIT
                                        (FORMAT NIL "reading ~A partition" FROM-PART))
        TO-UNIT (DECODE-UNIT-ARGUMENT TO-UNIT
                                      (FORMAT NIL "writing ~A partition" TO-PART)
                                      NIL
                                      T))
  (UNWIND-PROTECT
   (PROGN
     (SETQ RQB (GET-DISK-RQB N-PAGES-AT-A-TIME))
     (MULTIPLE-VALUE (FROM-PART-BASE FROM-PART-SIZE NIL FROM-PART)
       (FIND-DISK-PARTITION-FOR-READ FROM-PART NIL FROM-UNIT))
     (MULTIPLE-VALUE (TO-PART-BASE TO-PART-SIZE NIL TO-PART)
       (FIND-DISK-PARTITION-FOR-WRITE TO-PART NIL TO-UNIT))
     (SETQ PART-COMMENT (PARTITION-COMMENT FROM-PART FROM-UNIT))
     (FORMAT T "~&Copying ~S" PART-COMMENT)
     (AND (OR (NUMBERP FROM-PART) (STRING-EQUAL FROM-PART "LOD" 0 0 3 3))
          (NOT WHOLE-THING-P)
          (not (and (closurep from-unit)
                    (eq (closure-function from-unit) 'FS:BAND-MAGTAPE-HANDLER)))
          (LET ((RQB NIL) (BUF NIL))
            (UNWIND-PROTECT
              (PROGN (SETQ RQB (GET-DISK-RQB 1))
                     (SETQ BUF (RQB-BUFFER RQB))
                     (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
                     (LET ((SIZE (DPB (AREF BUF (1+ (* 2 %SYS-COM-VALID-SIZE)))
                                      1010      ;Knows page-size is 2^8
                                      (LDB 1010 (AREF BUF (* 2 %SYS-COM-VALID-SIZE))))))
                       (COND ((AND (> SIZE 10) ( SIZE FROM-PART-SIZE))
                              (SETQ FROM-PART-SIZE SIZE)
                              (FORMAT T "... using measured size of ~D. blocks." SIZE)))))
              (RETURN-DISK-RQB RQB))))
     (FORMAT T "~%")
     (UPDATE-PARTITION-COMMENT TO-PART "Incomplete Copy" TO-UNIT)
     (COND ((AND (CLOSUREP TO-UNIT)             ;magtape needs to know this stuff before
                 (FUNCALL TO-UNIT ':HANDLES-LABEL))     ;writing file.
            (FUNCALL TO-UNIT ':PUT PART-COMMENT ':COMMENT)
            (FUNCALL TO-UNIT ':PUT FROM-PART-SIZE ':SIZE)))
                                                ;Old hack which used to move WIRE-DISK-RQB outside loop flushed because
                                                ; DISK-READ sets modified bits during WIRE-DISK-RQB, which we may need to do.
     (DO ((FROM-ADR (+ FROM-PART-BASE (* 100. STARTING-HUNDRED)) (+ FROM-ADR AMT))
          (TO-ADR (+ TO-PART-BASE (* 100. STARTING-HUNDRED)) (+ TO-ADR AMT))
          (FROM-HIGH (+ FROM-PART-BASE FROM-PART-SIZE))
          (TO-HIGH (+ TO-PART-BASE TO-PART-SIZE))
          (N-BLOCKS (* 100. STARTING-HUNDRED) (+ N-BLOCKS AMT))
          (N-HUNDRED STARTING-HUNDRED)
          (AMT))
         ((OR (>= FROM-ADR FROM-HIGH) (>= TO-ADR TO-HIGH)))
       (SETQ AMT (MIN (- FROM-HIGH FROM-ADR) (- TO-HIGH TO-ADR) N-PAGES-AT-A-TIME))
       (COND ((NOT (= AMT N-PAGES-AT-A-TIME))
              (RETURN-DISK-RQB RQB)
              (SETQ RQB (GET-DISK-RQB AMT))))
       (condition-case ()
           (DISK-READ RQB FROM-UNIT FROM-ADR)
         (eagle-disk-error
          (format t "~&read error on block ~O" (- from-adr from-part-base))
          (push from-adr lambda:eagle-errors-while-copying))
         (:no-error
          (DISK-WRITE RQB TO-UNIT TO-ADR)))
       (COND ((NOT (= (FLOOR (+ N-BLOCKS AMT) 100.) N-HUNDRED))
              (SETQ N-HUNDRED (1+ N-HUNDRED))
              (FORMAT T "~D " N-HUNDRED)))
       (IF DELAY (PROCESS-SLEEP DELAY)
         (PROCESS-ALLOW-SCHEDULE)))             ;kludge
     (UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT TO-UNIT))
   ;;Unwind-protect forms
   (RETURN-DISK-RQB RQB))
  (DISPOSE-OF-UNIT FROM-UNIT)
  (DISPOSE-OF-UNIT TO-UNIT))



;use this to clear out parity errors after power up.
(DEFUN WIPE-LOW-MEMORY (&OPTIONAL (AMOUNT #o100000))
  (DOTIMES (ADR AMOUNT)
    (PHYS-MEM-WRITE ADR 0)))

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

(DEFCONSTANT sharestruct-ptr #xff000080)
(DEFCONSTANT sharestruct-lock #xff000084)
(DEFCONSTANT sharestruct-debug-level #xff000085)

(DEFCONSTANT sharestruct-share-lock 0)
(DEFCONSTANT sharestruct-max-iopbs-offset 4)
(DEFCONSTANT sharestruct-current-iopb-offset 8)
(DEFCONSTANT sharestruct-valid-table-offset 12.)

(DEFCONSTANT share-iopb-runme-offset 0)
(DEFCONSTANT share-iopb-slot-offset 4)
(DEFCONSTANT share-iopb-type-offset 8)
(DEFCONSTANT share-iopb-iopb-offset 12.)
(DEFCONSTANT share-iopb-interrupt-offset 16.)

(defun sharestruct-valid-p ()
  (not (zerop (ldb (byte 20. 0) (read-8086-multibus-address sharestruct-ptr)))))

; these are word addresses in mem-slot
(DEFCONSTANT lambda-share-iopb-structure #o520) ; see qcom
(DEFCONSTANT debug-program-share-iopb-structure #o540)

(defparameter share-lock-prevent-accidental-recursion nil)

(DEFCONSTANT enable-locking t)

(defun share-lock ()
  (cond ((or (null enable-locking)
             (typep *proc* 'local-access-path)))  ;we might take a page fault and hang!!
        ((null share-lock-prevent-accidental-recursion)
         (setq share-lock-prevent-accidental-recursion t)
         (with-timeout ((* 60. 5) (ferror nil "timeout waiting for share-lock"))
           (do ()
               ((zerop (nubus-stuff:%bus-byte-read sharestruct-lock)))))
         (nubus-stuff:%bus-byte-write sharestruct-lock 1))
        (t
         (ferror nil "share-lock called while cadr apparently already had lock"))))

(defun share-unlock ()
  (cond ((or (null enable-locking)
             (typep *proc* 'local-access-path)))
        (t (setq share-lock-prevent-accidental-recursion nil)
           (nubus-stuff:%bus-byte-write sharestruct-lock 0))))

(defun read-8086-multibus-address (nubus-pointer-location)
  (let ((multibus-address
          (8086-ptr-to-multibus-address
            (cond ((zerop (ldb 0002 nubus-pointer-location))
                   (send *proc* :bus-quad-slot-read-unsafe
                         (ldb (byte 8 24.) nubus-pointer-location)
                         (ldb (byte 24. 0) nubus-pointer-location)))
                  (t
                   (logior (send *proc* :bus-read-byte-unsafe nubus-pointer-location)
                           (ash (send *proc* :bus-read-byte-unsafe
                                      (+ nubus-pointer-location 1)) 8)
                           (ash (send *proc* :bus-read-byte-unsafe
                                      (+ nubus-pointer-location 2)) 16.)
                           (ash (send *proc* :bus-read-byte-unsafe
                                      (+ nubus-pointer-location 3)) 24.)))))))
    (values (map-multibus-address multibus-address) multibus-address)))


(defun map-multibus-address (nubus-address)
  "return nubus-address, unless it points to the multibus, and is mapped to the nubus.
in that case, follow the mapping, and return that address"
  (cond ((not (= (ldb (byte 8 24.) nubus-address) #xff))
         nubus-address)
        (t
         (let ((map-to (read-multibus-mapping-register (ldb #o1212 nubus-address))))
           (cond ((ldb-test #o2701 map-to)      ; check valid bit
                  (dpb (ldb #o0026 map-to)
                       (byte 22. 10.)
                       (ldb (byte 10. 0) nubus-address)))
                 (t
                  nubus-address))))))

(defun print-share-iopbs (&optional print-iopbs)
  (format t "~&sharestruct-debug-level = ~d."
          (nubus-stuff:%bus-byte-read sharestruct-debug-level))
  (format t "~&sharestruct-lock = ~o"
          (nubus-stuff:%bys-byte-read sharestruct-lock))
  (share-lock)
  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (format t "~&sharestruct = ~x" sharestruct)
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (ferror nil "~&sharestruct pointer not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-read-byte
                          (+ sharestruct sharestruct-max-iopbs-offset )))
              (currentiopb (nubus-stuff:%bus-byte-read
                             (+ sharestruct sharestruct-current-iopb-offset))))
          (format t "~&maxiopbs = ~d" maxiopbs)
          (format t "~&currentiopb = ~d" currentiopb)

          (dotimes (n maxiopbs)
            (let ((valid (send *proc* :bus-read-byte-unsafe
                           (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                  (siopb (read-8086-multibus-address
                           (+ sharestruct sharestruct-valid-table-offset
                              (* 4 maxiopbs) (* n 4)))))
              (format t "~&slot ~d: (~x) valid = #x~x siopb = #x~x"
                      n
                      (+ sharestruct sharestruct-valid-table-offset (* 4 n))
                      valid siopb)
              (cond ((not (zerop valid))
                     (print-share-iopb siopb print-iopbs)))))))
    (share-unlock)))

(defun print-share-iopb (adr &optional print-iopbs)
  (format t "~&~4tshare-iopb at ~o (#x~x)" adr adr)
  (format t "~&~8trunme = ~o"
          (send *proc* :bus-read-byte-unsafe (+ adr share-iopb-runme-offset)))
  (format t "~&~8tslot = ~o (#x~:*~x)"
          (send *proc* :bus-read-byte-unsafe (+ adr share-iopb-slot-offset)))
  (format t "~&~8ttype = ~o"
          (send *proc* :bus-read-byte-unsafe (+ adr share-iopb-type-offset)))
  (let ((iopb-address (read-8086-multibus-address (+ adr share-iopb-iopb-offset))))
    (format t "~&~8tiopb = ~o ~:* ~x" iopb-address)
    (if print-iopbs
        (print-iopb-at-nubus-address iopb-address)))
  (let ((inter-multi-loc
          (read-8086-multibus-address (+ adr share-iopb-interrupt-offset))))
    (format t "~&~8tinterrupt = ~o (= nubus ~x)"
            inter-multi-loc (map-multibus-address inter-multi-loc))))

(DEFCONSTANT cadr-share-slot
          (cond ((= si:processor-type-code si:cadr-type-code) #o377)
                ((= si:processor-type-code si:lambda-type-code)
                 (cond ((not (boundp 'si:*my-op*)) #o376)
                       (t (- #o375 (si:op-proc-number si:*my-op*)))))))

(DEFCONSTANT cadr-share-type #o377)

(defun remove-share-iopb (&optional (slot cadr-share-slot) (type cadr-share-type) (ask-p t))
  (share-lock)
  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (ferror nil "~&sharestruct pointer not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-byte-read
                          (+ sharestruct sharestruct-max-iopbs-offset))))

          (dotimes (n maxiopbs)
            (let ((valid (send *proc* :bus-read-byte
                           (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                  (siopb (read-8086-multibus-address
                           (+ sharestruct sharestruct-valid-table-offset
                              (* maxiopbs 4) (* n 4)))))
              (cond ((not (zerop valid))
                     (let ((this-slot (send *proc* :bus-read-byte-unsafe
                                            (+ siopb share-iopb-slot-offset)))
                           (this-type (send *proc* :bus-read-byte-unsafe
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
                         (if (if ask-p (y-or-n-p "Flush this IOPB ")
                               (format t "Flushing this IOPB ")
                               t)
                             (nubus-stuff:%bus-byte-write
                                   (+ sharestruct sharestruct-valid-table-offset
                                      (* n 4)) 0))))))))))
    (share-unlock)))

(defun invalidate-slot (slot-number)
  (share-lock)
  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (ferror nil "sharestruct not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-byte-read
                              (+ sharestruct sharestruct-max-iopbs-offset))))
          (cond ((>= slot-number maxiopbs)
                 (ferror nil "there are only ~d slots" maxiopbs)))
          (nubus-stuff:%bus-write (+ sharestruct sharestruct-valid-table-offset
                                     (* slot-number 4)) 0)))
    (share-unlock)))

(defun insert-share-iopb ()
  (remove-share-iopb cadr-share-slot cadr-share-type nil)
  (let ((prime-memory-adr (+ (ash (cadr (car (P-memory-configuration-list *proc*))) 10.)
                             ;;(dpb (SEND *PROC* :MEM-SLOT) (byte 4 24.) #xf0000000)
                             (* debug-program-share-iopb-structure 4))))
    (format t "~%prime-memory-adr for iopb=#x~x" prime-memory-adr)
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-runme-offset) 0)
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-slot-offset) cadr-share-slot)
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-type-offset) cadr-share-type)

    ;;set up pointer from share-iopb to real iopb
    ;;like the old code, use 650 in virtual address space for iopb, and point
    ;; to it with our multibus mapping reg
    (write-multibus-mapping-register lam-multibus-mapping-register-base
                                     (+ 1_23. (cadr-page-to-nubus-page 1)))
    (nubus-stuff:%bus-write send *proc* :bus-write (+ prime-memory-adr share-iopb-iopb-offset)
                  (multibus-address-to-8086-ptr
                    (+ (ash lam-multibus-mapping-register-base 10.) (* #o250 4))))

    ;;no interrupts
    (nubus-stuff:%bus-write (+ prime-memory-adr share-iopb-interrupt-offset) 0)

    (share-lock)
    (unwind-protect
        (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
          (cond ((zerop (ldb (byte 20. 0) sharestruct))
                 (ferror nil "~&sharestruct pointer not set up yet")))
          (let ((maxiopbs (nubus-stuff:%bus-byte-read
                            (+ sharestruct sharestruct-max-iopbs-offset))))

            (dotimes (n maxiopbs (ferror nil "out of iopb slots"))
              (cond ((zerop (nubus-stuff:%bus-byte-read
                              (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                     (nubus-stuff:%bus-write
                       (+ sharestruct sharestruct-valid-table-offset
                          (* 4 maxiopbs) (* 4 n))
                       (multibus-address-to-8086-ptr
                         (+ (ash lam-multibus-mapping-register-base 10.) (* #o140 4))))
                     (nubus-stuff:%bus-byte-write
                       (+ sharestruct sharestruct-valid-table-offset (* 4 n)) 1)
                     (return nil))))))
      (share-unlock))))

(DEFCONSTANT multibus-interrupt-1 #xff01c1e4)
(DEFCONSTANT multibus-interrupt-7 #xff01c1fc)

(DEFCONSTANT share-trace nil)

(defun share-go ()
  (if share-trace (send standard-output ':tyo #/g))
  (let ((prime-memory-adr (+ (ash (cadr (car (P-memory-configuration-list *proc*))) 10.)
                             ;(dpb (SEND *PROC* :MEM-SLOT) (byte 4 24.) #xf0000000)
                             (* debug-program-share-iopb-structure 4))))
    (nubus-stuff:%bus-byte-write (+ prime-memory-adr share-iopb-runme-offset) 1)
    (nubus-stuff:%bus-byte-write multibus-interrupt-7 1)))

(defun share-go-slot (slot-num)
  (share-lock)
  (unwind-protect
      (let ((sharestruct (read-8086-multibus-address sharestruct-ptr)))
        (format t "~&sharestruct = ~x" sharestruct)
        (cond ((zerop (ldb (byte 20. 0) sharestruct))
               (ferror nil "~&sharestruct pointer not set up yet")))
        (let ((maxiopbs (nubus-stuff:%bus-byte-read
                          (+ sharestruct sharestruct-max-iopbs-offset )))
              (currentiopb (nubus-stuff:%bus-byte-read
                             (+ sharestruct sharestruct-current-iopb-offset))))
          (format t "~&maxiopbs = ~d" maxiopbs)
          (format t "~&currentiopb = ~d" currentiopb)

          (dotimes (n maxiopbs)
            (let ((valid (send *proc* :bus-read-byte-unsafe
                           (+ sharestruct sharestruct-valid-table-offset (* 4 n))))
                  (siopb (read-8086-multibu s-address
                           (+ sharestruct sharestruct-valid-table-offset
                              (* 4 maxiopbs) (* n 4)))))
              (format t "~&slot ~d: (~x) valid = #x~x siopb = #x~x"
                      n
                      (+ sharestruct sharestruct-valid-table-offset (* 4 n))
                      valid siopb)
              (cond ((not (zerop valid))
                     (print-share-iopb siopb t)))
              (cond ((and (= n slot-num)
                          (yes-or-no-p "Goose this one?"))
                     (send *proc* :bus-write-byte-unsafe (+ siopb share-iopb-runme-offset) 1)))))))
    (share-unlock)))
