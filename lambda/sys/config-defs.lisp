;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Lowercase:T; readtable: ZL -*-

;; Copyright LISP Machine, Inc. 1984
;;   See filename "Copyright" for
;; licensing and release information.

(defun local-phys-to-sdu-phys (local-phys)
  (dpb (ldb (byte 4 4) sdu-quad-slot) (byte 4 28.) local-phys))

(defun sdu-phys-to-local-phys (sdu-phys)
  (if (= sdu-quad-slot #xff)
      sdu-phys
    (logxor #x10000000 sdu-phys)))

(defun virtual-to-sdu-phys (virtual-adr)
  (local-phys-to-sdu-phys (%lambda-sys-conf-virtual-to-phys virtual-adr)))

(defun sdu-phys-to-virtual (sdu-phys)
  (%lambda-sys-conf-phys-to-virtual (sdu-phys-to-local-phys sdu-phys)))

(defun virtual-to-local-phys (virtual-adr)
  (%lambda-sys-conf-virtual-to-phys virtual-adr))

(defun local-phys-to-virtual (local-phys)
  (%lambda-sys-conf-phys-to-virtual local-phys))

;;; configuration structures

;;;This defines things like
;;;
;;; (declare (special %system-configuration-version-number))
;;; (defun %system-configuration-version-number (array-16b)
;;;   (dpb (aref array-16b (1+ (* 2 %system-configuration-version-number)))
;;;        2020
;;;        (aref array-16b (* 2 %system-configuration-version-number))))
;;; (defun set-%system-configuration-version-number (array-16b val)
;;;   (aset (ldb 20 val) array-16b (* 2 %system-configuration-version-number))
;;;   (aset (ldb 2020 val) array-16b (1+ (* 2 %system-configuration-version-number)))
;;;   val)
;;; (defsetf %system-configuration-version-number set-%system-configuration-version-number)

;;;|||Replaced Lambda byte constants with BYTE calls. -Keith 23-oct-88

(eval-when (compile load eval)
(defun make-forms-for-sym (sym)
  (let ((set-func (intern (string-append "SET-" sym) (symbol-package sym))))
    `((declare (special ,sym))
      (defun ,sym (array-16b)
        (dpb (aref array-16b (1+ (* 2 ,sym)))
             (BYTE 16. 16.)
             (aref array-16b (* 2 ,sym))))
      (defun ,set-func (array-16b val)
        (aset (ldb (BYTE 16. 0.) val) array-16b (* 2 ,sym))
        (aset (ldb (BYTE 16. 16.) val) array-16b (1+ (* 2 ,sym)))
        val)
      (defsetf ,sym ,set-func))))
)

(defmacro define-accessors-for-structure (q-list)
  (when (or ( (string-length q-list) 3)
            (not (string-equal "-qs" (substring q-list (- (string-length q-list) 3)))))
    (ferror nil "bad structure name"))
  (let* ((main-name (intern (substring q-list 0 (- (string-length q-list) 3))
                            (symbol-package q-list)))
         (set-main-name (intern (string-append "SET-" main-name) (symbol-package q-list))))
    `(progn (defun ,main-name (array-16b n)
              (dpb (aref array-16b (1+ (* 2 n)))
                   (BYTE 16. 16.)
                   (aref array-16b (* 2 n))))
            (defun ,set-main-name (array-16b n val)
              (aset (ldb (BYTE 16. 0.) val) array-16b (* 2 n))
              (aset (ldb (BYTE 16. 16.) val) array-16b (1+ (* 2 n))))
            (defsetf ,main-name ,set-main-name)
            ,@(loop for q in (eval q-list)
                    append (make-forms-for-sym q)))))

(define-accessors-for-structure system-configuration-qs)
(define-accessors-for-structure processor-configuration-qs)
(define-accessors-for-structure chaos-share-dev-qs)
(define-accessors-for-structure share-tty-qs)

(defun make-sys-conf-virtual (virtual-adr)
  (make-array (* 2 page-size)
              :type 'art-16b
              :displaced-to virtual-adr
              :named-structure-symbol 'sys-conf
              :leader-length 3))

(defprop %system-configuration-share-struct-pointer t :nubus-physical-adr)
(defprop %system-configuration-ethernet-owner t :device-owner)
(defprop %system-configuration-tapemaster-owner t :device-owner)
(defprop %system-configuration-mti-8-line-owner t :device-owner)
(defprop %system-configuration-mti-16-line-owner t :device-owner)
(defprop %system-configuration-quarter-inch-tape-owner t :device-owner)
(defprop %system-configuration-sdu-serial-a-owner t :device-owner)
(defprop %system-configuration-sdu-serial-b-owner t :device-owner)
(defprop %system-configuration-share-tty-0 t :nubus-physical-adr)
(defprop %system-configuration-grey-owner t :device-owner)
(defprop %system-configuration-global-shared-base t :nubus-physical-adr)
(defprop %system-configuration-excelan-owner t :device-owner)
(defprop %system-configuration-excelan-2-owner t :device-owner)
;; Copied from LAD: RELEASE-3.SYS; CONFIG-DEFS.LISP#16 on 26-Mar-87 16:27:23
(defprop %system-configuration-shared-excelan-pointer t :nubus-physical-adr)
(defprop %system-configuration-sdu-interrupt-map t :nubus-physical-adr)
(defprop %system-configuration-titn-owner t :device-owner)
(defprop %system-configuration-sdu-nubus-base t :nubus-physical-adr)
(defprop %system-configuration-cmos-clock-chip-owner t :device-owner)
(defprop %system-configuration-newboot-version-number t :nubus-physical-adr)
(defprop %system-configuration-sdu-rom-version-number t :decimal)
(defprop %system-configuration-burr-brown-owner t :device-owner)
(defprop %system-configuration-second-burr-brown-owner t :device-owner)
(defprop %system-configuration-interphase-2181-owner t :device-owner)
(defprop %system-configuration-nubus-disk-owner t :device-owner)
(defprop %system-configuration-second-grey-owner t :device-owner)
(defprop %system-configuration-default-grey-owner t :device-owner)
(defprop %system-configuration-default-second-grey-owner t :device-owner)
(defprop %system-configuration-flavors-bus-link-owner t :device-owner)
(defprop %system-configuration-second-flavors-bus-link-owner t :device-owner)
(defprop %system-configuration-lmi-debug-board-owner t :device-owner)
(defprop %system-configuration-second-lmi-debug-board-owner t :device-owner)
(defprop %system-configuration-chaos-sharedev-buffer-size-in-bytes t :decimal)

(defselect ((sys-conf named-structure-invoke))
  (:describe (struct)
    (format t "~&~S:" struct)
    (dolist (q system-configuration-qs)
      (print-sysconf-entry q struct)))
  (:print-self (struct stream ignore ignore)
    (printing-random-object (struct stream :typep)
      (when (and (array-displaced-p struct) (not (array-indirect-p struct)))
        (format stream "#x~16r"
                (logand 37777777777
                        (virtual-to-local-phys
                          (%p-contents-offset struct (si:array-data-offset struct))))))))
  (:which-operations (ignore)
    '(:describe :print-self :which-operations)))

(defun print-sysconf-entry (q struct)
  (format t "~&~s:~46t" q)
  (let ((val (funcall q struct)))
    (cond-every ((get q :nubus-physical-adr)
                 (format t "#x~16r " val))
                ((get q :decimal)
                 (format t "~10r. " val))
                (otherwise
                 (format t "~8r " val))
                ((get q :four-byte-ascii)
                 (format t "/"~c~c~c~c/" "
                         (ldb 0010 val)
                         (ldb 1010 val)
                         (ldb 2010 val)
                         (ldb 3010 val)))
                ((get q :device-owner)
                 (cond ((= val #xffffffff)
                        (format t "/"not present/" "))
                       ((= val 0)
                        (format t "/"free/" "))
                       (t
                        (format t "/"owned by slot ~d./" " (logand val #xf)))))
                ((get q :screen-device)
                 (cond
                   ((eq q '%processor-conf-console)
                    (format t "/"~a/" " (nth (ldb 0010 val) processor-conf-console-types)))
                   ((or (= (ldb 3010 val) #xff)
                        (= val 0))
                    (format t "/"none/" "))
                   (t
                    (format t "/"~a" (nth (ldb 1010 val) processor-conf-console-types))
                    (selectq (ldb 1010 val)
                      (1
                       (format t " in slot ~d." (ldb 0010 val)))
                      (2
                       (format t " in slot ~d., screen ~d"
                               (ldb 0010 val)
                               (ldb 2010 val)))
                      )
                    (format t "/" ")
                    )))
                )))

(defun make-proc-conf-virtual (virtual-addr)
  (make-array (* 2 page-size)
              :type 'art-16b
              :displaced-to virtual-addr
              :named-structure-symbol 'proc-conf
              :leader-length 3))

(defprop %PROCESSOR-CONF-SYS-CONF-PTR t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-0 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-1 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-2 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-3 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-4 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-5 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-6 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-7 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-8 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-MEMORY-BASE-9 t :nubus-physical-adr)
(defprop %processor-conf-vcmem-slot t :screen-device)
(defprop %PROCESSOR-CONF-MICRO-BAND  t :four-byte-ascii)
(defprop %PROCESSOR-CONF-LOAD-BAND t :four-byte-ascii)
(defprop %PROCESSOR-CONF-PAGING-BAND t :four-byte-ascii)
(defprop %PROCESSOR-CONF-FILE-BAND t :four-byte-ascii)
(defprop %PROCESSOR-CONF-CHAOS-SHARE-0 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-CHAOS-SHARE-1 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-CHAOS-SHARE-2 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-CHAOS-SHARE-3 t :nubus-physical-adr)
(defprop %PROCESSOR-CONF-CHAOS-SHARE-4 t :nubus-physical-adr)
(defprop %processor-conf-console t :screen-device)
(defprop %processor-conf-aux-dev-0 t :screen-device)
(defprop %processor-conf-aux-dev-1 t :screen-device)
;; Copied from LAD: RELEASE-3.SYS; CONFIG-DEFS.LISP#16 on 26-Mar-87 16:27:24
;(defprop %processor-conf-aux-dev-2 t :screen-device)
;(defprop %processor-conf-aux-dev-3 t :screen-device)
(defprop %processor-conf-excelan-multibus-map-base t :nubus-physical-adr)

(defselect ((proc-conf named-structure-invoke))
  (:describe (struct)
    (format t "~&~S:" struct)
    (dolist (q processor-configuration-qs)
      (print-sysconf-entry q struct)))
  (:print-self (struct stream ignore ignore)
    (printing-random-object (struct stream :typep)
      (when (and (array-displaced-p struct) (not (array-indirect-p struct)))
        (format stream "#x~16r"
                (logand 37777777777
                        (virtual-to-local-phys
                          (%p-contents-offset struct (si:array-data-offset struct))))))))
  (:which-operations (ignore)
    '(:describe :which-operations :print-self)))

(defun make-chaos-share-virtual (virtual-addr)
  (make-array (* 2 page-size)
              :type 'art-16b
              :displaced-to virtual-addr
              :named-structure-symbol 'chaos-share
              :leader-length 3))

(defun make-chaos-share-physical (physical-addr)
  (make-array (* 2 page-size)
              :type 'art-16b
              :displaced-to (sdu-phys-to-virtual physical-addr)
              :named-structure-symbol 'chaos-share
              :leader-length 3))

(defprop %CHAOS-SHARE-INTR-ADDR t :nubus-physical-adr)

(defselect ((chaos-share named-structure-invoke))
  (:describe (struct)
    (format t "~&~S:" struct)
    (dolist (q chaos-share-dev-qs)
      (format t "~&~s:~40t~s" q (funcall q struct))
      (when (get q :nubus-physical-adr)
        (format t "~50,8t#x~16r" (funcall q struct)))))
  (:print-self (struct stream ignore ignore)
    (printing-random-object (struct stream :typep)
      (when (and (array-displaced-p struct) (not (array-indirect-p struct)))
        (format stream "#x~16r"
                (logand 37777777777
                        (virtual-to-local-phys
                          (%p-contents-offset struct (si:array-data-offset struct))))))))
  (:which-operations (ignore)
    '(:describe :print-self :which-operations)))

(defconst conf-max-reasonable-size (* 2 page-size)
  "maximum number of 16 bit words in a strucutre shared with the other processors")

(defun make-share-tty-virtual (virtual-adr)
  (make-array conf-max-reasonable-size
              :type 'art-16b
              :displaced-to virtual-adr
              :named-structure-symbol 'share-tty
              :leader-length 3))

(defselect ((share-tty named-structure-invoke))
  (:describe (struct)
    (format t "~&~S:" struct)
    (dolist (q share-tty-qs)
      (format t "~&~s:~40t~s" q (funcall q struct))))
  (:which-operations (ignore)
    '(:describe :which-operations)))

(defstruct (other-processor (:type :named-array)
                            (:print "#<~s ~o>"
                                    'other-processor
                                    (%pointer other-processor)))
  op-proc-conf
  op-proc-number ;slot number in proc conf
  op-chaos-xmit-ctl     ;displaced array to whole memory "channel"
                        ; This starts with chaos-share-dev-qs
  op-chaos-xmit-pkt     ;displaced array to buffer following chaos-share-dev-qs
  op-chaos-rcv-ctl
  op-chaos-rcv-pkt
  )

(defvar *other-processors* :unbound
  "Descriptions of all the other processors on this bus.")
(defvar *my-proc-conf* :unbound
  "A displaced array into this processor's processor configuration structure.")
(defvar *my-proc-number* :unbound
  "Slot number of this processor's RG board.")
(defvar *my-op* :unbound
  "This processor's OTHER-PROCESSOR description.")
(defvar *sys-conf* :unbound
  "A displaced array into the system configuration structure.")

;shared memory variables
(defvar *global-shared-memory-8* :unbound
  "An ART-8B array covering the shared memory.")

(defvar *global-shared-memory-16* :unbound
  "An ART-16B array covering the shared memory.")

(defvar *global-shared-memory-32* :unbound
  "An ART-32B array covering the shared memory.")

(defvar *global-shared-memory-size* :unbound
  "Number of bytes in the shared memory.")

;;;

(defun %set-processor-switch (bit state)
  (select-processor
    (:lambda
      (%processor-switches (dpb (if state 1 0) (byte 1 bit) (%processor-switches nil))))
    ((:cadr :explorer))))

(defun %cache (semaphore) (%set-processor-switch 2. semaphore))
(defun %video-cache (semaphore) (%set-processor-switch 3. semaphore))
(defun %fast-cache (semaphore) (%set-processor-switch 4. semaphore))
(defun %multiplier (semaphore) (%set-processor-switch 29. semaphore))
(defun %microsecond-clock (semaphore) (%set-processor-switch 31. semaphore))
(defun %debug-illops (semaphore) (%set-processor-switch 23. semaphore))

(defun print-processor-switches ()
  (select-processor
    (:cadr
      (ferror nil "PROCESSOR-SWITCHES for Lambda only."))
    (:lambda
      (let ((switches (%processor-switches nil)))
        (loop for l = lambda-processor-switches-bits then (cddr l)
              until (null l)
              for switch = (car l)
              for byte = (cadr l)
              for state = (ldb byte switches)
              do (format t "~&~A~55T~A" switch state))))
    (:explorer
      (ferror nil "PROCESSOR-SWITCHES for Lambda only."))))

(defun initialize-microsecond-clock (&aux start)
  ;; 50000 do-loop units on a 200 nsec machine is about 1/10 second.
  (select-processor
    (:lambda
      (%microsecond-clock t)
      (setq start (time))
      (dotimes (i 50000.))
      (if (> (- (time) start) 4)
          (%microsecond-clock t)
        (%microsecond-clock nil)
        (format t "~&[Microsecond clock wedged -- using TV clock.]")))
    ((:cadr :explorer))))

(defun print-all-config-structures ()
  (format t "~&*SYS-CONF*")
  (describe *sys-conf*)
  (format t "~2&*MY-PROC-CONF*")
  (describe *my-proc-conf*)
  (dolist (p *other-processors*)
    (format t "~2&~s" p)
    (describe (op-proc-conf p))))
