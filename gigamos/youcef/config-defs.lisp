;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-

;; Copyright LISP Machine, Inc. 1984
;;   See filename "Copyright" for
;; licensing and release information.


(defparameter sdu-quad-slot #xff)

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
;;;   (dpb (aref array-16b (1+ (ash %system-configuration-version-number 1)))
;;;        #o2020
;;;        (aref array-16b (ash %system-configuration-version-number 1))))
;;; (defun set-%system-configuration-version-number (array-16b val)
;;;   (aset (ldb 20 val) array-16b (ash %system-configuration-version-number 1))
;;;   (aset (ldb 2020 val) array-16b (1+ (ash %system-configuration-version-number 1)))
;;;   val)
;;; (defsetf %system-configuration-version-number set-%system-configuration-version-number)

(eval-when (compile load eval)
(defun make-forms-for-sym (sym)
  (let ((set-func (intern (si:format nil "SET-~A" sym) (si:symbol-package sym))))
    `((declare (special ,sym))
      (defun ,sym (array-16b)
        (dpb (array:aref array-16b (1+ (* ,sym 2)))
                (byte 16. 16.)
                (array:aref array-16b (* ,sym 2))))
      (defun ,set-func (array-16b val)
        (setf (array:aref array-16b (* ,sym 2)) (ldb (byte 16. 0) val))
        (setf (array:aref array-16b (1+ (* ,sym 2))) (ldb (byte 16. 16.) val))
        val)
      (defsetf ,sym ,set-func))))
)

(defmacro define-accessors-for-structure (q-list)
  (let* ((q-list-string (si:symbol-name q-list))
         (string-length (si:length q-list-string)))
    (when (or (<= string-length 3)
            (not (si:string-equal "-qs" (si:subseq q-list-string (- string-length 3)))))
      (error "bad structure name"))
    (let* ((main-name (si:intern (si:subseq q-list-string 0 (- string-length 3))
                                 (si:symbol-package q-list)))
           (set-main-name (si:intern (si:format nil "SET-~A" main-name) (si:symbol-package q-list))))
      `(progn (defun ,main-name (array-16b n)
                (dpb (array:aref array-16b (1+ (* n 2)))
                        (byte 16. 16.)
                        (array:aref array-16b (* n 2))))
              (defun ,set-main-name (array-16b n val)
                (setf (array:aref array-16b (* n 2)) (ldb (byte 16. 0) val))
                (setf (array:aref array-16b (1+ (* n 2))) (ldb (byte 16. 16.) val)))
              (defsetf ,main-name ,set-main-name)
              ,@(do ((qs (eval q-list) (cdr qs))
                     (result nil)
                     )
                    ((null qs) result)
                  (setq result (append result (make-forms-for-sym (car qs)))))
              )
      )
    )
  )

(define-accessors-for-structure system-configuration-qs)
(define-accessors-for-structure processor-configuration-qs)
(define-accessors-for-structure chaos-share-dev-qs)
(define-accessors-for-structure share-tty-qs)

(defvar *sys-conf-loc*)

(defun k-make-sys-conf-virtual (slot offset)
  (multiple-value-bind (ar loc)
      (nubus-stuff:make-external-structure
        (hw:dpb-unboxed slot (byte 8. 24.) offset) vinc:*qs-in-cluster*
        'li:(unsigned-byte 16.)
        'sys-conf
        3)
    (setq *sys-conf-loc* loc)
    ar)
  )

(defun %lambda-sys-conf-virtual-adr ()
  *sys-conf-loc*
  )

(defmacro defprop (symbol value property)
  `(setf (get ',symbol ',property) ,value)
  )

(defun init-system-configuration-props ()
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

  )

;(defselect ((sys-conf named-structure-invoke))
;  (:describe (struct)
;    (format t "~&~S:" struct)
;    (dolist (q system-configuration-qs)
;      (print-sysconf-entry q struct)))
;  (:print-self (struct stream ignore ignore)
;    (printing-random-object (struct stream :typep)
;      (when (and (array-displaced-p struct) (not (array-indirect-p struct)))
;       (format stream "#x~16r"
;               (logand #o37777777777
;                       (virtual-to-local-phys
;                         (%p-contents-offset struct (si:array-data-offset struct))))))))
;  (:which-operations (ignore)
;    '(:describe :print-self :which-operations)))

;(defun print-sysconf-entry (q struct)
;  (format t "~&~s:~46t" q)
;  (let ((val (funcall q struct)))
;    (cond-every ((get q :nubus-physical-adr)
;                (format t "#x~16r " val))
;               ((get q :decimal)
;                (format t "~10r. " val))
;               (otherwise
;                (format t "~8r " val))
;               ((get q :four-byte-ascii)
;                (format t "/"~c~c~c~c/" "
;                        (hw:ldb val (byte 8. 0) 0)
;                        (hw:ldb val (byte 8. 8.) 0)
;                        (hw:ldb val (byte 8. 16.) 0)
;                        (hw:ldb val (byte 8. 24.) 0)))
;               ((get q :device-owner)
;                (cond ((= val #xffffffff)
;                       (format t "/"not present/" "))
;                      ((= val 0)
;                       (format t "/"free/" "))
;                      (t
;                       (format t "/"owned by slot ~d./" " (logand val #xf)))))
;               ((get q :screen-device)
;                (cond
;                  ((eq q '%processor-conf-console)
;                   (format t "/"~a/" " (nth (hw:ldb val (byte 8. 0) 0) processor-conf-console-types)))
;                  ((or (= (hw:ldb val (byte 8. 24.) 0) #xff)
;                       (= val 0))
;                   (format t "/"none/" "))
;                  (t
;                   (format t "/"~a" (nth (hw:ldb val (byte 8. 8.) 0) processor-conf-console-types))
;                   (case (hw:ldb val (byte 8. 8.) 0)
;                     (1
;                      (format t " in slot ~d." (hw:ldb val (byte 8. 0) 0)))
;                     (2
;                      (format t " in slot ~d., screen ~d"
;                              (hw:ldb val (byte 8. 0) 0)
;                              (hw:ldb val (byte 8. 16.) 0)))
;                     )
;                   (format t "/" ")
;                   )))
;               )))

(defun make-proc-conf-virtual (virtual-addr)
  (select-processor
    (:lambda
      (make-array (* 2 page-size)
                  :type 'art-16b
                  :displaced-to virtual-addr
                  :named-structure-symbol 'proc-conf
                  :leader-length 3))
    (:k
      (array:zl-make-array vinc:*qs-in-cluster*
                     :element-type '(li:unsigned-byte 16.)
                     :named-structure-symbol 'proc-conf
                     :leader-length 3
                     :displaced-to virtual-addr))
    )
  )

(defun init-processor-conf-prop ()

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
  )

;(defselect ((proc-conf named-structure-invoke))
;  (:describe (struct)
;    (format t "~&~S:" struct)
;    (dolist (q processor-configuration-qs)
;      (print-sysconf-entry q struct)))
;  (:print-self (struct stream ignore ignore)
;    (printing-random-object (struct stream :typep)
;      (when (and (array-displaced-p struct) (not (array-indirect-p struct)))
;       (format stream "#x~16r"
;               (logand #o37777777777
;                       (virtual-to-local-phys
;                         (%p-contents-offset struct (si:array-data-offset struct))))))))
;  (:which-operations (ignore)
;    '(:describe :which-operations :print-self)))

(defun make-chaos-share-virtual (virtual-addr)
  (select-processor
    (:lambda
      (make-array (* 2 page-size)
                  :type 'art-16b
                  :displaced-to virtual-addr
                  :named-structure-symbol 'chaos-share
                  :leader-length 3))
    (:k
      (array:zl-make-array vinc:*qs-in-cluster*
                     :element-type '(li:unsigned-byte 16.)
                     :named-structure-symbol 'chaos-share
                     :leader-length 3
                     :displaced-to virtual-addr))
    )
  )

(defun make-chaos-share-physical (physical-addr)
  (select-processor
    (:lambda
      (make-array (* 2 page-size)
              :type 'art-16b
              :displaced-to (sdu-phys-to-virtual physical-addr)
              :named-structure-symbol 'chaos-share
              :leader-length 3))
    (:k
      (array:zl-make-array vinc:*qs-in-cluster*
                     :element-type '(li:unsigned-byte 16.)
                     :named-structure-symbol 'chaos-share
                     :leader-length 3
                     :displaced-to (sdu-phys-to-virtual physical-addr)))
    )
  )

(defun init-chaos-share-prop ()
  (defprop %CHAOS-SHARE-INTR-ADDR t :nubus-physical-adr)
  )

;(defselect ((chaos-share named-structure-invoke))
;  (:describe (struct)
;    (format t "~&~S:" struct)
;    (dolist (q chaos-share-dev-qs)
;      (format t "~&~s:~40t~s" q (funcall q struct))
;      (when (get q :nubus-physical-adr)
;       (format t "~50,8t#x~16r" (funcall q struct)))))
;  (:print-self (struct stream ignore ignore)
;    (printing-random-object (struct stream :typep)
;      (when (and (array-displaced-p struct) (not (array-indirect-p struct)))
;       (format stream "#x~16r"
;               (logand #o37777777777
;                       (virtual-to-local-phys
;                         (%p-contents-offset struct (si:array-data-offset struct))))))))
;  (:which-operations (ignore)
;    '(:describe :print-self :which-operations)))

(defconstant conf-max-reasonable-size 4096. ;(* 2 page-size)
  "maximum number of 16 bit words in a strucutre shared with the other processors")

(defun make-share-tty-virtual (slot offset)
  (select-processor
    (:lambda
      (make-array conf-max-reasonable-size
                  :type 'art-16b
                  :displaced-to (sdu-phys-to-virtual physical-addr)
                  :named-structure-symbol 'share-tty
                  :leader-length 3))
    (:k
      (array:zl-make-array conf-max-reasonable-size
                     :element-type '(li:unsigned-byte 16.)
                     :named-structure-symbol 'share-tty
                     :leader-length 3
                     :displaced-to (sdu-phys-to-virtual physical-addr)))
    )
  )

;(defselect ((share-tty named-structure-invoke))
;  (:describe (struct)
;    (format t "~&~S:" struct)
;    (dolist (q share-tty-qs)
;      (format t "~&~s:~40t~s" q (funcall q struct))))
;  (:which-operations (ignore)
;    '(:describe :which-operations))
;  )

(defstruct (other-processor ;(:type :named-array)
             (:conc-name nil)
;                           (:print "#<~s ~o>"
;                                   'other-processor
;                                   (%pointer other-processor))
             )
  op-proc-conf
  op-proc-number ;slot number in proc conf
  op-chaos-xmit-ctl     ;displaced array to whole memory "channel"
                        ; This starts with chaos-share-dev-qs
  op-chaos-xmit-pkt     ;displaced array to buffer following chaos-share-dev-qs
  op-chaos-rcv-ctl
  op-chaos-rcv-pkt
  )

(defvar *other-processors* nil ; :unbound
  "Descriptions of all the other processors on this bus.")
(defvar *my-proc-conf* nil ; :unbound
  "A displaced array into this processor's processor configuration structure.")
(defvar *my-proc-number* nil ;:unbound
  "Slot number of this processor's RG board.")
(defvar *my-op*  nil ; :unbound
  "This processor's OTHER-PROCESSOR description.")
(defvar *sys-conf* nil ; :unbound
  "A displaced array into the system configuration structure.")

;shared memory variables
(defvar *global-shared-memory-8* nil ; :unbound
  "An ART-8B array covering the shared memory.")

(defvar *global-shared-memory-16* nil ; :unbound
  "An ART-16B array covering the shared memory.")

(defvar *global-shared-memory-32* nil ; :unbound
  "An ART-32B array covering the shared memory.")

(defvar *global-shared-memory-size* nil ; :unbound
  "Number of bytes in the shared memory.")

;;;

;(defun %set-processor-switch (bit state)
;  (select-processor
;    (:lambda
;      (%processor-switches (hw:dpb (if state 1 0) (byte 1 bit) (%processor-switches nil))))
;    ((:cadr :explorer)))
;  )

(defun %cache (semaphore) (%set-processor-switch 2. semaphore))
(defun %video-cache (semaphore) (%set-processor-switch 3. semaphore))
(defun %fast-cache (semaphore) (%set-processor-switch 4. semaphore))
(defun %multiplier (semaphore) (%set-processor-switch 29. semaphore))
(defun %microsecond-clock (semaphore) (%set-processor-switch 31. semaphore))
(defun %debug-illops (semaphore) (%set-processor-switch 23. semaphore))

;(defun print-processor-switches ()
;  (select-processor
;    (:cadr
;      (ferror nil "PROCESSOR-SWITCHES for Lambda only."))
;    (:lambda
;      (let ((switches (%processor-switches nil)))
;       (loop for l = lambda-processor-switches-bits then (cddr l)
;             until (null l)
;             for switch = (car l)
;             for byte = (cadr l)
;             for state = (ldb byte switches)
;             do (format t "~&~A~55T~A" switch state))))
;    (:explorer
;      (ferror nil "PROCESSOR-SWITCHES for Lambda only."))))

;(defun initialize-microsecond-clock (&aux start)
;  ;; 50000 do-loop units on a 200 nsec machine is about 1/10 second.
;  (select-processor
;    (:lambda
;      (%microsecond-clock t)
;      (setq start (time))
;      (dotimes (i 50000.))
;      (if (> (- (time) start) 4)
;         (%microsecond-clock t)
;       (%microsecond-clock nil)
;       (format t "~&[Microsecond clock wedged -- using TV clock.]")))
;    ((:cadr :explorer))))

;(defun print-all-config-structures ()
;  (format t "~&*SYS-CONF*")
;  (describe *sys-conf*)
;  (format t "~2&*MY-PROC-CONF*")
;  (describe *my-proc-conf*)
;  (dolist (p *other-processors*)
;    (format t "~2&~s" p)
;    (describe (op-proc-conf p))))

(defun init-config-structure-props ()
  (init-system-configuration-props)
  (init-processor-conf-prop)
  (init-chaos-share-prop)
  )
