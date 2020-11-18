;;; -*- Mode:LISP; Package:LISP-INTERNALS; Lowercase:T; Base:8; Readtable:ZL -*-

;; Copyright LISP Machine, Inc. 1984
;;   See filename "Copyright" for
;; licensing and release information.

;;;call this if you detect an error in a function that may be called
;;;early in lisp-reinitialize.  In that case, the call to FERROR will
;;;cause the machine to halt before entering the debugger, so the
;;;print will at least tell you something about it first
(defun config-ferror (condition &rest format-args)
  (print (list 'ferror condition format-args) cold-load-stream)
  (lexpr-funcall 'ferror condition format-args))

;; need this variable right now.
(defparameter page-rqb-size #o100)
(defparameter *share-code-ready* nil)

;; This says it's legal to use the sytem configuration structure
(defun sys-conf-structure-available-p ()
  (select-processor
    (:lambda
      (or (not (zerop (%nubus-read sdu-quad-slot #x80)))
          (not (zerop (ldb #o1101 (%nubus-read sdu-quad-slot #x84)))))
      )
    (:k
      (or (not (zerop (hw:dpb (nubus-stuff:%slot-bus-read sdu-quad-slot #x80) vinc:%%fixnum-field 0)))
          (not (zerop (hw:ldb (nubus-stuff:%slot-bus-read sdu-quad-slot #x84) (byte 1 9.) 0))))
      )
    )
  )

;; This says that the sharing disk driver is running, so other processors may be active
(defun share-mode-active-p ()
  (select-processor
    (:lambda
      (not (zerop (%nubus-read sdu-quad-slot #x80))))
    (:k
      (not (zerop (hw:dpb (nubus-stuff:%slot-bus-read sdu-quad-slot #x80) vinc:%%fixnum-field 0))))
    )
  )

(defparameter rg-quad-slot #xFD)

(defun get-config-structure-pointer (&aux addr)
  ;; #x820 is the address that contains information about the validity
  ;; of the config structure pointer at address #x821.
  (loop
    (when (hw:32= (array:%vm-read32 #x820 0) (hw:unboxed-constant 1))
      (setq addr (nubus-stuff:%bus-read (array:%vm-read32 #x820 1)))
      (return (values (hw:ldb addr (byte 8. 24.) 0)
                      (hw:ldb addr (byte 24. 0) 0)))
      )
    )
  )

(defun k-slot ()
  (hw:ldb (hw:read-memory-status) hw:%%memory-status-nubus-slot-id #xF0)
  )


;; Copied from LAD: RELEASE-3.SYS; CONFIG.LISP#49 on 3-Oct-86 11:52:39
(defun find-processor-configuration-structure ()
  "Use the system configuration structure to build a model of all the processors on this
bus, and define the relationships between this processor and all the others."
  (setq *my-proc-conf* nil)
  (setq *other-processors* nil)
  (setq rg-quad-slot
        (select-processor
          (:lambda
            (%lambda-rg-quad-slot))
          (:k (k-slot))))
  (setq sdu-quad-slot
        (select-processor
          (:lambda (%lambda-sdu-quad-slot))
          (:k #XFF)
          )
        )
  (setq *sys-conf* (select-processor
                      (:lambda (make-sys-conf-virtual (%lambda-sys-conf-virtual-adr)))
                      (:k
                         (multiple-value-bind (slot offset)
                             (get-config-structure-pointer)
                           (k-make-sys-conf-virtual slot offset)))))
  (when (sys-conf-structure-available-p)
    ;; First collect all of the processors in the world.
    (do ((offset (%system-configuration-size *sys-conf*)
                 (+ offset (%system-configuration-processor-block-size *sys-conf*)))
         (proc-number 0 (1+ proc-number)))
        ((or (>= proc-number (%system-configuration-number-of-processors *sys-conf*))
             (>= (+ offset (%system-configuration-processor-block-size *sys-conf*))
                (select-processor
                  (:lambda (* 2 page-size))
                  (:k vinc:*qs-in-cluster*)))))
      (let ((proc-conf (make-proc-conf-virtual
                         (select-processor
                           (:lambda (+ (%lambda-sys-conf-virtual-adr) offset))
                           (:k
                             (hw:24+ offset (%lambda-sys-conf-virtual-adr))))))
            (op (make-other-processor)))
        (setf (op-proc-conf op) proc-conf)
        (setf (op-proc-number op) proc-number)
        (push op *other-processors*)))

    ;; Now find myself.
    (dolist (op *other-processors*)
      (when (select-processor
              (:lambda
                (and (= (ldb #o0004 rg-quad-slot)
                        (ldb #o0004 (%processor-conf-slot-number (op-proc-conf op))))))
              (:k
                (and (= (hw:ldb rg-quad-slot (byte 4 0) 0)
                        (hw:ldb (%processor-conf-slot-number (op-proc-conf op)) (byte 4 0) 0)))))
        (setq *my-op* op)
        (setq *my-proc-conf* (op-proc-conf op))
        (setq *my-proc-conf* (op-proc-conf op))
        (setq *my-proc-number* (op-proc-number op))
        (setq *other-processors* (delq op *other-processors*))
        (return nil)))

    (when (null *my-proc-conf*)
      (li:error "couldn't find my processor configuration structure"))
;    (config-ferror nil "couldn't find my processor configuration structure"))

    (let ((n-maps (%processor-conf-number-of-multibus-maps *my-proc-conf*)))
      (select-processor
        (:lambda
          (set-max-user-disk-transfer (if (zerop n-maps) #o76 (- n-maps 2))))
        (:k (setq page-rqb-size (if (zerop n-maps) #o76 (- n-maps 2))))
        )
      )

    (set-up-shared-memory)

    (setup-intmaps)

;someday we can turn this on...
    (turn-on-microsecond-clock-if-present)

    ;; Until this is done the first time, the chaos net will leave the share stuff alone
    (setq *share-code-ready* t)

;    (write-meter '%initial-watchdog (* 50. 5))
    ))

;Changing from the fake microsecond clock to the real one makes the value
;of (TIME) jump, but this is OK because we usually
;only do it from find-processor-configuration-structure
;and that is usually only called from lisp-reinitialize.  In that case,
;we will soon do the warm-boot initialization for INITIALIZE-TIMEBASE,
;and it wont matter that we are changing the value of (TIME) by a huge amount.
;
;If you call this function yourself, however, you will confuse the machine,
;and should proabably call (initialize-timebase) yourself.
(defun turn-on-microsecond-clock-if-present ()
  (select-processor
    (:lambda
      (unless (ldb-test %%processor-switch-use-stat2-for-usec-clock (%processor-switches nil))
        (si:%microsecond-clock t)
        (unless (do ((start (time:microsecond-time))
                     (time (time:microsecond-time) (time:microsecond-time))
                     (count 0 (add1 count)))
                    ((or (> time start) (> count 100))
                     (> time start))
                  (time:microsecond-time))
          (%processor-switches
            (dpb 0 %%processor-switch-use-stat2-for-usec-clock
                 (%processor-switches nil))))))
    (:k t)
    ((:explorer :cadr) nil)))

;; Copied from LAD: RELEASE-3.SYS; CONFIG.LISP#49 on 3-Oct-86 11:52:41
(defun turn-off-microsecond-clock ()
  (si:%microsecond-clock nil))

(defun find-paging-partition-name ()
  (when (sys-conf-structure-available-p)
    (select-processor
      (:lambda
        (make-array 4
                    :type :art-string
                    :displaced-to *my-proc-conf*
                    :displaced-index-offset (* 4 %processor-conf-paging-band)))
      (:k (si:make-array 4
                    :type array::art-string
                    :displaced-to *my-proc-conf*
                    :displaced-index-offset (* 4 %processor-conf-paging-band)))
      )
    )
  )

(defun find-file-partition-name ()
  (when (sys-conf-structure-available-p)
    (select-processor
      (:lambda
        (make-array 4
                    :type :art-string
                    :displaced-to *my-proc-conf*
                    :displaced-index-offset (* 4 %processor-conf-file-band))
        )
      (:k
        (si:make-array 4
                    :type array:art-string
                    :displaced-to *my-proc-conf*
                    :displaced-index-offset (* 4 %processor-conf-file-band)))
      )
    )
  )

(defun find-ucode-partition-name ()
  (when (sys-conf-structure-available-p)
    (select-processor
      (:lambda
        (make-array 4
                    :type :art-string
                    :displaced-to *my-proc-conf*
                    :displaced-index-offset (* 4 %processor-conf-micro-band)))
      (:k
        (si:make-array 4
                    :type array:art-string
                    :displaced-to *my-proc-conf*
                    :displaced-index-offset (* 4 %processor-conf-micro-band)))
      )
    )
  )

;;;check for broadcast pkts separately from this
;(defun processor-for-host-if-on-my-nubus (host)
;  (dolist (op si:*other-processors*)
;    (cond ((= host (si:%processor-conf-chaos-address (si:op-proc-conf op)))
;          (return op)))))
;
;(defflavor device-allocation-error () (error))

;(defmethod (device-allocation-error :case :proceed-asking-user :steal-device)
;          (cont read-func)
;  "Steal the device and hope the other processor doesn't try to use it again."
;  read-func
;  (funcall cont :steal-device))

;(defmethod (device-allocation-error :case :proceed-asking-user :try-allocating-again)
;          (cont read-func)
;  "Try to allocate the device again."
;  read-func
;  (funcall cont :try-allocating-again))

;(defmethod (device-allocation-error :case :proceed-asking-user :allocate-anyway)
;          (cont read-func)
;  "Allocate the device even though the configuration file says it's not on the bus."
;  read-func
;  (funcall cont :allocate-anyway))

;(defmethod (device-allocation-error :case :proceed-asking-user :reallocate)
;          (cont read-func)
;  "Reallocate the device."
;  read-func
;  (funcall cont :reallocate))

;(defsignal device-already-allocated device-allocation-error (device slot)
;  "Signaled if an attempt is made to allocate a device that belongs to another processor.")

;(defsignal device-not-on-bus device-allocation-error ()
;  "Signaled if device to be allocated is not on the bus.")

;(compile-flavor-methods device-allocation-error)


(defun set-up-shared-memory ()
  (setq *global-shared-memory-size* (%system-configuration-global-shared-size *sys-conf*))
  (select-processor
    (:lambda
      (setq *global-shared-memory-8*
            (make-array *global-shared-memory-size*
                        :type :art-8b
                        :displaced-to (sdu-phys-to-virtual
                                        (%system-configuration-global-shared-base *sys-conf*))))
      (setq *global-shared-memory-16*
            (make-array (// *global-shared-memory-size* 2)
                        :type :art-16b
                        :displaced-to (sdu-phys-to-virtual
                                        (%system-configuration-global-shared-base *sys-conf*))))
      (setq *global-shared-memory-32*
            (make-array (// *global-shared-memory-size* 4)
                        :type :art-32b
                        :displaced-to (sdu-phys-to-virtual
                                        (%system-configuration-global-shared-base *sys-conf*)))))
    (:k
      (setq *global-shared-memory-8*
            (array:zl-make-array *global-shared-memory-size*
                        :type array:art-8b
                        :displaced-to (sdu-phys-to-virtual
                                        (%system-configuration-global-shared-base *sys-conf*))))
      (setq *global-shared-memory-16*
            (array:zl-make-array (/ *global-shared-memory-size* 2)
                        :type array:art-16b
                        :displaced-to (sdu-phys-to-virtual
                                        (%system-configuration-global-shared-base *sys-conf*))))
      (setq *global-shared-memory-32*
            (array:zl-make-array (/ *global-shared-memory-size* 4)
                        :type array:art-32b
                        :displaced-to (sdu-phys-to-virtual
                                        (%system-configuration-global-shared-base *sys-conf*)))))
    )
  )

(defun share-mem-read (address)
  (select-processor
    (:lambda
      (cond ((> address *global-shared-memory-size*)
             (ferror nil "~s is larger than the size of the shared area" address)))
      (%nubus-read (ldb (byte 8 24.) (%system-configuration-global-shared-base *sys-conf*))
                   (+ (%system-configuration-global-shared-base *sys-conf*)
                      (logand #o77777774 address))))
    (:k
      (cond ((> address *global-shared-memory-size*)
             (li:error "~s is larger than the size of the shared area" address)))
      (nubus-stuff:%bus-read (hw:ldb (%system-configuration-global-shared-base *sys-conf*) (byte 8 24.) 0)
                   (+ (%system-configuration-global-shared-base *sys-conf*)
                      (logand #o77777774 address))))
    )
  )

(defun share-mem-write (address val)
  (select-processor
    (:lambda
      (cond ((> address *global-shared-memory-size*)
             (ferror nil "~s is larger than the size of the shared area" address)))
      (%nubus-write (ldb (byte 8 24.) (%system-configuration-global-shared-base *sys-conf*))
                    (+ (%system-configuration-global-shared-base *sys-conf*)
                       (logand #o77777774 address))
                    val))
    (:k
      (cond ((> address *global-shared-memory-size*)
             (li:error "~s is larger than the size of the shared area" address)))
      (nubus-stuff:%bus-write (hw:ldb (%system-configuration-global-shared-base *sys-conf*) (byte 8 24.) 0)
                    (+ (%system-configuration-global-shared-base *sys-conf*)
                       (logand #o77777774 address))
                    val))
    )
  )

(defsubst read-multibus-mapping-register (page-number)
  (let ((ans 0))
    (select-processor
      (:lambda
        (dotimes (c 3)
          (setq ans (dpb (%multibus-read-8 (+ #16r18000 (* 4 page-number) c))
                         (dpb c #o1102 8.)
                         ans))))
        (:k
          (dotimes (c 3)
            (setq ans (hw:dpb (nubus-stuff:%multibus-byte-read (+ #x18000 (* 4 page-number) c))
                           (hw:dpb c (byte 2 9.) 8.)
                           ans))))
        )
    ans))

(defsubst write-multibus-mapping-register (page-number data)
  (select-processor
    (:lambda
      (dotimes (c 3)
        (%multibus-write-8 (+ #16r18000 (* 4 page-number) c)
                           (ldb (dpb c #o1102 #o10) data))))
    (:k
      (dotimes (c 3)
        (nubus-stuff:%multibus-byte-write (+ #x18000 (* 4 page-number) c)
                           (hw:ldb data (hw:dpb c (byte 2 9.) 8) 0)))
      ))
      data)



;find-processor-configuration-structure is called in lisp-reinitialize
;just before running the system-initialization-list, but we also need
;to run it just after loading
;(add-initialization "first time find processor configuration structure"
;                   '(find-processor-configuration-structure)
;                   '(:once))

;;; temp stuff for stat counters

;(defconst %stat-counter-read-main 0)
;(defconst %stat-counter-write-main 1)
;(defconst %stat-counter-read-aux 2)
;(defconst %stat-counter-write-aux 3)
;(defconst %stat-counter-read-rg-mode 4)
;(defconst %stat-counter-write-control 5)

;(defconst %%aux-stat-count-control (byte 3 0))
;(defconst %%aux-stat-clock-control (byte 1 4))
;(defconst %%main-stat-count-control (byte 3 20.))
;(defconst %%main-stat-clock-control (byte 1 24.))

;(defconst %stat-clock-sm.clock 0)
;(defconst %stat-clock-uinst.clock 1)

;(defconst stat-clock-values '("SM.CLOCK" "UINST.CLOCK"))

;(defconst %stat-count-valid.statistics.bit 0)
;(defconst %stat-count-memory.start.next.cycle 1)
;(defconst %stat-count-csm.statistics.bit 2)
;(defconst %stat-count-increment.lc 3)
;(defconst %stat-count-t.hold 4)
;(defconst %stat-count-t.statistics.bit 5)
;(defconst %stat-count-not.connected 6)
;(defconst %stat-count-hi 7)

;(defconst stat-count-values '("VALID.STATISTICS.BIT"
;                             "MEMORY.START.NEXT.CYCLE"
;                             "CSM.STATISTICS.BIT"
;                             "INCREMENT.LC"
;                             "T.HOLD"
;                             "T.STATISTICS.BIT"
;                             "1.MHZ.CLOCK"     ;unconnected on main stat counter
;                             "HI"))

;(defsubst read-rg-mode ()
;  (logand 37777777777 (%stat-counter %stat-counter-read-rg-mode 0)))

;(defsubst read-main-stat-counter ()
;  (logand 37777777777 (%stat-counter %stat-counter-read-main 0)))

;(defsubst write-main-stat-counter (val)
;  (%stat-counter %stat-counter-write-main val))

;(defsubst write-main-stat-control (clock count)
;  (let ((rg-mode (%stat-counter %stat-counter-read-rg-mode 0)))
;    (setq rg-mode (dpb clock %%main-stat-clock-control rg-mode))
;    (setq rg-mode (dpb count %%main-stat-count-control rg-mode))
;    (%stat-counter %stat-counter-write-control rg-mode)))

;(defsubst read-aux-stat-counter ()
;  (logand 37777777777 (%stat-counter %stat-counter-read-aux 0)))

;(defsubst write-aux-stat-counter (val)
;  (%stat-counter %stat-counter-write-aux val))

;(defsubst write-aux-stat-control (clock count)
;  (let ((rg-mode (%stat-counter %stat-counter-read-rg-mode 0)))
;    (setq rg-mode (dpb clock %%aux-stat-clock-control rg-mode))
;    (setq rg-mode (dpb count %%aux-stat-count-control rg-mode))
;    (%stat-counter %stat-counter-write-control rg-mode)))

;(defun describe-stat-counters ()
;  (let (rg-mode main aux)
;    (without-interrupts
;      (setq rg-mode (%stat-counter %stat-counter-read-rg-mode 0))
;      (setq main (read-main-stat-counter))
;      (setq aux (read-aux-stat-counter)))
;    (format t "~&Main counts ~25a on ~12a and currently is ~15:d."
;           (nth (ldb %%main-stat-count-control rg-mode) stat-count-values)
;           (nth (ldb %%main-stat-clock-control rg-mode) stat-clock-values)
;           main)
;    (format t "~&Aux  counts ~25a on ~12a and currently is ~15:d."
;           (nth (ldb %%aux-stat-count-control rg-mode) stat-count-values)
;           (nth (ldb %%aux-stat-clock-control rg-mode) stat-clock-values)
;           aux)
;    (format t "~2&")
;    (format t "~&~15:d MAIN - AUX" (- main aux))
;    (format t "~&~15d MAIN // AUX" (// (float main) aux))
;    (format t "~&~15d AUX // MAIN" (// (float aux) main))
;    ))

;(defun reset-stat-counters ()
;  (without-interrupts
;    (write-main-stat-counter 0)
;    (write-aux-stat-counter 0)))

;(defun stat-cache-hits-vs-memory-cycles ()
;  (write-main-stat-control %stat-clock-sm.clock %stat-count-csm.statistics.bit)
;  (write-aux-stat-control %stat-clock-sm.clock %stat-count-memory.start.next.cycle)
;  (reset-stat-counters)
;  )
;; Copied from LAD: RELEASE-3.SYS; CONFIG.LISP#49 on 3-Oct-86 11:52:43.  updated 10/12/86 --rg
(defun check-memory-size-compatibility-with-physical-page-data ()
  (select-processor
    (:lambda
      (let ((sys-com-thinks (* 4 (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)))
            (proc-conf-thinks (+ (%processor-conf-memory-bytes-0 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-1 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-2 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-3 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-4 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-5 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-6 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-7 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-8 *my-proc-conf*)
                                 (%processor-conf-memory-bytes-9 *my-proc-conf*))))
        (when (//= proc-conf-thinks sys-com-thinks)
          (tv:notify nil "The amount of memory allocated to this processor by CONFIG, ~d bytes, is
greater than the amount of memory it is capable of using, ~d bytes.
You should adjust this the next time you run CONFIG so that you will not be wasting
~d bytes of memory."
                     proc-conf-thinks sys-com-thinks
                     (- proc-conf-thinks sys-com-thinks)))))
    (:k
;      (let ((sys-com-thinks (* 4 (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)))    ;
;           (proc-conf-thinks (+ (%processor-conf-memory-bytes-0 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-1 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-2 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-3 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-4 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-5 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-6 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-7 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-8 *my-proc-conf*)
;                                (%processor-conf-memory-bytes-9 *my-proc-conf*))))
;       (when (//= proc-conf-thinks sys-com-thinks)
;         (tv:notify nil "The amount of memory allocated to this processor by CONFIG, ~d bytes, is
;greater than the amount of memory it is capable of using, ~d bytes.
;You should adjust this the next time you run CONFIG so that you will not be wasting
;~d bytes of memory."
;                    proc-conf-thinks sys-com-thinks
;                    (- proc-conf-thinks sys-com-thinks))))
            )
    ((:CADR :EXPLORER))))

;; Copied from LAD: RELEASE-3.SYS; CONFIG.LISP#49 on 3-Oct-86 11:52:43
;(add-initialization "Check Physical Memory Size"
;                   '(check-memory-size-compatibility-with-physical-page-data)
;                   '(:cold :now))
