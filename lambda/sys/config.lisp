;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Lowercase:T; Base:8; Readtable:ZL -*-

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

;; This says it's legal to use the sytem configuration structure
(defun sys-conf-structure-available-p ()
  (and (= processor-type-code lambda-type-code)
       (or (not (zerop (%nubus-read sdu-quad-slot #x80)))
           (not (zerop (ldb (BYTE 1. 9.) (%nubus-read sdu-quad-slot #x84)))))))

;; This says that the sharing disk driver is running, so other processors may be active
(defun share-mode-active-p ()
  (and (= processor-type-code lambda-type-code)
       (not (zerop (%nubus-read sdu-quad-slot #x80)))))

;; Copied from LAD: RELEASE-3.SYS; CONFIG.LISP#49 on 3-Oct-86 11:52:39
(defun find-processor-configuration-structure ()
  "Use the system configuration structure to build a model of all the processors on this
bus, and define the relationships between this processor and all the others."
  (setq *my-proc-conf* nil)
  (setq *other-processors* nil)
  (setq rg-quad-slot (%lambda-rg-quad-slot))
  (setq sdu-quad-slot (%lambda-sdu-quad-slot))
  (setq *sys-conf* (make-sys-conf-virtual (%lambda-sys-conf-virtual-adr)))
  (when (sys-conf-structure-available-p)
    ;; First collect all of the processors in the world.
    (do ((offset (%system-configuration-size *sys-conf*)
                 (+ offset (%system-configuration-processor-block-size *sys-conf*)))
         (proc-number 0 (1+ proc-number)))
        ((or ( proc-number (%system-configuration-number-of-processors *sys-conf*))
             ( (+ offset (%system-configuration-processor-block-size *sys-conf*))
                (* 2 page-size))))  ;currently, sys-conf plus all proc-confs must fit in two
                                    ;pages -- rg 12/08/85.
      (let ((proc-conf (make-proc-conf-virtual
                         (+ (%lambda-sys-conf-virtual-adr) offset)))
            (op (make-other-processor)))
        (setf (op-proc-conf op) proc-conf)
        (setf (op-proc-number op) proc-number)
        (push op *other-processors*)))

    ;; Now find myself.
    (dolist (op *other-processors*)
      (when (and (= (ldb (BYTE 4. 0.) rg-quad-slot)
                    (ldb (BYTE 4. 0.) (%processor-conf-slot-number (op-proc-conf op)))))
        (setq *my-op* op)
        (setq *my-proc-conf* (op-proc-conf op))
        (setq *my-proc-conf* (op-proc-conf op))
        (setq *my-proc-number* (op-proc-number op))
        (setq *other-processors* (delq op *other-processors*))
        (return)))

    (when (null *my-proc-conf*)
      (config-ferror nil "couldn't find my processor configuration structure"))

    (let ((n-maps (%processor-conf-number-of-multibus-maps *my-proc-conf*)))
      (set-max-user-disk-transfer (if (zerop n-maps) 76 (- n-maps 2))))

    (set-up-shared-memory)

    (setup-intmaps)

;someday we can turn this on...
;    (turn-on-microsecond-clock-if-present)

    ;; Until this is done the first time, the chaos net will leave the share stuff alone
    (setq *share-code-ready* t)

    (write-meter '%initial-watchdog (* 50. 5))
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
    ((:explorer :cadr) nil)))

;; Copied from LAD: RELEASE-3.SYS; CONFIG.LISP#49 on 3-Oct-86 11:52:41
(defun turn-off-microsecond-clock ()
  (si:%microsecond-clock nil))

(defun find-paging-partition-name ()
  (when (sys-conf-structure-available-p)
    (make-array 4
                :type :art-string
                :displaced-to *my-proc-conf*
                :displaced-index-offset (* 4 %processor-conf-paging-band))))

(defun find-file-partition-name ()
  (when (sys-conf-structure-available-p)
    (make-array 4
                :type :art-string
                :displaced-to *my-proc-conf*
                :displaced-index-offset (* 4 %processor-conf-file-band))))

(defun find-ucode-partition-name ()
  (when (sys-conf-structure-available-p)
    (make-array 4
                :type :art-string
                :displaced-to *my-proc-conf*
                :displaced-index-offset (* 4 %processor-conf-micro-band))))

;;;check for broadcast pkts separately from this
(defun processor-for-host-if-on-my-nubus (host)
  (dolist (op si:*other-processors*)
    (cond ((= host (si:%processor-conf-chaos-address (si:op-proc-conf op)))
           (return op)))))

(defflavor device-allocation-error () (error))

(defmethod (device-allocation-error :case :proceed-asking-user :steal-device)
           (cont read-func)
  "Steal the device and hope the other processor doesn't try to use it again."
  read-func
  (funcall cont :steal-device))

(defmethod (device-allocation-error :case :proceed-asking-user :try-allocating-again)
           (cont read-func)
  "Try to allocate the device again."
  read-func
  (funcall cont :try-allocating-again))

(defmethod (device-allocation-error :case :proceed-asking-user :allocate-anyway)
           (cont read-func)
  "Allocate the device even though the configuration file says it's not on the bus."
  read-func
  (funcall cont :allocate-anyway))

(defmethod (device-allocation-error :case :proceed-asking-user :reallocate)
           (cont read-func)
  "Reallocate the device."
  read-func
  (funcall cont :reallocate))

(defsignal device-already-allocated device-allocation-error (device slot)
  "Signaled if an attempt is made to allocate a device that belongs to another processor.")

(defsignal device-not-on-bus device-allocation-error ()
  "Signaled if device to be allocated is not on the bus.")

(compile-flavor-methods device-allocation-error)


(defun set-up-shared-memory ()
  (setq *global-shared-memory-size* (%system-configuration-global-shared-size *sys-conf*))
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

(defun share-mem-read (address)
  (cond ((> address *global-shared-memory-size*)
         (ferror nil "~s is larger than the size of the shared area" address)))
  (%nubus-read (ldb (byte 8 24.) (%system-configuration-global-shared-base *sys-conf*))
               (+ (%system-configuration-global-shared-base *sys-conf*)
                  (logand 77777774 address))))

(defun share-mem-write (address val)
  (cond ((> address *global-shared-memory-size*)
         (ferror nil "~s is larger than the size of the shared area" address)))
  (%nubus-write (ldb (byte 8 24.) (%system-configuration-global-shared-base *sys-conf*))
                (+ (%system-configuration-global-shared-base *sys-conf*)
                   (logand 77777774 address))
                val))

(defsubst read-multibus-mapping-register (page-number)
  (let ((ans 0))
    (dotimes (c 3)
      (setq ans (dpb (%multibus-read-8 (+ #16r18000 (* 4 page-number) c))
                     (dpb c (BYTE 2. 9.) 10)
                     ans)))
    ans))

(defsubst write-multibus-mapping-register (page-number data)
  (dotimes (c 3)
    (%multibus-write-8 (+ #16r18000 (* 4 page-number) c)
                       (ldb (dpb c (BYTE 2. 9.) 10) data)))
  data)



;find-processor-configuration-structure is called in lisp-reinitialize
;just before running the system-initialization-list, but we also need
;to run it just after loading
(add-initialization "first time find processor configuration structure"
                    '(find-processor-configuration-structure)
                    '(:once))

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
    ((:CADR :EXPLORER))))

;; Copied from LAD: RELEASE-3.SYS; CONFIG.LISP#49 on 3-Oct-86 11:52:43
(add-initialization "Check Physical Memory Size"
                    '(check-memory-size-compatibility-with-physical-page-data)
                    '(:cold :now))
