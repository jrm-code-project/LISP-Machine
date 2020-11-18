;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Lowercase:T; Base:8; Readtable:ZL -*-


;;; >>>> NOT FOR ETHERNET

;;; user callable methods

;;;  :owner             NIL - free
;;;                     :NOT-ON-BUS
;;;                     0..31. slot number who owns the device
;;;
;;;  :quad-slot
;;;                     for a NUBUS device, the quad slot the device is in, else NIL
;;;
;;;  :device-still-owned-by-me-p
;;;                     T if I own the device
;;;                     NIL if it's either :NOT-ON-BUS, free, or owned by someone else
;;;                     This is supposed to be fast enough to be included in the main
;;;                     loop that uses a device, e.g. every tape record.
;;;
;;;  :error-if-i-dont-own-device
;;;                     If :device-still-owned-by-me-p would return NIL, this signals
;;;                     the appropriate error.
;;;                     This is also supposed to be fast.
;;;
;;;  :allocate-if-easy
;;;                     Allocates the device and returns T if it is currently free.
;;;                     Returns NIL if the device is not on the bus, or is owned by someone else.
;;;
;;;  :allocate
;;;                     Allocates the device and returns T if it is currently free.
;;;                     Signals the appropriate error if not.
;;;
;;;  :deallocate
;;;                     If I own the device, free it, otherwise, just return

;;; to make a new shared device
;;;
;;; in a :BEFORE :INIT deamon, you have to set sys-conf-owner-index and sys-conf-slot-index
;;; if you want, you can add deamons, or replace the following methods
;;;
;;;  :after-allocation
;;;                     gets called whenever this machine allocates the device, and
;;;                     has reason to believe that it did not own it before.
;;;
;;;  :after-deallocation
;;;                     gets called whenever this machine frees the device after having
;;;                     owned it for a while
;;;
;;;  :warm-boot
;;;                     gets sent to each instance of basic-shared-device
;;;                     during the warm-initialization-list
;;;                     you can add a deamon to this, but don't replace the primay method
;;;
;;;  :close
;;;                     called when device is closed.  the default is to just send
;;;                     :deallocate to self, but you can replace it
;;;
;;;  :close-for-process
;;;                     called with and argument PROCESS, this is like close
;;;                     except that it uses the specified process to to check against
;;;                     the value of the lock's process so that a process can free a lock
;;;                     not belonging to it. (i.e. the system menu process during a window's
;;;                     :BEFORE :KILL method).
;;;

(defflavor basic-shared-device
           ((sys-conf-owner-index nil)
            (owner-virtual-address nil)
            (sys-conf-slot-index nil)
            ;; i owned-it-last-time meanings (name of variable is poor choice).
            ;;  NIL = cold booted state, device may be owned in SYS-CONF, maybe initialized itself
            ;;        (maybe not, so you must assume that it is not), but certainly in-virtual-memory
            ;;        structures have not been set up.
            ;;  T   = device owned and initialized.
            ;; :WARM = there has been a warm boot, which some initializations are undone by the SDU
            ;;         when this happens, such as interrupt forwarding. Device should probably be
            ;;         reinitialized.
            (i-owned-it-last-time nil)
            lock
            (last-instance nil)
            (default-flavor-and-init-options nil)
            (name nil)
            )
           ()
  :gettable-instance-variables
  :settable-instance-variables
  (:method-combination (:case :set))
  )


(defvar all-shared-devices nil)

(defmethod (basic-shared-device :before :init) (ignore)
  (cond ((and (null sys-conf-owner-index)
              (null owner-virtual-address))
         (ferror nil "you must initialize sys-conf-owner-index when making the instance")))
  (push self all-shared-devices)
  (setq lock (%make-pointer dtp-locative (list nil)))
  )

;; Copied from LAD: RELEASE-3.SYS; SHARED-DEVICE.LISP#47 on 2-Oct-86 04:42:45
(defmethod (basic-shared-device :print-self) (stream print-depth slashify-p)
  print-depth
  (cond ((null slashify-p)
         (send stream :string-out name))
        (t
         (format stream "#~s ~s ~s"
                 (type-of self)
                 name
                 (if (sys-conf-structure-available-p)
                     (selectq (send self :owner)
                       (:not-on-bus "not on bus")
                       (nil "free")
                       (t (format nil "allocated by slot ~d." (send self :owner))))
                   "Not accessible")))))

(defmethod (basic-shared-device :read-instance) (ignore stream)
  (let ((nam (read stream)))
    (read stream)
    (fs:get-pathname-host nam)))

(defmethod (basic-shared-device :sys-conf-owner) (&aux lo hi)
  (cond ((null owner-virtual-address)
         (setq lo (aref *sys-conf* (* 2 sys-conf-owner-index)))
         (setq hi (aref *sys-conf* (1+ (* 2 sys-conf-owner-index)))))
        (t
         (setq lo (%p-ldb (byte 16. 0) owner-virtual-address))
         (setq hi (%p-ldb (byte 16. 16.) owner-virtual-address))))
  (cond ((and (= lo 177777) (= hi 177777))
         -1)
        (t
         (dpb hi 2020 lo))))


;remember to set the high bit if you are trying to say it is owned by a slot
(defmethod (basic-shared-device :set-sys-conf-owner) (new-owner)
  (cond ((null owner-virtual-address)
         (aset (ldb 0020 new-owner) *sys-conf* (* 2 sys-conf-owner-index))
         (aset (ldb 2020 new-owner) *sys-conf* (1+ (* 2 sys-conf-owner-index))))
        (t
         (%p-dpb (ldb 0020 new-owner) 0020 owner-virtual-address)
         (%p-dpb (ldb 2020 new-owner) 2020 owner-virtual-address)))
  new-owner)

;setf seems to generate forms that can use this
(defmethod (basic-shared-device :case :set :sys-conf-owner) (new-owner)
  (send self :set-sys-conf-owner new-owner))

(defmethod (basic-shared-device :quad-slot) ()
  (if sys-conf-slot-index
      (get-quad-slot (aref *sys-conf* (* 2 sys-conf-slot-index)))
    nil))

(defmethod (basic-shared-device :after-allocation) ignore)

(defmethod (basic-shared-device :after-deallocation) ignore)

(defmethod (basic-shared-device :warm-boot) ()
  (setq i-owned-it-last-time :warm))

(defmethod (basic-shared-device :before-cold-boot) ()
  (setq i-owned-it-last-time nil))

(defun shared-device-warm-boot ()
  (dolist (x all-shared-devices)
    (send x :warm-boot)))

(defun shared-device-before-cold-boot ()
  (dolist (x all-shared-devices)
    (send x :before-cold-boot)))

(add-initialization "shared device warm boot" '(shared-device-warm-boot) '(warm))
(add-initialization "shared device before cold boot" '(shared-device-before-cold-boot) '(:before-cold))

(defun get-quad-slot (n)
  (cond ((zerop (logand #x80 n))
         (ldb (byte 8 0) (logxor #xf0 n)))
        (t
         (ldb (byte 8 0) n))))

(defun get-slot-index (n)
  (cond ((zerop (logand #x80 n))
         (ldb (byte 8 0) n))
        (t
         (ldb (byte 8 0) (logxor #xf0 n)))))

(defmethod (basic-shared-device :owner) ()
  "Returns either :NOT-ON-BUS, NIL if no one owns it, or a slot number (between 0..31.)."
  (if (sys-conf-structure-available-p)
      (let ((owner (send self :sys-conf-owner)))
        (cond ((eq owner -1)
               :not-on-bus)
              ((ldb-test (byte 1 31.) owner)
               (get-slot-index owner))
              (t nil)))
    :not-on-bus))

(defmethod (basic-shared-device :device-still-owned-by-me-p) ()
  (let ((owner (send self :owner)))
    (and (numberp owner)
         (= owner (get-slot-index rg-quad-slot)))))

(defmethod (basic-shared-device :error-if-i-dont-own-device) ()
  (cond ((null (send self :device-still-owned-by-me-p))
         (cond ((eq (send self :owner) :not-on-bus)
                (signal-proceed-case
                  (() 'device-not-on-bus
                      "The device ~a disappeared from the bus!"
                      self)
                  (:reallocate
                   (send self :allocate-if-easy t))))
               (t
                (signal-proceed-case
                  (() 'device-already-allocated
                      "The ~a device was stolen by slot ~d."
                      self
                      (send self :owner))
                  (:reallocate
                   (send self :allocate-if-easy t))))))))


(defmethod (basic-shared-device :can-allocate-p) ()
  (let ((owner (send self :owner)))
    (or (null owner)
        (eq owner (get-slot-index rg-quad-slot)))))

(defmethod (basic-shared-device :allocate-if-easy) (&optional steal-it)
  (let ((slot (send self :owner)))
    (cond ((null slot)
           (send self :set-sys-conf-owner
                 (dpb 1 (byte 1 31.) (get-slot-index rg-quad-slot)))
           (send self :after-allocation)
           (setq i-owned-it-last-time t))
          ((eq slot (get-slot-index rg-quad-slot))
           (if (memq i-owned-it-last-time '(nil :warm))
               (send self :after-allocation))
           (setq i-owned-it-last-time t))
          (steal-it
           (send self :set-sys-conf-owner
                 (dpb 1 (byte 1 31.) (get-slot-index rg-quad-slot)))
           (send self :after-allocation)
           (setq i-owned-it-last-time t)))))

(defmethod (basic-shared-device :allocate) (&optional steal-it)
  (prog ()
     again
        (cond ((null (send self :allocate-if-easy steal-it))
               (cond ((eq (send self :owner) :not-on-bus)
                      (signal-proceed-case
                        (() 'device-not-on-bus
                            "Device ~a is not on the bus"
                            self)
                        (:allocate-anyway
                         (send self :allocate-if-easy t))))
                     (t
                      (signal-proceed-case
                        (() 'device-already-allocated
                            "Device ~a is already allocated by the processor in slot ~d."
                            self
                            (send self :owner))
                        (:steal-device
                         (send self :allocate-if-easy t))
                        (:try-allocating-again
                         (go again)))))))))

(defmethod (basic-shared-device :deallocate) ()
  (setq i-owned-it-last-time nil)
  (cond ((send self :device-still-owned-by-me-p)
         (send self :after-deallocation)
         (send self :set-sys-conf-owner 0))))

(defmethod (basic-shared-device :close-for-process) (abortp for-process)
  abortp
  (send self :deallocate)
  (send self :free-lock for-process))

(defmethod (basic-shared-device :close) (&optional abortp)
  abortp
  (send self :deallocate)
  (send self :free-lock))

(defmethod (basic-shared-device :get-lock) (&optional (wait-if-necessary t))
  (cond ((eq (car lock) current-process)
         t)
        ((%store-conditional lock nil current-process)
         t)
        (wait-if-necessary
         (process-lock lock nil (format nil "~a locked" self))
         (rplaca lock current-process)          ;in case someone returns from process-lock
         t)
        (t
         nil)))

(defmethod (basic-shared-device :steal-lock) ()
  (rplaca lock current-process))

(defmethod (basic-shared-device :free-lock) (&optional (for-process current-process))
  (cond ((null (car lock)))
        ((null (%store-conditional lock for-process nil))
         (ferror nil "free-lock called while I don't own the lock"))))


;----

(defflavor shared-device
           (pathname-flavor
            (system-type 'shared-device))
           (basic-shared-device si:basic-host si:property-list-mixin)
;required init option :pathname-flavor
  (:gettable-instance-variables pathname-flavor system-type)
  (:initable-instance-variables pathname-flavor system-type)
  )

(defmethod (shared-device :pathname-host-namep) (test-name)
  (string-equal name test-name))

(defmethod (shared-device :open) (flavor-and-init-options shared-device-pathname)
  (cond ((null flavor-and-init-options)
         (setq flavor-and-init-options default-flavor-and-init-options)))
  (let ((flavor (car flavor-and-init-options))
        (init-options (cdr flavor-and-init-options)))
    (setf (getf init-options :shared-device) shared-device-pathname)
    (send self :allocate)
    (send self :get-lock)
    (cond ((null flavor-and-init-options)
           shared-device-pathname)
          ((eq (car flavor-and-init-options) (type-of last-instance))
           last-instance)
          (t
           (setq last-instance (apply 'make-instance flavor init-options))))))

(defflavor shared-device-pathname
           ()
           (fs:pathname)
  )

(defmethod (shared-device-pathname :string-for-printing) ()
  (string-append (send (send self :host) :name) ":")
  )

(defmethod (shared-device-pathname :parse-namestring) (ignore namestring &optional (start 0) end)
  (declare (values (device directory name type version parse-end)))
  (values :unspecific
          :unspecific
          (string-upcase (substring namestring start end))
          :unspecific
          :unspecific)
  )

(defmethod (shared-device-pathname :homedir) (&optional ignore)
  self)

(defmethod (shared-device-pathname :open) (filename &key &optional
                                           flavor-and-init-options
                                           &allow-other-keys)
  (send (send filename :host) :open flavor-and-init-options self))

(defmethod (shared-device-pathname :close) (&optional abortp)
  (send (send self :host) :close abortp)
  )

(compile-flavor-methods shared-device-pathname)

(defun add-shared-device (&key &optional
                          (shared-device-flavor 'shared-device)
                          name
                          sys-conf-owner-index
                          owner-virtual-address
                          sys-conf-slot-index
                          property-list
                          (pathname-flavor 'shared-device-pathname)
                          default-flavor-and-init-options)
  (setq fs:*pathname-host-list*
        (del #'(lambda (x y) (string-equal x (send y :name))) name fs:*pathname-host-list*))
  (push (make-instance shared-device-flavor
                       :name name
                       :sys-conf-owner-index sys-conf-owner-index
                       :owner-virtual-address owner-virtual-address
                       :sys-conf-slot-index sys-conf-slot-index
                       :pathname-flavor pathname-flavor
                       :default-flavor-and-init-options default-flavor-and-init-options
                       :property-list property-list
                       )
        fs:*pathname-host-list*))

;-----

(defflavor sdu-serial-b-shared-device
           ()
           (shared-device)
  )

(defvar *sdu-serial-stream-interrupt-alist*
        '(("SDU-SERIAL-B" %SDU-PORT-B-RCV %SDU-PORT-B-XMIT)
          ("SDU-SERIAL-A" %SDU-PORT-A-RCV %SDU-PORT-A-XMIT)))

(defmethod (sdu-serial-b-shared-device :after-allocation) ()
  (DOLIST (I (CDR (OR (ASS #'STRING-EQUAL (SEND SELF :NAME) *sdu-serial-stream-interrupt-alist*)
                      (FERROR NIL "internal error, no interrupts known for this device"))))
    (forward-sdu-interrupt-to-lambda (EVAL I))))

(defmethod (sdu-serial-b-shared-device :before :open) (flavor-and-init-options shared-device-pathname)
  (let ((flavor (car flavor-and-init-options))
        (init-options (cdr flavor-and-init-options)))
    (when init-options
      (setf (getf init-options :shared-device) shared-device-pathname)
      (setq last-instance (apply 'make-instance flavor init-options)))))

(defmethod (sdu-serial-b-shared-device :after :open) (&rest ignore)
  (send last-instance :reset))

(defmethod (sdu-serial-b-shared-device :close) (&optional ignore)
  (send self :free-lock)
  (send self :deallocate)
  )

(defmethod (sdu-serial-b-shared-device :close-for-process) (ignore for-process)
  (send self :free-lock for-process)
  (send self :deallocate)
  )

(compile-flavor-methods sdu-serial-b-shared-device)

(add-shared-device :name "SDU-SERIAL-B"
                   :shared-device-flavor 'sdu-serial-b-shared-device
                   :sys-conf-owner-index %system-configuration-sdu-serial-b-owner
                   :default-flavor-and-init-options '(sdu-serial-stream))


(add-shared-device :name "SDU-SERIAL-A"
                   :shared-device-flavor 'sdu-serial-b-shared-device
                   :sys-conf-owner-index %system-configuration-sdu-serial-a-owner
                   :default-flavor-and-init-options '(sdu-serial-stream))

;-----

(defflavor half-inch-tape-shared-device
           ()
           (shared-device)
  )

(defmethod (half-inch-tape-shared-device :after-allocation) ()
  ;(fs:tm-init-internal)
  )

(compile-flavor-methods half-inch-tape-shared-device)

(add-shared-device :name "HALF-INCH-TAPE"
                   :shared-device-flavor 'half-inch-tape-shared-device
                   :sys-conf-owner-index %system-configuration-tapemaster-owner
                   )

;-----

(defflavor medium-resolution-color-shared-device
           ()
           (shared-device))

(defmethod (medium-resolution-color-shared-device :after-allocation) ()
  (set (intern "GREY-PROM-PLIST" "GREY") nil)
  (funcall (intern "DOWNLOAD-GREY-BOARD" "GREY")))

#||

(defmethod (medium-resolution-color-shared-device :warm-boot) ()
  ())

(undefmethod (medium-resolution-color-shared-device :warm-boot))

||#

(compile-flavor-methods medium-resolution-color-shared-device)

(add-shared-device :name "MEDIUM-RESOLUTION-COLOR"
                   :shared-device-flavor 'medium-resolution-color-shared-device
                   :sys-conf-owner-index %system-configuration-grey-owner
                   :sys-conf-slot-index %system-configuration-grey-slot
                   )

;-----

(defflavor excelan-network-interface-shared-device
           ()
           (shared-device))

(compile-flavor-methods excelan-network-interface-shared-device)

(add-shared-device :name "EXCELAN-NETWORK-INTERFACE"
                   :shared-device-flavor 'excelan-network-interface-shared-device
                   :sys-conf-owner-index %system-configuration-excelan-owner)
