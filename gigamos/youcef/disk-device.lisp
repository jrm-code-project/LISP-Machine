;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;


(defstruct (disk-paging-device
             (:include paging-device)
             (:conc-name "MPD-")
             (:constructor make-mpd-device))
           state
           quantum-bitmap
           (accumulated-physical-cluster-swap-list (make-array 16.))
           starting-address
           number-of-quanta
           dqin-for-this-operation
           this-cluster
           cluster-count
  )

(defparameter *size-in-quantums* nil)

(defun disk-device-open (device)
  (unless (eq (mpd-state device) :closed)
    (li:error "Device ~S is already open." device))
  (setf (mpd-quantum-bitmap device) (array:make-1d-array *size-in-quantums* array:art-1b))
  ;; init quantum-bitmap device.
  (let ((quantum-map (mpd-quantum-bitmap device)))
    (dotimes (i *size-in-quantums*)
      (setf (svref quantum-map i) 0)))
  (setf (mpd-starting-address device) 0)
  (setf (mpd-state device) :OPEN)
  )

(defun disk-device-close (device)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (setf (mpd-state device) :CLOSED)
  )

(defun disk-device-allocate-quantum (device)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (let ((quantum-map (mpd-quantum-bitmap device)))
    (dotimes (i (mpd-number-of-quanta device) (li:error "No more quanta left on Paging Device ~s" device))
      (when (zerop (svref quantum-map i))
        (setf (svref quantum-map i) 1)
        (return i)))
    )
  )

(defun disk-device-deallocate-quantum (device device-quantum-id)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (unless (> (mpd-number-of-quanta device) device-quantum-id)
    (li:error "Quantum ~D does not exists for device ~S" device-quantum-id device))
  (when (zerop (svref (mpd-quatum-bitmap device) device-quantum-id))
    (li:error "Quantum ~D was not allocated in device ~S" device-quantum-id device))
  (setf (svref (mpd-quantum-bitmap device) device-quantum-id) 0)
  )

(defun disk-device-start-write-cluster (device dqin cluster-in-quantum)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (setf (mpd-dqin-for-this-operation device) dqin)
  (setf (mpd-this-cluster device) cluster-in-quantum)
  (setf (mpd-state device) :Write-started)
  )

(defun disk-device-start-read-cluster (device dqin cluster-in-quantum)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (setf (mpd-dqin-for-this-operation device) dqin)
  (setf (mpd-this-cluster device) cluster-in-quantum)
  (setf (mpd-state device) :read-started)
  )


(defun disk-device-next-cluster-to-read (device physical-cluster-number)
  (unless (eq (mpd-state device) :read-started)
    (error "A read operation has not been started for device ~S" device))
  (setf (aref (mpd-accumulated-physical-cluster-swap-list device) (mpd-cluster-count device))
        physical-cluster-number)
  (incf (mpd-cluster-count device))
  )

(defun disk-device-next-cluster-to-write (device physical-cluster-number)
  (unless (eq (mpd-state device) :write-started)
    (error "A write operation has not been started for device ~S" device))
  (setf (aref (mpd-accumulated-physical-cluster-swap-list device) (mpd-cluster-count device))
        physical-cluster-number)
  (incf (mpd-cluster-count device))
  )

(defun k-mem-slot ()
  (hw:ldb (hw:read-memory-status) hw:%%memory-status-nubus-slot-id #xF0)
  )

(defun disk-device-activate-read (device)
;  (let ((device-mem-byte-address (hw:32+
;                                  (hw:dpb-unboxed (mpd-dqin-for-this-operation device)
;                                                  (byte 8. 16.) (hw:unboxed-constant 0))
;                                  (hw:dpb-unboxed *mem-slot* (byte 8. 24.) (mpd-starting-address device))))
;       (physical-cluster-vector (mpd-accumulated-physical-cluster-swap-list device))
;       (cluster-size 1024.))
;    (do ((count 0 (1+ count))
;        physical-byte-address)
;       ((= count (mpd-cluster-count device)))
;      (setq physical-byte-address (hw:dpb-unboxed (svref physical-cluster-vector count)
;                                                 (byte 16. 12.) (hw:unboxed-constant 0)))
;      (when (>= (+ count (mpd-this-cluster device)) 16.)
;       (li:error "Cluster ran off end of quantum during mem disk read"))
;      (setq device-mem-byte-address (hw:32+
;                                     (hw:dpb-unboxed (+ count (mpd-this-cluster device))
;                                                   (byte 4. 12.) (hw:unboxed-constant 0))
;                                     device-mem-byte-address))
;      (nubus-stuff:%cluster-copy device-mem-byte-address nil physical-byte-address t))
    (operation-complete device)
;    )
  )

(defun disk-device-activate-write (device)
;  (let ((device-mem-byte-address (hw:32+
;                                  (hw:dpb-unboxed (mpd-dqin-for-this-operation device)
;                                                  (byte 8. 16.) (hw:unboxed-constant 0))
;                                  (hw:dpb-unboxed *mem-slot* (byte 8. 24.) (mpd-starting-address device))))
;       (physical-cluster-vector (mpd-accumulated-physical-cluster-swap-list device))
;       (cluster-size 1024.))
;    (do ((count 0 (1+ count))
;        physical-byte-address)
;       ((= count (mpd-cluster-count device)))
;      (setq physical-byte-address (hw:dpb-unboxed (svref physical-cluster-vector count)
;                                                 (byte 16. 12.) (hw:unboxed-constant 0)))
;      (when (>= (+ count (mpd-this-cluster device)) 16.)
;       (li:error "Cluster ran off end of quantum during mem disk write"))
;      (setq device-mem-byte-address (hw:32+
;                                     (hw:dpb-unboxed (+ count (mpd-this-cluster device))
;                                                   (byte 4. 12.) (hw:unboxed-constant 0))
;                                     device-mem-byte-address))
;      (nubus-stuff:%cluster-copy physical-byte-address t device-mem-byte-address nil))
    (operation-complete device)
;    )
  )

(defun disk-device-operation-complete (device)
  (setf (mpd-dqin-for-this-operation device) nil)
  (setf (mpd-this-cluster device) nil)
  (setf (mpd-cluster-count device) 0)
  (setf (mpd-state device) :open)
  t
  )

(defun make-disk-device-device ()
  (make-mpd-device
    :state :closed
    :cluster-count 0
    :dqin-for-this-operation nil
    :this-cluster nil
    :number-of-quanta 255.
    :opener 'disk-device-open
    :closer 'disk-device-close
    :quantum-allocator 'disk-device-allocate-quantum
    :quantum-deallocator 'disk-device-deallocate-quantum
    :reader-initializer 'disk-device-start-read-cluster
    :writer-initializer 'disk-device-start-write-cluster
    :next-cluster-reader 'disk-device-next-cluster-to-read
    :next-cluster-writer 'disk-device-next-cluster-to-write
    :reader-activate 'disk-device-activate-read
    :writer-activate  'disk-device-activate-write
    :operation-completer 'disk-device-operation-complete
    )
  )
