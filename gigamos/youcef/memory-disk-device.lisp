;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;


(defstruct (memory-disk-paging-device
             (:include paging-device)
             (:conc-name "MPD-")
             (:constructor make-mpd-device))
           state
           mem-slot
           quantum-bitmap
           (accumulated-physical-cluster-swap-list (make-array 16.))
           starting-address
           number-of-quanta
           dqin-for-this-operation
           this-cluster
           cluster-count
  )

(defparameter *mem-slot* #Xfa)

(defun initialize-memory-disk (&optional (mem-slot *mem-slot*))
  (let ((device-addr (hw:dpb mem-slot (byte 8. 24) (hw:unboxed-constant 0))))
    (nubus-stuff:%bus-write (hw:32+ (hw:unboxed-constant #xfff7fc) device-addr) (hw:unboxed-constant 0))
    (dotimes (i #xff0)
      (nubus-stuff:%cluster-fill (hw:dpb-unboxed i (byte 12. 12.) device-addr)
                                 nil (hw:unboxed-constant 0)))
    (nubus-stuff:%bus-write (hw:32+ (hw:unboxed-constant #xfff7fc) device-addr) (hw:unboxed-constant 0))
    nil))
;       (data (hw:unboxed-constant 0))
;       (inc (hw:unboxed-constant 4))
;       (limit (hw:unboxed-constant #xffefff)))
;    ;; zero out control register.
;    (nubus-stuff:%bus-write (hw:32+ (hw:unboxed-constant #xfff7fc) device-addr) data)
;    (do ((i (hw:unboxed-constant 0) (hw:32+ i inc)))
;       ((hw:32> i limit) nil)
;      (nubus-stuff:%bus-write (hw:32+ i device-addr) data))
;    (nubus-stuff:%bus-write (hw:32+ (hw:unboxed-constant #xfff7fc) device-addr) data)
;    )
;  )

(defun mem-disk-open (device)
  (unless (eq (mpd-state device) :closed)
    (li:error "Device ~S is already open." device))
  (setf (mpd-mem-slot device) *mem-slot*)
  (setf (mpd-quantum-bitmap device) (array:make-1d-array 255. array:art-1b))
  ;; init quantum-bitmap device.
  (let ((quantum-map (mpd-quantum-bitmap device)))
    (dotimes (i 255.)
      (setf (svref quantum-map i) 0)))
  (setf (mpd-starting-address device) 0)
  (setf (mpd-state device) :OPEN)
  )

(defun mem-disk-close (device)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (setf (mpd-state device) :CLOSED)
  )

(defun mem-disk-allocate-quantum (device)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (let ((quantum-map (mpd-quantum-bitmap device)))
    (dotimes (i (mpd-number-of-quanta device) (li:error "No more quanta left on Memory Disk" device))
      (when (zerop (svref quantum-map i))
        (setf (svref quantum-map i) 1)
        (return i)))
    )
  )

(defun mem-disk-deallocate-quantum (device device-quantum-id)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (unless (> (mpd-number-of-quanta device) device-quantum-id)
    (li:error "Quantum ~D does not exists for device ~S" device-quantum-id device))
  (when (zerop (svref (mpd-quatum-bitmap device) device-quantum-id))
    (li:error "Quantum ~D was not allocated in device ~S" device-quantum-id device))
  (setf (svref (mpd-quantum-bitmap device) device-quantum-id) 0)
  )

(defun mem-disk-start-write-cluster (device dqin cluster-in-quantum)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (setf (mpd-dqin-for-this-operation device) dqin)
  (setf (mpd-this-cluster device) cluster-in-quantum)
  (setf (mpd-state device) :Write-started)
  )

(defun mem-disk-start-read-cluster (device dqin cluster-in-quantum)
  (unless (eq (mpd-state device) :open)
    (li:error "Device ~S is not open" device))
  (setf (mpd-dqin-for-this-operation device) dqin)
  (setf (mpd-this-cluster device) cluster-in-quantum)
  (setf (mpd-state device) :read-started)
  )


(defun mem-disk-next-cluster-to-read (device physical-cluster-number)
  (unless (eq (mpd-state device) :read-started)
    (error "A read operation has not been started for device ~S" device))
  (setf (aref (mpd-accumulated-physical-cluster-swap-list device) (mpd-cluster-count device))
        physical-cluster-number)
  (incf (mpd-cluster-count device))
;  (push physical-cluster-number (mpd-accumulated-physical-cluster-swap-list device))
  )

(defun mem-disk-next-cluster-to-write (device physical-cluster-number)
  (unless (eq (mpd-state device) :write-started)
    (error "A write operation has not been started for device ~S" device))
  (setf (aref (mpd-accumulated-physical-cluster-swap-list device) (mpd-cluster-count device))
        physical-cluster-number)
  (incf (mpd-cluster-count device))
;  (push physical-cluster-number (mpd-accumulated-physical-cluster-swap-list device))
  )

(defun k-mem-slot ()
  (hw:ldb (hw:read-memory-status) hw:%%memory-status-nubus-slot-id #xF0)
  )

(defun mem-disk-activate-read (device)
  (let ((device-mem-byte-address (hw:32+
                                   (hw:dpb-unboxed (mpd-dqin-for-this-operation device)
                                                   (byte 8. 16.) (hw:unboxed-constant 0))
                                   (hw:dpb-unboxed *mem-slot* (byte 8. 24.) (mpd-starting-address device))))
        (physical-cluster-vector (mpd-accumulated-physical-cluster-swap-list device))
        (cluster-size 1024.))
    (do ((count 0 (1+ count))
         physical-byte-address)
        ((= count (mpd-cluster-count device)))
      (setq physical-byte-address (hw:dpb-unboxed (svref physical-cluster-vector count)
                                                  (byte 16. 12.) (hw:unboxed-constant 0)))
      (when (>= (+ count (mpd-this-cluster device)) 16.)
        (li:error "Cluster ran off end of quantum during mem disk read"))
      (setq device-mem-byte-address (hw:32+
                                      (hw:dpb-unboxed (+ count (mpd-this-cluster device))
                                                    (byte 4. 12.) (hw:unboxed-constant 0))
                                      device-mem-byte-address))
      (nubus-stuff:%cluster-copy device-mem-byte-address nil physical-byte-address t))
;      (dotimes (i cluster-size)
;       (nubus-stuff:%local-memory-write
;         (hw:dpb-unboxed i (byte 10. 2.) physical-byte-address)
;         (nubus-stuff:%bus-read (hw:dpb-unboxed i (byte 10. 2.) device-mem-byte-address)))))
    (operation-complete device)))

(defun mem-disk-activate-write (device)
  (let ((device-mem-byte-address (hw:32+
                                   (hw:dpb-unboxed (mpd-dqin-for-this-operation device)
                                                   (byte 8. 16.) (hw:unboxed-constant 0))
                                   (hw:dpb-unboxed *mem-slot* (byte 8. 24.) (mpd-starting-address device))))
        (physical-cluster-vector (mpd-accumulated-physical-cluster-swap-list device))
        (cluster-size 1024.))
    (do ((count 0 (1+ count))
         physical-byte-address)
        ((= count (mpd-cluster-count device)))
      (setq physical-byte-address (hw:dpb-unboxed (svref physical-cluster-vector count)
                                                  (byte 16. 12.) (hw:unboxed-constant 0)))
      (when (>= (+ count (mpd-this-cluster device)) 16.)
        (li:error "Cluster ran off end of quantum during mem disk write"))
      (setq device-mem-byte-address (hw:32+
                                      (hw:dpb-unboxed (+ count (mpd-this-cluster device))
                                                    (byte 4. 12.) (hw:unboxed-constant 0))
                                      device-mem-byte-address))
      (nubus-stuff:%cluster-copy physical-byte-address t device-mem-byte-address nil))
;      (dotimes (i cluster-size)
;       (nubus-stuff:%bus-write
;         (hw:dpb-unboxed i (byte 10. 2.) device-mem-byte-address)
;         (nubus-stuff:%local-memory-read (hw:dpb-unboxed i (byte 10. 2.) physical-byte-address)))))
    (operation-complete device)))

(defun mem-disk-operation-complete (device)
  (setf (mpd-dqin-for-this-operation device) nil)
  (setf (mpd-this-cluster device) nil)
  (setf (mpd-cluster-count device) 0)
  (setf (mpd-state device) :open)
  t
  )

(defun make-mem-disk-device ()
  (make-mpd-device
    :state :closed
    :cluster-count 0
    :dqin-for-this-operation nil
    :this-cluster nil
    :number-of-quanta 255.
    :opener 'mem-disk-open
    :closer 'mem-disk-close
    :quantum-allocator 'mem-disk-allocate-quantum
    :quantum-deallocator 'mem-disk-deallocate-quantum
    :reader-initializer 'mem-disk-start-read-cluster
    :writer-initializer 'mem-disk-start-write-cluster
    :next-cluster-reader 'mem-disk-next-cluster-to-read
    :next-cluster-writer 'mem-disk-next-cluster-to-write
    :reader-activate 'mem-disk-activate-read
    :writer-activate  'mem-disk-activate-write
    :operation-completer 'mem-disk-operation-complete
    )
  )

(defun wire-down-pages (&aux pcd-data)
  ;; wire down existing world.
  (dotimes (i  vinc:*physical-memory-max-clusters*)
    (setq pcd-data (pcd:read-pcd i))
    (unless (eq (pcd:pcd-status pcd-data) pcd:$$status-invalid)
      (pcd:write-pcd i (setf (pcd:pcd-status pcd-data) pcd:$$status-wired))))
  )

(defun set-all-paging-device-numbers (number)
  (dotimes (q 4096.)
    (quantum-map:modify-quantum-map
      q #'(lambda (data) (hw:dpb number quantum-map:%%quantum-map-device data)))))


(defun paging-device-open? (device)
  (eq (mpd-state device) :open)
  )

;;;; Test functions.

(defvar *paging-device* nil)

(defvar *big-array* nil)

(defun set-up-paging-stuff ()
  (setq gr:*findcore-pointer* 0) ;;; Just until PCD-TABLE gets recompiled
  (boot-stack-groups)
  (or *paging-devices* (setq *paging-devices* (make-paging-devices)))
  (setq *paging-device* (make-mem-disk-device))
  (open-paging-device *paging-device*)
  (setf (svref *paging-devices* *page-band-paging-device-id*) *paging-device*)
  (set-all-paging-device-numbers 2)
  (error "Set dev numbers")
  (wire-down-pages)
  (error "Wired pages")
  ;; init mem disk
  (initialize-memory-disk)
  (error "Inited mem disk")
  ;; allocate a monstruous array that will not fit in core.
  (setq *big-array* (array:make-array  '(2000. 1024.) :element-type t))
  (loop)
  )

(defun force-paging ()
  (loop
    (let ((a *big-array*) v)
      (dotimes (i 2000.)
        (dotimes (j 1024.)
          (setf (aref a i j) i)))
      (error "Wrote *big-array*")
      (dotimes (i 2000.)
        (dotimes (j 1024.)
          (unless (= i (aref a i j))
            (error "Bad data reading *big-array*" i j (aref a i j)))))
      (error "Read *big-array*")
      (setq v (array:array-element-type a))))
  (loop))
