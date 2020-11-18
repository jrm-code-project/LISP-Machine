;;; -*- Mode:LISP; Package:SIM-DEBUG; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;;;;;;;;;
;;; Null paging device
;;;;;;;;;;;;;;;;;;;;;;;

;;; this paging device only allowes open, close and status, anything else is an error

(defstruct (null-paging-device (:include paging-devices::paging-device)
                               (:conc-name "NPD-")
                               (:constructor make-npd-internal))
  state)

(defun npd-opener (device)
  (setf (npd-state device) :open))

(defun npd-closer (device)
  (setf (npd-state device) :close))

(defun npd-illegal-op (device &rest ignore)
  (illop "Attempted paging operation to fake paging device ~a" device))

(defun make-null-paging-device ()
  (make-npd-internal
    :state :closed
    :status-generator           'npd-state
    :opener                     'npd-opener
    :closer                     'npd-closer
    :quantum-allocator          'npd-illegal-op
    :quantum-deallocator        'npd-illegal-op
    :reader-initializer         'npd-illegal-op
    :writer-initializer         'npd-illegal-op
    :next-cluster-reader        'npd-illegal-op
    :next-cluster-writer        'npd-illegal-op
    :reader-activate            'npd-illegal-op
    :writer-activate            'npd-illegal-op
    :operation-completer        'npd-illegal-op))

;;; swap device for memory system simulator

;;;;;;;;;;;;;;;;;;;;;;;
;;; Fake paging device
;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (lambda-disk-partition-paging-device
             (:include paging-devices::paging-device)
             (:conc-name "LDP-")
             (:constructor make-ldp-paging-device-internal))
  state
  disk-unit
  partition-name
  partition-offset
  partition-size
  quantum-bitmap
  accumulated-physical-cluster-swap-list
  dqin-for-this-operation
  this-op-cluster
  this-op-cluster-count
  this-op-rqb)

(defconstant *number-of-qs-in-disk-block* si:page-size)
(defconstant *disk-blocks-per-cluster* (floor *qs-in-cluster* *number-of-qs-in-disk-block*))
(defconstant *disk-blocks-per-quantum* (* *clusters-in-quantum* *disk-blocks-per-cluster*))

(defun error-unless-open (device)
  (when (eq (ldp-state device) :closed)
    (illop "this device isn't open"))
  (unless (eq (ldp-state device) :open)
    (illop "this device has an operation in progress")))

(defun ldp-status (device)
  (list (ldp-state              device)
        (ldp-disk-unit          device)
        (ldp-partition-name     device)
        (ldp-partition-offset   device)
        (ldp-partition-size     device)
        (ldp-quantum-bitmap     device)
        (ldp-accumulated-physical-cluster-swap-list device)
        (ldp-dqin-for-this-operation device)
        (ldp-this-op-cluster    device)
        (ldp-this-op-cluster-count device)
        (ldp-this-op-rqb        device)))

(defun ldp-open (device)
  (unless (eq (ldp-state device) :closed)
    (ferror nil "device already opened"))
  (multiple-value-bind (p-off p-size)
    (si:find-disk-partition (ldp-partition-name device) nil (ldp-disk-unit device))
    (setf (ldp-partition-offset device) p-off)
    (setf (ldp-partition-size device) p-size))
  (setf (ldp-quantum-bitmap device)
        (global::make-array (floor (ldp-partition-size device) *disk-blocks-per-quantum*)
                    :type art-1b :initial-element 0))
  (setf (ldp-state device) :open)
  t)

(defun ldp-close (device)
  (error-unless-open device)
  (setf (ldp-state device) :closed)
  t)

(defun ldp-quantum-allocator (device)
  (error-unless-open device)
  (dotimes (i (array-dimension (ldp-quantum-bitmap device) 0) nil)
    (when (zerop (aref (ldp-quantum-bitmap device) i))
      (aset 1 (ldp-quantum-bitmap device) i)
      (return i))))

(defun ldp-quantum-deallocator (device device-quantum-id-number)
  device
  (error-unless-open device)
  (if (= 1 (aref (ldp-quantum-bitmap device) device-quantum-id-number))
      (progn (aset 0 (ldp-quantum-bitmap device) device-quantum-id-number)
             t)
    (ferror nil "quantum not allocated")))

(defun ldp-start-read (device dqin cluster-in-quantum)
  (error-unless-open device)
  (setf (ldp-dqin-for-this-operation device) dqin)
  (setf (ldp-this-op-cluster device) cluster-in-quantum)
  (setf (ldp-state device) :read-started)
  t)

(defun ldp-next-cluster-read (device physical-cluster-number)
  device
  (unless (eq (ldp-state device) :read-started)
    (ferror nil "a read operation has not been started for this device"))
  (incf (ldp-this-op-cluster-count device))
  (push physical-cluster-number (ldp-accumulated-physical-cluster-swap-list device))
  t)

(defun ldp-activate-read (device)
  device
  (unless (eq (ldp-state device) :read-started)
    (ferror nil "a read operation has not been started for this device"))
  (let* ((transfer-size (* (ldp-this-op-cluster-count device) *disk-blocks-per-cluster*))
         (part-offset (+ (* (ldp-this-op-cluster device) *disk-blocks-per-cluster*)
                         (* (ldp-dqin-for-this-operation device) *disk-blocks-per-quantum*))))
    (when (>= (+ part-offset transfer-size) (ldp-partition-size device))
      (ferror nil "transfer outside of partition boundaries"))
    (setf (ldp-this-op-rqb device) (si:get-disk-rqb transfer-size))
    (si:disk-read (ldp-this-op-rqb device)
                  (ldp-disk-unit device) (+ (ldp-partition-offset device) part-offset))
    (setf (ldp-state device) :read-in-progress)
    ;disk-read doesn't return until the operation is complete
    (paging-devices::operation-complete device)
    t))

(defun ldp-start-write (device dqin cluster-in-quantum)
  (error-unless-open device)
  (setf (ldp-dqin-for-this-operation device) dqin)
  (setf (ldp-this-op-cluster device) cluster-in-quantum)
  (setf (ldp-state device) :write-started)
  t)

(defun ldp-next-cluster-write (device physical-cluster-number)
  device
  (unless (eq (ldp-state device) :write-started)
    (ferror nil "a write operation has not been started for this device"))
  (push physical-cluster-number (ldp-accumulated-physical-cluster-swap-list device))
  t)

(defun ldp-activate-write (device)
  device
  (unless (eq (ldp-state device) :write-started)
    (ferror nil "a write operation has not been started for this device"))
  (let* ((transfer-size (* (ldp-this-op-cluster-count device) *disk-blocks-per-cluster*))
         (part-offset (+ (* (ldp-this-op-cluster device) *disk-blocks-per-cluster*)
                         (* (ldp-dqin-for-this-operation device) *disk-blocks-per-quantum*))))
    (when (>= (+ part-offset transfer-size) (ldp-partition-size device))
      (ferror nil "transfer outside of partition boundaries"))
    (setf (ldp-this-op-rqb device) (si:get-disk-rqb transfer-size))
    (si:disk-write (ldp-this-op-rqb device)
                   (ldp-disk-unit device) (+ (ldp-partition-offset device) part-offset))
    (setf (ldp-state device) :write-in-progress)
    (paging-devices::operation-complete device)))

(defun ldp-operation-complete (device)
  device
  (let ((clusters (nreverse (ldp-accumulated-physical-cluster-swap-list device)))
        (buf (si:rqb-buffer (ldp-this-op-rqb device))))
    (dotimes (i (ldp-this-op-cluster-count device))
      (let ((cluster (pop clusters)))
        (copy-array-portion buf (* 2 i *qs-in-cluster*)
                            (* 2 (1+ i) *qs-in-cluster*)
                            (global::make-array (* 2 sys:page-size) :type art-16b
                                        :displaced-to
                                        (+ simulator::*physical-memory-region-origin*
                                           (* cluster *qs-in-cluster*)))
                            0 (* 2 sys:page-size))))
    (si:return-disk-rqb (ldp-this-op-rqb device))
    (setf (ldp-accumulated-physical-cluster-swap-list device) nil)
    (setf (ldp-dqin-for-this-operation device) nil)
    (setf (ldp-this-op-cluster device) nil)
    (setf (ldp-this-op-cluster-count device) 0)
    (setf (ldp-this-op-rqb device) nil)
    (setf (ldp-state device) :open)
    t))

(defun make-lambda-disk-partition-paging-device (unit part)
  (make-ldp-paging-device-internal
    :state                      :closed
    :disk-unit                  unit
    :partition-name             part
    :accumulated-physical-cluster-swap-list     nil
    :this-op-cluster-count      0

    :status-generator           'ldp-status
    :opener                     'ldp-open
    :closer                     'ldp-close
    :quantum-allocator          'ldp-quantum-allocator
    :quantum-deallocator        'ldp-quantum-deallocator
    :reader-initializer         'ldp-start-read
    :writer-initializer         'ldp-start-write
    :next-cluster-reader        'ldp-next-cluster-read
    :next-cluster-writer        'ldp-next-cluster-write
    :reader-activate            'ldp-activate-read
    :writer-activate            'ldp-activate-write
    :operation-completer        'ldp-operation-complete))

;;; For the purposes of simulation, the quantum map devices
;;; will be offsets into this vector which will point to
;;; quantum devices.

(defvar *quantum-devices* '())

(defvar *null-paging-device*)
(defvar *fake-paging-device*)

(defun initialize-paging-devices ()
  (when (not (boundp '*null-paging-device*))
    (setq *null-paging-device* (make-null-paging-device))
    (push *null-paging-device* *quantum-devices*))
  (when (not (boundp '*fake-paging-device*))
    (setq *fake-paging-device*
          (make-lambda-disk-partition-paging-device 0 "METR"))
    (push *fake-paging-device* *quantum-devices*)))

(eval-when (load)
  (initialize-paging-devices))

(defconstant *quantum-device-vector* (make-array 16.))

;;;;;;;;;;;;;;;
;;; Memory Map
;;;;;;;;;;;;;;;

;;; The parenthesized map values are marginal.
;;; They have the same effect, but should never
;;; be used.
(defconstant map-status-values
             #("read-mar"
               "read-only"
               "aged-to-read-mar"
               "direct-mapped"
               "(read-mar)"
               "(read-only)"
               "aged-to-read-only"
               "(normal)"
               "swapped-out"
               "(read-only)"
               "(unused)"
               "(normal)"
               "(swapped-out)"
               "(read-only)"
               "aged-to-normal"
               "normal"))

(defun map-status->string (map-status)
  (aref map-status-values map-status))

(defun show-map (entry stream)
  (let ((bits (map::read-map entry)))
    (format stream "~&~5,48d ~:[NUBUS ~7,48d~*~;LOCAL    ~*~4,48d~] ~
               Volatility ~D ~
               ~15a ~@[~*Fresh~]"
            entry
            (map::map-local-memory?      bits)
            (map::map-off-board-address  bits)
            (map::map-on-board-address   bits)
            (map::map-cluster-volatility bits)
            (aref map-status-values (map::extract-map-status bits))
            (map::cluster-is-fresh? bits))))

(defun show-gc-ram (quantum stream)
  (hw:write-md-unboxed (ash quantum (byte-position hw:%%gc-ram-md-byte)))
  (let ((bits (hw:read-gc-ram)))
    (format stream "~&~4,48d Volatility ~d ~:[not oldspace~;oldspace~]" quantum
            (ldb hw:%%gc-ram-quantum-volatility bits)
            (= (ldb hw:%%gc-ram-quantum-oldspace bits) hw:$$oldspace))))

;;;;;;;;
;;; PCD
;;;;;;;;

(defconstant pcd-status-values
             #("invalid"
               "wired"
               "normal"
               "age-1"
               "age-2"
               "age-3"                          ;
               "flushable"
               "pre-paged"))

(defun decode-pcd (entry)
  (list (ldb pcd::%%pcd-virtual-cluster-number entry)
        (aref pcd-status-values (ldb pcd::%%pcd-status entry))
        (ldb-test pcd::%%pcd-clean-bit     entry)
        (ldb-test pcd::%%pcd-read-only-bit entry)
        (ldb-test pcd::%%pcd-write-mar-bit entry)
        (ldb-test pcd::%%pcd-read-mar-bit  entry)))

(defun show-pcd (index stream)
  (apply #'format stream "~&~4,48d ~5,48d ~9a ~
                                          ~:[    ~;not-~]modified ~
                                          read-~:[write~;only ~] ~
                                          ~@[~*write-mar~] ~
                                          ~@[~*read-mar~]"
         index (decode-pcd (pcd::read-pcd index))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Simulation startup
;;;;;;;;;;;;;;;;;;;;;;;

;;; Fake up the initial memory.
(defun cold-boot-simulation ()
  (sim::reset-simulation)
  (setq sim::*memory-status*
        (sim:k-dpb sim::$$16meg-or-less sim::%%k-memory-status-16meg 0.))

  (setq trap::trap-mask (1- (ash 1. 32.)))
  (setq sim::*trap* 0.)

  (format t "~&Direct mapping ...")
  (map:direct-map)

  (format t " done.~&Loading memory ...")
  (load-boot-vector)
  (load-initial-physical-cluster-data)
  (load-boot-gc-ram-data)
  (load-initial-map-data)

;  (load-boot-transporter-ram-data)
  (load-boot-quantum-map)
  (load-boot-region-bits)
  (map-boot-vector 0.)
  (format t " done.")
  (when (y-or-n-p "~&Cold boot?")
    (boot::cold-boot-function)))

;;; NIL is at virtual address 0.
;;; The Boot vector is an art-32b whose data begins at location 512.
;;; in the middle of cluster 0.  We assume that the cold load builder
;;; will place a header of the appropriate type before it.

;;; The physical cluster table will go in quantum 1.
;;; It takes up half of a quantum.
;;; In the second half of the quantum, we put
;;; the quantum map and the region bits.  Each of
;;; these takes up a quater of the quantum.

;;; Physical locations of the initial data.

(defparameter *boot-vector-origin*                             512.)    ;absolute.

;; Cluster addresses
(defparameter *initial-map-data-physical-location*  (cluster->address 2.))      ;64 clusters

(defparameter *quantum-map-physical-location*                   66.)
(defparameter *quantum-map-clusters*                             4.)

(defparameter *region-bits-physical-location*                   70.)
(defparameter *region-bits-clusters*                             4.)

(defparameter *initial-physical-cluster-data-physical-location* 74.)    ;1 cluster

(defparameter *initial-gc-ram-data-physical-location*           (cluster->address 75.)) ;1 cluster

(defparameter *initial-transporter-ram-data-physical-location*  76.)    ;1 cluster

;;; Virtual addresses of nifty things.

;;; Quantum 0.
;;; Cluster 1.
(defparameter *temporary-map-entry-location*            (ash 1. (byte-position %%cluster-number)))


;;; Quantum 1.
;;; Clusters 0. 7.
(defparameter *physical-cluster-table-location*        (* 1 (ash 1 (byte-position %%quantum-number))))

;;; Clusters 8. 11.
(defparameter *quantum-map-virtual-location*            (+ *physical-cluster-table-location*
                                                          (ash 1. (1- (byte-position %%quantum-number)))))
;;; Clusters 12. 15.
(defparameter *region-bits-virtual-location*            (+ *quantum-map-virtual-location*
                                                          (ash 1 (- (byte-position %%quantum-number) 2))))


;;; The microcode assumes that all physical clusters
;;; beyond this one are free.

(defparameter *boot-physical-cluster-free-pointer* 50.)

;;; Someday, I'll figure out how to do this right.

(defun get-boot-vector-entries ()
        (list
          (list 'boot::*initial-map-data*
                *initial-map-data-physical-location*)
          (list 'boot::*initial-gc-ram-data*
                *initial-gc-ram-data-physical-location*)
          (list 'vmem::*physical-cluster-free-pointer*
                *boot-physical-cluster-free-pointer*)
          (list 'vmem::*temporary-map-entry* *temporary-map-entry-location*)
          (list 'vmem::*physical-cluster-initially-wired-pointer*
                *initial-physical-cluster-data-physical-location*)
          (list 'vmem::*physical-cluster-data-table*
                *physical-cluster-table-location*)
          (list 'vmem::*physical-cluster-free-list*     0.)
          (list 'vmem::*physical-cluster-free-clusters* 0.)
          (list 'vmem::*quantum-map*
                *quantum-map-virtual-location*)
          (list 'vmem::*region-bits*
                *region-bits-virtual-location*)
          ))

;;; Fake up boot vector for simulation.

(defun load-boot-vector ()
  (do ((tail     (get-boot-vector-entries) (rest tail))
       (location *boot-vector-origin*  (1+ location)))
      ((null tail) `())
    (micro::physical-memory-write-direct location (second (first tail)))))

(defun map-boot-vector (physical-cluster)
  (map::associate-local-memory physical-cluster 0 map::$$map-status-direct-mapped))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial physical cluster data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-initial-physical-cluster-data ()
        (list

          (list *quantum-map-physical-location*
                *quantum-map-virtual-location*
                pcd::$$init-map-wired
                *quantum-map-clusters*)

          (list *region-bits-physical-location*
                *region-bits-virtual-location*
                pcd::$$init-map-wired
                *region-bits-clusters*)

          ;; This must be last.  It maps NIL.
          (list 0.
                0.
                pcd::$$init-map-wired-read-only
                1.)))


;;; This function fakes up the initial physical cluster data.
(defun load-initial-physical-cluster-data ()
  (let ((pointer (1- *initial-physical-cluster-data-physical-location*)))
  (dolist (record (get-initial-physical-cluster-data))
    (micro::physical-memory-write-direct (incf pointer) (first  record))
    (micro::physical-memory-write-direct (incf pointer)
                                         (cluster-number (second record)))
    (micro::physical-memory-write-direct (incf pointer) (third  record))
    (micro::physical-memory-write-direct (incf pointer) (fourth record)))))

(defun get-boot-gc-ram-data ()
  (list
    ;; quantum, volatility, oldspace
    ;; Resident symbols
    (list 0. 1. hw:$$not-oldspace)
    ;; Paging tables
    (list 1. 0. hw:$$not-oldspace)))

(defun load-boot-gc-ram-data ()
  (dotimes (i gc-ram::*number-of-gc-ram-entries*)
    (micro::physical-memory-write-direct
      (+ i  *initial-gc-ram-data-physical-location*)
      (dpb-multiple-unboxed
        0.                hw:%%gc-ram-quantum-volatility
        hw:$$not-oldspace hw:%%gc-ram-quantum-oldspace
        0.)))
  (dolist (record (get-boot-gc-ram-data))
    (micro::physical-memory-write-direct
      (+ (first record) *initial-gc-ram-data-physical-location*)
      (dpb-multiple-unboxed
        (second record) hw:%%gc-ram-quantum-volatility
        (third  record) hw:%%gc-ram-quantum-oldspace
        0.))))

;(defun load-boot-transporter-ram-data ()
;  ;; All unused entries trap.
;  (dotimes (mode transporter-ram::*number-of-transporter-modes*)
;    (dotimes (type transporter-ram::*number-of-transport-types*)
;      (dotimes (md-byte transporter-ram::*number-of-transporter-md-byte-values*)
;       (let ((transporter-data 0.))
;         (dotimes (vma-boxed 2.)
;           (dotimes (md-boxed 2.)
;             (setq transporter-data
;                   (sim:k-dpb
;                     (sim:k-ldb
;                       (sim:k-dpb
;                         hw::$$trappable-pointer    sim::%%k-transporter-ram-trappable-pointer
;                         (sim:k-dpb
;                           hw::$$trap-if-oldspace     sim::%%k-transporter-ram-trap-if-oldspace
;                           (sim:k-dpb
;                             hw::$$trap-if-not-oldspace sim::%%k-transporter-ram-trap-if-not-oldspace
;                             (sim:k-dpb
;                               hw::$$box-error            sim::%%k-transporter-ram-box-error
;                               0))))
;                       (byte 4. 4.)
;                       0.)
;                     (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed)))
;                     transporter-data))))
;         (micro::physical-memory-write-direct
;           (+ (cluster->address *initial-transporter-ram-data-physical-location*)
;              (sim:k-dpb
;                md-byte (byte (byte-size sim::%%k-transporter-md-byte) 0.)
;                (sim:k-dpb
;                  type    (byte (byte-size sim::%%k-memory-status-transport-ram-bits)
;                                (byte-size sim::%%k-transporter-md-byte))
;                  (sim:k-dpb
;                    mode    (byte (byte-size sim::%%k-memory-control-transporter-mode)
;                                  (+ (byte-size sim::%%k-memory-status-transport-ram-bits)
;                                     (byte-size sim::%%k-transporter-md-byte)))
;                    0.))
;                transporter-data)))))

;  ;; Normal mode, all unboxed never traps.
;  (let ((mode      $$transport-mode-normal)
;       (vma-boxed $$unboxed)
;       (md-boxed  $$unboxed))
;    (dotimes (type *number-of-transport-types*)
;      (dotimes (md-byte *number-of-transporter-md-byte-values*)
;       (let* ((location
;                (+ (cluster->address *initial-transporter-ram-data-physical-location*)
;                   (dpb-multiple-unboxed
;                     md-byte (byte (byte-size %%k-transporter-md-byte) 0.)
;                     type    (byte (byte-size %%k-memory-status-transport-type)
;                                   (byte-size %%k-transporter-md-byte))
;                     mode    (byte (byte-size %%k-memory-control-transporter-mode)
;                                   (+ (byte-size %%k-memory-status-transport-type)
;                                      (byte-size %%k-transporter-md-byte)))
;                     0.)))
;              (transporter-data (micro::physical-memory-read-direct location)))
;         (setq transporter-data
;               (%dpb
;                 (%ldb
;                   (dpb-multiple-unboxed
;                     $$non-trappable-pointer     %%k-transporter-ram-trappable-pointer
;                     $$dont-trap-if-oldspace     %%k-transporter-ram-trap-if-oldspace
;                     $$dont-trap-if-not-oldspace %%k-transporter-ram-trap-if-not-oldspace
;                     $$no-box-error              %%k-transporter-ram-box-error
;                     0.)
;                   (byte 4. 4.)
;                   0.)
;                 (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed)))
;                 transporter-data))
;         (micro::physical-memory-write-direct location transporter-data)))))

;  ;; Normal mode, type no-transport, never trans trap.
;  (let ((mode $$transport-mode-normal)
;       (type $$transport-type-no-transport))
;    (dotimes (md-byte *number-of-transporter-md-byte-values*)
;      (let* ((location
;              (+ (cluster->address *initial-transporter-ram-data-physical-location*)
;                 (dpb-multiple-unboxed
;                   md-byte (byte (byte-size %%k-transporter-md-byte) 0.)
;                   type    (byte (byte-size %%k-memory-status-transport-type)
;                                 (byte-size %%k-transporter-md-byte))
;                   mode    (byte (byte-size %%k-memory-control-transporter-mode)
;                                 (+ (byte-size %%k-memory-status-transport-type)
;                                    (byte-size %%k-transporter-md-byte)))
;                   0.)))
;            (transporter-data (micro::physical-memory-read-direct location)))
;       (dotimes (vma-boxed 2.)
;         (dotimes (md-boxed 2.)
;           (setq transporter-data
;                 (%dpb
;                   (%ldb
;                     (dpb-multiple-unboxed
;                       $$dont-trap-if-oldspace     %%k-transporter-ram-trap-if-oldspace
;                       $$dont-trap-if-not-oldspace %%k-transporter-ram-trap-if-not-oldspace
;                       $$no-box-error              %%k-transporter-ram-box-error
;                       (%ldb transporter-data (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed))) 0.))
;                     (byte 4. 4.)
;                     0.)
;                   (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed)))
;                   transporter-data))))
;       (micro::physical-memory-write-direct location transporter-data)))))


(defparameter *boot-quanta*
             (list
               (list 0. *null-paging-device* 0)
               (list 1. *null-paging-device* 1)))

(defun load-boot-quantum-map ()
  (do ((devices (reverse *quantum-devices*) (if devices (rest devices) nil))
       (devnum  0                 (1+ devnum)))
      ((or (null devices) (= devnum 16.))
       (when devices
         (ferror nil "Too many quantum devices")))
    (setf (aref *quantum-device-vector* devnum)
          (if devices
              (first devices)
              *null-paging-device*
              )))

  ;; Zero all quanta.
  (dotimes (i *number-of-quanta*)
    (micro::physical-memory-write-direct
      (+ i (cluster->address *quantum-map-physical-location*))
      0.))

  ;; Write initial quanta.
  (dolist (q *boot-quanta*)
    (micro::physical-memory-write-direct
      (+ (first q) (cluster->address *quantum-map-physical-location*))
      (dpb-multiple-unboxed
        (find-position-in-list (second q) *quantum-devices*)
                                       quantum-map::%%quantum-map-device
        (third  q)                     quantum-map::%%quantum-map-region-origin
        quantum-map::$$quantum-mapped  quantum-map::%%Quantum-map-status
        0.))))

(defun init-boot-vector-macro ()
  (do ((tail (get-boot-vector-entries) (rest tail))
       (count 0 (1+ count))
       (code '() (cons `(SETQ ,(first (first tail)) (BOOT::READ-BOOT-VECTOR ,count)) code)))
      ((null tail) `(PROGN ,@(reverse code)))))

(defun initialize-from-boot-vector ()
  (eval (init-boot-vector-macro)))              ;GAK! CHOKE! ARGH!

(defun load-initial-map-data ()

  ;; Make everything volatility 0.
  (dotimes (i map::*number-of-map-entries*)
    (micro::physical-memory-write-direct
      (+ i *initial-map-data-physical-location*)
      (dpb-multiple-unboxed
        0.                     hw:%%map-volatility
        map::$$cluster-fresh map::%%map-fresh-cluster
      0.)))

  ;; Cluster 0 is volatility 1.
  (micro::physical-memory-write-direct
     *initial-map-data-physical-location*
    (dpb-multiple-unboxed
      1.                         hw:%%map-volatility
      map::$$cluster-not-fresh map::%%map-fresh-cluster
      0.)))

;;;;;;;;;;;;;;;;
;;; Region bits
;;;;;;;;;;;;;;;;

(defun get-boot-regions ()
        (list

          ;; Resident symbols
          (list 0.                              ;quantum 0
                1.                              ;one quantum long
                (region-bits:parameters->region-bits
                  region-bits:$$region-space-fixed
                  region-bits:$$scavenge-enabled
                  region-bits:$$region-read-only
                  0.))                          ;swapin quantum 0

          ;; Paging tables
          (list 1.                              ;quantum 1
                1.                              ;one quantum long
                (region-bits:parameters->region-bits
                  region-bits:$$region-space-fixed
                  region-bits:$$scavenge-disabled
                  region-bits:$$region-read-write
                  0.))                          ;swapin quantum 0
          ))

(defun load-boot-region-bits ()
  ;; Zero all the region bits.
  (dotimes (i region-bits:*number-of-regions*)
    (micro::physical-memory-write-direct (+ i (cluster->address *region-bits-physical-location*))
      (region-bits:parameters->region-bits
        region-bits:$$region-space-free
        region-bits:$$scavenge-disabled
        region-bits:$$region-read-only
        0.)))

  (dolist (record (get-boot-regions))
    (let ((quantum-start      (first  record))
          (quantum-count      (second record))
          (region-bits        (third  record)))
      (do ((quantum quantum-start (1+ quantum))
           (count   quantum-count (1- count)))
          ((zerop count))
        (micro::physical-memory-write-direct (+ quantum (cluster->address *region-bits-physical-location*))
          region-bits)))))

(defun read-region-bits-from-gc-ram (region)
  (hw:write-md-unboxed (hw:dpb region hw:%%gc-ram-md-byte 0))
  (hw:read-gc-ram))

(defun show-region-bits (region stream)
  (let ((region-bits (region-bits::read-region-bits region))
        (gc-ram-bits (read-region-bits-from-gc-ram  region)))
    (format stream "~&~4,48D Volatility ~D ~:[~
                    ~[FREE-~;STATIC-~;FIXED-~;COPY~;NEW-LIST-~;NEW-STRUCTURE-~;ERROR~;ERROR~]~
                    ~;OLD~*~]SPACE ~
                    Scavenge ~:[Dis~;En~]abled ~
                    Read-~:[Write~;Only~] ~
                    ~2,48D"
            region
            (hw:ldb gc-ram-bits hw:%%gc-ram-quantum-volatility 0)
            (= (hw:ldb gc-ram-bits hw:%%gc-ram-quantum-oldspace   0) hw:$$oldspace)
            (region-bits::region-space-type       region-bits)
            (region-bits::region-scavenge-enable? region-bits)
            (region-bits::region-read-only?       region-bits)
            (region-bits::region-swapin-quantum   region-bits))))

(defun show-quantum-map (quantum stream)
  (let ((bits (quantum-map::read-quantum-map quantum)))
    (format stream "~&~4,48D ~[EMPTY    ~;ALLOCATED~;ERROR    ~;MAPPED   ~] ~4,48D ~2,48D ~4,48D"
            quantum
            (quantum-map::quantum-status-bits bits)
            (quantum-map::region-origin      bits)
            (quantum-map::quantum-device     bits)
            (quantum-map::quantum-dqin       bits))))

;;;;;;;;;;;;;;;;
;;; Region data
;;;;;;;;;;;;;;;;

(defun show-region (region stream)
  (let* ((quantum-bits (quantum-map::read-quantum-map region))
         (origin (if (quantum-map::quantum-empty? quantum-bits)
                     (progn (format stream "~&Region ~D is not in the quantum map." region)
                            region)
                     (let ((origin (quantum-map:quantum-region-origin region)))
                       (when (not (= origin region))
                         (format stream "~&Region ~D seems to begin at ~D" region origin))
                       origin))))
    (show-region-bits origin stream)
    (format stream "~&Origin: ~8D, Free Pointer: ~8D, GC pointer: ~8D End: ~8D"
            (quantum->address    origin)
            (region-data:region-free-pointer origin)
            (region-data:region-gc-pointer   origin)
            (region-data:region-end          origin)
            )))

(defun create-region-data-for-cold-load ()
  (let ((region-data (region-bits:make-region
                       (ceiling (* region-bits:*number-of-regions* 4.) *qs-in-quantum*)
                       (region-bits:parameters->region-bits
                         region-bits:$$region-space-fixed
                         region-bits:$$scavenge-disabled
                         region-bits:$$region-read-write
                         15.)                   ;Swapin entire thing.
                       0.)))                    ;volatility 0.

    (setq region-data::*region-free-pointer*      (quantum->address region-data))
    (setq region-data::*region-allocation-status* (+ region-data::*region-free-pointer*
                                                     region-bits:*number-of-regions*))
    (setq region-data::*region-end*               (+ region-data::*region-allocation-status*
                                                     region-bits:*number-of-regions*))
    (setq region-data::*region-gc-pointer*        (+ region-data::*region-end*
                                                     region-bits:*number-of-regions*))

    ;; Bash data for initial regions.  Do this right someday.
    (setf (region-data:region-free-pointer      0) (hw:dpb 1. %%quantum-number  0.))
    (setf (region-data:region-allocation-status 0) 0)
    (setf (region-data:region-end               0) (hw:dpb 1. %%quantum-number  0.))
    (setf (region-data:region-gc-pointer        0) (hw:dpb 1. %%quantum-number  0.))

    (setf (region-data:region-free-pointer      1) (hw:dpb 2. %%quantum-number  0.))
    (setf (region-data:region-allocation-status 1) 0)
    (setf (region-data:region-end               1) (hw:dpb 2. %%quantum-number  0.))
    (setf (region-data:region-gc-pointer        1) (hw:dpb 2. %%quantum-number  0.))

    ;; We ourselves is full.
    (setf (region-data:region-free-pointer region-data)
          (+ region-data::*region-gc-pointer* region-bits:*number-of-regions*))
    (setf (region-data:region-allocation-status region-data) 0)
    (setf (region-data:region-end region-data)
          (+ region-data::*region-gc-pointer* region-bits:*number-of-regions*))
    (setf (region-data:region-gc-pointer region-data)
          (+ region-data::*region-gc-pointer* region-bits:*number-of-regions*))

    region-data
    ))

(defun create-area-data-for-cold-load (region-data-region)
  (let* ((qs-needed (+ mem:*number-of-regions*  ;Region list thread
                       mem:*number-of-areas*    ;Area region data
                       mem:*number-of-areas*    ;Area region bits
                       mem:*number-of-areas*    ;Area region size
                       ))
         (quanta-needed (ceiling qs-needed *qs-in-quantum*))
         (region-bits (region-bits:parameters->region-bits
                        region-bits:$$region-space-fixed
                        region-bits:$$scavenge-disabled
                        region-bits:$$region-read-write
                        (max 15. (1- (* quanta-needed *clusters-in-quantum*)))))
         (region (region-data:make-region quanta-needed region-bits 1)))

    (setq memlow:*region-list-thread* (region-data::region-free-pointer region))
    (region-data::advance-free-pointer region mem:*number-of-regions*)

    (setq memlow:*area-region-data* (region-data::region-free-pointer region))
    (region-data::advance-free-pointer region mem:*number-of-areas*)

    (setq memlow::*area-region-bits* (region-data::region-free-pointer region))
    (region-data::advance-free-pointer region mem:*number-of-areas*)

    (setq memlow::*area-region-size* (region-data::region-free-pointer region))
    (region-data::advance-free-pointer region mem:*number-of-areas*)

    (setf (region-data::region-gc-pointer region) (region-data::region-free-pointer region))

    ;;; Zero out the area tables
    (dotimes (area mem:*number-of-areas*)
      (setf (area-data::area-region-data area)
            (hw:dpb area-data::$$area-free area-data::%%area-data-area-status 0.)))

    ;;; Setup RESIDENT SYMBOL AREA
    (setf (area-data::area-region-data   0.)
          (dpb-multiple-boxed
            area-data::$$area-fixed area-data::%%area-data-area-status
            0.                      area-data::%%area-data-region-thread
            0.))
    (setf (area-data::region-list-thread 0.)
          (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag 0.))
    (setf (area-data::area-region-size   0.) 0.)
    (setf (area-data::area-region-bits   0.)
          (dpb-multiple-boxed
            (region-bits::parameters->region-bits
              region-bits::$$region-space-fixed
              region-bits::$$scavenge-enabled
              region-bits::$$region-read-only
              0.)))

    ;;; Setup PAGING TABLE AREA
    (setf (area-data::area-region-data   1.)
          (dpb-multiple-boxed
            area-data::$$area-fixed area-data::%%area-data-area-status
            1.                      area-data::%%area-data-region-thread
            0.))
    (setf (area-data::region-list-thread 1.)
          (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag 1.))
    (setf (area-data::area-region-size   1.) 0.)
    (setf (area-data::area-region-bits   1.)
          (dpb-multiple-boxed
            (region-bits::parameters->region-bits
              region-bits::$$region-space-fixed
              region-bits::$$scavenge-enabled
              region-bits::$$region-read-only
              0.)))

    ;;; Setup MEMORY MANAGEMENT AREA
    (setf (area-data::area-region-data   2.)
          (dpb-multiple-boxed
            area-data::$$area-fixed area-data::%%area-data-area-status
            region-data-region      area-data::%%area-data-region-thread
            0.))
    (setf (area-data::region-list-thread region-data-region)
          (hw:dpb-boxed area-data::$$thread-continues area-data::%%region-list-thread-end-flag region))
    (setf (area-data::region-list-thread region)
          (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag region-data-region))
    (setf (area-data::area-region-size 2.) 0.)
    (setf (area-data::area-region-bits 2.) region-bits)))

(defun show-area (area stream)
  (let ((bits (area-data::area-region-bits area))
        (data (area-data::area-region-data area))
        (size (area-data::area-region-size area)))
    (let ((status (hw:ldb data area-data::%%area-data-area-status 0))
          (thread (hw:ldb data area-data::%%area-data-region-thread 0))
          (rbits  (hw:ldb bits area-data::%%area-region-bits-the-bits 0))
          (vol    (hw:ldb bits area-data::%%area-region-bits-volatility 0)))
    (format stream "~&Area ~D ~[Free~;Empty~;~;Fixed~]. Regions will be Volatility ~D. ~D quanta."
            area
            status
            vol
            size)
    (when (= 1 (hw:ldb data area-data::%%area-data-area-has-regions 0))
      (let (region)
        (tagbody
         loop
            (setq region (hw:ldb thread (byte (byte-size quantum-map::%%quantum-map-region-origin) 0) 0))
            (when (= area-data::$$thread-ends (hw:ldb thread area-data::%%region-list-thread-end-flag 0))
              (when (not (= region area)) (format t "~&Thread does not re-connect!"))
              (return-from show-area nil))
            (show-region region stream)
            (setq thread (area-data::region-list-thread region))
            (go loop)))))))
