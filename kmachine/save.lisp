;;;-*- Mode:LISP; Package:K2; Base:10; Readtable:CL -*-

;;;; Save the world

;;; A saved image looks like this:
;;;
;;; Physical memory load:
;;;   00  Clusters 0-3
;;;   04  Quantum Map
;;;   08  Region Bits
;;;   0C  Initial PCD Data
;;;   0D  GC RAM
;;;   11  Region Data
;;;   21  Area Data
;;;   ??  add global vars
;;;   31  Initial Code
;;;
;;; Virtual memory load:
;;;   Regions
;;;
;;; This is the boot sequence:
;;;
;;;  1. Load the physical memory load
;;;     into physical memory.
;;;  2. Map in cluster 0
;;;  3. Map in Initial code (pointed to by boot vector)
;;;  4. ?? Load virtual memory data (regions) into virtual memory
;;;     (fast boot leave on boot device)
;;;  5. something sometime must put swaped-out in maps...
;;;  6. Size memory, put that in boot vector.
;;;  7. Put prom version in boot vector
;;;  8. Jump to code start
;;;


(defparameter saved-image-initial-gc-ram-data-physical-location (ash #xD 10.))
(defparameter saved-image-region-data-physical-location (ash #x11 10.))
(defparameter saved-image-area-data-physical-location (ash #x21 10.))

(defun initial-data-size ()
  (+
    (vinc:cluster->address 4)                   ; clusters 0-3
    vinc:*number-of-quanta*                     ; quantum map
    memory-management:*number-of-regions*       ; region bits
    (vinc:cluster->address 5)                   ; PCD data, GC RAM
    vinc:*qs-in-quantum*                        ; Region Data 1 quantum (* 4 region-bits:*number-of-regions*)
    vinc:*qs-in-quantum*                        ; Area Data   1 quantum (why so big?)
;   (vinc:cluster->address 1)                   ; global vars
    ))


(defun save-it ()
  (save-physical) (loop))


(defun save-physical ()
  (let ((initial-code-size (initial-code-size))
        (initial-data-size (initial-data-size)))
  (setup-boot-vector initial-data-size initial-code-size)
;  ;; save physical memory load size
;  (save-word (+ initial-data-size
;               initial-code-size))
  (save-cluster 0)
  ;; we want to leave space in memory
  ;; for the special clusters
  ;; (i think we only need to reserve cluster 2 in physical memory
  ;; but we need to be in sync with KOLD-LOADER which has the quantum map
  ;; in cluster 4)
  (skip-cluster)  ;cluster 1 temporary map entry
  (skip-cluster)  ;cluster 2 kbug communication area
  (skip-cluster)  ;cluster 3 *k-io-regs-cluster* nubus mapped
  ;; NUBUS-INTERRUPTS says cluster 4 is *bus-access-cluster*
  ;; reserved for external bus access routines
  (save-quantum-map)
  (save-region-bits)
  (save-initial-pcd-data)
  (save-gc-ram)
  (save-region-data)
  (save-area-data)
;  (save-global-vars)
  (save-initial-code)
  ;this marks the end
  (save-block 0 0)))

;;; Save all the regions
;;; except:
;;;  region  area
;;;     0      0     cluster 0 etc
;;;     1      1     PCD, Quantum Map, Region Bits
;;;     2      2     Region Data
;;;     3      3     Area Data
;;;  2048      4     Initial Code
;;;
(defun save-virtual ()
  (dotimes (region region-bits:*number-of-regions*)
    ;; don't save regions in physical memory load
    (when (and (not (<= region 3))
               (not (= region
                       (region-bits:region-number
                         (vinc:cluster->address
                           hw:*first-instruction-cluster*)))))
      (let ((region-space-type (region-bits:region-space-type
                                 (region-bits:read-region-bits region))))
        ;; don't save invalid or free regions
        (unless (or (= region-space-type region-bits:$$region-space-invalid)
                    (= region-space-type region-bits:$$region-space-free))
          (save-region region)))))
  (save-block 0 0))

(defun write-boot-vector (element data)
  (hw:write-md-unboxed data)
  (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ boot:**boot-vector-origin** element))
  (hw:memory-wait))


(defun setup-boot-vector (initial-code-offset initial-code-size)
  (write-boot-vector boot::**initial-code-physical-location-bv-offset**
                          initial-code-offset)
  (write-boot-vector boot::**initial-code-size-in-clusters-bv-offset**
                          (1+ (vinc:cluster-number initial-code-size)))
  (write-boot-vector boot::**initial-code-entry-point-bv-offset**
                          (initial-function-entry-point))
;; these are in same place as cold load
  (write-boot-vector boot::*initial-gc-ram-data-physical-location*
                          saved-image-initial-gc-ram-data-physical-location)
;  (write-boot-vector boot::*initial-transporter-ram-data-physical-location*
;                         (cluster->address *initial-transporter-ram-data-physical-location*))
  (write-boot-vector boot::*cold-load-flag* nil))

(defun skip-cluster ()
  (skip-words vinc:*qs-in-cluster*))

(defun skip-words (n)
  ;; save in 256. word chunks
  (dotimes (i (hw:ldb n (byte (- (byte-size vinc:%%fixnum-field) 8.) 8.) 0))
    (dotimes (i #x100)
      (kbug-set-data i NIL))
    (save-block kbug-data-transfer-area-addr #x100))
  (let ((rem (hw:ldb n (byte 8. 0.) 0.)))
    (unless (zerop rem)
      (dotimes (i rem)
        (kbug-set-data i NIL))
      (save-block kbug-data-transfer-area-addr rem))))

;(defun save-cluster (cluster)
;  (do ((count 0 (1+ count))
;       (addr (cons:make-pointer vinc:$$dtp-unboxed-locative
;                               (vinc:cluster->address cluster))
;            (hw:24+ 1 addr)))
;      ((>= count vinc:*qs-in-cluster*))
;    (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
;    (save-word (hw:read-md))))

(defun save-cluster (cluster)
  (save-block (vinc:cluster->address cluster)
              vinc:*qs-in-cluster*))

;(defun save-quantum-map ()
;  (dotimes (i vinc:*number-of-quanta*)
;    (save-word (quantum-map:read-quantum-map i))))

(defun save-quantum-map ()
  (save-block gr:*quantum-map*
              vinc:*number-of-quanta*))

;(defun save-region-bits ()
;  (dotimes (i region-bits:*number-of-regions*)
;    (save-word (region-bits:read-region-bits i))))

(defun save-region-bits ()
  (save-block gr:*region-bits*
              region-bits:*number-of-regions*))

;(defun save-region-data ()
;  (dotimes (i region-bits:*number-of-regions*)
;    (save-word (region-data:region-free-pointer i)))
;  (dotimes (i region-bits:*number-of-regions*)
;    (save-word (region-data:region-end-pointer i)))
;  (dotimes (i region-bits:*number-of-regions*)
;    (save-word (region-data:region-gc-pointer i))))

(defun save-region-data ()
  ;; (region-data:region-origin 2.)
  ;; 4096 regions, 4 tables = 16 clusters = 1 quantum
  (save-block gr:*region-free-pointer* vinc:*qs-in-quantum*))

(defun save-area-data ()
  ;; (region-data:region-origin 3.)
  (save-block gr:*region-list-thread* vinc:*qs-in-quantum*))

(defun store-frame (frame)
  (trap:without-traps
      #'(lambda ()
          (hw:nop)
          (hw:nop)
          (let ((oar (hw:read-open-active-return))
                (boxbits 0))
            (hw:write-open-active-return (hw:dpb frame (byte 8. 16.) oar))
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (kbug-set-data 0. (hw:o0))
            (kbug-set-data 1. (hw:o1))
            (kbug-set-data 2. (hw:o2))
            (kbug-set-data 3. (hw:o3))
            (kbug-set-data 4. (hw:o4))
            (kbug-set-data 5. (hw:o5))
            (kbug-set-data 6. (hw:o6))
            (kbug-set-data 7. (hw:o7))
            (kbug-set-data 8. (hw:o8))
            (kbug-set-data 9. (hw:o9))
            (kbug-set-data 10. (hw:o10))
            (kbug-set-data 11. (hw:o11))
            (kbug-set-data 12. (hw:o12))
            (kbug-set-data 13. (hw:o13))
            (kbug-set-data 14. (hw:o14))
            (kbug-set-data 15. (hw:o15))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o15)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o14)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o13)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o12)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o11)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o10)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o9)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o8)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o7)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o6)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o5)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o4)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o3)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o2)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o1)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o0)))
            (kbug-set-data 16. boxbits)
            (hw:write-open-active-return oar))))
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop))

(defun save-frame (frame)
  (store-frame frame)
  (save-block kbug-data-transfer-area-addr (1+ hw:frame-size)))

(defun save-global-vars ()
  (dotimes (i hw:number-of-global-frames)
    (save-frame i)))

;;; $$init-map-physical-cluster
;;; $$init-map-virtual-cluster
;;; $$init-map-initial-status
;;; $$init-map-cluster-count
(defun save-initial-pcd-data ()
  (let ((i -1))
    (macrolet ((set-data (value)
                 `(kbug-set-data (incf i) ,value)))
  ;; Initial instructions
  (set-data (vinc:cluster-number (initial-data-size)))
  (set-data hw:*first-instruction-cluster*)
  (set-data pcd::$$init-map-wired-read-only)
  (set-data (vinc:cluster-number (initial-code-size)))

  ;; Quantum Map
  (set-data boot:*quantum-map-physical-location*)
  (set-data (vinc:cluster-number boot:*quantum-map-virtual-location*))
  (set-data pcd::$$init-map-wired)
  (set-data boot:*quantum-map-clusters*)

  ;; Region Bits
  (set-data boot:*region-bits-physical-location*)
  (set-data (vinc:cluster-number boot:*region-bits-virtual-location*))
  (set-data pcd::$$init-map-wired)
  (set-data boot:*region-bits-clusters*)

  ;; Region Data
  (set-data (vinc:cluster-number saved-image-region-data-physical-location))
  (set-data (vinc:cluster-number gr:*region-free-pointer*))
  (set-data pcd::$$init-map-wired)   ;???
  (set-data 16.)   ; 4096 regions, 4 tables = 16 clusters = 1 quantum

  ;; Area Data
  (set-data (vinc:cluster-number saved-image-area-data-physical-location))
  (set-data (vinc:cluster-number gr:*region-list-thread*))
  (set-data pcd::$$init-map-wired)   ;???
  (set-data 16.)   ; 1 quantum

  ;; Shared memory cluster
  (set-data 2.)
  (set-data 2.)
  (set-data pcd::$$init-map-wired)
  (set-data 1.)

  ;; This must be last.  It maps NIL.
  (set-data 0.)
  (set-data 0.)
  (set-data pcd::$$init-map-wired)
  (set-data 1.)

  ;; fill out 256
  (do ((i (1+ i) (1+ i)))
      ((>= i #x100))
    (kbug-set-data i NIL))
  (save-block kbug-data-transfer-area-addr #x100)
  ;; fill out cluster
  (skip-words (- vinc:*qs-in-cluster* #x100)))))





;;; boot gc ram data was 1 cluster, but gc-ram is 4k long???
;(defun save-gc-ram ()
;  (dotimes (i gc-ram:*number-of-gc-ram-entries*)
;    (save-word (gc-ram:read-gc-ram i))))

(defun save-gc-ram ()
  (do ((i1 0 (+ i1 #x100)))
      ((>= i1 gc-ram:*number-of-gc-ram-entries*))
    (dotimes (i2 #x100)
      (kbug-set-data i2 (gc-ram:read-gc-ram (+ i1 i2))))
    (save-block kbug-data-transfer-area-addr #x100)))


;(defun save-region (region)
;  (let ((start-cluster (vinc:cluster-number (region-data:region-origin region)))
;       (end-cluster (vinc:cluster-number (region-data:region-free-pointer region))))
;    (do ((cluster start-cluster (1+ cluster)))
;       ((> cluster end-cluster))
;      (save-cluster cluster))))

(defun save-region (region)
  (save-block (region-data:region-origin region)
              (region-size region)))

(defun region-size (region)
  (cons:make-pointer vinc:$$dtp-fixnum
                     (hw:32- (region-data:region-end region)
                             ;; some magic needs to be done to do this
                             ;; (region-data:region-free-pointer region)
                             (region-data:region-origin region))))

;--------------------------------------------------------------------------------
;;;; Initial Code

;;; how do we know this?
(defconstant initial-code-area 4.)

(defun initial-code-region ()
  (area-data:area-data-region-thread (area-data:area-region-data initial-code-area)))

(defun initial-code-size ()
  ;; assuming only one region
  (region-size (initial-code-region)))

(defun initial-function-entry-point ()
  (hw:24- (%compiled-function-code (symbol:symbol-function 'boot:cold-boot-function))
          (region-bits:region-origin (initial-code-region))))

(defun save-initial-code ()
  ;; assume only one region?
  (save-region (initial-code-region)))


;--------------------------------------------------------------------------------
;;; IO stuff

(defun write-word (data)
  ;; DATA can be unboxed
  (kbug-stream-write-byte KBUG-K-OUTPUT-STREAM (hw:ldb data (byte 8  0.) 0))
  (kbug-stream-write-byte KBUG-K-OUTPUT-STREAM (hw:ldb data (byte 8  8.) 0))
  (kbug-stream-write-byte KBUG-K-OUTPUT-STREAM (hw:ldb data (byte 8 16.) 0))
  (kbug-stream-write-byte KBUG-K-OUTPUT-STREAM (hw:ldb data (byte 8 24.) 0)))

(defun get-physical-address (virtual-address)
  (let ((virtual-cluster (vinc:cluster-number virtual-address)))
    (let ((map-bits (map:read-map virtual-cluster)))
      ;; make sure its swapped in
      ;; this is broken becauset there can be more than one cluster here
      (when (= (map:extract-map-status map-bits)
               map:$$map-status-swapped-out)
        (map-fault:swap-in virtual-cluster)
        (setq map-bits (map:read-map virtual-cluster)))
      (dpb-multiple-unboxed
        0.                                  vinc:%%data-type
        (map:map-on-board-address map-bits) hw:%%mapped-vma-byte
        virtual-address))))

(defun save-block (start count)
  (write-word (get-physical-address start))
  (write-word (hw:dpb-unboxed 0 vinc:%%data-type count))
  ;; wait for ack
  (kbug-set-command #xFE)
  (do ()
      ((not (= (kbug-read-command) #xFE)))))
