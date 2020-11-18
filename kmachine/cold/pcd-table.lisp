;;; -*- Mode:LISP; Package:PHYSICAL-CLUSTER-DATA; Base:10; Readtable:CL -*-

(in-package 'physical-cluster-data)

(export '(
          %%pcd-write-bits

          $$cluster-no-read-mar
          $$cluster-no-write-mar
          $$cluster-read-only
          $$cluster-read-write

          $$status-normal

          $$write-clean
          $$write-clean-mar
          $$write-clean-read-only
          $$write-clean-read-only-mar
          $$write-normal
          $$write-mar
          $$write-read-only
          $$write-read-only-mar

          allocate-physical-cluster
          associate-cluster
          assure-free-physical-clusters
          create-physical-cluster-data-table
          free-physical-cluster
          free-unused-physical-clusters
          initialize-physical-cluster-data
          mark-modified-in-pcd
          read-pcd
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Physical cluster data
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fields in the physical cluster data table.

;; unused                                 (byte  7.  0.)
(defconstant %%pcd-read-mar-bit           (byte  1.  7.))
;; This field subsumes the next three fields for the purpose of fast dispatching
;; on write faults.
(defconstant %%pcd-write-bits             (byte  3.  8.))
(defconstant %%pcd-write-mar-bit          (byte  1.  8.))
(defconstant %%pcd-read-only-bit          (byte  1.  9.))
(defconstant %%pcd-clean-bit              (byte  1. 10.))
(defconstant %%pcd-virtual-cluster-number (byte (byte-size vinc:%%cluster-number) 11.))
;; unused                                 (byte  2. 27.)
(defconstant %%pcd-status                 (byte  3. 29.))

(vinc::defextractor pcd-status                  %%pcd-status)
(vinc::defextractor pcd-virtual-cluster-number %%pcd-virtual-cluster-number)

(defconstant $$status-invalid   0.)
(defconstant $$status-wired     1.)
(defconstant $$status-normal    2.)
(defconstant $$status-age-1     3.)
(defconstant $$status-age-2     4.)
(defconstant $$status-age-3     5.)
(defconstant $$status-flushable 6.)
(defconstant $$status-pre-paged 7.)

(defconstant $$write-normal              #b000)
(defconstant $$write-mar                 #b001)
(defconstant $$write-read-only           #b010)
(defconstant $$write-read-only-mar       #b011)
(defconstant $$write-clean               #b100)
(defconstant $$write-clean-mar           #b101)
(defconstant $$write-clean-read-only     #b110)
(defconstant $$write-clean-read-only-mar #b111)

(defconstant $$unclean-cluster #b0)
(defconstant $$clean-cluster   #b1)

(defconstant $$cluster-no-read-mar #b0)
(defconstant $$cluster-read-mar    #b1)

(defconstant $$cluster-no-write-mar #b0)
(defconstant $$cluster-write-mar    #b1)

(defconstant $$cluster-read-write #b0)
(defconstant $$cluster-read-only  #b1)

(vinc::defflag-extractor clean-cluster? %%pcd-clean-bit $$clean-cluster)

(defsubst read-pcd (index)
  (system-table-ref   gr::*physical-cluster-data-table* index))

(defsubst write-pcd (index new-value)
  (system-table-store gr::*physical-cluster-data-table* index new-value))

(defmacro modify-pcd (index thunk)
  `(LET ((THUNK ,thunk)
         (INDEX ,index))
     (WRITE-PCD INDEX
                (FUNCALL THUNK (READ-PCD INDEX)))))

;;; We keep the free clusters linked together so when it comes time to
;;; allocate them, we can do it fast.  If we have to go to findcore, we
;;; probably lose anyway.

(defun free-physical-cluster (cluster)
  (write-pcd cluster
    (vinc::dpb-multiple-unboxed
      (hw:ldb gr::*physical-cluster-free-list* (byte (byte-size %%pcd-virtual-cluster-number) 0.) 0.)
                           %%pcd-virtual-cluster-number
      $$status-invalid %%pcd-status
      0.))
  (setq gr::*physical-cluster-free-list* cluster)
  (incf gr::*physical-cluster-free-clusters*))

(defun allocate-physical-cluster ()
  (when (zerop gr::*physical-cluster-free-clusters*)
;    (trap::illop "No physical clusters to allocate")
    (findcore))
  (trap::without-traps
    #'(lambda ()
        (decf gr::*physical-cluster-free-clusters*)
        (let ((this-cluster gr::*physical-cluster-free-list*))
          (prog1 this-cluster
                 (setq gr::*physical-cluster-free-list*
                       (hw:ldb (read-pcd this-cluster) %%pcd-virtual-cluster-number 0.)))))))

;;; When we allocate a cluster, we setup the map to point at it.
;;; This assumes that it is paged to local memory.

(defun associate-cluster (physical-cluster virtual-cluster pcd-status read-only? read-mar? write-mar?)
  ;this hacks both the hardware map and the PCD.  However, note carefully this is not the same function
  ; as MAP:ASSOCIATE-CLUSTER!!
  (when (not (or (= pcd-status $$status-pre-paged)
                 (= pcd-status $$status-normal)))
    (trap::illop "Illegal initial status for cluster."))
  (map:associate-local-memory physical-cluster virtual-cluster
                     (if (= pcd-status $$status-pre-paged)
                         (if (= read-mar? $$cluster-read-mar)
                             map:$$map-status-read-mar-aged
                             map:$$map-status-read-only-aged)
                         (if (= read-mar? $$cluster-read-mar)
                             map:$$map-status-read-mar
                             map:$$map-status-read-only)))
  (write-pcd physical-cluster
             (vinc::dpb-multiple-unboxed
               pcd-status      %%pcd-status
               virtual-cluster %%pcd-virtual-cluster-number
               $$clean-cluster %%pcd-clean-bit
               read-only?      %%pcd-read-only-bit
               write-mar?      %%pcd-write-mar-bit
               read-mar?       %%pcd-read-mar-bit
               0.)))

(defun assure-free-physical-clusters (how-many)
  (let ((number-to-allocate (- how-many gr::*physical-cluster-free-clusters*)))
    (unless (minusp number-to-allocate)         ;Usually full (I hope!)
      (dotimes (n number-to-allocate)
        (findcore)))))

;;; Of course, when we start off, we won't be paging anything in.
;(defun findcore ()
;  (trap::illop "Findcore called."))

;;; Needs to adjust the map, etc.
;;;; Findcore
;;;; Loop throught the PCD looking for a flushable cluster.
;;;; We age clusters while we look.
(defun findcore ()
  (loop
    (setq gr:*findcore-pointer* (1+ gr:*findcore-pointer*))
    ;; wrap at end of table
    (when (= gr:*findcore-pointer* vinc:*physical-memory-max-clusters*)
      (setq gr:*findcore-pointer* 0))
    (let ((pcd-data (read-pcd gr:*findcore-pointer*)))
      (dispatch %%pcd-status pcd-data
         ($$status-invalid)
         ($$status-wired)
         ($$status-normal (age-cluster pcd-data $$status-age-1)
                          (map:age-virtual-cluster
                            (pcd-virtual-cluster-number pcd-data)))
         ($$status-age-1  (age-cluster pcd-data $$status-age-2))
         ($$status-age-2  (age-cluster pcd-data $$status-age-3))
         ($$status-age-3  (age-cluster pcd-data $$status-flushable))
         (($$status-flushable
           $$status-pre-paged)
;          (unless (clean-cluster? pcd-data)
;            (illop "Need to really page something out")
             (map-fault:swap-out-internal gr:*findcore-pointer*)
;            )
;         (map:free-virtual-cluster (pcd-virtual-cluster-number pcd-data))
;         (free-physical-cluster gr:*findcore-pointer*)
          (return-from findcore))))))

(defun age-cluster (pcd-data new-status)
  (write-pcd gr:*findcore-pointer*
             (setf (pcd-status pcd-data)
                   new-status)))

(defun rejuvenate-cluster (physical-cluster)
  (modify-pcd physical-cluster
              #'(lambda (pcd)
                  (setf (pcd-status pcd) pcd:$$status-normal)))
  )

;;; Booting the PCD.

;;; There is a structure which contains the configuration
;;; of physical memory at boot time.  Each entry in the
;;; structure has the physical cluster, where in virtual memory
;;; this cluster should go, how many contiguous clusters
;;; should be mapped this way, and what the status should be.
;;; The structure is terminated by the mapping for cluster 0.
;;; Since this contains NIL, it will always be on the list.

;;; The status should say whether the group of clusters is wired or not
;;; and whether the cluster is read-only or not.

(defconstant %%init-map-status    (byte 2. 0.))
(defconstant %%init-map-read-only (byte 1. 0.))
(defconstant %%init-map-wired     (byte 1. 1.))

(vinc::defflag-extractor init-read-only? %%init-map-read-only 1.)
(vinc::defflag-extractor init-wired?     %%init-map-wired     1.)

(defconstant $$init-map-normal          #b00)
(defconstant $$init-map-read-only       #b01)
(defconstant $$init-map-wired           #b10)
(defconstant $$init-map-wired-read-only #b11)

(defconstant $$init-map-physical-cluster  0)
(defconstant $$init-map-virtual-cluster   1)
(defconstant $$init-map-initial-status    2)
(defconstant $$init-map-cluster-count     3)

;;; After creating the physical-cluster-data-table, we should be
;;; able to take write faults that toggle the modified bit.

(defun init-pcd (physical-cluster virtual-cluster status write-bits)
  (write-pcd physical-cluster
    (vinc::dpb-multiple-unboxed
      virtual-cluster %%pcd-virtual-cluster-number
      status          %%pcd-status
      write-bits      %%pcd-write-bits
      0)))

(defconstant *clusters-in-pcd*
             (lisp::ceiling vinc:*physical-memory-max-clusters* vinc:*qs-in-cluster*))

(defun create-physical-cluster-data-table ()

  (setq gr::*physical-cluster-free-clusters* 0)
  (setq gr::*findcore-pointer* 0)

  ;; Allocate room for the physical cluster data.
  (let ((pcd-physical-location
          (boot-allocate-physical-clusters *clusters-in-pcd*)))

    ;; Setup the maps for the physical cluster data.
    (do ((virtual  (cluster-number gr::*physical-cluster-data-table*) (1+ virtual))
         (physical pcd-physical-location         (1+ physical))
         (count    *clusters-in-pcd*               (1- count)))
        ((zerop count) '())
      (map:associate-local-memory physical virtual map:$$map-status-normal))

    ;; Invalidate all physical clusters.
    (do ((physical 0 (1+ physical))
         (count vinc:*physical-memory-max-clusters* (1- count)))
        ((zerop count) '())
      (init-pcd physical 0 $$status-invalid $$write-clean))

    ;; Now, fill in the physical-cluster-data for the clusters it uses.
    ;; Note:  We mark them as modified.
    (do ((virtual  (cluster-number gr::*physical-cluster-data-table*)
                   (1+  virtual))
         (physical pcd-physical-location
                   (1+ physical))
         (count    *clusters-in-pcd*
                   (1- count)))
        ((zerop count) '())
      (init-pcd physical virtual $$status-wired $$write-normal))))

(defun read-pcd-init-entry (base entry-number offset)
  (let ((address (ash entry-number 2.)))
    (system-table-ref base (+ address offset))))

(defun initialize-physical-cluster-data (pcd-init-pointer)
  (labels ((initialize-entry (n)
             (let ((physical-cluster (read-pcd-init-entry pcd-init-pointer n $$init-map-physical-cluster))
                   (virtual-cluster  (read-pcd-init-entry pcd-init-pointer n $$init-map-virtual-cluster))
                   (status-bits      (read-pcd-init-entry pcd-init-pointer n $$init-map-initial-status))
                   (cluster-count    (read-pcd-init-entry pcd-init-pointer n $$init-map-cluster-count)))
               (let ((pcd-status (if (init-wired? status-bits)
                                     $$status-wired
                                     $$status-normal))
                     (pcd-write  (if (init-read-only? status-bits)
                                     $$write-clean-read-only
                                     $$write-clean)))
                 (do ((virtual  virtual-cluster  (1+ virtual))
                      (physical physical-cluster (1+ physical))
                      (count    cluster-count    (1- count)))
                     ((zerop count))
                   (init-pcd physical virtual pcd-status pcd-write)
                   (if (= pcd-write $$write-clean-read-only)
                       (map:associate-local-memory physical virtual map:$$map-status-read-only)
                       (map:associate-local-memory physical virtual map:$$map-status-normal))))
             (if (zerop physical-cluster)
                 ;; Make the cluster with nil on it dirty.
                 ;; This is a crock.  Another crock, entry for 0 terminates initialization table.
                 (progn
                   (map:write-map-status 0 map:$$map-status-normal)
                   (mark-modified-in-pcd (map:map-on-board-address (map::read-map 0))))
                 (initialize-entry (1+ n))))))

    (initialize-entry 0)))

(defun free-unused-physical-clusters (physical-memory-layout)   ;bit per megabyte bitmap
  (labels ((free-block (n)                      ;megabyte number.
             (cond ((= n vinc:*blocks-of-physical-memory*) nil)
                   ((map::physical-block-exists? n physical-memory-layout)
                    (free-real-block n)
                    (free-block (1+ n)))
                   (t (free-nonexistent-block n)
                      (free-block (1+ n)))))

           (free-real-block (n)
             (dotimes (cluster-in-block vinc:*clusters-in-physical-block*)
               (let ((cluster (hw:dpb n hw:%%cluster-physical-address-block cluster-in-block)))
                 (let ((status (pcd-status (read-pcd cluster))))
                   (when (= status $$status-invalid)
                     (free-physical-cluster cluster))))))

           (free-nonexistent-block (n)
             (dotimes (cluster-in-block vinc:*clusters-in-physical-block*)
               (write-pcd (hw:dpb n hw:%%cluster-physical-address-block cluster-in-block)
                          (hw:dpb-unboxed $$status-invalid %%pcd-status
                                          (hw:unboxed-constant 0))))))
    (free-block 0)))

;  ;; Everything not here already is paged out, we
;  ;; free all the remaining physical memory.
;  (dotimes (i (get-physical-memory-size))
;    (let ((status (pcd-status (read-pcd i))))
;      (when (= status $$status-invalid)
;       (free-physical-cluster i))))

;

(defun mark-modified-in-pcd (cluster)
  (modify-pcd cluster
    #'(lambda (pcd-entry)
        (hw:dpb $$unclean-cluster %%pcd-clean-bit pcd-entry))))
