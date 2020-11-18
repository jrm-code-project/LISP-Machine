;;; -*- Mode:LISP; Package:VIRTUAL-MEMORY; Base:10; Readtable:CL -*-

(in-package 'virtual-memory)

(export '(

          *temporary-map-entry*

          $$read-cdr
          $$read-no-cdr

          associate-temporary
          deassociate-temporary
          boot-allocate-physical-clusters
          md-start-write-generic
          system-table-ref
          system-table-store
          vma-start-read-generic
          write-md-generic
          write-vma-generic
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Virtual memory SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; SYSTEM TABLES
;;;;;;;;;;;;;;;;;;

(defun system-table-ref (table index)
  (hw:vma-start-read-no-transport (hw:24+ table index) :unboxed :unboxed)
  (hw:read-md))

(defun system-table-store (table index new-value)
  (hw:write-vma-unboxed (hw:24+ table index))
  (hw:md-start-write-no-gc-trap new-value :unboxed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Physical cluster free pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; At boot time, the PCD table doesn't exist, so we cannot
;;; just call findcore.  Instead, we use this free pointer
;;; and hope that we don't fall off the end of memory before
;;; we can findcore.  Called only by pcd:create-physical-cluster-data-table.

(defun boot-allocate-physical-clusters (how-many)
  (trap::without-traps
    #'(lambda ()
        (prog1 gr::*physical-cluster-free-pointer*
               (incf gr::*physical-cluster-free-pointer* how-many)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporary map entry
;;;;;;;;;;;;;;;;;;;;;;;;

;;; The temporary map entry is set up to never fault.
;;; It is used to trample on write only clusters, etc.

;;; Should think about making this atomic someday.
;;; For now, just ignore it and deal with it on an
;;; ad hoc basis.

(defconstant *temporary-map-entry* 1.)

(defun associate-temporary (virtual-cluster physical-cluster volatility)
  (hw:read-md) ;wait for mem
  (if (= gr:*temporary-map-entry-virtual-cluster* -1)
      (setq gr::*temporary-map-entry-virtual-cluster* virtual-cluster)
      (trap::illop "Recursive use of temporary map entry"))
  (map:associate-local-memory physical-cluster *temporary-map-entry*
                              map:$$map-status-normal)
  (map:write-cluster-volatility *temporary-map-entry* volatility))

(defun deassociate-temporary ()
  (hw:read-md) ;wait for mem
  (map::write-map-status *temporary-map-entry* map:$$map-status-direct-mapped)
  (setq gr:*temporary-map-entry-virtual-cluster* -1))

;(defun initialize-fresh-cluster (physical-cluster virtual-cluster)
;  (associate-temporary physical-cluster)
;  (labels ((zap-cluster (temporary-address virtual-address count)
;            (if (zerop count)
;                ()
;                (progn (hw:write-md-unboxed virtual-address)
;                       (hw:vma-start-write-unboxed-no-gc-trap temporary-address)
;                       (zap-cluster (1+ temporary-address)
;                                    (1+ virtual-address)
;                                    (1- count))))))
;    (zap-cluster (cluster->address gr::*temporary-map-entry*)
;                (cluster->address virtual-cluster)
;                vinc:*qs-in-cluster*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic memory instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Of course all of these should be called
;;; from compiled code only.

(defun write-md-generic (md boxed)
  (if (= boxed hw:$$boxed)
      (hw:write-md-boxed   md)
      (hw:write-md-unboxed md)))

(defun md-start-write-generic (md md-boxed gc-write-test)
  (if (= md-boxed hw:$$boxed)
      (if (= gc-write-test vinc:$$gc-write-test)
          (hw:md-start-write-boxed               md)
          (hw:md-start-write-no-gc-trap-boxed    md))
      (if (= gc-write-test vinc:$$gc-write-test)
          (hw:md-start-write-unboxed             md)
          (hw:md-start-write-no-gc-trap-unboxed  md))))

(defun write-vma-generic (vma vma-boxed)
  (if (= vma-boxed hw:$$boxed)
      (hw:write-vma-boxed   vma)
      (hw:write-vma-unboxed vma)))

(defun vma-start-write-generic (vma vma-boxed gc-trap)
  (if (= vma-boxed hw:$$boxed)
      (if (= gc-trap vinc:$$gc-write-test)
          (hw:vma-start-write-boxed              vma)
          (hw:vma-start-write-no-gc-trap-boxed   vma))
      (if (= gc-trap vinc:$$gc-write-test)
          (hw:vma-start-write-unboxed            vma)
          (hw:vma-start-write-no-gc-trap-unboxed vma))))

(defconstant $$read-no-cdr 0)
(defconstant $$read-cdr    1)

(defun vma-start-read-generic (vma-boxed md-boxed cdr trans-type location)
  (if (= vma-boxed hw:$$boxed)
      (if (= md-boxed hw:$$boxed)
          (if (= cdr $$read-no-cdr)
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read              location :boxed :boxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-no-transport location :boxed :boxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-visible-evcp location :boxed :boxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-will-write   location :boxed :boxed)))
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read-cdr              location :boxed :boxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-cdr-no-transport location :boxed :boxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-cdr-visible-evcp location :boxed :boxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-cdr-will-write   location :boxed :boxed))))
          (if (= cdr $$read-no-cdr)
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read              location :boxed :unboxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-no-transport location :boxed :unboxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-visible-evcp location :boxed :unboxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-will-write   location :boxed :unboxed)))
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read-cdr              location :boxed :unboxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-cdr-no-transport location :boxed :unboxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-cdr-visible-evcp location :boxed :unboxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-cdr-will-write   location :boxed :unboxed)))))
      (if (= md-boxed hw:$$boxed)
          (if (= cdr $$read-no-cdr)
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read              location :unboxed :boxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-no-transport location :unboxed :boxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-visible-evcp location :unboxed :boxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-will-write   location :unboxed :boxed)))
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read-cdr              location :unboxed :boxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-cdr-no-transport location :unboxed :boxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-cdr-visible-evcp location :unboxed :boxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-cdr-will-write   location :unboxed :boxed))))
          (if (= cdr $$read-no-cdr)
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read              location :unboxed :unboxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-no-transport location :unboxed :unboxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-visible-evcp location :unboxed :unboxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-will-write   location :unboxed :unboxed)))
              (cond ((= trans-type vinc:$$transport-type-transport)
                     (hw:vma-start-read-cdr              location :unboxed :unboxed))
                    ((= trans-type vinc:$$transport-type-no-transport)
                     (hw:vma-start-read-cdr-no-transport location :unboxed :unboxed))
                    ((= trans-type vinc:$$transport-type-visible-evcp)
                     (hw:vma-start-read-cdr-visible-evcp location :unboxed :unboxed))
                    ((= trans-type vinc:$$transport-type-write)
                     (hw:vma-start-read-cdr-will-write   location :unboxed :unboxed)))))))
