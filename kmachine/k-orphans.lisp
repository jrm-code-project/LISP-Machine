;;; -*- Mode:LISP; Package:VINCULUM; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get physical memory size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *16-meg-in-clusters* 1024.)
(defconstant *32-meg-in-clusters* 2048.)

(defun get-physical-memory-size ()
  (if (hw:32= (hw:ldb (hw:read-memory-status) hw:%%memory-status-16meg 0.) hw:$$16meg-or-less)
      *16-meg-in-clusters*
      *32-meg-in-clusters*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VMA START READ GENERIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant $$read-no-cdr 0)
(defconstant $$read-cdr    1)

(defun vma-start-read-generic (vma-boxed md-boxed cdr trans-type location)
  (dispatch (byte 1. 0.) vma-boxed
    (hw:$$boxed
      (dispatch (byte 1. 0.) md-boxed
        (hw:$$boxed
          (dispatch (byte 1. 0.) cdr
            ($$read-no-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-will-write   location))))
            ($$read-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read-cdr              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-cdr-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-cdr-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-cdr-will-write   location))))))
        (hw:$$unboxed
          (dispatch (byte 1. 0.) cdr
            ($$read-no-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read-md-unboxed              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-md-unboxed-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-md-unboxed-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-md-unboxed-will-write   location))))
            ($$read-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read-md-unboxed-cdr              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-md-unboxed-cdr-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-md-unboxed-cdr-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-md-unboxed-cdr-will-write   location))))))))
    (hw:$$unboxed
      (dispatch (byte 1. 0.) md-boxed
        (hw:$$boxed
          (dispatch (byte 1. 0.) cdr
            ($$read-no-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read-unboxed              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-unboxed-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-unboxed-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-unboxed-will-write   location))))
            ($$read-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read-unboxed-cdr              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-unboxed-cdr-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-unboxed-cdr-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-unboxed-cdr-will-write   location))))))
        (hw:$$unboxed
          (dispatch (byte 1. 0.) cdr
            ($$read-no-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read-unboxed-md-unboxed              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-unboxed-md-unboxed-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-unboxed-md-unboxed-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-unboxed-md-unboxed-will-write   location))))
            ($$read-cdr
              (dispatch (byte 2. 0.) trans-type
                ($$transport-type-transport
                  (hw:vma-start-read-unboxed-md-unboxed-cdr              location))
                ($$transport-type-no-transport
                  (hw:vma-start-read-unboxed-md-unboxed-cdr-no-transport location))
                ($$transport-type-visible-evcp
                  (hw:vma-start-read-unboxed-md-unboxed-cdr-visible-evcp location))
                ($$transport-type-write
                  (hw:vma-start-read-unboxed-md-unboxed-cdr-will-write   location))
                ))))))))
