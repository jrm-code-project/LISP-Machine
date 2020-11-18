;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic paging devices
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(li:defstruct (paging-device :named (:conc-name "PAGING-DEVICE-"))
  status-generator

  opener
  closer

  quantum-allocator
  quantum-deallocator

  reader-initializer
  next-cluster-reader
  reader-activate

  writer-initializer
  next-cluster-writer
  writer-activate

  operation-completer)

(defmacro define-generic-paging-device-operation (name args accessor)
  `(PROGN (DEFUN ,name ,args (FUNCALL (,accessor ,(car args)) ,@args))
          (EXPORT '(,name))))

(define-generic-paging-device-operation paging-device-status (device) paging-device-status-generator)

(define-generic-paging-device-operation open-paging-device   (device) paging-device-opener)
(define-generic-paging-device-operation close-paging-device  (device) paging-device-closer)

(define-generic-paging-device-operation allocate-quantum     (device) paging-device-quantum-allocator)
(define-generic-paging-device-operation deallocate-quantum   (device dqin) paging-device-quantum-deallocator)

(define-generic-paging-device-operation begin-read
                                        (device dqin relative-cluster-in-quantum)
  paging-device-reader-initializer)

(define-generic-paging-device-operation next-read-cluster
                                        (device physical-cluster-number)
  paging-device-next-cluster-reader)

(define-generic-paging-device-operation do-the-read (device)
  paging-device-reader-activate)


(define-generic-paging-device-operation begin-write
                                        (device dqin relative-cluster-in-quantum)
  paging-device-writer-initializer)

(define-generic-paging-device-operation next-write-cluster
                                        (device physical-cluster-number)
  paging-device-next-cluster-writer)

(define-generic-paging-device-operation do-the-write (device)
  paging-device-writer-activate)


(define-generic-paging-device-operation operation-complete (device)
  paging-device-operation-completer)

;;; Paging devices 12/10/87

(defvar *paging-devices* nil)

(defun make-paging-devices (&optional (number-of-devices 16.))
  (setq *paging-devices* (array:make-1d-array number-of-devices))
  )

(defvar *null-paging-device*         0)
(defvar *boot-band-paging-device-id* 1)
(defvar *page-band-paging-device-id* 2)
