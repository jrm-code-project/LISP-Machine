;;; -*- Mode:LISP; Package:VINCULUM; Readtable:CL; Base:10 -*-

;(export '(
  ;       *clusters-in-physical-block*
  ;       *clusters-in-quantum*
  ;       *blocks-of-physical-memory*
  ;       *number-of-quanta*
  ;       *number-of-virtual-clusters*
  ;       *physical-memory-max-clusters*
  ;       *qs-in-cluster*
  ;       *qs-in-quantum*

  ;       %%byte-position
  ;       %%byte-size
  ;       %%cluster-number
  ;       %%data-type
  ;       %%fixnum-field
  ;       %%fixnum-sign-bit
  ;       %%offset-in-cluster
  ;       %%pointer
  ;       %%quantum-number

  ;       $$dtc-both-character
  ;       $$dtc-both-fixnum
  ;       $$dtc-both-fixnum-with-overflow
  ;       $$dtc-hairy-number
  ;       $$dtc-none
  ;       $$dtc-right-array-and-left-structure
  ;       $$dtc-right-list
  ;       $$dtc-spare1

  ;       $$gc-write-test
  ;       $$no-gc-write-test
  ;       $$transporter-mode-normal
  ;       $$transport-type-no-transport
  ;       $$transport-type-transport
  ;       $$transport-type-visible-evcp
  ;       $$transport-type-write


  ;       field-maximum))

;;; This file has the software definitions that are firmly
;;; based on the hardware.  It isn't that the hardware
;;; determines these per se, but that the hardware was designed
;;; with these in mind.  It should be possible to change these
;;; without making ECO's, but it wasn't intended.

;;;;;;;;;;;;;;;;;;
;;; Field maximum
;;;;;;;;;;;;;;;;;;
(global:pkg-goto 'prims () global:*package*)

(global:proclaim '(lisp::declaration cool-for-constants))       ;was vinc:proclaim

(global:proclaim '(cool-for-constants vinc:field-maximum))      ;was vinc:proclaim

(global:pkg-goto 'vinc:vinc () global:*package*)

(global:defun field-maximum (byte-specifier)
  (lisp::expt 2. (byte-size byte-specifier)))

;;;;;;;;;;;;;;;;;;;;
;;; Physical memory
;;;;;;;;;;;;;;;;;;;;

;physical-block equals one megabyte.
(global:defconstant *clusters-in-physical-block* (field-maximum hw:%%physical-address-block-cluster-offset))
(global:defconstant *blocks-of-physical-memory*  (field-maximum hw:%%cluster-physical-address-block))

;;;;;;;;;;;;;;;;;;;
;;; Boxed Q Format
;;;;;;;;;;;;;;;;;;;

;;; Each boxed Q has a pointer field and a data field.
;;; The pointer field cannot extend beyond the mapped
;;; vma byte, and if it is made shorter, the map software
;;; must be made to fill both halves of the map.

;;; The data type cannot extend beyond the byte that
;;; is used to address the transporter ram in the MD.

(global:defconstant %%pointer   (byte 26. 0.))
(global:defconstant %%data-type hw:%%transporter-ram-md-byte)

;;;;;;;;;;;;;;;;;;;
;;; Pointer Format
;;;;;;;;;;;;;;;;;;;

;;; Since the bottom bits of the vma do not get
;;; mapped, the map has a granularity greater than
;;; one q.  Paging must be done on a granularity at
;;; least as large as this.  We choose to make it the
;;; same.

;;; The granularity is called a cluster.

(global:defconstant %%cluster-number    hw:%%mapped-vma-byte)    ;;(byte 16. 10.)
(global:defconstant %%offset-in-cluster hw:%%unmapped-vma-byte)  ;;(byte 10.  0.)

(global:defconstant *qs-in-cluster*              (field-maximum %%offset-in-cluster)) ;; 2^10.
(global:defconstant *number-of-virtual-clusters* (field-maximum %%cluster-number))    ;; 2^16.

;;; Since the bottom bits of the MD are not used in
;;; addressing the GC ram, the GC ram has a granularity
;;; greater than one q.  Garbage collection must be
;;; done on a granularity at least as large as this.  We
;;; choose to make it the same.

;;; This granularity is called a quantum.

(global:defconstant %%quantum-number hw:%%gc-ram-md-byte)

(global:defconstant *number-of-quanta*    (field-maximum %%quantum-number))
(global:defconstant *clusters-in-quantum* (lisp::expt 2. (lisp::- (byte-position %%quantum-number)
                                                     (byte-position %%cluster-number))))
(global:defconstant *qs-in-quantum*       (lisp::* *qs-in-cluster* *clusters-in-quantum*))

;;; The physical memory is limited by the number of wires
;;; coming out of the map.

(global:defconstant *physical-memory-max-clusters*  (field-maximum (byte 13. 0))        ; <17-Nov-88 RG>
                    ;storage allocation was set up for this and EVERYTHING breaks unless it is.
                    ;(field-maximum hw:%%map-on-board-address)  this is what is should be
  )

;;;;;;;;;;;;;;;;;;
;;; Fixnum format
;;;;;;;;;;;;;;;;;;

;;; Surprise, the fixnums are smaller than pointers.
;;; We are so lucky that this isn't a screw.

(global:defconstant %%fixnum-field             (byte 24. 0))

(global:defconstant %%fixnum-sign-bit          (byte 1 23.))
(global:defconstant %%fixnum-sign-and-datatype (byte 9 23.))
(global:defconstant %%fixnum-non-data          (byte 8 24.))

;;;;;;;;;;;;;;;;;;;
;;; Bignum format
;;;;;;;;;;;;;;;;;;;

(global:defconstant %%bignum-sign-high-word     (byte 1 31.))
(global:defconstant %%bignum-non-sign-high-word (byte 31. 0))
(global:defconstant %%bignum-words              (byte 19. 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Byte Specifier Format
;;;;;;;;;;;;;;;;;;;;;;;;;;

(global:defconstant %%byte-position hw:%%alu-status-internal-position-register) ;;(byte 8. 0)
(global:defconstant %%byte-size     hw:%%alu-status-internal-width-register)    ;;(byte 5. 8.)
(global:defconstant %%byte-position-non-fixnum (byte 17. 7.) "Used for testing if less than 127 and greater than -128")
   ;;Used for finding bits of a fixnum position unable to fit into a fixnum byte. (must be all zero or one)

;;;;;;;;;;;;;;;;;;
;;; GC Trap types
;;;;;;;;;;;;;;;;;;

(global:defconstant $$no-gc-write-test 0.)
(global:defconstant $$gc-write-test    1.)

;;;;;;;;;;;;;;;;;;;;;;
;;; Transporter types
;;;;;;;;;;;;;;;;;;;;;;

(global:defconstant $$transport-type-no-transport 0.)
(global:defconstant $$transport-type-transport    1.)
(global:defconstant $$transport-type-visible-evcp 2.)
(global:defconstant $$transport-type-write        3.)

;;;;;;;;;;;;;;;;;;;;;;
;;; Transporter modes
;;;;;;;;;;;;;;;;;;;;;;

(global:defconstant $$transporter-mode-normal 0.)
;;; three others unused

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Datatype check codes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global:defconstant $$dtc-none                    0.)
(global:defconstant $$dtc-spare1                          1.)
(global:defconstant $$dtc-hairy-number                    2.)
(global:defconstant $$dtc-both-character                  3.)
(global:defconstant $$dtc-right-array-and-left-structure 4.)
(global:defconstant $$dtc-right-list                      5.)
(global:defconstant $$dtc-both-fixnum                     6.)
(global:defconstant $$dtc-both-fixnum-with-overflow      7.)
