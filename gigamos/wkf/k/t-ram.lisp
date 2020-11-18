;;; -*- Mode:LISP; Package:TRANSPORTER-RAM; Base:10; Readtable:CL -*-

(defun load-transporter-ram (data-vector-origin)
  ;this is actually called during initialization.
  ;it loads the whole thing from a vector in memory.
  "  (on the K side:) LOAD-TRANSPORTER-RAM is called by EVENT-HORIZON which is called by COLD-BOOT-FUNCTION.
     (on the  side:) MEGA-BOOT calls K-BOOT calls PSEUDO-BOOT which tells the K to HW:JUMP to COLD-BOOT-FUNCTION.

     LOAD-TRANSPORTER-RAM reads the transporter ram data which was built by LOAD-BOOT-TRANSPORTER-RAM-DATA.
     (MAKE-COLD-LOAD calls COLD-LOAD-DATA calls COLD-TRANSPORTER-RAM-DATA calls LOAD-BOOT-TRANSPORTER-RAM-DATA)"

 (trap::without-traps
    #'(lambda ()
        (dotimes (trans-mode *number-of-transporter-modes*)
          (dotimes (trans-type *number-of-transport-types*)
            (dotimes (md-byte *number-of-transporter-md-byte-values*)
              (let ((transporter-data
                      (read-transporter-ram-data-from-memory md-byte trans-type trans-mode data-vector-origin)))
                (dotimes (vma-boxed 2.)
                  (dotimes (md-boxed 2.)
                    (write-transporter-ram vma-boxed md-boxed trans-type trans-mode md-byte
                      (hw:ldb transporter-data (byte 4. (vinc::dpb-multiple-unboxed
                                                          vma-boxed (byte 1. 2.)
                                                          md-boxed  (byte 1. 3.)
                                                          0))
                              0))
                    (li:break)))))))
        ;;; Clear out any traps.
        (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed 0.)
        (hw:read-md)
        nil)))
