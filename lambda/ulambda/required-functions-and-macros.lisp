;;; -*- Mode:Lisp; Package:Lambda; Base:8 -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;


(DEFVAR *LAMBDA-PACKAGE* (PKG-FIND-PACKAGE "LAMBDA"))

(defun compute-parity-11 (data &optional (force-error 0))
  (DO ((data (logand 1777 data))
       (PARITY-BIT 1)
       (BITNUM 0 (1+ BITNUM)))
      ((> BITNUM 9.)
       (dpb (logxor force-error parity-bit)
            (byte 1 10.)
            data))
    (setq parity-bit
          (logxor parity-bit
                  (ldb (byte 1 bitnum) data)))))

(defun compute-parity-32 (data &optional (force-error-mask 0))
  (do ((data (logand 1777777777 data))          ;only look at low 28 bits
       (parity-width 4)
;       (parity-bits 0)
       (parity-bits 17)
       (group 0 (+ group 4)))
      ((= group 28.)
       (DPB (logxor force-error-mask parity-bits)
            3404
            data))
    (setq parity-bits
          (logxor parity-bits
                  (ldb (byte parity-width group) data)) ;extract a group from the data
          )))

(defun compute-parity-for-ireg (data)
  (ecase (send *proc* :proc-type)
    (:explorer (compute-raven-cram-parity data))
    (:lambda
     (cond ((< (send *proc* :major-version) 100.)
            (compute-parity-64 data))
           (t
            data)))))

(defun compute-parity-64 (data &optional (force-error-mask 0))
  (do ((data (logand 77777777777777777777 data))                ;only look at low 60 bits
       (parity-width 4)
;       (parity-bits 0)
       (parity-bits 17)
       (group 0 (+ group 4)))
      ((= group 60.)
       (DPB (logxor force-error-mask parity-bits)
            7404
            data))
    (setq parity-bits
          (logxor parity-bits
                  (ldb (byte parity-width group) data)) ;extract a group from the data
          )))

(defun compute-parity-for-ireg (data &optional (force-error-mask 0))
  (compute-parity-64 data force-error-mask)
  )

;like LDB, but can load fields bigger than fixnum size.
(DEFUN LDB-BIG (FLD WD)
  (PROG (ANS BITS BITS-OVER SHIFT)
        (SETQ SHIFT 0 ANS 0 BITS (LDB 0006 FLD) BITS-OVER (LDB 0620 FLD))
    L   (SETQ ANS (LOGIOR ANS (ASH (LDB (DPB BITS-OVER 0620 (MIN BITS 23.)) WD) SHIFT)))
        (IF ( (SETQ BITS (- BITS 23.)) 0) (RETURN ANS))
        (SETQ SHIFT (+ SHIFT 23.)
              BITS-OVER (+ BITS-OVER 23.))
        (GO L)))

(DEFUN DPB-BIG (QUAN FLD WD)
  (PROG (ANS BITS BITS-OVER Q)
        (SETQ ANS WD BITS (LDB 0006 FLD) BITS-OVER (LDB 0620 FLD) Q QUAN)
    L   (SETQ ANS (DPB Q (DPB BITS-OVER 0620 (MIN BITS 23.)) ANS))
        (IF ( (SETQ BITS (- BITS 23.)) 0) (RETURN ANS))
        (SETQ Q (ASH Q -23.)
              BITS-OVER (+ BITS-OVER 23.))
        (GO L)))
