;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Patch-File:T; Base:8; Readtable:ZL -*-
;;; Private patches made by rg
;;; Reason:
;;;  to test new IOMSG usage
;;; Written 4-Jan-86 22:00:07 by rg of LMI Cambridge
;;; while running on Guinea Pig from band 2
;;; with Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 109.62, Experimental Local-File 64.1, Experimental FILE-Server 17.1, Experimental MagTape 3.4, microcode 1311, GC5 FS LAM.



; From file DJ: L.SYS; IOMSG.LISP#32 at 4-Jan-86 22:00:38
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; IOMSG  "


(defstruct (iomsg named)
  iomsg-lock
  iomsg-io-cmd)

(defsubst iomsg-array (iomsg)
  (io-cmd-buffer (iomsg-io-cmd iomsg)))

(defmacro with-iomsg-locked ((iomsg) &body body)
  `(with-lock ((iomsg-lock ,iomsg) :norecursive)
     ,@body))

(defun wire-iomsg-and-get-nubus-address (iomsg)
  (%wire-structure (iomsg-io-cmd iomsg))
  (vadr-to-nubus-phys (io-cmd-first-data-address (iomsg-io-cmd iomsg))))

;;;macros and setf things for the iomsg structure
(defmacro iomsg-done (iomsg)
  `(ldb 0010 (aref (iomsg-array ,iomsg) 0)))
(defsetf iomsg-done (iomsg) (val)
  `(aset (dpb ,val 0010 (aref (iomsg-array ,iomsg) 0)) (iomsg-array ,iomsg) 0))

(defmacro iomsg-dummy (iomsg)
  `(ldb 1010 (aref (iomsg-array ,iomsg) 0)))
(defsetf iomsg-dummy (iomsg) (val)
  `(aset (dpb ,val 1010 (aref (iomsg-array ,iomsg) 0)) (iomsg-array ,iomsg) 0))

(defmacro iomsg-channel (iomsg)
  `(aref (iomsg-array ,iomsg) 1))

(defmacro iomsg-fcode (iomsg)
  `(aref (iomsg-array ,iomsg) 2))

(defmacro iomsg-errcode (iomsg)
  `(aref (iomsg-array ,iomsg) 3))

(defmacro iomsg-count (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 5)
        2020
        (aref (iomsg-array ,iomsg) 4)))
(defsetf iomsg-count (iomsg) (val)
  `(progn
     (aset (ldb 0020 ,val) (iomsg-array ,iomsg) 4)
     (aset (ldb 2020 ,val) (iomsg-array ,iomsg) 5)))

(defmacro iomsg-value (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 7)
        2020
        (aref (iomsg-array ,iomsg) 6)))
(defsetf iomsg-value (iomsg) (val)
  `(progn
     (aset (ldb 0020 ,val) (iomsg-array ,iomsg) 6)
     (aset (ldb 2020 ,val) (iomsg-array ,iomsg) 7)))

;;;a.k.a. vecnum
(defmacro iomsg-offset (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 11)
        2020
        (aref (iomsg-array ,iomsg) 10)))
(defsetf iomsg-offset (iomsg) (val)
  `(progn
     (aset (ldb 0020 ,val) (iomsg-array ,iomsg) 10)
     (aset (ldb 2020 ,val) (iomsg-array ,iomsg) 11)))

;;;a.k.n. intaddr
(defmacro iomsg-buffer (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 13)
        2020
        (aref (iomsg-array ,iomsg) 12)))
(defsetf iomsg-buffer (iomsg) (val)
  `(progn
     (aset (ldb 0020 ,val) (iomsg-array ,iomsg) 12)
     (aset (ldb 2020 ,val) (iomsg-array ,iomsg) 13)))

(defmacro iomsg-wakeup (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 15)
        2020
        (aref (iomsg-array ,iomsg) 14)))
(defsetf iomsg-wakeup (iomsg) (val)
  `(progn
     (aset (ldb 0020 ,val) (iomsg-array ,iomsg) 14)
     (aset (ldb 2020 ,val) (iomsg-array ,iomsg) 15)))



(defun wait-for-iomsg-done (iomsg)
  (process-wait "iomsg" #'(lambda (x) (not (zerop (iomsg-done x)))) iomsg))

(defun execute-iomsg (iomsg)
  (with-ioport (standard-ioport)
    ;;; now write the pointer to the message
    (setf (ioport-buffer standard-ioport) (wire-iomsg-and-get-nubus-address iomsg))
    (setf (ioport-valid standard-ioport) 200)
    ;;; now send an interrupt if the vector is non-zero
    (let ((hi-8-bits (ioport-vector-hi-8 standard-ioport))
          (lo-24-bits (ioport-vector-lo-24 standard-ioport)))
      (cond ((or (= hi-8-bits #xff) (= hi-8-bits #xef))
             (%nubus-write-8 hi-8-bits lo-24-bits 1))
            ((and (zerop hi-8-bits)
                  (zerop lo-24-bits)))
            (t
             (ferror nil "bad interrupt address #x~16r~16r" hi-8-bits lo-24-bits))))
    (wait-for-iomsg-done iomsg)))

;;;vecnum is an alias for offset; intadr is an alias of buffer
(defun send-iomsg (iomsg &key fcode count &optional offset vecnum buffer intadr)
  (declare (:values value errcode count))
  (fill (iomsg-array iomsg) 0)
  (cond ((and offset vecnum)
         (ferror nil "vecnum and offset can't be supplied together")))
  (cond ((and buffer intadr)
         (ferror nil "buffer and intadr can't be supplied together")))
  (if (null offset)
      (setq offset vecnum))
  (if (null buffer)
      (setq buffer intadr))
  (with-iomsg-locked (iomsg)
    (setf (iomsg-wakeup iomsg) 0)
    (setf (iomsg-errcode iomsg) 0)
    (setf (iomsg-fcode iomsg) fcode)
    (setf (iomsg-count iomsg) count)
    (setf (iomsg-buffer iomsg) (or buffer 0))
    (setf (iomsg-offset iomsg) (or offset 0))
    (setf (iomsg-value iomsg) 123)
    (execute-iomsg iomsg)
    (values (iomsg-value iomsg) (iomsg-errcode iomsg) (iomsg-count iomsg))))

(defconst standard-iomsg (make-iomsg iomsg-io-cmd (create-io-cmd nil 1)))

(defun print-iomsg  (&optional (iomsg standard-iomsg))
  (FORMAT T "~&IOMSG at nubus address: #x~16r" (wire-iomsg-and-get-nubus-address iomsg))
  (format t "~&Done~20t #x~16r" (iomsg-done iomsg))
  (format t "~&Dummy~20t #x~16r" (iomsg-dummy iomsg))
  (format t "~&Channel~20t #x~16r" (iomsg-channel iomsg))
  (format t "~&Fcode~20t #x~16r (~:*~d.)" (iomsg-fcode iomsg))
  (format t "~&Errcode~20t #x~16r" (iomsg-errcode iomsg))
  (format t "~&Count~20t ~d. (~:*#x~16r)" (iomsg-count iomsg))
  (format t "~&Value~20t ~d. (~:*#x~16r)" (iomsg-value iomsg))
  (format t "~&Offset (vecnum)~20t ~d. (~:*#x~16r)" (iomsg-offset iomsg))
  (format t "~&Buffer (intaddr)~20t #x~16r" (iomsg-buffer iomsg))
  (format t "~&wakeup~20t #x~16r" (iomsg-wakeup iomsg)))


(defun passint (int-number nubus-address)
  (cond ((not (zerop (send-iomsg standard-iomsg
                                 ':fcode 12.
                                 ':count 0
                                 ':vecnum int-number
                                 ':intadr nubus-address)))
         (ferror nil "passint failed"))))

(defun old-set-up-lambda-interrupts ()
  (passint 12 36000002450)
  (passint 13 36000002454))

(defun lambda-interrupt-number-to-nubus-address (int-number for-board)
  (dpb (dpb (ldb 0404 for-board) 0404 rg-quad-slot)
       3010
       (dpb (+ 400 int-number) 0212 0)))

(defun set-up-serial-b-interrupts ()
  (passint 12 (lambda-interrupt-number-to-nubus-address 112 sdu-quad-slot))
  (passint 13 (lambda-interrupt-number-to-nubus-address 113 sdu-quad-slot)))


))

; From file DJ: L.SYS; SHARED-DEVICE.LISP#31 at 4-Jan-86 22:02:53
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; SHARED-DEVICE  "

(defmethod (sdu-serial-b-shared-device :after-allocation) ()
  ;;(new-set-up-serial-b-interrupts)
  ;; old way using IOMSG
  (set-up-serial-b-interrupts)
  )

))
