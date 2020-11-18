;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by GJC
;;; Reason:
;;;
;;; Written 5-Dec-85 14:41:30 by GJC of LMI Cambridge
;;; while running on Guinea Pig from band 1
;;; with Experimental System 109.31, Experimental Local-File 64.1, Experimental FILE-Server 17.1, Experimental MagTape 3.4, Experimental KERMIT 28.0, microcode 1312, GC5 FS LAM.


;; *** Note: ***
;;   You may lose because the buffer has no readtable attribute.
;; *************

; From file DJ: L.SYS; IOMSG.LISP#28 at 5-Dec-85 14:41:51
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: SYS; IOMSG  "

;;; iomsg structure as defined in iomsg.h; number is the index into the iomsg array (art-16b)
;;; 0       done
;;; 1       channel
;;; 2       fcode
;;; 3       errcode
;;; 4, 5    count
;;; 6, 7    value
;;; 8, 9    offset
;;; 10, 11  buffer
;;; 12, 13  wakeup


;; in order to use the new %wire-structure I have used GET-IO-CMD to get a wireable
;; structure. There is some minor runtime inefficiency in this. -gjc

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
             (%nubus-write hi-8-bits lo-24-bits 1))
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

))
