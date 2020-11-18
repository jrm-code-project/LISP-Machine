;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; readtable: ZL -*-

;;; This is an inner-system-file, so we are being careful about the
;;; form of the load-time-eval's. in particular, not evaluating hairy macros
;;; at load time.

;;; Copyright (c) 1984
;;; Lisp Machine, Inc.

;;; IOMSG

;;; The C definitions of the data structures are in /usr/include/sys/iomsg.h

(defstruct (ioport (:type :named-array))
  ioport-multibus-address)

(defvar standard-ioport nil)

(defmacro ioport-busy (ioport)
  `(%multibus-read-8 (ioport-multibus-address ,ioport)))
(defsetf ioport-busy (ioport) (val)
  `(%multibus-write-8 (ioport-multibus-address ,ioport) ,val))

(defmacro ioport-valid (ioport)
  `(%multibus-read-8 (1+ (ioport-multibus-address ,ioport))))
(defsetf ioport-valid (ioport) (val)
  `(%multibus-write-8 (1+ (ioport-multibus-address ,ioport)) ,val))


(defmacro ioport-buffer (ioport)
  `(let ((base-adr (+ 4 (ioport-multibus-address ,ioport))))
     (dpb (%multibus-read-8 (+ 3 base-adr))
          (BYTE 8. 24.)
          (dpb (%multibus-read-8 (+ 2 base-adr))
               (BYTE 8. 16.)
               (dpb (%multibus-read-8 (+ 1 base-adr))
                    (BYTE 8. 8.)
                    (%multibus-read-8 base-adr))))))

(defsetf ioport-buffer (ioport) (val)
  `(let ((base-adr (+ 4 (ioport-multibus-address ,ioport))))
     (%multibus-write-8 base-adr       (ldb (BYTE 8. 0.) ,val))
     (%multibus-write-8 (+ base-adr 1) (ldb (BYTE 8. 8.) ,val))
     (%multibus-write-8 (+ base-adr 2) (ldb (BYTE 8. 16.) ,val))
     (%multibus-write-8 (+ base-adr 3) (ldb (BYTE 8. 24.) ,val))))

(defmacro ioport-vector (ioport)
  `(let ((base-adr (+ 8 (ioport-multibus-address ,ioport))))
     (dpb (%multibus-read-8 (+ 3 base-adr))
          (BYTE 8. 24.)
          (dpb (%multibus-read-8 (+ 2 base-adr))
               (BYTE 8. 16.)
               (dpb (%multibus-read-8 (+ 1 base-adr))
                    (BYTE 8. 8.)
                    (%multibus-read-8 base-adr))))))
(defsetf ioport-vector (ioport) (val)
  `(let ((base-adr (+ 8 (ioport-multibus-address ,ioport))))
     (%multibus-write-8 base-adr       (ldb (BYTE 8. 0.) ,val))
     (%multibus-write-8 (+ base-adr 1) (ldb (BYTE 8. 8.) ,val))
     (%multibus-write-8 (+ base-adr 2) (ldb (BYTE 8. 16.) ,val))
     (%multibus-write-8 (+ base-adr 3) (ldb (BYTE 8. 24.) ,val))))
(defmacro ioport-vector-hi-8 (ioport)
  `(%multibus-read-8 (+ 3 8 (ioport-multibus-address ,ioport))))
(defmacro ioport-vector-lo-24 (ioport)
  `(let ((base-adr (+ 8 (ioport-multibus-address ,ioport))))
     (%logdpb (%multibus-read-8 (+ 2 base-adr))
              (BYTE 8. 16.)
              (dpb (%multibus-read-8 (+ 1 base-adr))
                   (BYTE 8. 8.)
                   (%multibus-read-8 base-adr)))))

(defun print-ioport (&optional (ioport standard-ioport))
  (format t "~&busy~30t~o" (ioport-busy ioport))
  (format t "~&valid~30t~o" (ioport-valid ioport))
  (format t "~&buffer~30t~16r" (ioport-buffer ioport))
  (format t "~&vector~30t~16r" (ioport-vector ioport)))

(defun sieze-ioport (ioport)
  (process-wait "ioport" #'(lambda (x) (zerop (ioport-busy x))) ioport)
  (setf (ioport-busy ioport) 200))

(defun free-ioport (ioport)
  (setf (ioport-busy ioport) 0)
  (setf (ioport-valid ioport) 0))

(defmacro with-ioport ((ioport) &body body)
  `(let ((i-have-ioport))
     (unwind-protect
         (progn
           (sieze-ioport ,ioport)
           (setq i-have-ioport t)
           ,@body)
       (if i-have-ioport
           (free-ioport ,ioport)))))


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
  `(ldb (BYTE 8. 0.) (aref (iomsg-array ,iomsg) 0)))
(defsetf iomsg-done (iomsg) (val)
  `(aset (dpb ,val (BYTE 8. 0.) (aref (iomsg-array ,iomsg) 0)) (iomsg-array ,iomsg) 0))

(defmacro iomsg-dummy (iomsg)
  `(ldb (BYTE 8. 8.) (aref (iomsg-array ,iomsg) 0)))
(defsetf iomsg-dummy (iomsg) (val)
  `(aset (dpb ,val (BYTE 8. 8.) (aref (iomsg-array ,iomsg) 0)) (iomsg-array ,iomsg) 0))

(defmacro iomsg-channel (iomsg)
  `(aref (iomsg-array ,iomsg) 1))

(defmacro iomsg-fcode (iomsg)
  `(aref (iomsg-array ,iomsg) 2))

(defmacro iomsg-errcode (iomsg)
  `(aref (iomsg-array ,iomsg) 3))

(defmacro iomsg-count (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 5)
        (BYTE 16. 16.)
        (aref (iomsg-array ,iomsg) 4)))
(defsetf iomsg-count (iomsg) (val)
  `(progn
     (aset (ldb (BYTE 16. 0.) ,val) (iomsg-array ,iomsg) 4)
     (aset (ldb (BYTE 16. 16.) ,val) (iomsg-array ,iomsg) 5)))

(defmacro iomsg-value (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 7)
        (BYTE 16. 16.)
        (aref (iomsg-array ,iomsg) 6)))
(defsetf iomsg-value (iomsg) (val)
  `(progn
     (aset (ldb (BYTE 16. 0.) ,val) (iomsg-array ,iomsg) 6)
     (aset (ldb (BYTE 16. 16.) ,val) (iomsg-array ,iomsg) 7)))

;;;a.k.a. vecnum
(defmacro iomsg-offset (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 11)
        (BYTE 16. 16.)
        (aref (iomsg-array ,iomsg) 10)))
(defsetf iomsg-offset (iomsg) (val)
  `(progn
     (aset (ldb (BYTE 16. 0.) ,val) (iomsg-array ,iomsg) 10)
     (aset (ldb (BYTE 16. 16.) ,val) (iomsg-array ,iomsg) 11)))

;;;a.k.n. intaddr
(defmacro iomsg-buffer (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 13)
        (BYTE 16. 16.)
        (aref (iomsg-array ,iomsg) 12)))
(defsetf iomsg-buffer (iomsg) (val)
  `(progn
     (aset (ldb (BYTE 16. 0.) ,val) (iomsg-array ,iomsg) 12)
     (aset (ldb (BYTE 16. 16.) ,val) (iomsg-array ,iomsg) 13)))

(defmacro iomsg-wakeup (iomsg)
  `(dpb (aref (iomsg-array ,iomsg) 15)
        (BYTE 16. 16.)
        (aref (iomsg-array ,iomsg) 14)))
(defsetf iomsg-wakeup (iomsg) (val)
  `(progn
     (aset (ldb (BYTE 16. 0.) ,val) (iomsg-array ,iomsg) 14)
     (aset (ldb (BYTE 16. 16.) ,val) (iomsg-array ,iomsg) 15)))



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

(defconst standard-iomsg nil)

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
  (dpb (dpb (ldb (BYTE 4. 4.) for-board) (BYTE 4. 4.) rg-quad-slot)
       (BYTE 8. 24.)
       (dpb (+ 400 int-number) (BYTE 10. 2.) 0)))

(defun set-up-serial-b-interrupts ()
  (passint 12 (lambda-interrupt-number-to-nubus-address 112 sdu-quad-slot))
  (passint 13 (lambda-interrupt-number-to-nubus-address 113 sdu-quad-slot)))

(defun allocate-multibus-mapping-registers (n-pages)
  (let ((register (send-iomsg standard-iomsg
                              :fcode 11.
                              :count (* n-pages 1024.)
                              :buffer 0)))
    (when (zerop register)
      (ferror nil "can't allocate mapping register"))
    (ldb (byte 22. 10.) register)))

;-----
; New interrupt table

(defmacro aref-32 (array-16b q)
  `(dpb (aref ,array-16b (1+ (* 2 ,q)))
        (BYTE 16. 16.)
        (aref ,array-16b (* 2 ,q))))
(defsetf aref-32 (array-16b q) (val)
  `(progn
     (aset (ldb (BYTE 16. 0.) ,val) ,array-16b (* 2 ,q))
     (aset (ldb (BYTE 16. 16.) ,val) ,array-16b (1+ (* 2 ,q)))
     ,val))

(defvar intmaps)

(defun setup-intmaps ()
  (setq intmaps nil)
  (let ((intmap-base (aref-32 si:*sys-conf* %system-configuration-sdu-interrupt-map)))
    (cond ((not (zerop intmap-base))
           (cond ((not (= sdu-quad-slot (ldb (byte 8 24.) intmap-base)))
                  (ferror nil "can't handle intmap if not on SDU")))
           (cond ((not (zerop (ldb (byte 2 0) intmap-base)))
                  (ferror nil "intmap must be on word boundary")))

           (let ((offset (ldb (byte 24. 0) intmap-base)))
             (let ((size-in-words
                     (%nubus-read sdu-quad-slot (+ offset (* 4 %intmap-size-in-words)))))
               (dotimes (int-number 32.)
                 (let ((intmap (make-array (* size-in-words 2)
                                           :type :art-16b
                                           :displaced-to (+ multibus-virtual-address
                                                            (// offset 4)
                                                            (* int-number size-in-words))
                                           :leader-length 3
                                           :named-structure-symbol 'intmap)))
                   (setf (array-leader intmap 2) int-number)
                   (push intmap intmaps)))
               (setq intmaps (reverse intmaps))))))))

(defun multibus-pointer-to-nubus-address (multibus-pointer)
  (let* ((seg (ldb (byte 16. 16.) multibus-pointer))
         (offset (ldb (byte 16. 0) multibus-pointer))
         (multibus-address (+ (ash seg 4) offset))
         (multibus-page (ash multibus-address -10.))
         (map-entry (read-multibus-mapping-register multibus-page)))
    (cond ((zerop (ldb (byte 1 23.) map-entry))
           (dpb sdu-quad-slot (byte 8 24.) multibus-address))
          (t
           (dpb map-entry
                (byte 22. 10.)
                (ldb (byte 10. 0) multibus-address))))))

(defselect ((intmap named-structure-invoke))
  (:describe (struct)
    (format t "~&~S:" struct)
    (dolist (q intmap-qs)
      (format t "~&~s:~40t~s" q (funcall q struct))))
  (:print-self (struct stream ignore ignore)
    (printing-random-object (struct stream :typep)
      (select (aref-32 struct %intmap-type)
        (%intmap-type-none (format t "OFF "))
        (%intmap-type-sdu (format t "SDU #x~16r " (aref-32 struct %intmap-multibus-addr)))
        (%intmap-type-nubus (format t "NUBUS #x~16r "
                                    (multibus-pointer-to-nubus-address
                                      (aref-32 struct %intmap-multibus-addr)))))
      (format stream "~a" (nth (array-leader struct 2) sdu-interrupt-numbers))))
  (:which-operations (ignore)
    '(:describe :print-self :which-operations)))

(defun print-intmaps ()
  (dolist (intmap intmaps)
    (format t "~&~2d ~a:~30t"
            (array-leader intmap 2) (nth (array-leader intmap 2) sdu-interrupt-numbers))
    (select (aref-32 intmap %intmap-type)
      (%intmap-type-none (format t "OFF"))
      (%intmap-type-sdu (format t "SDU"))
      (%intmap-type-nubus (format t "NUBUS #x~16r" (multibus-pointer-to-nubus-address
                                                     (aref-32 intmap %intmap-multibus-addr)))))))

(defun setup-multibus-map-to-rg-board-interrupt-registers ()
  (let ((multibus-page (aref-32 si:*my-proc-conf* %processor-conf-intmap-multibus-map)))
    (cond ((zerop multibus-page)
           (ferror nil "sdu program too old")))
    (write-multibus-mapping-register
      multibus-page
      (dpb 1
           (byte 1 23.)
           (dpb rg-quad-slot (byte 8 14.) 1)))))

;0 means first interrupt location = rg-quad-slot + 1 page
(defun convert-lambda-interrupt-number-to-multibus-pointer (int-number)
  (let ((multibus-page (aref-32 si:*my-proc-conf* %processor-conf-intmap-multibus-map)))
    (let ((multibus-address (+ (ash multibus-page 10.) (* int-number 4))))
      (dpb (ldb (byte 16. 4) multibus-address)
           (byte 16. 16.)
           (ldb (byte 4 0) multibus-address)))))

(defun turn-off-sdu-interrupt (sdu-int-number)
  (let ((intmap (nth sdu-int-number intmaps)))
    (if (not (= sdu-int-number (array-leader intmap 2)))
        (ferror nil "lisp intmap structure invalid"))
    (setf (aref-32 intmap %intmap-type) %intmap-type-none)))

(defun forward-sdu-interrupt-to-lambda (sdu-int-number)
  (let ((rg-int-number (+ 100 -8 sdu-int-number)))
    (setup-multibus-map-to-rg-board-interrupt-registers)
    (let ((intmap (nth sdu-int-number intmaps)))
      (turn-off-sdu-interrupt sdu-int-number)
      (setf (aref-32 intmap %intmap-multibus-addr)
            (convert-lambda-interrupt-number-to-multibus-pointer rg-int-number))
      (setf (aref-32 intmap %intmap-type) %intmap-type-nubus))))

(defun new-set-up-serial-b-interrupts ()
  (forward-sdu-interrupt-to-lambda %sdu-port-b-rcv)
  (forward-sdu-interrupt-to-lambda %sdu-port-b-xmit))

(defun turn-off-ethernet-interrupt ()
  (turn-off-sdu-interrupt %sdu-m0-3com))

(defun turn-on-ethernet-interrupt ()
  (forward-sdu-interrupt-to-lambda %sdu-m0-3com))


(DEFUN IOMSG-INITIALIZATIONS ()
  (or standard-ioport (setq standard-ioport (make-ioport ioport-multibus-address #x14)))
  (or standard-iomsg (setq standard-iomsg (make-iomsg iomsg-io-cmd (create-io-cmd nil 1))))
  ())

(IOMSG-INITIALIZATIONS)
