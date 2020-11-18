;;; -*- Mode:LISP; Package:METER; Base:8; Readtable:CL -*-

(defvar *page-trace-buffer* (si:make-wireable-array 128. 'art-32b nil))

(defun turn-on-page-trace ()
  (si:%wire-structure *page-trace-buffer*)
  (si:%page-trace *page-trace-buffer*))

(defun turn-off-page-trace ()
  (si:%page-trace nil)
  (si:%unwire-structure *page-trace-buffer*))

;       Microsecond clock value
;       Virtual address
;       Miscellany:
;        bit 31: swap-out flag,
;        bit 30: stack-group-switch flag
;        bit 29: transport flag
;        bit 28: scavenge flag
;        bits 15-0: micro-pc
;       Current function (just randomly picks up @M-AP, hopefully reasonable)

(defun print-page-trace ()
  (labels ((aref-32 (array index)
             (let ((data-offset (+ index (si:array-data-offset array))))
               (dpb (%p-ldb-offset (byte 16. 16.) array data-offset)
                    (byte 16. 16.)
                    (%p-ldb-offset (byte 16. 0) array data-offset)))))
    (do ((i 0 (+ i 4)))
        ((= i (array-length *page-trace-buffer*)))
      (format t "~&~10d ~15o ~15o ~15o"
              (aref-32 *page-trace-buffer* (* i 4))
              (aref-32 *page-trace-buffer* (+ 4 (* i 4)))
              (aref-32 *page-trace-buffer* (+ 8 (* i 4)))
              (aref-32 *page-trace-buffer* (+ 12. (* i 4)))))))
