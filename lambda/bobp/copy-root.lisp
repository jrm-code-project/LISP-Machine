;;; -*- Mode:LISP; Base:10 -*-

(defvar file-array (make-array (* 1000 1024) :type :art-8b))

(defvar read-count)

(defconst max-unix-block-number (* 20. 25. 800.))       ;heads * sectors * cylinders

(defun c-read-file-into-array (file ar &optional (size (array-length ar)))
  (with-open-file (s file :raw t)
    (do ((i 0 (1+ i))
         (ch (send s :tyi) (send s :tyi)))
        ((or (null ch)
             (= i size))
         i)
      (aset ch ar i)
      (setq read-count (1+ i)))))

(defun copy-sdu-root ()
  (get-sdu-part)
  (if (= read-count 1024000)
      (write-array-to-disk)))

(defun get-sdu-part ()
  (c-read-file-into-array "drac://dev//l//s205" file-array))

(defun write-array-to-disk ()
  (dotimes (i 1000)
    (let ((ba (make-array 1024                  ;i'th block of file-array
                         :type :art-8b
                         :displaced-to file-array
                         :displaced-index-offset (* i 1024))))
      (disk-write-block ba (+ 23 i)))))

(defconst disk-unit 0)
(defvar unix-rqb nil)

(defun disk-read-block (buf-array block-number)
  "read disk relative to cyl 0"
  (if (null unix-rqb)
      (setq unix-rqb (si:get-disk-rqb)))
  (disk-read-hook unix-rqb disk-unit block-number)
  (copy-array-contents (si:rqb-8-bit-buffer unix-rqb) buf-array))

(defun disk-read-hook (rqb unit block-number)
  "hook for si:disk-read-physical"
  (if (and (>= block-number 0)
           (< block-number max-unix-block-number))
      (si:disk-read-physical rqb unit block-number)
    (ferror nil "disk-read-hook called with block-number ~d." block-number)))

(defun disk-write-block (buf-array block-number)
  "write disk relative to cyl 0"
  (if (null unix-rqb)
      (setq unix-rqb (si:get-disk-rqb)))
  (copy-array-contents buf-array (si:rqb-8-bit-buffer unix-rqb))
  (disk-write-hook unix-rqb disk-unit block-number))

(defun disk-write-hook (rqb unit block-number)
  "hook for si:disk-write-physical"
  (if (and (>= block-number 0)
           (< block-number max-unix-block-number))
      (si:disk-write-physical rqb unit block-number)
    (ferror nil "disk-write-hook called with block-number ~d." block-number)))
