;;; -*- Mode:LISP; Package:LAMBDA; Base:8; Readtable:ZL -*-

(defvar saved-pages nil)

(defun save-page (n)
  (let ((copy-buffer-origin #o41000)
        (array (make-array #o400)))
    (dotimes (adr (array-length array))
      (aset (phys-mem-read (+ copy-buffer-origin (* n #o400) adr)) array adr))
    (setq saved-pages (append saved-pages (list array)))) )
