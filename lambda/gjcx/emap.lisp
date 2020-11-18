;;; -*- Mode:LISP; Package:USER; Base:10 -*-


(defun describe-excelan-map ()
  (format t "~&MB ADDR => NB ADDR~%")
  (dotimes (j (si:%system-configuration-excelan-multibus-map-size si:*sys-conf*))
    (let* ((mb-page (+ (si:%system-configuration-excelan-base-multibus-map-block si:*sys-conf*)
                       j))
           (nb-page (ldb (byte 22 0) (si:read-multibus-mapping-register mb-page))))
      (format t "x~6,'0X => x~8,'0X~%" (* mb-page 1024) (* nb-page 1024)))))
