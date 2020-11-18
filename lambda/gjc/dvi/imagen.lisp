;;;  -*- Mode:LISP; Package:TCP-APPLICATION; Base:10; Fonts:(CPTFONTB); Readtable:ZL -*-


;;; this will save a file with the minimal information
;;; about the bitmap. <HSIZE><VSIZE><128*HSIZE*VSIZE bytes of data>
;;;

(defun (:imagen-file si:print-bit-array) (PRINTER ARRAY LEFT TOP RIGHT BOTTOM &rest ignore)
  (let ((pathname (fs:parse-pathname (cadr printer))))
    (with-open-stream (stream (cond ((eq :lispm (send pathname :system-type))
                                     (open pathname :direction :output))
                                    ('else
                                     (open pathname :direction :output :raw t))))
      (when (null *imagen-data-bytes*)
        (setq *imagen-data-bytes* (make-array (// (* 1024 1024) 8)
                                              :type 'art-string)))
      (setup-imagen-data-bytes array left top (- right left) (- bottom top))
      (send stream :tyo (nth 0 *imagen-data-size*))
      (send stream :tyo (nth 1 *imagen-data-size*))
      (send stream :string-out *imagen-data-bytes*
            0 (* 128 (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*)))
      (setq *imagen-data-size* nil))))
