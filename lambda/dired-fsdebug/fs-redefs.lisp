;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:10 -*-


(defun MAKE-SCROLLABLE-MAP-DESCRIPTION (file &aux map (item-list (list nil)))
  (setq map (file-map file))
  (nconc item-list (make-item (format nil "~&~S maps out the following blocks:" map)))
  (nconc item-list (make-item (format nil "~2@TIndex~3@TLocation~7@TSize~3@TStatus")))
  (let ((nbits 0))
    (dotimes (i (map-nblocks map))
      (nconc
        item-list
        (make-item (format nil "~6O: ~10O ~10O   ~[Free~;Reserved~;Used~;Bad~]"
                i (map-block-location map i) (map-block-size map i)
                (aref page-usage-table (map-block-location map i)))))
      (incf nbits (map-block-size map i)))
    (nconc
      item-list
      (make-item (format nil "Total of ~D block~:P, ~D bit~:P."
              (map-nblocks map)
              nbits))))
  item-list)


(defun MAKE-ITEM (string)
  (list (tv:scroll-parse-item string)))


(defun SAFE-PATHNAME-EQUAL (path1 path2)
  (and (typep path1 'lm-pathname)
       (typep path2 'lm-pathname)
       (pathname-equal path1 path2)))


;(DEFUN DESCRIBE-MAP (MAP &OPTIONAL (STREAM STANDARD-OUTPUT))
;  (FORMAT STREAM "~&~S maps out the following blocks:~@
;~2@TIndex~3@TLocation~7@TSize~3@TStatus~%" MAP)
;  (LET ((NBITS 0))
;    (DOTIMES (I (MAP-NBLOCKS MAP))
;      (FORMAT STREAM "~6O: ~10O ~10O   ~[Free~;Reserved~;Used~;Bad~]~%"
;             I (MAP-BLOCK-LOCATION MAP I) (MAP-BLOCK-SIZE MAP I)
;             (setq *dbg-status* (AREF PAGE-USAGE-TABLE (MAP-BLOCK-LOCATION MAP I))))
;      (INCF NBITS (MAP-BLOCK-SIZE MAP I)))
;    (FORMAT STREAM "~%Total of ~D block~:P, ~D bit~:P.~%"
;           (MAP-NBLOCKS MAP)
;           NBITS)))

;(DEFUN DESCRIBE-MAP (MAP &OPTIONAL (STREAM STANDARD-OUTPUT))
;  (FORMAT STREAM "~&~S maps out the following blocks:~@
;~2@TIndex~3@TLocation~7@TSize~3@TStatus~%" MAP)
;  (LET ((NBITS 0))
;    (DOTIMES (I (MAP-NBLOCKS MAP))
;      (FORMAT STREAM "~6O: ~10O ~10O   ~[Free~;Reserved~;Used~;Bad~]~%"
;             I (MAP-BLOCK-LOCATION MAP I) (MAP-BLOCK-SIZE MAP I)
;             (AREF PAGE-USAGE-TABLE (MAP-BLOCK-LOCATION MAP I)))
;      (INCF NBITS (MAP-BLOCK-SIZE MAP I)))

;    (FORMAT STREAM "~%Total of ~D block~:P, ~D bit~:P.~%"
;           (MAP-NBLOCKS MAP)
;           NBITS)))
