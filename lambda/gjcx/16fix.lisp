;;; -*- Mode:LISP; Package:USER; Base:10; Fonts:(CPTFONTB); Readtable:ZL -*-

(defun 16fix-file (filename)
  "This fixes some 16b files that have been improperly restored from tape"
  (with-open-file (stream filename
                          :direction :input
                          :byte-size 16
                          :characters nil)
    (with-open-file (o (send (send stream :truename)
                             :new-version :newest)
                       :direction :output
                       :byte-size 16
                       :characters nil)
      (do ((c))
          ((null (setq c (send stream :tyi))))
        (send o :tyo (dpb (send stream :tyi) (byte 8 8) c))))))
