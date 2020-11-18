;;; -*- Mode:LISP; Base:10 -*-

(defun copy-p-to-dj ()
  (fs:balance-directories "drac://usr//bobp//p//*" "dj:bobp.p;*" :direction :1->2))
