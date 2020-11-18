;;; -*- Mode:LISP; Package:ZWEI; Base:10 -*-

(defmacro install-command-and-advertise (command char announcement &optional (comtab zwei:*zmacs-comtab*))
  ;; Install and notify.
  `(progn
     (zwei:command-store ,command ,char ,comtab)
     (format t "~&~C now ~A." ,char ,announcement)))


;;;Use as follows
;(install-command-and-advertise
;       'com-refind-file
;       #/Control-Meta-Hyper-Super-Shift-R
;       "refinds files from disk as needed")
