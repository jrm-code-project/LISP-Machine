; -*- mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

(defun unused-functions (&optional (modules si:loaded-modules)
                          (stream standard-output))
  (loop for module in modules
        for source = (find-module-source-file module)
        for names = (send module :select-nth 4)
        do (fs:process-in-load-environment
            (cdr (send source :file-plist))
            #'(lambda (foo names stream)
                (loop for i from 0 below (vector-length names)
                      for name = (vref names i)
                      if (null (who-calls (intern name)))
                      do (format stream "~&Not called: ~a" name)
                      do (format t "~&Not called: ~a" name)))
            source names stream)))

(defun unused-function-list (&optional (file "unused.lis")
                              (modules si:loaded-modules))
  (with-open-file (foo file :out)
    (unused-functions modules foo)
    (close foo)))

(defun module-from-pathname (pathname)
  (setq pathname (send (send (probe-file pathname) :new-type nil)
                       :new-version nil))
  (loop for module in si:loaded-modules
        for source = (send (send (pathname (si:module-loaded-file module))
                                 :new-type nil)
                           :new-version nil)
        if (send source :equal pathname) return module))

(defun unused-editor-functions (&optional (file "edused.lis"))
  (unused-function-list file
                        (delq nil (loop for f in *editor-files*
                                        collect (module-from-pathname f)))))
