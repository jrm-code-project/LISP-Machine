;;; -*- Mode:LISP; Base:10 -*-

(defun count-idle (&aux last)
  (setq last (si:%system-configuration-newboot-idle-count si:*sys-conf*))
  (loop for x = (si:%system-configuration-newboot-idle-count si:*sys-conf*)
        (format t "~&~d" (- x last))
        (setq last x)
        (si:sleep 1)))
