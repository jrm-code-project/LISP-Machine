;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defun reset-disk-meters ()
  (without-interrupts
    (dotimes (i 2)
      (si:write-meter 'si:%disk-wait-time 0)
      (si:write-meter 'si:%count-disk-page-read-operations 0)
      (si:write-meter 'si:%count-disk-page-write-operations 0))))

(defun disk-latency ()
  (let ((read-ops (si:read-meter 'si:%count-disk-page-read-operations))
        (write-ops (si:read-meter 'si:%count-disk-page-write-operations))
        (wait (si:read-meter 'si:%disk-wait-time)))
    (format t "~&~d reads, ~d writes, ~d microseconds" read-ops write-ops wait)
    (format t "~&~d milliseconds per op" (float (/ (/ wait (+ read-ops write-ops)) 1000.)))))
