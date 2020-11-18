;;; -*- Mode:LISP; Package:TV; Readtable:ZL; Base:10 -*-


(defun enable-macsyma-ints ()
  (setf (getf (io-buffer-plist (send terminal-io :io-buffer))
              :ASYNCHRONOUS-CHARACTERS)
        (append '((#\control-] KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER
                   (:NAME "Error Break" :PRIORITY 40.)
                   KBD-MACSYMA-TIMESOFAR))
                KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)))


(defun kbd-macsyma-timesofar (char &rest ignore)
  char
  (format t "~&[~S ~A]~%" si:user-id si:local-host))
