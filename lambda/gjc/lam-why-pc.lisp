;;; -*- Mode:LISP; Package:LAMBDA; Base:10 -*-

(defun lam-why-pc (&aux ucode-version)
  (format t "~&Please enter the version number of the microcode the machine was running.~%")
  (setq ucode-version (or (prompt-and-read '(:number :input-radix 10. :or-nil t)
                                           "UCODE version (default ~D)> "
                                           si:%microcode-version-number)
                          si:%microcode-version-number))
  (or (lam-select-symbols-for-version-if-possible ucode-version)
      (lam-load-ucode-symbols-for-version ucode-version))
  (format t "Please enter the PC's printed by the 'why' program.
Entering -1 exits this program, <~:C> does a reprint.~%" #\Return)
  (do ((pc))
      ((and (setq pc (prompt-and-read '(:number :input-radix 8. :or-nil t)
                                       "~&PC>"))
            (minusp pc)))
    (and pc (cram-symbolic-address pc))))
