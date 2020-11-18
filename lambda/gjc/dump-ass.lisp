;;; -*- Mode:LISP; Package:LAMBDA; Base:10 -*-

(defun dump-ass (&optional &key filename (processor :lambda))
  (or filename
      (setq filename (format nil "SYS:ULAMBDA;~A-MICROCODE QFASL >" processor)))
  (or (= (length *files-comprising-this-microcode*)
         (length (subset #'(lambda (x) (not (string-equal "UCODE" (send x :name))))
                         (si:system-source-files :lambda-ucode))))
      (ferror nil "inconsistent assembly state"))
  (let ((l *files-comprising-this-microcode*)
        (forms)
        (sexp-key (ecase processor
                    (:LAMBDA 'UA-LAMBDA-SEXP)
                    (:EXPLORER 'UA-EXPLORER-SEXP)))
        (defmic-key (ecase processor
                      (:LAMBDA 'LAMBDA-DEFMICS)
                      (:EXPLORER 'EXPLORER-DEFMICS))))
    (dolist (f l)
      (let ((sexp (send f :get sexp-key))
            (defmic (send f :get defmic-key)))
        (or sexp (ferror nil "File ~A has no ~S property" f sexp-key))
        (push `(funcall ',f :putprop ',sexp ',sexp-key) forms)
        (push `(funcall ',f :putprop ',defmic ',defmic-key) forms)))
    (let ((time (time)))
      (format t "~&Dumping data to ~A" filename)
      (COMPILER:DUMP-FORMS-TO-FILE filename
                                   forms
                                   '(:package :lam))
      (setq time (quotient (time-difference (time) time) 60.0))
      (format t " took ~\scientific\seconds.~%" time)
      filename)))
