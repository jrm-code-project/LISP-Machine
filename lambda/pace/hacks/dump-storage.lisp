;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-


(defun dump-storage (start)
  (do ((adr (%pointer start) (1+ adr))
       info)
      (())
    (format t "~&~10o: " adr)
    (setq info (nth-value 1 (lam:xpointer-info adr)))
    (cond ((= info 2)
           (format t "~15a " (nth (%p-cdr-code adr) q-cdr-codes))
           (cond ((= (%p-data-type adr) dtp-header)
                  (format t "~25a " (nth (%p-ldb %%header-type-field adr)
                                         q-header-types)))
                 (t
                  (format t "~25a " (nth (%p-data-type adr) q-data-types))))
           (format t "~10o" (%p-pointer adr))
           )
          (t
           (format t "Unboxed: ")
           (send standard-output :tyo (%p-ldb (byte 8 0) adr))
           (send standard-output :tyo (%p-ldb (byte 8 8) adr))
           (send standard-output :tyo (%p-ldb (byte 8 16.) adr))
           (send standard-output :tyo (%p-ldb (byte 8 24.) adr))
           ))
    (send standard-input :tyi)
    ))
