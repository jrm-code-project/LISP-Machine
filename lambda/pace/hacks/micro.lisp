;;; -*- Mode:LISP; Package:MICRO; Base:8; Readtable:ZL -*-

(defvar basic-block-addresses nil)
(defun find-basic-block-addresses ()
  (setq basic-block-addresses nil)
;  (format t "~o: " (si:highest-i-mem-location))
;  (dotimes (adr (si:highest-i-mem-location))
;    (if (zerop (ldb (byte 9. 0) adr)) (format t "~o " adr))
;    (let ((inst (si:read-c-mem adr)))
;      (when (= (ldb lam:lam-ir-op inst) lam:lam-op-jump)
;       (let ((jump-adr (ldb lam:lam-ir-jump-addr inst)))
;         (if (not (memq jump-adr basic-block-addresses))
;             (push jump-adr basic-block-addresses))))))
  (maphash #'(lambda (sym val)
               (let ((table-length (get sym 'lam:dispatch-field-width)))
                 (cond ((eq table-length t))
                       ((not (<= 0 val (- 4096. (ash 1 table-length))))
                        (ferror nil "dispatch table out of range"))
                       (t
                        (dotimes (offset (ash 1 table-length))
                          (let ((adr (%p-ldb (byte 16. 0)
                                             (%pointer-plus si:a-memory-virtual-address
                                                            (+ offset val)))))
                            (if (not (memq adr basic-block-addresses))
                                (push adr basic-block-addresses))))))))
           (si:symbol-table-d-mem-hash-table (assq %microcode-version-number si:*i-mem-symbol-tables*)))
  nil
  )
