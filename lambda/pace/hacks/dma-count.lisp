;;; -*- Mode:LISP; Package:MICRO; Base:8; Readtable:ZL -*-

;       ((M-2) DPB M-TEM (BYTE-FIELD 8 16.) (A-CONSTANT 200))  ;200 is DMA count, dpb in msb

(defun set-dma-count (n)
  (let* ((adr (i-mem-lookup '(siopb-1 24.)))
         (inst (read-c-mem adr)))
    (when (not (and (= (ldb lam-ir-op inst) lam-op-byte)
                    (= (ldb lam-ir-m-mem-dest inst) (m-mem-lookup 'm-2))
                    (= (ldb lam-ir-m-src inst) (m-mem-lookup 'm-tem))))
      (ferror nil "bad inst"))
    (write-c-mem adr (dpb (find-or-make-a-constant n t)
                          lam-ir-a-src
                          inst))))

(defun read-dma-count ()
  (let* ((adr (i-mem-lookup '(siopb-1 24.)))
         (inst (read-c-mem adr)))
    (when (not (and (= (ldb lam-ir-op inst) lam-op-byte)
                    (= (ldb lam-ir-m-mem-dest inst) (m-mem-lookup 'm-2))
                    (= (ldb lam-ir-m-src inst) (m-mem-lookup 'm-tem))))
      (ferror nil "bad inst"))
    (read-a-mem (ldb lam-ir-a-src inst))))
