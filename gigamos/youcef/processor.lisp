;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;

;;;
;;;


(defstruct (processor
             (:conc-name P-)
             )
  (prom-string nil)
  (proc-type nil)
  (tv-controller-type nil)
  (tv-slot nil)
  (tv-device-subindex nil)
  (disk-controller-type nil)
  (disk-share-mode nil)
  (disk-type nil)
  (proc-variables-to-switch nil)
  (saved-opcs nil)
  (saved-micro-stack nil)
  (memory-configuration-list nil)
  (page-band-unit nil)
  (proc-conf-pointer nil)
  (mem-slot nil)
  (proc-conf-bus-address nil)
  )
