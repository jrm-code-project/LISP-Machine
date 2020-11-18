;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-

;;; (c) Copyright 1986, Lisp Machine Incorporated.

; "rominfo" structure at #x90 in multibus ram
;   The structure is set up only by LMI version SDU ROMs (SDU Monitor V102+).
;   Be sure to qualify every reference with (rominfo-valid-p);
;   the accessors will return garbage if the structure is not present.
;   Ideally, (define-accessors-for-structure) should make every function
;   conditional on a test for validity.
; /usr/86include/rominfo.h

(defconst rominfo-qs '(
  %rominfo-magic-42                             ;magic number 42.
  %rominfo-size-in-words                        ;size of active structure
  %rominfo-version-number                       ;100+ for LMI roms
  %rominfo-console-type                         ;console types CT_ from sysconfig.h
  %rominfo-slot-if-video                        ;slot number of boot console, if video
  %rominfo-screen-if-quad                       ;screen number, for quad
  %rominfo-sysconf-nubus-addr                   ;nubus addr of sysconf, set up by newboot
  %rominfo-rom-font-addr                        ;8086 ptr to rom font
  %rominfo-init-vector                          ;8086 ptr to monitor re-init entry point
  %rominfo-optinfo                              ;if non-zero, ptr to more stuff
  ))
(mapc #'(lambda (x) (putprop x t 'system-constant)) rominfo-qs)
(assign-values rominfo-qs)

(define-accessors-for-structure rominfo-qs)

(defconst rom-info (make-array 20.
                               :type :art-16b
                               :displaced-to (+ multibus-virtual-address (// #x90 4))
                               :named-structure-symbol 'rom-info
                               :leader-length 3))

(defselect ((rom-info named-structure-invoke))
  (:describe (struct)
    (format t "~&~S:" struct)
    (dolist (q rominfo-qs)
      (print-sysconf-entry q struct)))
  (:print-self (struct stream ignore ignore)
    (printing-random-object (struct stream :typep)
      (format stream "Multibus:#x~16r"
              (* 4 (- (%p-contents-offset rom-info (array-data-offset rom-info))
                      multibus-virtual-address))
                )))
  (:which-operations (ignore)
    '(:describe :print-self :which-operations)))

(defprop %rominfo-magic-42 t :decimal)
(defprop %rominfo-size-in-words t :decimal)
(defprop %rominfo-version-number t :decimal)
(defprop %rominfo-slot-if-video t :decimal)
(defprop %rominfo-sysconf-nubus-addr t :nubus-physical-adr)
(defprop %rominfo-rom-font-addr t :nubus-physical-adr)
(defprop %rominfo-init-vector t :nubus-physical-adr)
(defprop %rominfo-optinfo t :nubus-physical-adr)

(defsubst rominfo-valid-p ()
  (= (%rominfo-magic-42 rom-info) 42.))
