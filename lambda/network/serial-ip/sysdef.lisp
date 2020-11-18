;;; -*- Mode:LISP; Package:USER; Readtable:CL; Syntax:COMMON-LISP; Base:10 -*-

;;; SYSDEF.LISP for SERIAL-IP system
;;; This is only useful on systems which provide the MIT/Lisp Machine MAKE-SYSTEM facility.
;;; 4/25/88 - Keith Corbett


#-(OR LMI TI SYMBOLICS)
(unless (fboundp 'make-system)
  (cerror
    "Proceed anyway"
    "Apparently this system does not support MAKE-SYSTEM, which is used to build SERIAL-IP"))

;;;Define Serial-IP network interface (belongs in SYSDCL)

;;Package definitions:

(if (find-package "SERIAL-PROTO")
    (use-package '("NETWORK" "LISP") "SERIAL-PROTO")
  (make-package "SERIAL-PROTO"
                :nicknames "SPROTO"
                :use '("NETWORK" "LISP")))

(if (find-package "SERIAL-IP")
    (use-package '("SERIAL-PROTO" "NETWORK" "LISP") "SERIAL-IP")
  (make-package "SERIAL-IP"
                :nicknames "SLIP"
                :use '("SERIAL-PROTO" "NETWORK" "LISP")))

(defsystem serial-ip
  (:short-name "SLIP")
  (:name "Serial-IP")
  (:pathname-default "SYS:NETWORK;SERIAL-IP;")
  (:patchable "SYS:NETWORK;SERIAL-IP;PATCH;")
  (:module proto ("SERIAL-PROTOCOL"))
  (:module sites ("SERIAL-SITES"))
  (:module driver ("SERIAL"))
  (:compile-load proto)
  (:compile-load sites)
  (:compile-load-init driver
                      (proto sites)
                      (:fasload proto sites)
                      (:fasload proto sites)))

#+LMI                                           ;All that's defined so far
(defsystem serial-ip-tests
  (:pathname-default "SYS:NETWORK;SERIAL-IP;TESTS;")
  (:module io-test
           (#+LMI "sdu-serial-test")
           )
  (:compile-load io-test))
