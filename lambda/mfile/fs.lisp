;;; -*- Mode: Lisp; Package: User; Base: 8 -*-

(make-system 'imicro :noconfirm)

(defpackage "MOBY-FILE-SYSTEM"
  :nicknames ("MFS" "MOBY-FILE")
  :size 1000.)

(DEFSYSTEM MOBY-FILE
  (:NAME "Moby-File")
  (:NICKNAMES "MOBY-FILE-SYSTEM")       ;this must come before short-name to win!
  (:SHORT-NAME "MFS")
  (:PATHNAME-DEFAULT "DJ: MFILE;")
;  (:PATCHABLE NIL "MFS")
  (:NOT-IN-DISK-LABEL)
;  (:PACKAGE MOBY-FILE-SYSTEM)
  (:MODULE DEFS ("FSDEFS" "MOBY-MICRO"))
  (:module btree ("btree"))
  (:MODULE MAIN ("FSSTR" "FSGUTS" "FSACC"))
  (:module parse ("lmpars"))
  (:module moby-init ("moby-init" "moby-swap" "moby-debug" "moby-exhibit"))
  (:COMPILE-LOAD DEFS)
  (:compile-load btree)
  (:COMPILE-LOAD MAIN
   (:FASLOAD DEFS btree))
  (:compile-load parse)
  (:compile-load moby-init
   (:fasload defs btree main)))

;(DEFSYSTEM FILE-SERVER
;  (:NAME "FILE-Server")
;  (:NICKNAMES "Server")
;  (:PATHNAME-DEFAULT "SYS: FILE;")
;  (:PATCHABLE NIL "Server")
;  (:NOT-IN-DISK-LABEL)
;  (:PACKAGE FILE-SYSTEM)
;  (:COMPILE-LOAD ("SERVER")))

;FS:
;(DEFUN LOAD-SYSTEMS (&REST SYSTEMS)
;  (LOOP FOR SYSTEM IN SYSTEMS
;       DO (SETQ SYSTEM (SI:FIND-SYSTEM-NAMED SYSTEM))
;          (MAKE-SYSTEM SYSTEM ':NOWARN)
;       WHEN (SI:SYSTEM-PATCHABLE-P SYSTEM)
;       COLLECT SYSTEM INTO PATCHABLE-SYSTEMS
;       FINALLY (LOAD-PATCHES ':NOSELECTIVE ':SYSTEMS PATCHABLE-SYSTEMS)))

;;;; The following are miscellaneous systems that are to be used with the Magtape and File
;;;; systems.  They are not patchable.

;(DEFSYSTEM DISTRIBUTION
;  (:NAME "Distribution")
;  (:NICKNAMES "Dis")
;  (:PATHNAME-DEFAULT "SYS: DISTRIBUTION;")
;  (:PACKAGE FILE-SYSTEM)
;  (:COMPILE-LOAD ("DIST")))

;(DEFSYSTEM ITS-TAPE
;  (:NAME "ITS-Tape")
;  (:NICKNAMES "ITST" "PDP10T")
;  (:PATHNAME-DEFAULT "SYS: TAPE;")
;  (:PACKAGE FILE-SYSTEM)
;  (:COMPILE-LOAD ("PDP10")))

;(DEFSYSTEM VMS-TAPE
;  (:NAME "VMS-Tape")
;  (:NICKNAMES "VMST")
;  (:PATHNAME-DEFAULT "SYS: TAPE;")
;  (:PACKAGE FILE-SYSTEM)
;  (:COMPILE-LOAD ("VMS")))
