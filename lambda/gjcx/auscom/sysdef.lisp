;;; -*- Mode:LISP; Package:AUSCOM; Base:10; Readtable:CL -*-


(SI:DEFINE-SIMPLE-TRANSFORMATION :CSTUFF SI:READFILE-C SI:FILE-NEWER-THAN-INSTALLED-P
                                 ("C") NIL ("Read" "Reading" "read") NIL)

(SI:DEFINE-SIMPLE-TRANSFORMATION :HSTUFF SI:READFILE-C SI:FILE-NEWER-THAN-INSTALLED-P
                                 ("H") NIL ("Read" "Reading" "read") NIL)

(PUSHNEW "C" SI:*SOURCE-FILE-TYPES*)
(PUSHNEW "H" SI:*SOURCE-FILE-TYPES*)

#+release-2
(when (< (nth-value 1 (si:get-system-version)) 146)
  (load "sys:patch;system-102-146" :set-default-pathname nil))

(DEFSYSTEM TST360-C
  (:PATHNAME-DEFAULT "AUSCOM:C;TST360;")
  ;; THIS SYSTEM IS FOR SELECT-SYSTEM-AS-TAG-TABLE USAGE.
  ;; THE C SOURCE CODE.
  (:MODULE INCLUDES ("DIAG.H" "DIAGDAT.H"  "KIWI.H"
                     "MSGS.H" "RABITS.H"
                     ))
  (:MODULE SOURCES ("KDSIO.C" "KIWISUBS.C" "TST360.C"))
  (:SKIP :HSTUFF INCLUDES)
  (:SKIP :CSTUFF SOURCES))

(DEFSYSTEM DIAG-C
  (:PATHNAME-DEFAULT "AUSCOM:C;DIAG;")
  ;; THIS SYSTEM IS FOR SELECT-SYSTEM-AS-TAG-TABLE USAGE.
  ;; THE C SOURCE CODE.
  (:MODULE INCLUDES ("DIAG.H" "DIAGDAT.H"  "KIWI.H"
                     "MSGS.H" "RABITS.H"
                     ))
  (:MODULE SOURCES ("D86L1.C" "D86L2.C" "D86L3.C" "D86LP.C" "D86MN.C" "HKINIT.C"
                    "KDSIO.C" "KIWISUBS.C" "TST360.C"))
  (:SKIP :HSTUFF INCLUDES)
  (:SKIP :CSTUFF SOURCES))


(DEFSYSTEM CHANNEL-INTERFACE
  ;; THE LISP TST360 INCLUDES A COUPLE DIAGS, JUST TO MAKE SURE DMA WORKS THROUGH THE SDU TO
  ;; THE NUBUS. BUT IS MOSTLY INTENDED TO BE THE START OF THE CHANNEL "AI-SERVER"
  (:NAME "Channel Interface")
  (:pathname-default "auscom:source;")
  #+release-3
  (:PATCHABLE "AUSCOM:PATCH;" "REL3")
  #+RELEASE-2
  (:PATCHABLE "AUSCOM:PATCH;" "REL2")
  #+release-2
  (:default-binary-file-type "OFASL")

  (:module mbu "mbu")
  (:module defs "driver-defs")
  (:module driver "driver")
  (:module diag "diag")
  (:MODULE DOWNLOAD-DATA "DOWNLOAD-DATA-D")
  (:module demo ("ebcdic" "demo"))
  (:compile-load mbu)
  (:compile-load defs (:fasload mbu) (:fasload mbu))
  (:compile-load driver (:fasload defs) (:fasload defs))
  (:COMPILE-LOAD DOWNLOAD-DATA (:FASLOAD DRIVER) (:FASLOAD DRIVER))
  (:compile-load diag (:fasload driver) (:fasload driver))
  (:compile-load demo (:fasload driver) (:fasload driver)))
