;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

;;;***************************************************************************************************
;;;*         !!!!   THIS FILE HAS BEEN MADE OBSOLETE BY JB:KBUG;DEF-CORR-PKG.LISP  !!!!              *
;;;***************************************************************************************************


;;;; K Machine System Packages


;;; K contains the symbols used in the assembler.  Packages in which
;;; assembly code is written should use this.
(defpackage k
  :use '()
  (:import-from lisp "OPEN" "RETURN" "OR" "AND" "IGNORE"))


;;; PRIMITIVES contains a subset of Common Lisp, both a subset of the
;;; functions and a subset of the functionality for individual functions.
;;; This is the subset which can be compiled directly into K Machine
;;; machine code.  PRIMS defines the language in which the lowest levels
;;; of the system is implemented.
(defpackage PRIMITIVES
  :use '()
  (:nicknames prims)
  (:auto-export-p t))
; (:import-from global "IMPORT"))


;;; HARDWARE defines the extreme low level of the interface to the
;;; hardware.  If the hardware changes, this will, too.
(defpackage HARDWARE
  :use '()
; (:import-from primitives "DEFCONSTANT" "BYTE")
  (:nicknames hw)
  (:auto-export-p t))


;;; GLOBAL-REGISTERS contains the names of the global registers.  These
;;; symbols are not imported into any other package, the package name is
;;; used explicitly to denote that a variable is contained in a global
;;; register.  These symbols could probably be in VINC instead.
(defpackage GLOBAL-REGISTERS
  :use '(primitives)
  (:nicknames GR))


;;; VINCULUM ("that which binds" or "glue") contains the software
;;; definitions that are firmly based on the hardware.  It isn't that the
;;; hardware determines these per se, but that the hardware was designed
;;; with these in mind.  It should be possible to change these without
;;; making ECO's, but it wasn't intended.
(defpackage VINCULUM
  :use '(prims)
  (:nicknames vinc)
  )
;;; Auto export bites hard here.
; (:auto-export-p t)
;  (:import-from global "NIL"))


;;; TRAP contains code to handle traps including saving the state of the
;;; machine and restarting.
(defpackage TRAP
  :use '(vinculum prims k))


;;; TIMERS contains the code to handle timer traps.
;;; (This could probably be part of TRAP).
(defpackage TIMERS
  :use '(vinculum prims))


(defpackage DATATYPE-RAM
  :use '(vinculum prims k)
  (:nicknames dt-ram))


(defpackage NUBUS-STUFF
  :use '(vinc prims k))


;;; The following 11 packages contain all the code which manages the
;;; lowest levels of the virtual memory system, including details of
;;; garbage collection.  They could probably be in one or two packages.

;;; MAP contains constants and functions for dealing with the memory map
;;; at the lowest level.
(defpackage MAP
  :use '(vinculum prims))

;;; GC-RAM contains functions which deal with the GC RAM.
(defpackage GC-RAM
  :use '(vinculum prims))

(defpackage PAGING-DEVICES
  :use '(vinculum prims))

(defpackage VIRTUAL-MEMORY
  :use '(vinculum prims)
  (:nicknames vmem))

(defpackage PHYSICAL-CLUSTER-DATA
  :use '(virtual-memory vinculum prims)
  (:nicknames pcd))

(defpackage QUANTUM-MAP
  :use '(virtual-memory vinculum prims))

(defpackage MEMORY-MANAGEMENT
  :use '(vinculum prims)
  (:nicknames memlow))

;;; MAP-FAULT contains code which handles MAP faults (page faults, write to fresh
;;; page, write to read only, etc)
(defpackage MAP-FAULT
  :use '(virtual-memory physical-cluster-data map vinculum prims))

(defpackage GC-FAULT
  :use '(map vinculum prims))

;;; MEMORY-MANAGEMENT-INTERFACE just exports symbols from some other
;;; packages that define the user interface to the memory system.  This
;;; seems gratuitous.
(defpackage MEMORY-MANAGEMENT-INTERFACE
  :use '(vinculum prims)
  (:nicknames mem))

(defpackage TRANSPORTER-RAM
  :use '(vinculum virtual-memory prims))


;;; The next level of the memory system organizes the memory into
;;; region and areas.

(defpackage REGION-BITS
  :use '(memory-management virtual-memory vinculum prims))

(defpackage REGION-DATA
  :use '(memory-management vinculum prims))

(defpackage AREA-DATA
  :use '(region-data memory-management vinculum prims))


;;; The next level of the memory system implements the lisp object and
;;; typed pointer model of memory.

;;; CONS contains functions for allocation, access and modification of
;;; lisp objects.  The exported symbols of this package (MAKE-POINTER,
;;; CONTENTS-OFFSET, CONS, CAR, SET-CAR, ALLOCATE-STRUCTURE etc.)
;;; constitute the interface to the lisp pointer and object memory system.
(defpackage CONS
  :use '(memory-management-interface vinculum prims k)
  (:shadow "CONS"))

;;; SYMBOL contains the implementation of symbols.
;;; This should probably be in LI.
(defpackage SYMBOL
  :use '(cons vinculum prims))

;;; NEW-MATH contains the Common Lisp mathematical functions.
(defpackage NEW-MATH
  :use '(vinculum prims k))

;;; ARRAY contains the Common Lisp array functions.
(defpackage ARRAY
  :use '(vinculum prims k))

;;; BOOT contains code and constants for booting the machine.
(defpackage BOOT
  :use '(vinculum virtual-memory prims k))

;;; LISP-INTERNALS contains all of Common Lisp plus the internal
;;; functions used to implement the Common Lisp functions.
(defpackage LISP-INTERNALS
  :use '(prims k)
  (:nicknames li)
  (:import-from k "IGNORE" "SINGLE-FLOAT" "DOUBLE-FLOAT"))

;;; SETF contains SETF and related functions.
;;; This should probably be in LI.
(defpackage SETF
  :use '(lisp))

;;; NLISP is a temporary name for LISP, the package which contains all
;;; of Common Lisp.
(defpackage NLISP
  :use '(prims))

;;; Debugger things

(defpackage KBUG-STREAMS
  :use '(;kbug2-common
         vinculum prims k))

(defpackage K2                                  ; KBUG2/K2 stuff
  :use '(kbug-streams vinculum prims k ;kbug2-common
                      ))

;;; The compiler and debugger use this but it doesn't appear on the K
(defpackage FASDUMP
  :use '(;kbug2-common
         lisp))
