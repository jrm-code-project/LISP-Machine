;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defpackage simulator
  :use '(global)
  (:nicknames sim))

(defpackage k
  :use '()
  (:import-from lisp "OPEN" "RETURN" "OR" "AND" "IGNORE"))

(defpackage primitives
  :use '()
  (:nicknames prims)
  (:auto-export-p t))
;  (:import-from global "IMPORT"))

(defpackage hardware
  :use '()
;  (:import-from primitives "DEFCONSTANT" "BYTE")
  (:nicknames hw)
  (:auto-export-p t))

(defpackage global-registers
  :use '(primitives)
  (:nicknames GR))

(defpackage vinculum
  :use '(prims)
  (:nicknames vinc)
  )
;;; Auto export bites hard here.
; (:auto-export-p t)
;  (:import-from global "NIL"))

(defpackage primitive-arithmetic
  :use '(vinculum prims)
  (:nicknames parith))

(defpackage timers
  :use '(vinculum prims))

(defpackage map
  :use '(vinculum prims))

(defpackage gc-ram
  :use '(vinculum prims))

(defpackage datatype-ram
  :use '(vinculum prims k)
  (:nicknames dt-ram))

(defpackage paging-devices
  :use '(vinculum prims))

(defpackage virtual-memory
  :use '(vinculum prims)
  (:nicknames vmem))

(defpackage physical-cluster-data
  :use '(virtual-memory vinculum prims)
  (:nicknames pcd))

(defpackage quantum-map
  :use '(virtual-memory vinculum prims))

(defpackage memory-management
  :use '(vinculum prims)
  (:nicknames memlow))

(defpackage region-bits
  :use '(memory-management virtual-memory vinculum prims))

(defpackage map-fault
  :use '(virtual-memory physical-cluster-data map vinculum prims))

(defpackage gc-fault
  :use '(map vinculum prims))

(defpackage region-data
  :use '(memory-management vinculum prims))

(defpackage area-data
  :use '(region-data memory-management vinculum prims))

(defpackage memory-management-interface
  :use '(vinculum prims)
  (:nicknames mem))

(defpackage boot
  :use '(vinculum virtual-memory prims k))

(defpackage transporter-ram
  :use '(vinculum virtual-memory prims))

(defpackage cons
  :use '(memory-management-interface vinculum prims k)
  (:shadow "CONS"))

(defpackage new-math
  :use '(vinculum prims k))

(defpackage array
  :use '(vinculum prims k))

(defpackage trap
  :use '(vinculum prims k))

;;; This package has to have access to a
;;; running lisp to build an environment
;;; for booting.
(defpackage sim-debug
  :use '(vinculum global))

;(defpackage kbug2-common                       ; definitions for KBUG2 and K2
;  :use '(vinculum prims k))

;;;; Sorta similar to the above.
;;;; Things depend upon kbug being common to both package environments, but
;;;; we can't USE both versions of KBUG2-common.
;(if (boundp 'si::*current-package-environment*)
;    (if (eq si::*current-package-environment*
;           (si::find-package-environment "DEBUGGER" #'identity
;                                         #'(lambda () (error "Package-environment not set up."))))

;       (defpackage kbug
;         :use '(lisp k lam kbug2-common)
;         (:import-from "ZETALISP" "DEFSUBST"))
;       (defpackage kbug
;         :use '(lisp k lam)
;         (:import-from "ZETALISP" "DEFSUBST")))
;    (defpackage kbug
;      :use '(lisp k lam)
;      (:import-from "ZETALISP" "DEFSUBST")))

(defmacro hairy-defpackage (package-name k-use-list lambda-use-list &rest stuff)
  `(eval-when (compile load)
     (if (or (not (boundp 'si::*current-package-environment*))
             (eq si::*current-package-environment*
                 (si::find-package-environment "COMPILER" #'identity
                                               #'(lambda () (error "Package-environment not set up.")))))
         (defpackage ,package-name
           :use ,k-use-list
           ,@stuff)
       (if (eq si::*current-package-environment*
               (si::find-package-environment "DEBUGGER" #'identity
                                             #'(lambda () (error "Package-environment not set up."))))
           (defpackage ,package-name
             :use ,lambda-use-list
             ,@stuff)
         (ferror nil "Random package environment!")))))

(defpackage kbug
  :use '(;kbug2-common
         lisp k lam)
  (:import-from "ZETALISP" "DEFSUBST"))

(defpackage lisp-internals
  :use '(prims k)
  (:nicknames li)
  (:import-from k "IGNORE" "SINGLE-FLOAT" "DOUBLE-FLOAT"))

(defpackage nlisp
  :use '(prims))

(defpackage setf
  :use '(lisp))

(defpackage k-debug
  :use '(vinc prims k))

(defpackage nubus-stuff
  :use '(vinc prims k))

(defpackage cold
  :use '(k lisp))

(defpackage kbug-streams
  :use '(;kbug2-common
         vinculum prims k))

(defpackage k2                                  ; KBUG2/K2 stuff
  :use '(kbug-streams vinculum prims k ;kbug2-common
                      ))

(defpackage symbol
  :use '(cons vinculum prims))

(defpackage fasdump
  :use '(;kbug2-common
         lisp))
