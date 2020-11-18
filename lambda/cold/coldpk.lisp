; -*- Mode:LISP; Readtable:T; Base:8; Lowercase:T; Package: (cold :size 1000. :use :global)) -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;This package is the program
(defpackage cold (:use "GLOBAL") (:size 1000.))

;This package is used to contain symbols which stand for symbols in the cold-load
;being built; this includes all the system data structure definition symbols.
;These shadow the ones in GLOBAL, so that you can make incompatible changes.
(defpackage cold-symbols
  (:use)
  (:size 20.)
  (:nicknames "SYM")
  (:relative-names ("GLOBAL" "GLOBAL"))
  ;; Borrow symbols needed to load QCOM and QDEFS
  ;; But don't actually get under global, want to shadow all the system parameter symbols
  ;; Code here and there relies on the fact that NIL is the same.
  (:import-from global
                defun defvar defconst setq cond and or ibase defprop putprop + - > = < 1+ 1-
                t nil error length special dpb %p-ldb-offset *read-base*
                assign-values assign-alternate assign-values-init-delta * byte ash)
  (:import-from si get-alternate)
  (:import-from cold defmic))

(if (not (fboundp 'sym:get-alternate))
    (fset 'sym:get-alternate 'si:get-alternate))

(if (not (fboundp 'sym:defmic))
    (fset 'sym:defmic 'cold:defmic))

;;This package is really under NIL, not GLOBAL, but there is a bug in the package system.
;;This package is used to contain symbols which stand for symbols in the cold-load
;;being built; this includes all the system data structure definition symbols.
;;These shadow the ones in GLOBAL, so that you can make incompatible changes.
;(package-declare cold-symbols none 10000.
;   ()
;   (refname global global)
;   (myrefname global cold-symbols)
;   (myrefname global sym)
;   ;; Borrow symbols needed to load QCOM and QDEFS
;   ;; But don't actually get under global, want to shadow all the system parameter symbols
;   ;; Code here and there relies on the fact that NIL is the same.
;   (borrow global defun defvar defconst setq cond and or ibase defprop putprop + - > = < 1+ 1-
;                 t nil error length special dpb %p-ldb-offset
;                 assign-values assign-alternate get-alternate assign-values-init-delta)
;   (borrow cold defmic))

(fset 'cold-symbols:logdpb #'dpb)

;Before we can compile anything in the cold-load package, we need to load the parameters
(si:define-simple-transformation cold:load-cold-parameters cold:load-parameters
        cold:parameters-not-loaded () ()
        ("Load cold parameters" "Loading cold parameters" "cold parameters loaded"))

(defun cold:parameters-not-loaded ()
  (not (boundp (progn t 'cold:big-fixnum))))

(defsystem cold
  (:package cold)
  (:pathname-default "SYS: COLD;")
  (:warnings-pathname-default "SYS:COLD;COLD-CWARNS.LISP")
  (:module util ("COLDUT"))
  (:module server "MINI-SERVER" :package si)
  (:module file-lists ("SYS: SYS; SYSDCL" "SYS: SYS; INNER-SYSTEM-FILE-ALIST") :package si)
  (:skip :fasload util)
  (:skip cold:load-cold-parameters nil (:fasload util))
  (:compile-load (util "COLDLD") (cold:load-cold-parameters))
  (:compile-load server)
  (:compile-load file-lists))
