;;; -*- Mode:LISP; Package:COMPILER; Cold-Load:T; Lowercase:T; Base:10; Readtable:ZL -*-

;;; &&& This file is the first one loaded for the COMPILER system.  It
;;; is also a cold-load file.  The immediate purpose of this file is
;;; strictly to prevent cold-load lossage, e.g. in GETDECL. <27oct88>

;;; $$$ Set the following cleverly -- compiler knows processor it's
;;; compiled this file to, and thus the host type it will run on!
;;; <27-Oct-88 keith>

(defvar-resettable *host-computer*      ; ||| 29sep88 smh
                   ;;;Currently, either 'LAMBDA-INTERFACE or 'K
                   '#.*target-computer* '#.*target-computer*)

(defvar-resettable *target-computer*
                ;get generators off this.
                   '#.*target-computer* '#.*target-computer*)

;;;Map host / target processor symbol to processor keyword:

(defun target-processor-symbol ()
  (case *target-computer*
    (lambda-interface :lambda)
    (k :falcon)
    (t (ferror nil "*TARGET-COMPUTER* is ~s, which is not defined as a valid target of compilation"
               *target-computer*))))

;;;Map the other way:

(defun processor-target-symbol (sym)
  (ecase sym
    (:lambda 'lambda-interface)
    (:falcon 'k)))
