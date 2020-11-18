;;; -*- Mode:LISP; Package:USER; Base:10 -*-

;;; (C) Copyright 1987, LISP MACHINE INC
;;; See filename "Copyright.Text" for more information.
;;; *********************************************************
;;; *********************************************************
;;; *** NOTE: This is an EXAMPLE, not LMI supported code. ***
;;; *** information contained in this example is subject  ***
;;; *** to change without notice. The ways of doing       ***
;;; *** the things contained in the example may change    ***
;;; *** between system releases. Some techniques which    ***
;;; *** are mere examples in one release may become built ***
;;; *** in system features in the next release. Use good  ***
;;; *** judgement when copying these techniques. Most     ***
;;; *** examples have been motivated by specific customer ***
;;; *** requests, and may not be the best engineered      ***
;;; *** or most efficient solution for someone else.      ***
;;; *********************************************************
;;; *********************************************************


(dolist (n '(112 115 116))
  (or (>= (nth-value 1 (si:get-system-version 'system-revision-level))
          n)
      (load (si:patch-system-pathname 'system-revision-level :patch-file 3 n)
            :set-default-pathname nil)))

;  (112 "speed up directory listing by speeding up FS:FILE-TRUENAME -GJC" "wsr" NIL)
;  (115 "another fix required by NFS-SERVER -GJC" "wsr" NIL)
;  (116 "Fixes to file system:" ...)


(defun SUN:do-stuff ()
  (dolist (l '(sun:port-mapper sun:mount))
    (process-run-function (format nil "~A" l)
                          #'(lambda (s)
                              (let ((rpc:*rpc-server-trace* nil))
                                (rpc:run-rpc-udp-server s)))
                          l)))

(defun SUN:do-nfs (&optional (trace t))
  (let ((rpc:*rpc-server-trace* trace))
    (rpc:run-rpc-udp-server 'sun:nfs)))
