;;; -*- Mode:LISP; Package:USER; Base:10 -*-

;; here is how to bootstrap the TI software network config at LMI.

;; * create a directory LM:SITE; for the SITEINFO.XFASL file to go.
;; add the local host EXP1, and DJ.
;; Also add a optional site option :CHAOS-HOST-TABLE-SERVER-HOSTS '("DJ")
;;
;;
;; Then run this function:

(defun dump-crufty-for-ti-site (to-file)
  (update-site-configuration-info)
  (with-open-file (stream to-file :direction :output)
    (dolist (x si:host-alist)
      (let ((name (car x)))
        (format stream "(si:parse-host ~S NIL T)~%(fs:add-chaosnet-file-computer ~S)~2%"
                name name)))))


;; Then make sure you (update-site-configuration-info) on DJ.
;; then load the file it produced onto the ti.
;; then process/distribute network configuration from in-core-data.

;; WARNING: DONT TRY TO USE THE TI FUNCTION "CONVERT OLD SITE/HOST TABLE FORMAT"
;;          THIS LOSES BIG. ALSO, EVALUATING A DEFINE-HOST FROM HSTTBL.LISP LOSES.
