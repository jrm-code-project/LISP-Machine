;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(LET* ((P (SEND FS:FDEFINE-FILE-PATHNAME :TRANSLATED-PATHNAME))
       (H (SEND P :HOST))
       (W (SEND P :NEW-PATHNAME
                :NAME :WILD :TYPE :WILD :VERSION :WILD))
       (D (IF (ATOM (SEND P :DIRECTORY)) (LIST (SEND P :DIRECTORY)) (SEND P :DIRECTORY))))
  (FORMAT T "~&;; Logical host PASCAL on physical host ~S rooted at ~S~%"
          H D)
  (FS:SET-LOGICAL-PATHNAME-HOST "PASCAL"
                                :PHYSICAL-HOST H
                                :TRANSLATIONS `(("SOURCE;" ,W)
                                                ("*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD))))
                                                ("*;*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD :WILD)))))))



(defpackage pascal
  )


(si:set-system-source-file "PASCAL" "PASCAL:SOURCE;SYSDEF")
