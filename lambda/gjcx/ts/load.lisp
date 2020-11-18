;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-


(LET* ((P (SEND FS:FDEFINE-FILE-PATHNAME :TRANSLATED-PATHNAME))
       (H (SEND P :HOST))
       (W (SEND P :NEW-PATHNAME
                :NAME :WILD :TYPE :WILD :VERSION :WILD))
       (D (IF (ATOM (SEND P :DIRECTORY)) (LIST (SEND P :DIRECTORY)) (SEND P :DIRECTORY)))
       (LH "TS"))
  (FORMAT T "~&;; Logical host ~S on physical host ~S rooted at ~S~%"
          LH H D)
  (FS:SET-LOGICAL-PATHNAME-HOST LH
                                :PHYSICAL-HOST H
                                :TRANSLATIONS `(("SOURCE;" ,W)
                                                ("*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD))))
                                                ("*;*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD :WILD)))))))



(si:set-system-source-file "TS" "TS:SOURCE;SYSDEF")
