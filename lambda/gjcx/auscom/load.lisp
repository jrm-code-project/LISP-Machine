;;; -*- Mode:LISP; Package:USER; Base:10 -*-

(ecase (si:get-system-version)
  (102 (pushnew :release-2 *features*))
  (110 (pushnew :release-3 *features*))
  (115 (pushnew :release-3 *features*))
  (116 (pushnew :release-3 *features*)))

(LET* ((P (SEND FS:FDEFINE-FILE-PATHNAME :TRANSLATED-PATHNAME))
       (H (SEND P :HOST))
       (W (SEND P :NEW-PATHNAME
                :NAME :WILD :TYPE :WILD :VERSION :WILD))
       (D (IF (ATOM (SEND P :DIRECTORY)) (LIST (SEND P :DIRECTORY)) (SEND P :DIRECTORY))))
  (FORMAT T "~&;; Logical host AUSCOM on physical host ~S rooted at ~S~%"
          H D)
  (FS:SET-LOGICAL-PATHNAME-HOST "AUSCOM"
                                :PHYSICAL-HOST H
                                :TRANSLATIONS `(("SOURCE;" ,W)
                                                ("*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD))))
                                                ("*;*;" ,(SEND W :NEW-DIRECTORY (APPEND D '(:WILD :WILD)))))))


(SI:SET-SYSTEM-SOURCE-FILE "CHANNEL-INTERFACE"
                           "AUSCOM:SOURCE;SYSDEF")

(SI:SET-SYSTEM-SOURCE-FILE "TST360-C"
                           "AUSCOM:SOURCE;SYSDEF")


(defpackage mbu
  (:size 500)
  (:EXPORT "DEFINE-MULTIBUS-IO-REGISTERS"
           "MAKE-MULTIBUS-IO-REGISTERS"
           "DEFINE-MULTIBUS-MEMORY-REGISTERS"
           "MAKE-MULTIBUS-MEMORY-REGISTERS"
           "ALLOCATE-USER-MULTIBUS-DMA-BUFFER"
           "MULTIBUS-REGISTER-READ"
           "MULTIBUS-REGISTER-WRITE"
           "MULTIBUS-REGISTER-MODIFY"
           "MULTIBUS-REGISTER-WRITE-DEFAULT"
           "MULTIBUS-REGISTER-READ-SYMBOLIC"
           "MULTIBUS-REGISTER-PRINT"
           "RESET-MULTIBUS-INTERRUPT-5"
           "PROCESS-WAIT-MULTIBUS-INTERRUPT-5"
           "DISABLE-USER-MULTIBUS-INTERRUPTS"
           "SETUP-USER-MULTIBUS-INTERRUPTS"
           "SETUP-USER-MULTIBUS"
           "DESCRIBE-USER-MULTIBUS"
           "MULTIBUS-REGISTER-PRINT"
           "X"
           "*USER-MULTIBUS*"
           "*IO-REG-TRACE*"
           "USER-MULTIBUS-DMA-BUFFER.MULTIBUS-ADDRESS"))


(DEFPACKAGE AUSCOM
  (:SIZE 500)
  (:USE "GLOBAL" "MBU")
  (:SHADOW "READ" "WRITE" "MODIFY"))
