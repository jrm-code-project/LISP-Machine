;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10; Patch-File:T -*-


;;; patch for release 2.0, SO THAT YOU DONT NEED "N" DIFFERENT SITE FILES.

;;; NOTE: THIS WONT REALLY BE USEABLE UNTIL YOU HAVE THE PATCH FOR THE FTP-SERVER
;;; TO HACK QFASL FILES CORRECTLY.

(DEFUN PERMISSIBLE-ACCESS-PATH-FLAVOR-P (HOST ACCESS-D &AUX COND-TYPE COND-ARGS)
  (DOLIST (CONDITION (CDDR ACCESS-D) T)
    (UNLESS
      (IF (EQ CONDITION :LOCAL)
          (OR (EQ SI:LOCAL-HOST HOST)
              (LET ((CHAOS (SEND HOST :SEND-IF-HANDLES :CHAOS-ADDRESS))) ; KLUDGE!
                (AND CHAOS
                     (IF SI:LOCAL-HOST
                         (= CHAOS (SEND SI:LOCAL-HOST :CHAOS-ADDRESS))
                       NIL))))
        (SETQ COND-TYPE (FIRST CONDITION) COND-ARGS (REST CONDITION))
        (SELECTQ COND-TYPE
          (:FILE-SYSTEM-TYPE (MEMQ (SEND HOST :FILE-SYSTEM-TYPE) COND-ARGS))
          (:NETWORK
           (DOLIST (NETWORK COND-ARGS)
             (IF (AND (SEND HOST :NETWORK-TYPEP NETWORK)
                      (OR (NOT (EQ NETWORK :CHAOS))
                          (EQ SI:LOCAL-HOST HOST)
                          (DO ((ADDR (SEND HOST :NETWORK-ADDRESS :CHAOS))
                               (L SI:*OTHER-PROCESSORS* (CDR L)))
                              ((OR (NULL L) (NULL ADDR)) NIL)
                            (IF (EQUAL (SI:%PROCESSOR-CONF-CHAOS-ADDRESS (SI:OP-PROC-CONF (CAR L))) ADDR)
                                (RETURN T)))
                          (AND SI:*ETHERNET-HARDWARE-CONTROLLER*
                               ;; OTHERWISE WE MUST HAVE A 3COM BOARD
                               ;; AND THE OTHER GUY MUST NOT BE EXCELAN-INTERFACE-ONLY
                               (NOT (GET HOST :EXCELAN-INTERFACE-ONLY)))))
                 (RETURN T))))
          (:PROTOCOL T) ; not supported yet
          (OTHERWISE T)))
      (RETURN NIL))))
