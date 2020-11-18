;;; -*- Mode:LISP; Package:TCP-APPLICATION; Readtable:CL; Base:10 -*-

#|
  Copyright LISP Machine, Inc. 1985,1987
   See filename "Copyright.Text" for
  licensing and release information.


defs below are from cap://lmi//mwt//exos3.1//include//EXOS//net//in.h
and cap://lmi//mwt//exos3.1//include//EXOS//net//misc.h


Constants and structures defined by the internet system,
Per RFC 790, September 1981.
4/02/85 10:31:28 -gjc added IPPORT-IMAGEN

|#

;;;SYM and friends

(export '("DEFSYM" "SYM" "SYM-BOUNDP" "SYM-VALUE"))

(defmacro defsym (name value &optional documentation)
  `(*defsym ',name ,value ,documentation))

(defvar *defsymbols* nil "a list of all DEFSYM defined symbols")

(defun *defsym (name value documentation)
  (when (global:record-source-file-name name 'defsym)
    (setf (documentation name 'defsym) documentation)
    (setf (get name 'tcp-sym-value) value)
    (pushnew name *defsymbols*)
    name))

(defmacro sym (x)
  `(get ',x 'tcp-sym-value))

(defun sym-boundp (x)
  (not (null (get x 'tcp-sym-value))))

(defun sym-value (x)
  (get x 'tcp-sym-value))

; Port/socket numbers: network standard functions

(DEFSYM IPPORT-ECHO             7)
(DEFSYM IPPORT-DISCARD          9)
(DEFSYM IPPORT-SYSTAT           11)
(DEFSYM IPPORT-DAYTIME          13)
(DEFSYM IPPORT-NETSTAT          15)
(DEFSYM IPPORT-FTP-DATA         20)
(DEFSYM IPPORT-FTP              21)
(DEFSYM IPPORT-TELNET           23)
(DEFSYM IPPORT-SMTP             25)
(DEFSYM IPPORT-IMAGEN           35)
(DEFSYM IPPORT-TIMESERVER       37)
(DEFSYM IPPORT-NAMESERVER       42)
(DEFSYM IPPORT-WHOIS            43)
(DEFSYM IPPORT-MTP              57)

; Port/socket numbers: host specific functions

(DEFSYM IPPORT-TFTP             69)
(DEFSYM IPPORT-RJE              77)
(DEFSYM IPPORT-FINGER           79)
(DEFSYM IPPORT-TTYLINK          87)
(DEFSYM IPPORT-SUPDUP           95)

;LMI specific ports
(DEFSYM IPPORT-LMIDISK          256)

; UNIX TCP sockets

(DEFSYM IPPORT-EXECSERVER       512)
(DEFSYM IPPORT-LOGINSERVER      513)
(DEFSYM IPPORT-CMDSERVER        514)

; UNIX UDP sockets

(DEFSYM IPPORT-BIFFUDP          512)
(DEFSYM IPPORT-WHOSERVER        513)

; Ports < IPPORT-RESERVED are reserved for
; privileged processes (e.g. root).
;;;***Actually, this is a Unix-ism; the "Assigned Numbers" documents only reserve port up to 256

(DEFSYM IPPORT-RESERVED         1024)
