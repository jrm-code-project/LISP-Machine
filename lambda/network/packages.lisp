
;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(defpackage network
  (:use "LISP")
  (:nicknames "NET")
  (:prefix-name "NET")
  (:import-from 'global
                "ADD-INITIALIZATION"
                "ALLOCATE-RESOURCE"
                "ARRAY-INDEXED-P"
                "ARRAY-INITIALIZE"
                "ARRAY-LEADER"
                "ARRAY-LENGTH"
                "COPY-ARRAY-CONTENTS"
                "COPY-ARRAY-PORTION"
                "DEALLOCATE-RESOURCE"
                "DEFRESOURCE"
                "DEFSELECT"
                "DEFSELECT-INCREMENTAL"
                "DEFSUBST"
                "DELETE-INITIALIZATION"
                "FALSE"
                "INITIALIZATIONS"
                "LOCF"
                "MAKE-PROCESS"
                "NAMED-STRUCTURE-INVOKE"
                "NAMED-STRUCTURE-P"
                "NCONS"
                "PROCESS-RUN-FUNCTION"
                "PROCESS-WAIT"
                "PROCESS-WAIT-WITH-TIMEOUT"
                "SELECT-PROCESSOR"
                "STRING-APPEND"
                "STRING-LENGTH"
                "STRING-SEARCH"
                "STRING-SEARCH-CHAR"
                "STRING-SEARCH-SET"
                "SUBSTRING"
                "TIME-DIFFERENCE"
                "TIME-INCREMENT"
                "TIME-LESSP"
                "TRUE"
                "WITH-LOCK"
                "WITHOUT-FLOATING-UNDERFLOW-TRAPS"
                "WITHOUT-INTERRUPTS"
                ;;Flavor stuff
                "COMPILE-FLAVOR-METHODS"
                "DEFFLAVOR"
                "DEFMETHOD"
                "DEFWRAPPER"
                "LEXPR-SEND"
                "MAKE-INSTANCE"
                "SELF"
                "SEND"
                ;;Debugging stuff
                "ARGLIST"
                "EH-ARG"
                "EH-LOC"
                )
  (:export
                "ADD-INITIALIZATION"
                "ALLOCATE-RESOURCE"
                "ARRAY-INDEXED-P"
                "ARRAY-INITIALIZE"
                "ARRAY-LEADER"
                "ARRAY-LENGTH"
                "COPY-ARRAY-CONTENTS"
                "COPY-ARRAY-PORTION"
                "DEALLOCATE-RESOURCE"
                "DEFRESOURCE"
                "DEFSELECT"
                "DEFSELECT-INCREMENTAL"
                "DEFSUBST"
                "DELETE-INITIALIZATION"
                "FALSE"
                "INITIALIZATIONS"
                "LOCF"
                "MAKE-PROCESS"
                "NAMED-STRUCTURE-INVOKE"
                "NAMED-STRUCTURE-P"
                "NCONS"
                "PROCESS-RUN-FUNCTION"
                "PROCESS-WAIT"
                "PROCESS-WAIT-WITH-TIMEOUT"
                "SELECT-PROCESSOR"
                "STRING-APPEND"
                "STRING-LENGTH"
                "STRING-SEARCH"
                "STRING-SEARCH-CHAR"
                "STRING-SEARCH-SET"
                "SUBSTRING"
                "TIME-DIFFERENCE"
                "TIME-INCREMENT"
                "TIME-LESSP"
                "TRUE"
                "WITH-LOCK"
                "WITHOUT-FLOATING-UNDERFLOW-TRAPS"
                "WITHOUT-INTERRUPTS"
                ;;Flavor stuff for tcp-stream
                "COMPILE-FLAVOR-METHODS"
                "DEFFLAVOR"
                "DEFMETHOD"
                "DEFWRAPPER"
                "LEXPR-SEND"
                "MAKE-INSTANCE"
                "SELF"
                "SEND"
                ;;Debugging stuff
                "ARGLIST"
                "EH-ARG"
                "EH-LOC"
                ))

(defpackage ethernet
  (:use "NET" "LISP"))

(defpackage arp
  (:use "NET" "LISP"))

(defpackage internet
  (:use "NET" "LISP")
  (:nicknames "IP"))

(defpackage icmp
  (:use "IP" "NET" "LISP"))

(defpackage udp
  (:use "IP" "NET" "LISP"))

(defpackage tcp
  (:use "IP" "NET" "LISP"))

(defpackage tcp-application
  (:nicknames "TCPA")
  (:prefix-name "TCPA")
  (:use "TCP" "IP" "NET" "LISP"))

(defpackage ftp
  (:use "TCP-APPLICATION" "TCP" "IP" "NET" "LISP")
  (:shadow "TYPE"))

(defpackage telnet
  (:use "TCP-APPLICATION" "TCP" "IP" "NET" "LISP"))

(defpackage network-user
  ;;Like USER, but has a few functions shadowed...
  (:shadow "LOGOUT" "ED" "FED" "INSPECT" "KERMIT" "PEEK" "SUPDUP" "TELNET" "ZMAIL" "DIRED" "MAIL")
  (:use "GLOBAL"))
