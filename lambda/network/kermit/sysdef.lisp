;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

;;Copyright LISP Machine, Inc. 1984, 1985, 1986
;;   See filename "Copyright" for
;;licensing and release information.

;;; These are the Kermit files. They should be loaded in this order usually:
;;; 1. Proto
;;; 2. Calls
;;; 3. <native>-SERIAL
;;; 4. Term
;;; 5. Open
;;; 6. S-term
;;; 7. Server
;;; 8. Window
;;;
;;; PROTO is the basic Columbia University (Frank da Cruz, et al) Kermit protocal.
;;;  It's was translated from the C by Mark David at LMI. Unless otherwise noted,
;;;  the rest of the code for these was written and developed by Mark David largely
;;;  assisted by George Carrette @ LMI in 1984.
;;; CALLS contains the definition of Kstate, the flavor object which 'wraps' the myriad special
;;;  variables and has the methods to make the top level calls to the protocol.
;;; <native>-SERIAL should be specified for each computer; defines OPEN form for serial port.
;;;  This has been added because Lambda-specific serial port was (still is?) scattered.
;;; TERM is the Heath (aka z19, z29, h19, zenith,...) terminal emulator.
;;; OPEN has alot of the functions to open files and hack filenames (for different computers).
;;; S-TERM is George Carrette's remote login interface to the Lisp Machine thru the RS-232.
;;; SERVER is the remote server protocol. It's very minimal. It can be invoked thru s-term.
;;; WINDOW is the window interface to Calls, which in turn is the interface to Proto.
;;;  It also takes care of such vital things as making the serial stream, managing
;;;  everything, etc.

(defpackage kermit
  (:size 500))

(defpackage s-terminal
  (:size 200))

(defsystem kermit
  (:name "Kermit")
  (:patchable "sys:network;kermit;" "KERMIT")
  (:pathname-default "SYS:network;KERMIT;")
  (:warnings-pathname-default "SYS: NETWORK; KERMIT; KERMIT-CWARNS.LISP")
  (:module kermit-protocol "PROTO")
  (:module kermit-calls "CALLS")
  (:module native-serial #+Lambda "lambda-sdu-serial")
  (:module kermit-open "OPEN")
  (:module kermit-server "SERVER")
  (:module kermit-window "WINDOW")
  (:module kermit-terminal "TERM")
  (:module s-terminal "S-TERM")
  (:compile-load kermit-protocol)
  (:compile-load native-serial)
  (:compile-load kermit-calls (:fasload kermit-protocol native-serial))
  (:compile-load kermit-terminal)
  (:compile-load kermit-open (:fasload kermit-protocol))
  (:compile-load s-terminal)
  (:compile-load kermit-open  (:fasload kermit-protocol))
  (:compile-load kermit-server (:fasload kermit-protocol))
  (:compile-load kermit-window
                 (:fasload kermit-protocol kermit-terminal s-terminal kermit-open kermit-calls native-serial))
  )
