;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-


;;; (A KLUDGE TO GET AROUND HAVING TO MAKE NEW COLD LOADS JUST TO CHANGE THIS VARIABLE -GJC)
;;; 27-Oct-86 10:10:32

;;; Now a feature 21-Aug-87 16:18:35 -gjc


(SETQ INNER-SYSTEM-FILE-ALIST
      '(("SYS: SYS2; DEFSEL QFASL >" "SI" T)    ;By (resource si:named-structure-invoke T)
        ("SYS: SYS2; RESOUR QFASL >" "SI" T)    ;By FILLARRAY (barf T)
        ("SYS: SYS; QMISC QFASL >" "SI" T)
        ("SYS: SYS; DESCRIBE QFASL >" "SI" T)   ;Why not?
        ("SYS: SYS; SORT QFASL >" "SI" T)               ;Needed by FLAVOR
        ("SYS: IO; FORMAT QFASL >" "FORMAT" T)  ;ditto
        ("SYS: IO1; FQUERY QFASL >" "FORMAT" T) ;Needed by everything in sight
        ("SYS: SYS2; HASH QFASL >" "SI" T)      ;Needed by FLAVOR,PATHNM
        ("SYS: SYS2; FLAVOR QFASL >" "SI" T)    ;Needed by PROCES
        ("SYS: SYS2; HASHFL QFASL >" "SI" T)    ;Make flavors really work.
        ("SYS: SYS2; PRODEF QFASL >" "SI" T)    ;Definitions for PROCES
        ("SYS: SYS2; PROCES QFASL >" "SI" T)
        ("SYS: SYS2; NUMER QFASL >" "SI" T)     ;SI:EXPT-HARD needed by PROCES
        ("SYS: DEBUGGER; EH QFASL >" "EH" T)
        ("SYS: DEBUGGER; EHF QFASL >" "EH" T)
        ("SYS: DEBUGGER; TRAP QFASL >" "EH" T)
        ("SYS: DEBUGGER; CONDITION-FLAVORS QFASL >" "EH" T)
        ("SYS: DEBUGGER; EHC QFASL >" "EH" T)
        ("SYS: DEBUGGER; EHBPT QFASL >" "EH" T)
        ("SYS: SYS2; DISASS QFASL >" "COMPILER" T)      ;EH calls subroutines in DISASS
        ("SYS: IO; NEW-DISK QFASL >" "SI" T)
        ("SYS: IO; DISK QFASL >" "SI" T)
        ("SYS: IO; UDISK QFASL >" "SI" T)
        ("SYS: IO; DLEDIT QFASL >" "SI" T)      ; This will init error handler.
        ("SYS: SYS2; LOGIN QFASL >" "SI" T)     ;ditto
        ("SYS: IO; RDDEFS QFASL >" "SI" T)      ;Load this before trying to read any #\'s
        ("SYS: NETWORK; PACKAGES QFASL >" "SI" T)
        ("SYS: NETWORK; HOST QFASL >" "SI" T)
        ("SYS: IO; FILE; ACCESS QFASL >" "FS" T)
        ("SYS: NETWORK; TRANSPARENT QFASL >" "NETWORK" T)
        ("SYS: IO; STREAM QFASL >" "SI" T)
        ("SYS: LAMBDA-DIAG; LAM-PACKAGE QFASL >" "SI" T)

        ;; PATHNM must be the last file in this list.  It breaks things while cold loading
        ;; that QLD knows how to fix after this alist is loaded.
        ("SYS: IO; FILE; PATHNM QFASL >" "FS" T)
        ))

(SETQ REST-OF-PATHNAMES-FILE-ALIST
      '(("SYS: IO; FILE; PATHST QFASL >" "FS" T)
        ("SYS: FILE; LMPARS QFASL >" "FS" T)
        ("SYS: IO; FILE; OPEN QFASL >" "FS" T)

        ;;Share Device
        #+(target lambda) ("SYS: COLD; SYSCONF QFASL >" "SI" T)
        #+(target lambda) ("SYS: SYS; CONFIG-DEFS QFASL >" "SI" T)
        #+(target lambda) ("SYS: SYS; IOMSG QFASL >" "SI" T)
        #+(target lambda) ("SYS: SYS; SHARED-DEVICE QFASL >" "SI" T)
        #+(target lambda) ("SYS: SYS; CONFIG QFASL >" "SI" T)

        ;;Minimal Network
        #+(target lambda) ("SYS: NETWORK; KERNEL; DEFS QFASL >" "NET" T)
        #+(target lambda) ("SYS: NETWORK; KERNEL; LIBRARY QFASL >" "NET" T)
        #+(target lambda) ("SYS: NETWORK; KERNEL; NETWORK-INTERFACE QFASL >" "NET" T)
        #+(target lambda) ("SYS: NETWORK; KERNEL; PACKETS QFASL >" "NET" T)

        #+(target lambda) ("SYS: NETWORK; DRIVERS; LOOPBACK QFASL >" "NET" T)
        #+(target lambda) ("SYS: NETWORK; DRIVERS; SHARE QFASL >" "SI" T)
        ;;;#+(target lambda) ("SYS: NETWORK; DRIVERS; 3COM QFASL >" "ETHERNET" T)
        ;;;#+(target lambda) ("SYS: NETWORK; DRIVERS; EXCELAN QFASL >" "ETHERNET" T)

        #+(target lambda) ("SYS: NETWORK; DRIVERS; DRIVER-PROCESS QFASL >" "NET" T)
        #+(target lambda) ("SYS: NETWORK; KERNEL; NETWORK-PROTOCOL QFASL >" "NET" T)
        #+(target lambda) ("SYS: NETWORK; KERNEL; ARP QFASL >" "NET" T)

        #+(target lambda) ("SYS: NETWORK; CHAOS; CHSNCP QFASL >" "CHAOS" T)
        #+(target lambda) ("SYS: NETWORK; CHAOS; CHUSE QFASL >" "CHAOS" T)
        #+(target lambda) ("SYS: NETWORK; CHAOS; QFILE QFASL >" "FS" T)

        #+(target lambda) ("SYS: NETWORK; KERNEL; CONFIGURE QFASL >" "NET" T)

        ("SYS: IO1; TIME QFASL >" "TIME" T)     ;QFILE needs (encode-universal-time) for file creation dates
        ("SYS: IO1; TIMPAR QFASL >" "TIME" T)   ;and potentially (time:parse-universal-time) if server is bad
        ;;the following just because (time:initialize-timebase) needs (chaos:decode-canonical-time-packet)
        #+(target lambda) ("SYS: NETWORK; CHAOS; CHSAUX QFASL >" "CHAOS" T)
        ))


(SETQ SITE-FILE-ALIST
      '(("SYS: SITE; SITE QFASL >" "SI" T)))


(SETQ HOST-TABLE-FILE-ALIST
      '(("SYS: SITE; HSTTBL QFASL >" "CHAOS" T)
        ("SYS: SITE; LMLOCS QFASL >" "SI" T)
        ("SYS: SITE; SYS TRANSLATIONS >" "FS")))


(SETQ SYSTEM-SYSTEM-FILE-ALIST
      '(("SYS: SYS; QCCOLD QFASL >" "COMPILER") ;Split out from QCDEFS -Keith
        ("SYS: SYS; QCDEFS QFASL >" "COMPILER")
        ("SYS: SYS2; MAKSYS QFASL >" "SI")
        ("SYS: SYS2; PATCH QFASL >" "SI")
        ("SYS: SYS; SYSDCL QFASL >" "SI")))


(TERPRI)
(PRINC "[DONE]")
