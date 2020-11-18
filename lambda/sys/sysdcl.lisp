;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-
;;; Declarations for SYSTEMs initally loaded
;;; ** (c) Copyright 1984 Massachusetts Institute of Technology **

;;; This file has been tailored for the LMI Lambda system.

(DEFSYSTEM SYSTEM
  (:NAME "System")
  (:SHORT-NAME "SYS")
  (:PATCHABLE "SYS: PATCH;")
  (:warnings-pathname-default "SYS:PATCH;SYSTEM-CWARNS.LISP")
  (:MODULE ALLDEFS ("SYS: SYS2; DEFMAC"         ;These are defs files for whole system
                    "SYS: SYS2; LMMAC"
                    "SYS: SYS2; SETF"
                    "SYS: SYS2; STRUCT"
                    "SYS: SYS; TYPES"
                    "SYS: DEBUGGER; ERRMAC"))
  (:COMPONENT-SYSTEMS FONTS             ;Before SYSTEM-INTERNALS because things use fonts
                      SYSTEM-INTERNALS
                      GARBAGE-COLLECTOR
                      FORMAT            ;a NO-OP if loading for first time
                      COMPILER          ;must be before NETWORK
                      SHARE             ;LMI multiprocessing stuff.
                      FILE-SYSTEM
;;;                   QFASL-REL         ;not used in quite a while
                      TIME              ;must be before TV
                      TV                ;Window system
                      FANCY-LANDSCAPE   ;Put back into SYSTEM - others may depend on it.
                      FONT-UTILITIES    ;Pieces of FED needed by rest of system
                      PEEK              ;System Status Utility
                      ;;Here because TELNET needs TV: flavors, and everything makes PEEK objects...
                      NETWORK           ;Kernel, TCP/IP, Chaos, etc.
                      SUPDUP
;;;                   ZWEI              ;not-optional, but we want it to be separately patchable
;;;                   FED               ;optional -- loadable by user
;;;                   COLOR
                      EH
;;;                   PRESS             ;moved to mit-specific
                      MATH              ;anybody using this lately?
;;;                   HACKS             ;Optional -- loadable by user
                      METER
                      SRCCOM
;;;                   CONVERSE          ;Needs Zwei, load on top of system
;;;                   ISPELL            ;optional -- loadable by user
                      SERIAL
                      )
  (:MODULE LAST "SYS: SYS; LAST")
  (:COMPILE-LOAD ALLDEFS)
  (:DO-COMPONENTS (:FASLOAD ALLDEFS))
  (:COMPILE-LOAD LAST))

(DEFSYSTEM SYSTEM-INTERNALS
  ;(:PACKAGE SYSTEM-INTERNALS)
  (:NICKNAMES "SI")
  (:MODULE DEFS ("SYS: SYS2; PRODEF"
                 "SYS: IO; RDDEFS"
                 "SYS: SYS2; SGDEFS"
                 "SYS: SYS2; NUMDEF"
                 "SYS: SYS; STORAGE-DEFS"))
  (:MODULE METH "SYS: SYS2; METH")
  (:MODULE CLASS "SYS: SYS2; CLASS")
  (:MODULE MAIN ("SYS: SYS2; ADVISE"
                 "SYS: SYS2; BAND"              ;recompile this if structures in CHSNCP change!!!
                 "SYS: SYS2; CHARACTER"
                 "SYS: SYS; CLPACK"             ;packages
                 "SYS: WINDOW; COLD"
                 "SYS: SYS2; DEFSEL"            ;defselect
                 "SYS: IO; NEW-DISK"
                 "SYS: IO; DISK"
                 "SYS: IO; UDISK"
                 "SYS: SYS; DESCRIBE"
                 "SYS: IO; DLEDIT"              ;disk-label editor
                 "SYS: IO; DRIBBL"              ;dribble
                 "SYS: SYS2; ENCAPS"            ;encapsulations
                 "SYS: SYS; EVAL"
                 "sys: sys; special-forms"      ;moved from EVAL
                 "SYS: SYS2; FLAVOR"
                 "SYS: SYS; GENRIC"             ;new commonlisp functions
                 "SYS: COLD; GLOBAL"
                 "SYS: IO; GRIND"
                 "SYS: IO1; HARDCOPY"
                 "SYS: SYS2; HASH"
                 "SYS: SYS2; HASHFL"            ;flavorized hash table stuff
                 "SYS: NETWORK; PACKAGES"       ; network package definitions
                 "SYS: NETWORK; HOST"           ; Still in SYSTEM-INTERNALS (for now)
                 "SYS: IO1; INC"                ; Will take lots of work to do this again, however, some functions are called.
                 "SYS: IO1; INFIX"
                 "SYS: IO; INPUT-READERS"
                 "SYS: LAMBDA-DIAG; LAM-PACKAGE"
                 "SYS: COLD; LISP"
                 "SYS: SYS2; LOGIN"
                 "SYS: SYS2; LOOP"
                 "SYS: SYS; LTOP"
                 "SYS: SYS2; MACARRAY"          ;mucklisp array crud
                 "SYS: SYS2; MAKSYS"
                 "SYS: SYS2; NUMER"
                 "SYS: SYS2; PATCH"
                 "SYS: SYS2; PLANE"
                 "SYS: IO; PRINT"
                 "SYS: SYS2; PROCES"
                 "SYS: IO; QIO"
                 "sys: sys; qccold"             ; $$$ Split off from QCDEFS <27-Oct-88 keith>
                 "sys: sys; qcdefs"
                 "SYS: SYS; QFASL"
                 "SYS: SYS; QFCTNS"
                 "SYS: SYS; QMISC"
                 "SYS: SYS; QNEW"
                 "SYS: SYS; QRAND"
                 "SYS: SYS2; QTRACE"            ;trace
                 "SYS: SYS2; RAT"
                 "SYS: IO; READ"
                 "SYS: SYS; REP"                ;read-eval-print loop
                 "SYS: SYS2; RESOUR"            ;resources
                 "SYS: SYS2; SELEV"
                 "SYS: SYS; SGFCTN"
                 "SYS: SYS; SORT"
                 "SYS: SYS; SPDWIM"             ;Spelling dwim.
                 "SYS: SYS2; STEP"
                 "SYS: SYS; STORAGE"
                 "SYS: IO; STREAM"
                 "SYS: SYS2; STRING"
                 "SYS: SYS; SYSDCL"
                 "SYS: COLD; SYSTEM"
                 "SYS: SYS2; UNFASL"
                 "SYS: IO; UNIBUS"
                 "SYS: SYS2; CLMAC"             ;alternate macro definitions for some zl special forms
                 "SYS: SYS2; ANALYZE"
                 "SYS: NETWORK; TABLE"
                 "SYS: NETWORK; TRANSPARENT"
                 ))
;;; Don't reload readtables -- they are built specially into the Cold Load.  Loading them again
;;; causes screwups, as they will set global:*readtable* and si:common-lisp-readtable, but the
;;; variables si:initial-readtable, si:initial-common-lisp-readtable, si:standard-read-table,
;;; and si:*all-readtables* never get updated.
  (:MODULE RDTBL ("SYS: IO; RDTBL" "SYS: IO; CRDTBL"))
  (:MODULE EXPORT ("SYS: COLD; EXPORT"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
;;;  (:FASLOAD RDTBL)
  (:READFILE EXPORT)
  (:COMPILE-LOAD METH (:FASLOAD DEFS))
  (:COMPILE-LOAD CLASS (:FASLOAD DEFS) (:FASLOAD METH)))


(DEFSYSTEM FONTS
  (:PACKAGE FONTS)
  (:PATHNAME-DEFAULT "SYS: FONTS;")
  (:FASLOAD ("TVFONT" "CPTFONTB" "CPTFONT" "CPTFONTSH" "BIGFNT" "TINY" "5X5"
             "MEDFNT" "MEDFNB" "METS" "METSI" "43VXMS"
             "HL6" "HL7" "HL10" "HL10B" "HL12" "HL12I" "HL12B" "HL12BI"
             "TR8" "TR8I" "TR8B" "TR10" "TR10I" "TR10B" "TR10BI"
             "TR12" "TR12B" "TR12I" "TR12BI"
             "MOUSE" "SEARCH" "TOG" "ABACUS" "NARROW"))
  ;(:READFILE ("EQUIVALENCE")) dover randomness from MIT
  )

(defsystem network
  (:name "Network System")
  (:short-name "Network")
  (:pathname-default "sys: network; kernel;")
  (:component-systems network-drivers chaos ip-tcp)
  (:module defs "DEFS")
  (:module library "LIBRARY")
  (:module network-interface ("NETWORK-INTERFACE"))
  (:module packets ("PACKETS"))
  (:module network-protocol ("NETWORK-PROTOCOL"))
  (:module arp ("ARP"))
  (:module transport-protocol ("TRANSPORT-PROTOCOL"))
  (:module configuration "CONFIGURE")
  (:module netspy "NETSPY")
  (:compile-load defs)
  (:compile-load-init library defs (:fasload defs) (:fasload defs))
  (:compile-load-init network-interface defs (:fasload defs library) (:fasload defs library))
  (:compile-load-init packets (defs network-interface)
                      (:fasload defs library network-interface) (:fasload defs library network-interface))
  (:compile-load-init network-protocol (defs network-interface)
                      (:fasload defs network-interface packets) (:fasload defs network-interface packets))
  (:compile-load-init arp (defs network-interface network-protocol)
                      (:fasload defs network-protocol) (:fasload network-protocol))
  (:compile-load-init transport-protocol (defs network-interface network-protocol)
                      (:fasload defs arp) (:fasload arp))
  (:do-components (:fasload defs library network-interface packets network-protocol arp transport-protocol))
  (:compile-load netspy)
  (:compile-load configuration)
  )

(defsystem network-drivers
  (:name "Network Device Drivers")
  (:short-name "Network-Drivers")
  (:pathname-default "SYS: NETWORK; DRIVERS;")
  (:module drivers ("LOOPBACK"
                    "3COM"
                    "EXCELAN"
                    "SHARE"
;;;                 "EXPLORER"
;;;                 "SYS: LAMBDA-E-NET; NU-ETHER"       ;TI Explorer Ethernet
;;;                 "CADR"
                    "DRIVER-PROCESS"))
  (:compile-load drivers)
  )

(defsystem ip-tcp
  (:name "IP-TCP")
  (:pathname-default "SYS: NETWORK; IP-TCP; KERNEL;")
  (:component-systems tcp-user tcp-server)      ;User must come before Server
  (:module ip ("IP"))
  (:module icmp ("ICMP"))
  (:module udp ("UDP"))
  (:module udp-stream ("UDP-STREAM"))
  (:module tcp ("TCP"))
  (:module tcp-stream ("TCP-STREAM"))
  (:module ports ("PORTS"))
  (:module easy ("EASY"))
  (:module tcp-udp-server ("GENERIC-SERVER"))
  (:compile-load ip)
  (:compile-load-init icmp (ip) (:fasload ip) (:fasload ip))
  (:compile-load-init udp (ip) (:fasload ip) (:fasload ip))
  (:compile-load-init tcp (ip) (:fasload ip) (:fasload ip))
  (:compile-load-init udp-stream (udp) (:fasload udp) (:fasload udp))
  (:compile-load-init tcp-stream (tcp) (:fasload tcp) (:fasload tcp))
  (:compile-load ports)
  (:compile-load-init easy (tcp-stream udp-stream ports)
                      (:fasload tcp-stream udp-stream ports) (:fasload tcp-stream udp-stream ports))
  (:compile-load-init tcp-udp-server (tcp udp tcp-stream udp-stream)
                      (:fasload tcp-stream udp-stream) (:fasload tcp-stream udp-stream))
  (:do-components (:fasload ip icmp udp udp-stream tcp tcp-stream ports easy tcp-udp-server))
  )

(defsystem tcp-server
  (:name "TCP-SERVER")
  (:pathname-default "SYS: NETWORK; IP-TCP; SERVER;")
  (:module udp-server-applications ("UDP-RWHO-SERVER"))
  (:module termcap "TERMCAP")
  (:module telnet-utilities "TELNET-UTILITIES")
  (:module telnet "TELNET")
  (:module ftp "FTP")
  (:module smtp "SMTP")
  (:module disk "DISK")
  (:module time "TIME")
  (:module finger "FINGER")
  (:module all (telnet ftp smtp disk time finger))
  (:compile-load udp-server-applications)
  (:compile-load termcap)
  (:compile-load telnet-utilities (:fasload termcap) (:fasload termcap))
  (:compile-load all (:fasload telnet-utilities) (:fasload telnet-utilities))
  )

(defsystem tcp-user
  (:name "TCP-USER")
  (:pathname-default "SYS: NETWORK; IP-TCP; USER;")
  (:module simple-user-applications ("TCP-TEST" "IMAGEN"))
  (:module defs ("FTP-SYMS"))
  (:module telnet ("TELNET"))
  (:module ftp ("FTP"))
  (:module chaos ("CHAOS-SERVER"))
  (:module smtp ("SMTP"))
  (:module all (telnet ftp chaos smtp))
  (:compile-load simple-user-applications)
  (:compile-load defs)
  (:compile-load all (:fasload defs) (:fasload defs)))

(DEFSYSTEM CHAOS
  ;(:PACKAGE CHAOS)
  (:PATHNAME-DEFAULT "SYS: NETWORK; CHAOS;")
  (:MODULE NCP ("CHSNCP" "CHUSE"))
  (:MODULE AUX "CHSAUX")
;;; (:MODULE TEST "CHATST")
;;; (:MODULE EFTP "EFTP")
  (:COMPILE-LOAD (NCP AUX #|TEST EFTP|#))
;;;a defsystem bug keeps (make-system 'system :recompile) from working if this is present!
;  (:COMPILE-LOAD (:GENERATE-HOST-TABLE (("SYS: CHAOS; HOSTS" "SYS: SITE; HSTTBL"))))
  )

(defsystem share
  (:name "Share")
  (:pathname-default "sys: sys;")
  (:package si)
  (:module defs ("sys: cold; sysconf"))
  (:module macros ("config-defs"))
  (:module main ("iomsg"
                 "shared-device"
                 "config"                       ;last file in system ... turns it all on
                 ))
  (:compile-load defs)
  (:compile-load macros (:fasload defs))
  (:compile-load main (:fasload defs) (:fasload macros)))

(DEFSYSTEM SITE
  ;(:PACKAGE SYSTEM-INTERNALS)
  (:MODULE SITE ("SYS: SITE; SITE" "SYS: SITE; LMLOCS"))
  (LOAD-SITE-FILE (:COMPILE SITE))
  (:MODULE HOST-TABLE (("SYS: CHAOS; HOSTS" "SYS: SITE; HSTTBL")) #|:PACKAGE CHAOS|#)
  (LOAD-SITE-FILE (:COMPILE (:GENERATE-HOST-TABLE HOST-TABLE))))

(DEFSYSTEM TIME
  ;(:PACKAGE TIME)
  (:PATHNAME-DEFAULT "SYS: IO1;")
  (:COMPILE-LOAD ("TIME" "TIMPAR")))

(DEFSYSTEM SUPDUP
  (:COMPILE-LOAD ("SYS: NETWORK; SUPDUP")))

(DEFSYSTEM FORMAT
  ;(:PACKAGE FORMAT)
  (:COMPILE-LOAD ("SYS: IO; FORMAT"
                  ;"SYS: IO; FORMAT-MACRO"
                  "SYS: IO1; FQUERY"
                  "SYS: IO1; OUTPUT")))

;(DEFSYSTEM QFASL-REL
;  ;(:PACKAGE QFASL-REL)
;  (:PATHNAME-DEFAULT "SYS: IO1;")
;  (:COMPILE-LOAD ("RELLD" "RELDMP")))

(DEFSYSTEM COMPILER
  (:PACKAGE COMPILER)
  (:MODULE DEFS ("sys: sys; qccold" "SYS: SYS; QCDEFS"))        ; $$$ Split these two up for cold load <27-Oct-88 keith>
  (:MODULE MAIN ("SYS: SYS2; DISASS"
                 "SYS: SYS; QCFASD"
                 "SYS: SYS; QCFILE"
                 "SYS: SYS; QCP1"
                 "SYS: SYS; QCP2"
                 "SYS: SYS; QCOPT"
                 "SYS: SYS; QCLUKE"
                 "SYS: SYS; QCPEEP"
                 "SYS: SYS; QCLAP"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:READFILE ("SYS: COLD; DEFMIC"
              "SYS: COLD; DOCMIC")))

(DEFSYSTEM COLOR
  ;(:PACKAGE COLOR)
  (:COMPILE-LOAD ("SYS: WINDOW; COLOR")))

(DEFSYSTEM ZWEI
  ;(:PACKAGE ZWEI)
  (:PATHNAME-DEFAULT "SYS: ZWEI;")
  (:patchable "sys: zwei; patch;")
  (:NOT-IN-DISK-LABEL)
  (:warnings-pathname-default "SYS: ZWEI; PATCH; ZWEI-CWARNS.LISP")
  (:MODULE DEFS ("DEFS"                         ;Structure definitions and declarations.
                 "MACROS"))                     ;Lisp macros used in the ZWEIs source.
  (:MODULE SCREEN ("SCREEN"))                   ;Interface to screen system
  (:MODULE MAIN ("COMTAB"                       ;Functions regarding comtabs and command loop.
                 "DISPLA"                       ;Redisplay, and screen-related functions.
                 "FOR"                          ;Forward-this, forward-that functions.
                 "INDENT"                       ;Indention functions
                 "INSERT"                       ;Insertion and deletion, and related functions
                 "METH"                         ;Important methods for windows and buffers.
                 "PRIMIT"                       ;Random primitives and utilities.
                 "NPRIM"                        ;More recently written primitives
                 "HISTORY"                      ;Kill history, mini buffer history, etc.
                 "FONT"                         ;Font hacking stuff
                 "KBDMAC"                       ;Keyboard macro stream
                 "SEARCH"                       ;Searching functions

                 "COMA"                         ;Vanilla commands.
                 "COMB"                         ;More vanilla commands.
                 "COMC"                         ;Yet more vanilla commands.
                 "COMD"                         ;Still more vanilla commands.
                 "COME"                         ;Even more vanilla commands.
                 "COMF"                         ;More and more vanilla commands
                 "COMG"                         ;And more vanilla commands
                 "COMH"
                 "COMS"                         ;Searching and replacing commands.
                 "DIRED"                        ;Directory editor.
                 "BDIRED"                       ;Directory Differences editor.
                 "DOC"                          ;Self-documentation commands and functions.
                 "FASUPD"                       ;Update fasl file from core.
                 "FILES"                        ;File commands and utilities.
                 "HOST"                         ;Define ED:, ED-BUFFER:, ED-FILE: hosts.
                 "NEW-ISPELL"                   ;spelling corrector
                 "LPARSE"                       ;Parsing lisp code.
                 "MODES"                        ;Major and minor mode functions and commands
                 "MOUSE"                        ;Mouse commands less screen interface
                 "PATED"                        ;Patch commands.
                 "PL1MOD"                       ;PL/I mode commands.
                 "POSS"                         ;Visiting lists of things
                 "STREAM"                       ;Editor stream

                 "SECTIO"                       ;Some section specific command for ZMACS
                 "ZMNEW"
                 "ZMACS"))                      ;Multiple-buffer and file commands.

  (:MODULE ZYMURG ("ZYMURG"))                   ;Combined methods.

  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD-init SCREEN defs (:FASLOAD DEFS))       ; Recompile if DEFS has been recompiled
  (:COMPILE-LOAD-init MAIN defs (:FASLOAD DEFS SCREEN))
  (:COMPILE-LOAD-init ZYMURG defs (:FASLOAD DEFS SCREEN MAIN)))

(defsystem font-utilities
  (:module defs "sys: io1; fntdef")             ;Font definition
  (:module main ("sys: io1; fntcnv"             ;Font <-> Font Descriptor Tools
                 "sys: window; font-utilities"));Utility functions
  (:readfile defs)
  (:compile-load main (:readfile defs)))

;;;Font-Editor changed:
;;; 1) not part of SYSTEM system - user option
;;; 2) system now called FONT-EDITOR, FED is a nickname
;;; 3) moved files from SYS:WINDOW; to SYS:WINDOW;FONT-EDITOR;
;;; 4) .SYSDEF now separate file:

(si:set-system-source-file :font-editor "sys:window;font-editor;sysdef")

;(DEFSYSTEM PRESS
;  (:PACKAGE PRESS)
;  (:PATHNAME-DEFAULT "SYS: IO1;")
;  (:MODULE RFONTW "RFONTW")
;  (:MODULE PRESS "PRESS")
;  (:MODULE FONTW "PRESS-FONTS; FONTS WIDTHS >")
;  (:COMPILE-LOAD RFONTW)
;  (:COMPILE-LOAD PRESS)
;  (:LOAD-FONTS-WIDTHS FONTW (:FASLOAD RFONTW)))

;;; error handler, debugger
(DEFSYSTEM EH
  (:NICKNAMES "Debugger" "DBG")
  ;(:PACKAGE EH)
  (:PATHNAME-DEFAULT "SYS: DEBUGGER;")
  (:COMPILE-LOAD ("EH" "EHF" "TRAP" "CONDITION-FLAVORS" "EHC" "EHBPT" "EHW")))

(DEFSYSTEM GARBAGE-COLLECTOR
  (:short-name "GC")
  (:module defs ("SYS: SYS; STORAGE-DEFS"))
  (:module main ("SYS: SYS2; GC"))
  (:compile-load defs)
  (:compile-load main (:fasload defs)))

(DEFSYSTEM TV
  ;(:PACKAGE TV)
  (:PATHNAME-DEFAULT "SYS: WINDOW;")
  (:MODULE DEFS "TVDEFS")                       ;DEFVARS, FONT, IO-BUFFER; Flavors: SHEET SCREEN BLINKER
  (:MODULE MAIN ("SCRMAN"                       ;Screen manager
                 "SHEET"                        ;Sheets: methods.  Blinkers
                 "SHWARM"                       ;Sheets: methods, low level primitives.  Screens: methods
                 "BASWIN"                       ;Windows: flavors & methods.
                 "WHOLIN"                       ;Who line
                 "MOUSE"                        ;Mouse
                 "BASSTR"                       ;IO buffers, Keyboard process
                 "STREAM"                       ;Stream operations, default rubout handler
                 "GRAPHICS"                     ;Graphics-mixin
                 "MENU"                         ;Menus
                 "COMETH"                       ;Compile-flavor-methods, initialize, define some resources
                 ;; The above must be loaded before any windows get created
                 "SYSMEN"                       ;System menu
                 "SCRED"                        ;Screen editor as called by system menu
                 "TYPWIN"                       ;Typeout windows and mouse sensitive items
                 "SCROLL"                       ;Scroll windows
                 "TSCROL"                       ;Text scroll windows
                 "FRAME"                        ;Frames, panes, constraint frames
                 "CHOICE"                       ;Margin regions & scrolling, choice box menus
                 "CSRPOS"                       ;Maclisp compatible (cursorpos)
                 "INSPCT"                       ;The inspector
                 "RH"))                         ;The alternate (normal) rubout handler
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS) (:FASLOAD DEFS)))

(DEFSYSTEM PEEK
  ;(:PACKAGE TV)
  (:PATHNAME-DEFAULT "SYS: WINDOW;")
  (:MODULE MAIN "PEEK")
  (:MODULE CHAOS "SYS: NETWORK; CHAOS; PEEKCH" #|:PACKAGE CHAOS|#)
  (:MODULE NETSTAT "SYS: NETWORK; KERNEL; NETSTAT" #|:PACKAGE NETWORK|#)
  (:MODULE FILE "PEEKFS" #|:PACKAGE FS|#)
  (:COMPILE-LOAD MAIN)
  (:COMPILE-LOAD CHAOS)
  (:COMPILE-LOAD NETSTAT)
  (:COMPILE-LOAD FILE))

(DEFSYSTEM FILE-SYSTEM
  ;(:PACKAGE FILE-SYSTEM)
  (:NICKNAMES "FS")
  (:PATHNAME-DEFAULT "SYS: IO; FILE;")
  (:MODULE BASIC-PATHNAMES ("ACCESS" "PATHNM"))
  (:MODULE HOST-PATHNAMES ("PATHST" "SYS: FILE; LMPARS"))
  (:MODULE FILE-IO ("OPEN" "BALDIR"))
  (:MODULE CHAOS-FILE-IO ("SYS: NETWORK; CHAOS; QFILE"))
  (:MODULE TCP-FILE-IO ("SYS: NETWORK; IP-TCP; USER; FTP-ACCESS"))
  (:COMPILE-LOAD BASIC-PATHNAMES)
  (:COMPILE-LOAD HOST-PATHNAMES (:FASLOAD BASIC-PATHNAMES))
  (:COMPILE-LOAD FILE-IO (:FASLOAD BASIC-PATHNAMES))
  (:COMPILE-LOAD CHAOS-FILE-IO (:FASLOAD  BASIC-PATHNAMES HOST-PATHNAMES))
  (:COMPILE-LOAD TCP-FILE-IO (:FASLOAD BASIC-PATHNAMES HOST-PATHNAMES)))

(DEFSYSTEM MATH
  ;(:PACKAGE MATH)
  (:COMPILE-LOAD ("SYS: SYS2; MATRIX")))

;;; Random use programs and demos
(DEFSYSTEM HACKS
  (:PACKAGE HACKS)
  (:PATHNAME-DEFAULT "SYS: DEMO;")
  (:warnings-pathname-default "SYS: DEMO; HACKS-CWARNS.LISP")
  (:MODULE DEFS "HAKDEF")
  (:MODULE MAIN ("ABACUS" "ALARM" "BEEPS"
;;;              "CAFE" "COLXOR" "COLORHACK" ;leave out color stuff
                 "CROCK"
                 "DC" "DEUTSC" "DLWHAK" "DOCTOR" "DOCSCR"
                 "GEB" "HCEDIT" "MUNCH" "OHACKS" "ORGAN" "QIX"
                 "ROTATE" "ROTCIR" "WORM" "WORM-TRAILS"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:FASLOAD ("TVBGAR" "WORMCH")))

;;; Source compare
(DEFSYSTEM SRCCOM
  ;(:PACKAGE SRCCOM)
  (:COMPILE-LOAD ("SYS: IO1; SRCCOM")))

(DEFSYSTEM METER
  ;(:PACKAGE METER)
  (:COMPILE-LOAD ("SYS: IO1; METER")))

;;; Interactive message program
(DEFSYSTEM CONVERSE
  ;(:PACKAGE ZWEI)
  (:COMPILE-LOAD ("SYS: IO1; CONVER")))

(defsystem ispell
  (:warnings-pathname-default "SYS: ZWEI; ISPELL-CWARNS.LISP")
  (:module main ("sys:zwei;spell-check"))
  (:compile-load main))

(defsystem serial
  (:pathname-default "SYS:IO1;")
  (:module devices (
;;;                 "SERIAL"                    ; for CADR
                    "SDU-SERIAL"                ; for LAMBDA
;;;                 "EXP-PRINTER-SUPPORT"       ; for explorer
;;;                 "EXP-PRINTER-HOST"          ; for explorer
                    ))
  (:compile-load devices))

(defsystem fancy-landscape
  (:pathname-default "sys:window;")
  (:warnings-pathname-default "SYS: WINDOW; FANCY-LANDSCAPE-CWARNS.LISP")
  (:module control-panel   "control-panel")
  (:module gauge           "gauge")
  (:module fancy-landscape "fancy-landscape")
  (:compile-load control-panel)
  (:compile-load gauge)
  (:compile-load fancy-landscape (:fasload control-panel gauge)))

;;; Systems not initially loaded, but done right afterwards
;;; MIT-Specific definition moved to SYS: SITE; MIT-SPECIFIC SYSTEM

#|                                              ;obsolete....
(DEFSYSTEM CADR
  (:NAME "CADR")
  (:PATHNAME-DEFAULT "SYS:CC")
  (:PATCHABLE "SYS: PATCH")
  (:INITIAL-STATUS :RELEASED)
  (:NOT-IN-DISK-LABEL)
  ;(:PACKAGE CADR)
  (:COMPONENT-SYSTEMS CADR-MICRO-ASSEMBLER CADR-DEBUGGER))

(DEFSYSTEM CADR-DEBUGGER
  ;(:PACKAGE CADR)
  (:NICKNAMES "CC")
  (:PATHNAME-DEFAULT "SYS: CC")
  (:MODULE DEFS ("SYS: CC LQFMAC"
                 "SYS: CC LCADMC"))
  (:MODULE MAIN ("SYS: CC CC"
                 "SYS: CC CCGSYL"
                 "SYS: CC LCADRD"
                 "SYS: CC DIAGS"
                 "SYS: CC DMON"
                 "SYS: CC LDBG"
                 "SYS: CC ZERO"
                 "SYS: CC CADLD"
                 "SYS: CC QF"
                 "SYS: CC CCWHY"
                 "SYS: CC CCDISK"
                 "SYS: CC DCHECK"
                 "SYS: CC CHPLOC"
                 "SYS: CC SALVAG"))
  (:COMPILE-LOAD DEFS)
  (:READFILE ("SYS: CC CADREG"))
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))

(DEFSYSTEM CADR-MICRO-ASSEMBLER
  ;(:PACKAGE MICRO-ASSEMBLER)
  (:NICKNAMES "CADR-MICRO-ASSEMBLER")
  (:MODULE ASS "SYS: SYS CADRLP")
  (:MODULE MAIN ("SYS: SYS CDMP"
                 "SYS: SYS QWMCR"
                 "SYS: IO FREAD"
                 "SYS: SYS2 USYMLD"))
  (:COMPILE-LOAD ASS)
  (:READFILE ("SYS: COLD QCOM"
              "SYS: COLD DEFMIC"
              "SYS: SYS CADSYM")
             (:FASLOAD ASS))
  (:COMPILE-LOAD MAIN))

(SET-SYSTEM-SOURCE-FILE "Cadr UCODE" "SYS: UCADR UCODE")
|#

(DEFSYSTEM ZMAIL
  (:NAME "ZMail")
  (:PATHNAME-DEFAULT "SYS: ZMAIL;")
  (:SHORT-NAME "ZM")
  (:PATCHABLE "SYS: ZMAIL; PATCH;")
  (:NOT-IN-DISK-LABEL)
  (:warnings-pathname-default "SYS: ZMAIL; PATCH; ZMAIL-CWARNS.LISP")
  ;(:PACKAGE ZWEI)
  (:MODULE DEFS "DEFS")
  (:MODULE TV ("MULT" "BUTTON") #|:PACKAGE TV|#)
  (:MODULE MFHOST ("MFHOST" "MFHOST2"))
  (:MODULE MAIN ("TOP" "MFILES" MFHOST "REFER" "LMFILE"
                 "COMNDS" "MAIL" "WINDOW" "FILTER" "PROFIL" "SYS:FILE;ZMAIL" TV))
  (:MODULE COMETH "COMETH")
; (:MODULE PARSE "PARSE")
  (:MODULE RFC733 "RFC733")
  (:MODULE LEX733 "LEX733")
; (:MODULE FONTS "NARROW")      ;now in fonts system
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
; (:COMPILE-LOAD PARSE)
  (:COMPILE-LOAD RFC733
;                (:FASLOAD PARSE)
                 )
; (:RTC-LOAD LEX733)            ;Someday
  (:FASLOAD LEX733)
; (:FASLOAD FONTS)
  (:COMPILE-LOAD COMETH (:FASLOAD DEFS MAIN)))

(defsystem unix
  (:name "Unix-Interface")
  (:short-name "unix")
  ;(:package unix)
  (:pathname-default "sys: unix;")
  (:patchable nil "unix-patch")
  (:NOT-IN-DISK-LABEL)
  (:warnings-pathname-default "SYS: UNIX; UNIX-INTERFACE-CWARNS.LISP")
  (:module main ("lamtty" "sdu-utils"))
  (:compile-load main))

;;; Systems defined elsewhere.

(SET-SYSTEM-SOURCE-FILE :FILE-SYSTEM-UTILITIES "SYS: FILE; FS")
(SET-SYSTEM-SOURCE-FILE :LOCAL-FILE "SYS: FILE; FS")
(SET-SYSTEM-SOURCE-FILE :FILE-SERVER "SYS: FILE; FS")

(SET-SYSTEM-SOURCE-FILE :COLD "SYS: COLD; COLDPK")


;;; These are the files in the cold load
#|                                              ;Start of CADR list
(DEFPARAMETER COLD-LOAD-FILE-LIST
              '("SYS: FONTS; CPTFON QFASL >"
                "SYS: SYS; QRAND QFASL >"
                "SYS: IO; QIO QFASL >"
;;;             "SYS: IO; RDTBL QFASL >"        ;done specially
;;;             "SYS: IO; CRDTBL QFASL >"       ;done specially
                "SYS: IO; READ QFASL >"
                "SYS: IO; INPUT-READERS >"
                "SYS: IO; PRINT QFASL >"
                "SYS: WINDOW; COLD QFASL >"
                "SYS: SYS; SGFCTN QFASL >"
                "SYS: SYS; EVAL QFASL >"
                "SYS: SYS; SPECIAL-FORMS QFASL >"       ;moved from EVAL
                "SYS: SYS; TYPES QFASL >"
                "SYS: SYS; LTOP QFASL >"
                "SYS: SYS; REP QFASL >"
                "SYS: SYS; QFASL QFASL >"
                "SYS: IO; MINI QFASL >"
                "SYS: SYS; QFCTNS QFASL >"
                "SYS: SYS2; STRING QFASL >"
                "SYS: SYS2; CHARACTER QFASL >"
                "SYS: SYS; GENRIC QFASL >"
                "SYS: SYS; CLPACK QFASL >"
                "SYS: COLD; GLOBAL QFASL >"
                "SYS: COLD; SYSTEM QFASL >"
                "SYS: COLD; LISP QFASL >"))
|#                                              ;End of CADR list

(DEFPARAMETER LAMBDA-COLD-LOAD-FILE-LIST
              '("SYS: FONTS; CPTFON QFASL"
                "SYS:COLD;FIRST QFASL >"
                "SYS: SYS; STORAGE QFASL >"
                "SYS: SYS; QRAND QFASL >"
                "SYS: IO; QIO QFASL >"
;;;             "SYS: IO; RDTBL QFASL >"        ;done specially
;;;             "SYS: IO; CRDTBL QFASL >"       ;done specially
                "SYS: IO; READ QFASL >"
                "SYS: IO; INPUT-READERS QFASL >"
                "SYS: IO; PRINT QFASL >"
                "SYS: WINDOW; COLD QFASL >"
                "SYS: SYS; SGFCTN QFASL >"
                "SYS: SYS; EVAL QFASL >"
                "SYS: SYS; SPECIAL-FORMS QFASL >"       ;moved from EVAL
                "SYS: SYS; TYPES QFASL >"
                "SYS: SYS; LTOP QFASL >"
                "SYS: SYS; REP QFASL >"
                "sys: sys; qccold qfasl >"      ; $$$ Compiler supports QFASL  <27-Oct-88 keith>
                "SYS: SYS; QFASL QFASL >"
;;; NETWORKING TAKEN OUT 19-May-86 10:55:29 -GJC
;;;         "SYS: NETWORK; CHAOS; ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; NU-ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; 3COM-ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; SHARE-ETHERNET-MINI QFASL >"
;;; REPLACED WITH:
                "SYS:COLD;STREAM QFASL"
                "SYS:COLD;MINI QFASL"
;;;
                "SYS: SYS; QFCTNS QFASL >"
                "SYS: SYS2; STRING QFASL >"
                "SYS: SYS2; CHARACTER QFASL >"
                "SYS: SYS; GENRIC QFASL >"
                "SYS: SYS; CLPACK QFASL >"
                "SYS: COLD; GLOBAL QFASL >"
                "SYS: COLD; SYSTEM QFASL >"
                "SYS: COLD; LISP QFASL >"))

(DEFPARAMETER FALCON-COLD-LOAD-FILE-LIST        ;for starters..
              '("SYS: FONTS; CPTFON QFASL"
                "SYS:COLD;FIRST QFASL >"
                "SYS: SYS; STORAGE QFASL >"
                "SYS: SYS; QRAND QFASL >"
                "SYS: IO; QIO QFASL >"
;;;             "SYS: IO; RDTBL QFASL >"        ;done specially
;;;             "SYS: IO; CRDTBL QFASL >"       ;done specially
                "SYS: IO; READ QFASL >"
                "SYS: IO; INPUT-READERS QFASL >"
                "SYS: IO; PRINT QFASL >"
                "SYS: WINDOW; COLD QFASL >"
                "SYS: SYS; SGFCTN QFASL >"
                "SYS: SYS; EVAL QFASL >"
                "SYS: SYS; SPECIAL-FORMS QFASL >"       ;moved from EVAL
                "SYS: SYS; TYPES QFASL >"
                "SYS: SYS; LTOP QFASL >"
                "SYS: SYS; REP QFASL >"
                "sys: sys; qccold qfasl >"      ; $$$ Compiler supports QFASL  <27-Oct-88 keith>
                "SYS: SYS; QFASL QFASL >"
;;; NETWORKING TAKEN OUT 19-May-86 10:55:29 -GJC
;;;         "SYS: NETWORK; CHAOS; ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; NU-ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; 3COM-ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; ETHER-MINI QFASL >"
;;;         "SYS: LAMBDA-E-NET; SHARE-ETHERNET-MINI QFASL >"
;;; REPLACED WITH:
                "SYS:COLD;STREAM QFASL"
                "SYS:COLD;MINI QFASL"
;;;
                "SYS: SYS; QFCTNS QFASL >"
                "SYS: SYS2; STRING QFASL >"
                "SYS: SYS2; CHARACTER QFASL >"
                "SYS: SYS; GENRIC QFASL >"
                "SYS: SYS; CLPACK QFASL >"
                "SYS: COLD; GLOBAL QFASL >"
                "SYS: COLD; SYSTEM QFASL >"
                "SYS: COLD; LISP QFASL >"))

;; Note: These are defined here now as NIL,
;;       actual definitions moved to SYS:SYS;INNER-SYSTEM-FILE-ALIST
;;       as SETQ's 21-Aug-87 16:09:34 -gjc

(DEFVAR INNER-SYSTEM-FILE-ALIST NIL)

(DEFVAR REST-OF-PATHNAMES-FILE-ALIST NIL)

(DEFVAR SITE-FILE-ALIST NIL)

(DEFVAR HOST-TABLE-FILE-ALIST NIL)

(defsystem system-macros
  (:module main ("SYS: SYS2; DEFMAC"
                 "SYS: SYS2; LMMAC"
                 "SYS: SYS2; STRUCT"
                 "SYS: SYS2; SETF"
                 "SYS: SYS; TYPES"
                 "SYS: SYS; STORAGE-DEFS"
                 "SYS: DEBUGGER; ERRMAC"))
  (:compile-load main))

;;;These are the source files from which the cold load band is
;;;constructed.  The only LAMBDA-COLD-LOAD-FILE-LIST file not present is
;;;CPTFON, which as a font file doesn't have a source.

(defsystem cold-load
  (:pathname-default "sys:cold;")
  (:warnings-pathname-default "sys:cold;cold-load-cwarns.lisp")
  (:module main ("SYS:COLD;FIRST"
                 "SYS: SYS; STORAGE"
                 "SYS: SYS; QRAND"
                 "SYS: IO; QIO"
                 "SYS: IO; READ"
                 "SYS: IO; INPUT-READERS"
                 "SYS: IO; PRINT"
                 "SYS: WINDOW; COLD"
                 "SYS: SYS; SGFCTN"
                 "SYS: SYS; EVAL"
                 "SYS: SYS; SPECIAL-FORMS"      ;moved from EVAL
                 "SYS: SYS; TYPES"
                 "SYS: SYS; LTOP"
                 "SYS: SYS; REP"
                 "sys: sys; qccold"     ; $$$ Compiler supports QFASL  <27-Oct-88 keith>
                 "SYS: SYS; QFASL"
;;; NETWORKING TAKEN OUT 19-May-86 10:55:29 -GJC
;;;              "SYS: LAMBDA-E-NET; NU-ETHER-MINI"
;;;              "SYS: LAMBDA-E-NET; 3COM-ETHER-MINI"
;;;              "SYS: LAMBDA-E-NET; ETHER-MINI"
;;; REPLACED WITH:
                 "SYS:COLD;STREAM"
                 "SYS:COLD;MINI"
;;;
                 "SYS: SYS; QFCTNS"
                 "SYS: SYS2; STRING"
                 "SYS: SYS2; CHARACTER"
                 "SYS: SYS; GENRIC"
                 "SYS: SYS; CLPACK"
                 "SYS: COLD; GLOBAL"
                 "SYS: COLD; SYSTEM"
                 "SYS: COLD; LISP"))
  (:compile-load main))

;;;After booting on the cold-load (an image of the COLD-LOAD system
;;;files), these get transferred over by the MINI interface and actually
;;;LOADed (downloaded?) in the new running "cold" world.
;;;
;;;After that, there's enough of a system to load the rest, by way of
;;;(MAKE-SYSTEM 'SYSTEM).

(defsystem inner-system
  (:warnings-pathname-default "sys:patch;inner-system-cwarns.lisp")
  (:module main (("SYS: SYS; INNER-SYSTEM-FILE-ALIST")
                 ("SYS: SYS2; DEFSEL")
                 ("SYS: SYS2; RESOUR")
                 ("SYS: SYS; QMISC")
                 ("SYS: SYS; DESCRIBE")
                 ("SYS: SYS; SORT")
                 ("SYS: IO; FORMAT")
                 ("SYS: IO1; FQUERY")
                 ("SYS: SYS2; HASH")
                 ("SYS: SYS2; FLAVOR")
                 ("SYS: SYS2; HASHFL")
                 ("SYS: SYS2; PRODEF")
                 ("SYS: SYS2; PROCES")
                 ("SYS: SYS2; NUMER")
                 ("SYS: DEBUGGER; EH")
                 ("SYS: DEBUGGER; EHF")
                 ("SYS: DEBUGGER; TRAP")
                 ("SYS: DEBUGGER; CONDITION-FLAVORS")
                 ("SYS: DEBUGGER; EHC")
                 ("SYS: DEBUGGER; EHBPT")
                 ("SYS: SYS2; DISASS")
                 ("SYS: IO; NEW-DISK")
                 ("SYS: IO; DISK")
                 ("SYS: IO; UDISK")
                 ("SYS: IO; DLEDIT")
                 ("SYS: SYS2; LOGIN")
                 ("SYS: IO; RDDEFS")
                 ("SYS: NETWORK; PACKAGES")
                 ("SYS: NETWORK; HOST")
                 ("SYS: IO; FILE; ACCESS")
                 ("SYS: IO; STREAM")
                 ("SYS: LAMBDA-DIAG; LAM-PACKAGE")
                 ("SYS: IO; FILE; PATHNM")
                 ("SYS: IO; FILE; PATHST")
                 ("SYS: FILE; LMPARS")
                 ("SYS: IO; FILE; OPEN")
                 ("SYS: IO1; TIME")
                 ("SYS: IO1; TIMPAR")

                 ("SYS: NETWORK; TRANSPARENT")

                 ("SYS: SYS; CONFIG-DEFS")
                 ("SYS: SYS; IOMSG")
                 ("SYS: SYS; SHARED-DEVICE")
                 ("SYS: SYS; CONFIG")

                 ("SYS: NETWORK; KERNEL; DEFS")
                 ("SYS: NETWORK; KERNEL; LIBRARY")
                 ("SYS: NETWORK; KERNEL; NETWORK-INTERFACE")
                 ("SYS: NETWORK; KERNEL; PACKETS")
                 ("SYS: NETWORK; DRIVERS; LOOPBACK")
                 ("SYS: NETWORK; DRIVERS; SHARE")

                 ;; +++ Problems with network in booting cold system 128!!! <29-Oct-88 keith>
                 ;;;These don't load here yet, but may have to.
                 ;;("SYS: NETWORK; DRIVERS; 3COM")
                 ;;("SYS: NETWORK; DRIVERS; EXCELAN")

                 ("SYS: NETWORK; DRIVERS; DRIVER-PROCESS")
                 ("SYS: NETWORK; KERNEL; NETWORK-PROTOCOL")
                 ("SYS: NETWORK; KERNEL; ARP")

                 ("SYS: NETWORK; CHAOS; CHSNCP")
                 ("SYS: NETWORK; CHAOS; CHUSE")
                 ("SYS: NETWORK; CHAOS; QFILE")

                 ("SYS: NETWORK; KERNEL; CONFIGURE")

                 ("SYS: SITE; SITE")
                 ("SYS: SITE; HSTTBL")
                 ("SYS: SITE; LMLOCS")
                 ;; defsystem bites the big one
                 ;("SYS: SITE; SYS TRANSLATIONS")
                 ))
  (:COMPILE-LOAD MAIN))

;;;The "Outer System" is the set of standard interfaces that always get
;;;loaded on top of SYSTEM.  They are combined here for convenience in
;;;recompiling, e.g. for cold load.

;;;Defined elsewhere:
(si:set-system-source-file :tape "SYS:TAPE;SYSDEF")
(si:set-system-source-file :lambda-diag "SYS:LAMBDA-DIAG;DIAG-SYSTEM")

(DEFSYSTEM OUTER-SYSTEM
  (:NAME "Outer System")
  (:SHORT-NAME "Outer")
  (:warnings-pathname-default "SYS:PATCH;OUTER-SYSTEM-CWARNS.LISP")
  (:COMPONENT-SYSTEMS
    ZWEI
    ZMAIL                                  ;Depends on ZWEI
    CONVERSE                               ;Depends on ZWEI
    LOCAL-FILE
    FILE-SERVER
    UNIX-INTERFACE
    TAPE
    LAMBDA-DIAG))

;;;The "Optional System" is the set of optional interfaces that we send
;;;customers in a form that they can load if they want.  They are combined
;;;here strictly for convenience in recompiling or tag searching.

(defsystem optional-system
  (:name "Optional Software")
  (:short-name "SW Options")
  (:warnings-pathname-default "SYS:PATCH;OPTIONAL-SYSTEM-CWARNS.LISP")
  (:component-systems
    TIGER                                       ;Lambda-to-lambda printing
    SITE-EDITOR                                 ;Network configuration file mgmt.
    KERMIT                                      ;Kermit comms. protocol
    WINDOW-MAKER                                ;window code-generating tool
    FED                                         ;font-editor
    OBJECTLISP                                  ;object-oriented lisp extensions
    MEDIUM-RESOLUTION-COLOR                     ;supports the medium-res color monitor
    GATEWAY                                     ;on-line documentation mangler
    ))
