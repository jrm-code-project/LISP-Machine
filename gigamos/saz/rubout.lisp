;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for ZMail version 74.14
;;; Written 3-Nov-88 12:28:19 by saz (David M.J. Saslav) at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.138, Experimental ZWEI 126.28, Experimental ZMail 74.13, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, Microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, ) (Lambda/Falcon Development System, saved on October 4, 1988 by saz Have a nice day....



; From modified file DJ: L.ZMAIL; COMNDS.LISP#601 at 3-Nov-88 12:28:19
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZMAIL; COMNDS  "

(DEFUN INITIALIZE-ZMAIL-COMTABS (MODE-COMTAB)
  (SET-COMTAB MODE-COMTAB '(#/H-F COM-FORWARD-ADDRESS
                            #/H-B COM-BACKWARD-ADDRESS
                            #/H-K COM-KILL-ADDRESS
                            #/H-RUBOUT COM-BACKWARD-KILL-ADDRESS
                            #/H-T COM-EXCHANGE-ADDRESSES))

  (SETQ *ZMAIL-COMTAB* (SET-COMTAB '*ZMAIL-COMTAB*
                                   '(#/C-D COM-ZMAIL-DELETE-AND-UP
                                     #/C-G COM-BEEP
                                     #/C-F COM-ZMAIL-FIND-STRING
                                     #/C-L COM-RECENTER-WINDOW
                                     #/C-N COM-ZMAIL-DOWN-TO-NEXT
                                     #/C-P COM-ZMAIL-UP-TO-PREVIOUS
                                     #/C-R COM-EDIT-CURRENT-MSG
                                     #/C-U COM-UNIVERSAL-ARGUMENT
                                     #/C-V COM-NEXT-SCREEN
                                     #/C-Z COM-QUIT
                                     #/C-SP COM-ZMAIL-SET-POP-MARK
                                     #/M-V COM-PREVIOUS-SCREEN
                                     #/M-X COM-ZMAIL-EXTENDED-COMMAND
                                     #/M-? COM-ZMAIL-SELF-DOCUMENT
                                     #/M-~ COM-ZMAIL-NOT-MODIFIED
                                     #/M-< COM-ZMAIL-START-OF-MSG
                                     #/M-> COM-ZMAIL-END-OF-MSG
                                     #/C-M-V COM-SCROLL-SUMMARY-WINDOW
                                     #/C-M-SP COM-ZMAIL-MOVE-TO-PREVIOUS-POINT
                                     #/. COM-ZMAIL-START-OF-MSG
                                     #/? COM-ZMAIL-DOCUMENTATION
                                     #/C COM-ZMAIL-CONTINUE
                                     #/D COM-ZMAIL-DELETE
                                     #/E COM-ZMAIL-EXPUNGE
                                     #/F COM-ZMAIL-FORWARD
                                     #/G COM-GET-NEW-MAIL
                                     #/J COM-ZMAIL-JUMP
                                     #/L COM-ZMAIL-KEYWORDS
                                     #/M COM-ZMAIL-MAIL
                                     #/O COM-ZMAIL-MOVE
                                     #/N COM-ZMAIL-NEXT
                                     #/P COM-ZMAIL-PREVIOUS
                                     #/Q COM-ZMAIL-QUIT
                                     #/R COM-ZMAIL-REPLY
                                     #/S COM-ZMAIL-SAVE
                                     #/U COM-ZMAIL-UNDELETE
                                     #/X COM-ZMAIL-EXTENDED-COMMAND
                                     #/Z COM-ZMAIL-LARGE-ARGUMENT
                                     #/BREAK COM-ZMAIL-BREAK
                                     #/BS COM-PREVIOUS-SCREEN
                                     #/RUBOUT COM-PREVIOUS-SCREEN
                                     #/HAND-DOWN COM-NEXT-SCREEN
                                     #/HAND-UP COM-PREVIOUS-SCREEN
                                     #/HELP COM-ZMAIL-DOCUMENTATION
                                     #/FF COM-ZMAIL-REFRESH
                                     #/RESUME COM-ZMAIL-CONTINUE
                                     #/SP COM-NEXT-SCREEN
                                     #/- COM-NEGATE-NUMERIC-ARG
                                     #/C-- COM-NEGATE-NUMERIC-ARG
                                     #/M-- COM-NEGATE-NUMERIC-ARG
                                     #/C-M-- COM-NEGATE-NUMERIC-ARG
                                     (#/0 10.) COM-NUMBERS
                                     (#/C-0 10.) COM-NUMBERS
                                     (#/M-0 10.) COM-NUMBERS
                                     (#/C-M-0 10.) COM-NUMBERS
                                     )))

  (SETQ *MSG-COMTAB* (SET-COMTAB '*MSG-COMTAB*
                                 '(#/END COM-QUIT-ZMAIL-EDIT
                                   #/C- COM-QUIT-ZMAIL-EDIT
                                   #/ABORT COM-QUIT-ZMAIL-EDIT)))
  (SET-COMTAB-INDIRECTION *MSG-COMTAB* MODE-COMTAB)

  (SETQ *MSG-CONTROL-X-COMTAB*
        (SET-COMTAB '*MSG-CONTROL-X-COMTAB*
                    '(#/A COM-ADD-MORE-TEXT
                      #/C COM-ADD-CC-FIELD
                      #/S COM-ADD-SUBJECT-FIELD
                      #/T COM-ADD-TO-FIELD)))
  (SET-COMTAB-INDIRECTION *MSG-CONTROL-X-COMTAB* *STANDARD-CONTROL-X-COMTAB*)

  (SET-COMTAB MODE-COMTAB
              (LIST #/C-X (MAKE-EXTENDED-COMMAND *MSG-CONTROL-X-COMTAB*)))

  (SETQ *REPLY-CONTROL-X-COMTAB*
        (SET-COMTAB '*REPLY-CONTROL-X-COMTAB*
                    '(#/2 COM-ZMAIL-REPLY-TWO-WINDOWS
                      #/3 COM-ZMAIL-REPLY-THREE-WINDOWS
                      #/1 COM-ZMAIL-REPLY-ONE-WINDOW
                      #/M COM-ZMAIL-RECURSIVE-MAIL
                      #/O COM-ZMAIL-OTHER-WINDOW
                      #/Y COM-PRUNE-YANKED-HEADERS
                      #/C-R COM-RESTORE-DRAFT-FILE
                      #/C-S COM-SAVE-DRAFT-FILE
                      #/C-W COM-WRITE-DRAFT-FILE
                      #/C-M-S COM-SAVE-DRAFT-AS-MSG)))
  (SET-COMTAB-INDIRECTION *REPLY-CONTROL-X-COMTAB* *MSG-CONTROL-X-COMTAB*)

  (SETQ *REPLY-COMTAB* (SET-COMTAB '*REPLY-COMTAB*
                                   '(#/END COM-MAIL-END
                                     #/C- COM-SEND-MESSAGE
                                     #/C-M-Y COM-ZMAIL-YANK
                                     #/ABORT COM-ABORT-SEND
                                     #/SUPER-ABORT COM-REALLY-ABORT-SEND
                                     #/C-] COM-ABORT-SEND)
                                   (MAKE-COMMAND-ALIST '(COM-ADD-TO-FIELD COM-ADD-CC-FIELD
                                                         COM-ADD-FTO-FIELD
                                                         COM-ADD-FCC-FIELD
                                                         COM-ADD-SUBJECT-FIELD
                                                         COM-ADD-IN-REPLY-TO-FIELD
                                                         COM-ADD-MORE-TEXT COM-ADD-FROM-FIELD
                                                         COM-ZMAIL-YANK-CURRENT-MSG
                                                         COM-PRUNE-YANKED-HEADERS
                                                         COM-SEND-MESSAGE COM-ABORT-SEND
                                                         COM-RESTORE-DRAFT-FILE
                                                         COM-WRITE-DRAFT-FILE
                                                         COM-SAVE-DRAFT-FILE
                                                         COM-SAVE-DRAFT-AS-MSG
                                                         COM-CHANGE-SUBJECT-PRONOUNS))))
  (SET-COMTAB *REPLY-COMTAB*
              (LIST #/C-X (MAKE-EXTENDED-COMMAND *REPLY-CONTROL-X-COMTAB*)))
  (SET-COMTAB *REPLY-COMTAB* (LIST #/MOUSE-3-1
                                   (MAKE-MENU-COMMAND '(COM-ADD-TO-FIELD COM-ADD-CC-FIELD
                                                        COM-ADD-FCC-FIELD
                                                        COM-ADD-SUBJECT-FIELD
                                                        COM-ADD-IN-REPLY-TO-FIELD
                                                        COM-ADD-MORE-TEXT COM-ADD-FROM-FIELD
                                                        COM-PRUNE-YANKED-HEADERS
                                                        COM-SEND-MESSAGE COM-ABORT-SEND
                                                        COM-RESTORE-DRAFT-FILE
                                                        COM-WRITE-DRAFT-FILE
                                                        COM-SAVE-DRAFT-FILE
                                                        COM-SAVE-DRAFT-AS-MSG
                                                        COM-CHANGE-SUBJECT-PRONOUNS))))
  (SET-COMTAB-INDIRECTION *REPLY-COMTAB* MODE-COMTAB)
  (SETQ *OTHER-COMMAND-ALIST* (MAKE-COMMAND-ALIST '(COM-ZMAIL-VIEW-FILE COM-ZMAIL-WHOIS))))


;;; Until next system release, this is the only thing which affects
;;; normal ZMail operation...
(set-comtab *zmail-comtab* '(#/RUBOUT COM-PREVIOUS-SCREEN))

))
