;-*- Mode:LISP; Package:USER; Base:8 -*- The Filecomputer's Lisp Machine init file.

;; Turn on nifty paging feature, but turn off Read-Compares, until disk controller
;; (or whatever) starts to win.
(SETQ SI:%DISK-SWITCHES 14)

(SI:SET-ALL-SWAP-RECOMMENDATIONS 2)
(ASET 8 #'SYS:AREA-SWAP-RECOMMENDATIONS MACRO-COMPILED-PROGRAM)
(ASET 4 #'SYS:AREA-SWAP-RECOMMENDATIONS SYS:FASL-CONSTANTS-AREA)

;In case band was dumped with file server turned on.
(FS:DISABLE-FILE-SERVER)
(FS:DISABLE-MAIL-SERVER)        ;Flush the dummy mail server that Lispms all have


(DEFUN HELLO ()
  (BEEP)
  (FORMAT T "~%Stay here until the login is finished!!!~%")
  (LOAD-PATCHES ':NOSELECTIVE)

  ;; LFS initializations
  (FORMAT T "~%Building LFS System...")
  (MAKE-SYSTEM 'LFS ':NOCONFIRM ':NOWARN)
;Not till system 79
;  (MAKE-SYSTEM 'LMFILE-REMOTE ':NOCONFIRM ':NOWARN)
  (FORMAT T "~%Booting LMFILE on partition SRVR...")
  (FS:CONSIDER-UNIT 0 "SRVR")
  (FS:START-FILE-SYSTEM)
  (ERRSET (LET ((OLDHST (FS:GET-PATHNAME-HOST "LFS")))
            (SETQ FS:*PATHNAME-HOST-LIST*
                  (DELQ OLDHST FS:*PATHNAME-HOST-LIST*)))
          NIL)
  (FS:ADD-LFS-HOST "FC")
;Not till system 79
;  (ERRSET (FS:ADD-FC-HOST "FC-VIA-NET" CHAOS:MY-ADDRESS) NIL)
  (FORMAT T "~%LMFILE Booted and running.")

  ;; LM initializations
  (ADD-INITIALIZATION "Read file system into core"
                      '(FS:READ-FILE-SYSTEM-INTO-CORE)
                      '(WARM NOW))
  ;; Temporary
  (OR (FBOUNDP 'FS:RFILE-SERVER)
      (LOAD "AI: LMFS; FSERVE QFASL"))

  ;; Random initializations.
; TV:SET-DEFAULT-FONT really loses in many programs (particularly the editor).
; (TV:SET-DEFAULT-FONT FONTS:MEDFNT)
  ;; Allow typeout to continue past the end of screen without hanging.
  (LOGIN-SETQ TV:MORE-PROCESSING-GLOBAL-ENABLE NIL)
  (PROCESS-RUN-FUNCTION "Change Package" #'PKG-GOTO 'FS)
  (FS:ENABLE-FILE-SERVER)
  (FS:ENABLE-MAIL-SERVER)       ;Turn on the real one.
  (FILE-DISPLAY))


;;; List directory on some ITS machine

(DEFUN DIR (&OPTIONAL (DIRECTORY (FS:USER-HOMEDIR)))
  (TERPRI)
  (TIME:PRINT-CURRENT-DATE)
  (FS:VIEWF  (FS:MERGE-PATHNAME-DEFAULTS DIRECTORY ".FILE. (DIR)")))

(DEFUN FILE-DISPLAY ()
  (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
  (SI:PRINT-HERALD)
  (FORMAT T "~%~%MIT AI Lab File Computer using this console.~%")
  (TIME:PRINT-CURRENT-DATE)
  (FORMAT T "~%~%")
  T)

(GLOBALIZE 'FILE-DISPLAY)

;;;  And away we go....
(HELLO)
