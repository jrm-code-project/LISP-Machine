;;; -*- Mode:LISP; Package:KERMIT; Base:10; Readtable:ZL -*-

;;; hacks for telnet negotiation in kermit.

(defconst       IAC     255             " interpret as command: ")

(defvar *telsyms*
        '(
          (     DONT    254             " you are not to use option ")
          (     DO      253             " please, you use option ")
          (     WONT    252             " I won't use option ")
          (     WILL    251             " I will use option ")
          (     SB      250             " interpret as subnegotiation ")
          (     GA      249             " you may reverse the line ")
          (     EL      248             " erase the current line ")
          (     EC      247             " erase the current character ")
          (     AYT     246             " are you there ")
          (     AO      245             " abort output--but let prog finish ")
          (     IP      244             " interrupt process--permanently ")
          (     BREAK   243             " break ")
          (     DM      242             " data mark--for connect. cleaning ")
          (     NOP     241             " nop ")
          (     SE      240             " end sub negotiation ")

          (  SYNCH      242             " for telfunc calls ")))

(defvar *telopts*
        '(

          (  TELOPT_BINARY      0       " 8-bit data path ")
          (  TELOPT_ECHO        1       " echo ")
          (     TELOPT_RCP      2       " prepare to reconnect ")
          (     TELOPT_SGA      3       " suppress go ahead ")
          (     TELOPT_NAMS     4       " approximate message size ")
          (     TELOPT_STATUS   5       " give status ")
          (     TELOPT_TM       6       " timing mark ")
          (     TELOPT_RCTE     7       " remote controlled transmission and echo ")
          (  TELOPT_NAOL        8       " negotiate about output line width ")
          (  TELOPT_NAOP        9       " negotiate about output page size ")
          (  TELOPT_NAOCRD      10      " negotiate about CR disposition ")
          (  TELOPT_NAOHTS      11      " negotiate about horizontal tabstops ")
          (  TELOPT_NAOHTD      12      " negotiate about horizontal tab disposition ")
          (  TELOPT_NAOFFD      13      " negotiate about formfeed disposition ")
          (  TELOPT_NAOVTS      14      " negotiate about vertical tab stops ")
          (  TELOPT_NAOVTD      15      " negotiate about vertical tab disposition ")
          (  TELOPT_NAOLFD      16      " negotiate about output LF disposition ")
          (  TELOPT_XASCII      17      " extended ascic character set ")
          (     TELOPT_LOGOUT   18      " force logout ")
          (     TELOPT_BM       19      " byte macro ")
          (     TELOPT_DET      20      " data entry terminal ")
          (     TELOPT_SUPDUP   21      " supdup protocol ")
          (  TELOPT_EXOPL       255     " extended-options-list ")))

(defun telnet-negotiation (&aux c)
  (setq c (serial-tyi))
  (let ((meaning (mem #'(lambda (item elem) (= item (cadr elem))) c *telsyms*)))
    (cond ((eq

  (select c
    (dont
     (format t "DONT "))
    (do
     (format t "DO "))
    (wont (format t "WONT "))
    (will (format t "WILL ")))
  (format t "~D" (serial-tyi)))

(defun enable-telnet ()
  (setq *serial-stream-mask* 255)
  (fset 'READ-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL 'xREAD-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL))

(DEFUN  XREAD-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL ()
  (LET ((KEYSTROKE (SERIAL-TYI)))
    (when (not keystroke)
      (return-from XREAD-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL :eof))
    (when (eq keystroke IAC)
      (return-from XREAD-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL (telnet-negotiation)))
    (COND (*INPUT-FLOW-CONTROL*
           ;; of course, instead of doing this we might just as well work a bit
           ;; on speeding up the terminal emulation itself so that we wouldnt fall
           ;; behind at 9600 baud. but we make it fine at 1200 baud already, the
           ;; most usual operating speed over dial up lines. It is losers in the field
           ;; who want to flow-control. On the other hand would also be nice to have
           ;; a termcap entry for this "terminal" that had sufficient padding.
           ;; the worlds slowest and most expensive H19.
           ;; With a unibus channel of 5k characters we never fall behind, which is
           ;; the solution I prefer. On the other hand it should be the microcode
           ;; that implements hardware and software flow control on input.
           ;; But even that may lose depending on the uart's silo characterists.
           (MULTIPLE-VALUE-BIND (AMOUNT MAX)
               ;; a reasonable optimization regardless of flow control nonsense
               ;; is to look ahead and process all easy characters if available,
               ;; collect them into a string and do a string out.
               (SEND *SERIAL-STREAM* :INPUT-CHARACTERS-AVAILABLE)
             (COND ((EQ *INPUT-FLOW-CONTROL* :CATCHING-UP)
                    (COND ((< (* 3 AMOUNT) (* 1 MAX))
                           ;; less than 1/3 full, must be catching up now, so let 'em rip.
                           (SEND *SERIAL-STREAM* :TYO #o21)
                           (SETQ *INPUT-FLOW-CONTROL* :READY))))
                   ((> (* 3 AMOUNT) (* 2 MAX))
                    ;; if more than 2/3 full we must be falling behind, so make 'em stop.
                    (SETQ *INPUT-FLOW-CONTROL* :CATCHING-UP)
                    (SEND *SERIAL-STREAM* :TYO #o23))))))

    (COND (*tek-in-graphics-mode*
           (COND ((eq keystroke *tek-pad*))
                 ((EQ KEYSTROKE *TEK-PRINTMODE*)
                  (SETQ *TEK-IN-GRAPHICS-MODE* NIL))
                 ((EQ KEYSTROKE *CR*)
                  (SETQ *TEK-IN-GRAPHICS-MODE* NIL)
                  (SEND *SERIAL-STREAM* :UNTYI KEYSTROKE))
                 ((EQ KEYSTROKE *TEK-GRAPHMODE*)
                  (setq *tek-firstp* t))
                 ((EQ KEYSTROKE #\ALT)
                  (escape-dispatch))
                 ((EQ KEYSTROKE #O12)
                  (TERMINAL-LINEFEED))
                 ((>= KEYSTROKE #O40)
                  (SEND *SERIAL-STREAM* :UNTYI KEYSTROKE)
                  (MULTIPLE-VALUE-BIND (X Y)
                      (get-tek-vector)
                    (COND (*tek-firstp*
                           (SETQ *tek-firstp* NIL)
                           (TEK-MOVE X Y))
                          ('ELSE
                           (TEK-DRAW X Y)))))))
          ((AND *TEK-EMULATIONP* (EQ KEYSTROKE *TEK-GRAPHMODE*))
           (setq *tek-in-graphics-mode* t)
           (setq *tek-firstp* t))

          ((EQ KEYSTROKE #O33)                  ;ASCII <ALTMODE> [ESCAPE]
           (ESCAPE-DISPATCH))

          ((and *snarf-telnet-negotiations* (= keystroke #o377)) (serial-tyi))

          ((< #O31 KEYSTROKE #O200)
           (AND *LOGFILE* TURN-ON-LOGGING? (SEND *LOGFILE* ':TYO KEYSTROKE))    ;LOGFILE KLUDGE
           (COND (*INSERT-FLAG* (TERMINAL-INSERT-CHAR)))
           (LET ((STORE (TERMINAL-ERASE-ALUF)))
             (TERMINAL-SET-ERASE-ALUF (IF *REVERSE-VIDEO-FLAG* TV:ALU-IOR TV:ALU-ANDCA))
             (TERMINAL-CLEAR-CHAR)
             (TERMINAL-SET-ERASE-ALUF STORE))
           (COND ((> (TERMINAL-READ-CURSORPOS) (TERMINAL-CHARACTER-WIDTH))
                  (TERMINAL-CR)))

           (TERMINAL-TYO KEYSTROKE))

          (T (SELECTQ KEYSTROKE
               (#O7 (TERMINAL-BEEP))
               (#O10 (TERMINAL-BACKSPACE))
               (#O11 (TERMINAL-TAB)
                     (AND *LOGFILE* TURN-ON-LOGGING? (SEND *LOGFILE* ':TYO #O211)))
               (#O12 (TERMINAL-LINEFEED)
                     (if *auto-clear-eol-on-linefeed-flag* (terminal-clear-eol)))
               (#O15 (TERMINAL-CR)
                     (AND *TEK-EMULATIONP* (TEK-RESET-MEMORY))
                     (AND *LOGFILE* TURN-ON-LOGGING? (SEND *LOGFILE* ':TYO #O215)))
               (T (terminal-tyo keystroke)
                  (COND (*TERMINAL-DEBUG-MODE*
                         (FORMAT INTERACTION-PANE
                                 "~%Unrecognized /"control character/": ~O [~:@C]"
                                 KEYSTROKE KEYSTROKE))))
               )))))
