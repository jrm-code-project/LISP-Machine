;;; -*- Mode:LISP; Package:KERMIT; Base:10; Readtable:ZL -*-


#|

Copyright LISP Machine, Inc. 1984, 1985, 1986
  See filename "Copyright" for
licensing and release information.

This program is KERMIT-TERMINAL. This is to be used to make your lisp machine
terminal stream act like it is an "H19" terminal. It is a self contained
module with only one entry point, via a message to KTERM-STATE object,
which contains all required state information.

No "special" window is required. That is, a lisp listener
should do fine. A tv:minimum-window will not, of course, work.
There are some hooks in the code for features provided by the
KERMIT programs frames, but these are not used if they do not exist.

For the H19 graphics protocol, see the Zenith manual for
the Z29 terminal. "Z-29 user's & technical guide"
Appendix B -- Zenith Mode Code Info 1983, Zenith Data Systems.

To use this make an instance of KTERM-STATE and send it
a :MAKE-CONNECTION or :MAKE-DUAL-CONNECTION message.
The :MAKE-CONNECTION method is prefered if the serial stream supports
:LISTEN competently, because that results in less process switching
and less process-quantization effects on performance and behavior.

Example:

 (with-open-file (stream "sdu-serial-b:")
   (send (make-instance 'kterm-state) :make-connection stream terminal-io))

|#


(DEFCONST *ESCAPE-DISPATCH-TABLE* (MAKE-HASH-TABLE))


(DECLARE (SPECIAL INTERACTION-PANE))

(DEFCONST *SERIAL-STREAM* :unbound)

(DEFCONST *TERMINAL* :unbound)

(DEFCONST *BAD-ESCAPES* ())

(defconst *local-echo-mode* nil)


(DEFCONST *LOGFILE* NIL "a stream to log terminal session, if desired")


(DEFCONST TURN-ON-LOGGING? NIL)


(DEFCONST *TERMINAL-DEBUG-MODE* NIL)


(DEFCONST *INPUT-FLOW-CONTROL* NIL)


(DEFCONST *INSERT-FLAG* ())



(DEFCONST *REVERSE-VIDEO-FLAG* ())



(DEFCONST *CURSOR-SAVE* '(0 0))



(DEFCONST *SYSTEM-POSITION* '(0 0))


(DEFCONST *USE-BIT-7-FOR-META* NIL)

(defconst *use-control-z-for-control-meta* nil)

(DEFCONST *AUTO-CR-ON-LF-FLAG* NIL)



(DEFCONST *AUTO-LF-ON-CR-FLAG* NIL)





(DEFSUBST TERMINAL-INSERT-CHAR ()
  (SEND *TERMINAL* ':INSERT-CHAR 1 ':CHARACTER))





(DEFSUBST TERMINAL-ERASE-ALUF ()
  (SEND *TERMINAL* ':ERASE-ALUF))





(DEFSUBST TERMINAL-SET-ERASE-ALUF (ALU)
  (SEND *TERMINAL* ':SET-ERASE-ALUF ALU))





(DEFSUBST TERMINAL-TYO (CHAR-CODE)
  (SEND *TERMINAL* ':TYO CHAR-CODE))



(DEFSUBST TERMINAL-READ-CURSORPOS ()
  (SEND *TERMINAL* ':READ-CURSORPOS ':CHARACTER))


(DEFSUBST TERMINAL-SET-CURSORPOS (X Y)
  (SEND *TERMINAL* ':SET-CURSORPOS
        X Y
        ':CHARACTER))


(DEFSUBST TERMINAL-INSERT-LINE (&OPTIONAL (NTIMES 1))
  (TV:SHEET-INSERT-LINE *TERMINAL* NTIMES))


(DEFSUBST TERMINAL-DELETE-LINE (&OPTIONAL (NTIMES 1))
  (TV:SHEET-DELETE-LINE *TERMINAL* NTIMES))


(DEFSUBST TERMINAL-CLEAR-CHAR ()
  (SEND *TERMINAL* ':CLEAR-CHAR))


(DEFSUBST TERMINAL-CHARACTER-WIDTH ()
  (MULTIPLE-VALUE-BIND (WIDTH IGNORE)
      (SEND *TERMINAL* ':SIZE-IN-CHARACTERS)
    (min 80. WIDTH)))

(DEFSUBST TERMINAL-CHARACTER-HEIGHT ()
  (MULTIPLE-VALUE-BIND (IGNORE HEIGHT)
      (SEND *TERMINAL* ':SIZE-IN-CHARACTERS)
    HEIGHT))


(defconst *wrap-around-instead-of-scrolling-flag* nil)

(DEFSUBST TERMINAL-END-OF-PAGE-EXCEPTION ()
  (SEND *TERMINAL* :HOME-CURSOR)
  (cond (*wrap-around-instead-of-scrolling-flag*
         (send *terminal* :clear-eol))
        (t
         (SEND *TERMINAL* :DELETE-LINE)
         (TERMINAL-SET-CURSORPOS 0 (- (TERMINAL-CHARACTER-HEIGHT) 2))
         (send *terminal* :insert-line))))


(DEFSUBST TERMINAL-CR ()
  (MULTIPLE-VALUE-BIND (IGNORE Y)
      (TERMINAL-READ-CURSORPOS)
    (TERMINAL-SET-CURSORPOS 0 Y)
    (AND *AUTO-LF-ON-CR-FLAG*
         (COND ((EQ Y (- (TERMINAL-CHARACTER-HEIGHT) 2))
                (TERMINAL-END-OF-PAGE-EXCEPTION))
               (T (TERMINAL-SET-CURSORPOS 0 (1+ Y)))))
    (if (and *wrap-around-instead-of-scrolling-flag*
             *auto-cr-on-lf-flag*)
        (send *terminal* ':clear-eol))
    NIL))




(DEFSUBST TERMINAL-LINEFEED ()
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (COND ((and (fixp y) (>= Y (- (TERMINAL-CHARACTER-HEIGHT) 2)))
           (TERMINAL-END-OF-PAGE-EXCEPTION))
          (T (TERMINAL-SET-CURSORPOS
               (IF *AUTO-CR-ON-LF-FLAG* 0 X)
               (1+ Y))))
    (if (and *wrap-around-instead-of-scrolling-flag*
             (not *auto-cr-on-lf-flag*))
        (send *terminal* ':clear-eol))
    NIL))




(defvar *serial-stream-mask* #o177)


(defsubst serial-tyi ()
  (let ((ch? (send *serial-stream* ':tyi)))
    (and ch? (logand ch? *serial-stream-mask*))))




(DEFSUBST TERMINAL-SAVE-POS-1 ()
  (SETQ *SYSTEM-POSITION* (MULTIPLE-VALUE-LIST (TERMINAL-READ-CURSORPOS))))




(DEFSUBST TERMINAL-RESTORE-POS-1 ()
  (TERMINAL-SET-CURSORPOS (CAR *SYSTEM-POSITION*) (CADR *SYSTEM-POSITION*)))






(DEFSUBST TERMINAL-GOTO-BEG-OF-LINE ()
  (MULTIPLE-VALUE-BIND (IGNORE Y)
      (TERMINAL-READ-CURSORPOS)
    (TERMINAL-SET-CURSORPOS 0 Y)))












(DEFSUBST TERMINAL-BACKSPACE ()
  (TERMINAL-TYO #\BACKSPACE))



(DEFSUBST TERMINAL-BEEP ()
  (BEEP))




(DEFSUBST TERMINAL-TAB ()
  (TERMINAL-TYO #\TAB))












;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;;;     definition of DEF-TERMINAL-ESCAPE

;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





(DEFMACRO DEF-TERMINAL-ESCAPE (KEY-NUMBER NEED-TO-DEFINE-P FUNCTION-NAME &BODY BODY)
  (COND (NEED-TO-DEFINE-P
         `(PROGN 'COMPILE
                 (PUTHASH ,KEY-NUMBER ',FUNCTION-NAME *ESCAPE-DISPATCH-TABLE*)
                 (DEFUN ,FUNCTION-NAME () . ,BODY)))
        ('ALREADY-DEFINED-BY-SYSTEM-OR-USER
         `(PUTHASH ,KEY-NUMBER ',FUNCTION-NAME *ESCAPE-DISPATCH-TABLE*))))



;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;;;     terminal escape definitions

;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<






(DEF-TERMINAL-ESCAPE #/[ T TERMINAL-EAT-TEMP    ; this may be wrong
  ;; 'Enter Hold Screen Mode' ZEHS
  (LET (I1 I2 FLAG)
    (SETQ I1 (SERIAL-TYI))
    (SETQ I2 (SERIAL-TYI))
    (COND ((EQ I1 #\?) (SETQ FLAG T) (SERIAL-TYI))
          ((OR (> I2 #\9) (< I2 #\0))
           (SETQ I1 (- I1 #\0)))
          (T (SETQ I1 (+ (* 10. (- I1 #\0)) (- I2 #\0)))
             (SETQ I2 (SERIAL-TYI))))
    (COND ((NOT FLAG)
           (SELECTQ I2
             (#\L (TERMINAL-INSERT-LINE I1))
             (#\M (TERMINAL-DELETE-LINE I1)))))))






(DEF-TERMINAL-ESCAPE #\\ T EXIT-EAT-TEMP
  (TERMINAL-CLEAR-SCREEN))                      ; this may be wrong






(DEF-TERMINAL-ESCAPE #\H T TERMINAL-HOME-CURSOR
  (SEND *TERMINAL* ':HOME-CURSOR))





(DEF-TERMINAL-ESCAPE #\p T TERMINAL-REVERSE-VIDEO
  (SETQ *REVERSE-VIDEO-FLAG* T)
  NIL)





(DEF-TERMINAL-ESCAPE #\q T TERMINAL-NORMAL-VIDEO
  (SETQ *REVERSE-VIDEO-FLAG* NIL)
  NIL)






(DEF-TERMINAL-ESCAPE #\x T TERMINAL-SET-MODE
  (SELECTQ (SERIAL-TYI)
    (#O10 (SETQ *AUTO-LF-ON-CR-FLAG* T))
    (#O11 (SETQ *AUTO-CR-ON-LF-FLAG* T))
    (OTHERWISE ()))
  (COND (*TERMINAL-DEBUG-MODE* (FORMAT INTERACTION-PANE "~% SET MODE:  ~O [~C] ")))
  NIL)






(DEF-TERMINAL-ESCAPE #\y T TERMINAL-RESET-MODE
  (SELECTQ (SERIAL-TYI)
    (#O10 (SETQ *AUTO-LF-ON-CR-FLAG* NIL))
    (#O11 (SETQ *AUTO-CR-ON-LF-FLAG* NIL))
    (OTHERWISE ()))
  (COND (*TERMINAL-DEBUG-MODE* (FORMAT INTERACTION-PANE "~% SET MODE:  ~O [~C] ")))
  NIL)









(DEF-TERMINAL-ESCAPE #\C T TERMINAL-CURSOR-FORWARD
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (UNLESS (EQ X 79.)
      (TERMINAL-SET-CURSORPOS (1+ X) Y))))





(DEF-TERMINAL-ESCAPE #\D T TERMINAL-CURSOR-BACKWARDS
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (UNLESS (EQ X 0)
      (TERMINAL-SET-CURSORPOS (1- X) Y))))






(DEF-TERMINAL-ESCAPE #\B T TERMINAL-CURSOR-DOWN
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (UNLESS (EQ Y (- (TERMINAL-CHARACTER-HEIGHT) 2))
      (TERMINAL-SET-CURSORPOS X (1+ Y)))))





(DEF-TERMINAL-ESCAPE #\A T TERMINAL-CURSOR-UP
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (UNLESS (EQ Y 0)
      (TERMINAL-SET-CURSORPOS X (1- Y)))))




(DEF-TERMINAL-ESCAPE #\I T TERMINAL-REVERSE-INDEX
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (COND ((ZEROP X)
           (TERMINAL-SET-CURSORPOS 0 (- (TERMINAL-CHARACTER-HEIGHT) 2))
           (TERMINAL-DELETE-LINE)
           (TERMINAL-SET-CURSORPOS X Y)
           (TERMINAL-INSERT-LINE))
          (T (TERMINAL-CURSOR-UP)))))






(DEF-TERMINAL-ESCAPE #\n T TERMINAL-REPORT-CURSOR
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (SEND *SERIAL-STREAM* ':TYO #O33)           ;33 is ascii <altmode>
    (SEND *SERIAL-STREAM* ':TYO #\Y)
    (SEND *SERIAL-STREAM* ':TYO (+ 32. Y))
    (SEND *SERIAL-STREAM* ':TYO (+ 32. X))))






(DEF-TERMINAL-ESCAPE #\J T TERMINAL-CLEAR-EOF
  (SEND *TERMINAL* ':CLEAR-EOF))






(DEF-TERMINAL-ESCAPE #\j T TERMINAL-SAVE-POS
  (SETQ *CURSOR-SAVE*
        (MULTIPLE-VALUE-LIST (TERMINAL-READ-CURSORPOS))))






(DEF-TERMINAL-ESCAPE #\k T TERMINAL-RESTORE-POS
  (TERMINAL-SET-CURSORPOS (CAR *CURSOR-SAVE*) (CADR *CURSOR-SAVE*)))





(DEF-TERMINAL-ESCAPE #\Y T TERMINAL-SET-POS
  (LET ((Y (SERIAL-TYI))
        (X (SERIAL-TYI)))
    (cond (*terminal-debug-mode*
           (format t "~&  setpos X=~D Y=~D" (- x 32.) (- y 32.))))
    (TERMINAL-SET-CURSORPOS (- X 32.) (- Y 32.))))






(DEF-TERMINAL-ESCAPE #\E T TERMINAL-CLEAR-SCREEN
  (SEND *TERMINAL* ':CLEAR-SCREEN))





(DEF-TERMINAL-ESCAPE #\b T TERMINAL-CLEAR-BOD
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (DOTIMES (LINE (1- Y))
      (TERMINAL-SET-CURSORPOS 0 LINE)
      (TERMINAL-CLEAR-EOL))
    (TERMINAL-SET-CURSORPOS 0 Y)
    (DOTIMES (DUMMY X)
      (TERMINAL-CLEAR-CHAR)
      (TERMINAL-CURSOR-FORWARD))
    (TERMINAL-CURSOR-BACKWARDS)))








(DEF-TERMINAL-ESCAPE #\l T TERMINAL-CLEAR-LINE
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (TERMINAL-SET-CURSORPOS 0 Y)
    (TERMINAL-CLEAR-EOL)
    (TERMINAL-SET-CURSORPOS X Y)))








(DEF-TERMINAL-ESCAPE #\o T TERMINAL-ERASE-BOL
  (MULTIPLE-VALUE-BIND (X Y)
      (TERMINAL-READ-CURSORPOS)
    (TERMINAL-SET-CURSORPOS 0 Y)
    (DOTIMES (DUMMY X)
      (TERMINAL-CLEAR-CHAR)
      (TERMINAL-CURSOR-FORWARD))
    (TERMINAL-CURSOR-BACKWARDS)))






(DEF-TERMINAL-ESCAPE #\K T TERMINAL-CLEAR-EOL
  (SEND *TERMINAL* ':CLEAR-EOL))







(DEF-TERMINAL-ESCAPE #\L T TERMINAL-INSERT-ONE-LINE
  (TERMINAL-SAVE-POS-1)
  (TERMINAL-SET-CURSORPOS 0 (- (TERMINAL-CHARACTER-HEIGHT) 2))
  (TERMINAL-DELETE-LINE)
  (TERMINAL-RESTORE-POS-1)
  (TERMINAL-INSERT-LINE)
  (TERMINAL-GOTO-BEG-OF-LINE))








(DEF-TERMINAL-ESCAPE #\M T TERMINAL-DELETE-ONE-LINE
  (TERMINAL-DELETE-LINE)
  (TERMINAL-SAVE-POS-1)
  (TERMINAL-SET-CURSORPOS 0 (- (TERMINAL-CHARACTER-HEIGHT) 2))
  (TERMINAL-INSERT-LINE)
  (TERMINAL-RESTORE-POS-1)
  (TERMINAL-GOTO-BEG-OF-LINE))






(DEF-TERMINAL-ESCAPE #\N T TERMINAL-DELETE-CHAR
  (SEND *TERMINAL* ':DELETE-CHAR))







(DEF-TERMINAL-ESCAPE #\@ T TERMINAL-INSERT-MODE
  (SETQ *INSERT-FLAG* T)
  NIL)








(DEF-TERMINAL-ESCAPE #\O T TERMINAL-EXIT-INSERT-MODE
  (SETQ *INSERT-FLAG* NIL))




(DEFSUBST ESCAPE-DISPATCH ()
  (LET* ((KEYSTROKE (SERIAL-TYI))
         (METHOD (GETHASH KEYSTROKE *ESCAPE-DISPATCH-TABLE*)))
    (COND (METHOD
           (FUNCALL METHOD)
           (COND (*TERMINAL-DEBUG-MODE*
                  (FORMAT INTERACTION-PANE "~%  ~O  [~:@C]  ~S " KEYSTROKE KEYSTROKE METHOD))))
          (T (PUSH KEYSTROKE *BAD-ESCAPES*)
             (COND (*TERMINAL-DEBUG-MODE*
                    (FORMAT INTERACTION-PANE "~% ~O [~C] <<*** BAD ESCAPE CHARACTER"
                            KEYSTROKE KEYSTROKE)))))))




(defvar *auto-clear-eol-on-linefeed-flag* nil)  ; only used by tcp:telnet-h19 - mwt
(defvar *snarf-telnet-negotiations* nil)        ; ditto



;;; tektroniks emulation.

(DEFVAR *TEK-EMULATIONP* NIL)

(DEFCONST *TEK-PAD* #o26)               ; control-V
(DEFCONST *TEK-GRAPHMODE* #o35)
(DEFCONST *TEK-PRINTMODE* #o37)

(defvar *tek-in-graphics-mode* nil)

(defvar *tek-firstp* t)

(DEFUN READ-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL ()
  (LET ((KEYSTROKE (SERIAL-TYI)))
    (when (not keystroke)
      (return-from READ-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL :eof))
    (when (eq keystroke #o377)
      ;; telnet IAC which can only be seen if *serial-stream-mask* is set to #o377
      (return-from READ-CHAR-FROM-SERIAL-STREAM-TO-TERMINAL (handle-telnet-iac)))
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

          ((< #O31 KEYSTROKE #O200)
           (AND *LOGFILE* TURN-ON-LOGGING? (SEND *LOGFILE* ':TYO KEYSTROKE))    ;LOGFILE KLUDGE
           (COND (*INSERT-FLAG* (TERMINAL-INSERT-CHAR)))
           (LET ((STORE (TERMINAL-ERASE-ALUF)))
             (TERMINAL-SET-ERASE-ALUF (IF *REVERSE-VIDEO-FLAG* TV:ALU-IOR TV:ALU-ANDCA))
             (TERMINAL-CLEAR-CHAR)
             (TERMINAL-SET-ERASE-ALUF STORE))
           (TERMINAL-TYO KEYSTROKE)
           (when (>= (TERMINAL-READ-CURSORPOS) (TERMINAL-CHARACTER-WIDTH))
             (TERMINAL-CR)
             (terminal-linefeed)))

          (T (SELECTQ KEYSTROKE
               (0)
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


(defun process-wait-listen (&rest streams)
  "waits on input on the streams, returns the stream which has input ready."
  (dolist (stream streams)
    (if (send stream ':listen)
        (return-from process-wait-listen stream)))
  (let ((return-value))
    (process-wait "wait-listen"
                  #'(lambda (return-value streams)
                      (dolist (stream streams)
                        (if (send stream ':listen)
                            (return (setf (car return-value) stream)))))
                  (locf return-value)
                  streams)
    return-value))

;;; sending characters from terminal to serial-stream:




(defsubst terminal-tyi ()
  (send *terminal* ':tyi))



(defsubst serial-tyo (char)
  (send *serial-stream* ':tyo char))




;;; this is now somewhat specialize for
;;; kermit by having this mouse menu tracking
;;; business, but its just the easiest way to
;;; keep the menu active while Connect is running.
;;; See the file "sys:kermit;kermit-window" for
;;; the extra meaning to this.



(defsubst terminal-any-tyi ()
  (send *terminal* ':any-tyi))

(defun read-char-from-keyboard-to-serial-stream ()
  (declare (special *escchr*))
  (let ((key-stroke (terminal-any-tyi)))
    (cond ((and (not (atom key-stroke)) (eq (car key-stroke) ':menu))
           (funcall (cadddr key-stroke) ':execute (cadr key-stroke)))
          ((not (fixnump key-stroke)) (beep))
          (t (if *local-echo-mode*
                 (format *terminal* "~C" key-stroke))
             (when (memq (ldb %%kbd-char key-stroke) '(#\Rubout #\Delete))
               (setq key-stroke (dpb #o177 %%kbd-char key-stroke)))
             (select key-stroke
               (*escchr* (network-keystroke-handler))
               (#\Call (serial-tyo #\))        ; send a [top-c] (for ascii ctrl-z)
               (t (let ((char (ldb %%kbd-char key-stroke))
                        (control (ldb %%kbd-control key-stroke))
                        (meta (ldb %%kbd-meta key-stroke))
                        (super (ldb %%kbd-super key-stroke)))
                    (when (memq char '(#\return #\tab #\form #\line))
                      ;; wasnt handled before. only noticed it when telneting
                      ;; to another lambda.
                      (setq char (- char #o200)))
                    (when (eq super 1)
                      (serial-tyo #o34)         ;Control-\
                      (setq char (logior char #o40)))
                    (cond ((and *use-control-z-for-control-meta* (eq control 1) (eq meta 1))
                           (serial-tyo #\top-c))                ;;  [TOP-C] IS An Ascii CTRL-Z
                          (t
                           (when (eq meta 1)
                             (cond (*use-bit-7-for-meta*
                                    (setq char (logior #o200 (logand char #o177))))
                                   (t
                                    (serial-tyo #o33)   ;Escape
                                    (setq char (logior char #o40)))))
                           (when (eq control 1)
                             (setq char (logand char #o37)))))
                    (serial-tyo char))
                  nil))))))





(defun network-keystroke-handler ()
  (declare (special kermit-frame *escchr*))
  (terminal-network-prompt)                     ;PROMPT THE USER

  (let ((terminal-io interaction-pane))
    (if (and (boundp 'kermit-frame) kermit-frame)
        (tv:with-selection-substitute (interaction-pane kermit-frame)
          (network-keystroke-handler-internal))
      (network-keystroke-handler-internal))))


(defun network-keystroke-handler-internal ()
  (let ((key-stroke (char-upcase (terminal-tyi))))

    (unless (eq key-stroke #\rubout)
      (format interaction-pane "~:@C" key-stroke))

    (condition-case ()

        (prog1                                  ; hey, return ':close sometimes
         (selectq key-stroke

           (#/W (format interaction-pane "...scrolling is now ~:[enabled~;disabled~]"
                        (setq *wrap-around-instead-of-scrolling-flag*
                              (not *wrap-around-instead-of-scrolling-flag*))))
           (#\CLEAR-SCREEN (terminal-clear-screen))
           (#\CONTROL-CLEAR-SCREEN (send interaction-pane ':clear-screen))
           ((#\HELP #/H) (terminal-network-help))
           (#\SPACE nil)
           (#\control-y (terminal-control-y-pop-up-ed-string-hack))
           (#/E (terminal-read-eval-print))
           (#/I (TERMINAL-SET-INPUT-CHARACTERISTICS))
           (#\control-d
            (format t "~&Turning ~A Terminal Debug mode.~%"
                    (if (setq *terminal-debug-mode* (not *terminal-debug-mode*))
                        "ON" "OFF")))
           (#/D (format t "~&Turning ~A Local Echo mode.~%"
                        (if (setq *local-echo-mode* (not *local-echo-mode*))
                            "ON" "OFF")))
           (#\CONTROL-B (terminal-get-and-set-new-baud-rate))
           (#\CONTROL-S (terminal-set-status-of-connection))
           (#\STATUS (terminal-show-status-of-connection))
           (#/F (terminal-flush-input-buffer))
           (#/L (terminal-start-logging))

           (#\C-L (terminal-close-logging))
           (#/K (format interaction-pane "...closing stream ~S..."
                        *serial-stream*)
                (send *serial-stream* ':close ':abort)
                (format interaction-pane "and disconnecting.~%")
                ':close)

           ;;KERMIT PROTOCOL:

           (#/0 (terminal-transmit-nul))
           (#/B (terminal-transmit-break))
           (#/C (format interaction-pane "...disconnecting.~%")
                ':disconnect)
           (#/P (terminal-push-to-system-command-processor))
           (#/Q (terminal-quit-logging))
           (#/R (terminal-resume-logging))
           (#/T (toggle-tek-emulation))
           (#/S (terminal-show-status-of-connection))
           (#/? (terminal-network-help))
           (#\NETWORK (terminal-transmit-network-escape-character))
           (#\RUBOUT)                           ;do nothing
           (otherwise (if (eq key-stroke kermit:*escchr*)
                           (terminal-transmit-network-escape-character)
                         (if (not (eq key-stroke #\RUBOUT))
                             (format interaction-pane
                                     "  <-- ?? Unknown argument to <NETWORK> ??")))))
         (terpri interaction-pane))
      (sys:abort nil))))


(defun terminal-control-y-pop-up-ed-string-hack ()
  (let
    ((string-to-transmit?                       ;null if aborted
       (zwei:pop-up-edstring ""
                             '(:mouse)
                             ()
                             (- (tv:sheet-inside-right *terminal*)
                                (tv:sheet-inside-left *terminal*))
                             (- (tv:sheet-inside-bottom *terminal*)
                                (tv:sheet-inside-top *terminal*))
                             "Edit Text and hit <END> to transmit.")))
    (if string-to-transmit?
        (loop for i from 0 below (array-active-length string-to-transmit?)
              as char = (aref string-to-transmit? i)
              doing
               (send *serial-stream* ':tyo char)
               (cond
                 (*local-echo-mode* (send *terminal* ':tyi char)))
               (cond
                 ((send *serial-stream* ':listen)
                  (read-char-from-serial-stream-to-terminal)))))))

(DEFUN TERMINAL-NETWORK-HELP ()
  (SI:WITH-HELP-STREAM (S :LABEL '(:STRING "Terminal Network Help"
                                           :FONT FONTS:METSI :TOP :CENTERED)
                          :SUPERIOR *TERMINAL*)
    (FORMAT S "
Single-keystroke Arguments to the <NETWORK> escape:

 C              Close -- escape back to kermit command level
 <ctrl> Y       Yank some text into a pop up window and send it thru serial stream
 <ctrl> D       Debug toggle -- toggles terminal debug mode
 D              Duplex toggle -- switch between local and remote terminal echoing
 K              Kill stream -- send current stream a :close message and disconnect
 <clear-screen> Clear terminal screen
 <ctrl><clear>  Clear interaction screen
 F              Flush serial input buffer
 <ctrl>B        Control Baud -- set baud rate
 I              Set some input characterists
 E              Eval -- evaluate lisp expression
 P              Push -- break to lisp. Hit <resume> to return
 B              Transmit a break
 0              Transmit a nul
 s,<status>     Show serial stream status
 L              Log connection in a disk file
 <control>L     Close logging to disk file
 Q              Quit logging temporarily
 R              Resume logging
 T              Toggle Tektronics 4010 Emulation.
 W              Toggle scrolling
 ?,<help>,h     type this stuff  ~%")))



(defun toggle-duplex ()
  (format t "~&Local Echo mode being turned ~A.~%"
          (if *local-echo-mode* "OFF" "ON"))
  (setq *local-echo-mode* (not *local-echo-mode*)))

(defun terminal-flush-input-buffer ()
  (send *serial-stream* ':clear-input)
  (when (eq *input-flow-control* :catching-up)
    (send *serial-stream* :tyo #o21)
    (setq *input-flow-control* :ready)))


(DEFUN TERMINAL-SET-INPUT-CHARACTERISTICS ()
  (LET ((CHANGEP (Y-OR-N-P "~&Input flow control is presently ~:[off~;on~]. Turn it ~:[on~;off~]?"
                           *INPUT-FLOW-CONTROL*
                           *INPUT-FLOW-CONTROL*)))
    (if changep (setq *INPUT-FLOW-CONTROL* (not *INPUT-FLOW-CONTROL*)))
    (setq changep (prompt-and-read '(:number :input-radix 10. :or-nil t)
                                   "~&Input buffer size is now ~D. Enter new size or <RETURN>:"
                                   (send *serial-stream* :input-buffer-size)))
    (cond ((and (typep changep '(fixnum 1))
                (not (= changep (send *serial-stream* :input-buffer-size))))
           (send *serial-stream* :set-input-buffer-size changep)))
    (format t "~&Input from ~S~%with~:[out~;~] flow control, and with ~D characters buffered.~%"
            *serial-stream*
            *input-flow-control*
            (send *serial-stream* :input-buffer-size))))


;;; this macro here because this gets compiled first (before kermit-window).

(defmacro with-second-font-and-more-processing (window &body body)
  "sets window's font to its second font and turns on more processing during body.
sets them back to the way they were afterwards."
  (let ((font (gensym))
        (more-p (gensym)))

       `(let ((,font (send ,window ':current-font))
             (,more-p (send ,window ':more-p)))
          (unwind-protect
              (progn
                (send ,window ':set-current-font 1)
                (send ,window ':set-more-p t)
                ,@body)
            (send ,window ':set-current-font ,font)
            (send ,window ':set-more-p ,more-p)))))

(DEFUN TERMINAL-TRANSMIT-NETWORK-ESCAPE-CHARACTER ()
  (declare (special *escchr*))
  (serial-tyo *escchr*))



(defun terminal-show-status-of-connection ()
  (si:with-help-stream (standard-output
                         :label `(:string "Terminal Status"
                                          ,@(if (boundp 'fonts:metsi)
                                                '(:font fonts:metsi))
                                          :top :centered)
                         :superior *terminal*)
    ;; status of logging:
    (format t "~&Logging is ~A~A."
            (if *logfile* "ON" "OFF")
            (if *logfile*
                (if turn-on-logging? " and ENABLED" " but DISABLED")
              ""))
    ;; and show logfile name if any:
    (if *logfile*
        (format t "~&Logfile name is: ~A" *logfile*))
    ;; status of echo:
    (format t "~&Local-echo-mode is ~A."
            (if *local-echo-mode* "ON" "OFF"))
    ;; terminal sizes:
    (let ((font (send *terminal* ':current-font)))
      (format t "~&Terminal sizes:~% Height: ~D lines; ~D pixels per line.~A"
              (terminal-character-height)
              (tv:font-char-height font)
              (format nil "~% Width: ~D characters; ~D pixels per character."
                      (terminal-character-width)
                      (tv:font-char-width font))))
    ;; graphics emulation
    (format t "~&Tektronics 4010 emulation is ~:[OFF~;ON~]." *TEK-EMULATIONP*)

    ;; line status:
    (format t "~%Remote input flow control processing is ~:[not active~;enabled~]."
            *input-flow-control*)
    (cond ((and (get 'unix:unix-stream 'si:flavor)
                (typep *serial-stream* 'unix:unix-stream))
           (describe *serial-stream*))
          ((typep *serial-stream* 'si:sdu-serial-stream)
           (MULTIPLE-VALUE-BIND (AMOUNT MAX)
               ;; a reasonable optimization regardless of flow control nonsense
               ;; is to look ahead and process all easy characters if available,
               ;; collect them into a string and do a string out.
               (SEND-IF-HANDLES *SERIAL-STREAM* :INPUT-CHARACTERS-AVAILABLE)
             (format t "~%baud rate of ~A: ~d,~
                        ~%~D input character~p unprocessed, ~D buffer capacity."
              *serial-stream*
              (send *serial-stream* ':baud-rate)
              amount amount max))
           (send *serial-stream* :describe-status))
          (t (describe *serial-stream*)))
    ))




;;; LOGGING: here it is.

;;; All we do is this: if the incoming character from the
;;; serial stream is a printing ascii character, we put it
;;; in the log file. Printing characters are in the range
;;; 32 to 177 plus 11, 14, and 15 (octal). Linefeeds and any
;;; other control characters are not sent. No input from  the
;;; user's side is included whatsoever. The code for the actual
;;; capture of characters is thus isolated within the function
;;; read-char-from-serial-stream-to-terminal.





(defun terminal-start-logging ()
  (cond (*logfile*
         (format interaction-pane "~& Cannot open a new logfile!!")
         (tv:beep))
        ((setq *logfile*
               (open (terminal-get-logfile-name-from-user) '(:out)))
         (setq turn-on-logging? t)
         (format interaction-pane "~& Logging output to file ~A~%"
                 (send *logfile* ':truename)))
        (t (format interaction-pane "~& Unable to open logfile.")
           (tv:beep)))
  nil)












(defun terminal-get-logfile-name-from-user ()
  (let ((default-pathname
          (fs:merge-pathname-defaults
            "TERMINAL.LOG"
            (if (boundp 'kermit-default-pathname)
                kermit-default-pathname
              (fs:user-homedir)))))
    (fs:merge-pathname-defaults
      (prompt-and-read
        ':string-trim
        (format nil
                "~&Name log file: (DEFAULT: ~A)>"
                default-pathname))
      default-pathname)))











(defun terminal-quit-logging ()
  (cond ((and *logfile* turn-on-logging?)
         (format interaction-pane
                 "~&Turning off logged output to ~A~%"
                 (send *logfile* ':truename))
         (setq turn-on-logging? nil))
        ((not *logfile*)
         (format interaction-pane
                 "~& ?? There is no logging being done.~%"))
        ((not turn-on-logging?)
         (format interaction-pane
                 "~& ?? Logging is not turned on.~%"))))












(DEFUN TERMINAL-RESUME-LOGGING ()
  (COND ((AND *LOGFILE* (NOT TURN-ON-LOGGING?))
         (FORMAT INTERACTION-PANE "~&Turning on logged output to ~A~%"
                 (SEND *LOGFILE* ':TRUENAME))
         (SETQ TURN-ON-LOGGING? T))
        ((NOT *LOGFILE*)
         (FORMAT INTERACTION-PANE
                 "~& ?? There is no logging being done.~%"))
        (TURN-ON-LOGGING?
         (FORMAT INTERACTION-PANE
                 "~& ?? Logging is not turned off.~%"))))









(DEFUN TERMINAL-CLOSE-LOGGING ()
  (COND (*LOGFILE*
         (FORMAT INTERACTION-PANE "~&Closing logged output to ~A" (SEND *LOGFILE* ':TRUENAME))
         (SEND *LOGFILE* ':CLOSE)
         (SETQ *LOGFILE* NIL)
         (SETQ TURN-ON-LOGGING? NIL))
        (T (FORMAT INTERACTION-PANE
                   " ?? There is no log file to close~%"))))

(defun toggle-tek-emulation ()
  (format interaction-pane "~&Turning ~:[OFF~;ON~] Tektronics 4010 emulation.~%"
          (setq *TEK-EMULATIONP* (not *TEK-EMULATIONP*))))

#-common
(DEFUN TERMINAL-PUSH-TO-SYSTEM-COMMAND-PROCESSOR ()
  (LET ((TERMINAL-IO INTERACTION-PANE))
      (BREAK KERMIT)))

#+common
(DEFUN TERMINAL-PUSH-TO-SYSTEM-COMMAND-PROCESSOR ()
  (LET ((TERMINAL-IO INTERACTION-PANE))
      (BREAK "Kermit Break while in Connect.")))







(DEFUN TERMINAL-TRANSMIT-NUL ()
  (SERIAL-TYO 0))

(DEFUN TERMINAL-CLOSE-CONNECTION ()
  NIL)








(DEFUN TERMINAL-GET-AND-SET-NEW-BAUD-RATE ()
  (LET (TO-WHAT)
    (SELECTOR SI:PROCESSOR-TYPE-CODE EQ
      (SI:LAMBDA-TYPE-CODE
       (SEND *SERIAL-STREAM*
             ':SET-BAUD-RATE
             (IF (ZEROP (SETQ TO-WHAT
                              (PROMPT-AND-READ ':NUMBER
                                               "~%The current baud rate is ~D. Answering with 0 keeps it.~%Baud rate? >>"
                                               (SEND *SERIAL-STREAM* ':BAUD-RATE))))
                 (SEND *SERIAL-STREAM* ':BAUD-RATE)
               TO-WHAT)))
      (SI:CADR-TYPE-CODE
       (SEND *SERIAL-STREAM*
             ':PUT
             ':BAUD
             (IF (ZEROP (SETQ TO-WHAT
                              (PROMPT-AND-READ ':NUMBER
                                               "~%The current baud rate is ~D. Answering with 0 keeps it.~%Baud rate? >>"
                                               (SEND *SERIAL-STREAM* ':GET ':BAUD))))
                 (SEND *SERIAL-STREAM* ':GET ':BAUD)
               TO-WHAT))))))






(DEFUN TERMINAL-SET-STATUS-OF-CONNECTION ()
  NIL)










(DEFUN TERMINAL-READ-EVAL-PRINT ()
  (FORMAT INTERACTION-PANE "~%EVAL>")
  (LET ((DEBUG-IO INTERACTION-PANE)
        (QUERY-IO INTERACTION-PANE)
        (ERROR-OUTPUT INTERACTION-PANE)
        (TERMINAL-IO INTERACTION-PANE)
        (STANDARD-INPUT INTERACTION-PANE)
        (STANDARD-OUTPUT INTERACTION-PANE))
    (CONDITION-CASE ()
        (PRINT (EVAL (READ)))
      (SYS:ABORT NIL))))











(DEFUN TERMINAL-TRANSMIT-BREAK ()

  ;;PUT ASCII NUL [0] ON LINE FOR 1/4 SECOND

  (LOOP WITH TIME = (TIME)
        DOING (COND ((> (TIME-DIFFERENCE TIME (TIME)) 15.)
                     (RETURN NIL))
                    (T (SERIAL-TYO 0)))))





(DEFUN TERMINAL-NETWORK-PROMPT ()
  (FORMAT INTERACTION-PANE "~&NETWORK>"))




(defvar *notify-when-eof-is-read* nil)


(defflavor kterm-state
           ;; analogous to kstate.
           ;; these are all used free by connect & its subroutines.
           ((*logfile* nil)
            (turn-on-logging? nil)
            (*local-echo-mode* nil)
            (*terminal-debug-mode* nil)
            (*insert-flag* nil)
            (*reverse-video-flag* nil)
            (*cursor-save* (list 0 0))
            (*system-position* (list 0 0))
            (*use-bit-7-for-meta* nil)
            (*auto-cr-on-lf-flag* nil)
            (*INPUT-FLOW-CONTROL* NIL)
            (*notify-when-eof-is-read* nil)
            (*snarf-telnet-negotiations* nil)
            (*auto-clear-eol-on-linefeed-flag* nil)
            (*wrap-around-instead-of-scrolling-flag* nil)
            (*serial-stream-mask* #o177)
            ;; one of these days make this into a set of mixins.
            ;; although it is certainly easier to debug and develop code using specials
            ;; and THEN add the instance variable definitions.
            (*TEK-EMULATIONP* NIL)
            (*tek-in-graphics-mode* nil)
            (*tek-firstp* t)
            (*tek-hi-y* 0)
            (*tek-low-y* 0)
            (*tek-exlow-yx* 0)
            (*tek-hi-x* 0)
            (*tek-low-x* 0)
            (*tek-x* 0)
            (*tek-y* 0)
            )
           ()
  :special-instance-variables)


;; for kermit window interface to call

(defmethod (kterm-state :make-connection)
           (serial-stream terminal-stream)
  ;; now all the special instance variables are bound.
  (connect serial-stream terminal-stream))


(defmethod (kterm-state :make-dual-connection)
           (serial-stream terminal-stream)
  ;; now all the special instance variables are bound.
  (dual-connect serial-stream terminal-stream self))


(compile-flavor-methods kterm-state)


;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;;     CONNECT
;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

(defun connect (*serial-stream* *terminal*
                &aux
                (interaction-pane               ;needed by kermit
                  (if (boundp 'interaction-pane)
                      interaction-pane *terminal*))
                (*ttyfd* *serial-stream*)       ;needed by kermit
                (return-value nil)
                )
  "Make *terminal* a virtual terminal connected with *serial-stream*, a serial stream.

        A simulation of a Heath//H19//Z29 type of terminal is attempted
        for communication with ASCII terminals. Do <NETWORK> <HELP>
        for help and feature explanation. <Network>C to Close (disconnect)"

  (declare (special interaction-pane *ttyfd*))

  (let ((char-aluf (send *terminal* ':char-aluf)))

    (loop initially

          (send *terminal* ':set-char-aluf tv:alu-xor)

          with winner = (process-wait-listen *serial-stream* *terminal*)

          doing

          (cond ((eq winner *serial-stream*)
                 (when (eq (read-char-from-serial-stream-to-terminal) :eof)
                   (format t "~&End of file on serial stream, closing...~%")
                   (close *serial-stream*)
                   (setq return-value :close)
                   (loop-finish))
                 (setq winner (process-wait-listen *terminal* *serial-stream*)))
                ('else
                 (cond ((memq (setq return-value (read-char-from-keyboard-to-serial-stream))
                              '(:close :disconnect))
                        (loop-finish))  ; we're done
                       (t (setq winner (process-wait-listen *serial-stream* *terminal*))))))

          finally
          (send *terminal* ':set-char-aluf char-aluf)
          (return return-value))))

(defun dual-connect (*serial-stream* *terminal* state
                     &aux
                     (interaction-pane
                       (if (boundp 'interaction-pane)
                           interaction-pane *terminal*))
                     (*ttyfd* *serial-stream*)
                     (return-value nil))
  (declare (special interaction-pane *ttyfd*))
  (let ((p)
        (char-aluf (send *terminal* :char-aluf)))
    (unwind-protect
        (progn (send *terminal* :set-char-aluf tv:alu-xor)
               (setq p (process-run-function
                         "kermit connect input"
                         #'funcall
                         state
                         :funcall-inside-yourself
                         (closure '(*serial-stream* *terminal* *ttyfd* interaction-pane)
                                  #'(lambda ()
                                      (do ()
                                          ((eq (read-char-from-serial-stream-to-terminal) :eof)
                                           (if *notify-when-eof-is-read*
                                               (format *terminal*
                                                       "...type <NETWORK>C to disconnect~%"))
                                           ))))))
               (do ()
                   ((memq (setq return-value (read-char-from-keyboard-to-serial-stream))
                          '(:close :disconnect))))
               return-value)
      (and p (send p :kill))
      (close *serial-stream*)
      (send *terminal* :set-char-aluf char-aluf))))


;; TEKTRONICS TERMINAL EMULATION SUPPORT

(defvar *tek-hi-y* 0)
(defvar *tek-low-y* 0)
(defvar *tek-exlow-yx* 0)
(defvar *tek-hi-x* 0)
(defvar *tek-low-x* 0)
(defvar *tek-x* 0)
(defvar *tek-y* 0)


(DEFVAR *TEK-MIN-X* 0)
(DEFVAR *TEK-MAX-X* 1023)
(DEFVAR *TEK-MIN-Y* 0)
(DEFVAR *TEK-MAX-Y* 779)

(defvar *screen-x-utilization* 0.90 "the fraction of screen size in X utilized")
(defvar *screen-y-utilization* 0.90 "the fraction of screen size in Y utilized")

(defvar *TEK-move-cursorp* t "If T then :SET-CURSORPOS is done to follow MOVE and DRAW")

(defvar *tek-12bit-mode* nil)

(defmacro tek-hi-bitz-p (c)             ; for decoding Tek codes
  `(= (logand ,c #o140) #o40))

(defmacro tek-low-y-bitz-p (c)
  `(= (logand ,c #o140) #o140))

(defmacro tek-low-x-bitz-p (c)
  `(= (logand ,c #o140) #o100))


(DEFUN TEK-RESET-MEMORY ()
  (SETQ *tek-hi-y* 0)
  (SETQ *tek-low-y* 0)
  (SETQ *tek-exlow-yx* 0)
  (SETQ *tek-hi-x* 0)
  (SETQ *tek-low-x* 0)
  (SETQ *tek-x* 0)
  (SETQ *tek-y* 0))


(defun get-tek-vector ()
  (let ((z (serial-tyi)))
    (cond ((tek-hi-bitz-p z)
           (setq *tek-hi-y* (logand z #o37))
           (setq z (SERIAL-TYI))))
    (cond ((tek-low-y-bitz-p z)
           (setq *tek-low-y* (logand z #o37))
           (setq z (SERIAL-TYI))
           (cond ((tek-low-y-bitz-p z)
                  (setq *tek-exlow-yx* *tek-low-y*)
                  (setq *tek-low-y* (logand z #o37))
                  (setq z (SERIAL-TYI))))
           (cond ((tek-hi-bitz-p z)
                  (setq *tek-hi-x* (logand z #o37))
                  (setq z (SERIAL-TYI))))))
    (setq *tek-low-x* (logand z #o37))
    (if *tek-12bit-mode*
        (values (+ (lsh *tek-hi-x* 7)
                   (lsh *tek-low-x* 2)
                   (logand *tek-exlow-yx* 3))
                (+ (lsh *tek-hi-y* 7)
                   (lsh *tek-low-y* 2)
                   (logand (lsh *tek-exlow-yx* -2) 3)))
      (values (+ (lsh *tek-hi-x* 5) *tek-low-x*)
              (+ (lsh *tek-hi-y* 5) *tek-low-y*)))))


(defun TEK-move (x y)
  "Conventional plot MOVE command"
  (setq *TEK-x* x)
  (setq *TEK-y* y)
  (if *TEK-move-cursorp* (tmovecursor *TEK-x* *TEK-y*)))

(DEFUN TEK-DRAW (X Y)
  "Conventional plot DRAW command"
  (TDRAWLINE *TEK-X* *TEK-Y* (SETQ *TEK-X* X) (SETQ *TEK-Y* Y)))

(DEFUN LINEAR-MAP (X X0 X1 Y0 Y1)
  ;;       (X0 - X) Y1 + (X - X1) Y0
  ;; Y = - -------------------------
  ;;                X1 - X0
  (- (// (+ (* (- X0 X) Y1) (* (- X X1) Y0))
         (- X1 X0))))


(DEFUN NICE-EDGES (&AUX (X0 0) (Y0 0))
  (MULTIPLE-VALUE-BIND (X1 Y1)
      (SEND *TERMINAL* :INSIDE-SIZE)
    (LET ((shave-x (FIX (* 0.5 (- 1 *screen-x-utilization*) (- X1 X0))))
          (shave-y (FIX (* 0.5 (- 1 *screen-x-utilization*) (- Y1 Y0)))))
      (VALUES (+ X0 shave-x)
              (+ Y0 shave-y)
              (- X1 shave-x)
              (- Y1 shave-y)))))

(DEFUN TDRAWLINE (FROM-X FROM-Y TO-X TO-Y)
  (MULTIPLE-VALUE-BIND (X0 Y0 X1 Y1)
      (NICE-EDGES)
    (LET ((F-X (LINEAR-MAP TO-X *TEK-min-x* *TEK-max-x* X0 X1))
          (F-Y (LINEAR-MAP TO-Y *TEK-min-y* *TEK-max-y* Y1 Y0)))
      (SEND *TERMINAL* :DRAW-LINE
            (LINEAR-MAP FROM-X *TEK-min-x* *TEK-max-x* X0 X1)
            (LINEAR-MAP FROM-Y *TEK-min-y* *TEK-max-y* Y1 Y0)
            F-X
            F-Y)
      ;; unclear if Y should be offset by (send *terminal* :line-height)
      ;; or something like that.
      (IF *TEK-move-cursorp* (send *terminal* :set-cursorpos f-x  f-y)))))


(defun tmovecursor (to-x to-y)
  (MULTIPLE-VALUE-BIND (X0 Y0 X1 Y1)
      (NICE-EDGES)
    (send *terminal* :set-cursorpos
          (LINEAR-MAP TO-X *TEK-min-x* *TEK-max-x* X0 X1)
          (LINEAR-MAP TO-Y *TEK-min-y* *TEK-max-y* Y1 Y0))))



;;; More hair. TELNET-IAC

(defun enable-telnet-iac ()
  (if (boundp 'kterm-state)
      (set-in-instance kterm-state '*serial-stream-mask* #o377)
    (setq *serial-stream-mask* #o377)))

(defun get-symbol (number table)
  (let ((x (mem #'(lambda (item elem) (= item (first elem))) number table)))
    (if x
        (values (second (car x)) (third (car x))))))

(defun handle-telnet-iac (&aux c sym explain)
  (format t "~&TELNET-COMMAND: ")
  (setq c (serial-tyi))
  (multiple-value (sym explain) (get-symbol c telnet:*telsyms*))
  (if (not sym) (return-from handle-telnet-iac (format t " UNKNOWN")))
  (format t "~S(~A)" sym explain)
  (let ((function (get sym 'telnet-command)))
    (when function
      (funcall function))))

(defun (:property telnet:dont telnet-command) ()
  (telnet-command 'telnet:dont))

(defun (:property telnet:do telnet-command) ()
  (telnet-command 'telnet:do))

(defun (:property telnet:wont telnet-command) ()
  (telnet-command 'telnet:will))

(defun (:property telnet:will telnet-command) ()
  (telnet-command 'telnet:will))

(defun (:property telnet:sb telnet-command) ()
  (do ((c))
      ((or (not (setq c (serial-tyi)))
           (eq (get-symbol c telnet:*telsyms*) 'telnet:SE)))))

(defun telnet-command (what &aux c sym explain)
  (setq c (serial-tyi))
  (multiple-value (sym explain) (get-symbol c telnet:*telopts*))
  (if (not sym) (return-from telnet-command (format t "unknown option: ~D" c)))
  (format t "~S(~A)" sym explain)
  (let ((function (get sym 'telnet-option)))
    (when function
      (funcall function what))))
