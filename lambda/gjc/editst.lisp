
;; -*- Mode: Lisp; Package: System-Internals -*-

;; Lisp Machine Editor Stream

;; The rubout handler implemented here is a crock since it duplicates
;; the functions of ZWEI.  However, this editor is implemented in terms of stream
;; operations, and thus can be used from consoles other than the
;; local Lisp Machine console.  This file makes use of the definitions
;; in LMMAX;UTILS.

;; There are a couple ideas here:
;; The editor stream itself is separated from the underlying console
;; stream.  This is nice since the console stream need know nothing about
;; the rubout handler, and thus rubout handlers can be changed easily
;; without having to duplicate the code for the entire console stream.
;; In addition, it makes it easy to have one editor stream work for
;; different console streams.

;; Another thought would be to split the bare console stream into
;; an input stream from a keyboard and an output stream to a window
;; on a given monitor.  The editor stream would then bind these two
;; streams together into a single interactive i/o stream.  I can't see
;; much use for this, unless somebody wanted a keyboard stream without
;; having a window, or there were multiple keyboard types in use.

;; Terminology:

;; CONSOLE-STREAM -- a bi-directional stream to some console.  This stream
;;                   knows nothing about rubout handling.
;; EDITOR-STREAM -- a bi-directional stream connected to some console stream.
;;                  It handles both the :RUBOUT-HANDLER and :UNTYI stream operations.
;; TV-STREAM -- a bi-directional stream to the Lisp Machine console.  Special
;;              case of a console stream.
;; SUPDUP-STREAM -- a bi-directional stream to a console connected via the
;;                  Chaos net.  Special case of a console stream.

;; MAKE-TV-STREAM takes a piece of paper, and returns a
;; stream which talks to the TV terminal of the Lisp Machine.  For input,
;; it uses the keyboard.  For output, it types on the piece of paper.

(DECLARE (SPECIAL TV-STREAM-PC-PPR))

(DEFUN MAKE-TV-STREAM (TV-STREAM-PC-PPR)
  (CLOSURE '(TV-STREAM-PC-PPR) #'TV-STREAM))

(DEFSELECT (TV-STREAM TV-STREAM-DEFAULT-HANDLER)

  ;; Input operations
  (:TYI         () (KBD-TYI))
  (:TYI-NO-HANG () (KBD-TYI-NO-HANG))
  (:LISTEN      () (KBD-CHAR-AVAILABLE))
  (:CLEAR-INPUT ()
    (DO () ((NOT (KBD-CHAR-AVAILABLE)))       ;Clear hardware/firmware buffers
      (KBD-TYI)))

  ;; Output operations
  ;; TV-TYO doesn't print "control" characters.
  (:TYO (CHAR)
    (TV-TYO TV-STREAM-PC-PPR CHAR))
  (:STRING-OUT (STRING &OPTIONAL (BEGIN 0) END)
    (TV-STRING-OUT TV-STREAM-PC-PPR STRING BEGIN END))
  (:LINE-OUT (STRING &OPTIONAL (BEGIN 0) END)
    (TV-STRING-OUT TV-STREAM-PC-PPR STRING BEGIN END)
    (TV-CRLF TV-STREAM-PC-PPR))
  ;; If at the beginning of a line, clear it.  Otherwise, go
  ;; to the beginning of the next line and clear it.
  (:FRESH-LINE ()
    (COND ((= (PC-PPR-CURRENT-X TV-STREAM-PC-PPR)
              (PC-PPR-LEFT-MARGIN TV-STREAM-PC-PPR))
           (TV-CLEAR-EOL TV-STREAM-PC-PPR))
          (T (TV-CRLF TV-STREAM-PC-PPR))))
  (:TRIGGER-MORE ()
    (OR (ZEROP (PC-PPR-EXCEPTIONS TV-STREAM-PC-PPR))
        (TV-EXCEPTION TV-STREAM-PC-PPR)))
  (:BEEP (&OPTIONAL (WAVELENGTH TV-BEEP-WAVELENGTH) (DURATION TV-BEEP-DURATION))
         (%BEEP WAVELENGTH DURATION))

  ;; If no unit is specified, then the "preferred" unit is used.  This is the
  ;; unit that :SET-CURSORPOS defaults to and that :COMPUTE-MOTION uses.
  ;; The :CHARACTER unit of these operations is useless with variable width fonts.
  (:READ-CURSORPOS (&OPTIONAL (UNIT ':PIXEL))
    (MULTIPLE-VALUE-BIND (X Y)
        (TV-READ-CURSORPOS TV-STREAM-PC-PPR)
      (SELECTQ UNIT
        (:PIXEL)
        (:CHARACTER (SETQ X (// X (PC-PPR-CHAR-WIDTH TV-STREAM-PC-PPR)))
                    (SETQ Y (// Y (PC-PPR-LINE-HEIGHT TV-STREAM-PC-PPR))))
        (:OTHERWISE (FERROR NIL "~S is not a known unit." UNIT)))
      (MVRETURN X Y)))
  (:SET-CURSORPOS (X Y &OPTIONAL (UNIT ':PIXEL))
    ;; For compatibility
    (IF (FIXP UNIT) (PSETQ UNIT X X Y Y UNIT))
    (SELECTQ UNIT
      (:PIXEL)
      (:CHARACTER (SETQ X (* X (PC-PPR-CHAR-WIDTH TV-STREAM-PC-PPR)))
                  (SETQ Y (* Y (PC-PPR-LINE-HEIGHT TV-STREAM-PC-PPR))))
      (:OTHERWISE (FERROR NIL "~S is not a known unit." UNIT)))
    (TV-SET-CURSORPOS TV-STREAM-PC-PPR X Y))
  (:SET-CURSORPOS-RELATIVE (X Y &OPTIONAL (UNIT ':PIXEL))
    (SELECTQ UNIT
      (:PIXEL)
      (:CHARACTER (SETQ X (* X (PC-PPR-CHAR-WIDTH TV-STREAM-PC-PPR)))
                  (SETQ Y (* Y (PC-PPR-LINE-HEIGHT TV-STREAM-PC-PPR))))
      (:OTHERWISE (FERROR NIL "~S is not a known unit." UNIT)))
    (TV-SET-CURSORPOS-RELATIVE TV-STREAM-PC-PPR X Y))
  (:SIZE-IN-CHARACTERS ()
    (MVRETURN (// (- (PC-PPR-RIGHT-MARGIN TV-STREAM-PC-PPR)
                     (PC-PPR-LEFT-MARGIN TV-STREAM-PC-PPR))
                  (PC-PPR-CHAR-WIDTH TV-STREAM-PC-PPR))
              (// (- (PC-PPR-BOTTOM-MARGIN TV-STREAM-PC-PPR)
                     (PC-PPR-TOP-MARGIN TV-STREAM-PC-PPR))
                  (PC-PPR-LINE-HEIGHT TV-STREAM-PC-PPR))))

  ;; Compute the new cursor position if STRING were output at the specified
  ;; point.  NIL for X-POS and Y-POS mean use the current cursor position.
  ;; X-POS and Y-POS are in pixels.
  (:COMPUTE-MOTION (X-POS Y-POS STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
    (TV-COMPUTE-MOTION TV-STREAM-PC-PPR X-POS Y-POS STRING BEGIN END))

  ;; Compute the motion for printing a string and then move the cursor there.
  ;; This eliminates the problem of knowing whether to use the :PIXEL or :CHARACTER
  ;; unit when calling :SET-CURSORPOS.  Since the string is passed in here as an
  ;; argument, this stream need know nothing about the typein buffer maintained by the
  ;; editor stream.  This might be better named :MOVE-OVER-STRING.
  (:CURSOR-MOTION (X-POS Y-POS STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
    (MULTIPLE-VALUE-BIND (X Y HOW-FAR)
        (TV-COMPUTE-MOTION TV-STREAM-PC-PPR X-POS Y-POS STRING BEGIN END)
      ;; Check for page wrap-around.
      (IF HOW-FAR
          (MULTIPLE-VALUE (X Y) (TV-COMPUTE-MOTION TV-STREAM-PC-PPR 0 0 STRING HOW-FAR END)))
      (TV-SET-CURSORPOS TV-STREAM-PC-PPR X Y)))

  ;; This assumes that the cursor is positioned before the string to be underlined.
  ;; The operation should be defined to xor, so that underlining twice erases.
  (:UNDERLINE (STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING))
                      &AUX (X1 (PC-PPR-CURRENT-X TV-STREAM-PC-PPR))
                           (Y1 (+ (PC-PPR-CURRENT-Y TV-STREAM-PC-PPR)
                                  (PC-PPR-LINE-HEIGHT TV-STREAM-PC-PPR))))
    (MULTIPLE-VALUE-BIND (X2 Y2)
        (TV-COMPUTE-MOTION TV-STREAM-PC-PPR X1 Y1 STRING BEGIN END)
      (TV-DRAW-LINE X1 Y1 X2 Y2 TV-ALU-XOR (PC-PPR-SCREEN TV-STREAM-PC-PPR))))

  ;; Font hackery.  FONT-LIST is a list of strings describing fonts, since
  ;; internal font representations are per-stream.  Try to avoid calling
  ;; TV-REDEFINE-PC-PPR whenever a font change is made since this reallocates
  ;; a new font map rather than reusing the old one.  If the typein buffer is
  ;; to contain font changes, then it should contain 16-bit characters.
  (:SET-FONTS (&REST FONT-LIST)
    (TV-REDEFINE-PC-PPR TV-STREAM-PC-PPR ':FONTS
                        (MAPCAR #'(LAMBDA (FONT)
                                    (SETQ FONT (INTERN FONT "FONTS"))
                                    (IF (BOUNDP FONT) (SYMEVAL FONT) FONTS:CPTFONT))
                                FONT-LIST)))
  (:SET-CURRENT-FONT (N)
    (TV-TYO TV-STREAM-PC-PPR (+ 240 N)))
  (:CHAR-WIDTH (&OPTIONAL CHAR FONT)
    (COND ((NOT CHAR) (PC-PPR-CHAR-WIDTH TV-STREAM-PC-PPR))
          (T (SETQ FONT (IF (FIXP FONT)
                            (AREF (PC-PPR-FONT-MAP TV-STREAM-PC-PPR))
                            (PC-PPR-CURRENT-FONT TV-STREAM-PC-PPR)))
             (TV-CHAR-WIDTH TV-STREAM-PC-PPR CHAR FONT))))
  (:LINE-HEIGHT () (PC-PPR-LINE-HEIGHT TV-STREAM-PC-PPR))
  (:STRING-WIDTH (STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
    (TV-STRING-LENGTH TV-STREAM-PC-PPR STRING BEGIN END))

  (:DRAW-LINE (X0 Y0 X1 Y1 &OPTIONAL (TV-ALU TV-ALU-IOR))
    (LET ((TOP (PC-PPR-TOP TV-STREAM-PC-PPR))
          (LEFT (PC-PPR-LEFT TV-STREAM-PC-PPR)))
      (TV-DRAW-LINE (+ LEFT X0) (+ TOP Y0) (+ LEFT X1) (+ TOP Y1) TV-ALU
                    TV-DEFAULT-SCREEN)))

  ;; The character or string to be inserted is passed along so that variable
  ;; width fonts can work correctly.  Fixed width font console
  ;; streams can ignore this argument.  By default, the characters are printed
  ;; in the newly created whitespace since this is what happens most of the time.
  ;; :INSERT-STRING and :DELETE-STRING both assume that the strings contain no newlines.
  (:INSERT-CHAR (&OPTIONAL (CHAR #\SPACE) (COUNT 1))
    (TV-INSERT-WIDTH TV-STREAM-PC-PPR
                     (* COUNT (TV-CHAR-WIDTH TV-STREAM-PC-PPR CHAR
                                             (PC-PPR-CURRENT-FONT TV-STREAM-PC-PPR))))
    (DOTIMES (I COUNT) (TV-TYO TV-STREAM-PC-PPR CHAR)))
  (:INSERT-STRING (STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
    (TV-INSERT-WIDTH TV-STREAM-PC-PPR (TV-STRING-LENGTH TV-STREAM-PC-PPR STRING BEGIN END))
    (TV-STRING-OUT TV-STREAM-PC-PPR STRING BEGIN END))
  (:DELETE-CHAR (&OPTIONAL (CHAR #\SPACE) (COUNT 1))
    (TV-DELETE-WIDTH TV-STREAM-PC-PPR
                     (* COUNT (TV-CHAR-WIDTH TV-STREAM-PC-PPR CHAR
                                             (PC-PPR-CURRENT-FONT TV-STREAM-PC-PPR)))))
  (:DELETE-STRING (STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
    (TV-DELETE-WIDTH TV-STREAM-PC-PPR (TV-STRING-LENGTH TV-STREAM-PC-PPR STRING BEGIN END)))

  (:CLEAR-SCREEN () (TV-CLEAR-PC-PPR TV-STREAM-PC-PPR))
  (:CLEAR-EOL    () (TV-CLEAR-EOL TV-STREAM-PC-PPR))

  ;; Operations particular to this type of stream
  (:PC-PPR () TV-STREAM-PC-PPR)
  (:SET-PC-PPR (PC-PPR) (SETQ TV-STREAM-PC-PPR PC-PPR))
  )

(DEFUN TV-STREAM-DEFAULT-HANDLER (OP &OPTIONAL ARG1 &REST REST)
  (STREAM-DEFAULT-HANDLER #'TV-STREAM OP ARG1 REST))

;; The stream in AI:LMIO;SUPSER also follows this protocol.


;; Below is the definition of the editor stream.  The particular
;; "editor" or rubout handler used is a parameter of the stream.
;; This stream handles the :UNTYI and :RUBOUT-HANDLER operations
;; so that console streams don't have to worry about this.

;; Set up the :WHICH-OPERATIONS list ahead of time so as to avoid
;; excessive consing.  This operation gets invoked every time READ
;; or READLINE is called, for instance.

(DECLARE (SPECIAL ES-CONSOLE-STREAM ES-EDITOR ES-UNTYI-CHAR
                  ES-KILL-RING ES-BUFFER ES-WHICH-OPERATIONS
                  ES-X-ORIGIN ES-Y-ORIGIN))

(DEFUN MAKE-EDITOR-STREAM (ES-CONSOLE-STREAM ES-EDITOR &OPTIONAL (KILL-RING-SIZE 20.))
  (LET ((ES-UNTYI-CHAR NIL) (ES-KILL-RING NIL)
        (ES-BUFFER) (ES-X-ORIGIN) (ES-Y-ORIGIN)
        ;; Be sure to include :CLEAR-INPUT for BREAK.  Don't include :TYI and
        ;; :TYI-NO-HANG since we can't really do them unless the underlying
        ;; stream can.
        (ES-WHICH-OPERATIONS (UNION '(:UNTYI :RUBOUT-HANDLER :CLEAR-INPUT :LISTEN)
                                    (FUNCALL ES-CONSOLE-STREAM ':WHICH-OPERATIONS))))
    (DOTIMES (I KILL-RING-SIZE)
      (PUSH (MAKE-ARRAY NIL 'ART-8B 400 NIL '(0 0 0)) ES-KILL-RING))
    ;; Make the kill ring into a real ring.
    (RPLACD (LAST ES-KILL-RING) ES-KILL-RING)
    (SETQ ES-BUFFER (CAR ES-KILL-RING))
    (CLOSURE '(ES-CONSOLE-STREAM ES-EDITOR ES-UNTYI-CHAR
                                 ES-KILL-RING ES-BUFFER ES-WHICH-OPERATIONS)
             #'EDITOR-STREAM)))

;; FILL-POINTER points to what has been typed so far.  SCAN-POINTER points
;; to what has been read so far.  TYPEIN-POINTER points to where in the
;; middle of the line we are typing.

(DEFMACRO FILL-POINTER   () '(ARRAY-LEADER ES-BUFFER 0))
(DEFMACRO SCAN-POINTER   () '(ARRAY-LEADER ES-BUFFER 1))
(DEFMACRO TYPEIN-POINTER () '(ARRAY-LEADER ES-BUFFER 2))

;; The third argument in the DEFSELECT function specification means
;; that we're handling :WHICH-OPERATIONS ourselves.
;; May want to flush UNTYI-CHAR infavor of using the buffer.

(DEFSELECT (EDITOR-STREAM EDITOR-STREAM-DEFAULT-HANDLER T)
  (:WHICH-OPERATIONS () ES-WHICH-OPERATIONS)

  ;; Input Operations
  ;; Don't need EOF-OPTION for :TYI since this will always be an interactive stream.
  ;; Accept the option anyway since some functions may work for both interactive and
  ;; batch streams.
  (:TYI (&OPTIONAL IGNORE &AUX SCAN-POINTER)
    (COND
      ;; Give buffered-back character, if any.  Don't bother echoing or putting
      ;; it in the editor buffer since this has already happened.  We are guaranteed
      ;; that the untyi'ed char is the one just tyi'ed.
      (ES-UNTYI-CHAR
        (PROG1 ES-UNTYI-CHAR (SETQ ES-UNTYI-CHAR NIL)))
      ;; If not using a rubout processor, go directly to the underlying stream.
      ((NOT RUBOUT-HANDLER)
       (FUNCALL ES-CONSOLE-STREAM ':TYI))
      ;; If using a rubout processor and unread input exists, then return it.
      ((> (FILL-POINTER) (SETQ SCAN-POINTER (SCAN-POINTER)))
       (SETF (SCAN-POINTER) (1+ SCAN-POINTER))
       (AREF ES-BUFFER SCAN-POINTER))
      ;; Else get more input via the rubout processor.
      ;; This is kind of a kludge.  We really want a handle of SELF.
      ;; The stream state will be passed along since the specials are still bound.
      (T (FUNCALL ES-EDITOR #'EDITOR-STREAM))))
  (:TYI-NO-HANG (&AUX SCAN-POINTER)
    (COND
      ;; Give buffered-back character, if any.
      (ES-UNTYI-CHAR
        (PROG1 ES-UNTYI-CHAR (SETQ ES-UNTYI-CHAR NIL)))
      ;; If not using a rubout processor, go directly to the underlying stream.
      ((NOT RUBOUT-HANDLER)
       (FUNCALL ES-CONSOLE-STREAM ':TYI-NO-HANG))
      ;; Give buffered input from the rubout processor, if unread input exists.
      ((> (FILL-POINTER) (SETQ SCAN-POINTER (SCAN-POINTER)))
       (SETF (SCAN-POINTER) (1+ SCAN-POINTER))
       (AREF ES-BUFFER SCAN-POINTER))
      ;; In the case where the rubout handler is on, but no unread input exists,
      ;; just go to the underlying stream and try to get a character.  Don't
      ;; bother with echoing it or putting it in the character buffer, although
      ;; this is probably what we should do.
      (T (FUNCALL ES-CONSOLE-STREAM ':TYI-NO-HANG))))
  (:UNTYI (CHAR)
    (SETQ ES-UNTYI-CHAR CHAR))
  ;; Should we check here to see of the console stream really supports :LISTEN?
  (:LISTEN ()
    (COND (ES-UNTYI-CHAR)
          ((> (FILL-POINTER) (SCAN-POINTER))
           (AREF ES-BUFFER (SCAN-POINTER)))
          (T (FUNCALL ES-CONSOLE-STREAM ':LISTEN))))
  ;; If the rubout handler is on, empty the buffer.  Perhaps we should clear the
  ;; input from the screen in this case as well since the user is still typing.
  ;; I don't see how this could ever happen.  If the handler is off, don't clear
  ;; the buffer since we want to save the input to be yanked back.
  (:CLEAR-INPUT ()
    (COND (RUBOUT-HANDLER
            (SETF (FILL-POINTER) 0)
            (SETF (SCAN-POINTER) 0)
            (SETF (TYPEIN-POINTER) 0)))
    (SETQ ES-UNTYI-CHAR NIL)
    (FUNCALL ES-CONSOLE-STREAM ':CLEAR-INPUT))

  ;; The first argument for this operation is an alist of options
  ;; or NIL.  The options currently supported are :FULL-RUBOUT, :PROMPT,
  ;; :REPROMPT and :PASS-THROUGH.

  (:RUBOUT-HANDLER (OPTIONS READ-FUNCTION &REST ARGS TEMP)
    ;; Save whatever the previous input was by cycling through the kill ring.
    ;; If the previous input was the empty string, as in exiting via the :FULL-RUBOUT
    ;; option, don't bother.
    (COND ((NOT (ZEROP (FILL-POINTER)))
           (SETQ ES-KILL-RING (CIRCULAR-LIST-LAST ES-KILL-RING))
           (SETQ ES-BUFFER (CAR ES-KILL-RING))))
    ;; Empty the rubout handler buffer of the new buffer.
    (SETF (FILL-POINTER) 0)
    (SETF (TYPEIN-POINTER) 0)

    ;; Prompt if desired.
    (SETQ TEMP (ASSQ ':PROMPT OPTIONS))
    (IF TEMP (FUNCALL (CADR TEMP) #'EDITOR-STREAM NIL))
    ;; Record the position on the screen at which the handler was entered.
    (IF (MEMQ ':READ-CURSORPOS ES-WHICH-OPERATIONS)
        (MULTIPLE-VALUE (ES-X-ORIGIN ES-Y-ORIGIN)
          (FUNCALL ES-CONSOLE-STREAM ':READ-CURSORPOS)))

    ;; Absorb and echo the untyi'ed char.  We don't need to do this while inside
    ;; the rubout handler since characters get echoed there.  This code runs when
    ;; READ is called from inside the error handler, for instance.
    ;; This is kind of a crock since the rubout handler should decide what to do
    ;; with the character.
    (COND (ES-UNTYI-CHAR (ARRAY-PUSH ES-BUFFER ES-UNTYI-CHAR)
                         (FUNCALL ES-CONSOLE-STREAM ':TYO (LDB %%KBD-CHAR ES-UNTYI-CHAR))
                         (SETQ ES-UNTYI-CHAR NIL)))

    ;; These two specials used for communication up and down the stack.
    ;; First says we're inside the rubout handler, and the second passes
    ;; options down to the editor function.  These specials could be flushed if this
    ;; state information were kept in the stream, but it would have to be explicitly
    ;; turned on and off when a specific stack frame was entered and exited.
    (DO ((RUBOUT-HANDLER T)
         (TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS OPTIONS))
        (NIL)
      ;; Loop until normal (non-throw) exit, which will pass by the CATCH and DO.
      ;; Each time we enter this loop (after the first time), we are preparing for
      ;; a re-scan of the input string, so reset the scan pointer.
      (SETF (SCAN-POINTER) 0)
      (*CATCH 'RUBOUT-HANDLER
              (PROGN
                (ERRSET
                  ;; APPLY is more efficient than LEXPR-FUNCALL if the read
                  ;; function takes a &rest argument.
                  (MULTIPLE-VALUE-RETURN (APPLY READ-FUNCTION ARGS)))
                ;; On read error, reprint contents of buffer so user can rub out ok.
                ;; The ERRSET lets the error message get printed.
                (IF (MEMQ ':READ-CURSORPOS ES-WHICH-OPERATIONS)
                    (MULTIPLE-VALUE (ES-X-ORIGIN ES-Y-ORIGIN)
                      (FUNCALL ES-CONSOLE-STREAM ':READ-CURSORPOS)))
                (FUNCALL ES-CONSOLE-STREAM ':STRING-OUT ES-BUFFER)
                ;; If the user makes an error during read-time, swallow
                ;; all input until a rubout (or some editing character)
                ;; is typed.  When the edit is complete, we will
                ;; throw back out of here.  This should be changed to stop
                ;; echoing until the edit is done.
                (DO () (NIL)
                  (EDITOR-STREAM ':TYI))))
      ;; When a rubout or other editing operation is done, throws back to the
      ;; catch to reread the input.  But if the :FULL-RUBOUT option was specified
      ;; and everything was rubbed out, we return NIL and the specified value.
      (IF (AND (ZEROP (FILL-POINTER))
               (SETQ TEMP (ASSQ ':FULL-RUBOUT OPTIONS)))
          (RETURN NIL (CADR TEMP)))
      )))

;; Returns the "last" element in a circular list, i.e. the cons pointing to the cons we
;; are holding on to.

(DEFUN CIRCULAR-LIST-LAST (LIST)
  (DO ((L (CDR LIST) (CDR L))
       (PREV LIST L))
      ((EQ LIST L) PREV)))

;; (DEFUN CIRCULAR-LIST-LENGTH (LIST)
;;   (IF (NULL LIST) 0
;;       (DO ((L (CDR LIST) (CDR L))
;;         (I 1 (1+ I)))
;;        ((EQ LIST L) I))))

;; Forward all other operations to the underlying stream rather than
;; STREAM-DEFAULT-HANDLER.  Again, we really need a handle on SELF.

(DEFUN EDITOR-STREAM-DEFAULT-HANDLER (&REST REST)
  (LEXPR-FUNCALL ES-CONSOLE-STREAM REST))


;; Below are two editor functions which form a part of the editor stream.
;; The first is a simple display editor which is a partial emacs, and the
;; second is a printing terminal rubout handler for use via supdup.

;; These functions get called whenever a :TYI message is sent to the stream
;; and we are inside the rubout handler.  If the user types a normal
;; character it is echoed, put in the buffer, and returned.  If the user
;; types a rubout, or any other editing character, any number of editing
;; commands are processed by modifying the buffer, then, when the first
;; non-editing character is typed, a throw is done back to the top level of
;; the read function and the buffered input will be re-scanned.  The
;; character must be typed at the end of the line in order for the throw to
;; take place.  This may be a bug.

(DEFVAR DISPLAY-EDITOR-COMMAND-ALIST NIL)

(DEFUN DISPLAY-EDITOR
       (STREAM &AUX CH CH-CHAR CH-CONTROL-META COMMAND
               (OPTIONS TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS)
               (RUBBED-OUT-SOME NIL)
               (PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH OPTIONS)))
               (NUMERIC-ARG NIL))
  ;; Read characters.  If an ordinary character typed and nothing rubbed out,
  ;; return immediately.  Otherwise, let all editing operations complete
  ;; before returning.
  ;; This {{#/}} is correct, but is it needed?
  (SETF (TYPEIN-POINTER) (FILL-POINTER))
  (*CATCH
    'RETURN-CHARACTER
    (DO () (NIL)
      ;; Read a character from the underlying stream.  This is a kludge for
      ;; speed to avoid making two function calls -- one back to the editor
      ;; stream and then one to the console stream.
      ;; Really should be:
      ;; (LET ((RUBOUT-HANDLER NIL)) (FUNCALL STREAM ':TYI))
      ;; or better (FUNCALL STREAM ':RAW-TYI) which would bypass the rubout handler.
      (SETQ CH (FUNCALL ES-CONSOLE-STREAM ':TYI))
      (SETQ CH-CHAR (LDB %%KBD-CHAR CH))
      (SETQ CH-CONTROL-META (LDB %%KBD-CONTROL-META CH))
      (SETQ COMMAND (ASSQ (CHAR-UPCASE CH) DISPLAY-EDITOR-COMMAND-ALIST))

      (COND
        ;; Don't touch this character
        ((MEMQ CH PASS-THROUGH)
         (DE-INSERT-CHAR STREAM CH 1 RUBBED-OUT-SOME)
         (SETQ RUBBED-OUT-SOME T))
        ;; A stream editor character of some sort.  The RUBBED-OUT-SOME bit can
        ;; only be cleared by entering this function again.  The stream editor
        ;; editor function is passed the stream and a numeric argument.
        (COMMAND
          (SETQ RUBBED-OUT-SOME
                (OR (FUNCALL (CDR COMMAND) STREAM (OR NUMERIC-ARG 1)) RUBBED-OUT-SOME))
          (SETQ NUMERIC-ARG NIL))

        ;;Handle control-number, control-u, and control-minus specially.
        ((AND (NOT (ZEROP CH-CONTROL-META))
              ({{#/}} CH-CHAR #/0)
              ({{#/}} CH-CHAR #/9))
         (SETQ CH-CHAR (- CH-CHAR #/0))
         (SETQ NUMERIC-ARG (+ (* (OR NUMERIC-ARG 0) 10.) CH-CHAR)))
        ((= (CHAR-UPCASE CH) #{{#/}}/U)
         (SETQ NUMERIC-ARG (* (OR NUMERIC-ARG 1) 4)))

        ;; Some other random control character -- beep and ignore
        ((NOT (ZEROP CH-CONTROL-META))
         (FUNCALL STREAM ':BEEP)
         (SETQ NUMERIC-ARG NIL))

        ;; Self-inserting character.  Set RUBBED-OUT-SOME since if we return,
        ;; we were typing in the middle of the line.  Typing at the end of the
        ;; line throws to RETURN-CHARACTER.
        (T (DE-INSERT-CHAR STREAM CH (OR NUMERIC-ARG 1) RUBBED-OUT-SOME)
           (SETQ NUMERIC-ARG NIL)
           (SETQ RUBBED-OUT-SOME T))))))

;; A self-inserting character.  A character gets here by being non-control-meta
;; and not having an editing command associated with it.  Or it can get here
;; via the :PASS-THROUGH option.  If a control-meta character gets here, it is
;; echoed as one, but loses its control-meta bits in the buffer.  Also, inserting
;; a character with bucky bits on doesn't currently work since TV-CHAR-WIDTH returns
;; 0 as the width, and TV-TYO can't print it.

(DEFUN DE-INSERT-CHAR (STREAM CH N RUBBED-OUT-SOME &AUX STRING)
  (SETQ STRING (MAKE-ARRAY NIL 'ART-16B N))
  (DOTIMES (I N) (ASET CH STRING I))
  (UNWIND-PROTECT (DE-INSERT-STRING STREAM STRING 0 N RUBBED-OUT-SOME)
                  (RETURN-ARRAY STRING)))

;; Insert a string into the buffer and print it on the screen.  The string
;; is inserted at the current cursor position, and the cursor is left after the string.

(DEFUN DE-INSERT-STRING (STREAM STRING BEGIN END RUBBED-OUT-SOME
                                &AUX (WIDTH (- END BEGIN))
                                     (TYPEIN-POINTER (TYPEIN-POINTER))
                                     (FILL-POINTER   (FILL-POINTER)))
  ;; Increase the size of of the typein buffer, if necessary.
  (IF (= FILL-POINTER (ARRAY-LENGTH ES-BUFFER))
      (ADJUST-ARRAY-SIZE ES-BUFFER (+ (* 2 FILL-POINTER) WIDTH)))
  ;; Make room for the characters to be inserted.  Be sure to increment the fill

  ;; pointer first so that we don't reference outside the active part of the string.
  (INCREMENT FILL-POINTER   WIDTH)
  (INCREMENT TYPEIN-POINTER WIDTH)
  (SETF (FILL-POINTER)   FILL-POINTER)
  (SETF (TYPEIN-POINTER) TYPEIN-POINTER)
  (COPY-VECTOR-SEGMENT (- FILL-POINTER TYPEIN-POINTER)
                       ES-BUFFER (- TYPEIN-POINTER WIDTH)
                       ES-BUFFER TYPEIN-POINTER)
  ;; Now copy the string in.
  (COPY-VECTOR-SEGMENT WIDTH STRING BEGIN ES-BUFFER (- TYPEIN-POINTER WIDTH))
  ;; Update the screen.
  (COND
    ;; If the string is being inserted at the end of the line, then we will return
    ;; from the rubout handler.  If the input buffer has been edited, then we will
    ;; rescan.  If not, we return the first character of the string and leave the
    ;; scan pointer pointing at the second character, thus avoiding a rescan.
    ;; Also, updating the display is easier in this case since we don't have to hack
    ;; insert chars.  This is the case for normal typein -- the scan pointer will
    ;; keep up with the typein pointer.
    ;; I think we need an option so that yanks at the end of the line don't throw
    ;; but remain in the editor.
    ((= TYPEIN-POINTER FILL-POINTER)
     (FUNCALL STREAM ':STRING-OUT STRING BEGIN END)
     (COND (RUBBED-OUT-SOME (*THROW 'RUBOUT-HANDLER T))
           (T (INCREMENT (SCAN-POINTER))
              (*THROW 'RETURN-CHARACTER (AREF STRING BEGIN)))))
    ;; If typing in the middle of the line, we just insert the text and don't bother
    ;; rescanning.  That's only done when text appears at the end of the line to keep
    ;; the transcript from being confusing.  This may be the wrong thing.
    ;; No need to update the scan pointer in this case since we're going to throw back
    ;; to the rubout handler.
    ((MEMQ ':INSERT-STRING ES-WHICH-OPERATIONS)
     (FUNCALL STREAM ':INSERT-STRING STRING BEGIN END))
    ;; If the console can't insert chars, simulate it vt52 style.
    (T (FUNCALL STREAM ':CLEAR-EOL)
       (FUNCALL STREAM ':STRING-OUT STRING BEGIN END)
       (FUNCALL STREAM ':STRING-OUT ES-BUFFER TYPEIN-POINTER)
       (DE-CURSOR-MOTION STREAM TYPEIN-POINTER)))
  ;; If we didn't throw out, then the input has been modified.  Back to the editor.
  T)


;; Display Editor Commands

(DEFMACRO DEF-DE-COMMAND ((NAME . CHARS) ARGS . BODY)
  `(PROGN 'COMPILE
          ,@(MAPCAR #'(LAMBDA (CHAR)
                              `(PUSH '(,CHAR . ,NAME) DISPLAY-EDITOR-COMMAND-ALIST))
                    CHARS)
          (DEFUN ,NAME ,ARGS . ,BODY)))

;; Moves the cursor to a given position on the screen only.  Does no
;; bounds checking.  If the index into the buffer corresponding with the
;; screen cursor position is known, and it is before the position we want to
;; go to, then this will run faster.

(DEFUN DE-CURSOR-MOTION (STREAM POSITION &OPTIONAL CURRENT-POSITION)
  (IF (AND CURRENT-POSITION ({{#/}} POSITION CURRENT-POSITION))
      (FUNCALL STREAM ':CURSOR-MOTION NIL NIL ES-BUFFER CURRENT-POSITION POSITION)
      (FUNCALL STREAM ':CURSOR-MOTION ES-X-ORIGIN ES-Y-ORIGIN ES-BUFFER 0 POSITION)))

;; Moves the cursor on the screen and in the buffer.  Checks appropriately.

(DEFUN DE-SET-POSITION (STREAM POSITION)
  (SETQ POSITION (MIN (MAX POSITION 0) (FILL-POINTER)))
  (DE-CURSOR-MOTION STREAM POSITION (TYPEIN-POINTER))
  (SETF (TYPEIN-POINTER) POSITION)
  NIL)

;; Reprinting Input

(DEFUN DE-REPRINT-INPUT (STREAM CHAR &AUX PROMPT)
  (SETQ PROMPT (OR (ASSQ ':REPROMPT TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS)
                   (ASSQ ':PROMPT TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS)))
  (IF PROMPT (FUNCALL (CADR PROMPT) STREAM CHAR))
  (MULTIPLE-VALUE (ES-X-ORIGIN ES-Y-ORIGIN) (FUNCALL STREAM ':READ-CURSORPOS))
  (FUNCALL STREAM ':STRING-OUT ES-BUFFER)
  (DE-CURSOR-MOTION STREAM (TYPEIN-POINTER))
  NIL)


;; FORM clears and redisplays input.  Cursor is left where it was before.
(DEF-DE-COMMAND (DE-FORM #\FORM #{{#/}}/L) (STREAM &OPTIONAL IGNORE)
  (FUNCALL STREAM ':CLEAR-SCREEN)
  (DE-REPRINT-INPUT STREAM #\FORM))

;; VT reprints input on next line.  Cursor is left at the end of the typein line
.
(DEF-DE-COMMAND (DE-VT #\VT) (STREAM &OPTIONAL IGNORE)
  (DE-END-OF-BUFFER STREAM)
  (FUNCALL STREAM ':LINE-OUT "{{#/}}")
  (DE-REPRINT-INPUT STREAM #\VT))


;; Moving Around

;; Returns the position of the first newline appearing after POS.

(DEFUN DE-SEARCH-FORWARD-NEWLINE (POS)
  (DO ((FILL-POINTER (FILL-POINTER))
       (I POS (1+ I)))
      ((OR (= I FILL-POINTER) (= (AREF ES-BUFFER I) #\RETURN)) FILL-POINTER)))


;; Returns the position of the first newline appearing before POS-1.
;; Returns -1 if reached the beginning of the buffer.

(DEFUN DE-SEARCH-BACKWARD-NEWLINE (POS)
  (DO ((I (1- POS) (1- I)))
      ((OR (= I -1) (= (AREF ES-BUFFER I) #\RETURN)) I)))

(DEF-DE-COMMAND (DE-BEGINNING-OF-LINE #{{#/}}/A) (STREAM &OPTIONAL IGNORE)
  (DE-SET-POSITION STREAM (1+ (DE-SEARCH-BACKWARD-NEWLINE (TYPEIN-POINTER)))))

(DEF-DE-COMMAND (DE-END-OF-LINE #{{#/}}/E) (STREAM &OPTIONAL IGNORE)
  (DE-SET-POSITION STREAM (DE-SEARCH-FORWARD-NEWLINE (TYPEIN-POINTER))))

(DEF-DE-COMMAND (DE-BEGINNING-OF-BUFFER #{{#/}}/<) (STREAM &OPTIONAL IGNORE)
  (DE-SET-POSITION STREAM 0))

(DEF-DE-COMMAND (DE-END-OF-BUFFER #{{#/}}/>) (STREAM &OPTIONAL IGNORE)
  (DE-SET-POSITION STREAM (FILL-POINTER)))

(DEF-DE-COMMAND (DE-FORWARD-CHAR #{{#/}}/F) (STREAM &OPTIONAL (N 1))
  (DE-SET-POSITION STREAM (+ (TYPEIN-POINTER) N)))

(DEF-DE-COMMAND (DE-BACKWARD-CHAR #{{#/}}/B) (STREAM &OPTIONAL (N 1))
  (DE-SET-POSITION STREAM (- (TYPEIN-POINTER) N)))

(DEF-DE-COMMAND (DE-PREVIOUS-LINE #{{#/}}/P) (STREAM &OPTIONAL (N 1) &AUX LINE-BEGIN INDENT)
  (SETQ LINE-BEGIN (DE-SEARCH-BACKWARD-NEWLINE (TYPEIN-POINTER)))
  (SETQ INDENT (- (TYPEIN-POINTER) LINE-BEGIN))
  (DOTIMES (I N)
    (IF (= LINE-BEGIN -1) (RETURN))
    (SETQ LINE-BEGIN (DE-SEARCH-BACKWARD-NEWLINE LINE-BEGIN)))
  ;; When moving up from a long line to a short line, be sure not to go off the end.
  (DE-SET-POSITION STREAM
                   (+ LINE-BEGIN (MIN INDENT (DE-SEARCH-FORWARD-NEWLINE (1+ LINE-BEGIN))))))

(DEF-DE-COMMAND (DE-NEXT-LINE #{{#/}}/N) (STREAM &OPTIONAL (N 1) &AUX LINE-BEGIN INDENT)
  (SETQ LINE-BEGIN (DE-SEARCH-BACKWARD-NEWLINE (TYPEIN-POINTER)))
  (SETQ INDENT (- (TYPEIN-POINTER) LINE-BEGIN))
  (DOTIMES (I N)
    (COND ((= LINE-BEGIN (FILL-POINTER))
           (SETQ LINE-BEGIN (DE-SEARCH-BACKWARD-NEWLINE LINE-BEGIN))
           (RETURN)))
    (SETQ LINE-BEGIN (DE-SEARCH-FORWARD-NEWLINE (1+ LINE-BEGIN))))
  (DE-SET-POSITION STREAM
                   (+ LINE-BEGIN (MIN INDENT (DE-SEARCH-FORWARD-NEWLINE (1+ LINE-BEGIN))))))

;; Deleting Things

;; Deletes a buffer interval as marked by two pointers passed in.  The typein pointer
;; is left at the beginning of the interval.

(DEFUN DE-DELETE-STRING (STREAM BEGIN END &AUX WIDTH TYPEIN-POINTER
                                (FILL-POINTER (FILL-POINTER)))
  (SETQ BEGIN (MAX BEGIN 0))
  (SETQ END   (MIN END FILL-POINTER))
  (SETQ WIDTH (- END BEGIN))
  (DE-SET-POSITION STREAM BEGIN)
  ;; Set this after moving the cursor to the beginning of the string.
  (SETQ TYPEIN-POINTER (TYPEIN-POINTER))
  (COND
    ;; Efficiency hack for clearing to the end of line, as in C-K and CLEAR.
    ;; Don't need to add up character widths as needed in :DELETE-STRING.
    ((= END FILL-POINTER)
     (FUNCALL STREAM ':CLEAR-EOL))
    ;; Console can delete characters.  Pass in the string to make variable width fonts win.
    ((MEMQ ':DELETE-STRING ES-WHICH-OPERATIONS)
     (FUNCALL STREAM ':DELETE-STRING ES-BUFFER BEGIN END))
    ;; Console can't delete characters.  Be sure to do a clear-eol to flush those
    ;; characters at the very end.
    (T (FUNCALL STREAM ':CLEAR-EOL)
       (FUNCALL STREAM ':STRING-OUT ES-BUFFER END)
       (DE-CURSOR-MOTION STREAM END)))
  ;; Now actually delete the characters from the buffer.  Do this before decrementing
  ;; the fill pointer so that we don't attempt to reference outside the string.
  (COPY-VECTOR-SEGMENT (- FILL-POINTER END)
                       ES-BUFFER (+ TYPEIN-POINTER WIDTH)
                       ES-BUFFER TYPEIN-POINTER)
  (SETF (FILL-POINTER) (- FILL-POINTER WIDTH))
  ;; If all typein has been deleted and the :FULL-RUBOUT option is
  ;; active, then throw now.  This will throw if the user types rubout
  ;; immediately after entering the read function.
  (IF (AND (ZEROP (FILL-POINTER))
           (ASSQ ':FULL-RUBOUT TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS))
      (*THROW 'RUBOUT-HANDLER T))
  ;; If it turns out that nothing was deleted, then don't bother rescanning input.
  ({{#/}} WIDTH 0))

(DEF-DE-COMMAND (DE-DELETE-CHAR #{{#/}}/D) (STREAM &OPTIONAL (N 1))
  (DE-DELETE-STRING STREAM (TYPEIN-POINTER) (+ (TYPEIN-POINTER) N)))

(DEF-DE-COMMAND (DE-RUBOUT-CHAR #\RUBOUT) (STREAM &OPTIONAL (N 1))
  (DE-DELETE-STRING STREAM (- (TYPEIN-POINTER) N) (TYPEIN-POINTER)))

;; CLEAR flushes all buffered input.  If the full rubout option is in
;; use, then we will throw out of here.  No need to prompt since the prompt still there.
(DEF-DE-COMMAND (DE-CLEAR #\CLEAR) (STREAM &OPTIONAL IGNORE)
  (DE-DELETE-STRING STREAM 0 (FILL-POINTER)))

;; If at the end of the line, change this to kill only the newline.


(DEF-DE-COMMAND (DE-CLEAR-EOL #{{#/}}/K) (STREAM &OPTIONAL IGNORE)
  (DE-DELETE-STRING STREAM (TYPEIN-POINTER) (DE-SEARCH-FORWARD-NEWLINE (TYPEIN-POINTER))))


;; Word Commands

(DEFUN DE-ALPHABETIC? (CHAR)
  (SETQ CHAR (CHAR-UPCASE CHAR))
  (AND ({{#/}} CHAR #/A) ({{#/}} CHAR #/Z)))

;; Returns the position of the first (non) alphabetic character
;; in the buffer.  If no alphabetic characters between current
;; typein position and end of line, return nil.

(DEFUN DE-SEARCH-FORWARD-ALPHABETIC (POS)
  (DO ((FILL-POINTER (FILL-POINTER))
       (I POS (1+ I)))
      ((= I FILL-POINTER) NIL)
    (IF (DE-ALPHABETIC? (AREF ES-BUFFER I)) (RETURN I))))

(DEFUN DE-SEARCH-FORWARD-NON-ALPHABETIC (POS)
  (DO ((FILL-POINTER (FILL-POINTER))
       (I POS (1+ I)))
      ((= I FILL-POINTER) NIL)
    (IF (NOT (DE-ALPHABETIC? (AREF ES-BUFFER I))) (RETURN I))))

(DEFUN DE-SEARCH-BACKWARD-ALPHABETIC (POS)
  (DO ((I POS (1- I)))
      ((= I -1) NIL)
    (IF (DE-ALPHABETIC? (AREF ES-BUFFER I)) (RETURN I))))

(DEFUN DE-SEARCH-BACKWARD-NON-ALPHABETIC (POS)
  (DO ((I POS (1- I)))
      ((= I -1) NIL)
    (IF (NOT (DE-ALPHABETIC? (AREF ES-BUFFER I))) (RETURN I))))

;; Search for a point N words away and return that point.
;; If on an alphabetic character, skip to the first non alphabetic one.
;; If on a non-alphabetic, skip over non-alphabetics and then over alphabetics,
;; If no alphabetics follow the non-alphabetics, then don't move at all.

(DEFUN DE-SEARCH-FORWARD-WORD (N &AUX (POS (TYPEIN-POINTER)) SEARCH-POS)
  (DO ((I 0 (1+ I)))
      ((= I N) POS)
    (COND ((DE-ALPHABETIC? (AREF ES-BUFFER POS))
           (SETQ POS (DE-SEARCH-FORWARD-NON-ALPHABETIC POS)))
          (T (SETQ SEARCH-POS (DE-SEARCH-FORWARD-ALPHABETIC POS))
             (IF (NOT SEARCH-POS) (RETURN POS))
             (SETQ POS (DE-SEARCH-FORWARD-NON-ALPHABETIC SEARCH-POS))))
    ;;If within a word and can't find whitespace, leave at right end.
    (IF (NOT POS) (RETURN (FILL-POINTER)))))

;; Search for a point N words back and return that point.
;; If on an alphabetic character, skip to the character just following the
;; first non-alphabetic one.  If on a non-alphabetic, skip over non-alphabetics
;; and then over alphabetics.  If no alphabetics after non-alphabetics, then
;; don't move at all.  Treat cursor on first character of a word as a special case.

(DEFUN DE-SEARCH-BACKWARD-WORD (N &AUX (POS (TYPEIN-POINTER)) SEARCH-POS)
  (DO ((I 0 (1+ I)))
      ((= I N) POS)
    (COND
      ;;At beginning of line -- punt
      ((= POS 0) (RETURN 0))
      ;;Inside a word but not at the beginning of a word.
      ((AND (DE-ALPHABETIC? (AREF ES-BUFFER POS))
            (DE-ALPHABETIC? (AREF ES-BUFFER (1- POS))))
       (SETQ POS (DE-SEARCH-BACKWARD-NON-ALPHABETIC POS)))
      ;;Within whitespace or at beginning of a word.
      (T (SETQ SEARCH-POS (IF (DE-ALPHABETIC? (AREF ES-BUFFER POS))
                       (1- POS) POS))
         (SETQ SEARCH-POS (DE-SEARCH-BACKWARD-ALPHABETIC SEARCH-POS))
         (IF (NOT SEARCH-POS) (RETURN POS))
         (SETQ POS (DE-SEARCH-BACKWARD-NON-ALPHABETIC SEARCH-POS))))
    ;;If within a word and can't find whitespace, leave at left end.
    (IF (NOT POS) (RETURN 0))
    ;;Leave cursor on first character of the word
    (INCREMENT POS)
    ))

(DEF-DE-COMMAND (DE-FORWARD-WORD #{{#/}}/F) (STREAM &OPTIONAL (N 1))
  (DE-SET-POSITION STREAM (DE-SEARCH-FORWARD-WORD N)))

(DEF-DE-COMMAND (DE-BACKWARD-WORD #{{#/}}/B) (STREAM &OPTIONAL (N 1))
  (DE-SET-POSITION STREAM (DE-SEARCH-BACKWARD-WORD N)))

(DEF-DE-COMMAND (DE-DELETE-WORD #{{#/}}/D) (STREAM &OPTIONAL (N 1))
  (DE-DELETE-STRING STREAM (TYPEIN-POINTER) (DE-SEARCH-FORWARD-WORD N)))

(DEF-DE-COMMAND (DE-RUBOUT-WORD #{{#/}}\RUBOUT) (STREAM &OPTIONAL (N 1))
  (DE-DELETE-STRING STREAM (DE-SEARCH-BACKWARD-WORD N) (TYPEIN-POINTER)))

(DEF-DE-COMMAND (DE-TWIDDLE-CHARS #{{#/}}/T) (STREAM &OPTIONAL IGNORE &AUX DELETE-POINTER STRING)
  (SETQ DELETE-POINTER (TYPEIN-POINTER))
  ;; At end of line, go back two chars; in middle of line, one; at beginning, none.
  (DECREMENT DELETE-POINTER (COND ((= DELETE-POINTER 0) 0)
                                  ((= DELETE-POINTER (FILL-POINTER)) 2)
                                  (T 1)))
  (SETQ STRING (SUBSTRING ES-BUFFER DELETE-POINTER (+ DELETE-POINTER 2)))
  (DE-DELETE-STRING STREAM DELETE-POINTER (+ DELETE-POINTER 2))

  (SETQ STRING (STRING-NREVERSE STRING))
  (DE-INSERT-STRING STREAM STRING 0 2 T))


;; Kill Ring Commands

;; This doesn't really yank things killed, only previous complete lines typed
;; or the current line.  C-0 C-Y yanks the current line, C-1 C-Y yanks the previous
;; thing typed.  Everything but the last character is brought back, since that
;; is generally the character which triggers the return of the read function.
;; E.g. ) in reading a list, space in reading a symbol, cr in readline, control-c
;; in qsend.  This way, we will be asked for more input rather than having
;; the read function return for us.  Alternately, we could force the editor
;; to retain control and yank back the complete line.
;; We pass in T for RUBBED-OUT-SOME since it isn't currently given
;; to us.  So, we always rescan when yanking at the end of the line.

(DEF-DE-COMMAND (DE-YANK #{{#/}}/Y) (STREAM &OPTIONAL (N 1)
                                       &AUX (YANKED (NTH N ES-KILL-RING)))
        (DE-INSERT-STRING STREAM YANKED 0 (MAX 0 (1- (STRING-LENGTH YANKED))) T))

;; Move the thing at the top of the kill ring to the bottom of the kill ring
(DEFUN DE-POP-KILL-RING ()
  (SETF (FIRST ES-KILL-RING) (SECOND ES-KILL-RING))
  (SETF (SECOND ES-KILL-RING) ES-BUFFER)
  (POP ES-KILL-RING))

;; If previous command was a Yank or a Yank-Pop, then remove what was placed there
;; by the command, yank in the top thing on the ring, and cycle the ring forward.
;; Otherwise, treat the command like an ordinary yank, except pop the ring afterward.

(DEF-DE-COMMAND (DE-YANK-POP #{{#/}}/Y) (STREAM &OPTIONAL IGNORE
                                           &AUX YANKED YANKED-LENGTH
                                           (TYPEIN-POINTER (TYPEIN-POINTER)))
  (PROG ()
   ;; First see if it was a Yank-Pop.
   (SETQ YANKED (CAR (CIRCULAR-LIST-LAST ES-KILL-RING)))
   (SETQ YANKED-LENGTH (1- (STRING-LENGTH YANKED)))
   (IF (AND ({{#/}} TYPEIN-POINTER YANKED-LENGTH)
            (STRING-EQUAL ES-BUFFER YANKED
                          (- TYPEIN-POINTER YANKED-LENGTH) 0
                          TYPEIN-POINTER YANKED-LENGTH))
       (RETURN))
   ;; Then check for an ordinary yank.  If it matches, then pop it off
   ;; the ring so that we'll get the next thing.
   (SETQ YANKED (SECOND ES-KILL-RING))
   (SETQ YANKED-LENGTH (1- (STRING-LENGTH YANKED)))
   (COND ((AND ({{#/}} TYPEIN-POINTER YANKED-LENGTH)
               (STRING-EQUAL ES-BUFFER YANKED
                             (- TYPEIN-POINTER YANKED-LENGTH) 0
                             TYPEIN-POINTER YANKED-LENGTH))
          (DE-POP-KILL-RING)
          (RETURN)))
   ;; No match.
   (SETQ YANKED NIL))
  ;; The previous command was a yank of some type.  Delete the yanked screen from
  ;; the screen and the buffer.
  (IF YANKED
      (DE-DELETE-STRING STREAM (- TYPEIN-POINTER YANKED-LENGTH) TYPEIN-POINTER))
  ;; Yank the thing at the top of the kill ring, and pop the kill ring
  (DE-YANK STREAM 1)
  (DE-POP-KILL-RING))

#+DEBUG
(DEF-DE-COMMAND (DE-DEBUG #\HELP) (STREAM &OPTIONAL IGNORE)
  (FORMAT STREAM "~%[Fill pointer = ~D  Typein pointer = ~D]~%"
          (FILL-POINTER) (TYPEIN-POINTER))
  (DE-REPRINT-INPUT STREAM #\HELP))


;; Now a printing (heh, heh) terminal editor.  For poor souls coming via supdup
;; and for purposes of comparison.

(DEFUN PRINTING-EDITOR
       (STREAM &AUX CH PROMPT
               (OPTIONS TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS)
               (RUBBED-OUT-SOME NIL)
               (DOING-RUBOUT NIL)
               (PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH OPTIONS))))
  (*CATCH
    'RETURN-CHARACTER
    (DO () (NIL)
      (SETQ CH (FUNCALL ES-CONSOLE-STREAM ':TYI))
      (COND ((MEMQ CH PASS-THROUGH)
             (PRINTING-EDITOR-INSERT-CHAR STREAM CH RUBBED-OUT-SOME DOING-RUBOUT))
            ;; Control-D and Control-U kill the current line, ITS and Twenex style.
            ;; If nothing in the buffer, don't do anything.
            ((MEMQ CH '(#{{#/}}/d #{{#/}}/D #{{#/}}/u #{{#/}}/U))
             (COND ((NOT (ZEROP (FILL-POINTER)))
                    (SETF (FILL-POINTER) 0)
                    (SETQ RUBBED-OUT-SOME T)
                    (COND (DOING-RUBOUT (FUNCALL STREAM ':TYO #/])
                                        (SETQ DOING-RUBOUT NIL)))
                    (FUNCALL STREAM ':LINE-OUT " XXX")
                    (SETQ PROMPT (OR (ASSQ ':REPROMPT OPTIONS) (ASSQ ':PROMPT OPTIONS)))
                    (IF PROMPT (FUNCALL PROMPT STREAM CH))
                    (IF (ASSQ ':FULL-RUBOUT OPTIONS) (*THROW 'RUBOUT-HANDLER T)))))
            ;; Control-K or Control-L echo themselves and then reprint the current
            ;; line, prompting if necessary.
            ((MEMQ CH '(#\VT #\FORM #{{#/}}/k #{{#/}}/K #{{#/}}/l #{{#/}}/L))
             (FUNCALL STREAM ':TYO CH)
             (FUNCALL STREAM ':FRESH-LINE)
             (SETQ PROMPT (OR (ASSQ ':REPROMPT OPTIONS) (ASSQ ':PROMPT OPTIONS)))
             (IF PROMPT (FUNCALL PROMPT STREAM CH))
             (FUNCALL STREAM ':STRING-OUT ES-BUFFER))
            ;; Echo characters backwards Unix style.
            ((= CH #\RUBOUT)
             (MULTIPLE-VALUE (RUBBED-OUT-SOME DOING-RUBOUT)
               (PRINTING-EDITOR-RUBOUT-CHAR STREAM OPTIONS RUBBED-OUT-SOME DOING-RUBOUT)))
            ;; Control-W flushes word.
            ((MEMQ CH '(#{{#/}}/w #{{#/}}/W))
             (COND ((NOT (ZEROP (FILL-POINTER)))
                    ;; First flush whitespace.
                    (DO ()
                        ((OR (ZEROP (FILL-POINTER))
                             (DE-ALPHABETIC? (AREF ES-BUFFER (1- (FILL-POINTER))))))
                      (MULTIPLE-VALUE (RUBBED-OUT-SOME DOING-RUBOUT)
                        (PRINTING-EDITOR-RUBOUT-CHAR STREAM OPTIONS
                                                     RUBBED-OUT-SOME DOING-RUBOUT)))
                    ;; Then flush alphabetics.
                    (DO ()
                        ((OR (ZEROP (FILL-POINTER))
                             (NOT (DE-ALPHABETIC? (AREF ES-BUFFER (1- (FILL-POINTER)))))))
                      (MULTIPLE-VALUE (RUBBED-OUT-SOME DOING-RUBOUT)
                        (PRINTING-EDITOR-RUBOUT-CHAR STREAM OPTIONS
                                                     RUBBED-OUT-SOME DOING-RUBOUT))))
                   (DOING-RUBOUT
                     (FUNCALL STREAM ':LINE-OUT "]")
                     (SETQ DOING-RUBOUT NIL))))
            ;; Flush random control characters.
            ((NOT (ZEROP (LDB %%KBD-CONTROL-META CH))) (FUNCALL STREAM ':BEEP))
            (T (PRINTING-EDITOR-INSERT-CHAR STREAM CH RUBBED-OUT-SOME DOING-RUBOUT))))))

(DEFUN PRINTING-EDITOR-RUBOUT-CHAR (STREAM OPTIONS RUBBED-OUT-SOME DOING-RUBOUT &AUX CH)
  (COND ((NOT (ZEROP (FILL-POINTER)))
         (SETQ RUBBED-OUT-SOME T)
         (SETQ CH (ARRAY-POP ES-BUFFER))
         ;; If we're already rubbing out, then echo character.
         ;; If we're not rubbing out and the character is a space, then backspace.
         ;; If this is our first rubout, print "[" and echo.
         (COND (DOING-RUBOUT (FUNCALL STREAM ':TYO CH))
               ((= CH #\SPACE) (FUNCALL STREAM ':TYO #\BS))
               (T (SETQ DOING-RUBOUT T)
                  (FUNCALL STREAM ':TYO #/[)
                  (FUNCALL STREAM ':TYO CH)))
         ;; Rubbed out everything, and :FULL-RUBOUT option active, so throw.
         (IF (AND (ZEROP (FILL-POINTER)) (ASSQ ':FULL-RUBOUT OPTIONS))
             (*THROW 'RUBOUT-HANDLER T))
         (MVRETURN RUBBED-OUT-SOME DOING-RUBOUT))
        ;; Nothing left in the input buffer.   If we were rubbing out, close the
        ;; rubout and go to the next line.
        (DOING-RUBOUT
          (FUNCALL STREAM ':LINE-OUT "]")
          (MVRETURN RUBBED-OUT-SOME NIL))))

;; This is only called when typing at the end of the line.  There is no
;; such thing as typing in the middle of the line here.

(DEFUN PRINTING-EDITOR-INSERT-CHAR (STREAM CH RUBBED-OUT-SOME DOING-RUBOUT)
  (IF DOING-RUBOUT (FUNCALL STREAM ':TYO #/]))
  (FUNCALL STREAM ':TYO CH)
  (ARRAY-PUSH-EXTEND ES-BUFFER CH)
  (INCREMENT (SCAN-POINTER))
  (IF RUBBED-OUT-SOME
      (*THROW 'RUBOUT-HANDLER T)
      (*THROW 'RETURN-CHARACTER CH)))


#+DEBUG
(PROGN 'COMPILE

;; These functions for debugging the editor stream.
;; Random "read" functions which test various aspects of the stream.

(DEFUN READ-10-CHARS (&OPTIONAL (STREAM STANDARD-INPUT) EOF-OPTION &AUX RESULT)
  (IF (AND (NOT RUBOUT-HANDLER)
           (MEMQ ':RUBOUT-HANDLER (FUNCALL STREAM ':WHICH-OPERATIONS)))
      ;; If the :FULL-RUBOUT option is activated, this will return NIL.
      ;; Keep looping until a string is returned.
      (DO () (NIL)
        (SETQ RESULT (FUNCALL STREAM ':RUBOUT-HANDLER
                              '((:FULL-RUBOUT T)
                                (:PROMPT ES-DEBUG-PROMPT)
                                (:REPROMPT ES-DEBUG-REPROMPT))
                              #'READ-10-CHARS STREAM EOF-OPTION))
        (IF RESULT (RETURN RESULT))
        (ES-DEBUG-FULL-RUBOUT STREAM))
      (LET ((CHARS (MAKE-ARRAY NIL ART-STRING 10.)))
        (DOTIMES (I 10.)
          (ASET (FUNCALL STREAM ':TYI EOF-OPTION) CHARS I))
        CHARS)))

;; A study in compensating for missing capabilities -- :INSERT-CHAR
;; [1] Let the receiving end (console stream) worry about it, press file style.
;;     It would have to maintain a complete screen image.
;; [2] The editor stream can handle some cases.  This would only work during
;;     interactive typein.
;; [3] Let the toplevel program handle it.  It most likely has the screen image
;;     buffered in another form.  But now, everybody has to worry about the missing
;;     capability, although some programs will have better ideas about how to
;;     compensate for them.  I think having both [1] and [3] is the right thing.

(DEFUN READ-10-CHARS-FLASH (&OPTIONAL (STREAM TERMINAL-IO)
                                      &AUX (W-O (FUNCALL STREAM ':WHICH-OPERATIONS))
                                           CHARS X Y)
  (SETQ CHARS (READ-10-CHARS STREAM))
  (COND ((MEMQ ':READ-CURSORPOS W-O)
         ;; Kludge.  Hopefully unnecessary in nws.
         (SETQ X (SYMEVAL-IN-CLOSURE STREAM 'ES-X-ORIGIN))
         (SETQ Y (SYMEVAL-IN-CLOSURE STREAM 'ES-Y-ORIGIN))
         (FUNCALL STREAM ':SET-CURSORPOS X Y)
         (COND ((MEMQ ':INSERT-CHAR W-O)
                (FUNCALL STREAM ':INSERT-CHAR #/[)
                (FUNCALL STREAM ':CURSOR-MOTION NIL NIL CHARS))
               (T (FUNCALL STREAM ':TYO #/[)
                  (FUNCALL STREAM ':STRING-OUT CHARS)))
         (FUNCALL STREAM ':TYO #/])))
  (IF (MEMQ ':BEEP W-O) (FUNCALL STREAM ':BEEP))
  (RETURN-ARRAY CHARS))

;; Uses the rubout handler to read characters until a Control-C is typed, but
;; does no consing at all.  Useful for testing if a rubout handler conses
;; and also tests the :PASS-THROUGH option.

(DEFUN CHARACTER-SINK (&OPTIONAL (STOP-CHARS '(#{{#/}}/c #{{#/}}/C)) (STREAM STANDARD-INPUT))
  (COND ((AND (NOT RUBOUT-HANDLER)
              (MEMQ ':RUBOUT-HANDLER (FUNCALL STREAM ':WHICH-OPERATIONS)))
         (FUNCALL STREAM ':RUBOUT-HANDLER `((:PASS-THROUGH . ,STOP-CHARS)
                                            (:PROMPT ES-DEBUG-PROMPT)
                                            (:REPROMPT ES-DEBUG-REPROMPT))
                  #'CHARACTER-SINK STOP-CHARS STREAM))
        ;; CHAR-EQUAL throws away bucky bits.
        (T (DO () ((MEMQ (FUNCALL STREAM ':TYI) STOP-CHARS)))
           (DOTIMES (I 3) (FUNCALL STREAM ':BEEP)))))

(DEFUN ES-DEBUG-PROMPT (STREAM IGNORE)
  (FORMAT STREAM "~&~11A" "Prompt:"))

(DEFUN ES-DEBUG-REPROMPT (STREAM IGNORE)
  (FORMAT STREAM "~11A" "Reprompt:"))

(DEFUN ES-DEBUG-FULL-RUBOUT (STREAM)
  (FORMAT STREAM "[Full Rubout]~%"))


;; Various testing functions which affect a local lisp listener only.
;; Test display editor, printing editor, multiple font stuff.

(DEFUN ES-DEBUG-DISPLAY (&AUX WINDOW STREAM)
  (SETQ WINDOW (<- SELECTED-WINDOW ':PANE))
  (SETQ TERMINAL-IO
        (MAKE-EDITOR-STREAM (MAKE-TV-STREAM (<- WINDOW ':PC-PPR)) #'DISPLAY-EDITOR))
  (<- WINDOW ':STREAM<- TERMINAL-IO))

(DEFUN ES-DEBUG-PRINTING (&AUX WINDOW)
  (SETQ WINDOW (<- SELECTED-WINDOW ':PANE))
  (SETQ TERMINAL-IO
        (MAKE-EDITOR-STREAM (MAKE-TV-STREAM (<- WINDOW ':PC-PPR)) #'PRINTING-EDITOR))
  ;; Make the stream look like a printing console.
  (SET-IN-CLOSURE TERMINAL-IO 'ES-WHICH-OPERATIONS
                  '(:TYI :TYI-NO-HANG :LISTEN :TYO :STRING-OUT :LINE-OUT :FRESH-LINE :BEEP
                         :UNTYI :RUBOUT-HANDLER :CLEAR-INPUT :LISTEN))
  (<- WINDOW ':STREAM<- TERMINAL-IO))

(DEFUN ES-DEBUG-FONT (&OPTIONAL (FONT "TR10B"))
  (SETQ FONT (STRING-UPCASE FONT))
  ;; Can't use LOAD-IF-NEEDED since CPTFONT comes from CPTFON >.
  (IF (NOT (SECOND (MULTIPLE-VALUE-LIST (INTERN FONT "Fonts"))))
      (LOAD (FORMAT NIL "AI:LMFONT;~A" FONT)))
  (SETQ FONT (SYMEVAL (INTERN FONT "Fonts")))
  (TV-REDEFINE-PC-PPR (FUNCALL TERMINAL-IO ':PC-PPR) ':FONTS (LIST FONT)))

(DEFUN ES-DEBUG-OFF (&AUX WINDOW)
  (SETQ WINDOW (<- SELECTED-WINDOW ':PANE))
  (SETQ TERMINAL-IO (TV-MAKE-STREAM (<- WINDOW ':PC-PPR)))
  (<- WINDOW ':STREAM<- TERMINAL-IO))

) ;; End of Debug conditionalization

(DEFUN DE-TV-MAKE-STREAM (PC-PPR)
  (MAKE-EDITOR-STREAM (MAKE-TV-STREAM PC-PPR) #'DISPLAY-EDITOR))

;; This function is for turning the thing on globally.  It clobbers
;; TV-MAKE-STREAM and TOP-WINDOW and should only be called from inside
;; the initial lisp listener.

(DEFUN DE-GLOBAL-ENABLE ()
  (FSET 'TV-MAKE-STREAM  #'DE-TV-MAKE-STREAM)
  (SETQ TERMINAL-IO (DE-TV-MAKE-STREAM CONSOLE-IO-PC-PPR))
  (<- TOP-WINDOW ':STREAM<- TERMINAL-IO))

#-DEBUG (DE-GLOBAL-ENABLE)

;; To do:  Modify kill ring representation to not include current buffer.
;;         Change font set and default font.
;;         Make kill ring stuff work for printing editor.
;;         Make printing editor cross out characters with slashes.  Be careful
;;          about newlines.
;;         Write a Macsyma printing editor (regular editor plus ?? command)
{{#/}}{{#/}}
