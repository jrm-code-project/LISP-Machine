;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980, 1981 Massachusetts Institute of Technology **

(DEFMACRO COERCE-FONT (FONT-VARIABLE SHEET)
  `(UNLESS (TYPEP ,FONT-VARIABLE 'FONT)
     (SETQ ,FONT-VARIABLE (SEND (SHEET-GET-SCREEN ,SHEET) :PARSE-FONT-SPECIFIER
                                ,FONT-VARIABLE))))

;;; Miscellaneous user functions

(DEFUN SCREEN-REDISPLAY (&OPTIONAL (TYPE :COMPLETE-REDISPLAY) (SCREEN DEFAULT-SCREEN))
  "Redisplay the entire contents of SCREEN"
  (SEND SCREEN :REFRESH TYPE)
  (WHO-LINE-CLOBBERED))

(DEFMETHOD (SCREEN :BEEP) (&OPTIONAL BEEP-TYPE)
  "Beep the beeper."
  (AND BEEP
       (WITHOUT-INTERRUPTS  ;otherwise might quit out and leave screen complemented
         (OR (EQ BEEP ':BEEP) (COMPLEMENT-BOW-MODE SELF))
         (IF (EQ BEEP ':FLASH)
             (%BEEP 0 BEEP-DURATION)    ;Delay same time without making any noise
           (BEEP BEEP-TYPE 'IGNORE))
         (OR (EQ BEEP ':BEEP) (COMPLEMENT-BOW-MODE SELF)))))

(DEFMETHOD (SHEET :BEEP) (&OPTIONAL BEEP-TYPE)
  (AND SUPERIOR (SEND SUPERIOR :BEEP BEEP-TYPE)))

(DEFUN BEEP (&OPTIONAL BEEP-TYPE (STREAM *TERMINAL-IO*))
  "Ring the bell and flash the screen.
Works via the :BEEP operation on STREAM if STREAM supports it.
The value of BEEP controls what this function does:
 T means flash the screen and make noise,
 :BEEP means just make noise, :FLASH means just flash.
 NIL means do nothing.
BEEP-TYPE says why the beep is being done.  Standard values include:
 ZWEI::CONVERSE-PROBLEM -- Converse was unable to send a message.
 ZWEI::CONVERSE-MESSAGE-RECEIVED -- A Converse message has come in.
 ZWEI::NO-COMPLETION -- Completion in a minibuffer failed.
 TV:NOTIFY -- A notification cannot be printed on the selected window.
 SUPDUP::TERMINAL-BELL -- ``Bell'' received for terminal
 FQUERY -- When a question needs to be answered
 NIL -- anything else.
BEEP-TYPE does not have any effect, currently,
but you can redefine BEEP to to different things for different beep types."
  (WHEN BEEP
    (IF (OPERATION-HANDLED-P STREAM :BEEP)
        (SEND STREAM :BEEP BEEP-TYPE)
      (select-processor
        ((:lambda :cadr)
         (%BEEP BEEP-WAVELENGTH BEEP-DURATION))
        (:explorer
          (explorer-beep))))))


(DEFUN BLACK-ON-WHITE (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  "Set SCREEN to display one bits as black and zeros as white."
  (ecase si::video-board-type
    (:cadr
     (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
                  (LOGIOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))
    (:vcmem
     (%NUBUS-WRITE TV:TV-QUAD-SLOT 4
                   (LOGIOR 20 (%NUBUS-READ TV:TV-QUAD-SLOT 4))))
    (:quad
     (%p-store-tag-and-pointer (%pointer-plus si::quad-video-control-virtual-address
                                              (// #x48 4))
                               0 1))
    (:none) ;if no video board, don't crash (someday...)
    (:explorer
     (%nubus-write tv::tv-quad-slot #xe00080 2))))

(DEFUN WHITE-ON-BLACK (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  "Set SCREEN to display one bits as white and zeros as black."
  (ecase si::video-board-type
    (:cadr
     (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
                  (LOGAND -5 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN))))) ;1's comp of 4
    (:vcmem
     (%NUBUS-WRITE TV:TV-QUAD-SLOT 4
                   (LOGAND (LOGNOT 20) (%NUBUS-READ TV:TV-QUAD-SLOT 4))))
    (:quad
     (%p-store-tag-and-pointer (%pointer-plus si::quad-video-control-virtual-address
                                              (// #x48 4))
                               0 0))
    (:none)
    (:explorer
     (%nubus-write tv::tv-quad-slot #xe00080 0))))

(DEFUN COMPLEMENT-BOW-MODE (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  "Complement whether SCREEN displays one bits as white or as black."
  (ecase si::video-board-type
    (:cadr
     (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
                    (LOGXOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))
    (:vcmem
     (%NUBUS-WRITE TV:TV-QUAD-SLOT 4
                   (LOGXOR 20 (%NUBUS-READ TV:TV-QUAD-SLOT 4))))
    (:quad
     (let ((adr (%pointer-plus si::quad-video-control-virtual-address
                               (// #x48 4))))
       (%p-store-tag-and-pointer adr 0 (logxor 1 (%p-ldb (byte 1 0) adr)))))
    (:explorer
     (%nubus-write tv:tv-quad-slot #xe00080
                   (logxor 2 (logand 2 (%nubus-read tv:tv-quad-slot #xe00080)))))
    (:none)))

(defun get-bow-mode (&optional (screen default-screen))
  "If screen is White characters on a black background, return T."
  (= 0
     (ecase si::video-board-type
       (:cadr  (ldb (byte 1 2) (%xbus-read (screen-control-address screen))))
       (:vcmem (ldb (byte 1 4) (%nubus-read tv:tv-quad-slot 4)))
       (:quad  (let ((adr (%pointer-plus si::quad-video-control-virtual-address
                                         (// #x48 4))))
                 (%p-ldb (byte 1 0) adr)))
       (:explorer (ldb (byte 1 1) (%nubus-read tv:tv-quad-slot #xe00080)))
       (:none 1))))


(DEFMETHOD (SHEET :DRAW-RECTANGLE) (RECTANGLE-WIDTH RECTANGLE-HEIGHT X Y
                                    &OPTIONAL (ALU CHAR-ALUF))
  (PREPARE-SHEET (SELF)
    (DRAW-RECTANGLE-INSIDE-CLIPPED RECTANGLE-WIDTH RECTANGLE-HEIGHT X Y ALU SELF)))

(DEFMETHOD (SHEET :BITBLT) (ALU WID HEI FROM-ARRAY FROM-X FROM-Y TO-X TO-Y
                                   &AUX (IL (SHEET-INSIDE-LEFT))
                                        (IT (SHEET-INSIDE-TOP))
                                        (IW (SHEET-INSIDE-WIDTH))
                                        (IH (SHEET-INSIDE-HEIGHT)))
  (LET* ((ABS-WID (ABS WID))
         (ABS-HEI (ABS HEI))
         (LEFT-OVERRUN   (MAX 0 (- TO-X)))
         (RIGHT-OVERRUN  (MAX 0 (- (+ TO-X ABS-WID) IW)))
         (TOP-OVERRUN    (MAX 0 (- TO-Y)))
         (BOTTOM-OVERRUN (MAX 0 (- (+ TO-Y ABS-HEI) IH)))
         (CLIPPED-WID (* (IF (MINUSP WID) -1 1)
                         (MAX 0 (- ABS-WID LEFT-OVERRUN RIGHT-OVERRUN))))
         (CLIPPED-HEI (* (IF (MINUSP HEI) -1 1)
                         (MAX 0 (- ABS-HEI TOP-OVERRUN BOTTOM-OVERRUN)))))
    (AND (NOT (ZEROP CLIPPED-WID))                              ;bitblt errs when w=h=0
         (NOT (ZEROP CLIPPED-HEI))                              ;and dims are out of bounds
         (PREPARE-SHEET (SELF)
           (BITBLT ALU
                   CLIPPED-WID CLIPPED-HEI
                   FROM-ARRAY
                   (\ (+ FROM-X LEFT-OVERRUN) (PIXEL-ARRAY-WIDTH FROM-ARRAY))  ;***
                   (\ (+ FROM-Y TOP-OVERRUN) (PIXEL-ARRAY-HEIGHT FROM-ARRAY))  ;***
                   SCREEN-ARRAY
                   (+ IL (MIN (MAX 0 TO-X) IW)) (+ IT (MIN (MAX 0 TO-Y) IH)))))))

(DEFMETHOD (SHEET :BITBLT-FROM-SHEET) (ALU WID HEI FROM-X FROM-Y TO-ARRAY TO-X TO-Y
                                              &AUX (IL (SHEET-INSIDE-LEFT))
                                                   (IR (SHEET-INSIDE-RIGHT))
                                                   (IT (SHEET-INSIDE-TOP))
                                                   (IB (SHEET-INSIDE-BOTTOM)))
  (SETQ FROM-X (+ IL FROM-X) FROM-Y (+ IT FROM-Y))
  (LET* ((CLIPPED-FROM-X
           (MIN (MAX IL FROM-X) IR))
         (CLIPPED-FROM-Y
           (MIN (MAX IT FROM-Y) IB))
         (WID-SIGN (IF (MINUSP WID) -1 1))
         (HEI-SIGN (IF (MINUSP HEI) -1 1))
         (LEFT-OVERRUN
           (- CLIPPED-FROM-X FROM-X))
         (RIGHT-OVERRUN
           (MAX 0 (- (+ CLIPPED-FROM-X (ABS WID)) IR)))
         (TOP-OVERRUN
           (- CLIPPED-FROM-Y FROM-Y))
         (BOTTOM-OVERRUN
           (MAX 0 (- (+ CLIPPED-FROM-Y (ABS HEI)) IB)))
         (CLIPPED-WID
           (* WID-SIGN (MAX 0 (- (ABS WID) LEFT-OVERRUN RIGHT-OVERRUN))))
         (CLIPPED-HEI
           (* HEI-SIGN (MAX 0 (- (ABS HEI) TOP-OVERRUN BOTTOM-OVERRUN)))))

    (AND (NOT (ZEROP CLIPPED-WID))              ;bitblt has this weird bug where it
         (NOT (ZEROP CLIPPED-HEI))              ;doesn't check to see if wid and hei are = 0
         (PREPARE-SHEET (SELF)
           (BITBLT ALU
                   CLIPPED-WID CLIPPED-HEI
                   SCREEN-ARRAY CLIPPED-FROM-X CLIPPED-FROM-Y
                   TO-ARRAY (+ TO-X LEFT-OVERRUN) (+ TO-Y TOP-OVERRUN))))))

(DEFMETHOD (SHEET :BITBLT-WITHIN-SHEET) (ALU WID HEI FROM-X FROM-Y TO-X TO-Y
                                                &AUX (IL (SHEET-INSIDE-LEFT))
                                                     (IR (SHEET-INSIDE-RIGHT))
                                                     (IT (SHEET-INSIDE-TOP))
                                                     (IB (SHEET-INSIDE-BOTTOM)))
  (SETQ FROM-X (+ IL FROM-X) FROM-Y (+ IT FROM-Y)
        TO-X (+ IL TO-X) TO-Y (+ IT TO-Y))
  (LET* ((CLIPPED-FROM-X
           (MIN (MAX IL FROM-X) IR))
         (CLIPPED-FROM-Y
           (MIN (MAX IT FROM-Y) IB))
         (CLIPPED-TO-X
           (MIN (MAX IL TO-X) IR))
         (CLIPPED-TO-Y
           (MIN (MAX IT TO-Y) IB))
         (WID-SIGN (IF (MINUSP WID) -1 1))
         (HEI-SIGN (IF (MINUSP HEI) -1 1))
         (LEFT-OVERRUN
           (MAX 0 (- CLIPPED-FROM-X FROM-X) (- CLIPPED-TO-X TO-X)))
         (RIGHT-OVERRUN
           (MAX 0 (- (+ CLIPPED-FROM-X (ABS WID)) IR) (- (+ CLIPPED-TO-X (ABS WID)) IR)))
         (TOP-OVERRUN
           (MAX 0 (- CLIPPED-FROM-Y FROM-Y) (- CLIPPED-TO-Y TO-Y)))
         (BOTTOM-OVERRUN
           (MAX 0 (- (+ CLIPPED-FROM-Y (ABS HEI)) IB) (- (+ CLIPPED-TO-Y (ABS HEI)) IB)))
         (CLIPPED-WID
           (* WID-SIGN (MAX 0 (- (ABS WID) LEFT-OVERRUN RIGHT-OVERRUN))))
         (CLIPPED-HEI
           (* HEI-SIGN (MAX 0 (- (ABS HEI) TOP-OVERRUN BOTTOM-OVERRUN)))))

    (AND (NOT (ZEROP CLIPPED-WID))
         (NOT (ZEROP CLIPPED-HEI))
         (PREPARE-SHEET (SELF)
           (BITBLT ALU
                   CLIPPED-WID CLIPPED-HEI
                   SCREEN-ARRAY (+ FROM-X LEFT-OVERRUN) (+ FROM-Y TOP-OVERRUN)
                   SCREEN-ARRAY (+ TO-X LEFT-OVERRUN) (+ TO-Y TOP-OVERRUN))))))

(DEFMETHOD (SHEET :DRAW-CHAR) (FONT CHAR X-BITPOS Y-BITPOS &OPTIONAL (ALU CHAR-ALUF))
  (PREPARE-SHEET (SELF)
    (DRAW-CHAR FONT CHAR
               (+ X-BITPOS LEFT-MARGIN-SIZE)
               (+ Y-BITPOS TOP-MARGIN-SIZE)
               ALU SELF)))

(DEFMETHOD (SHEET :INCREMENT-BITPOS) (DX DY)
  (SHEET-INCREMENT-BITPOS SELF DX DY))

(DEFUN SHEET-INCREMENT-BITPOS (SHEET DX DY &AUX X Y MORE-VPOS)
  "Increment cursor X and cursor Y, keeping within sheet.
Sets exception flags according to new positions"
  (SETF (SHEET-CURSOR-X SHEET)
        (SETQ X (MAX (+ DX (SHEET-CURSOR-X SHEET)) (SHEET-INSIDE-LEFT SHEET))))
  (SETF (SHEET-CURSOR-Y SHEET)
        (SETQ Y (MAX (+ DY (SHEET-CURSOR-Y SHEET)) (SHEET-INSIDE-TOP SHEET))))
  (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
       (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
  (AND (SETQ MORE-VPOS (SHEET-MORE-VPOS SHEET))
       ( Y MORE-VPOS)
       (SETF (SHEET-MORE-FLAG SHEET) 1))
  NIL)

(DEFUN SHEET-SET-FONT (SHEET FONT)
  "Set the current font of SHEET to FONT.
The current font is what ordinary output is printed in.
FONT may be a font object, a name of one, a name of a name, etc."
  (SEND SHEET :SET-CURRENT-FONT FONT T))

(DEFMETHOD (SHEET :SIZE-IN-CHARACTERS) ()
  (VALUES (TRUNCATE (SHEET-INSIDE-WIDTH) CHAR-WIDTH) (SHEET-NUMBER-OF-INSIDE-LINES)))

(DEFMETHOD (SHEET :SET-SIZE-IN-CHARACTERS) (WIDTH-IN-CHARS HEIGHT-IN-CHARS
                                                           &OPTIONAL OPTION)
   (SEND SELF :SET-SIZE
              (DECODE-CHARACTER-WIDTH-SPEC WIDTH-IN-CHARS)
              (DECODE-CHARACTER-HEIGHT-SPEC HEIGHT-IN-CHARS)
              OPTION))

(DEFMETHOD (SHEET :SET-CURSORPOS) (X Y &OPTIONAL (UNIT :PIXEL))
  (CASE UNIT
    (:PIXEL)
    (:CHARACTER
      (AND X (SETQ X (* X CHAR-WIDTH)))
      (AND Y (SETQ Y (* Y LINE-HEIGHT))))
    (OTHERWISE
      (FERROR "~S is not a known unit." UNIT)))
  (SHEET-SET-CURSORPOS SELF X Y))

(DEFUN SHEET-SET-CURSORPOS (SHEET X Y)
  "Set 'cursor' position of SHEET in terms of raster units.
The cursor is where ordinary output will appear
/(the top left corner of the next character).
Cursorposes are relative to the left and top margins.
The arguments are `clipped' to stay inside the sheet's margins."
  (DO ((INHIBIT-SCHEDULING-FLAG T T)  ;Keep trying until we get the lock
       (LOCK) (BL))
      ((AND (SETQ LOCK (SHEET-CAN-GET-LOCK SHEET))
            (NOT (SHEET-OUTPUT-HELD-P SHEET)))
       (SETQ X (IF X (MIN (+ (MAX (FIX X) 0) (SHEET-INSIDE-LEFT SHEET))
                          (SHEET-INSIDE-RIGHT SHEET))
                   (SHEET-CURSOR-X SHEET)))
       (SETQ Y (IF Y (MIN (+ (MAX (FIX Y) 0) (SHEET-INSIDE-TOP SHEET))
                          (SHEET-INSIDE-BOTTOM SHEET))
                   (SHEET-CURSOR-Y SHEET)))
       (AND (= (SHEET-CURSOR-X SHEET) X) (= (SHEET-CURSOR-Y SHEET) Y)
            (RETURN NIL))                       ;Not moving, don't open the blinker
       (AND (SETQ BL (SHEET-FOLLOWING-BLINKER SHEET))
            (OPEN-BLINKER BL))
       (AND (SHEET-MORE-VPOS SHEET)             ;If more processing enabled, delay until
                                                ; bottom of sheet
            (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
       (SETF (SHEET-CURSOR-X SHEET) X)
       (SETF (SHEET-CURSOR-Y SHEET) Y)
       (SETF (SHEET-EXCEPTIONS SHEET) 0)
       (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
            (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
       T)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (IF LOCK
        (SEND SHEET :OUTPUT-HOLD-EXCEPTION)
      (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET))))

(DEFMETHOD (SHEET :INCREMENT-CURSORPOS) (DX DY &OPTIONAL (UNIT :PIXEL))
  (CASE UNIT
    (:PIXEL)
    (:CHARACTER
     (AND DX (SETQ DX (- (* CHAR-WIDTH DX)
                         (NTH-VALUE 1 (CEILING (- CURSOR-X LEFT-MARGIN-SIZE) CHAR-WIDTH)))))
     (AND DY (SETQ DY (- (* LINE-HEIGHT DY)
                         (NTH-VALUE 1 (CEILING (- CURSOR-Y TOP-MARGIN-SIZE) LINE-HEIGHT))))))
    (OTHERWISE
      (FERROR "~S is not a known unit." UNIT)))
  (PREPARE-SHEET (SELF)
    (OR (ZEROP (SHEET-EXCEPTIONS)) (SHEET-HANDLE-EXCEPTIONS SELF))
    (SHEET-INCREMENT-BITPOS SELF DX DY)))

(DEFMETHOD (SHEET :READ-CURSORPOS) (&OPTIONAL (UNIT :PIXEL))
  (CASE UNIT
    (:PIXEL
     (VALUES (- CURSOR-X LEFT-MARGIN-SIZE)
             (- CURSOR-Y TOP-MARGIN-SIZE)))
    (:CHARACTER
     (VALUES (CEILING (- CURSOR-X LEFT-MARGIN-SIZE) CHAR-WIDTH)
             (CEILING (- CURSOR-Y TOP-MARGIN-SIZE) LINE-HEIGHT)))
    (OTHERWISE
     (FERROR "~S is not a known unit." UNIT))))

(DEFUN SHEET-READ-CURSORPOS (SHEET)
  "Return the cursor position in raster units relative to margins"
  (DECLARE (VALUES CURSOR-X CURSOR-Y))
  (VALUES (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
          (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))

(DEFMETHOD (SHEET :HOME-CURSOR) ()
  (SHEET-HOME SELF))

(DEFUN SHEET-HOME (SHEET)
  "Position SHEET's cursor to upper left corner (inside the margins)."
  (PREPARE-SHEET (SHEET)
    (AND (SHEET-MORE-VPOS SHEET)                ;If MORE processing, put it off 'til last line
         (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SETF (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))
    (SETF (SHEET-EXCEPTIONS SHEET) 0)))

(DEFMETHOD (SHEET :HOME-DOWN) ()
  (SHEET-SET-CURSORPOS SELF 0
                       (* (TRUNCATE (- (SHEET-INSIDE-HEIGHT) LINE-HEIGHT)
                                     LINE-HEIGHT)
                                 LINE-HEIGHT))
  (AND MORE-VPOS (SETQ MORE-VPOS (LOGIOR 100000 MORE-VPOS)))) ;Delay until next time

(DEFMETHOD (SHEET :TERPRI) ()
  (SHEET-CRLF SELF))

(DEFUN SHEET-CRLF (SHEET)
  "Move SHEET's cursor to beginning of next line, and clear the line."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))        ;Handle exceptions first
        (SHEET-HANDLE-EXCEPTIONS SHEET))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SHEET-INCREMENT-BITPOS SHEET 0 (SHEET-LINE-HEIGHT SHEET))
    (SHEET-CLEAR-EOL SHEET)))

(DEFMETHOD (SHEET :LINE-OUT) (STRING &OPTIONAL (START 0) END)
  (SEND SELF :STRING-OUT STRING START END)
  (SEND SELF :TERPRI))

(DEFMETHOD (SHEET :FRESH-LINE) ()
  (IF (= CURSOR-X (SHEET-INSIDE-LEFT))
      (PROGN (SHEET-CLEAR-EOL SELF) NIL)
    (SHEET-CRLF SELF)
    T))

(DEFMETHOD (SHEET :CLEAR-CHAR) (&OPTIONAL CHAR)
  (SHEET-CLEAR-CHAR SELF CHAR))

(DEFUN SHEET-CLEAR-CHAR (SHEET &OPTIONAL CHAR)
  "Clear the character position SHEET's cursor points at.
CHAR may be a character whose width controls how wide an area to clear."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
        (SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
                                                     (SHEET-CURRENT-FONT SHEET))
                              (SHEET-CHAR-WIDTH SHEET))
                     (SHEET-LINE-HEIGHT SHEET)
                     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
                     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFMETHOD (SHEET :CLEAR-REST-OF-LINE) ()
  (SHEET-CLEAR-EOL SELF))

(DEFMETHOD (SHEET :CLEAR-EOL) ()
  (SEND SELF :CLEAR-REST-OF-LINE))

(DEFUN SHEET-CLEAR-EOL (SHEET)
  "Clear from SHEET's cursor to the right margin."
  (PREPARE-SHEET (SHEET)
    ;; Note that this need not handle **MORE** exception, because the **more**
    ;; would bash the line this is clearing anyway.  We don't want to **more**
    ;; if the next operation is going to be tyi.
    (OR (ZEROP (SHEET-END-PAGE-FLAG SHEET))
        (SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (MAX (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET))
                          0)
                     (MIN (- (SHEET-INSIDE-BOTTOM SHEET) (SHEET-CURSOR-Y SHEET))
                          (SHEET-LINE-HEIGHT SHEET))
                     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
                     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFMETHOD (SHEET :CLEAR-STRING) (STRING &OPTIONAL START END)
  (SHEET-CLEAR-STRING SELF STRING START END))

(DEFUN SHEET-CLEAR-STRING (SHEET STRING &OPTIONAL START END)
  "Clear enough space after SHEET's cursor to hold STRING, or part of it.
If STRING contains Return characters, we clear space on each line
to hold the characters of STRING on that line."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
        (SHEET-HANDLE-EXCEPTIONS SHEET))
    (DO ((LINE-START (OR START 0))
         (WHOLE-END (OR END (LENGTH STRING)))
         (PSEUDO-CURSOR-X (SHEET-CURSOR-X SHEET)
                          (SHEET-INSIDE-LEFT SHEET))
         (PSEUDO-CURSOR-Y (SHEET-CURSOR-Y SHEET)
                          (+ PSEUDO-CURSOR-Y LINE-HEIGHT))
         MAXIMUM-X FINAL-INDEX
         (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
         (LINE-END))
        (())
      ;; Do vertical wrap around at bottom of sheet.
      (IF (>= (+ PSEUDO-CURSOR-Y LINE-HEIGHT) (SHEET-INSIDE-BOTTOM SHEET))
          (SETQ PSEUDO-CURSOR-Y (SHEET-INSIDE-TOP SHEET)))
      ;; Find end of this line of the string.
      (SETQ LINE-END (OR (STRING-SEARCH-CHAR #/RETURN STRING LINE-START WHOLE-END)
                         WHOLE-END))
      ;; Does it fit in one screen-line?  If not, how much does?
      (MULTIPLE-VALUE (NIL FINAL-INDEX MAXIMUM-X)
        (SHEET-STRING-LENGTH SHEET STRING LINE-START LINE-END
                             (SHEET-INSIDE-RIGHT SHEET)
                             (SHEET-CURRENT-FONT SHEET)
                             PSEUDO-CURSOR-X))
      ;; We only handle what fits in this screen-line.
      (IF (= FINAL-INDEX LINE-END)
          (SETQ LINE-START (1+ LINE-END))
        (SETQ LINE-START FINAL-INDEX
              MAXIMUM-X (SHEET-INSIDE-RIGHT SHEET)))
      ;; Clear that much.
      (%DRAW-RECTANGLE (- MAXIMUM-X PSEUDO-CURSOR-X)
                       LINE-HEIGHT
                       PSEUDO-CURSOR-X PSEUDO-CURSOR-Y
                       (SHEET-ERASE-ALUF SHEET) SHEET)
      ;; If entire specified portion of string is done, exit.
      (IF (>= LINE-START WHOLE-END) (RETURN NIL)))))

(DEFMETHOD (SHEET :CLEAR-BETWEEN-CURSORPOSES) (START-X START-Y END-X END-Y)
  (SHEET-CLEAR-BETWEEN-CURSORPOSES SELF START-X START-Y END-X END-Y))

(DEFUN SHEET-CLEAR-BETWEEN-CURSORPOSES (SHEET START-X START-Y END-X END-Y
                                        &AUX (ALUF (SHEET-ERASE-ALUF SHEET)) MID-Y)
  "Erase on SHEET from START-X, START-Y to END-X, END-Y.
All positions are relative to SHEET's margins.
Does nothing if start is after end on the same line,
but if on different lines, assumes screen wrap-around"
  (SETQ START-X (MIN (+ START-X (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
        START-Y (MIN (+ START-Y (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
        END-X (MIN (+ END-X (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
        END-Y (MIN (+ END-Y (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET)))
  (PREPARE-SHEET (SHEET)
    (COND ((= START-Y END-Y)
           (COND ((< START-X END-X)
                  (%DRAW-RECTANGLE (- END-X START-X)
                                   (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y)
                                        (SHEET-LINE-HEIGHT SHEET))
                                   START-X START-Y ALUF SHEET))))
          (T (%DRAW-RECTANGLE (- (SHEET-INSIDE-RIGHT SHEET) START-X)
                              (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y)
                                   (SHEET-LINE-HEIGHT SHEET))
                              START-X START-Y ALUF SHEET)
             (SETQ MID-Y (+ START-Y (SHEET-LINE-HEIGHT SHEET)))
             (%DRAW-RECTANGLE END-X (MIN (- (SHEET-INSIDE-BOTTOM SHEET) END-Y)
                                         (SHEET-LINE-HEIGHT SHEET))
                              (SHEET-INSIDE-LEFT SHEET) END-Y ALUF SHEET)
             (IF (< START-Y END-Y)
                 (AND (< MID-Y END-Y)
                      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (- END-Y MID-Y)
                                       (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET))
                 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET)
                                  (- (SHEET-INSIDE-BOTTOM SHEET) MID-Y)
                                  (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET)
                 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET)
                                  (- END-Y (SHEET-INSIDE-TOP SHEET))
                                  (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)
                                  ALUF SHEET))))))

(DEFMETHOD (SHEET :CLEAR-SCREEN) ()
  (SHEET-CLEAR SELF))

(DEFMETHOD (SHEET :CLEAR-WINDOW) ()
  (SHEET-CLEAR SELF))

(DEFUN SHEET-CLEAR (SHEET &OPTIONAL (MARGINS-P NIL))
  "Erase all of SHEET.  If MARGINS-P, erase its margins too."
  (PREPARE-SHEET (SHEET)
    (SHEET-HOME SHEET)                          ;Handles any exceptions
    (IF MARGINS-P
        (%DRAW-RECTANGLE (SHEET-WIDTH SHEET) (SHEET-HEIGHT SHEET)
                         0 0
                         (SHEET-ERASE-ALUF SHEET) SHEET)
        (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (SHEET-INSIDE-HEIGHT SHEET)
                         (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)
                         (SHEET-ERASE-ALUF SHEET) SHEET))
    (SCREEN-MANAGE-FLUSH-KNOWLEDGE SHEET)))

(DEFMETHOD (SHEET :CLEAR-REST-OF-WINDOW) ()
  (SHEET-CLEAR-EOF SELF))

(DEFMETHOD (SHEET :CLEAR-EOF) ()
  (SEND SELF :CLEAR-REST-OF-WINDOW))

(DEFUN SHEET-CLEAR-EOF (SHEET &AUX HT TEM)
  "Clear from SHEET's cursor to right margin, and all area below."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
        (SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-CLEAR-EOL SHEET)
    (AND (PLUSP (SETQ HT (- (SHEET-INSIDE-BOTTOM SHEET)
                            (SETQ TEM (+ (SHEET-CURSOR-Y SHEET) (SHEET-LINE-HEIGHT SHEET))))))
         (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) HT
                          (SHEET-INSIDE-LEFT SHEET) TEM
                          (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :INSERT-LINE) (&OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  (SHEET-INSERT-LINE SELF LINE-COUNT UNIT))

(DEFUN SHEET-INSERT-LINE (SHEET &OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  "Make room for some line before the line the cursor is currently on.
The data on this line and below is moved downward on the screen,
and that near the bottom of SHEET is discarded.
LINE-COUNT is how many lines to insert; default 1.
UNIT is :CHARACTER (the default) or :PIXEL, and says what unit
LINE-COUNT is expressed in.  :CHARACTER means it is multiplied
by the window's line-height."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
          (WIDTH (SHEET-INSIDE-WIDTH SHEET))
          (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
          HEIGHT
          DELTA-HEIGHT)
      (SETQ HEIGHT (IF (EQ UNIT ':CHARACTER) (* LINE-COUNT LINE-HEIGHT) LINE-COUNT))
      ;; Compute minus height of block to BLT
      (SETQ DELTA-HEIGHT
            (- HEIGHT (- (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES SHEET))
                         (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)))))
      (OR ( DELTA-HEIGHT 0)                    ;If some bits to move, move them
          (BITBLT ALU-SETA
                  WIDTH DELTA-HEIGHT
                  ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
                  ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)))
      (%DRAW-RECTANGLE WIDTH HEIGHT
                       (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
                       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :DELETE-LINE) (&OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  (SHEET-DELETE-LINE SELF LINE-COUNT UNIT))

(DEFUN SHEET-DELETE-LINE (SHEET &OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  "Discard one or more lines starting at the cursor vpos, moving data below up.
Blank lines appear at the bottom of SHEET.
LINE-COUNT is how many lines to delete; default 1.
UNIT is :CHARACTER (the default) or :PIXEL, and says what unit
LINE-COUNT is expressed in.  :CHARACTER means it is multiplied
by the window's line-height."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
          (WIDTH (SHEET-INSIDE-WIDTH SHEET))
          (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
          HEIGHT
          DELTA-HEIGHT)
      (SETQ HEIGHT (IF (EQ UNIT :CHARACTER) (* LINE-COUNT LINE-HEIGHT) LINE-COUNT))
      (AND (PLUSP (SETQ DELTA-HEIGHT
                        (- (+ (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)) HEIGHT)
                           (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES SHEET)))))
           (FERROR "Illegal line-count ~S for ~S" LINE-COUNT SHEET))
      (BITBLT ALU-SETA WIDTH (- DELTA-HEIGHT)
              ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)
              ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH HEIGHT
                       (SHEET-INSIDE-LEFT SHEET) (- (SHEET-CURSOR-Y SHEET) DELTA-HEIGHT)
                       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :INSERT-CHAR) (&OPTIONAL (-WIDTH- 1) (UNIT :CHARACTER))
  (SHEET-INSERT-CHAR SELF -WIDTH- UNIT))

(DEFUN SHEET-INSERT-CHAR (SHEET &OPTIONAL (WIDTH 1) (UNIT :CHARACTER))
  "Make room for characters at SHEET's cursor, moving rest of line right.
The last part of the line is discarded.  The cursorpos does not change.
If UNIT is :CHARACTER, WIDTH is a number of characters.
This is accurate only for fixed-width fonts.
Alternatively, WIDTH may be a number of pixels
if UNIT is :PIXEL."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
          (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
          (WIDTH (IF (EQ UNIT ':PIXEL) WIDTH
                     (* WIDTH (SHEET-CHAR-WIDTH SHEET)))))
      (BITBLT ALU-SETA
              (- WIDTH (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET)))
              LINE-HEIGHT
              ARRAY (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
              ARRAY (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH LINE-HEIGHT
                       (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
                       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :DELETE-CHAR) (&OPTIONAL (-WIDTH- 1) (UNIT :CHARACTER))
  (SHEET-DELETE-CHAR SELF -WIDTH- UNIT))

(DEFUN SHEET-DELETE-CHAR (SHEET &OPTIONAL (WIDTH 1) (UNIT :CHARACTER))
  "Discard characters after SHEET's cursor, moving rest of line left.
Blank space is created near the right margin.
The cursor position does not change.
If UNIT is :CHARACTER, WIDTH is a number of characters.
This is accurate only for fixed-width fonts.
Alternatively, WIDTH may be a number of pixels
if UNIT is :PIXEL."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
          (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
          (WIDTH (IF (EQ UNIT :PIXEL) WIDTH
                     (* WIDTH (SHEET-CHAR-WIDTH SHEET)))))
      (BITBLT ALU-SETA
              (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET) WIDTH)
              LINE-HEIGHT
              ARRAY (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-CURSOR-Y SHEET)
              ARRAY (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH LINE-HEIGHT
                       (- (SHEET-INSIDE-RIGHT SHEET) WIDTH)
                       (SHEET-CURSOR-Y SHEET)
                       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :INSERT-STRING) (STRING &OPTIONAL (START 0) END (TYPE-TOO T))
  (SHEET-INSERT-STRING SELF STRING START END TYPE-TOO))

(DEFUN SHEET-INSERT-STRING (SHEET STRING &OPTIONAL (START 0) END (TYPE-TOO T) &AUX LEN)
  "Make room for STRING after SHEET's cursor, moving rest of line right.
The last part of the line is discarded.  The cursorpos does not change.
If TYPE-TOO is non-NIL, STRING is output into the space
and the cursor is left after it."
  (SETQ LEN (IF (NUMBERP STRING)
                (SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET))
                (SHEET-STRING-LENGTH SHEET STRING START END)))
  (SHEET-INSERT-CHAR SHEET LEN :PIXEL)
  (AND TYPE-TOO (SHEET-STRING-OUT SHEET STRING START END)))

(DEFMETHOD (SHEET :DELETE-STRING) (STRING &OPTIONAL (START 0) END)
  (SHEET-DELETE-STRING SELF STRING START END))

(DEFUN SHEET-DELETE-STRING (SHEET STRING &OPTIONAL (START 0) END &AUX LEN)
  "Delete enough space for STRING, after SHEET's cursor.
The following part of the line moves left, creating blank space at the end.
The cursor position does not change."
  (SETQ LEN (IF (NUMBERP STRING)
                (SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET))
                (SHEET-STRING-LENGTH SHEET STRING START END)))
  (SHEET-DELETE-CHAR SHEET LEN :PIXEL))

(DEFMETHOD (SHEET :DISPLAY-LOZENGED-STRING) (STRING)
  (SHEET-DISPLAY-LOZENGED-STRING SELF STRING))

(DEFUN SHEET-DISPLAY-LOZENGED-STRING (SHEET STRING)
  "Display STRING on SHEET inside a lozenge.
This is how special characters with no graphic or formatting meaning are output."
  (SETQ STRING (STRING STRING))
  (LET ((WIDTH (LOZENGED-STRING-WIDTH STRING)))
    ;; Make sure there is enough room on the line, if not CRLF and
    ;; hope the sheet isn't too narrow.  Relies on the fact that handling
    ;; of all exceptions leaves you no further to the right than you were
    ;; (usually at the left margin).
    (PREPARE-SHEET (SHEET)
      (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
          (SHEET-HANDLE-EXCEPTIONS SHEET))
      (COND ((> (+ (SHEET-CURSOR-X SHEET) WIDTH)
                (IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
                    (SHEET-INSIDE-RIGHT SHEET)
                    (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-WIDTH SHEET))))
             (SEND SHEET :END-OF-LINE-EXCEPTION)))
      (SETF (SHEET-CURSOR-X SHEET)
            (SHEET-DISPLAY-LOZENGED-STRING-INTERNAL SHEET STRING
                        (SHEET-CURSOR-X SHEET) (1+ (SHEET-CURSOR-Y SHEET))
                        (SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-ALUF SHEET))))))

(DEFUN LOZENGED-STRING-WIDTH (STRING)
  (+ 9. (* 6 (STRING-LENGTH STRING))))

(DEFUN SHEET-DISPLAY-LOZENGED-STRING-INTERNAL (SHEET STRING X0 Y0 XLIM ALUF)
  (LET* ((WIDTH (LOZENGED-STRING-WIDTH STRING))
         TEM (TRUNCATED 8))
    (WHEN (MINUSP (SETQ TEM (- XLIM (+ WIDTH X0))))
      (SETQ TRUNCATED 4)
      (SETQ WIDTH (+ WIDTH TEM)))
    (IF (> WIDTH 4)
        ;; Put the string then the box around it
        (LET ((X1 (+ X0 WIDTH -1))
              (Y1 (+ Y0 8)))
          (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING (+ X0 4) (+ Y0 2) X1 NIL
                                       (SEND (SHEET-GET-SCREEN SHEET)
                                             :PARSE-FONT-DESCRIPTOR FONTS:5X5)
                                       ALUF)
          (%DRAW-LINE X0 (+ Y0 4) (+ X0 3) (1+ Y0) ALUF T SHEET)
          (%DRAW-LINE (1+ X0) (+ Y0 5) (+ X0 3) (1- Y1) ALUF T SHEET)
          (WHEN (PLUSP (SETQ TEM (- WIDTH TRUNCATED)))
            (%DRAW-RECTANGLE TEM 1 (+ X0 4) Y0 ALUF SHEET)
            (%DRAW-RECTANGLE TEM 1 (+ X0 4) Y1 ALUF SHEET))
          (WHEN (EQ TRUNCATED 8)
            (%DRAW-LINE X1 (+ Y0 4) (- X1 3) (1+ Y0) ALUF T SHEET)
            (%DRAW-LINE (1- X1) (+ Y0 5) (- X1 3) (1- Y1) ALUF T SHEET))
          (1+ X1))
      XLIM)))

(defmethod (sheet :write-char) (ch)
  ;;>> Obviously, the whole sheet/font situation is a total loss and needs to be chucked...
  (check-type ch character)
  (if (zerop (char-font ch))
      (send self :tyo (char-code ch))
    (send self :tyo (char-code ch) (char-font ch))))

(DEFMETHOD (SHEET :TYO) (CH &OPTIONAL FONT)
  (SHEET-TYO SELF CH FONT)
  CH)

(DEFUN SHEET-TYO (SHEET CHAR &OPTIONAL FONT)
  "Output printing or formatting character CHAR on SHEET in FONT.
FONT defaults to the current font of SHEET.
Weird characters are printed in lozenges."
  (DO ((X) (Y) (WIDTH) (ALU) (CWT) (FIT) (LKT))
      (())
    (ETYPECASE CHAR
      (FIXNUM
       (IF (< CHAR #O200)
           (PREPARE-SHEET (SHEET)
             (SHEET-HANDLE-EXCEPTIONS-IF-NECESSARY SHEET)
             (COND ((NULL FONT)
                    (SETQ FONT (SHEET-CURRENT-FONT SHEET))
                    (SETQ Y (+ (SHEET-CURSOR-Y SHEET) (SHEET-BASELINE-ADJ SHEET))))
                   (T
                    (COERCE-FONT FONT SHEET)
                    (SETQ Y (+ (SHEET-CURSOR-Y SHEET)
                               (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))))
             (SETQ CWT (FONT-CHAR-WIDTH-TABLE FONT))
             (SETQ FIT (FONT-INDEXING-TABLE FONT))
             (SETQ ALU (SHEET-CHAR-ALUF SHEET))
             (SETQ WIDTH (IF CWT (AREF CWT CHAR) (FONT-CHAR-WIDTH FONT)))
             (COND (( (+ (SETQ X (SHEET-CURSOR-X SHEET)) WIDTH)
                       (IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
                           (SHEET-INSIDE-RIGHT SHEET)
                         (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-WIDTH SHEET))))
                    (COND ((NULL (SETQ LKT (FONT-LEFT-KERN-TABLE FONT)))
                           (IF (NULL FIT)
                               (%DRAW-CHAR FONT CHAR X Y ALU SHEET)
                             (DRAW-CHAR FONT CHAR X Y ALU SHEET)))
                          (T
                           (IF (NULL FIT)
                               (%DRAW-CHAR FONT CHAR (- X (AREF LKT CHAR)) Y ALU SHEET)
                             (DRAW-CHAR FONT CHAR (- X (AREF LKT CHAR)) Y ALU SHEET))))
                    (SETF (SHEET-CURSOR-X SHEET) (+ X WIDTH))
                    (RETURN-FROM SHEET-TYO CHAR))
                   (T
                    (SEND SHEET :END-OF-LINE-EXCEPTION)
                    (SHEET-TYO SHEET CHAR FONT))))
         (COND ((AND (= CHAR #/NEWLINE) (ZEROP (SHEET-CR-NOT-NEWLINE-FLAG SHEET)))
                (SHEET-CRLF SHEET))
               ((= CHAR #/TAB)
                (SHEET-TAB-1 SHEET))
               ((AND (= CHAR #/BACKSPACE) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
                (SHEET-BACKSPACE-1 SHEET))
               (T
                (SHEET-DISPLAY-LOZENGED-STRING SHEET
                 (STRING (OR (CAR (RASSQ CHAR SI::XR-SPECIAL-CHARACTER-NAMES))
                             (FORMAT NIL "~3O" CHAR)))))))
       (RETURN-FROM SHEET-TYO CHAR))
      (CHARACTER
       (SETQ CHAR (CHAR-INT CHAR))))))

(DEFMETHOD (SHEET :BACKWARD-CHAR) (&OPTIONAL CHAR)
  (SHEET-BACKSPACE-1 SELF CHAR))

(DEFUN SHEET-BACKSPACE-1 (SHEET &OPTIONAL CHAR)
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
        (SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS SHEET
                            (- (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
                                                               (SHEET-CURRENT-FONT SHEET))
                                        (SHEET-CHAR-WIDTH SHEET)))
                            0)))

(DEFUN SHEET-TAB-1 (SHEET)
  (PREPARE-SHEET (SHEET)
     (OR (ZEROP (SHEET-EXCEPTIONS SHEET)) (SHEET-HANDLE-EXCEPTIONS SHEET))
     (LET ((TAB-WIDTH (SHEET-TAB-WIDTH SHEET)))
       (SHEET-INCREMENT-BITPOS SHEET (- TAB-WIDTH (\ (- (SHEET-CURSOR-X SHEET)
                                                        (SHEET-INSIDE-LEFT SHEET))
                                                     TAB-WIDTH))
                               0))))

(DEFMETHOD (SHEET :FORWARD-CHAR) (&OPTIONAL CHAR)
  (SHEET-SPACE SELF CHAR))

(DEFUN SHEET-SPACE (SHEET &OPTIONAL CHAR)
  "Move SHEET's cursor forward one character position.
Will move to a new line if necessary.
If CHAR is specified, that character's width is the distance to move."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
        (SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS SHEET
                            (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
                                                            (SHEET-CURRENT-FONT SHEET))
                                     (SHEET-CHAR-WIDTH SHEET))
                            0)))

(DEFMETHOD (SHEET :STRING-OUT) (STRING &OPTIONAL START STOP)
  (sheet-string-out self string start stop))

(DEFUN SHEET-STRING-OUT (SHEET STRING &OPTIONAL (START 0) (STOP NIL))
  "Output STRING or portion thereof on SHEET."
  (WHEN (SYMBOLP STRING) (SETQ STRING (SYMBOL-NAME STRING)))
  (WHEN (NULL START) (SETQ START 0))
  (WHEN (NULL STOP) (SETQ STOP (ARRAY-ACTIVE-LENGTH STRING)))
  (DO ((XLIM (IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG sheet))
                 (SHEET-INSIDE-RIGHT sheet)
               (- (SHEET-INSIDE-RIGHT sheet) (sheet-CHAR-WIDTH sheet))))
       (new-x)
       (STOPPED))
      (())
    (PREPARE-SHEET (SHEET)
      (SHEET-HANDLE-EXCEPTIONS-IF-NECESSARY sheet)
      (MULTIPLE-VALUE-SETQ (STOPPED new-X)
        (%DRAW-STRING SHEET (sheet-CHAR-ALUF sheet) (sheet-CURSOR-X sheet) (sheet-CURSOR-Y sheet)
                      STRING (sheet-CURRENT-FONT sheet) START STOP XLIM))
      (setf (sheet-cursor-x sheet) new-x)
      (COND ((> STOPPED STOP)                          ;All characters drawn.
             (RETURN NIL))
            ((EQ (ZL:AREF STRING STOPPED) #/NEWLINE)    ;Stopped at newline.
             (SEND SHEET :END-OF-LINE-EXCEPTION)
             (SETQ START (1+ STOPPED)))
            (T                                         ;Stopped at horizontal limit.
             (SEND SHEET :END-OF-LINE-EXCEPTION)
             (SETQ START STOPPED))))))

;;; *** SHEET-STRING-OUT now does the right thing with all kinds of strings ***

(defmethod (sheet :fat-string-out) (string &optional (start 0) end)
  (sheet-string-out self string start end))

(deff sheet-fat-string-out 'sheet-string-out)
(compiler:make-obsolete sheet-fat-string-out "use TV:SHEET-STRING-OUT")

;;; Editor's line redisplay primitive, output STRING from START to END,
;;; first setting position to (SET-XPOS,SET-YPOS) and doing a clear-eol
;;; DWIDTH is a special hack for DIS-LINE redisplay of italic fonts, it means
;;; draw an extra character starting one character back, since the clear-eol
;;; will have erased part of the last character where it sticks out past its width.
;;; (If this can really happen, it's going to mean trouble with the margins, too!)
;;; This function never does more than one line; it stops rather than wrapping around.
;;; If you put a carriage return in the string, above may not be true.
;;; Where this leaves the sheet's actual cursorpos is undefined (somewhere on the line)

;;; This is NOT related to the :LINE-OUT operation!
(defun sheet-line-out (sheet string &optional (start 0) (stop nil) set-xpos set-ypos dwidth)
  ;; Returns index of next character to do and where cursor got to except the first value can
  ;; be incremented, to show that the line was completed (as though it counted an implicit
  ;; carriage return).
  (declare (values index xpos))
  (let* ((inside-right (sheet-inside-right sheet))
         (inside-left (sheet-inside-left sheet))
         (margin-flag (not (zerop (sheet-right-margin-character-flag sheet))))
         (xpos (if set-xpos (+ set-xpos inside-left) (sheet-cursor-x sheet)))
         (ypos (if set-ypos (+ set-ypos (sheet-inside-top sheet))
                 (sheet-cursor-y sheet)))
         (stop-index)
         (stop-xpos))
  (prepare-sheet (sheet)
    (setq stop (or stop (array-active-length string)))
    (setf (sheet-cursor-y sheet) ypos)          ;%DRAW-STRING depends on this in some cases.
    (%draw-rectangle
      (- inside-right xpos) (sheet-line-height sheet) xpos ypos (sheet-erase-aluf sheet) sheet)
    (when dwidth                                ;Italic correction, back up one character.
      (setq xpos (- xpos dwidth))
      (decf start))
    (multiple-value (stop-index stop-xpos)
      (%draw-string sheet (sheet-char-aluf sheet) xpos ypos string 0 start stop
                    (if margin-flag (- inside-right (sheet-char-width sheet)) inside-right)))
    (when (< stop-index stop)
      (when margin-flag (send sheet :tyo-right-margin-character stop-xpos ypos #/!)))
    (values stop-index (- stop-xpos inside-left)))))

(defmethod (sheet :screen-line-out) (string &optional (start 0) (stop nil) set-xpos set-ypos dwidth)
  (sheet-line-out self string start stop set-xpos set-ypos dwidth))

(DEFMETHOD (SHEET :COMPUTE-MOTION) (STRING &OPTIONAL (START 0) END X Y &REST ARGS)
  (APPLY #'SHEET-COMPUTE-MOTION SELF X Y STRING START END ARGS))

(DEFCONST PRINTING-CHARACTER-TRANSLATE-TABLE (MAKE-ARRAY #o200 :INITIAL-VALUE 1))

;; Change to T to fix the bug
;; that we stop after a character that goes across STOP-X
;; though we ought to stop before it.
;; Should not turn on the fix until the callers are changed to match.
;; If indeed the change really should be made.
(DEFCONST COMPUTE-MOTION-ROUND-DOWN NIL)

(DEFUN SHEET-COMPUTE-MOTION (SHEET X Y STRING
                             &OPTIONAL (START 0) (END NIL) (CR-AT-END-P NIL)
                                       (STOP-X 0) (STOP-Y NIL) BOTTOM-LIMIT RIGHT-LIMIT
                                       FONT
                                       (LINE-HT (IF FONT (FONT-CHAR-HEIGHT FONT)
                                                  (SHEET-LINE-HEIGHT SHEET)))
                                       (TAB-WIDTH
                                         (IF FONT (* (FONT-CHAR-WIDTH FONT)
                                                     (SHEET-TAB-NCHARS SHEET))
                                           (SHEET-TAB-WIDTH SHEET))))
  "Compute the motion that would be caused by outputing a string.
This is used by the editor and by TV:STREAM-MIXIN.
In computing the motion, it will chose the font in one of two ways:
 If given an ART-FAT-STRING array (16 bit string) like the editor uses,
  it will take the font from the CHAR-FONT field of the
  character, and look in SHEET's font-map.
 If given an ART-STRING array (8 bit string), it will take the font from
  FONT, or the SHEET-CURRENT-FONT of the sheet.
SHEET is used to supply information such as the font map,
 and for defaulting such things as BOTTOM-LIMIT, RIGHT-LIMIT
 and LINE-HT.
STRING, with START and END, specifies what characters to process.
CR-AT-END-P if non-NIL says /"output/" a Return after
 STRING or the portion of STRING, and count that
 in the cursor motion.
STOP-X and STOP-Y specify a cursor position at which to stop.
 Processing stops when both coordinates are  the stop points.
 The stop points default to the bottom left corner of SHEET.
 Specify a very large value for STOP-Y if you do not
 want processing to stop before the end of STRING.
BOTTOM-LIMIT and RIGHT-LIMIT are a cursor position
 at which to wrap around; these default to
 the inside-size of SHEET.
FONT specifies the font to use, if STRING is not a fat string.
LINE-HT is the line height to use for Return characters,
 defaulting to SHEET's line height.
TAB-WIDTH is the width to use for Tab characters,
 defaulting to SHEET's SHEET-TAB-WIDTH.

Processing stops either because the string or portion has
been processed or because the stopping-point has been reached.

Returns 4 values:
FINAL-X, FINAL-Y are the cursor position
 at which processing stopped.
FINAL-STRING-INDEX is the index
 in the string at which processing stopped (could be the length
 of the string, if the stop point was passed then), T if stopped
 due to reaching the stop point after the additional Return,
 or NIL if stopped due to finishing.
MAXIMUM-X was the largest X-position ever encountered during processing."

; *** The interface to this crock should be redesigned.  Also note that the
; *** exact treatment of STOP-X and STOP-Y does not agree with SHEET-STRING-LENGTH.
; *** This is what turning on COMPUTE-MOTION-ROUND-DOWN is going to fix.
  (DECLARE (VALUES FINAL-X FINAL-Y FINAL-STRING-INDEX MAXIMUM-X))
  (IF FONT
      (COERCE-FONT FONT SHEET)
    (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (PROG (CWA CW CH FONTX TEM I N NN II MARGIN-FLAG MAXIMUM-X OLD-X)
    (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET)) (SETQ MARGIN-FLAG T))
    (AND (NULL X) (SETQ X (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))))
    (AND (NULL Y) (SETQ Y (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))
    (AND (NULL STOP-Y)
         (SETQ STOP-Y (1+ (SHEET-INSIDE-HEIGHT SHEET))))
                    ;   ^-- THIS 1+ IS SO CAN USE  RATHER THAN >
    (OR RIGHT-LIMIT (SETQ RIGHT-LIMIT (SHEET-INSIDE-WIDTH SHEET)))
    (AND MARGIN-FLAG (SETQ RIGHT-LIMIT (- RIGHT-LIMIT (SHEET-CHAR-WIDTH SHEET))))
    (AND (NULL BOTTOM-LIMIT)
         (SETQ BOTTOM-LIMIT (- (SHEET-INSIDE-HEIGHT SHEET) LINE-HT)))
    (SETQ MAXIMUM-X X
          I START
          N (OR END (ARRAY-ACTIVE-LENGTH STRING))
          CW (FONT-CHAR-WIDTH FONT))
    ;; At this point, decide whether we can use the fast version.
    (COND
      ;; If FONTX is non-NIL, then we have a string with font changes.
      ((neq (%p-mask-field-offset #.%%array-type-field string 0) #.art-string)
       (SETQ FONTX T))
      ;; The current font is variable width.
      ((SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))
      ;; No font changes and the current font is fixed width.  We can use the fast version.
      (T (GO FAST)))
    ;;This is the slow version.
SLOW
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    (COND ((AND ( Y STOP-Y) ( X STOP-X))      ;Reached sticking-point
           (RETURN (VALUES X Y I MAXIMUM-X)))
          ((NOT (< I N))                        ;If string exhausted
           (COND (CR-AT-END-P
                  (SETQ X 0 Y (+ Y LINE-HT))    ;CRLF if told to
                  (AND (> Y BOTTOM-LIMIT) (SETQ Y 0))))
           (RETURN (VALUES X Y (AND ( X STOP-X) ( Y STOP-Y)) MAXIMUM-X))))
    ;; Move quickly over the remaining characters until we reach
    ;; an x-position at which something must be done.
    (UNLESS (EQ FONTX T)
      (LET (WIDTH-INCR
            (LIMIT (MIN RIGHT-LIMIT (IF ( Y STOP-Y) STOP-X RIGHT-LIMIT))))
        (SETQ WIDTH-INCR
              (%STRING-WIDTH (OR CWA
                                 PRINTING-CHARACTER-TRANSLATE-TABLE)
                             (IF FONTX (DPB FONTX %%CH-FONT 0) 0)
                             STRING I N
                             (IF CWA (- LIMIT X) (FLOOR (- LIMIT X) CW))))
        (UNLESS CWA
          (SETQ WIDTH-INCR (* WIDTH-INCR CW)))
        (SETQ I (%POP))
        ;; increment positions.
        (SETQ X (+ WIDTH-INCR X))
        ;; At end of string, loop back, to exit.
        (IF (= I N) (GO SLOW))
        ;; Otherwise we stopped due to funny char or font change or reaching the X limit.
        ))
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    (IF (AND ( Y STOP-Y) ( X STOP-X))         ;If reached sticking-point, done.
        (RETURN (VALUES X Y I MAXIMUM-X)))
    (SETQ CH (CHAR-CODE (SETQ TEM (ZL:AREF STRING I))))
    (WHEN (AND FONTX (NEQ (SETQ TEM (CHAR-FONT TEM)) FONTX)) ;Changing fonts
      (SETQ FONTX TEM
            FONT (LET ((FONT-MAP (SHEET-FONT-MAP SHEET)))
                   (AREF FONT-MAP (IF ( FONTX (ARRAY-ACTIVE-LENGTH FONT-MAP))
                                      0 FONTX)))
            CWA (FONT-CHAR-WIDTH-TABLE FONT)
            CW (FONT-CHAR-WIDTH FONT)))
    (SETQ OLD-X X)
    ;; Try to do this one char.
    (COND ((= CH #/NEWLINE)
           (SETQ X 0 Y (+ Y LINE-HT))
           (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
          ((< CH #o200)                         ;Printing character
           (SETQ X (+ (COND (CWA (AREF CWA CH)) (T CW)) X))) ;do char width
          ((= CH #/TAB)                         ;Tab (have to do here since x-dependent)
           (SETQ TEM TAB-WIDTH)
           (SETQ X (* (TRUNCATE (+ X TEM) TEM) TEM)))
          (T                                    ;Format effector
           (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
    ;; If this char went past the stop-point, pretend we stopped before it.
    (WHEN (AND COMPUTE-MOTION-ROUND-DOWN
               (> X STOP-X) ( Y STOP-Y))
      (RETURN (VALUES OLD-X Y I MAXIMUM-X)))
    ;; If this char went past the right margin, do a CR, then redo this char
    (COND ((> X RIGHT-LIMIT)
           (SETQ X 0 Y (+ Y LINE-HT))
           (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
          (T (SETQ I (1+ I))))
    (GO SLOW)

    ;;Here is the fast loop.  The basic idea is to scan as fast as possible
    ;;over printing characters, with all checking outside the loop.
FAST
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    ;;First, decide the most characters we want to scan over in a whack
    (SETQ NN
          (MIN (+ (TRUNCATE (- (COND (( Y STOP-Y)      ;Stop-point is in this line
                                      STOP-X)
                                     (T RIGHT-LIMIT))   ;Stop for this line is margin
                               X)
                            CW)
                  I)
               N))                              ;NN is limiting value of I
    ;Now, scan over printing characters.
    (AND ( (SETQ II I) NN)                     ;Save initial I, and check for null loop
         (GO SCX))
SCN (%STRING-WIDTH PRINTING-CHARACTER-TRANSLATE-TABLE
                   0 STRING II NN NIL)
    (SETQ I (%POP))
    (SETQ X (+ (* (- I II) CW) X))              ;Account for the motion of those chars
SCX (SETQ NN X)
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    (COND ((AND ( Y STOP-Y) ( X STOP-X))      ;If reached sticking-point, done.
           (RETURN (VALUES X Y I MAXIMUM-X)))
          ((NOT (< I N))                        ;If string exhausted
           (COND (CR-AT-END-P                   ;Do return X off end of line
                  (SETQ X 0 Y (+ Y LINE-HT))    ;crlf if told to
                  (AND (> Y BOTTOM-LIMIT) (SETQ Y 0))))
           (RETURN (VALUES X Y (AND ( X STOP-X) ( Y STOP-Y)) MAXIMUM-X))))
    (SETQ OLD-X X)
    ;; Try to do this one char.
    (COND ((= (SETQ CH (ZL:AREF STRING I)) #/NEWLINE)
           (SETQ X 0 Y (+ Y LINE-HT))
           (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
          ((< CH #o200)                         ;Printing character
           (SETQ X (+ CW X)))
          ((= CH #/TAB)                         ;Tab (have to do here since x-dependent)
           (SETQ TEM TAB-WIDTH)
           (SETQ X (* (TRUNCATE (+ X TEM) TEM) TEM)))
          (T                                    ;Format effector
           (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
    ;; If this char went past the stop-point, pretend we stopped before it.
    (WHEN (AND COMPUTE-MOTION-ROUND-DOWN
               (> X STOP-X) ( Y STOP-Y))
      (RETURN (VALUES OLD-X Y I MAXIMUM-X)))
    ;; If this char went past the right margin, do a CR and then redo this char.
    (COND ((> X RIGHT-LIMIT)
           (SETQ X 0 Y (+ Y LINE-HT))
           (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
          (T (SETQ I (1+ I))))
    (GO FAST)
))

(DEFMETHOD (SHEET :CHARACTER-WIDTH) (CHAR &OPTIONAL (FONT CURRENT-FONT))
  (SHEET-CHARACTER-WIDTH SELF CHAR
                         (IF (TYPEP FONT 'FONT)
                             FONT
                           (SEND (SHEET-GET-SCREEN SELF) :PARSE-FONT-DESCRIPTOR FONT))))

(DEFUN SHEET-CHARACTER-WIDTH (SHEET CH FONT &AUX TEM)
  "Returns the width of a character, in raster units.
For backspace, it can return a negative number.
For tab, the number returned depends on the current cursor position.
For return, the result is zero.
CH can be NIL; then you get the font's character-width."
  (COERCE-FONT FONT SHEET)
  (if (characterp ch) (setq ch (char-int ch)))
  (COND ((NULL CH) (FONT-CHAR-WIDTH FONT))
        ((< CH #o200)                           ;Ordinary printing character
         (IF (SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT))
             (AREF TEM CH)
           (FONT-CHAR-WIDTH FONT)))
        ((= CH #/NEWLINE) 0)                    ;Return
        ((= CH #/TAB)                           ;Tab
         (SETQ TEM (SHEET-TAB-WIDTH SHEET))
         (- (* (TRUNCATE (+ (SHEET-CURSOR-X SHEET) TEM) TEM) TEM)
            (SHEET-CURSOR-X SHEET)))
        ((AND (= CH #/BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
         (MINUS (SHEET-CHAR-WIDTH SHEET)))              ;Backspace
        (T                                              ;Misc lozenge character
         (LET ((TEM (CAR (RASSQ CH SI::XR-SPECIAL-CHARACTER-NAMES))))
           (LOZENGED-STRING-WIDTH (OR TEM "777"))))))

(DEFMETHOD (SHEET :STRING-LENGTH) (&REST ARGS)
  (APPLY #'SHEET-STRING-LENGTH SELF ARGS))

(DEFUN SHEET-STRING-LENGTH (SHEET STRING &OPTIONAL (START 0) (END NIL) (STOP-X NIL)
                                                   FONT (START-X 0)
                                                   (TAB-WIDTH
                                                     (IF FONT (* (FONT-CHAR-WIDTH FONT)
                                                                 (SHEET-TAB-NCHARS SHEET))
                                                       (SHEET-TAB-WIDTH SHEET)))
                                                   &AUX (MAX-X START-X))
  "Return the length in X-position of STRING or a portion.
START and END specify the portion (default is all).
START-X is an X-position to begin computation at.
STOP-X is an X-position at which to stop processing and return.
 FINAL-INDEX will indicate where in the string this was reached.
FONT is the font to use (default is SHEET's current font);
 but if STRING is an ART-FAT-STRING, each character's font
 is looked up in SHEET's font-map.
TAB-WIDTH is the width to use for tab characters,
 defaulting to SHEET's SHEET-TAB-WIDTH.

The cursor position does not wrap around during processing;
arbitrarily large values can be returned.
Use TV:SHEET-COMPUTE-MOTION if you want wrap-around.

Three values are returned:
FINAL-X is the X-position when processing stopped
 (due to end of string or portion, or reaching STOP-X).
FINAL-INDEX is the index in the string at which processing stopped.
MAXIMUM-X is the largest X-position reached during processing.
 This can be larger than FINAL-X if the string contains
 Backspaces or Returns."
  (DECLARE (VALUES FINAL-X FINAL-INDEX MAXIMUM-X))
  (IF FONT
      (COERCE-FONT FONT SHEET)
    (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (PROG (CWA CW CH FONTX TEM I N NN II STRINGP (X START-X))
    (SETQ I START
          N (OR END (ARRAY-ACTIVE-LENGTH STRING))
          CW (FONT-CHAR-WIDTH FONT))
    ;At this point, decide whether we can use the fast version
SLOW
    (AND (SETQ STRINGP (= (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0)
                          #.ART-STRING))                ;i.e. no font changes
         (NULL (SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT))) ;and fixed width
         (GO FAST))
SLOW0
    (OR (< I N) (RETURN (VALUES X I MAX-X)))                    ;If string exhausted
    ;; Move quickly over 20 characters, or all the remaining characters,
    ;; if that does not go past STOP-X.
    (WHEN (OR STRINGP FONTX)
      (LET ((WIDTH-INCR
              (%STRING-WIDTH (OR CWA
                                 PRINTING-CHARACTER-TRANSLATE-TABLE)
                             (IF FONTX (DPB FONTX %%CH-FONT 0) 0)
                             STRING I N
                             (AND STOP-X
                                  (IF CWA
                                      (- STOP-X X)
                                    (FLOOR (- STOP-X X) CW))))))
        (UNLESS CWA
          (SETQ WIDTH-INCR (* WIDTH-INCR CW)))
        (SETQ I (%POP))
        (SETQ X (+ WIDTH-INCR X))
        (SETQ MAX-X (MAX X MAX-X))
        ;; Loop back if reached end of string.
        (IF (= I N) (GO SLOW0))
        ;; Otherwise we stopped due to funny char or font change or reaching STOP-X.
        ))
    (SETQ CH (CHAR-CODE (SETQ TEM (ZL:AREF STRING I))))
    (COND ((AND (NOT STRINGP)                           ;Changing fonts
                (NEQ (SETQ TEM (CHAR-FONT TEM)) FONTX))
           (SETQ FONTX TEM
                 FONT (AREF (SHEET-FONT-MAP SHEET) FONTX)
                 CWA (FONT-CHAR-WIDTH-TABLE FONT)
                 CW (FONT-CHAR-WIDTH FONT))))
    (COND ((< CH #o200)                                 ;Printing character
           (SETQ NN (IF CWA (AREF CWA CH) CW)))
          ((= CH #/TAB)
           (SETQ TEM TAB-WIDTH)
           (SETQ NN (- (* (TRUNCATE (+ X TEM) TEM) TEM) X)))
          ((AND (= CH #/BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
           (SETQ NN (- (MAX 0 (- X (SHEET-CHAR-WIDTH SHEET))) X)))
          ((= CH #/CR)
           (SETQ NN 0 X 0))
          (T                                    ;Lozenged character
           (SETQ NN (SHEET-CHARACTER-WIDTH SHEET CH FONT))))
    (SETQ X (+ X NN))
    (IF (> X MAX-X) (SETQ MAX-X X))
    (AND STOP-X (> X STOP-X)                    ;If char doesn't fit, stop before it
         (RETURN (VALUES (- X NN) I MAX-X)))
    (SETQ I (1+ I))
    (GO SLOW)

    ;Here is the fast loop.  The basic idea is to scan as fast as possible
    ;over printing characters, with all checking outside the loop.
FAST
    ;First, decide the most characters we want to scan over in a whack
    (SETQ NN (COND ((NULL STOP-X) N)            ;NN is limiting value of I
                   ((MIN (+ (TRUNCATE (- STOP-X X) CW)
                            I)
                         N))))
    ;Now, scan over printing characters.
    (AND ( (SETQ II I) NN)                     ;Save initial I, and check for null loop
         (GO SLOW0))
SCN (%STRING-WIDTH PRINTING-CHARACTER-TRANSLATE-TABLE
                   0 STRING II NN NIL)
    (SETQ I (%POP))
;    (SETQ TEM #o200)                           ;This is really a ridiculous bum
;SCN (AND (< (ZL:AREF STRING I) TEM)            ;If this is a printing character
;        (< (SETQ I (1+ I)) NN)                 ; and we haven't reached stop point
;        (GO SCN))                              ; then continue to loop (9 instructions)
    (SETQ X (+ (* (- I II) CW) X))              ;Account for the motion of those chars
    (IF (> X MAX-X) (SETQ MAX-X X))
    (GO SLOW0)                                  ;Either string exhausted, non-printing,
                                                ; or reached stop-x
))

(DEFMETHOD (SHEET :STRING-OUT-EXPLICIT)
           (STRING START-X START-Y X-LIMIT Y-LIMIT FONT ALU
            &OPTIONAL (START 0) END MULTI-LINE-LINE-HEIGHT)
  (SHEET-STRING-OUT-EXPLICIT-1 SELF STRING START-X START-Y X-LIMIT Y-LIMIT FONT ALU
                               START END MULTI-LINE-LINE-HEIGHT))

(COMPILER:MAKE-OBSOLETE SHEET-STRING-OUT-EXPLICIT
                        "use the :STRING-OUT-EXPLICIT operation with rearranged args")
(DEFUN SHEET-STRING-OUT-EXPLICIT (SHEET STRING START-X Y XLIM FONT ALU
                                  &OPTIONAL (START 0) (END NIL)
                                  MULTI-LINE-LINE-HEIGHT YLIM)
  (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING START-X Y XLIM YLIM
                               FONT ALU START END MULTI-LINE-LINE-HEIGHT))

(DEFUN SHEET-STRING-OUT-EXPLICIT-1 (SHEET STRING START-X Y XLIM YLIM FONT ALU
                                    &OPTIONAL (START 0) (END NIL)
                                    MULTI-LINE-LINE-HEIGHT
                                    &AUX FIT FWT LKT
                                    (X START-X))
  "Output STRING on SHEET without using SHEET's cursor, font, etc.
Output starts at cursor position START-X, Y but SHEET's cursor is not moved.
Output stops if x-position XLIM or y-position YLIM is reached.
Font FONT is used, and alu-function ALU.
START and END specify a portion of STRING to be used.
MULTI-LINE-LINE-HEIGHT is how far to move down for Return characters;
 Return also moves back to x-position START-X.
 NIL means output <Return> with a lozenge.
All position arguments are relative to SHEET's outside edges."
  (DECLARE (VALUES FINAL-X FINAL-INDEX FINAL-Y))
  (COERCE-FONT FONT SHEET)
  (SETQ FIT (FONT-INDEXING-TABLE FONT)
        FWT (FONT-CHAR-WIDTH-TABLE FONT)
        LKT (FONT-LEFT-KERN-TABLE FONT))
  (OR XLIM (SETQ XLIM (SHEET-WIDTH SHEET)))
  (PREPARE-SHEET (SHEET)
    (DO ((I START (1+ I))
         (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
         (WIDTH (FONT-CHAR-WIDTH FONT))
         (CH))
        (( I N) (VALUES X Y I))
      (SETQ CH (ZL:AREF STRING I))
      (COND ((AND MULTI-LINE-LINE-HEIGHT (= CH #/NEWLINE))
             (SETQ X START-X Y (+ Y MULTI-LINE-LINE-HEIGHT))
             (IF (AND YLIM (> (+ Y MULTI-LINE-LINE-HEIGHT) YLIM))
                 (RETURN (VALUES X Y I))))
            (( CH #o200)
             (LET* ((STRING (STRING (OR (CAR (RASSQ CH SI::XR-SPECIAL-CHARACTER-NAMES))
                                        (FORMAT NIL "~3O" CH))))
                    (NX (+ X (LOZENGED-STRING-WIDTH STRING))))
               (IF (> NX XLIM) (RETURN (VALUES X Y I)))
               (SHEET-DISPLAY-LOZENGED-STRING-INTERNAL SHEET STRING
                                                       X (1+ Y) XLIM ALU)
               (SETQ X NX)))
            (T (IF FWT (SETQ WIDTH (AREF FWT CH)))
               (IF (> (+ X WIDTH) XLIM) (RETURN (VALUES X Y I)))
               (DRAW-CHAR FONT CH
                          (IF LKT (- X (AREF LKT CH)) X)
                          Y ALU SHEET)
               (SETQ X (+ X WIDTH)))))))

;like %DRAW-CHAR but works on wide characters just as on narrow ones.
(DEFUN DRAW-CHAR (FONT CH X Y ALU SHEET)
  "Draw character CH in FONT at X, Y in SHEET using alu-function ALU.
X and Y are relative to SHEET's outside edges."
  (COERCE-FONT FONT SHEET)
  (if (characterp ch) (setq ch (char-int ch)))
  (LET ((FIT (FONT-INDEXING-TABLE FONT)))
    (IF FIT
        (DO ((CH (AREF FIT CH) (1+ CH))
             (LIM (AREF FIT (1+ CH)))
             (BPP (IF (ARRAYP SHEET) (ARRAY-BITS-PER-PIXEL SHEET)
                    (SHEET-BITS-PER-PIXEL SHEET)))
             (X X (+ X (TRUNCATE (FONT-RASTER-WIDTH FONT) BPP))))
            (( CH LIM))
          (%DRAW-CHAR FONT CH X Y ALU SHEET))
      (%DRAW-CHAR FONT CH X Y ALU SHEET))))

(defun %draw-string (sheet alu xpos ypos string font start stop xlim)
  "Draw STRING on SHEET starting with the character at index START and stopping after
drawing the character at index STOP, presuming it all fits.  Output starts at XPOS,
YPOS on the sheet and continues until all appropriate characters are drawn, or until
the next character to be drawn would extend past XLIM.  The index of the next character
to be drawn, and the xpos where it would go are returned.  If a NEWLINE is encountered,
returns its index and xpos immediately.  The sheet's cursor position is ignored and
left unchanged."
  (declare (values index xpos))
  ;; Warning!  This function has been extensively tweaked by someone who knows what's
  ;; actually going on in the machine.  There are a lot of weird efficiency hacks,
  ;; particularly in the use of local variables and the stack, but also some weird
  ;; branching.  If you must modify this function, keep in mind that the Zmacs redisplay
  ;; spends more than 90% of its time here.  This function, in turn, should spend at
  ;; least 90% of its time in %draw-char (it currently does).
  (prog (c (i start) width (tab-width (sheet-tab-width sheet)) (base-ypos ypos) (npos xpos)
         font-index font-next (font-map (sheet-font-map sheet))
         font-index-table font-width-table font-kern-table lozenge-string
         (inside-left (sheet-inside-left sheet))
         (graphic-char-limit #o200))            ;In local variable for speed.

        (when ( i stop) (return (values (1+ stop) xpos)))
        (unless (eq (%p-mask-field-offset #.%%array-type-field string 0) #.art-string)
          (go multiple-font))

   single-font
        (when (fixnump font) (setq font (aref font-map font)))
        (setq ypos (+ ypos (- (sheet-baseline sheet) (font-baseline font))))
        (cond ((setq font-index-table (font-indexing-table font))
               (setf (sheet-cursor-x sheet) xpos)
               (go single-hairy-font-loop))
              ((or (setq font-width-table (font-char-width-table font))
                   (setq font-kern-table (font-left-kern-table font)))
               (setq width (font-char-width font))
               (go single-variable-width-font-loop))
              (t
               (setq width (font-char-width font))
               (go single-fixed-width-font-loop)))

   ;; I considered setting up a little pipeline for this case, but currently the bottleneck
   ;; is %draw-char.  We need a font format that's a little easier to draw.  A faster
   ;; video board wouldn't hurt either.  Currently the inner loop is 20. trivial instructions,
   ;; one cached aref of a simple string, and one %draw-char.

   single-fixed-width-font-loop
        (when ( (setq c (zl:aref string i)) graphic-char-limit)
          (case c
            (#/tab     (go single-fixed-width-font-tab))
            (#/return  (return (values i npos)))
            (otherwise (go single-fixed-width-font-hard))))
   single-fixed-width-font-char
        (when (> (setq npos (+ (setq xpos npos) width)) xlim)
          (go exceeded-x-limit))
        (%draw-char font c xpos ypos alu sheet)
        (when (eq (incf i) stop)
          (go finished))
        (if (< (setq c (zl:aref string i)) graphic-char-limit)
            (go single-fixed-width-font-char)
          (case c
            (#/tab     (go single-fixed-width-font-tab))
            (#/return  (return (values i npos)))
            (otherwise (go single-fixed-width-font-hard))))
   single-fixed-width-font-tab
        (setq npos (+ (* (truncate (+ (setq xpos npos) tab-width) tab-width)
                         tab-width) inside-left))
        (when (> npos xlim) (return (values i xpos)))
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (go single-fixed-width-font-loop)
   single-fixed-width-font-hard
        (setq lozenge-string (or (car (rassq c si:xr-special-character-names))
                                 (format nil "~O" c)))
        (setq npos (+ (setq xpos npos) (lozenged-string-width lozenge-string)))
        (when (> npos xlim) (return (values i xpos)))
        (setf (sheet-cursor-x sheet) xpos)
        (sheet-display-lozenged-string sheet lozenge-string)
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (go single-fixed-width-font-loop)

   exceeded-x-limit
        (return (values i xpos))
   finished
        (return (values (1+ stop) npos))

   single-variable-width-font-loop
        (when ( (setq c (zl:aref string i)) graphic-char-limit)
          (case c
            (#/tab     (go single-variable-width-font-tab))
            (#/return  (return (values i npos)))
            (otherwise (go single-variable-width-font-hard))))
   single-variable-width-font-char
        (when (> (setq npos (+ (setq xpos npos)
                               (if font-width-table (aref font-width-table c) width))) xlim)
          (go exceeded-x-limit))
        (if font-kern-table
            (%draw-char font c (- xpos (aref font-kern-table c)) ypos alu sheet)
          (%draw-char font c xpos ypos alu sheet))
        (when (eq (incf i) stop)
          (go finished))
        (go single-variable-width-font-loop)
   single-variable-width-font-tab
        (setq npos (+ (* (truncate (+ (setq xpos npos) tab-width) tab-width)
                         tab-width) inside-left))
        (when (> npos xlim) (return (values i xpos)))
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (go single-variable-width-font-loop)
   single-variable-width-font-hard
        (setq lozenge-string (or (car (rassq c si:xr-special-character-names))
                                 (format nil "~O" c)))
        (setq npos (+ (setq xpos npos) (lozenged-string-width lozenge-string)))
        (when (> npos xlim) (return (values i xpos)))
        (setf (sheet-cursor-x sheet) xpos)
        (sheet-display-lozenged-string sheet lozenge-string)
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (go single-variable-width-font-loop)

   single-hairy-font-loop
        (setq width (sheet-character-width sheet (setq c (zl:aref string i)) font))
        (when (> (+ (sheet-cursor-x sheet) width) xlim)
          (return (values i (sheet-cursor-x sheet))))
        (sheet-tyo sheet c font)
        (when (eq (incf i) stop)
          (return (values (1+ stop) (sheet-cursor-x sheet))))
        (go single-hairy-font-loop)

   multiple-font
        (setq font-next (%logldb #.%%ch-font (setq c (zl:aref string i))))
   multiple-font-main-loop-font-changed
        (setq font (aref font-map (setq font-index font-next)))
        (setq font-index-table (font-indexing-table font))
        (setq font-width-table (font-char-width-table font))
        (setq font-kern-table (font-left-kern-table font))
        (setq width (font-char-width font))
        (setq ypos (+ base-ypos (- (sheet-baseline sheet) (font-baseline font))))
   multiple-font-main-loop-font-unchanged
        (when ( (setq c (%logldb #.%%ch-char c)) #o200) (go multiple-font-special-character))

   multiple-font-graphic-character
        (when (> (setq npos (+ (setq xpos npos)
                               (if font-width-table (aref font-width-table c) width)))
                 xlim)
          (return (values i xpos)))
        (when (null font-index-table)
          (if font-width-table
              (if font-kern-table
                  (go multiple-font-variable-width-with-kerning-loop-short-cut)
                (go multiple-font-variable-width-loop-short-cut))
            (go multiple-font-fixed-width-loop-short-cut)))
        (setf (sheet-cursor-x sheet) xpos)      ;Must transmit latest position.
        (sheet-tyo sheet c font)                ;Indexed character (wider than 32 bits).
        (go multiple-font-loop-tail)

   multiple-font-special-character
        (case c
          (#/tab
           (when (> (setq npos (+ (setq xpos npos)
                                  (- tab-width
                                     (\ (- xpos inside-left)
                                        tab-width))))
                    xlim)
             (return (values i xpos))))
          (#/return
           (return (values i (setq xpos npos))))
          (otherwise
           (setq lozenge-string (or (car (rassq c si:xr-special-character-names))
                                    (format nil "~O" c)))
           (when (> (setq npos (+ (setq xpos npos)
                                  (lozenged-string-width lozenge-string)))
                    xlim)
             (return (values i xpos)))
           (setf (sheet-cursor-x sheet) xpos)
           (sheet-display-lozenged-string sheet lozenge-string)))

   multiple-font-loop-tail
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (if (eq (setq font-next (%logldb #.%%ch-font (setq c (zl:aref string i)))) font-index)
            (go multiple-font-main-loop-font-unchanged)
          (go multiple-font-main-loop-font-changed))

   multiple-font-fixed-width-loop
        (when ( (setq c (%logldb #.%%ch-char c)) #o200)
          (go multiple-font-special-character))
        (when (> (setq npos (+ (setq xpos npos) width))
                 xlim)
          (return (values i xpos)))
   multiple-font-fixed-width-loop-short-cut
        (%draw-char font c xpos ypos alu sheet)
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (if (eq (setq font-next (%logldb #.%%ch-font (setq c (zl:aref string i)))) font-index)
            (go multiple-font-fixed-width-loop)
          (go multiple-font-main-loop-font-changed))

   multiple-font-variable-width-loop
        (when ( (setq c (%logldb #.%%ch-char c)) #o200)
          (go multiple-font-special-character))
        (when (> (setq npos (+ (setq xpos npos)
                               (if font-width-table (aref font-width-table c) width)))
                 xlim)
          (return (values i xpos)))
   multiple-font-variable-width-loop-short-cut
        (%draw-char font c xpos ypos alu sheet)
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (if (eq (setq font-next (%logldb #.%%ch-font (setq c (zl:aref string i)))) font-index)
            (go multiple-font-variable-width-loop)
          (go multiple-font-main-loop-font-changed))

   multiple-font-variable-width-with-kerning-loop
        (when ( (setq c (%logldb #.%%ch-char c)) #o200)
          (go multiple-font-special-character))
        (when (> (setq npos (+ (setq xpos npos)
                               (if font-width-table (aref font-width-table c) width)))
                 xlim)
          (return (values i xpos)))
   multiple-font-variable-width-with-kerning-loop-short-cut
        (%draw-char font c (- xpos (aref font-kern-table c)) ypos alu sheet)
        (when (eq (incf i) stop) (return (values (1+ stop) npos)))
        (if (eq (setq font-next (%logldb #.%%ch-font (setq c (zl:aref string i)))) font-index)
            (go multiple-font-variable-width-with-kerning-loop)
          (go multiple-font-main-loop-font-changed))

        ))

(DEFMETHOD (SHEET :DISPLAY-CENTERED-STRING-EXPLICIT) (&REST ARGS)
  (APPLY #'SHEET-DISPLAY-CENTERED-STRING-EXPLICIT SELF ARGS))

(COMPILER:MAKE-OBSOLETE SHEET-DISPLAY-CENTERED-STRING-EXPLICIT
                        "use the :STRING-OUT-CENTERED-EXPLICIT operation with rearranged args")
(DEFUN SHEET-DISPLAY-CENTERED-STRING-EXPLICIT
       (SHEET STRING &OPTIONAL
        (LEFT (TV:SHEET-INSIDE-LEFT SHEET)) Y-POS
        (RIGHT (SHEET-INSIDE-RIGHT SHEET))
        (FONT (SHEET-CURRENT-FONT SHEET))
        (ALU (SHEET-CHAR-ALUF SHEET))
        (START 0) END
        (MULTI-LINE-LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
        Y-LIMIT)
  (SHEET-STRING-OUT-CENTERED-EXPLICIT SELF STRING LEFT Y-POS RIGHT Y-LIMIT
                                      FONT ALU START END MULTI-LINE-LINE-HEIGHT))

(DEFMETHOD (SHEET :STRING-OUT-CENTERED-EXPLICIT)
           (STRING &OPTIONAL
            (LEFT (SHEET-INSIDE-LEFT))
            (Y-POS (sheet-inside-top))
            (RIGHT (SHEET-INSIDE-RIGHT))
            (Y-LIMIT -1)
            (FONT CURRENT-FONT)
            (ALU CHAR-ALUF)
            (START 0) END
            (MULTI-LINE-LINE-HEIGHT LINE-HEIGHT))
  (SHEET-STRING-OUT-CENTERED-EXPLICIT SELF STRING LEFT Y-POS RIGHT Y-LIMIT
                                      FONT ALU START END MULTI-LINE-LINE-HEIGHT))

(DEFUN SHEET-STRING-OUT-CENTERED-EXPLICIT
       (SHEET STRING
        &OPTIONAL (LEFT (TV:SHEET-INSIDE-LEFT SHEET)) Y-POS
        (RIGHT (SHEET-INSIDE-RIGHT SHEET)) YLIM
        (FONT (SHEET-CURRENT-FONT SHEET))
        (ALU (SHEET-CHAR-ALUF SHEET))
        (START 0) END
        (MULTI-LINE-LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
        &AUX WID SWID)
  "Output STRING or portion on SHEET centered between LEFT and RIGHT.
LEFT and RIGHT are relative to SHEET's outside edge.
The string is truncated on the right if it is too wide.
Y-POS specifies the vertical position of the top of the output,
relative to SHEET's top margin.
FONT and ALU function are used, defaulting to SHEET's font and char-aluf.
START and END specify a portion of STRING to output; default is all.
MULTI-LINE-LINE-HEIGHT is the distance for Return to move down;
 then each line is centered individually.
 It can be NIL meaning output Return as <Return>.
 Default is sheet's line-height.

SHEET's cursor is not used or moved."
  (IF FONT
      (COERCE-FONT FONT SHEET)
      (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (OR ALU (SETQ ALU ALU-IOR))
  (SETQ WID (- RIGHT LEFT)
        STRING (STRING STRING))
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (DO ((BEG START)) ((>= BEG END))
    (LET ((STOP (OR (STRING-SEARCH-CHAR #/RETURN STRING BEG END) END)))
      (MULTIPLE-VALUE (NIL NIL NIL SWID)  ;Compute how wide the string is, and whether to truncate
        (SHEET-COMPUTE-MOTION SHEET 0 0 STRING BEG STOP NIL
                              1.0s10 1.0s10 1.0s10 1.0s10 FONT))
      (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING
                                   (+ LEFT (MAX (TRUNCATE (- WID SWID) 2) 0))
                                   Y-POS
                                   RIGHT NIL FONT ALU BEG STOP)
      (IF (OR (NULL MULTI-LINE-LINE-HEIGHT)
              ( (+ Y-POS MULTI-LINE-LINE-HEIGHT) YLIM))
          (RETURN NIL))
      (INCF Y-POS MULTI-LINE-LINE-HEIGHT)
      (SETQ BEG (1+ STOP)))))

(DEFMETHOD (SHEET :STRING-OUT-CENTERED) (&REST ARGS)
  (APPLY #'SHEET-DISPLAY-CENTERED-STRING SELF ARGS))

(DEFMETHOD (SHEET :DISPLAY-CENTERED-STRING) (&REST ARGS)
  (APPLY #'SHEET-DISPLAY-CENTERED-STRING SELF ARGS))

;;; This function displays a string centered between two X coordinates, truncated if necessary
;;; The arguments are relative to the margins, as usual.
(DEFUN SHEET-DISPLAY-CENTERED-STRING (SHEET STRING
                                      &OPTIONAL (LEFT 0) (RIGHT (SHEET-INSIDE-WIDTH SHEET))
                                                (Y-POS (- (SHEET-CURSOR-Y SHEET)
                                                          (SHEET-INSIDE-TOP SHEET)))
                                      &AUX WID SWID SLEN)
  "Output STRING on SHEET centered between LEFT and RIGHT.
LEFT and RIGHT are relative to SHEET's margin.
Y-POS specifies the vertical position of the top of the output,
relative to SHEET's top margin.  The output may be multiple lines.
SHEET's current font, alu function and line height are used.
SHEET's cursor is left at the end of the string."
  (SETQ WID (- RIGHT LEFT)
        STRING (STRING STRING))
  (MULTIPLE-VALUE (SWID SLEN)  ;Compute how wide the string is, and whether to truncate
     (SHEET-STRING-LENGTH SHEET STRING 0 NIL WID))
  ;; SHEET-SET-CURSORPOS takes arguments in a different coordinate system
  (SHEET-SET-CURSORPOS SHEET (+ LEFT (MAX (TRUNCATE (- WID SWID) 2) 0)) Y-POS)
  (SHEET-STRING-OUT SHEET STRING 0 SLEN))

(DEFMETHOD (SHEET :STRING-OUT-X-Y-CENTERED-EXPLICIT) (&REST ARGS)
  (APPLY #'SHEET-DISPLAY-X-Y-CENTERED-STRING SELF ARGS))

(DEFMETHOD (SHEET :DISPLAY-X-Y-CENTERED-STRING) (&REST ARGS)
  (APPLY #'SHEET-DISPLAY-X-Y-CENTERED-STRING SELF ARGS))

;; Copied from LAD: RELEASE-3.WINDOW; SHWARM.LISP#365 on 2-Oct-86 04:09:57
(DEFUN SHEET-DISPLAY-X-Y-CENTERED-STRING (SHEET STRING
                                          &OPTIONAL
                                          (LEFT (SHEET-INSIDE-LEFT SHEET))
                                          (TOP (SHEET-INSIDE-TOP SHEET))
                                          (RIGHT (SHEET-INSIDE-RIGHT SHEET))
                                          (BOTTOM (SHEET-INSIDE-BOTTOM SHEET))
                                          (FNT (SHEET-CURRENT-FONT SHEET))
                                          (ALU (SHEET-CHAR-ALUF SHEET))
                                          (START 0) END
                                          (MULTI-LINE-LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET)))
  "Display STRING on SHEET centered in both X and Y, in font FNT.
It is centered horizontally between LEFT and RIGHT,
vertically between TOP and BOTTOM.
All four coordinates are relative to SHEET's outside edges.
SHEET's cursor is not used or moved."
  (LET ((WID (- RIGHT LEFT)))
    (MULTIPLE-VALUE-BIND (NIL SHEI SLEN SWID)
        (SHEET-COMPUTE-MOTION SHEET 0 0 STRING START END nil ;don't count last interline space
                              0 (sheet-inside-height sheet)
                              #o30000000 NIL FNT MULTI-LINE-LINE-HEIGHT)
;       (SHEET-STRING-LENGTH SHEET STRING START END WID FNT)
      (incf shei (font-char-height fnt))
      (UNLESS (NUMBERP SLEN) (SETQ SLEN NIL))
      (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING
                                   (+ LEFT (MAX (TRUNCATE (- WID SWID) 2) 0))
                                   (MAX (truncate (- (+ top bottom) shei)
                                                  2)
                                        TOP)
                                   RIGHT BOTTOM
                                   FNT ALU START SLEN MULTI-LINE-LINE-HEIGHT))))

;;; Most screens can be seen by the "user"
(DEFMETHOD (SCREEN :USER-VISIBLE) ()
  T)

;;; A mixin that causes inferiors to be scaled when the size of the window changes
;;; and propagates changes in the default font.
;;; TIME-STAMP is (as for any sheet), the time-stamp for comparison with this sheet's superior
;;; CURRENT-TIME-STAMP is the stamp which propagates down into our inferiors.  If
;;; an inferior's TIME-STAMP is EQ to our CURRENT-TIME-STAMP, then the inferior is
;;; up to date.  Otherwise we compare the two stamps and resolve the differences.
;;; This comparison happens to the active inferiors when our stamp changes and
;;; to any newly-activated inferior.
;;; This mixin is the only thing which knows the format of time stamps (other than
;;; that they are compared with EQ).  A time stamp is a list which represents
;;; the state of a window that has this mixin:
;;;     (serial-number our-inside-width our-inside-height default-font)
;;; serial-number is incremented every time a new time stamp is generated, and is
;;; only there for human beings looking at the stamps.
;;; Other elements may be added as needed.
(DEFFLAVOR SCALE-INFERIORS-MIXIN (CURRENT-TIME-STAMP) ()
  (:REQUIRED-FLAVORS SHEET)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-TIME-STAMP))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :AFTER :INIT) (IGNORE)
  (SETQ CURRENT-TIME-STAMP
        (LIST 0 (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
              (LOOP FOR ELT IN (SCREEN-FONT-ALIST (SHEET-GET-SCREEN SELF))
                    APPEND (LIST (CAR ELT) (FONT-EVALUATE (CDR ELT)))))))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :INFERIOR-TIME-STAMP) (INF)
  INF                                           ;Inferiors all have same time stamp
  CURRENT-TIME-STAMP)

(DEFUN SCALE-INFERIORS-MIXIN-UPDATE-INFERIOR (INFERIOR)
  (DECLARE (:SELF-FLAVOR SCALE-INFERIORS-MIXIN))
  (LET ((INF-TIME-STAMP (SHEET-TIME-STAMP INFERIOR)))
    (COND ((NEQ INF-TIME-STAMP CURRENT-TIME-STAMP)
           (LET ((OLD-FONT (FOURTH (SHEET-TIME-STAMP INFERIOR)))
                 (NEW-FONT (FOURTH CURRENT-TIME-STAMP)))
             (OR (EQUAL OLD-FONT NEW-FONT)
                 (SEND INFERIOR :CHANGE-OF-DEFAULT-FONT NIL NIL)))
           (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR INFERIOR NIL INF-TIME-STAMP)))))

(DEFUN SCALE-INFERIORS-MIXIN-SCALE-INFERIOR (INFERIOR EXPOSE
                                &OPTIONAL (INF-TIME-STAMP (SHEET-TIME-STAMP INFERIOR)))
  (DECLARE (:SELF-FLAVOR SCALE-INFERIORS-MIXIN))
  (OR (EQ CURRENT-TIME-STAMP INF-TIME-STAMP)
      ;; Hasn't had edges set in the current time slice, so set them
      (LET* ((SIZE-LAST-TIME (CDR INF-TIME-STAMP))
             (NEW-LEFT (TRUNCATE (* (SHEET-X-OFFSET INFERIOR)
                                    (SHEET-INSIDE-WIDTH))
                                 (FIRST SIZE-LAST-TIME)))
             (NEW-TOP (TRUNCATE (* (SHEET-Y-OFFSET INFERIOR)
                                   (SHEET-INSIDE-HEIGHT))
                                (SECOND SIZE-LAST-TIME)))
             (NEW-WIDTH (TRUNCATE (* (SHEET-WIDTH INFERIOR)
                                     (SHEET-INSIDE-WIDTH))
                                  (FIRST SIZE-LAST-TIME)))
             (NEW-HEIGHT (TRUNCATE (* (SHEET-HEIGHT INFERIOR)
                                      (SHEET-INSIDE-HEIGHT))
                                   (SECOND SIZE-LAST-TIME))))
        (COND ((AND (= (SHEET-X-OFFSET INFERIOR) NEW-LEFT)
                    (= (SHEET-Y-OFFSET INFERIOR) NEW-TOP)
                    (= (SHEET-WIDTH INFERIOR) NEW-WIDTH)
                    (= (SHEET-HEIGHT INFERIOR) NEW-HEIGHT))
               (SETQ NEW-LEFT NIL))
              ((NOT (SEND INFERIOR :SET-EDGES
                                   NEW-LEFT NEW-TOP
                                   (+ NEW-LEFT NEW-WIDTH) (+ NEW-TOP NEW-HEIGHT)
                                   :VERIFY))
               ;; Won't go, try not to change size
               (SETQ NEW-WIDTH (SHEET-WIDTH INFERIOR)
                     NEW-HEIGHT (SHEET-HEIGHT INFERIOR))
               (AND (> (+ NEW-WIDTH NEW-LEFT) (SHEET-INSIDE-RIGHT))
                    (SETQ NEW-LEFT (- (SHEET-INSIDE-RIGHT) NEW-WIDTH)))
               (AND (> (+ NEW-HEIGHT NEW-TOP) (SHEET-INSIDE-BOTTOM))
                    (SETQ NEW-TOP (- (SHEET-INSIDE-BOTTOM) NEW-HEIGHT)))
               (OR (SEND INFERIOR :SET-EDGES
                                  NEW-LEFT NEW-TOP
                                  (+ NEW-LEFT NEW-WIDTH) (+ NEW-TOP NEW-HEIGHT)
                                  :VERIFY)
                   ;; Won't go, don't change size at all
                   (SETQ NEW-LEFT NIL))))
        (COND (NEW-LEFT
               (SEND INFERIOR :SET-EDGES
                              NEW-LEFT NEW-TOP
                              (+ NEW-LEFT NEW-WIDTH) (+ NEW-TOP NEW-HEIGHT))
               (AND EXPOSE (SEND INFERIOR :EXPOSE)))
              (T (SEND INFERIOR :UPDATE-TIME-STAMP))))))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :BEFORE :INFERIOR-ACTIVATE) (INFERIOR)
  ;Catch up with any changes that happened while we were inactive
  (SCALE-INFERIORS-MIXIN-UPDATE-INFERIOR INFERIOR)
  INFERIOR)

(DEFWRAPPER (SCALE-INFERIORS-MIXIN :CHANGE-OF-SIZE-OR-MARGINS) (IGNORE . BODY)
  `(DELAYING-SCREEN-MANAGEMENT
     (LET ((OLD-EXP-INFS (REVERSE EXPOSED-INFERIORS)))
       (DOLIST (I EXPOSED-INFERIORS)
         (SEND I :DEEXPOSE))
       ,@BODY
       (SETQ CURRENT-TIME-STAMP
             (LIST (1+ (CAR CURRENT-TIME-STAMP))
                   (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
                   (fourth current-time-stamp)))
       (DOLIST (I OLD-EXP-INFS)
         (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR I T))
       (DOLIST (I INFERIORS)
         (OR (MEMQ I EXPOSED-INFERIORS)
             (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR I NIL))))))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :BEFORE :CHANGE-OF-DEFAULT-FONT) (IGNORE IGNORE)
  (SETQ CURRENT-TIME-STAMP
        (LIST (1+ (CAR CURRENT-TIME-STAMP))
              (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
              (LOOP FOR ELT IN (SCREEN-FONT-ALIST (SHEET-GET-SCREEN SELF))
                    APPEND (LIST (CAR ELT) (FONT-EVALUATE (CDR ELT)))))))

(DEFVAR KILL-RECURSION NIL
  "Non-NIL if killing the inferiors of another window being killed.")

;; Kill the processes only after any :after :kill daemons have run.
(DEFWRAPPER (SHEET :KILL) (IGNORE . BODY)
  `(LET ((PROCESSES (UNLESS KILL-RECURSION (SEND SELF :PROCESSES)))
         (KILL-RECURSION T))
     ,@BODY
     (KILL-PROCESSES PROCESSES)))

(DEFMETHOD (SHEET :KILL) ()
  (SEND SELF :DEACTIVATE)
  (DOLIST (I (COPY-LIST INFERIORS))
    (SEND I :KILL)))

(DEFUN KILL-PROCESSES (PROCESSES)
  (DOLIST (P PROCESSES)
    (UNLESS (OR (NULL P) (EQ P CURRENT-PROCESS))
      (SEND P :KILL)))
  (IF (MEMQ CURRENT-PROCESS PROCESSES)
      (SEND CURRENT-PROCESS :KILL)))

;; Uses :APPEND method combination and returns a list of processes to be killed.
(DEFMETHOD (SHEET :PROCESSES) ()
  (LOOP FOR I IN (COPY-LIST INFERIORS)
        NCONC (SEND I :PROCESSES)))

;;; This is so that default mouse handler, when executing for a window that's not selectable
;;; by the mouse, and having a screen for an superior, will not cause the mouse process to
;;; send a :SELECT message to that screen.
(defmethod (sheet :default :self-or-substitute-selected-p) ()
  t)


(DEFFLAVOR STANDARD-SCREEN () (SCALE-INFERIORS-MIXIN SCREEN))

;;; Before making our first screen, compile any methods it requires
(COMPILE-FLAVOR-METHODS SCREEN STANDARD-SCREEN)

;;; This height may get hacked by the who-line making code if the wholine ends up
;;; at the bottom of the main screen (which it usually does!)
(DEFVAR MAIN-SCREEN-WIDTH
  (SELECT-PROCESSOR
    (:CADR 768.)
    (:LAMBDA ;; 800. ;; portrait
             1024.   ;; landscape
             )
    (:EXPLORER 1024.)))

(DEFVAR MAIN-SCREEN-HEIGHT
  (SELECT-PROCESSOR
    (:CADR 963.)
    (:LAMBDA ;; 1020. ;; portrait
             796.     ;; landscape
             )
    (:EXPLORER 804.)))

(DEFVAR MAIN-SCREEN-LOCATIONS-PER-LINE
  (SELECT-PROCESSOR
    (:CADR 24.)
    (:LAMBDA 32.)
    (:EXPLORER 32.)))

(DEFCONST MAIN-SCREEN-BUFFER-ADDRESS IO-SPACE-VIRTUAL-ADDRESS)
(DEFCONST MAIN-SCREEN-CONTROL-ADDRESS 377760)
(DEFCONST MAIN-SCREEN-BUFFER-LENGTH 100000)

;;;Set things up
(DEFUN INITIALIZE ()
  (SHEET-CLEAR-LOCKS)
  (WHO-LINE-SETUP)
  ;; Set up screen and sheet for the main monitor (CPT typically)
  (OR MAIN-SCREEN
      (SETQ MAIN-SCREEN
            (DEFINE-SCREEN 'STANDARD-SCREEN "Main Screen"
              :BUFFER MAIN-SCREEN-BUFFER-ADDRESS
              :CONTROL-ADDRESS MAIN-SCREEN-CONTROL-ADDRESS
              :PROPERTY-LIST '(:VIDEO :BLACK-AND-WHITE :CONTROLLER :SIMPLE)
              :HEIGHT (- MAIN-SCREEN-HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN))
              :WIDTH MAIN-SCREEN-WIDTH
              :LOCATIONS-PER-LINE MAIN-SCREEN-LOCATIONS-PER-LINE)))
  (SETQ MOUSE-SHEET MAIN-SCREEN)
  (SETQ DEFAULT-SCREEN MAIN-SCREEN
        INHIBIT-SCREEN-MANAGEMENT NIL
        SCREEN-MANAGER-TOP-LEVEL T
        SCREEN-MANAGER-QUEUE NIL))

(DEFUN DEFINE-SCREEN (FLAVOR NAME &REST ARGS)
  (LET ((SCREEN (APPLY #'MAKE-INSTANCE FLAVOR :NAME NAME ARGS)))
    (PUSH SCREEN ALL-THE-SCREENS)
    (SEND SCREEN :EXPOSE)
    SCREEN))

(DEFVAR MAIN-SCREEN-AND-WHO-LINE NIL)
(DEFUN MAIN-SCREEN-AND-WHO-LINE ()
  (IF MAIN-SCREEN-AND-WHO-LINE
      (SI:CHANGE-INDIRECT-ARRAY MAIN-SCREEN-AND-WHO-LINE (SHEET-ARRAY-TYPE MAIN-SCREEN)
                                (IF ARRAY-INDEX-ORDER
                                    (LIST MAIN-SCREEN-HEIGHT
                                          (* (send main-screen :locations-per-line) 32.))
                                  (LIST (* (send main-screen :locations-per-line) 32.)
                                        MAIN-SCREEN-HEIGHT))
                                (SCREEN-BUFFER MAIN-SCREEN) NIL)
    (SETQ MAIN-SCREEN-AND-WHO-LINE
          (MAKE-PIXEL-ARRAY (* (send main-screen :locations-per-line) 32.) MAIN-SCREEN-HEIGHT
                            :area sheet-area
                            :TYPE (SHEET-ARRAY-TYPE MAIN-SCREEN)
                            :DISPLACED-TO (SCREEN-BUFFER MAIN-SCREEN))))
  MAIN-SCREEN-AND-WHO-LINE)

(DEFVAR INITIAL-LISP-LISTENER)

;This function is called from an initialization in COMETH
(DEFUN WINDOW-INITIALIZE (&AUX FIRST-TIME)
  (INITIALIZE)
  (DOLIST (S ALL-THE-SCREENS)
    (SEND S :EXPOSE))
  (SETQ KBD-TYI-HOOK NIL PROCESS-IS-IN-ERROR NIL)
  (OR (EQ WHO-LINE-PROCESS SI:INITIAL-PROCESS)  ;So it stays latched here during loading
      (SETQ WHO-LINE-PROCESS NIL))
  (OR INITIAL-LISP-LISTENER     ;Set to NIL in LTOP
      (SETQ INITIAL-LISP-LISTENER (MAKE-INSTANCE 'LISP-LISTENER :PROCESS SI:INITIAL-PROCESS)
            FIRST-TIME T))
  (SEND INITIAL-LISP-LISTENER :SELECT)
  (WHEN FIRST-TIME (SETQ *TERMINAL-IO* INITIAL-LISP-LISTENER))
  (OR (MEMQ 'TV:BLINKER-CLOCK SI:CLOCK-FUNCTION-LIST)
      (PUSH 'TV:BLINKER-CLOCK SI:CLOCK-FUNCTION-LIST)))

;;; Lambda scan line table hacking.

(DEFCONST SCAN-LINE-TABLE-BEGIN #16R6000)
(DEFCONST SCAN-LINE-TABLE-LENGTH (// #16R1000 4))

(DEFUN READ-SCAN-LINE-TABLE (ADR)
  (%NUBUS-READ tv:tv-quad-slot (+ (* ADR 4) SCAN-LINE-TABLE-BEGIN)))

(DEFUN WRITE-SCAN-LINE-TABLE (ADR DATA)
  (%NUBUS-WRITE tv:tv-quad-slot (+ (* ADR 4) SCAN-LINE-TABLE-BEGIN) DATA))

(DEFUN LOAD-SCAN-LINE-TABLE (WORDS-PER-LINE)
  (DO ((LINE-NUMBER 0 (1+ LINE-NUMBER))
       (BIT-MAP-POINTER 0 (+ BIT-MAP-POINTER (* 2 WORDS-PER-LINE))))
      ((>= LINE-NUMBER SCAN-LINE-TABLE-LENGTH) ())
    (WRITE-SCAN-LINE-TABLE LINE-NUMBER BIT-MAP-POINTER)))

(DEFUN SET-UP-SCAN-LINE-TABLE ()
  (select-processor
    (:lambda
      (case si::video-board-type
        (:vcmem
         (LOAD-SCAN-LINE-TABLE (SHEET-LOCATIONS-PER-LINE MAIN-SCREEN)))))
    ((:cadr :explorer)))
  (INITIALIZE-RUN-LIGHT-LOCATIONS))

(ADD-INITIALIZATION "Load scan line table" '(SET-UP-SCAN-LINE-TABLE))

;;; Try to adapt to the proliferation of incompatible video boards.

(DEFUN SET-CONSOLE-SIZE (WIDTH HEIGHT)
  (DELAYING-SCREEN-MANAGEMENT
    (WITH-MOUSE-USURPED
      (LOCK-SHEET (MAIN-SCREEN)
        (LOCK-SHEET (WHO-LINE-SCREEN)
          (WITHOUT-INTERRUPTS
            (LET ((MS MOUSE-SHEET) (SW SELECTED-WINDOW))
              (WHEN (SHEET-ME-OR-MY-KID-P MS MAIN-SCREEN) (SETQ MOUSE-SHEET NIL))
              (SEND WHO-LINE-SCREEN :DEEXPOSE)
              (SEND MAIN-SCREEN :DEEXPOSE)
              (INITIALIZE-RUN-LIGHT-LOCATIONS)
              (SI::CLEAR-SCREEN-BUFFER SYS:IO-SPACE-VIRTUAL-ADDRESS)
              (SETQ MAIN-SCREEN-HEIGHT HEIGHT)
              (SETQ MAIN-SCREEN-WIDTH WIDTH)
              (SEND WHO-LINE-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                                    :WIDTH WIDTH
                                    :TOP (- HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
              (SEND MAIN-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                                :WIDTH WIDTH
                                :HEIGHT (- HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
              (COND ((> WIDTH 800.)
                     (SEND WHO-LINE-RUN-STATE-SHEET
                           :CHANGE-OF-SIZE-OR-MARGINS :LEFT 328. :RIGHT 520.)
                     (SEND WHO-LINE-FILE-STATE-SHEET
                           :CHANGE-OF-SIZE-OR-MARGINS :LEFT 520. :RIGHT width)
                     (SEND WHO-LINE-DOCUMENTATION-WINDOW
                           :CHANGE-OF-SIZE-OR-MARGINS :WIDTH width))
                    (T
                     (SEND WHO-LINE-RUN-STATE-SHEET
                           :CHANGE-OF-SIZE-OR-MARGINS :LEFT 328. :RIGHT 480.)
                     (SEND WHO-LINE-FILE-STATE-SHEET
                           :CHANGE-OF-SIZE-OR-MARGINS :LEFT 480. :RIGHT width)
                     (SEND WHO-LINE-DOCUMENTATION-WINDOW :CHANGE-OF-SIZE-OR-MARGINS :WIDTH width)))
              (SEND WHO-LINE-SCREEN :EXPOSE)
              (SEND MAIN-SCREEN :EXPOSE)
              (MOUSE-SET-SHEET MAIN-SCREEN)
              (WHEN SW (SEND SW :SELECT)))))))
    (DOLIST (RESOURCE-NAME WINDOW-RESOURCE-NAMES)
      ;; this loses for things like pop-up-finger-window which we would like to be
      ;;  full screen width on all monitor configurations.
      (SI:MAP-RESOURCE
        (LAMBDA (WINDOW &REST IGNORE)
          (OR (TYPEP WINDOW 'INSTANCE) (FERROR "Lossage"))
          (IF (TYPEP WINDOW 'TV:BASIC-MENU)
              (LET ((GEO (SEND WINDOW :GEOMETRY)))
                (DO ((L GEO (CDR L))) ((NULL L))
                  (SETF (CAR L) NIL)))
            (LET* ((SUPERIOR (SEND WINDOW :SUPERIOR))
                   (BOTTOM (SEND WINDOW :HEIGHT))
                   (SUPHEIGHT (OR (SEND-IF-HANDLES SUPERIOR :INSIDE-HEIGHT)
                                  (SEND SUPERIOR :HEIGHT))))
              (IF (> BOTTOM SUPHEIGHT)
                  (SEND WINDOW :SET-SIZE (SEND WINDOW :WIDTH) SUPHEIGHT)))))
        RESOURCE-NAME)))
  (INITIALIZE-RUN-LIGHT-LOCATIONS)      ;everything is really the right size now.
  T)

(DEFUN CONFIGURE-CONSOLE (TYPE)
  "Configure windows on primary console for :PORTRAIT, :LANDSCAPE, or :EXPLORER monitors."
  (ECASE TYPE
    (:PORTRAIT
     (SET-CONSOLE-SIZE 800. 1020.))
    (:LANDSCAPE
     (SET-CONSOLE-SIZE 1024. 796.))
    (:EXPLORER
     (SET-CONSOLE-SIZE 1024. 804.))))

(DEFUN LANDSCAPE ()
  "Configure all existing (primary) screens and windows for landscape monitor."
  (CONFIGURE-CONSOLE :LANDSCAPE))

(DEFUN PORTRAIT ()
  "Configure all existing (primary) screens and windows for portrait monitor."
  (CONFIGURE-CONSOLE :PORTRAIT))

;;;

(DEFUN SET-DEFAULT-FONT (FONT-DESCRIPTOR)
  "Make FONT-DESCRIPTOR be the default font.
All windows that were set up to /"use the default font/"
will presently be using FONT-DESCRIPTOR.
FONT-DESCRIPTOR should be a font descriptor, such as the name of a font."
  (SET-STANDARD-FONT :DEFAULT FONT-DESCRIPTOR))

(DEFUN SET-STANDARD-FONT (PURPOSE FONT-DESCRIPTOR)
  "Make FONT-DESCRIPTOR be the standard font for purpose PURPOSE.
All windows that were set up to /"use the font for PURPOSE/"
will presently be using FONT-DESCRIPTOR.
FONT-DESCRIPTOR should be a font descriptor, such as the name of a font."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (SET-SCREEN-STANDARD-FONT SCREEN PURPOSE FONT-DESCRIPTOR)))

(DEFUN SET-SCREEN-STANDARD-FONT (SCREEN PURPOSE FONT-DESCRIPTOR)
  "Make FONT-DESCRIPTOR be the standard font for purpose PURPOSE on SCREEN.
All windows on SCREEN that were set up to /"use the font for PURPOSE/"
will presently be using FONT-DESCRIPTOR."
  ;; Make absolutely sure we really have a font,
  ;; since if we don't this will wedge everything.
  (LET ((FONT (SEND SCREEN :PARSE-FONT-DESCRIPTOR FONT-DESCRIPTOR))
        FONT-NAME)
    (CHECK-TYPE FONT FONT)
    (SETQ FONT-NAME (FONT-NAME FONT))
    (LET* ((STANDARD-NAME (SEND SCREEN :FONT-NAME-FOR PURPOSE T))
           (OLD-FONT (SYMBOL-VALUE STANDARD-NAME)))
      ;; If the screen has no entry for PURPOSE on its font alist,
      ;; :FONT-NAME-FOR returns T because we supplied that as the default.
      ;; We do that so we can avoid clobbering the font names on the
      ;; DEFAULT-FONT-ALIST.
      (COND ((NEQ STANDARD-NAME T)
             (SET STANDARD-NAME FONT-NAME)
             (IF (EQ PURPOSE :DEFAULT)
                 (SEND SCREEN :CHANGE-OF-DEFAULT-FONT
                              (FONT-EVALUATE OLD-FONT) (FONT-EVALUATE FONT-NAME))
               (SEND SCREEN :CHANGE-OF-DEFAULT-FONT NIL NIL)))))))

;;; Random stuff to support the explorer beeper.

(DefConst Beep-Frequency 1000)                  ; Hz
(DefConst Beep-Time 7.)                 ; 60ths of a second

(DefConst Speaker-Enable 400)
(DefConst Speaker-Disable 0)

;;; The following constants relate to the sound register address field.
(DefConst TONE-1            0)                ;  set up tone register 1
(DefConst ATTENUATION-1     1)                ;  2 db Attenuation
(DefConst TONE-2            2)                ;  set up tone register 2
(DefConst ATTENUATION-2     3)                ;  6 db Attenuation
(DefConst TONE-3            4)                ;  set up tone register 3
(DefConst ATTENUATION-3     5)                ; 10 db Attenuation
(DefConst NOISE-CONTROL     6)
(DefConst NOISE-ATTENUATION 7)

(DefConst ATTENUATE-SILENT  #o17)             ; turn it off.
(DefConst ATTENUATE-LOUD    0)                ; Full Blast !!!

(DefConst %sib-monitor-speaker-enable #xF2000C)
(DefConst %SOUND-CONTROL-REGISTER-OFFSET #xF20014)

(DefConst %%Sound-Reg-Select 403)
(DefConst %%Sound-Command 701)
(DefConst %%Sound-First-Data 4)
(DefConst %%Sound-Second-Data 406)

(DEFUN WRITE-SOUND-REG (REG VAL &OPTIONAL (USE-SECOND-VAL NIL))
  (%NUBUS-WRITE tv-quad-slot %SOUND-CONTROL-REGISTER-OFFSET
                (DPB REG %%SOUND-REG-SELECT                   ; set up selected register
                     (DPB 1 %%SOUND-COMMAND                ; set sound first word data bit
                          (LDB %%SOUND-FIRST-DATA VAL))))     ; write first word
  (IF USE-SECOND-VAL
      (%NUBUS-WRITE tv-quad-slot %SOUND-CONTROL-REGISTER-OFFSET
                    (LDB %%SOUND-SECOND-DATA VAL))))

(Defun Explorer-Beep ()
  (Labels ((convert-freq (freq)
            (fix (quotient 2048000. (float (* 32. (max 4. (min 16000. freq))))))))
    (Write-Sound-Reg tone-1 (convert-freq beep-frequency) T)
    (Write-Sound-Reg Attenuation-1 Attenuate-Loud)
    (process-sleep Beep-Time "Beep")
    (Write-Sound-Reg Attenuation-1 Attenuate-Silent))
  )

(Defun Initialize-Sound ()
  (select-processor
    ((:lambda :cadr))
    (:explorer
      (Write-Sound-Reg Attenuation-1 Attenuate-Silent)
      (Write-Sound-Reg Attenuation-2 Attenuate-Silent)
      (Write-Sound-Reg Attenuation-3 Attenuate-Silent)
      (Write-Sound-Reg Noise-Attenuation Attenuate-Silent)
      (%nubus-write tv-quad-slot %SIB-MONITOR-SPEAKER-ENABLE Speaker-Enable)))
  )

(add-initialization "Initialize Sound" '(initialize-sound) '(now))
