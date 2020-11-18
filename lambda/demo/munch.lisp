;;; -*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-

;;; The switch register uses the TOG font to find its characters.  There is a pair
;;; of lights at 101 and 102, and a pair of switches at 60 and 61.  They are followed
;;; immediately by other pairs (at 103 and 104, and 62 and 63, repectively) by
;;; the same thing in the other color.

(DEFFLAVOR SWITCH-REGISTER-MIXIN
        (N-SWITCHES                             ; Number of switches in this register
         START-X                                ; X-position of first switch
         START-Y                                ; Y-position of first switch
         X-SPACING                              ; Spacing between switches (including switch)
         (VALUE 0)                              ; Current value of switch
         (STATE NIL)                            ; Value being displayed, NIL -> bashed.
         (CHAR-ORIGIN #o60)                     ; Where in font to find the two chars
         MOUSE-BLINKER                          ; Blinker for mouse-sensitivity
         (COLOR-PATTERN NIL)                    ; Which color to make the switches
         )
        ()
  (:INCLUDED-FLAVORS TV:WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES N-SWITCHES VALUE)
  (:INITABLE-INSTANCE-VARIABLES N-SWITCHES VALUE)
  (:SETTABLE-INSTANCE-VARIABLES COLOR-PATTERN)
  (:INIT-KEYWORDS :SWITCHES :LIGHTS :OCTAL :HEX :RADIX)
  )

;;; Get the least significant bit of a number.
(DEFSUBST LOW-BIT (X)
  (LDB (BYTE 1 0) X))

;;; This is the method to update the screen, given the
;;; old STATE and new VALUE.  It draws characters and updates STATE.
(DEFMETHOD (SWITCH-REGISTER-MIXIN :UPDATE) ()
  ;; Loop from right to left, from least significant bit up.
  (DO ((X (+ START-X (* (1- N-SWITCHES) X-SPACING)) (- X X-SPACING))
       (V VALUE (LSH V -1))
       (C COLOR-PATTERN (LSH C -1))
       (S STATE)
       (I 0 (1+ I)))
      (( I N-SWITCHES))
    ;; The character should be redisplayed if STATE is NIL, or if the bit in
    ;; STATE is different from the bit in VALUE.
    (LET ((V-BIT (LOW-BIT V))                   ; Current bit of VALUE.
          (C-BIT (LOW-BIT C)))                  ; Current bit of COLOR-PATTERN.
      (IF (OR (NULL S)
              ( V-BIT (PROG1 (LOW-BIT S) (SETQ S (LSH S -1)))))
          ;; Character needs to be redrawn.
          (SEND SELF :PLUNK-CHAR
                     FONTS:TOG
                     (AND S (+ CHAR-ORIGIN (LOW-BIT S) (* 2 C-BIT)))
                     (+ CHAR-ORIGIN V-BIT (* 2 C-BIT))
                     X
                     START-Y))))
  (SETQ STATE VALUE))

(DEFUN WIDTH-OF-CHARACTER (FONT CHAR)
  (LET ((TABLE (TV:FONT-CHAR-WIDTH-TABLE FONT)))
    (IF TABLE (AREF TABLE CHAR) (TV:FONT-CHAR-WIDTH FONT))))

;;; This function is like :DRAW-CHAR, but it does not overwrite.  It erases
;;; the old character at the location (except if OLD-CHAR is NIL it does
;;; not erase anything), and draws the new character.
(DEFMETHOD (SWITCH-REGISTER-MIXIN :PLUNK-CHAR) (FONT OLD-CHAR NEW-CHAR X Y)
  (WHEN OLD-CHAR
    (SEND SELF :DRAW-RECTANGLE
               (WIDTH-OF-CHARACTER FONT OLD-CHAR)
               (TV:FONT-CHAR-HEIGHT FONT)
               X Y
               TV:ERASE-ALUF))
  (SEND SELF :DRAW-CHAR FONT NEW-CHAR X Y))

;;; This function deduces the internal spacing given the size of the
;;; window and N-SWITCHES.  You give it the new width and height,
;;; and it returns the start-x, start-y, and x-spacing, or else NIL
;;; if the width or height is unacceptable.
(DEFUN DEDUCE-SWITCH-REGISTER-SPACING (WIDTH HEIGHT)
  (DECLARE (:SELF-FLAVOR SWITCH-REGISTER-MIXIN))
  (LET ((CHAR-WIDTH (WIDTH-OF-CHARACTER FONTS:TOG CHAR-ORIGIN))
        (CHAR-HEIGHT (TV:FONT-CHAR-HEIGHT FONTS:TOG))
        (CELL-WIDTH (TRUNCATE WIDTH N-SWITCHES))
        (CELL-HEIGHT HEIGHT))
    (IF (AND (< CHAR-WIDTH CELL-WIDTH)
             (< CHAR-HEIGHT CELL-HEIGHT))
        (PROG () (RETURN (VALUES (TRUNCATE (- CELL-WIDTH CHAR-WIDTH) 2)
                                 (TRUNCATE (- CELL-HEIGHT CHAR-HEIGHT) 2)
                                 CELL-WIDTH)))
        NIL)))

(DEFUN SET-SWITCH-REGISTER-SPACING ()
  (DECLARE (:SELF-FLAVOR SWITCH-REGISTER-MIXIN))
  (MULTIPLE-VALUE-BIND (INSIDE-WIDTH INSIDE-HEIGHT)
      (SEND SELF :INSIDE-SIZE)
    (MULTIPLE-VALUE-BIND (SX SY XS)
        (DEDUCE-SWITCH-REGISTER-SPACING INSIDE-WIDTH INSIDE-HEIGHT)
      (COND ((NOT (NULL SX))
             (SETQ START-X SX START-Y SY X-SPACING XS))))))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (SET-SWITCH-REGISTER-SPACING))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :VERIFY-NEW-EDGES) (IGNORE IGNORE WIDTH HEIGHT)
  (IF (DEDUCE-SWITCH-REGISTER-SPACING WIDTH HEIGHT)
      NIL
      "Not enough room for that many switches."))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :AFTER :INIT) (INIT-PLIST)
  (IF (NOT (BOUNDP 'FONTS:TOG))
      (LOAD "SYS:FONTS;TOG" :PACKAGE "FONTS"))
  (IF (GET INIT-PLIST ':LIGHTS)
      (SETQ CHAR-ORIGIN 101))
  (SETQ MOUSE-BLINKER (TV:MAKE-BLINKER SELF 'TV:HOLLOW-RECTANGULAR-BLINKER))
  (SEND MOUSE-BLINKER :SET-VISIBILITY NIL)
  (SEND MOUSE-BLINKER :SET-SIZE (+ (TV:FONT-BLINKER-WIDTH FONTS:TOG) 4)
                                (+ (TV:FONT-BLINKER-HEIGHT FONTS:TOG) 4))
  (COND ((NULL COLOR-PATTERN)
         ;; User didn't specify a color pattern, make an alternating one.
         (LET ((RADIX (GET INIT-PLIST ':RADIX)))
           (LET ((N-BITS (COND ((NOT (NULL RADIX))
                                (1- (HAULONG RADIX)))
                               ((GET INIT-PLIST ':HEX) 4)
                               (T 3))))
             (DO ((PATTERN 0)
                  (BYTE-SPEC #o0001 (+ BYTE-SPEC #o0100))
                  (I 0 (1+ I)))
                 (( I N-SWITCHES)
                  (SETQ COLOR-PATTERN PATTERN))
               (IF (ODDP (TRUNCATE I N-BITS))
                   (SETQ PATTERN (DPB 1 BYTE-SPEC PATTERN))))))))
  (SET-SWITCH-REGISTER-SPACING))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :AFTER :REFRESH) (&REST IGNORE)
  (WHEN (NULL TV:RESTORED-BITS-P)
    (SETQ STATE NIL)
    (SEND SELF :UPDATE)))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :SET-VALUE) (NEW-VALUE)
  (SETQ VALUE NEW-VALUE)
  (SEND SELF :UPDATE))

(DEFUN SWITCH-REGISTER-CELL (X)
  (DECLARE (:SELF-FLAVOR SWITCH-REGISTER-MIXIN))
  (MIN (TRUNCATE X X-SPACING) (1- N-SWITCHES)))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :MOUSE-BUTTONS) (IGNORE X IGNORE)
  (LET ((BITS (- N-SWITCHES (SWITCH-REGISTER-CELL X) 1)))
    (PROCESS-RUN-FUNCTION "SREG click"
                          SELF
                          :NEW-MOUSE-VALUE
                          (LOGXOR VALUE (LSH 1 BITS)))))

;;; This message exists specifically so that you can put daemons on it.
(DEFMETHOD (SWITCH-REGISTER-MIXIN :NEW-MOUSE-VALUE) (NEW-VALUE)
  (SETQ VALUE NEW-VALUE)
  (SEND SELF :UPDATE))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :MOUSE-MOVES) (X IGNORE)
  (TV:MOUSE-SET-BLINKER-CURSORPOS)
  (LET ((CELL (SWITCH-REGISTER-CELL X)))
    (SEND MOUSE-BLINKER :SET-CURSORPOS
                        (- (+ START-X (* CELL X-SPACING)) 2)
                        (- START-Y 2))
    (SEND MOUSE-BLINKER :SET-VISIBILITY T)))

(DEFMETHOD (SWITCH-REGISTER-MIXIN :AFTER :HANDLE-MOUSE)  ()
  (SEND MOUSE-BLINKER :SET-VISIBILITY NIL))

(DEFFLAVOR SWITCH-REGISTER () (SWITCH-REGISTER-MIXIN TV:WINDOW)
  (:DEFAULT-INIT-PLIST
    :BLINKER-P NIL
    :LABEL NIL
    :SAVE-BITS T
    :FONT-MAP (LIST FONTS:TOG)))

;;; TESTING

(DEFVAR S)

(DEFUN S (&REST OPTIONS)
  (SETQ S (APPLY #'MAKE-INSTANCE 'SWITCH-REGISTER
                                :EDGES-FROM ':MOUSE
                                :EXPOSE-P T
                                :N-SWITCHES 8.
                                OPTIONS)))

(DEFFLAVOR MUNCH-WINDOW () (TV:BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER))

(DEFFLAVOR MUNCH-BITS-PANE () (TV:WINDOW)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :LABEL NIL))

(DEFFLAVOR MUNCH-SWITCH-REGISTER-PANE () (SWITCH-REGISTER-MIXIN TV:WINDOW)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :LABEL NIL :N-SWITCHES 16.))

(DEFMETHOD (MUNCH-SWITCH-REGISTER-PANE :AFTER :NEW-MOUSE-VALUE) (NEW-VALUE)
  (SEND SELF :FORCE-KBD-INPUT (LIST ':NEW-MOUSE-VALUE NEW-VALUE)))

(DEFFLAVOR MUNCH-NUMBER-PANE () (TV:BORDERS-MIXIN TV:TOP-BOX-LABEL-MIXIN TV:WINDOW)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :LABEL NIL
                       :MORE-P NIL :FONT-MAP '(FONTS:43VXMS)))

(DEFMETHOD (MUNCH-NUMBER-PANE :PRINT-OUT) (-VALUE-)
  (SEND SELF :SET-CURSORPOS 0 0)
  (SEND SELF :CLEAR-WINDOW)
  (FORMAT SELF "~O" -VALUE-))

(DEFCONST
  *MUNCH-HELP-LINES*
  '("To modify value in switches: NETWORK through \ keys"
    "toggle corresponding switches. - and = shift in bits."
    "Numbers shift in three bits.  CLEAR-INPUT zeroes."
    "Hands up and down increment and decrement, hands"
    "right and left shift.  N gets next higher number with"
    "same number of one bits. END exits."
    "HOLD OUTPUT stops the action, RESUME resumes it."))

(DEFFLAVOR MUNCH-HELP-PANE () (TV:WINDOW-PANE)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :LABEL NIL :MORE-P NIL))

(DEFMETHOD (MUNCH-HELP-PANE :AFTER :REFRESH) (&OPTIONAL TYPE)
  (AND (OR (NOT TV:RESTORED-BITS-P) (EQ TYPE ':SIZE-CHANGED))
       (TV:SHEET-FORCE-ACCESS (SELF)
         (SEND SELF :SET-CURSORPOS 0 0)
         (DO ((L *MUNCH-HELP-LINES* (CDR L)))
             ((NULL (CDR L))
              (SEND SELF :STRING-OUT (CAR L)))
           (SEND SELF :LINE-OUT (CAR L))))))

(DEFVAR *MUNCH-WINDOW* NIL)

(DEFUN MAKE-MUNCH-WINDOW (EDGES)
  (SETQ *MUNCH-WINDOW*
        (MAKE-INSTANCE 'MUNCH-WINDOW
                       :EDGES EDGES
                       :PANES '((NUMBER MUNCH-NUMBER-PANE :LABEL "Munching Squares")
                                (BITS MUNCH-BITS-PANE)
                                (SREG MUNCH-SWITCH-REGISTER-PANE)
                                (HELP MUNCH-HELP-PANE))
                       :CONSTRAINTS `((MAIN . ((NUMBER BITS SREG HELP)
                                               ((NUMBER 1 :LINES))
                                               ((SREG 0.2s0))
                                               ((HELP ,(LENGTH *MUNCH-HELP-LINES*) :LINES))
                                               ((BITS :EVEN))))))))

(DEFUN MUNCH (&OPTIONAL (INITIAL-VALUE #o401));TRY ALSO 1, 10421, 11111, 100001, ETC.
  (IF (NULL *MUNCH-WINDOW*) (MAKE-MUNCH-WINDOW '#o(140 130 1140 1300)))
  (LET ((BITS-PANE (SEND *MUNCH-WINDOW* :GET-PANE 'BITS))
        (OLD-SELECTED-WINDOW TV:SELECTED-WINDOW))
    (UNWIND-PROTECT
        (PROGN
          (SEND *MUNCH-WINDOW* :EXPOSE)
          (SEND BITS-PANE :SELECT)
          (SEND BITS-PANE :DO-MUNCHING
                          INITIAL-VALUE
                          (SEND *MUNCH-WINDOW* :GET-PANE 'NUMBER)
                          (SEND *MUNCH-WINDOW* :GET-PANE 'SREG)))
      (SEND *MUNCH-WINDOW* :DEACTIVATE)
      (SEND OLD-SELECTED-WINDOW :SELECT))))

(DEFMETHOD (MUNCH-BITS-PANE :DO-MUNCHING) (INITIAL-VALUE NUMBER-PANE SREG-PANE)
  (LET ((VALUE INITIAL-VALUE)
        (AB 0)
        (X-OFFSET (+ (TV:SHEET-INSIDE-LEFT)
                     (TRUNCATE (- (TV:SHEET-INSIDE-WIDTH) 256.) 2)))
        (Y-OFFSET (+ (TV:SHEET-INSIDE-TOP)
                     (TRUNCATE (- (TV:SHEET-INSIDE-HEIGHT) 256.) 2))))
    (SEND SELF :CLEAR-WINDOW)
    (SEND NUMBER-PANE :PRINT-OUT VALUE)
    (SEND SREG-PANE :SET-VALUE VALUE)
    (CATCH 'QUIT-MUNCHING
      (LOOP WITH RUNNING-P = T
            DO (LOOP AS CHAR = (SEND SELF :TYI-NO-HANG)
                     DO (COND ((NULL CHAR)
                               (IF (NOT RUNNING-P)
                                   (SEND SELF :UNTYI (SEND SELF :TYI))))
                              (T
                               (LET ((X (MUNCH-PROCESS-CHAR CHAR VALUE)))
                                 (COND ((NUMBERP X)
                                        (SETQ VALUE (LOGAND #o177777 X) AB 0)
                                        (SEND SELF :CLEAR-WINDOW)
                                        (SEND NUMBER-PANE :PRINT-OUT VALUE)
                                        (SEND SREG-PANE :SET-VALUE VALUE))
                                       ((EQ X 'GO)
                                        (SETQ RUNNING-P T))
                                       ((EQ X 'STOP)
                                        (SETQ RUNNING-P NIL))))))
                       (IF RUNNING-P
                           (TV:PREPARE-SHEET (SELF)
                             (DO ((X) (Y)) ((TV:KBD-HARDWARE-CHAR-AVAILABLE))
                               (SETQ AB (LOGAND #o177777 (+ AB VALUE)))
                               (SETQ X (LOGAND AB #o377))
                               (SETQ Y (+ Y-OFFSET (LOGXOR X (LDB 1010 AB))))
                               (SETQ X (+ X X-OFFSET))
                               (AS-2-REVERSE
                                 (LOGXOR 1 (AR-2-REVERSE TV:SCREEN-ARRAY X Y))
                                 TV:SCREEN-ARRAY X Y)))))))))

(DEFUN MUNCH-PROCESS-CHAR (CHAR -VALUE-)
  (DECLARE (:SELF-FLAVOR MUNCH-SWITCH-REGISTER-PANE))
  (IF (NOT (ATOM CHAR))
      (SELECTQ (FIRST CHAR)
        (:NEW-MOUSE-VALUE
         (SECOND CHAR)))
      (SELECTOR CHAR CHAR-EQUAL
        (#/END
         (THROW 'QUIT-MUNCHING NIL))
        (#/N
         (NHNWSNOOB -VALUE-))
        ((#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
         (+ (LSH -VALUE- 3) (- (LDB %%CH-CHAR CHAR) #/0)))
        (#/CLEAR-INPUT
         0)
        ((#/+ #/SP #/HAND-UP)
         (1+ -VALUE-))
        ((#/- #/HAND-LEFT)
         (LSH -VALUE- 1))
        (#/=
         (1+ (LSH -VALUE- 1)))
        (#/HAND-RIGHT
         (LSH -VALUE- -1))
        (#/HAND-DOWN
         (1- -VALUE-))
        (#/CLEAR-SCREEN
         -VALUE-)
        ((#/HOLD-OUTPUT #/STOP-OUTPUT)
         'STOP)
        (#/RESUME
         'GO)
        (OTHERWISE
          (LET ((BIT-POSITION (FIND-POSITION-IN-LIST
                                (CHAR-UPCASE (LDB %%CH-CHAR CHAR))
                                '(#/\ #/` #/) #/( #/P #/O #/I #/U
                                  #/Y #/T #/R #/E #/W #/Q #/TAB #/NETWORK))))
            (IF BIT-POSITION
                (LOGXOR -VALUE- (LSH 1 BIT-POSITION))
                NIL))))))

(DEFUN NHNWSNOOB (A)    ;NEXT HIGHER NUMBER WITH SAME NUMBER OF ONE BITS (SEE HAKMEM)
  (IF (ZEROP A)
      0
      (LET* ((C (LOGAND A (- 0 A)))
             (B (+ A C)))
        (LOGIOR B (TRUNCATE (LSH (LOGXOR A B) -2) C)))))

(DEFDEMO "Munching Squares" "A classic display hack from the PDP-1." (MUNCH))

(COMPILE-FLAVOR-METHODS SWITCH-REGISTER-MIXIN SWITCH-REGISTER
                        MUNCH-WINDOW MUNCH-BITS-PANE MUNCH-SWITCH-REGISTER-PANE
                        MUNCH-NUMBER-PANE MUNCH-HELP-PANE)
