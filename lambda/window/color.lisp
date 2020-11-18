;;; -*- Mode: LISP;  Package: COLOR;  Base: 8-*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;***** Things to do *****
; Functions to save and restore copy of hardware color map


;SCREEN data structure for the color TV
(DEFVAR COLOR-SCREEN)

;sync program bits
;1  HSYNC -- Note! This bit is inverted.
;2  VSYNC
;4  COMPOSITE - (not used really, encode on HSYNC)
;10  BLANKING
;     0  PROC CYC
;     20  REFRESH
;     40  INC TVMA
;     60  STEP TVMA
;     0    --
;     100  CLR TVMA
;     200  EOL
;     300  EOP
;Assume 60MHZ bit clock, therefore 15Mhz (66.7 ns) TTL clock, 533ns SYNC clk
; 30. sync clks per line, 10. for horz sync,
;41.6 lines for 666 usec vertical
;1037 lines per 16.66 ms frame

;;; 64 Mhz main clock, 12 Mhz video data rate.
;;; During each sync period, 4 8 bit nibbles are shifted out.
;;; Therefore a TV cycle is done every 2 sync periods.

(COMMENT
;; This sync program is only CLOSE to NTSC video

(SETQ SYNC '(
   1 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 111 ;equalizing pulses, clr-tvma
   2 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   3 32 32 12 32 32 32 (39. 12) (5 13) (45. 12) (3 13) 213 13   ;vert sync
   3   30 30 10 30 30 30 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   13. 30 30 10 30 30 30 (4 11) (88. 11) 211 11         ;vert retrace
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   227. 30 30 10 30 30 30 (9. 11) 11 (36. 41 1) 1 1 (8. 11) 211 71   ;12 mhz video, 9x64 bits
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   1 30 30 10 30 30 30 (9. 11) (83. 11) 211 11
   1 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 111 ;equalizing pulses, clr-tvma
   2 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   1 30 30 10 30 30 31 (44. 11) (45. 12) (3 13) 213 73  ;extra 1/2 line, step-tvma
   2 32 32 12 32 32 32 (39. 12) (5 13) (45. 12) (3 13) 213 13   ;vert sync
   1 32 32 12 32 32 32 (39. 12) (5 13) (5 10) (43. 11) 211 11   ;1/2 vert sync, equalizing
   3 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   13. 30 30 10 30 30 30 (4 11) (88. 11) 211 11         ;vert retrace
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   227. 30 30 10 30 30 30 (9. 11) 11 (36. 41 1) 1 1 (8. 11) 211 71
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   1 30 30 10 30 30 30 (9. 11) (83. 11) 311 11
   (30 1))))

;;; This is really NTSC standard video
(DEFCONST SYNC '(
   1 30 30 10 31 31 31 (45. 11) (3 10) (46. 11) 211 111 ;equalizing pulses, clr-tvma
   2 30 30 10 31 31 31 (45. 11) (3 10) (46. 11) 211 11  ;equalizing pulses
   3 32 32 12 32 32 32 (39. 12) (6 13) (45. 12) (4 13) 213 13   ;vert sync
   3 30 30 10 31 31 31 (45. 11) (3 10) (46. 11) 211 11  ;equalizing pulses

   13. 30 30 10 30 30 30 (6 11) (88. 11) 211 11         ;vert retrace
   6 30 30 10 30 30 30 (12. 11) (74. 1) (8. 11) 211 11
   227. 30 30 10 30 30 30 (11. 11) 11 (36. 41 1) 1 1 (8. 11) 211 71   ;12 mhz video, 9x64 bits
   6 30 30 10 30 30 30 (12. 11) (74. 1) (8. 11) 211 11
   1 30 30 10 30 30 30 (11. 11) (83. 11) 211 11

   1 30 30 10 30 30 30 (45. 11) (3 10) (46. 11) 211 111 ;equalizing pulses, clr-tvma
   2 30 30 10 31 31 31 (45. 11) (3 10) (46. 11) 211 11  ;equalizing pulses
   1 30 30 10 31 31 31 (45. 11) (45. 10) (4 11) 211 11  ;equalizing pulses
   1 30 30 10 30 30 30 (39. 10) (6 13) (45. 12) (4 13) 213 73  ;step-tvma
   1 32 32 12 32 32 32 (39. 12) (6 13) (45. 12) (4 11) 213 13   ;vert sync
   1 32 32 12 30 30 30 (39. 10) (6 11) (3 10) (46. 11) 211 11   ;equalizing
   3 30 30 10 31 31 31 (45. 11) (3 10) (46. 11) 211 11  ;equalizing pulses

   13. 30 30 10 30 30 30 (6 11) (88. 11) 211 11         ;vert retrace
   6 30 30 10 30 30 30 (12. 11) (74. 1) (8. 11) 211 11
   227. 30 30 10 30 30 30 (11. 11) 11 (36. 41 1) 1 1 (8. 11) 211 71
   6 30 30 10 30 30 30 (12. 11) (74. 1) (8. 11) 211 11
   1 30 30 10 30 30 30 (11. 11) (83. 11) 311 11

   (30 1)))

;Function which reads an XBUS location with parity checking disabled.
;This is useful if it might be NXM.  Unfortunately the CADR machine
;mode register cannot be read back so we guess that the value that
;is supposed to be in it is 46 (prom-disable, error-stop-enable, normal,
; no stat-stop-enable, parity-stop-enable).
;The mode register is writable at location 766012
;XBUS-ADDR is an I/O address like %XBUS-READ.
(DEFUN XBUS-READ-NO-PARITY (XBUS-ADDR)
  (PROG2 (%UNIBUS-WRITE 766012 42)      ;Turn off error-stop-enable (normal speed)
         (%XBUS-READ XBUS-ADDR)
         (%UNIBUS-WRITE 766012 46)      ;Turn on error-stop-enable
         (%UNIBUS-WRITE 766044 0)))     ;Clear xbus nxm and parity indicators

(DEFUN XBUS-LOCATION-EXISTS-P (XBUS-ADDR BITS)
  "T if it is possible to turn on bits BITS in address XBUS-ADDR."
  (%XBUS-WRITE XBUS-ADDR BITS)
  (BIT-TEST BITS (XBUS-READ-NO-PARITY XBUS-ADDR)))

(DEFUN COLOR-EXISTS-P (&OPTIONAL (SCREEN COLOR-SCREEN))
  "T if this machine has color screen hardware."
  (SELECT PROCESSOR-TYPE-CODE
    (SI:CADR-TYPE-CODE (XBUS-LOCATION-EXISTS-P (LOGAND (TV:SCREEN-BUFFER SCREEN) 377777) 1))
    (SI:LAMBDA-TYPE-CODE NIL)))

(DEFVAR COLOR-MAP-ON 377)
(DEFVAR COLOR-MAP-OFF 0)

; FUNCTIONS TO SETUP COLOR MAP
; NOTE: As the hardware does not allow reading back of the color map, it is necessary
;       to maintain an array of what we expect to be in the color map.  As this array should
;       be kept accurate, the only function that should be used to write the color map
;       is WRITE-COLOR-MAP.  The added advantage is that the array can be saved and restored.

;Write one location in the color map
;Sync to horizontal sync.
(DEFUN WRITE-COLOR-MAP (LOC R G B
                        &OPTIONAL (SYNCHRONIZE NIL) (SCREEN COLOR-SCREEN)
                        &AUX (TV-ADR (TV:SCREEN-CONTROL-ADDRESS SCREEN))
                             (HARDWARE-COLOR-MAP (GET (LOCF (TV:SCREEN-PROPERTY-LIST SCREEN))
                                                      ':HARDWARE-COLOR-MAP)))
  "Set the value of color LOC in the color map.
LOC is a number between 0 and 17, that can appear in a pixel;
R, G and B (red, green and blue) are numbers from 0 to 377
that together say how pixels containing LOC should appear on the screen.
SYNCHRONIZE = T says wait until retrace time to make the change,
 to avoid a visible glitch.
SCREEN is the screen to operate on."
  (SETQ LOC (LOGAND LOC 17)
        R (- 377 (LOGAND (FIX R) 377))
        G (- 377 (LOGAND (FIX G) 377))
        B (- 377 (LOGAND (FIX B) 377)))
  (AND SYNCHRONIZE (PROG NIL A (COND ((GREATERP (LOGAND 40 (%XBUS-READ TV-ADR)) 0)
                                      (RETURN NIL)))(GO A)))
  (%XBUS-WRITE-SYNC (+ TV-ADR 4) (DPB R 1010 LOC) 0 TV-ADR 100 100)
  (%XBUS-WRITE-SYNC (+ TV-ADR 4) (DPB G 1010 (DPB 1 0602 LOC)) 0 TV-ADR 100 100)
  (%XBUS-WRITE-SYNC (+ TV-ADR 4) (DPB B 1010 (DPB 2 0602 LOC)) 0 TV-ADR 100 100)
  (ASET (- 377 R) HARDWARE-COLOR-MAP LOC 0)
  (ASET (- 377 G) HARDWARE-COLOR-MAP LOC 1)
  (ASET (- 377 B) HARDWARE-COLOR-MAP LOC 2))

;Use this when you know you are in the vertical retrace.  Dont waste time waiting
;for horizontal retrace.
(DEFUN WRITE-COLOR-MAP-IMMEDIATE (LOC R G B
                        &OPTIONAL (SCREEN COLOR-SCREEN)
                        &AUX (TV-ADR (TV:SCREEN-CONTROL-ADDRESS SCREEN))
                             (HARDWARE-COLOR-MAP (GET (LOCF (TV:SCREEN-PROPERTY-LIST SCREEN))
                                                      ':HARDWARE-COLOR-MAP)))
  "Like WRITE-COLOR-MAP but faster but only allowed under special conditions.
The condition is that you must already be in the vertical retrace."
  (SETQ LOC (LOGAND LOC 17)
        R (- 377 (LOGAND (FIX R) 377))
        G (- 377 (LOGAND (FIX G) 377))
        B (- 377 (LOGAND (FIX B) 377)))
  (%XBUS-WRITE (+ TV-ADR 4) (DPB R 1010 LOC))
  (%XBUS-WRITE (+ TV-ADR 4) (DPB G 1010 (DPB 1 0602 LOC)))
  (%XBUS-WRITE (+ TV-ADR 4) (DPB B 1010 (DPB 2 0602 LOC)))
  (ASET (- 377 R) HARDWARE-COLOR-MAP LOC 0)
  (ASET (- 377 G) HARDWARE-COLOR-MAP LOC 1)
  (ASET (- 377 B) HARDWARE-COLOR-MAP LOC 2))

(DEFUN BLT-COLOR-MAP (ARRAY &OPTIONAL (SCREEN COLOR-SCREEN)
                            &AUX (TV-ADR (TV:SCREEN-CONTROL-ADDRESS SCREEN))
                                 (TV-ADR4 (+ 4 TV-ADR))
                                 (HARDWARE-COLOR-MAP (GET
                                                      (LOCF (TV:SCREEN-PROPERTY-LIST SCREEN))
                                                      ':HARDWARE-COLOR-MAP))
                                 (I 0)
                                 (TEM-ARRAY (MAKE-ARRAY 60 ':TYPE 'ART-16B)))
  "Write the entire color map of SCREEN from ARRAY (a 16 by 3 array).
ARRAY could be a saved copy of HARDWARE-COLOR-MAP.
We wait for vertical retrace to tell the hardware."
      (DO ((I 0 (1+ I)))                        ;Precompute xbus-writands
          (( I 20))
        (ASET (DPB (- 377 (FIX (AREF ARRAY I 0))) 1010 I) TEM-ARRAY I)
        (ASET (DPB (- 377 (FIX (AREF ARRAY I 1))) 1010 (+ I 100)) TEM-ARRAY (+ I 20))
        (ASET (DPB (- 377 (FIX (AREF ARRAY I 2))) 1010 (+ I 200)) TEM-ARRAY (+ I 40)))
      (DO () ((ZEROP (LOGAND 40 (%XBUS-READ TV-ADR))))) ;til not vert retracing
      (DO () ((BIT-TEST (%XBUS-READ TV-ADR) 40))        ;til retrace starts
        (COND ((< I 20) (ASET (AREF ARRAY I 0) HARDWARE-COLOR-MAP I 0)
                        (ASET (AREF ARRAY I 1) HARDWARE-COLOR-MAP I 1)
                        (ASET (AREF ARRAY I 2) HARDWARE-COLOR-MAP I 2)
                        (SETQ I (1+ I)))))      ;update internal version while-u-wait
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 0))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 1))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 2))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 3))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 4))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 5))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 6))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 7))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 8))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 11))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 12))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 13))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 14))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 15))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 16))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 17))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 20))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 21))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 22))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 23))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 24))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 25))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 26))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 27))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 30))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 31))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 32))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 33))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 34))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 35))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 36))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 37))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 40))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 41))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 42))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 43))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 44))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 45))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 46))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 47))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 50))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 51))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 52))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 53))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 54))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 55))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 56))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 57))
      (DO ((I I (1+ I)))                        ;finish updating internal map version
          (( I 20))
        (ASET (AREF ARRAY I 0) HARDWARE-COLOR-MAP I 0)
        (ASET (AREF ARRAY I 1) HARDWARE-COLOR-MAP I 1)
        (ASET (AREF ARRAY I 2) HARDWARE-COLOR-MAP I 2))
      (RETURN-STORAGE (PROG1 TEM-ARRAY (SETQ TEM-ARRAY NIL))))

;Read a map location.  Returns four values: R, G, B, LOC
(DEFUN READ-COLOR-MAP (LOC &OPTIONAL (SCREEN COLOR-SCREEN))
  "Read one color map entry for SCREEN, for color LOC.
LOC is a number from 0 to 17.
The values are three arguments R, G, B you could pass to WRITE-COLOR-MAP,
and the fourth value is LOC."
  (PROG ((LOC (LOGAND LOC 17))
         (HARDWARE-COLOR-MAP (GET (LOCF (TV:SCREEN-PROPERTY-LIST SCREEN))
                                  ':HARDWARE-COLOR-MAP)))
    (RETURN (AREF HARDWARE-COLOR-MAP LOC 0)
            (AREF HARDWARE-COLOR-MAP LOC 1)
            (AREF HARDWARE-COLOR-MAP LOC 2)
            LOC)))

;;; Fill the color map with the specified color from the specified starting address
(DEFUN FILL-COLOR-MAP (R G B &OPTIONAL (START 1) (SCREEN COLOR-SCREEN))
  "Write color map colors START through 17 of SCREEN with R, G, B."
  (DO ((I START (1+ I)))
      ((> I 17))
      (WRITE-COLOR-MAP I R G B T SCREEN)))

;;; Make a color screen
(DEFFLAVOR COLOR-SCREEN () (TV:STANDARD-SCREEN))

(DEFMETHOD (COLOR-SCREEN :PARSE-FONT-SPECIFIER) (FD)
  (SETQ FD (TV:SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:COLOR-FONT))
  (cond  ((GET (FONT-NAME FD) 'COLOR-FONT))
         ((GET (FONT-NAME FD) 'FONTS:COLOR-FONT)
          (SETQ FD (SYMBOL-VALUE (GET (FONT-NAME FD) 'FONTS:COLOR-FONT))))
         (T (SETQ FD (MAKE-COLOR-FONT FD))))
  FD)

(DEFMETHOD (COLOR-SCREEN :PARSE-FONT-DESCRIPTOR) (FD)
  (SETQ FD (TV:SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:COLOR-FONT))
  (COND  ((GET (FONT-NAME FD) 'COLOR-FONT))
         ((GET (FONT-NAME FD) 'FONTS:COLOR-FONT)
          (SETQ FD (SYMBOL-VALUE (GET (FONT-NAME FD) 'FONTS:COLOR-FONT))))
         (T (SETQ FD (MAKE-COLOR-FONT FD))))
  FD)

(DEFWRAPPER (COLOR-SCREEN :EXPOSE) (IGNORE . BODY)
  "Don't actually expose the color screen if there is no color monitor.  This
function is a TOTAL KLUDGE."
  `(COND ((COLOR-EXISTS-P SELF)
          (OR TV:EXPOSED-P (SETUP SYNC SELF))
          . ,BODY)
         (T (SETUP SYNC SELF))))

(DEFUN MAKE-SCREEN (&OPTIONAL (NAME "COLOR") (XBUS-ADR -600000) (CONTROL-ADR 377750))
  (TV:DEFINE-SCREEN 'COLOR-SCREEN NAME
    ':BITS-PER-PIXEL 4 ':BUFFER XBUS-ADR
    ':HEIGHT 454. ':WIDTH 576.
    ':CONTROL-ADDRESS CONTROL-ADR
    ':PROPERTY-LIST
    `(:VIDEO :COLOR
             :CONTROLLER :SIMPLE
             :HARDWARE-COLOR-MAP ,(MAKE-ARRAY '(20 3) ':TYPE 'ART-8B))))

;Init a color screen
(DEFUN SETUP (&OPTIONAL (SYNC-PROG SYNC) (SCREEN COLOR-SCREEN)
              &AUX (TV-COLOR-ADR (TV:SCREEN-CONTROL-ADDRESS SCREEN)))
  (COND ((COLOR-EXISTS-P SCREEN)
         (SI:STOP-SYNC TV-COLOR-ADR)
         (SI:FILL-SYNC SYNC-PROG 0 TV-COLOR-ADR)
         (SI:START-SYNC 3 0 36. TV-COLOR-ADR)
         (WHEN (VARIABLE-BOUNDP COLOR-SCREEN)   ;May not be, if just creating it now.
           (R-G-B-COLOR-MAP)
           (SI:CLEAR-SCREEN-BUFFER (SEND COLOR-SCREEN ':BUFFER)))
         ;; Make appropriate menu entries
         (OR (ASS 'EQUALP "Color Window" TV:*SYSTEM-MENU-WINDOWS-COLUMN*)
             (SETQ TV:*SYSTEM-MENU-WINDOWS-COLUMN*
                   (APPEND TV:*SYSTEM-MENU-WINDOWS-COLUMN*
                           (NCONS '("Color Window"
                                    :EVAL (TV:SYSTEM-MENU-CREATE-WINDOW COLOR-SCREEN)
                                    :DOCUMENTATION "Create a window on the color screen"))))))
        (T
         (SETQ TV:*SYSTEM-MENU-WINDOWS-COLUMN*
               (REMQ (ASS 'EQUALP "Color Window" TV:*SYSTEM-MENU-WINDOWS-COLUMN*)
                     TV:*SYSTEM-MENU-WINDOWS-COLUMN*)))))

(DEFVAR BITBLT-ARRAY NIL)

(ADD-INITIALIZATION "Set COLOR:BITBLT-ARRAY"
                    '(SETQ BITBLT-ARRAY
                           (MAKE-ARRAY (IF SI:ARRAY-INDEX-ORDER '(1. 8.) '(8. 1.))
                                       ':TYPE 'ART-4B))
                    '(:NOW) 'ARRAY-ORDER-INITIALIZATION-LIST)

(DEFSUBST COLOR-FILLARRAY (COLOR)
  (ARRAY-INITIALIZE BITBLT-ARRAY COLOR))

(DEFUN BARS (SIZE &OPTIONAL (START 0) (SCREEN COLOR-SCREEN))
  "Draw 16 vertical bars of width SIZE on color-screen SCREEN.
Start at horizontal position START.
Bar n is drawn in color n."
  (TV:PREPARE-SHEET (SCREEN)
    (DO ((COLOR 0 (1+ COLOR))
         (SCREEN-WIDTH (TV:SHEET-WIDTH SCREEN))
         (HEIGHT (TV:SHEET-HEIGHT SCREEN))
         (TARGET-ARRAY (TV:SHEET-SCREEN-ARRAY SCREEN)))
        ((> COLOR 16))
      (COLOR-FILLARRAY (1+ COLOR))
      (OR (> (+ START (* SIZE COLOR) SIZE) SCREEN-WIDTH)
          (BITBLT TV:ALU-SETA SIZE HEIGHT
                  BITBLT-ARRAY 0 0
                  TARGET-ARRAY (+ START (* SIZE COLOR)) 0)))))

(DEFUN RECTANGLE (X Y WIDTH HEIGHT COLOR
                      &OPTIONAL (ALU TV:ALU-SETA) (SCREEN COLOR-SCREEN))
  "Draw a colored rectangle of color COLOR, with top left corner X and Y.
The width and height are WIDTH and HEIGHT.  It is drawn on color screen SCREEN."
  (COLOR-FILLARRAY COLOR)
  (TV:PREPARE-SHEET (SCREEN)
    (BITBLT ALU WIDTH HEIGHT
            BITBLT-ARRAY 0 0
            (TV:SHEET-SCREEN-ARRAY SCREEN) X Y)))

(COMPILER:MAKE-OBSOLETE COLOR-BITBLT "use COLOR:RECTANGLE with slightly different args")
(DEFUN COLOR-BITBLT (X1 Y1 X2 Y2 &OPTIONAL (COLOR 17) (ALU TV:ALU-SETA) (SCREEN COLOR-SCREEN))
  "Fill rectangle with endpoints X1, Y1 and X2, Y2 with color COLOR."
  (COLOR-FILLARRAY COLOR)
  (TV:PREPARE-SHEET (SCREEN)
    (BITBLT ALU (- X2 X1) (- Y2 Y1)
            BITBLT-ARRAY 0 0
            (TV:SHEET-SCREEN-ARRAY SCREEN) X1 Y1)))

(DEFUN R-G-B-BARS (&OPTIONAL (WIDTH 20) (SCREEN COLOR-SCREEN))
  "Draw alternating red, green and blue bars of width WIDTH on SCREEN."
  (R-G-B-COLOR-MAP)
  (BARS WIDTH 0 SCREEN))

(DEFCONST R-G-B `((,COLOR-MAP-ON ,COLOR-MAP-OFF ,COLOR-MAP-OFF)
                  (,COLOR-MAP-OFF ,COLOR-MAP-ON ,COLOR-MAP-OFF)
                  (,COLOR-MAP-OFF ,COLOR-MAP-OFF ,COLOR-MAP-ON)))

(DEFUN R-G-B-COLOR-MAP ()
  "Set color 0 to black, and the remaining colors to alternating red, green, blue."
  (WRITE-COLOR-MAP 0 COLOR-MAP-OFF COLOR-MAP-OFF COLOR-MAP-OFF)
  (DO ((I 1 (1+ I)))
      ((> I 17))
      (LEXPR-FUNCALL (FUNCTION WRITE-COLOR-MAP) I
                     (NTH (\ I 3) R-G-B))))

(DEFUN CLEAR (&OPTIONAL (SCREEN COLOR-SCREEN))
  "Clear the color screen SCREEN."
  (FUNCALL SCREEN ':CLEAR-SCREEN))

(DEFUN FLOOD (&OPTIONAL (SCREEN COLOR-SCREEN))
  "Fill all of color screen SCREEN with color 17 ."
  (TV:PREPARE-SHEET (SCREEN)
    (%DRAW-RECTANGLE (TV:SHEET-WIDTH SCREEN) (TV:SHEET-HEIGHT SCREEN)
                     0 0 TV:ALU-IOR SCREEN)))

(DEFUN ALL-BARS (&OPTIONAL (WIDTH 20) (SCREEN COLOR-SCREEN))
  "Fill the entire width of SCREEN with red, green and blue bars of width WIDTH."
  (R-G-B-COLOR-MAP)
  (CLEAR SCREEN)
  (LET ((SCREEN-WIDTH (TV:SHEET-WIDTH SCREEN))
        (BARS-WIDTH (* WIDTH 17)))
       (DO ((START 0 (+ START BARS-WIDTH)))
           (( START SCREEN-WIDTH))
           (BARS WIDTH START SCREEN))))

(DEFUN CROSSHATCH (&OPTIONAL (BAR-SIZE 1) (SPACING 20) (SCREEN COLOR-SCREEN))
  "Draw colored crosshatching with lines of width BAR-SIZE and spacing SPACING.
Does not set the color map."
  (CLEAR SCREEN)
  (MULTIPLE-VALUE-BIND (WIDTH HEIGHT) (FUNCALL SCREEN ':SIZE)
    (LET ((NV) (NH))
      (SETQ NV (- (TRUNCATE WIDTH SPACING) (COND ((> (\ WIDTH SPACING) BAR-SIZE) 0)
                                           (T 1)))
            NH (- (TRUNCATE HEIGHT SPACING) (COND ((> (\ HEIGHT SPACING) BAR-SIZE) 0)
                                            (T 1))))
      (SETQ WIDTH (+ (* NV SPACING) BAR-SIZE)
            HEIGHT (+ (* NH SPACING) BAR-SIZE))
      (DO ((X 0 (+ X SPACING))
           (I 0 (1+ I)))
          ((> I NV))
        (RECTANGLE X 0 BAR-SIZE HEIGHT (1+ (\ I 16))))
      (DO ((Y 0 (+ Y SPACING))
           (I 0 (1+ I)))
          ((> I NH))
        (RECTANGLE 0 Y WIDTH BAR-SIZE (1+ (\ I 16)))))))

(DEFUN WHITE-CROSSHATCH (&REST ARGS)
  "Draw crosshatching with white lines.  ARGS are passed to CROSSHATCH."
  (WRITE-COLOR-MAP 0 COLOR-MAP-OFF COLOR-MAP-OFF COLOR-MAP-OFF)
  (FILL-COLOR-MAP COLOR-MAP-ON COLOR-MAP-ON COLOR-MAP-ON 1)
  (LEXPR-FUNCALL #'CROSSHATCH ARGS))

(DEFUN SPECTRUM-COLOR-MAP ()
  "Fill the color map with a spectrum."
  (DO ((I 1 (1+ I))
       (OFFSET 0 (COND ((= I 7) 1) ((= I 16) 2) (T OFFSET))))
      ((> I 17))
      (WRITE-COLOR-MAP I (* (LDB 0001 (+ I OFFSET)) COLOR-MAP-ON)
                         (* (LDB 0101 (+ I OFFSET)) COLOR-MAP-ON)
                         (* (LDB 0201 (+ I OFFSET)) COLOR-MAP-ON))))

(DEFUN TRACK-MOUSE (&OPTIONAL (CURSOR-WIDTH 30) (CURSOR-HEIGHT 5) (COLOR 1))
  "Track the mouse on the color screen."
  (TV:WITH-MOUSE-GRABBED
    (DO ((BOX-X) (BOX-Y)
         (MAX-X (- (TV:SHEET-WIDTH COLOR-SCREEN) CURSOR-WIDTH))
         (MAX-Y (- (TV:SHEET-HEIGHT COLOR-SCREEN) CURSOR-HEIGHT)))
        ((FUNCALL TERMINAL-IO ':TYI-NO-HANG)
         (RECTANGLE BOX-X BOX-Y CURSOR-WIDTH CURSOR-HEIGHT COLOR TV:ALU-XOR))
      (WHEN BOX-X
        (TV:MOUSE-INPUT T)
        (RECTANGLE BOX-X BOX-Y CURSOR-WIDTH CURSOR-HEIGHT COLOR TV:ALU-XOR))
      (OR (ZEROP TV:MOUSE-LAST-BUTTONS)
          (SETQ COLOR TV:MOUSE-LAST-BUTTONS))
      (RECTANGLE (SETQ BOX-X (MIN TV:MOUSE-X MAX-X)) (SETQ BOX-Y (MIN TV:MOUSE-Y MAX-Y))
                 CURSOR-WIDTH CURSOR-HEIGHT
                 COLOR TV:ALU-XOR))))

(DEFUN RANDOM-COLOR-MAP (&OPTIONAL (START 1) (SYNCHRONIZE NIL) (SCREEN COLOR-SCREEN))
  "Fill the color map from START through 15. with random colors."
       (DO ((I START (1+ I)))
           ((= I 20))
           (WRITE-COLOR-MAP I (RANDOM (1+ COLOR-MAP-ON)) (RANDOM (1+ COLOR-MAP-ON))
                            (RANDOM (1+ COLOR-MAP-ON)) SYNCHRONIZE SCREEN)
           (SETQ SYNCHRONIZE NIL)))             ;ONLY SYNCHRONIZE THE FIRST CHANGE

(DEFUN GRAY-COLOR-MAP (&OPTIONAL (BASE 0))
  "Fill the color map with a gray scale; color 0 = BASE,
and the other colors going up in whiteness and wrapping around
so that color 17 is just darker than BASE."
       (DO ((I 0 (1+ I))
            (LEVEL))
           ((= I 20))
           (SETQ LEVEL (\ (+ BASE (* 20 I)) 400))
           (WRITE-COLOR-MAP I LEVEL LEVEL LEVEL)))

(DEFUN COLORIZE (&OPTIONAL (DELAY 4))
  "Set the color map randomly, again and again."
       (DO ()
           ((FUNCALL TERMINAL-IO ':TYI-NO-HANG))
           (PROCESS-ALLOW-SCHEDULE)
           (RANDOM-COLOR-MAP 1 (> DELAY 2))     ;IF THE DELAY IS > 2, SYNCHRONIZE WRITES
           (OR (ZEROP DELAY) (PROCESS-SLEEP DELAY))))

(DEFUN GRAY-COLORIZE (&OPTIONAL (DELAY 2))
  "Set the color map to a gray scale but rotate it randomly again and again."
       (DO ()
           ((FUNCALL TERMINAL-IO ':TYI-NO-HANG))
           (PROCESS-ALLOW-SCHEDULE)
           (GRAY-COLOR-MAP (RANDOM 400))
           (OR (ZEROP DELAY) (PROCESS-SLEEP DELAY))))


(DEFUN COLORATE (&OPTIONAL (DELAY 4) (STEPS 1000.))
  "Choose two colors and melt each into the other, repeatedly."
       (DO ()
           ((FUNCALL TERMINAL-IO ':TYI-NO-HANG))
           (LET ((J (RANDOM 20))
                 (KFOO (RANDOM 17)))
             (LET ((K (IF (< KFOO J) KFOO (1+ KFOO))))
               (MULTIPLE-VALUE-BIND (JR JG JB) (READ-COLOR-MAP J)
                 (MULTIPLE-VALUE-BIND (KR KG KB) (READ-COLOR-MAP K)
                   (DO ((I STEPS (1- I))
                        (FSTEPS (FLOAT STEPS))
                        (FJR (FLOAT JR))
                        (FJG (FLOAT JG))
                        (FJB (FLOAT JB))
                        (FKR (FLOAT KR))
                        (FKG (FLOAT KG))
                        (FKB (FLOAT KB)))
                       ((< I 0))
                     (LET ((P (// (FLOAT I) FSTEPS))
                           (1-P (// (FLOAT (- STEPS I)) FSTEPS)))
                       (WRITE-COLOR-MAP J
                                        (FIX (+ (* P FJR) (* 1-P FKR)))
                                        (FIX (+ (* P FJG) (* 1-P FKG)))
                                        (FIX (+ (* P FJB) (* 1-P FKB))))
                       (WRITE-COLOR-MAP K
                                        (FIX (+ (* P FKR) (* 1-P FJR)))
                                        (FIX (+ (* P FKG) (* 1-P FJG)))
                                        (FIX (+ (* P FKB) (* 1-P FJB))))))))))
           (OR (ZEROP DELAY) (PROCESS-SLEEP DELAY))))

(DECLARE (SPECIAL :10LEAF))

(DEFUN PUT-UP-PICT (&OPTIONAL (PICT :10LEAF)
                    &AUX MIN MAX HEIGHT WIDTH SC X0 Y0
                         (SCR (TV:SHEET-SCREEN-ARRAY COLOR-SCREEN)))
  (CLEAR)
  (GRAY-COLOR-MAP)
  (SETQ MIN (CDR (ARRAYDIMS PICT)))
  (SETQ WIDTH (CAR MIN) HEIGHT (CADR MIN))
  (SETQ X0 (TRUNCATE (- (TV:SHEET-WIDTH COLOR-SCREEN) WIDTH) 2)
        Y0 (TRUNCATE (- (TV:SHEET-HEIGHT COLOR-SCREEN) HEIGHT) 2))
  (SETQ MIN 377 MAX 0)
  (DO I 0 (1+ I) (>= I WIDTH)
    (DO J 0 (1+ J) (>= J HEIGHT)
      (SETQ MIN (MIN MIN (AREF PICT I J)))
      (SETQ MAX (MAX MAX (AREF PICT I J)))))
  (SETQ SC (// (- MAX MIN) 20.0S0))
  (DO I 0 (1+ I) (>= I WIDTH)
    (DO J 0 (1+ J) (>= J HEIGHT)
      (ASET (FIX (// (- (AREF PICT I J) MIN) SC))
            SCR (+ I X0) (+ J Y0)))))

(DEFUN COLOR-DRAW-LINE (X1 Y1 X2 Y2
                        &OPTIONAL (COLOR 17) (ALU TV:ALU-SETA) (SCREEN COLOR-SCREEN))
  "Draw a line from X1, Y1 to X2, Y2 in color COLOR."
    (AND (> X1 X2) (SWAPF X1 X2) (SWAPF Y1 Y2))
    (TV:PREPARE-SHEET (SCREEN)
      (LET ((DX (- X2 X1))
            (DY (- Y2 Y1))
            (PIXEL-ARRAY (TV:SHEET-SCREEN-ARRAY COLOR-SCREEN)))
        (LET ((DIR-Y (IF (MINUSP DY) -1 1))
              (DY (ABS DY)))
          (COND ((ZEROP DY) (RECTANGLE X1 Y1 (- X2 X1) 1 COLOR ALU))
                ((ZEROP DX) (RECTANGLE X1 (MIN Y1 Y2) 1 (- (MAX Y1 Y2) (MIN Y1 Y2))
                                       COLOR ALU))
                ((> DX DY)                      ;X IS LARGER STEP
                 (DO ((X1 X1 (1+ X1))
                      (REM (TRUNCATE DY 2) (+ REM DY)))
                     ((> X1 X2))
                   (IF ( REM DX) (SETQ Y1 (+ Y1 DIR-Y) REM (- REM DX)))
                   (AS-2-REVERSE (BOOLE ALU COLOR (AR-2-REVERSE PIXEL-ARRAY X1 Y1))
                                 PIXEL-ARRAY X1 Y1)))
                (T                              ;Y IS LARGER STEP
                 (DO ((I 0 (1+ I))
                      (Y Y1 (+ Y DIR-Y))
                      (REM (TRUNCATE DX 2) (+ REM DX)))
                     ((> I DY))
                   (IF ( REM DY) (SETQ X1 (1+ X1) REM (- REM DY)))
                   (AS-2-REVERSE (BOOLE ALU COLOR (AR-2-REVERSE PIXEL-ARRAY X1 Y))
                                 PIXEL-ARRAY X1 Y))))))))

(DEFUN COLOR-DRAW-CHAR (FONT CHAR X Y &OPTIONAL (COLOR 0) (DEVICE COLOR-SCREEN))
  "Draw character CHAR in font FONT at position X, Y in color COLOR.
FONT is an ordinary black-and-white font.
X and Y are the position of the top left corner of the character."
  (TV:PREPARE-SHEET (DEVICE)
    (%COLOR-DRAW-CHAR FONT CHAR X Y COLOR DEVICE)))

(DEFUN %COLOR-DRAW-CHAR (FONT CHAR X Y COLOR DEVICE)
  (LET ((WIDTH (FONT-RASTER-WIDTH FONT))
        (SCREEN (TV:SHEET-SCREEN-ARRAY DEVICE))
        (HEIGHT (FONT-RASTER-HEIGHT FONT)))
    (DO ((H 0 (1+ H)))
        ((> H HEIGHT))
      (DO ((W 0 (1+ W)))
          ((> W WIDTH))
        (IF (= (FED:FONT-GET-PIXEL FONT CHAR H W) 1)
            (ASET COLOR SCREEN (+ X W) (+ Y H)))))))

(DEFUN COLOR-PRINC (STRING X Y &OPTIONAL (COLOR 0) (FONT FONTS:CPTFONT)
                           (DEVICE COLOR-SCREEN)
                           &AUX (WIDTH (FONT-CHAR-WIDTH FONT))
                           (WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT)))
  "Output STRING starting at position X, Y in color COLOR using font FONT.
FONT is an ordinary black-and-white font.
X and Y are the position of the top left corner of the first character."
  (DO ((W X (+ W (IF WIDTH-TABLE (AREF WIDTH-TABLE (AREF C I)) WIDTH)))
       (C (FORMAT NIL "~A" STRING))
       (CH)
       (I 0 (1+ I)))
      ((>= I (STRING-LENGTH C)))
    (OR (= (SETQ CH (AREF C I)) #\SPACE)
        (COLOR-DRAW-CHAR FONT (AREF C I) W Y COLOR DEVICE))))

;;; Color font hackery (convert a font to be usable on the color screen)
(DEFUN MAKE-COLOR-FONT (BW-FONT &OPTIONAL (BIT-LIST '(1 1 1 1)) (FONT-NAME-SUFFIX ""))
  "Given ordinary font BW-FONT, return a font that can be used on the color screen.
Make the new font's name by adding COLOR- on the front and FONT-NAME-SUFFIX
at the end of BW-FONT's name.
BIT-LIST is either a fixnum of four bits, or a list of four single bits,
specifying which four bits of the color pixel this font will write in.
Each bit of the pixel is operated on separately by combining the bit
that is there with the bit in the font, using the alu-function."
  (COND ((FIXP BIT-LIST)
         (DO ((I 0 (1+ I))
              (X BIT-LIST)
              (L NIL))
             ((>= I 4)
              (SETQ BIT-LIST (NREVERSE L)))
           (PUSH (LDB 0001 X) L)
           (SETQ X (LSH X -1))))
        ((OR ( (LENGTH BIT-LIST) 4)
             (DOLIST (B BIT-LIST) (OR (NUMBERP B) (RETURN T))))
         (FERROR NIL "Illegal format for bit list ~S" BIT-LIST)))
  (LET ((FIT) (MAXW 1) (SIZE 0) (RASTER-WIDTH) (WORDS-PER-CHAR) (RASTERS-PER-WORD)
        (COLOR-FONT) (NEW-FIT)
        (FONT-NAME (INTERN (STRING-APPEND "COLOR-" (FONT-NAME BW-FONT) FONT-NAME-SUFFIX)
                           'FONTS)))
    (IF (SETQ FIT (FONT-INDEXING-TABLE BW-FONT))
        (DOTIMES (CHAR #o200)
          (SETQ MAXW (MAX MAXW (- (AREF FIT (1+ CHAR)) (AREF FIT CHAR))))))
    (SETQ MAXW (* (FONT-RASTER-WIDTH BW-FONT) MAXW 4))
    (COND ((> MAXW 32.)
           ;; Will need a font indexing table, make raster width 32., and
           ;; figure out the size of the array
           (SETQ RASTERS-PER-WORD 1
                 WORDS-PER-CHAR (FONT-RASTER-HEIGHT BW-FONT)
                 RASTER-WIDTH 32.)
           (COND ((NULL FIT)
                  ;; Old font has no indexing table, so every character is same size
                  (SETQ SIZE (* (CEILING MAXW 32.) WORDS-PER-CHAR 200)))
                 (T
                  ;; Otherwise, loop through and figure out size on a per character basis
                  (DOTIMES (CHAR 200)
                    (SETQ SIZE (+ SIZE (* 4 WORDS-PER-CHAR
                                          (- (AREF FIT (1+ CHAR)) (AREF FIT CHAR)))))))))
          ((NULL FIT)
           ;; Won't need an indexing table, and didn't have one
           (SETQ RASTER-WIDTH MAXW)
           (SETQ RASTERS-PER-WORD (TRUNCATE 32. RASTER-WIDTH))
           (SETQ WORDS-PER-CHAR (CEILING (FONT-RASTER-HEIGHT BW-FONT) RASTERS-PER-WORD))
           (SETQ SIZE (* WORDS-PER-CHAR 200)))
          (T (FERROR NIL "We don't need an indexing table, but black-and-white font did?")))
    (SETQ COLOR-FONT (TV:MAKE-FONT :MAKE-ARRAY (:LENGTH (* SIZE 32.) :TYPE ART-1B)
                                   :FONT-NAME FONT-NAME))
    (SETF (FILL-POINTER COLOR-FONT) 0)                  ;Fill pointer
    (SET FONT-NAME COLOR-FONT)
    ;; Copy parameters which are the same
    (SETF (FONT-CHAR-HEIGHT COLOR-FONT) (FONT-CHAR-HEIGHT BW-FONT))
    (SETF (FONT-CHAR-WIDTH COLOR-FONT) (FONT-CHAR-WIDTH BW-FONT)) ;Software width is the same
    (SETF (FONT-RASTER-HEIGHT COLOR-FONT) (FONT-RASTER-HEIGHT BW-FONT))
    (SETF (FONT-RASTER-WIDTH COLOR-FONT) RASTER-WIDTH)
    (SETF (FONT-RASTERS-PER-WORD COLOR-FONT) RASTERS-PER-WORD)
    (SETF (FONT-WORDS-PER-CHAR COLOR-FONT) WORDS-PER-CHAR)
    (SETF (FONT-BASELINE COLOR-FONT) (FONT-BASELINE BW-FONT))
    (SETF (FONT-CHAR-WIDTH-TABLE COLOR-FONT) (FONT-CHAR-WIDTH-TABLE BW-FONT))
    (SETF (FONT-LEFT-KERN-TABLE COLOR-FONT) (FONT-LEFT-KERN-TABLE BW-FONT))
    (SETF (FONT-BLINKER-WIDTH COLOR-FONT) (FONT-BLINKER-WIDTH BW-FONT))
    (SETF (FONT-BLINKER-HEIGHT COLOR-FONT) (FONT-BLINKER-HEIGHT BW-FONT))

    ;; Setup new font indexing table if necessary
    (IF (> MAXW 32.)
        (SETF (FONT-INDEXING-TABLE COLOR-FONT) (SETQ NEW-FIT (MAKE-ARRAY 201))))

    (COND ((NULL NEW-FIT)
           ;; This is pretty easy -- loop over each char and duplicate bits
           (LET ((RAS-PER-W (FONT-RASTERS-PER-WORD BW-FONT))
                 (RW (FONT-RASTER-WIDTH BW-FONT)))
             (DOTIMES (CHAR 200)
               (LET ((BW-BASE (* CHAR (FONT-WORDS-PER-CHAR BW-FONT) 32.))
                     (CBASE (* CHAR WORDS-PER-CHAR 32.))
                     (CPIX))
                 ;; Generator loop, calls sink on every bit
                 (DOTIMES (RAS (FONT-RASTER-HEIGHT BW-FONT))
                   (SETQ CPIX 0)
                   (DOTIMES (PIX RW)
                     ;; Here once for each pixel of bw font
                     (LET ((PIXEL (AREF BW-FONT
                                        (+ BW-BASE                      ;Offset
                                           (* 32. (TRUNCATE RAS RAS-PER-W))     ;Number of words in
                                           (* RW (\ RAS RAS-PER-W))     ;Num rasters in word
                                           PIX))))                      ;Pixel within raster
                       (DOLIST (B BIT-LIST)
                         ;; Now store pixel four times
                         (ASET (* PIXEL B) COLOR-FONT
                               (+ CBASE
                                  (* 32. (TRUNCATE RAS RASTERS-PER-WORD))
                                  (* RASTER-WIDTH (\ RAS RASTERS-PER-WORD))
                                  CPIX))
                         (SETQ CPIX (1+ CPIX))))))))))
          (T
           ;; We will have an indexing table
           (LET ((RAS-PER-W (FONT-RASTERS-PER-WORD BW-FONT))
                 (RW (FONT-RASTER-WIDTH BW-FONT))
                 (COLOR-IDX -1))
             (DOTIMES (CHAR 200)
               (ASET (1+ COLOR-IDX) NEW-FIT CHAR)
               (LET* ((BW-IDX (IF FIT
                                  (AREF FIT CHAR)
                                  CHAR))
                      (BW-NWIDE (IF FIT (- (AREF FIT (1+ CHAR)) BW-IDX) 1))
                      (BW-BASE)
                      (CBASE)
                      (IDX)
                      (CPIX))
                   (DOTIMES (RAS (FONT-RASTER-HEIGHT BW-FONT))
                     (SETQ IDX COLOR-IDX
                           CPIX RASTER-WIDTH)
                     (DOTIMES (CHAR-WITHIN-CHAR BW-NWIDE)
                       (SETQ BW-BASE (* (FONT-WORDS-PER-CHAR BW-FONT)
                                        (+ BW-IDX CHAR-WITHIN-CHAR)
                                        32.))
                       (DOTIMES (PIX RW)
                         ;; Here once for each pixel of bw font
                         (LET ((PIXEL (AREF BW-FONT
                                            (+ BW-BASE                    ;Offset
                                               (* 32. (TRUNCATE RAS RAS-PER-W)) ;Number of words in
                                               (* RW (\ RAS RAS-PER-W))   ;Num rasters in word
                                               PIX))))                    ;Pixel within raster
                           (DOLIST (B BIT-LIST)
                             ;; Now store pixel four times
                             (SETQ CPIX (1+ CPIX))
                             (IF ( CPIX RASTER-WIDTH)
                                 (SETQ IDX (1+ IDX)
                                       CPIX 0
                                       CBASE (* IDX WORDS-PER-CHAR 32.)))
                             (ASET (* PIXEL B) COLOR-FONT
                                   (+ CBASE
                                      (* 32. (TRUNCATE RAS RASTERS-PER-WORD))
                                      (* RASTER-WIDTH (\ RAS RASTERS-PER-WORD))
                                      CPIX)))))))
                   (SETQ COLOR-IDX IDX)))
             (ASET (1+ COLOR-IDX) NEW-FIT #o200))))
    (PUTPROP FONT-NAME T 'COLOR-FONT)
    (PUTPROP FONT-NAME (FONT-NAME BW-FONT) 'FONTS:CPT-FONT)
    (PUTPROP (FONT-NAME BW-FONT) FONT-NAME 'FONTS:COLOR-FONT)
    COLOR-FONT))

(COMPILE-FLAVOR-METHODS COLOR-SCREEN)

(ADD-INITIALIZATION "Color CPTFONT" '(MAKE-COLOR-FONT FONTS:CPTFONT) '(ONCE))
(ADD-INITIALIZATION "Color MOUSE" '(MAKE-COLOR-FONT FONTS:MOUSE) '(ONCE))
(ADD-INITIALIZATION "Color MEDFNT" '(MAKE-COLOR-FONT FONTS:MEDFNT) '(ONCE))
(ADD-INITIALIZATION "Color 5X5" '(MAKE-COLOR-FONT FONTS:5X5) '(ONCE))
(ADD-INITIALIZATION "Color Make Screen" '(SETQ COLOR-SCREEN (MAKE-SCREEN)) '(ONCE))
