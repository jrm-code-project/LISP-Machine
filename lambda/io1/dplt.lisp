;-*- MODE:LISP; PACKAGE:(DPLT GLOBAL 1000.); BASE:8 -*-
; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Send hardcopies of PLT files from SUDS to the Dover

;;; Not supported: rotated chars, PADs (other PC related stuff?)

;;; To print files use:  (PRINT-FILE file1 file2 keyword1 keyword2 ...)
;;; where file1, file2, etc are strings which are the names of files to be printed
;;; (second filename defaults to PLT) and keyword1, keyword2, etc are keywords
;;; indicated below.  Filenames and keywords may be interspersed in any order.
;;; The default action is that all the files specified are printed in a single
;;; Dover queue request by writing a file MC:.DOVR.;DPLT > (thus, the printing
;;; is indirect).  This can be overridden by specifying a keyword :FILE followed
;;; by a string giving and alternate filename or NIL, indicating that the files
;;; should be directly sent to the Dover over the ChaosNet (in this case, files
;;; are printed one at a time).  The keyword :SCALE can be used to scale the
;;; drawings (default is 1) and :BLANK-PAGE (with no argument) is used to
;;; supply an extra blank page at the end (used when the Dover leaves spots on
;;; the last page).  :COPIES can be used to get more than one copy.

;;; (PRINT-TXT-FILE file1 file2...) can be used to print WLR files in LPT6
;;; (it pays attention to DPLT:COPIES too).

;;; Todo: get additional rotated fonts, run scale test again
;;        author name

;;; User variables
(DEFVAR SCALE 1)                                ;The user can change this
(DEFVAR COPIES 1)                               ;number of copies
(DEFVAR SPOOL-FILENAME "MC:.DOVR.;DPLT >")      ;default is to spool
(DEFVAR BLANK-PAGE NIL)                         ;in case the dover is printing bird shit

;;; Constants (should use # for these?)
(DEFCONST MICAS-PER-INCH 2540.)
(DEFCONST DOVER-PAGE-WIDTH (FIX (* 8.5 MICAS-PER-INCH)))        ;8.5 inch page
(DEFCONST DPLT-TO-MICAS-SCALE-FACTOR (// MICAS-PER-INCH 800.0)) ;2540. micas/in / 800. pts/in
(DEFCONST SUDS-CHARACTER-RASTER-TO-MICAS (* .625e-3 MICAS-PER-INCH))    ; 5/8 mil/raster

;;; SUDS Font mapping
;;; Definitely could use more SAIL fonts (e.g. sizes 5,7 9,10,12,14,18,20)

(DEFSTRUCT (DPLT-FONT :LIST (:CONSTRUCTOR NIL))                 ;simple lists
           FNAME FONT-TYPEFACE FONT-SIZE FONT-HEIGHT FONT-WIDTH FONT-INDEX REAL-FONT-WIDTH)

(DEFCONST FONT-TABLE (MAKE-ARRAY '(8 8)))               ; By SUDS Font-number

;;; Candidate fonts if we don't have specific font for SUDS size and style
;;; In order of increasing font-width
(DEFCONST TEXT-FONTS '(("SAIL" "" 6. NIL NIL 0 NIL)
                     ("SAIL" "" 8. NIL NIL 0 NIL)
                     ("Gacha" "" 10. NIL NIL 0 NIL)
                     ("Gacha" "" 12. NIL NIL 0 NIL)
                     ("Helvetica" "" 18. NIL NIL 0 NIL)))

;;; Fonts to use as last resort if character doesn't exist in text-font
(DEFCONST SYMBOL-FONTS '(("SAIL" "" 6. NIL NIL 0 NIL)
                       ("SAIL" "" 8. NIL NIL 0 NIL)))

(DEFCONST DIAMOND-FONT '("Math" "" 10. NIL NIL 0 NIL))
(DEFCONST TITLE-FONT '("Helvetica" "B" 12. NIL NIL 0 NIL))
(DEFCONST TEXT-X-OFFSET -35.)                           ;random bums
(DEFCONST TEXT-Y-OFFSET 18.)

;;; Select a font from a font list, given a DPLT character size
;;; defines font for PRESS if necessary, sets up REAL-FONT-WIDTH and FONT-HEIGHT

(DEFUN SELECT-FONT (CSIZE FONT-NUMBER FONT-STYLE
                    &AUX REAL-WIDTH SYMBOL-FONT NEW-FONT-P
                    (FONT (AREF FONT-TABLE FONT-NUMBER FONT-STYLE)))
  (COND (FONT (NOTICE-FONT FONT 5400.))
        (T (MULTIPLE-VALUE (FONT NEW-FONT-P) (CHOOSE-BEST-FONT CSIZE TEXT-FONTS))
           (AND NEW-FONT-P
                (FORMAT T "~&;;Defaulting ~D mica SUDS font-~O to ~D mica ~A~D~A"
                        CSIZE FONT-NUMBER (REAL-FONT-WIDTH FONT)
                        (FNAME FONT) (FONT-SIZE FONT) (FONT-TYPEFACE FONT)))))
  (SETQ REAL-WIDTH (REAL-FONT-WIDTH FONT))
  (COND ((OR (< CSIZE (* REAL-WIDTH .75)) (> CSIZE (* REAL-WIDTH 1.50)))
         (MULTIPLE-VALUE (FONT NEW-FONT-P) (CHOOSE-BEST-FONT CSIZE TEXT-FONTS))
         (AND NEW-FONT-P
              (FORMAT T "~&;;Replacing ~D mica default SUDS font-~O ~
                        with ~D mica ~A~D~A for font-size ~D"
                      REAL-WIDTH FONT-NUMBER (REAL-FONT-WIDTH FONT)
                      (FNAME FONT) (FONT-SIZE FONT) (FONT-TYPEFACE FONT) CSIZE))))
  ;Use same font for symbols if possible, otherwise choose the best font that has all symbols
  (SETQ SYMBOL-FONT
        (LOOP FOR F IN SYMBOL-FONTS
              WHEN (AND (EQUALP (FNAME F) (FNAME FONT))
                        (EQUALP (FONT-TYPEFACE F) (FONT-TYPEFACE FONT))
                        (EQUAL (FONT-SIZE F) (FONT-SIZE FONT)))
                RETURN FONT
              FINALLY (RETURN (CHOOSE-BEST-FONT CSIZE SYMBOL-FONTS))))
  (SETF (FONT-WIDTH FONT) CSIZE)                ;Force character spacing assumed by SUDS
  (SETF (FONT-WIDTH SYMBOL-FONT) CSIZE)
  (VALUES FONT SYMBOL-FONT))

(DEFUN CHOOSE-BEST-FONT (CSIZE FONT-LIST)
  ;; Choose the largest font that will fit
  (LOOP WITH LAST-F = (FIRST FONT-LIST)
        FOR F IN FONT-LIST
        DO (IF (< CSIZE (GET-REAL-WIDTH F)) (RETURN (NOTICE-FONT LAST-F 5400.)))
               (SETQ LAST-F F)
               FINALLY (RETURN (NOTICE-FONT (CAR (LAST FONT-LIST)) 5400.))))

(DEFUN NOTICE-FONT (FONT ROTATION &AUX FONT-INDEX PRESS-FONT DEFINED-FONTS)
  (IF (FONT-INDEX FONT) FONT
      (SETQ DEFINED-FONTS (LENGTH PRESS:PRESS-FONT-LIST)
            FONT-INDEX (PRESS:PRESS-DEFINE-FONT-FAKE (FNAME FONT) (FONT-TYPEFACE FONT)
                                          (FONT-SIZE FONT) ROTATION)
            PRESS-FONT (NTH FONT-INDEX PRESS:PRESS-FONT-LIST))
      (SETF (FONT-INDEX FONT) FONT-INDEX)
      (SETF (REAL-FONT-WIDTH FONT) (FIFTH PRESS-FONT))
      (SETF (FONT-HEIGHT FONT) (SIXTH PRESS-FONT))
      (VALUES FONT ( DEFINED-FONTS (LENGTH PRESS:PRESS-FONT-LIST)))))

(DEFUN GET-REAL-WIDTH (FONT)
  (COND ((REAL-FONT-WIDTH FONT))
        (T (SETF (REAL-FONT-WIDTH FONT)
                 (CAR (PRESS:GET-FONT-WIDTH-AND-HEIGHT
                        (FNAME FONT) (FONT-TYPEFACE FONT) (FONT-SIZE FONT))))
           (REAL-FONT-WIDTH FONT))))

;;; Define the box frame for the drawing and text such as titles and filename.
(DEFSTRUCT (DPLT-FRAME :LIST (:CONSTRUCTOR NIL))        ;simple list
           FLEFT FRIGHT FTOP FBOTTOM LABEL-TOP DATE-LEFT FILE-LEFT
           TITLE1 TITLE2 DATE FILE BASELINE)    ;these are all locations

(DEFVAR FRAME (MAPCAR #'(LAMBDA(X) (FIX (* MICAS-PER-INCH X)))
                      '(0 10.2 8.2 .4 .7 6.0 8.05
                        .5 3.0 6.14 8.2 .48)))

(DEFVAR LOGO NIL)

(DEFVAR PATHNAME-DEFAULTS (FS:MAKE-PATHNAME-DEFAULTS))

(OR (BOUNDP 'PRESS:FONT-WIDTH-DATA)
    (PRESS:LOAD-FONT-WIDTHS "SYS: PRESS-FONTS; FONTS WIDTHS >"))

;;; HACK FOR PRINTING WLR FILES, ETC.

(DEFUN PRINT-TXT-FILE (&REST FILES)
  (DOLIST (FILE FILES)
    (PRESS:PRINT-FILE (FS:MERGE-AND-SET-PATHNAME-DEFAULTS FILE PATHNAME-DEFAULTS)
                      ':FONT-NAME "LPT" ':FONT-SIZE 6 ;:PAGE-HEADINGS T
                      ':COPIES DPLT:COPIES ':SPOOL "MC:.DOVR.;DTXT >")))

(DEFF PRESS:DPLT-PRINT-FILE 'PRINT-FILE)

(DEFUN PRINT-FILE (&REST FILE-LIST
                   &AUX (SCALE SCALE) (SPOOL-FILENAME SPOOL-FILENAME)
                   (BLANK-PAGE BLANK-PAGE) (COPIES COPIES))
  (LOOP WITH NEW-FILE-LIST = NIL
        FOR L ON FILE-LIST
        DO (IF (NOT (SYMBOLP (CAR L)))
               (SETQ NEW-FILE-LIST (PUSH (FS:MERGE-AND-SET-PATHNAME-DEFAULTS (CAR L)
                                                                     PATHNAME-DEFAULTS
                                                                     "PLT")
                                         NEW-FILE-LIST))
               (SELECTQ (CAR L)
                   (:SCALE (SETQ SCALE (CADR L)))
                   (:COPIES (SETQ COPIES (CADR L)))
                   (:FILE  (SETQ SPOOL-FILENAME
                                 (AND (CADR L)
                                      (FS:MERGE-PATHNAME-DEFAULTS (CADR L)
                                                                          SPOOL-FILENAME))))
                   (:BLANK-PAGE (SETQ BLANK-PAGE T))
                   (T (FERROR NIL "~%~A Unknown keyword: DPLT:PRINT-FILE" (CAR L))))
               (SETQ L (REST1 L)))
        FINALLY (SETQ FILE-LIST (NREVERSE NEW-FILE-LIST)))
  (IF (NULL SPOOL-FILENAME) (PRESS:PRINT-DOVER-STATUS)) ;Let the user know the possibilities..
  (PRESS:BIND-PRESS-VARIABLES
    (SETQ PRESS:DOVER-X0 0.
          PRESS:DOVER-Y2 1100.                  ;Y0 = .4 real margin x 2540.
          PRESS:LINE-WIDTH 20.)
    (UNWIND-PROTECT
      (PROGN
        (IF SPOOL-FILENAME (START-OUTPUT))
        (DOLIST (FILE FILE-LIST)
          (IF (NULL (PROBEF FILE)) (FORMAT T "~% File ~S not found, continuing..." FILE)
              (IF (NULL SPOOL-FILENAME) (START-OUTPUT))
              (PRESS:PRESS-START-PAGE)
              (OUTPUT-FILE FILE)
              (PRESS:PRESS-END-PAGE)
              (IF (NULL SPOOL-FILENAME) (END-OUTPUT))))
        (IF SPOOL-FILENAME (END-OUTPUT)))
      (IF SPOOL-FILENAME (CLOSE PRESS:PRESS-EFTP-STREAM ':ABORT)))))

(GLOBALIZE 'DPLT-PRINT-FILE)
(FSET 'DPLT-PRINT-FILE 'PRINT-FILE)
(GLOBALIZE 'PRINT-TXT-FILE)

(DEFUN START-OUTPUT ()
  (PRESS:PRESS-START-FILE (OR SPOOL-FILENAME PRESS:DOVER-ADDRESS))
  ;; undefine fonts
  (SETQ PRESS:PRESS-FONT-LIST NIL)
  ;; unassign all font selections
  (DOLIST (FONT-LIST `(,TEXT-FONTS ,SYMBOL-FONTS (,TITLE-FONT ,DIAMOND-FONT)))
    (DOLIST (F FONT-LIST)
      (SETF (FONT-INDEX F) NIL)))
  (DOTIMES (FONT 8)
    (DOTIMES (FACE 8)
      (LET ((F (AREF FONT-TABLE FONT FACE)))
        (AND F (SETF (FONT-INDEX F) NIL)))))
  ;; define some initial fonts we'll always need
  (NOTICE-FONT TITLE-FONT 5400.)                ;rotate 90.
  (NOTICE-FONT DIAMOND-FONT 0))

(DEFUN END-OUTPUT ()
  (IF BLANK-PAGE
      (PROGN (PRESS:PRESS-START-PAGE)                   ;blank page for bird shits
             (PRESS:PRESS-END-PAGE)))
  (PRESS:PRESS-END-FILE "SUDS Plots" (TIME:PRINT-CURRENT-TIME NIL) COPIES))

;;; PLT files are stored in a 36-bit format, so do the best we can...
;;; Read in 18. bit bytes (so they remain as small integers)

(DEFMACRO IN18 (STREAM)                         ;18. bit input, 9 bit bytes
  `(+ (ASH (FUNCALL ,STREAM ':TYI) 9.) (FUNCALL ,STREAM ':TYI)))

;;; Occassionally need 36. bits at a time here

(DEFMACRO IN36 (STREAM)
  `(+ (ASH (IN18 ,STREAM) 18.) (IN18 ,STREAM)))

(DEFVAR STREAM)
(DEFVAR PASS)                                   ;Indicates sizing pass or printing pass
(DEFVAR X-OFFSET) (DEFVAR Y-OFFSET)             ;initialized during prescan
(DEFVAR MIN-X) (DEFVAR MIN-Y) (DEFVAR MAX-X) (DEFVAR MAX-Y)
(DEFVAR VERSION)                                ;file version number.

;;; Process a DPLT file.  Since plots are not centered, must prescan to find
;;; the range of the plot to compute its offsets.

(DEFUN OUTPUT-FILE (FILE)
  (WITH-OPEN-FILE (STREAM (FUNCALL (FS:PARSE-PATHNAME FILE) ':NEW-TYPE "PLT")
                          '(:IN :FIXNUM :BYTE-SIZE 9.))
    (SETQ VERSION (IN36 STREAM))
    (COND ((< VERSION #o20)                     ;does file have bounding box?
           (SETQ X-OFFSET 0 Y-OFFSET 0          ;no, prescan to determine offsets
                 MIN-X  1_16.                   ;init to largest and smallest possible
                 MIN-Y  1_16.
                 MAX-X  -1_16.
                 MAX-Y  -1_16.)
           (PROCESS-FILE 'PASS1)                ;prescan to find max, min
           (FUNCALL STREAM ':SET-POINTER 0)     ;backup for reread
           (IN36 STREAM))                       ;skip version number on pass2
          (T (SETQ MIN-X (DPLT-TO-MICAS (IN18 STREAM))
                   MIN-Y (DPLT-TO-MICAS (IN18 STREAM))
                   MAX-X (DPLT-TO-MICAS (IN18 STREAM))
                   MAX-Y (DPLT-TO-MICAS (IN18 STREAM)))))
    (SETQ X-OFFSET (- (TRUNCATE (+ (FLEFT FRAME) (FRIGHT FRAME)) 2)
                      (TRUNCATE (+ MAX-X MIN-X) 2))
          Y-OFFSET (- (TRUNCATE (+ (LABEL-TOP FRAME) (FTOP FRAME)) 2)
                      (TRUNCATE (+ MAX-Y MIN-Y) 2)))
    (PROCESS-FILE 'PASS2)
    (DRAW-FRAME-AND-TITLE)))

(DEFUN PROCESS-FILE (PASS &AUX X Y TYPE LH RH)
  (IF (> VERSION 16)
      (LOOP FOR STRING = (READ-ASCIZ-STRING)
            UNTIL (EQUAL STRING "")
            AS FONT = (IN18 STREAM) AND POINT-SIZE = (IN18 STREAM)
            AND WIDTH = (IN18 STREAM) AND HEIGHT = (IN18 STREAM)
            DO (LOOP FOR STYLE IN '("" "I" "B" "BI" "V" "VI" "VB" "VBI")
                     AS STYLE-NO FROM 0
                     DO (ASET (LIST STRING STYLE POINT-SIZE NIL NIL NIL NIL)
                              FONT-TABLE FONT STYLE-NO))))
  (LOOP INITIALLY (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ)) ;first item
        UNTIL (EQ 'END TYPE)
        DO (SELECTQ TYPE                        ;each DRAW function returns next values
             (VECTOR  (MULTIPLE-VALUE (X Y TYPE LH RH) (DRAW-VECTORS X Y)))
             (TEXT    (MULTIPLE-VALUE (X Y TYPE LH RH) (DRAW-TEXT X Y RH)))     ; RH has size
             (DIAMOND (MULTIPLE-VALUE (X Y TYPE LH RH) (DRAW-DIAMOND X Y)))
             (OTHERWISE (FERROR NIL "~% DPLT Lossage, shouldn't get here")))))

;;; Read a 36. bit word (IN18 2 halves) and decode the item type.
;;; Read and translate coordinates.

(DEFUNP DPLT-READ (&AUX (LH (IN18 STREAM)) (RH (IN18 STREAM))
                        (X (+ (DPLT-TO-MICAS LH) X-OFFSET))
                        (Y (+ (DPLT-TO-MICAS RH) Y-OFFSET)))
  (RETURN X Y (COND ((= 400001 LH) 'END)        ;end of display items
                    ((BIT-TEST 1 RH) 'CONTINUE) ;proceed in the current mode
                    ((NOT (BIT-TEST 1 LH)) 'VECTOR)
                    (T (SETQ LH (IN18 STREAM) RH (IN18 STREAM)) ;read both halves
                       (SELECTQ LH              ;decode off LH
                         (0 'TEXT)
                         (2 'DIAMOND)           ;dont hack pads...
                         (OTHERWISE (FERROR NIL "~% Invalid data in file.")))))
          LH RH))                               ;Return this for some commands to reparse

;;; Convert 17. bit numbers which are left adjusted in 18. bits and
;;; are in 2's complement form, rather than LISPM number form.

(DEFUN DPLT-TO-MICAS (RAW)
  (IF (BIT-TEST 1_17. RAW) (SETQ RAW (- (1+ (LOGXOR 777777 RAW)))))     ;convert sign
  (FIX (* DPLT-TO-MICAS-SCALE-FACTOR (ASH RAW -1))))

;;; Sort of a crock - rotate the page and do the user's scaling

(DEFMACRO DPLT-TO-DOVER (X Y)
  `(LET ((SAVE-X ,X))
     (SETQ ,X (FIXR (* SCALE (- DOVER-PAGE-WIDTH ,Y)))
           ,Y (FIXR (* SCALE SAVE-X)))))

;;; Draw a line, converting to rotated Dover coords

(DEFUN LINE (X1 Y1 X2 Y2)
  (DPLT-TO-DOVER X1 Y1)
  (DPLT-TO-DOVER X2 Y2)
  (PRESS:PRESS-LINE X1 Y1 X2 Y2))

;;; Update the range of the plot (used during PASS1 scan of the file)

(DEFUN MIN-MAX-TEST (X Y)
  (IF (< X MIN-X) (SETQ MIN-X X))
  (IF (> X MAX-X) (SETQ MAX-X X))
  (IF (< Y MIN-Y) (SETQ MIN-Y Y))
  (IF (> Y MAX-Y) (SETQ MAX-Y Y)))


;;; DRAWING routines for various plot items

;;; Vector Mode (loop until mode changes)

(DEFUNP DRAW-VECTORS (X Y &AUX TYPE LH RH)
  (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y))
  (LOOP FOR LAST-X = X THEN X AND LAST-Y = Y THEN Y
        DO  (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ))
        WHILE (MEMQ TYPE '(CONTINUE VECTOR))
        DOING (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y)
                  (IF (NOT (EQ TYPE 'VECTOR))   ;if VECTOR, just a set point
                      (LINE LAST-X LAST-Y X Y))))
  (RETURN X Y TYPE LH RH))


(DEFUNP DRAW-TEXT (X Y RH &AUX LH TYPE
                   TEXT-FONT SYMBOL-FONT FONT-WIDTH FONT-HEIGHT CURRENT-FONT
                   (X0 X)
                   (FONT-STYLE (LDB 1703 RH))           ; Vertical, Bold, Italic bits
                   (FONT-NUMBER (LDB 1403 RH))          ;SUDS character size
                   (CSIZE (FIX (* (LDB 0113 RH) SUDS-CHARACTER-RASTER-TO-MICAS))))
                                                        ;SUDS Width is in .625 mil units
  (MULTIPLE-VALUE (TEXT-FONT SYMBOL-FONT)
    (SELECT-FONT CSIZE FONT-NUMBER FONT-STYLE))
  (SETQ FONT-HEIGHT (FONT-HEIGHT TEXT-FONT)
        FONT-WIDTH (FONT-WIDTH TEXT-FONT)
        CURRENT-FONT TEXT-FONT)
  (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y)
      (PRESS:PRESS-SELECT-FONT (FONT-INDEX CURRENT-FONT)))
  (LOOP WITH X1 = (+ X TEXT-X-OFFSET) AND Y1 = (+ Y TEXT-Y-OFFSET)
        DO (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ))
        WHILE (EQ TYPE 'CONTINUE)
        DOING
          (LOOP WITH WORD = (+ (ASH LH 18.) RH)
                FOR BP FROM 3507 DOWNTO 0107 BY 0700
                FOR CH = (LDB BP WORD) THEN (LDB BP WORD)
                UNLESS (ZEROP CH)
                DO
                 (SELECTQ CH
                   (#\CR (SETQ X1 X0 Y1 (- Y1 FONT-HEIGHT)) (MIN-MAX-TEST X1 Y1))
                   (T (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X1 (+ Y1 FONT-HEIGHT))
                          (LET ((X X1)(Y Y1) (FONT (IF (> CH 37) TEXT-FONT SYMBOL-FONT)))
                            (OR (EQ FONT CURRENT-FONT)
                                (PRESS:PRESS-SELECT-FONT
                                 (FONT-INDEX (SETQ CURRENT-FONT FONT))))
                            (DPLT-TO-DOVER X Y)
                            (PRESS:PRESS-SET-CURSOR X Y)
                            (PRESS:PRESS-CHAR CH)))
                      (SETQ X1 (+ X1 FONT-WIDTH))))))
  (RETURN X Y TYPE LH RH))

(DEFUNP DRAW-DIAMOND (X Y &AUX TYPE LH RH)
  (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y)
      (DPLT-TO-DOVER X Y)                       ;otherwise, on PASS2 print a diamond
      (PRESS:PRESS-SELECT-FONT (FONT-INDEX DIAMOND-FONT))
      (PRESS:PRESS-SET-CURSOR (- X 70.) (- Y 130.))
      (PRESS:PRESS-CHAR 017))
  (LOOP DO  (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ))       ;shouldn't be needed...
        WHILE (EQ 'CONTINUE TYPE))
  (RETURN X Y TYPE LH RH))

;;; Draw the frame and plot titles

(DEFUN DRAW-FRAME-AND-TITLE (&AUX
     (AUTHOR-LIST (READ-ASCIZ-STRING))
     (TITLE1      (READ-ASCIZ-STRING))
     (TITLE2      (READ-ASCIZ-STRING))
     (BINARY-DATE (IN18 STREAM))
     (BINARY-TIME (IN18 STREAM))
     (IGNORE  (IN36 STREAM))            ;FN1 in sixbit
     (IGNORE  (IN36 STREAM))            ;FN2 in sixbit
     (IGNORE  (IN36 STREAM))            ;DIR in sixbit
     (DATE-TIME   (FORMAT NIL "~D-~[JAN~;FEB~;MAR~;APR~;MAY~;JUN~;JUL~;AUG~;SEP~;OCT~;NOV~;DEC~]-~D  ~2,'0D:~2,'0D"
                          (1+ (\ BINARY-DATE 31.))
                          (\ (TRUNCATE BINARY-DATE 31.) 12.)
                          (+ 1964. (TRUNCATE BINARY-DATE (* 12. 31.)))
                          (TRUNCATE BINARY-TIME 60.)
                          (\ BINARY-TIME 60.)))
     ;; We don't care about the file type or version.
     (FILE-NAME (FUNCALL (FUNCALL (FUNCALL (FUNCALL STREAM ':PATHNAME) ':NEW-TYPE NIL)
                                  ':GENERIC-PATHNAME)
                         ':STRING-FOR-PRINTING))
     (LEFT   (FLEFT FRAME))
     (RIGHT  (FRIGHT FRAME))
     (TOP    (FTOP FRAME))
     (BOTTOM (FBOTTOM FRAME))
     (LTOP   (LABEL-TOP FRAME))
     (DATEL  (DATE-LEFT FRAME))
     (FILEL  (FILE-LEFT FRAME))
     (BASEL  (BASELINE FRAME))
     (PRESS:LINE-WIDTH (* 2 PRESS:LINE-WIDTH))) ;doubly thick lines here
  (LINE LEFT  BOTTOM LEFT  TOP)
  (LINE LEFT  TOP    RIGHT TOP)
  (LINE RIGHT TOP    RIGHT BOTTOM)
  (LINE RIGHT BOTTOM LEFT  BOTTOM)
  (LINE LEFT  LTOP   RIGHT  LTOP)
  (LINE DATEL LTOP   DATEL BOTTOM)
  (LINE FILEL LTOP   FILEL BOTTOM)
  (IF LOGO (DRAW-LOGO LOGO RIGHT LTOP))
  (PRESS:PRESS-SELECT-FONT (FONT-INDEX TITLE-FONT))
  (SETQ AUTHOR-LIST AUTHOR-LIST)                ;maybe print this someday
  (PRINT-STRING TITLE1 (TITLE1 FRAME) BASEL)
  (PRINT-STRING TITLE2 (TITLE2 FRAME) BASEL)
  (PRINT-STRING DATE-TIME (DATE FRAME) BASEL)
  (PRINT-STRING FILE-NAME (+ (FILE FRAME) (* (- 17. (STRING-LENGTH FILE-NAME)) 180.)) BASEL))

(DEFUN DRAW-LOGO (POLYLINES XORG YORG)  ;Origin is lower-right corner
  (DOLIST (POLYLINE POLYLINES)
    (DO ((XORG (- XORG (FIX (* MICAS-PER-INCH (FIRST POLYLINE)))))
         (YORG (+ YORG (FIX (* MICAS-PER-INCH (SECOND POLYLINE)))))
         (SCAL (THIRD POLYLINE))
         (X0 (FOURTH POLYLINE) X1)
         (Y0 (FIFTH POLYLINE) Y1)
         (L (NTHCDR 5 POLYLINE) (CDDR L))
         (X1) (Y1))
        ((NULL L))
      (SETQ X1 (FIRST L) Y1 (SECOND L))
      (LINE (+ (FIX (* MICAS-PER-INCH SCAL X0)) XORG)
            (+ (FIX (* MICAS-PER-INCH SCAL Y0)) YORG)
            (+ (FIX (* MICAS-PER-INCH SCAL X1)) XORG)
            (+ (FIX (* MICAS-PER-INCH SCAL Y1)) YORG)))))

(DEFUN READ-ASCIZ-STRING ()
  (APPLY 'STRING-APPEND
         (LOOP FOR WORD = (IN36 STREAM) THEN (IN36 STREAM)
               NCONCING (LOOP FOR BP FROM 3507 DOWNTO 0107 BY 0700
                              FOR CH = (LDB BP WORD) THEN (LDB BP WORD)
                              UNLESS (ZEROP CH)
                              COLLECTING CH)
               UNTIL (ZEROP (LDB 0107 WORD)))))

(DEFUN PRINT-STRING (STRING X Y)
  (DPLT-TO-DOVER X Y)
  (PRESS:PRESS-SET-CURSOR X Y)
  (PRESS:PRESS-STRING STRING))

(DEFUN SIXBIT-TO-STRING (NUM)
  (DO ((PPSS 3606 (- PPSS 0600))
       (L NIL))
      ((MINUSP PPSS)
       (DO () ((OR (NULL L) (NOT (= (CAR L) 40))))
         (SETQ L (CDR L)))
       (APPLY #'STRING-APPEND (NREVERSE L)))
   (PUSH (+ (LDB PPSS NUM) 40) L)))
