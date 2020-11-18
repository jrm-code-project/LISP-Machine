;;; -*-Mode:LISP; Package:FED; Base:8; readtable: ZL-*-
;;;  * (c) Copyright 1980 Massachusetts Institute of Technology **

;The functions in this file
;are used to convert between the various formats for fonts as used on the LISP
;Machine.  These are the formats currently supported in some way:
;       KST format is used for communication with the PDP-10.
;       FD (or Font Descriptor) Format is used as a machine resident format
;               which is easily manipulated.  The format consists of a 200
;               or more element array with a leader.  The elements of this array are
;               themselves two dimensional arrays which contain the actual
;               pixel values for the character.
;       FONT (or internal) Format is the format actually used by the tv display
;               routines.  The format is fairly complicated and its direct
;               use is not recommended when a conversion to FD format would
;               be better.
;       AL format is used for ALTO fonts.
;       AC another xerox format.
;       KS kerned strike xerox format.
;       AST stars and spaces.

;Conversion functions:
; FONT-INTO-FONT-DESCRIPTOR FONTNAME => FONT-DESCRIPTOR
;  (you might really want FONT-NAME-FONT-DESCRIPTOR, which remembers the FONT-DESCRIPTOR)
; FONT-DESCRIPTOR-INTO-FONT FONT-DESCRIPTOR => FONTNAME
; READ-X-INTO-FONT FILENAME &OPTIONAL FONTNAME => FONTNAME
; READ-X-INTO-FONT-DESCRIPTOR FILENAME &OPTIONAL FONTNAME => FONT-DESCRIPTOR
; WRITE-FONT-INTO-X FONTNAME &OPTIONAL FILENAME => OUTPUT-TRUENAME
; WRITE-FONT-DESCRIPTOR-INTO-X FONT-DESCRIPTOR &OPTIONAL FILENAME => OUTPUT-TRUENAME

;Other useful functions in here:
; THICKEN-FONT[-DESCRIPTOR], makes B(old) fonts.
; ROTATE-FONT[-DESCRIPTOR], makes R(otated) fonts.  I.e. landscape from portrait.

; CD-RASTER-RANGE CHAR-DESCRIPTOR => MINX MINY MAXX MAXY
; CD-UNUSED-RASTER-HEIGHT CHAR-DESCRIPTOR => BOTTOM TOP

;First some helping functions:

(DEFUN MAX-RASTER-WIDTH (FONT-DESCRIPTOR &AUX (GUESS 0) TEMP)
  "Return the maximum raster width of all characters in FONT-DESCRIPTOR."
  (DO ((CHAR-CODE 0 (1+ CHAR-CODE))
       (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FONT-DESCRIPTOR)))
      (( CHAR-CODE FONT-LENGTH) GUESS)
    (COND ((SETQ TEMP (AREF FONT-DESCRIPTOR CHAR-CODE))
           (SETQ GUESS (MAX GUESS (ARRAY-DIMENSION TEMP 1)))))))

(DEFUN MAX-RASTER-HEIGHT (FONT-DESCRIPTOR &AUX (GUESS 0) TEMP)
  "Return the maximum raster height of all characters in FONT-DESCRIPTOR."
  (DO ((CHAR-CODE 0 (1+ CHAR-CODE))
       (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FONT-DESCRIPTOR)))
      (( CHAR-CODE FONT-LENGTH) GUESS)
    (COND ((SETQ TEMP (AREF FONT-DESCRIPTOR CHAR-CODE))
           (SETQ GUESS (MAX GUESS (ARRAY-DIMENSION TEMP 0)))))))

;;; Memoizing version of FONT-INTO-FONT-DESCRIPTOR
;;; that wants a font name (symbol in FONTS:) rather than the font itself.
;;; The FONT-DESCRIPTOR property of the symbol holds the descriptor.
;;; The FONT-DESCRIBED property holds the font itself which the descriptor matches.
;;; If anyone changes the font, we can see that the old descriptor is no good.
(DEFUN FONT-NAME-FONT-DESCRIPTOR (FONTNAME &AUX FD)
  "Return a font-descriptor whose data is equivalent to font FONTNAME.
If we computed one previously, we return it; otherwise, we create one.
This assumes that anyone who alters the contents of the font
also alters the corresponding font descriptor."
  (OR (SYMBOLP FONTNAME)
      (SETQ FONTNAME (FONT-NAME FONTNAME)))
  (SETQ FD (GET FONTNAME 'FONT-DESCRIPTOR))
  (COND ((AND FD (EQ (GET FONTNAME 'FONT-DESCRIBED) (SYMEVAL FONTNAME))))
        (T (SETQ FD (FONT-INTO-FONT-DESCRIPTOR (SYMEVAL FONTNAME)))
           (PUTPROP FONTNAME (SYMEVAL FONTNAME) 'FONT-DESCRIBED)
           (PUTPROP FONTNAME FD 'FONT-DESCRIPTOR)))
  FD)

;;; Set a font given a font descriptor.  Keep the descriptor around.
;;; Forward the old definition of the font to the new one.
(DEFUN FONT-NAME-SET-FONT-AND-DESCRIPTOR (FONTNAME FONT-DESCRIPTOR)
  "Define or redefine a font named FONTNAME from data in FONT-DESCRIPTOR."
  (LET ((OLDFONT (AND (BOUNDP FONTNAME) (SYMEVAL FONTNAME))))
    (SET FONTNAME (FONT-DESCRIPTOR-INTO-FONT FONT-DESCRIPTOR))
    (AND OLDFONT (STRUCTURE-FORWARD OLDFONT (SYMEVAL FONTNAME)))
    (PUTPROP FONTNAME FONT-DESCRIPTOR 'FONT-DESCRIPTOR)
    (PUTPROP FONTNAME (SYMEVAL FONTNAME) 'FONT-DESCRIBED)
    FONT-DESCRIPTOR))

(DEFUN FONT-NAME-STORE-CD (FONTNAME CD CHAR-CODE &AUX FONT)
  "Store character CHAR-CODE in font named FONTNAME from data in CD.
CD should be a CHARACTER-DESCRIPTOR.  Both the font itself
and any remembered font-descriptor are modified.
A new font array is constructed if the new character's raster size
is too big for the old one."
  (LET ((WIDTH (ARRAY-DIMENSION CD 1))
        (HEIGHT (ARRAY-DIMENSION CD 0))
        TEM FD)
    (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME))
    (FD-STORE-CD FD CD CHAR-CODE)
    (AND (= CHAR-CODE #\SP)
         (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD)))
    (COND ((OR (NOT (BOUNDP FONTNAME))
               (NULL (SETQ FONT (SYMEVAL FONTNAME)))
               ( CHAR-CODE (MAX (OR (FONT-FILL-POINTER FONT) 200) 200))
               (> WIDTH
                  (COND ((SETQ TEM (FONT-INDEXING-TABLE FONT))
                         (* (FONT-RASTER-WIDTH FONT)
                            (- (AREF TEM (1+ CHAR-CODE))
                               (AREF TEM CHAR-CODE))))
                        (T (FONT-RASTER-WIDTH FONT))))
               (> HEIGHT (FONT-RASTER-HEIGHT FONT)))
           (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD))
          (T (STORE-CD-IN-FONT CD FONT CHAR-CODE NIL)))))

(DEFUN FD-STORE-CD (FD CD CH)
  "Store character-descriptor CD as character CH in font descriptor FD."
  (AND ( CH (ARRAY-LENGTH FD))
       (ADJUST-ARRAY-SIZE FD (+ CH 100)))
  (AND ( CH (FD-FILL-POINTER FD))
       (SETF (FD-FILL-POINTER FD) (1+ CH)))
  (ASET CD FD CH))

;Functions for referring to specified pixels of characters in an internal format font.

;ROW and COL are measured from top/left as usual.  An alternative would be:
;       COL is measured from the left, with Kerning hacked.
;       ROW is positive above the baseline and negative below.
;  (SETQ ROW (- (FONT-BASELINE FONT) ROW))
;  (AND (SETQ TEM (FONT-LEFT-KERN-TABLE FONT))
;       (SETQ COL (+ COL (AREF TEM CHAR))))
;However it looks like this would cause more trouble than it would save.
;Attempts to reference outside of the raster return 0, or barf if storing.
;Conceivably it might be good to not barf at attempts to store 0 out of bounds?

(DEFUN FONT-GET-PIXEL (FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  "Get the pixel at position ROW, COL in character CHAR of FONT.
FONT should be a font array, not a name."
  (COND ((OR (< ROW 0)
             ( ROW (FONT-RASTER-HEIGHT FONT))
             (< COL 0)
             (COND ((SETQ TEM (FONT-INDEXING-TABLE FONT))
                    (SETQ CHAR (+ (AREF TEM CHAR) (TRUNCATE COL (FONT-RASTER-WIDTH FONT))))
                    (SETQ COL (\ COL (FONT-RASTER-WIDTH FONT)))
                    ( CHAR (AREF TEM NEXTCHAR)))
                   (( COL (FONT-RASTER-WIDTH FONT)))))
         0)     ;out of bounds, return 0
        (T
         (DO ((FONT FONT (FONT-NEXT-PLANE FONT))
              (PIXEL 0)
              (PLANENUM 0 (1+ PLANENUM)))
             ((NULL FONT) PIXEL)
           (SETQ PIXEL
                 (+ PIXEL (LSH (AREF FONT
                                     (+ (* 32. (+ (* (FONT-WORDS-PER-CHAR FONT) CHAR)
                                                  (TRUNCATE ROW (FONT-RASTERS-PER-WORD FONT))))
                                        (+ (* (FONT-RASTER-WIDTH FONT)
                                              (\ ROW (FONT-RASTERS-PER-WORD FONT)))
                                           COL)))
                               PLANENUM)))))))

(DEFUN FONT-SET-PIXEL (PIXEL FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  "Set the pixel at position ROW, COL in character CHAR of font FONT to PIXEL.
FONT should be a font array, not a name."
  (COND ((OR (< ROW 0)
             ( ROW (FONT-RASTER-HEIGHT FONT))
             (< COL 0)
             (COND ((SETQ TEM (FONT-INDEXING-TABLE FONT))
                    (SETQ CHAR (+ (AREF TEM CHAR) (TRUNCATE COL (FONT-RASTER-WIDTH FONT))))
                    (SETQ COL (\ COL (FONT-RASTER-WIDTH FONT)))
                    ( CHAR (AREF TEM NEXTCHAR)))
                   (( COL (FONT-RASTER-WIDTH FONT)))))
         (FERROR NIL "Store of ~C in ~S at ~O,~O out of character bounds" CHAR FONT ROW COL))
        (T
         (DO ((FONT FONT (FONT-NEXT-PLANE FONT))
              (BIT PIXEL (LSH BIT -1)))
             ((NULL FONT) PIXEL)
             (ASET BIT FONT
                   (+ (* 32. (+ (* (FONT-WORDS-PER-CHAR FONT) CHAR)
                                (TRUNCATE ROW (FONT-RASTERS-PER-WORD FONT))))
                      (+ (* (FONT-RASTER-WIDTH FONT)
                            (\ ROW (FONT-RASTERS-PER-WORD FONT)))
                         COL)))))))

;This function takes an FD format font and creates an internal format
;       font from it.  All of the hairy formats of the stored font
;       are taken care of by this function so the user doesn't have
;       to worry about them.

(DEFUN FONT-DESCRIPTOR-INTO-FONT
       (FONT-DESCRIPTOR
        &OPTIONAL (NBR-PLANES-OUT NIL)
        &AUX (FONT-OUT NIL)
        (FONT-DESCRIPTOR-LENGTH (ARRAY-ACTIVE-LENGTH FONT-DESCRIPTOR))
        (FONT-LENGTH (MAX FONT-DESCRIPTOR-LENGTH 200))
        (COL-INCR (COND ((FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR) 2)
                        (T 1)))
        (SPACE-WIDTH (OR (FIX (+ (FD-SPACE-WIDTH FONT-DESCRIPTOR) 0.5)) 0))
        (WIDTH (TRUNCATE SPACE-WIDTH COL-INCR))
        (HEIGHT (FD-LINE-SPACING FONT-DESCRIPTOR))
        (BASELINE (FD-BASELINE FONT-DESCRIPTOR))
        (RASTER-WIDTH (MAX 1 (CEILING (MAX-RASTER-WIDTH FONT-DESCRIPTOR)
                                      COL-INCR)))
        (RASTER-HEIGHT (MAX-RASTER-HEIGHT FONT-DESCRIPTOR))
        (RASTERS-PER-WORD (TRUNCATE 32. (MIN 32. RASTER-WIDTH)))
        (WORDS-PER-RASTER-ELEMENT (CEILING RASTER-HEIGHT RASTERS-PER-WORD))
        (TOTAL-RASTER-ELEMENTS FONT-LENGTH)
        (BLINKER-WIDTH (FLOOR (FD-BLINKER-WIDTH FONT-DESCRIPTOR) COL-INCR))
        (BLINKER-HEIGHT (FD-BLINKER-HEIGHT FONT-DESCRIPTOR))
        (INDEXING-TABLE NIL)
        (CHARS-EXIST-TABLE (MAKE-ARRAY FONT-LENGTH ':TYPE 'ART-1B))
        TEMP                                    ;General temporary
        )
  "Create a font-array from font-descriptor FONT-DESCRIPTOR.
Its name is set from that in FONT-DESCRIPTOR."
  ;;Set up NBR-PLANES-OUT if defaulted
  (COND ((NULL NBR-PLANES-OUT)
         (SETQ NBR-PLANES-OUT COL-INCR)))
  ;;Create INDEXING-TABLE if needed
  (COND ((> RASTER-WIDTH 32.)
         (SETQ INDEXING-TABLE (MAKE-ARRAY (1+ FONT-LENGTH) ':TYPE 'ART-16B))
         (ASET 0 INDEXING-TABLE 0)
         (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
             (( CHAR-CODE FONT-LENGTH)
              (SETQ TOTAL-RASTER-ELEMENTS (AREF INDEXING-TABLE FONT-LENGTH)))
           (SETQ TEMP (AND (< CHAR-CODE FONT-DESCRIPTOR-LENGTH)
                           (AREF FONT-DESCRIPTOR CHAR-CODE)))
           (ASET (+ (AREF INDEXING-TABLE CHAR-CODE)
                    (COND ((NULL TEMP) 0)
                          (T (CEILING (ARRAY-DIMENSION TEMP 1) 32.))))
                 INDEXING-TABLE (1+ CHAR-CODE)))
         (SETQ RASTER-WIDTH 32.)))
  ;;set up all the planes of the font
  (DO ((I NBR-PLANES-OUT (1- I)))
      ((ZEROP I))
    ;;Make up a (one-plane) font and make it's next plane be the last one we made
    (SETQ TEMP (TV:MAKE-FONT :MAKE-ARRAY (:TYPE 'ART-1B
                                               :LENGTH (* TOTAL-RASTER-ELEMENTS
                                                          WORDS-PER-RASTER-ELEMENT 32.))))
    (SETF (FONT-NEXT-PLANE TEMP) FONT-OUT)
    (SETQ FONT-OUT TEMP)
    ;;Now set all the other fields in the leader
    (SETF (FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
    (SETF (FONT-CHAR-WIDTH FONT-OUT) WIDTH)
    (SETF (FONT-CHAR-HEIGHT FONT-OUT) HEIGHT)
    (SETF (FONT-RASTER-WIDTH FONT-OUT) RASTER-WIDTH)
    (SETF (FONT-RASTER-HEIGHT FONT-OUT) RASTER-HEIGHT)
    (SETF (FONT-RASTERS-PER-WORD FONT-OUT) RASTERS-PER-WORD)
    (SETF (FONT-WORDS-PER-CHAR FONT-OUT) WORDS-PER-RASTER-ELEMENT)
    (SETF (FONT-BASELINE FONT-OUT) BASELINE)
    (SETF (FONT-BLINKER-WIDTH FONT-OUT) BLINKER-WIDTH)
    (SETF (FONT-BLINKER-HEIGHT FONT-OUT) BLINKER-HEIGHT)
    (SETF (FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
    (SETF (FONT-CHARS-EXIST-TABLE FONT-OUT) CHARS-EXIST-TABLE)
    (SETF (FONT-INDEXING-TABLE FONT-OUT) INDEXING-TABLE)
    (SETF (FONT-FILL-POINTER FONT-OUT) FONT-LENGTH))
  (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
      (( CHAR-CODE FONT-LENGTH))
    (SETQ TEMP (AND (< CHAR-CODE FONT-DESCRIPTOR-LENGTH)
                    (AREF FONT-DESCRIPTOR CHAR-CODE)))
    (COND (TEMP
           (STORE-CD-IN-FONT TEMP FONT-OUT CHAR-CODE
                             (FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR)))))
  FONT-OUT)

;Store the data in CD into character number CHAR-CODE of FONT.
;It is assumed that the dimensions of the CD fit within the raster dimensions of the font.
;This is not recommended for users to call.
(DEFUN STORE-CD-IN-FONT (CD FONT CHAR-CODE &OPTIONAL (DOUBLE-WIDTH-P NIL) &AUX
                         (FONT-LENGTH (FONT-FILL-POINTER FONT))
                         (WIDTH (ARRAY-DIMENSION CD 1))
                         (HEIGHT (ARRAY-DIMENSION CD 0))
                         (FONT-HEIGHT (FONT-RASTER-HEIGHT FONT))
                         (FONT-WIDTH (FONT-RASTER-WIDTH FONT))
                         PIXEL (LOSING-KERN 0)
                         (COL-INCR (COND (DOUBLE-WIDTH-P 2) (T 1))))
  (OR (AND FONT-LENGTH ( FONT-LENGTH 200))
      (SETQ FONT-LENGTH 200))
  ;; Update the font's char-width-table, creating one if necessary.
  (LET ((CW (CEILING (ROUND (CD-CHAR-WIDTH CD)) COL-INCR))
        (FCW (FONT-CHAR-WIDTH FONT))
        (FCWT (FONT-CHAR-WIDTH-TABLE FONT)))
    (COND (FCWT
           (ASET CW FCWT CHAR-CODE))
          ((NOT (= CW FCW))
           (SETF (FONT-CHAR-WIDTH-TABLE FONT)
                 (SETQ FCWT (MAKE-ARRAY FONT-LENGTH)))
           (AND DOUBLE-WIDTH-P
                (SETF (FONT-CHAR-WIDTH-TABLE (FONT-NEXT-PLANE FONT))
                      FCWT))
           (DO I 0 (1+ I) (= I FONT-LENGTH)
               (ASET FCW FCWT I))
           (ASET CW FCWT CHAR-CODE)))
    (AND (= CHAR-CODE #\SP)
         (SETF (FONT-CHAR-WIDTH FONT) CW)))
  ;; Update the font's left-kern table, creating one if necessary.
  (LET ((CK (CD-CHAR-LEFT-KERN CD))
        (FCKT (FONT-LEFT-KERN-TABLE FONT)))
    ;;this is to prevent lossage with this function making left kern tables unnecessarily.
    ;; the problem is that a character with a blank left column gets a kern of -1, regardless
    ;; of whether it would fit into the raster regardless.
    (AND (MINUSP CK)
         ( (CD-CHAR-WIDTH CD) (- WIDTH CK))
         (SETQ LOSING-KERN (- CK) CK 0))
    (COND (FCKT (ASET CK FCKT CHAR-CODE))
          ((NOT (ZEROP CK))
           (SETF (FONT-LEFT-KERN-TABLE FONT)    ;Must be ART-INUM since left-kern can be -ve
                 (SETQ FCKT (MAKE-ARRAY FONT-LENGTH ':TYPE ART-INUM)))
           (AND DOUBLE-WIDTH-P
                (SETF (FONT-LEFT-KERN-TABLE (FONT-NEXT-PLANE FONT))
                      FCKT))
           (ASET CK FCKT CHAR-CODE))))
  ;; Tell the font this char exists.
  (ERRSET (ASET 1 (FONT-CHARS-EXIST-TABLE FONT) CHAR-CODE) NIL)
  ;; In wide fonts, the raster width depends on the character, and is a multiple of 32.
  (COND ((FONT-INDEXING-TABLE FONT)
         (SETQ FONT-WIDTH (* (CEILING (ARRAY-DIMENSION CD 1) 32.) 32.))))
  ;; Now copy the data.
  (DO ((ROW 0 (1+ ROW))
       (ONE-BIT-FONT (NULL (FONT-NEXT-PLANE FONT)))
       (RASTER-WIDTH (FONT-RASTER-WIDTH FONT)))
      (( ROW FONT-HEIGHT))
    (DO (
         ;; Count columns in font descriptor.
         (COL 0 (IF (< PIXEL-COL LOSING-KERN) COL
                  (+ COL COL-INCR)))
         ;; Count columns in font.
         (PIXEL-COL 0 (1+ PIXEL-COL))
         ;; for one-bit fonts this is index in font itself of start of row.
         ;; For multi-bit fonts it is not used.
         (NEXT-BIT-FONT-INDEX
           (+ (* 32. (+ (* (FONT-WORDS-PER-CHAR FONT)
                           (IF (FONT-INDEXING-TABLE FONT)
                               (AREF (FONT-INDEXING-TABLE FONT) CHAR-CODE)
                             CHAR-CODE))
                        (FLOOR ROW (FONT-RASTERS-PER-WORD FONT))))
              (* RASTER-WIDTH
                 (\ ROW (FONT-RASTERS-PER-WORD FONT))))
           (1+ NEXT-BIT-FONT-INDEX)))
        (( PIXEL-COL FONT-WIDTH))
      ;; Get pixel out of font descriptor.
      ;; If font is "double width", two pixels of font descriptor
      ;; are combined into one pixel for the font itself.
      (SETQ PIXEL (COND ((< PIXEL-COL LOSING-KERN) 0)
                        ((OR ( COL WIDTH) ( ROW HEIGHT)) 0)
                        (DOUBLE-WIDTH-P
                         (+ (COND (( (1+ COL) WIDTH) 0)
                                  (T (AREF CD ROW (1+ COL))))
                            (* 2 (AREF CD ROW COL))))
                        (T (AREF CD ROW COL))))
      ;; Store pixel into font.
      ;; If pixels are only one bit and chars not too wide, use a short cut.
      (COND (ONE-BIT-FONT
             ;; In wide font, notice when our horizontal advance
             ;; carries us into the "next character" of the many characters
             ;; in the font which actually represent vertical strips of one character.
             (AND (ZEROP (\ PIXEL-COL RASTER-WIDTH))
                  (NOT (ZEROP PIXEL-COL))
                  (SETQ NEXT-BIT-FONT-INDEX
                        (+ NEXT-BIT-FONT-INDEX
                           (* 32. (FONT-WORDS-PER-CHAR FONT))
                           (- RASTER-WIDTH))))
             (ASET PIXEL FONT NEXT-BIT-FONT-INDEX))
            (T
             (FONT-SET-PIXEL PIXEL FONT CHAR-CODE
                             ROW PIXEL-COL))))))

;Create an FD format font from an internal format font

(DEFUN FONT-INTO-FONT-DESCRIPTOR (FONT &OPTIONAL (DBL-WIDTH-P NIL)
       &AUX FONT-DESCRIPTOR
            (FONT-LENGTH (FONT-FILL-POINTER FONT))
            (LINE-SPACING (FONT-CHAR-HEIGHT FONT))
            (RASTER-HEIGHT (FONT-RASTER-HEIGHT FONT))
            (BASELINE (FONT-BASELINE FONT))
            (BLINKER-HEIGHT (FONT-BLINKER-HEIGHT FONT))
            (BLINKER-WIDTH (FONT-BLINKER-WIDTH FONT))
            (SPACE-WIDTH (FONT-CHAR-WIDTH FONT))
            FONT-CHARS-EXIST-TABLE
            TEMP RASTER-WIDTH CHARACTER-WIDTH LEFT-KERN PIXEL
            )
  "Create an return a font-descriptor containing the data from FONT."
  (ERRSET (SETQ FONT-CHARS-EXIST-TABLE (FONT-CHARS-EXIST-TABLE FONT)) NIL)
  ;; Correct for old fonts that may not have valid fill pointers.
  (OR (AND FONT-LENGTH ( FONT-LENGTH 200))
      (SETQ FONT-LENGTH 200))
  (SETQ FONT-DESCRIPTOR (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH FONT-LENGTH)
                                              FD-FILL-POINTER FONT-LENGTH))
  (SETF (FD-NAME FONT-DESCRIPTOR) (FONT-NAME FONT))
  (SETF (FD-LINE-SPACING FONT-DESCRIPTOR) LINE-SPACING)
  (SETF (FD-BASELINE FONT-DESCRIPTOR)BASELINE)
  (SETF (FD-BLINKER-HEIGHT FONT-DESCRIPTOR) BLINKER-HEIGHT)
  (SETF (FD-BLINKER-WIDTH FONT-DESCRIPTOR) BLINKER-WIDTH)
  (SETF (FD-SPACE-WIDTH FONT-DESCRIPTOR) SPACE-WIDTH)
  (SETF (FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR) DBL-WIDTH-P)
  (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
      (( CHAR-CODE FONT-LENGTH))
    (AND FONT-CHARS-EXIST-TABLE
         (ZEROP (AREF FONT-CHARS-EXIST-TABLE CHAR-CODE))
         (GO SKIP-CHAR))
    (SETQ CHARACTER-WIDTH (COND ((SETQ TEMP (FONT-CHAR-WIDTH-TABLE FONT))
                                 (AREF TEMP CHAR-CODE))
                                (T (FONT-CHAR-WIDTH FONT))))
    (SETQ RASTER-WIDTH
          (FONT-CHAR-MIN-RASTER-WIDTH FONT CHAR-CODE))
    ;; If we don't know for sure which chars exist,
    ;; discard chars containing no information.
    (AND (NULL FONT-CHARS-EXIST-TABLE)
         (ZEROP RASTER-WIDTH)
         ( CHAR-CODE #\SPACE)
         (= CHARACTER-WIDTH (FONT-CHAR-WIDTH FONT))
         (GO SKIP-CHAR))
    (SETQ LEFT-KERN (COND ((SETQ TEMP (FONT-LEFT-KERN-TABLE FONT))
                           (AREF TEMP CHAR-CODE))
                          (T 0)))
    (SETQ TEMP (MAKE-CHAR-DESCRIPTOR
                 :MAKE-ARRAY (:TYPE 'ART-4B
                                   :LENGTH (LIST RASTER-HEIGHT RASTER-WIDTH))
                 CD-CHAR-WIDTH CHARACTER-WIDTH
                 CD-CHAR-LEFT-KERN LEFT-KERN))
    (ASET TEMP FONT-DESCRIPTOR CHAR-CODE)
    (COND (DBL-WIDTH-P (DO ((ROW 0 (1+ ROW)))
                           (( ROW RASTER-HEIGHT))
                         (DO ((COLI 0 (1+ COLI))
                              (COL 0 (+ 2 COL)))
                             (( COL RASTER-WIDTH))
                           (SETQ PIXEL (FONT-GET-PIXEL FONT CHAR-CODE ROW COLI))
                           (ASET PIXEL TEMP ROW COL)
                           (ASET (LSH PIXEL -1) TEMP ROW (1+ COL)))))
          (T (DO ((ROW 0 (1+ ROW)))
                 (( ROW RASTER-HEIGHT))
               (DO ((COL 0 (1+ COL)))
                   (( COL RASTER-WIDTH))
                 (ASET (FONT-GET-PIXEL FONT CHAR-CODE ROW COL)
                       TEMP ROW COL)))))
    SKIP-CHAR)
  FONT-DESCRIPTOR)

;;; Some useful font munging functions
(DEFUN THICKEN-FONT-DESCRIPTOR (FD &OPTIONAL NEW-NAME &AUX LEN NFD)
  "Given font-descriptor FD, make a new one whose characters are /"thicker/".
NEW-NAME specifies the name to give the new font-descriptor;
by default, a /"B/" is appended to the old name.
The new font descriptor is returned, and the new name is not actually defined."
  (OR NEW-NAME (SETQ NEW-NAME (INTERN (STRING-APPEND (FD-NAME FD) #/B) "FONTS")))
  (SETQ LEN (ARRAY-ACTIVE-LENGTH FD)
        NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-Q :LENGTH LEN)
                                  FD-FILL-POINTER (FD-FILL-POINTER FD)
                                  FD-NAME NEW-NAME
                                  FD-LINE-SPACING (FD-LINE-SPACING FD)
                                  FD-BASELINE (FD-BASELINE FD)
                                  FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
                                  FD-BLINKER-WIDTH (1+ (FD-BLINKER-WIDTH FD))
                                  FD-SPACE-WIDTH (1+ (FD-SPACE-WIDTH FD))))
  (DO ((I 0 (1+ I))
       (CD) (NCD))
      (( I LEN))
    (AND (SETQ CD (AREF FD I))
         (LET ((WIDTH (ARRAY-DIMENSION CD 1))
               (HEIGHT (ARRAY-DIMENSION CD 0)))
           (SETQ NCD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-4B
                                                       :LENGTH (LIST HEIGHT (1+ WIDTH)))
                                           CD-CHAR-WIDTH (1+ (CD-CHAR-WIDTH CD))
                                           CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
           (DOTIMES (J HEIGHT)
             (DOTIMES (I WIDTH)
               (SETF (AREF NCD J I) (LOGIOR (AREF CD J I) (AREF NCD J I)))
               (SETF (AREF NCD J (1+ I)) (AREF CD J I))))
           (ASET NCD NFD I))))
  NFD)

(DEFUN THICKEN-FONT (FONT-SYMBOL &AUX FD NFD NFS)
  "Create a thicker (bolder) version of font named FONT-SYMBOL.
The new font is given a name which is /"B/" appended to the old name."
  (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
        NFD (THICKEN-FONT-DESCRIPTOR FD)
        NFS (FD-NAME NFD))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR NFS NFD)
  NFS)

(DEFUN UNTHICKEN-FONT-DESCRIPTOR (FD NEW-NAME &AUX LEN NFD)
  "Given font-descriptor FD, make a new one whose characters are less thick.
NEW-NAME specifies the name to give the new font-descriptor.
The new font descriptor is returned, and the new name is not actually defined."
  (SETQ LEN (ARRAY-ACTIVE-LENGTH FD)
        NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-Q :LENGTH LEN)
                                  FD-FILL-POINTER (FD-FILL-POINTER FD)
                                  FD-NAME NEW-NAME
                                  FD-LINE-SPACING (FD-LINE-SPACING FD)
                                  FD-BASELINE (FD-BASELINE FD)
                                  FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
                                  FD-BLINKER-WIDTH (FD-BLINKER-WIDTH FD)
                                  FD-SPACE-WIDTH (FD-SPACE-WIDTH FD)))
  (DO ((I 0 (1+ I))
       (CD) (NCD))
      (( I LEN))
    (AND (SETQ CD (AREF FD I))
         (LET ((WIDTH (ARRAY-DIMENSION CD 1))
               (HEIGHT (ARRAY-DIMENSION CD 0)))
           (SETQ NCD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE 'ART-4B
                                                       :LENGTH (LIST HEIGHT WIDTH))
                                           CD-CHAR-WIDTH (CD-CHAR-WIDTH CD)
                                           CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
           ;110  100
           (DOTIMES (J HEIGHT)
             (DO* ((I (1- WIDTH) (1- I))
                   (RIGHT 0 THIS)
                   (THIS (AREF CD J I) LEFT)
                   LEFT)
                  ((= I 0))
               (SETQ LEFT (AREF CD J (1- I)))
               (IF (AND ( LEFT 0) ( THIS 0) (= RIGHT 0))
                   (ASET 0 NCD J I)
                 (ASET (AREF CD J I) NCD J I))))
           (ASET NCD NFD I))))
  NFD)

(DEFUN UNTHICKEN-FONT (FONT-SYMBOL NFS &AUX FD NFD)
  "Create a less thick (bold) version of font named FONT-SYMBOL.
NFS is the name for the new font."
  (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
        NFD (UNTHICKEN-FONT-DESCRIPTOR FD NFS))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR NFS NFD)
  NFS)

(DEFUN ROTATE-FONT-DESCRIPTOR (FD &AUX LENGTH NFD)
  (SETQ LENGTH (ARRAY-ACTIVE-LENGTH FD)
        NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH LENGTH)
                                  FD-FILL-POINTER (FD-FILL-POINTER FD)
                                  FD-NAME (INTERN (STRING-APPEND (FD-NAME FD) #/R) "FONTS")
                                  FD-BASELINE (FD-SPACE-WIDTH FD)
                                  FD-LINE-SPACING (FD-SPACE-WIDTH FD)
                                  FD-BLINKER-HEIGHT (FD-BLINKER-WIDTH FD)
                                  FD-BLINKER-WIDTH (FD-BLINKER-HEIGHT FD)
                                  FD-SPACE-WIDTH (FD-LINE-SPACING FD)
                                  FD-ROTATION 90.))
  (LOOP FOR CH FROM 0 BELOW LENGTH
        AS CD = (AREF FD CH)
        WHEN CD
        DO (ASET (ROTATE-CHAR-DESCRIPTOR CD) NFD CH))
  NFD)

(DEFUN ROTATE-CHAR-DESCRIPTOR (CD)
  (LET* ((HEI (ARRAY-DIMENSION CD 0))
         (WID (ARRAY-DIMENSION CD 1))
         (typ (array-type cd))
         (NCD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:LENGTH (LIST WID HEI)
                                                         :TYPE typ) ;(ARRAY-TYPE CD))
                                    CD-CHAR-LEFT-KERN 0
                                    CD-CHAR-WIDTH HEI)))
    (LOOP FOR X FROM 0 BELOW WID
          DO (LOOP FOR Y FROM 0 BELOW HEI
                   DO (ASET (AREF CD Y X) NCD (- WID X 1) Y)))
    NCD))

(DEFUN ROTATE-FONT (FONT-SYMBOL &AUX FD NFD NFS)
  "Create a rotated version of font named FONT-SYMBOL.
The new font is given a name which is /"R/" appended to the old name."
  (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
        NFD (ROTATE-FONT-DESCRIPTOR FD)
        NFS (FD-NAME NFD))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR NFS NFD)
  NFS)

;; KST files

;; Given filename and fontname from filename &optional fontname, canonicalize them
(DEFUN GET-INPUT-FILENAME-AND-FONTNAME (FILENAME FONTNAME FILE-TYPE)
  (DECLARE (RETURN-LIST FILENAME FONTNAME))
  (SETQ FILENAME (FS:MERGE-PATHNAME-DEFAULTS FILENAME (PATHNAME-DEFAULTS) FILE-TYPE))
  (OR FONTNAME (SETQ FONTNAME (FUNCALL FILENAME ':NAME)))
  (AND (STRINGP FONTNAME) (SETQ FONTNAME (INTERN (STRING-UPCASE FONTNAME) "FONTS")))
  (VALUES FILENAME FONTNAME))

;; Read in a kst file and make and return a FONT-DESCRIPTOR,
;; which is an alternate convenient representation for a font.
(DEFUN READ-KST-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME &AUX FD)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "KST"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:FIXNUM :IN :BYTE-SIZE 9.))
    (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME :MAKE-ARRAY (:LENGTH 200)))
    ;; Discard KSTID.
    (DOTIMES (I 4) (FUNCALL STREAM ':TYI))
    ;; Discard column position adjust until I find out what it means.
    (OR (ZEROP (FUNCALL STREAM ':TYI))
        (FERROR NIL
                "Nonzero column-position-adjust in font ~A -- what does that mean?"
                FONTNAME))
    (SETF (FD-SPACE-WIDTH FD) 0)                ;Just in case no space character.
    (SETF (FD-BASELINE FD) (FUNCALL STREAM ':TYI))
    (SETF (FD-LINE-SPACING FD) (READ-KST-HALFWORD STREAM))
    (SETF (FD-BLINKER-HEIGHT FD)
          (FD-LINE-SPACING FD))
    (SETF (FD-NAME FD) FONTNAME)
    (LET (KERN CHAR-CODE RASTER-WIDTH CHAR-WIDTH BYTE-LIST BYTE-LIST-HEAD CD TEM
          (LINE-HEIGHT (FD-LINE-SPACING FD)))
      (LOOP AS HEADER = (LOGIOR (ASH (READ-KST-HALFWORD STREAM) 18.)
                                (READ-KST-HALFWORD STREAM))
            UNTIL (= HEADER -1)
            UNLESS (= HEADER 1)
              DO (FERROR NIL "~O where character header expected; KST file misformatted"
                             HEADER)
        DO
        (SETQ KERN (READ-KST-HALFWORD STREAM))
        (SETQ CHAR-CODE (READ-KST-HALFWORD STREAM))
        (SETQ RASTER-WIDTH (READ-KST-HALFWORD STREAM))
        (SETQ CHAR-WIDTH (READ-KST-HALFWORD STREAM))
        (SETQ CD (MAKE-CHAR-DESCRIPTOR
                   :MAKE-ARRAY (:TYPE ART-1B :LENGTH (LIST LINE-HEIGHT RASTER-WIDTH))))
        (SETF (CD-CHAR-WIDTH CD) CHAR-WIDTH)
        (SETF (CD-CHAR-LEFT-KERN CD) KERN)
        (FD-STORE-CD FD CD CHAR-CODE)
        (AND (= CHAR-CODE #\SP)
             (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
        ;; read in the bits of the character
        (SETQ BYTE-LIST NIL
              BYTE-LIST-HEAD (LIST NIL NIL NIL NIL))
        (DOTIMES (VPOS LINE-HEIGHT)
          ;; Read in the next row.
          (DOTIMES (HPOS RASTER-WIDTH)
            ;; If byte is exhausted, get next byte into (car byte-list)
            (COND ((ZEROP (\ HPOS 8))
                   (SETQ BYTE-LIST (READ-KST-BYTES STREAM BYTE-LIST BYTE-LIST-HEAD))))
            (SETQ TEM (LOGAND 1 (LSH (CAR BYTE-LIST) (- (\ HPOS 8)))))
            (ASET TEM CD VPOS HPOS)))))
    ;; Truncate fd to discard unused elements at the end.
    (DO ((I (1- (ARRAY-LENGTH FD)) (1- I)))
        ((OR (MINUSP I)
             (AREF FD I))
         (ADJUST-ARRAY-SIZE FD (1+ I))))
    (SETF (FD-FILL-POINTER FD) (ARRAY-LENGTH FD))
    ;; Set width of blinker and space fields from the space character.
    (SETF (FD-BLINKER-WIDTH FD)
          (FD-SPACE-WIDTH FD)))
  FD)

;; Read in a kst file and define a font.
;; The font name defaults from the file name.
;;;??? This still assumes that the font length is no more than 200!
;;; It seems hard to fix this.
(DEFUN READ-KST-INTO-FONT (FILENAME &OPTIONAL FONTNAME
                                    &AUX FONT CHARS-EXIST-TABLE
                                         RASTER-WIDTH RASTER-HEIGHT
                                         RASTERS-PER-WORD WORDS-PER-CHAR)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "KST"))
  ;; Read file once to determine font parameters.
  (MULTIPLE-VALUE (RASTER-WIDTH RASTER-HEIGHT)
    (READ-KST-MAX-RASTER-WIDTH FILENAME))
  ;; If this is a hairy wide font, then instead of writing it directly
  ;; make a font-descriptor and turn it into a font.
  (COND ((> RASTER-WIDTH 32.)
         (FONT-NAME-SET-FONT-AND-DESCRIPTOR
           FONTNAME
           (READ-KST-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
         FONTNAME)
        (T
         (SETQ RASTERS-PER-WORD (FLOOR 32. RASTER-WIDTH))
         (SETQ WORDS-PER-CHAR (CEILING RASTER-HEIGHT RASTERS-PER-WORD))
         ;; Now that we know the parameters, allocate the font.
         (SETQ FONT (TV:MAKE-FONT :MAKE-ARRAY (:TYPE 'ART-1B
                                                    :LENGTH (* WORDS-PER-CHAR 32. 200))))
         (SETF (FONT-RASTERS-PER-WORD FONT) RASTERS-PER-WORD)
         (SETF (FONT-WORDS-PER-CHAR FONT) WORDS-PER-CHAR)
         (SETF (FONT-RASTER-WIDTH FONT) RASTER-WIDTH)
         (SETF (FONT-RASTER-HEIGHT FONT) RASTER-HEIGHT)
         (SETF (FONT-CHAR-HEIGHT FONT) RASTER-HEIGHT)
         (SETF (FONT-BLINKER-HEIGHT FONT) RASTER-HEIGHT)
         (SETF (FONT-NAME FONT) FONTNAME)
         (SETQ CHARS-EXIST-TABLE (MAKE-ARRAY 200 ':TYPE 'ART-1B))
         (SETF (FONT-CHARS-EXIST-TABLE FONT) CHARS-EXIST-TABLE)
         ;; Now actually read in the data of the font.
         (WITH-OPEN-FILE (STREAM FILENAME '(:FIXNUM :IN :BYTE-SIZE 9.))
           ;; Discard KSTID.
           (DOTIMES (I 4) (FUNCALL STREAM ':TYI))
           ;; Discard column position adjust until I find out what it means.
           (OR (ZEROP (FUNCALL STREAM ':TYI))
               (FERROR NIL
                       "Nonzero column-position-adjust in font ~A -- what does that mean?"
                       FONTNAME))
           (SETF (FONT-BASELINE FONT) (FUNCALL STREAM ':TYI))
           ;; Discard line height (already determined).
           (READ-KST-HALFWORD STREAM)
           (LET (KERN CHAR-CODE CHAR-WIDTH CHAR-RASTER-WIDTH
                 BYTE-LIST BYTE-LIST-HEAD TEM BIT-POS WORD-POS
                 (LINE-HEIGHT RASTER-HEIGHT))
             (LOOP AS HEADER = (LOGIOR (ASH (READ-KST-HALFWORD STREAM) 18.)
                                       (READ-KST-HALFWORD STREAM))
                   UNTIL (= HEADER -1)
                   UNLESS (= HEADER 1)
                     DO (FERROR NIL
                                "~O where character header expected; KST file misformatted"
                                HEADER)
               DO
               (SETQ KERN (READ-KST-HALFWORD STREAM))
               (SETQ CHAR-CODE (READ-KST-HALFWORD STREAM))
               ;; While all chars have the same raster width in the lisp machine font,
               ;; we need the raster width stored in the kst file to read the kst file.
               (SETQ CHAR-RASTER-WIDTH (READ-KST-HALFWORD STREAM))
               (SETQ CHAR-WIDTH (READ-KST-HALFWORD STREAM))
               (ASET 1 CHARS-EXIST-TABLE CHAR-CODE)
               ;; Now store the char width and left kern, creating the tables if nec.
               (COND ((NULL (FONT-CHAR-WIDTH FONT))
                      (SETF (FONT-CHAR-WIDTH FONT) CHAR-WIDTH))
                     ((FONT-CHAR-WIDTH-TABLE FONT)
                      (ASET CHAR-WIDTH (FONT-CHAR-WIDTH-TABLE FONT) CHAR-CODE))
                     ((= CHAR-WIDTH (FONT-CHAR-WIDTH FONT)))
                     (T (SETF (FONT-CHAR-WIDTH-TABLE FONT)
                              (MAKE-ARRAY 200))
                        (ASET CHAR-WIDTH (FONT-CHAR-WIDTH-TABLE FONT) CHAR-CODE)))
               (AND (= CHAR-CODE #\SP)
                    (SETF (FONT-CHAR-WIDTH FONT) CHAR-WIDTH))
               (COND ((NOT (ZEROP KERN))
                      (OR (FONT-LEFT-KERN-TABLE FONT)
                          (SETF (FONT-LEFT-KERN-TABLE FONT)
                                ;; Use art-32b so can hold both signs.
                                (MAKE-ARRAY 200 ':TYPE 'ART-INUM)))
                      (ASET KERN (FONT-LEFT-KERN-TABLE FONT) CHAR-CODE)))
               ;; read in the bits of the character
               (SETQ BYTE-LIST NIL
                     BYTE-LIST-HEAD (LIST NIL NIL NIL NIL))
               (SETQ WORD-POS (* CHAR-CODE WORDS-PER-CHAR)
                     BIT-POS 0)
               (DOTIMES (VPOS LINE-HEIGHT)
                 ;; Find next row in font - advance to word boundary if nec.
                 (AND (> (+ BIT-POS RASTER-WIDTH) 32.)
                      (SETQ BIT-POS 0 WORD-POS (1+ WORD-POS)))
                 ;; Read in that row.
                 (DOTIMES (HPOS CHAR-RASTER-WIDTH)
                   ;; If byte is exhausted, get next byte into (car byte-list)
                   (COND ((ZEROP (\ HPOS 8))
                          (SETQ BYTE-LIST (READ-KST-BYTES STREAM BYTE-LIST
                                                          BYTE-LIST-HEAD))))
                   (SETQ TEM (LOGAND 1 (LSH (CAR BYTE-LIST) (- (\ HPOS 8)))))
                   (ASET TEM FONT (+ (LSH WORD-POS 5) BIT-POS HPOS)))
                 ;; Advance past this row in the font.
                 (SETQ BIT-POS (+ BIT-POS RASTER-WIDTH)))
               )))
         ;; Set width of blinker and space fields from the space character.
         (SETF (FONT-BLINKER-WIDTH FONT)
               (FONT-CHAR-WIDTH FONT))
         (SET FONTNAME FONT)
         (PUTPROP FONTNAME FILENAME 'KST-FILE)
         FONTNAME)))

;; Scan a kst file and return two values which are the
;; raster width and raster height needed in a TV format font to contain that font.
(DEFUN READ-KST-MAX-RASTER-WIDTH (FILENAME &AUX RASTER-HEIGHT (RASTER-WIDTH 0)
                                                CHAR-RASTER-WIDTH)
  (SETQ FILENAME (FS:MERGE-PATHNAME-DEFAULTS FILENAME (PATHNAME-DEFAULTS) "KST"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:FIXNUM :IN :BYTE-SIZE 9.))
    ;; Discard KSTID.
    (DOTIMES (I 4) (FUNCALL STREAM ':TYI))
    ;; Discard column-position-adjust
    (FUNCALL STREAM ':TYI)
    ;; Discard baseline.
    (FUNCALL STREAM ':TYI)
    ;; Remember font line height as raster height.
    (SETQ RASTER-HEIGHT (READ-KST-HALFWORD STREAM))
    ;; Keep maxing raster widths of characters into raster-width
    (SETQ RASTER-WIDTH 0)
    (LOOP AS HEADER = (LOGIOR (ASH (READ-KST-HALFWORD STREAM) 18.) (READ-KST-HALFWORD STREAM))
          UNTIL (= HEADER -1)
          UNLESS (= HEADER 1)
            DO (FERROR NIL "~O where character header expected; KST file misformatted" HEADER)
      DO
      ;; Ignore char's left kern.
      (READ-KST-HALFWORD STREAM)
      ;; Ignore its character code.
      (READ-KST-HALFWORD STREAM)
      ;; Max in its raster width
      (SETQ CHAR-RASTER-WIDTH (READ-KST-HALFWORD STREAM))
      (SETQ RASTER-WIDTH (MAX RASTER-WIDTH CHAR-RASTER-WIDTH))
      ;; Ignore its character width.
      (READ-KST-HALFWORD STREAM)
      ;; Skip the bits of the character
      (LET ((BYTES (* RASTER-HEIGHT (CEILING CHAR-RASTER-WIDTH 8))))
        (SETQ BYTES (* 4 (CEILING BYTES 4)))
        (DOTIMES (I BYTES)
          (FUNCALL STREAM ':TYI)))))
  (VALUES RASTER-WIDTH RASTER-HEIGHT))

;; Fetch the next 8-bit byte where stream is a 9-bit byte stream.
;; byte-list-head should be a list of 4 things we can clobber.
;; byte-list is a tail of it.
;; We advance it, and if it is empty we fill byte-list-head
;; with four more 8-bit bytes and return that.
;; The car of our value is the next byte.
;; Save the value for the byte-list arg next time.
(DEFUN READ-KST-BYTES (STREAM BYTE-LIST BYTE-LIST-HEAD)
  (OR (CDR BYTE-LIST)
      ;; Exhausted the word - read another.
      (LET ((HWD1 (READ-KST-HALFWORD STREAM))
            (HWD2 (READ-KST-HALFWORD STREAM)))
        (SETQ BYTE-LIST BYTE-LIST-HEAD)
        ;; Turn it into 4 8-bit bytes in byte-list.
        (RPLACA BYTE-LIST (LDB 1210 HWD1))
        (RPLACA (CDR BYTE-LIST) (LDB 0210 HWD1))
        (RPLACA (CDDR BYTE-LIST)
                (+ (LSH (LDB 0002 HWD1) 6)
                   (LDB 1406 HWD2)))
        (RPLACA (CDDDR BYTE-LIST) (LDB 0410 HWD2))
        BYTE-LIST)))

;; Read two 9-bit bytes from stream, make an 18-bit halfword,
;; and sign-extend it.
(DEFUN READ-KST-HALFWORD (STREAM &AUX HWD)
  (SETQ HWD (DPB (FUNCALL STREAM ':TYI) 1111 (FUNCALL STREAM ':TYI)))
  (COND ((BIT-TEST HWD 400000)
         (LOGIOR HWD -400000))
        (T HWD)))

;; Writing KST files

;;; Given FONTNAME and FILENAME from FONTNAME &OPTIONAL FILENAME, canonicalize them.
(DEFUN GET-OUTPUT-FILENAME-AND-FONTNAME (FONTNAME FILENAME FILE-TYPE)
  (DECLARE (RETURN-LIST FONTNAME FILENAME))
  (AND (STRINGP FONTNAME) (SETQ FONTNAME (INTERN (STRING-UPCASE FONTNAME) "FONTS")))
  (OR FILENAME (SETQ FILENAME (STRING FONTNAME)))
  (SETQ FILENAME (FS:MERGE-PATHNAME-DEFAULTS FILENAME (PATHNAME-DEFAULTS) FILE-TYPE))
  (VALUES FONTNAME FILENAME))

(DEFUN GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR (FONT-DESCRIPTOR FILENAME FILE-TYPE)
  (OR FILENAME (SETQ FILENAME (STRING (FD-NAME FONT-DESCRIPTOR))))
  (FS:MERGE-PATHNAME-DEFAULTS FILENAME (PATHNAME-DEFAULTS) FILE-TYPE))

;; It would be good to check for chars that are all zero and
;; flush them, and also to compute the actual needed raster width and use it.
(DEFUN WRITE-FONT-INTO-KST (FONTNAME &OPTIONAL FILENAME &AUX FONT FONT-LENGTH)
  (MULTIPLE-VALUE (FONTNAME FILENAME)
    (GET-OUTPUT-FILENAME-AND-FONTNAME FONTNAME FILENAME "KST"))
  (SETQ FONT (SYMEVAL FONTNAME))
  (SETQ FONT-LENGTH (FONT-FILL-POINTER FONT))
  (OR (AND FONT-LENGTH ( FONT-LENGTH 200))
      (SETQ FONT-LENGTH 200))
  (COND ((FONT-INDEXING-TABLE FONT)
         (LET ((FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME)))
           (WRITE-FONT-DESCRIPTOR-INTO-KST FD FILENAME)))
        (T
         (AND (> (FONT-RASTER-HEIGHT FONT)
                 (FONT-CHAR-HEIGHT FONT))
              (FORMAT T "Warning: font raster height exceeds line height"))
         (WITH-OPEN-FILE (STREAM FILENAME '(:FIXNUM :OUT :BYTE-SIZE 9.))
           ;; Write KSTID as 0.
           (DOTIMES (I 4) (FUNCALL STREAM ':TYO 0))
           ;; Write column position adjust as 0.
           (FUNCALL STREAM ':TYO 0)
           ;; Write baseline and height into second header word.
           (FUNCALL STREAM ':TYO (FONT-BASELINE FONT))
           (WRITE-KST-HALFWORD STREAM (FONT-CHAR-HEIGHT FONT))
           ;; Then write out all the characters.
           (LET (KERN-TABLE CHAR-WIDTH-TABLE CHARS-EXIST-TABLE
                 WORD-POS BIT-POS BYTE-COUNT BYTE
                 CHAR-RASTER-WIDTH BYTE-LIST BYTE-LIST-HEAD)
             (SETQ KERN-TABLE (FONT-LEFT-KERN-TABLE FONT)
                   CHAR-WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT))
             (ERRSET (SETQ CHARS-EXIST-TABLE (FONT-CHARS-EXIST-TABLE FONT)) NIL)
             (DOTIMES (CHAR-CODE FONT-LENGTH)
               (AND CHARS-EXIST-TABLE
                    (ZEROP (AREF CHARS-EXIST-TABLE CHAR-CODE))
                    (GO SKIP-CHAR))
               ;; Each char must start with a word containing a 1.
               (WRITE-KST-HALFWORD STREAM 0)
               (WRITE-KST-HALFWORD STREAM 1)
               ;; left kern and char code fill the next word.
               (WRITE-KST-HALFWORD STREAM
                                   (OR (AND KERN-TABLE (AREF KERN-TABLE CHAR-CODE)) 0))
               (WRITE-KST-HALFWORD STREAM CHAR-CODE)
               ;; Raster width and char width are the next word.
               (SETQ CHAR-RASTER-WIDTH (MAX 1 (FONT-CHAR-MIN-RASTER-WIDTH FONT CHAR-CODE)))
               (WRITE-KST-HALFWORD STREAM CHAR-RASTER-WIDTH)
               (WRITE-KST-HALFWORD STREAM
                                   (COND (CHAR-WIDTH-TABLE
                                          (OR (AREF CHAR-WIDTH-TABLE CHAR-CODE) 0))
                                         (T (FONT-CHAR-WIDTH FONT))))
               ;; Write out the bits of the character
               ;; Word-pos and bit-pos are used to point at a bit in the font.
               (SETQ WORD-POS (* (FONT-WORDS-PER-CHAR FONT) CHAR-CODE))
               (SETQ BIT-POS 0 BYTE-COUNT 0)
               ;; Byte-list and its head are used to accumulate 4 bytes
               ;; and then output them at once as a word.
               ;; This is needed because the stream wants 9-bit bytes.
               (SETQ BYTE-LIST-HEAD (LIST NIL NIL NIL NIL))
               (SETQ BYTE-LIST BYTE-LIST-HEAD)
               (COND ((NOT (ZEROP CHAR-RASTER-WIDTH))
                      (DOTIMES (VPOS (FONT-CHAR-HEIGHT FONT))
                        ;; Prepare to extract next row of char from font.
                        (AND (> (+ BIT-POS (FONT-RASTER-WIDTH FONT)) 32.)
                             (SETQ WORD-POS (1+ WORD-POS) BIT-POS 0))
                        (SETQ BYTE 0)
                        ;; Get the row a bit at a time and fill up 8-bit bytes.
                        ;; Output the bytes when full.  Output the excess at the end.
                        ;; Count the bytes output with byte-count
                        (DOTIMES (HPOS CHAR-RASTER-WIDTH)
                          (COND ((AND (= (\ HPOS 8) 0) (NOT (ZEROP HPOS)))
                                 (SETQ BYTE-COUNT (1+ BYTE-COUNT))
                                 (SETQ BYTE-LIST
                                       (WRITE-KST-BYTE STREAM BYTE BYTE-LIST BYTE-LIST-HEAD))
                                 (SETQ BYTE 0)))
                          (OR ( VPOS (FONT-RASTER-HEIGHT FONT))
                              (SETQ BYTE (+ BYTE (LSH (AREF FONT
                                                            (+ (* 32. WORD-POS) HPOS BIT-POS))
                                                      (\ HPOS 8))))))
                        (SETQ BYTE-LIST (WRITE-KST-BYTE STREAM BYTE BYTE-LIST BYTE-LIST-HEAD))
                        (SETQ BYTE-COUNT (1+ BYTE-COUNT))
                        (SETQ BIT-POS (+ BIT-POS (FONT-RASTER-WIDTH FONT))))
                      ;; Pad to a word boundary.
                      (DO () ((ZEROP (\ BYTE-COUNT 4)))
                        (SETQ BYTE-LIST (WRITE-KST-BYTE STREAM 0 BYTE-LIST BYTE-LIST-HEAD))
                        (SETQ BYTE-COUNT (1+ BYTE-COUNT)))))
               SKIP-CHAR)
             ;; Mark end of file with two -1 words.
             (DOTIMES (I 8)
               (FUNCALL STREAM ':TYO 777)))
           (FUNCALL STREAM ':CLOSE)
           (FUNCALL STREAM ':TRUENAME)))))

(DEFUN WRITE-FONT-DESCRIPTOR-INTO-KST (FD &OPTIONAL FILENAME
                                          &AUX (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FD)))
  (SETQ FILENAME (GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR FD FILENAME "KST"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:FIXNUM :OUT :BYTE-SIZE 9.))
    ;; Write KSTID as 0.
    (DOTIMES (I 4) (FUNCALL STREAM ':TYO 0))
    ;; Write column position adjust as 0.
    (FUNCALL STREAM ':TYO 0)
    ;; Write baseline and height into second header word.
    (FUNCALL STREAM ':TYO (FD-BASELINE FD))
    (WRITE-KST-HALFWORD STREAM (FD-LINE-SPACING FD))
    ;; Then write out all the characters.
    (LET (CD CHAR-HEIGHT BYTE-COUNT BYTE BYTE-LIST BYTE-LIST-HEAD)
       (DOTIMES (CHAR-CODE FONT-LENGTH)
          (COND ((AND (SETQ CD (AREF FD CHAR-CODE))
                      ;; Wide fonts without chars-exist-tables can have 0-width chars.
                      (OR (NOT (ZEROP (ARRAY-DIMENSION CD 1)))
                          (NOT (ZEROP (CD-CHAR-WIDTH CD)))))
            ;; Each char must start with a word containing a 1.
            (WRITE-KST-HALFWORD STREAM 0)
            (WRITE-KST-HALFWORD STREAM 1)
            ;; left kern and char code fill the next word.
            (WRITE-KST-HALFWORD STREAM (CD-CHAR-LEFT-KERN CD))
            (WRITE-KST-HALFWORD STREAM CHAR-CODE)
            ;; Raster width and char width are the next word.
            (WRITE-KST-HALFWORD STREAM (ARRAY-DIMENSION CD 1))
            (WRITE-KST-HALFWORD STREAM (CD-CHAR-WIDTH CD))
            ;; Write out the bits of the character
            ;; Byte-list and its head are used to accumulate 4 bytes
            ;; and then output them at once as a word.
            ;; This is needed because the stream wants 9-bit bytes.
            (SETQ BYTE-LIST-HEAD (LIST NIL NIL NIL NIL))
            (SETQ BYTE-LIST BYTE-LIST-HEAD)
            (SETQ BYTE-COUNT 0)
            (SETQ CHAR-HEIGHT (ARRAY-DIMENSION CD 0))
            (AND (> CHAR-HEIGHT (FD-LINE-SPACING FD))
                 (FERROR NIL "Character ~C height exceeds font line height in KST file"
                         CHAR-CODE))
            (COND ((NOT (ZEROP (ARRAY-DIMENSION CD 1)))
                   (DOTIMES (VPOS (FD-LINE-SPACING FD))
                     ;; Prepare to extract next row of char from font.
                     (SETQ BYTE 0)
                     ;; Get the row a bit at a time and fill up 8-bit bytes.
                     ;; Output the bytes when full.  Output the excess at the end.
                     ;; Count the bytes output with byte-count
                     (DOTIMES (HPOS (ARRAY-DIMENSION CD 1))
                       (COND ((AND (= (\ HPOS 8) 0) (NOT (ZEROP HPOS)))
                              (SETQ BYTE-COUNT (1+ BYTE-COUNT))
                              (SETQ BYTE-LIST
                                    (WRITE-KST-BYTE STREAM BYTE BYTE-LIST BYTE-LIST-HEAD))
                              (SETQ BYTE 0)))
                       (OR ( VPOS CHAR-HEIGHT)
                           (SETQ BYTE (+ BYTE (LSH (AREF CD VPOS HPOS) (\ HPOS 8))))))
                     (SETQ BYTE-LIST (WRITE-KST-BYTE STREAM BYTE BYTE-LIST BYTE-LIST-HEAD))
                     (SETQ BYTE-COUNT (1+ BYTE-COUNT)))
                   ;; Pad to a word boundary.
                   (DO () ((ZEROP (\ BYTE-COUNT 4)))
                     (SETQ BYTE-LIST (WRITE-KST-BYTE STREAM 0 BYTE-LIST BYTE-LIST-HEAD))
                     (SETQ BYTE-COUNT (1+ BYTE-COUNT))))))))
       ;; Mark end of file with two -1 words.
       (DOTIMES (I 8)
         (FUNCALL STREAM ':TYO 777)))
    (FUNCALL STREAM ':CLOSE)
    (FUNCALL STREAM ':TRUENAME)))

;; Write an 8-bit byte to the kst file.  We pack 4 bytes per word.
;; The stream is assumed to want 9-bit bytes.
;; Byte-list-head should be a list of length 4 we can clobber.
;; byte-list should initially be the same thing;  we return a new value to set it to.
(DEFUN WRITE-KST-BYTE (STREAM BYTE BYTE-LIST BYTE-LIST-HEAD)
  (RPLACA BYTE-LIST BYTE)
  (POP BYTE-LIST)
  (COND ((NULL BYTE-LIST)
         (SETQ BYTE-LIST BYTE-LIST-HEAD)
         (WRITE-KST-HALFWORD STREAM
                             (+ (LSH (FIRST BYTE-LIST) 10.)
                                (LSH (SECOND BYTE-LIST) 2.)
                                (LDB 0602 (THIRD BYTE-LIST))))
         (WRITE-KST-HALFWORD STREAM
                             (+ (LSH (LDB 0006 (THIRD BYTE-LIST)) 12.)
                                (LSH (FOURTH BYTE-LIST) 4)))))
  BYTE-LIST)

(DEFUN WRITE-KST-HALFWORD (STREAM HALFWORD)
  (FUNCALL STREAM ':TYO (LDB 1111 HALFWORD))
  (FUNCALL STREAM ':TYO (LDB 0011 HALFWORD)))

;; Compute the smallest raster width needed to store the specified char
;; as defined by the specified font.
;; low-level means we are looking at one sub-character in a wide font.
(DEFUN FONT-CHAR-MIN-RASTER-WIDTH (FONT CHAR-CODE
                                   &OPTIONAL LOW-LEVEL
                                   &AUX BIT-POS WORD-POS TEM
                                        MIN-RASTER-WIDTH F-RASTER-WIDTH RASTER-HEIGHT)
    (COND ((AND (NOT LOW-LEVEL)
                (SETQ TEM (FONT-INDEXING-TABLE FONT)))
           ;; If it's a wide font, go by the number of vertical stripes,
           ;; but also see how wide the rightmost stripe really needs to be.
           (LET ((START-IDX (AREF TEM CHAR-CODE))
                 (END-IDX (AREF TEM (1+ CHAR-CODE))))
             (IF (= START-IDX END-IDX)
                 0
                 (MAX 0
                      (+ (* 32. (- END-IDX START-IDX))
                         -32.
                         (FONT-CHAR-MIN-RASTER-WIDTH FONT (1- END-IDX) T))))))
          (T (SETQ WORD-POS (* CHAR-CODE (FONT-WORDS-PER-CHAR FONT))
                   BIT-POS 0
                   MIN-RASTER-WIDTH 0
                   F-RASTER-WIDTH (FONT-RASTER-WIDTH FONT)
                   RASTER-HEIGHT (FONT-RASTER-HEIGHT FONT))
             (DOTIMES (VPOS RASTER-HEIGHT)
                 (AND (> (+ BIT-POS F-RASTER-WIDTH) 32.)
                      (SETQ BIT-POS 0 WORD-POS (1+ WORD-POS)))
                 (DO ((HPOS 0 (1+ HPOS)) (INDEX (+ BIT-POS (LSH WORD-POS 5)) (1+ INDEX)))
                     ((= HPOS F-RASTER-WIDTH))
                    (OR (ZEROP (AREF FONT INDEX))
                        (SETQ MIN-RASTER-WIDTH (MAX (1+ HPOS) MIN-RASTER-WIDTH))))
                 (SETQ BIT-POS (+ F-RASTER-WIDTH BIT-POS)))
             MIN-RASTER-WIDTH)))

;; ALTO .AL format
;;;??? Not yet converted to load more than 200 chars into a font.

;; Load an ALTO font file into a font, the easy way, via a font descriptor
(DEFUN READ-AL-INTO-FONT (FILENAME &OPTIONAL FONTNAME)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AL"))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME
                                     (READ-AL-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  FONTNAME)

;; Load an ALTO font file into a font descriptor
(DEFUN READ-AL-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME
                                     &AUX FD ARRAY LINE-HEIGHT PROPORTIONAL MAX-WIDTH)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AL"))
  (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME))
  (SETF (FD-NAME FD) FONTNAME)
  (WITH-OPEN-FILE (STREAM FILENAME '(:IN :FIXNUM))
    (SETQ LINE-HEIGHT (FUNCALL STREAM ':TYI))
    (SETF (FD-LINE-SPACING FD) LINE-HEIGHT)
    (SETF (FD-BLINKER-HEIGHT FD) LINE-HEIGHT)
    (LET ((BASELINE-AND-MAX-WIDTH (FUNCALL STREAM ':TYI)))
      (SETQ PROPORTIONAL (LDB-TEST 1701 BASELINE-AND-MAX-WIDTH))
      (SETF (FD-BASELINE FD) (LDB 1007 BASELINE-AND-MAX-WIDTH))
      (SETF (FD-SPACE-WIDTH FD) (SETQ MAX-WIDTH (LDB 0010 BASELINE-AND-MAX-WIDTH))))
    (SETQ ARRAY (MAKE-ARRAY 1000. ':TYPE 'ART-16B ':LEADER-LIST '(0)))
    (DO CH (FUNCALL STREAM ':TYI) (FUNCALL STREAM ':TYI) (NULL CH)
      (ARRAY-PUSH-EXTEND ARRAY CH)))
  (DO ((CH 0 (1+ CH))
       (CD)
       (CHAR-WIDTH))
      (( CH 200))      ;Alto font could have 400 characters, our fonts don't yet
    (SETQ CHAR-WIDTH 0)
    (DO ((IDX CH)
         (XW))
        (NIL)
      (SETQ IDX (+ IDX (AREF ARRAY IDX)))
      (SETQ XW (AREF ARRAY IDX))
      (IF (ZEROP (PROG1 (LOGAND XW 1) (SETQ XW (FLOOR XW 2))))
          (SETQ CHAR-WIDTH (+ CHAR-WIDTH 16.)
                IDX XW)
          (RETURN (SETQ CHAR-WIDTH (+ CHAR-WIDTH XW)))))
    (SETQ CD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE ART-1B
                                               :LENGTH (LIST LINE-HEIGHT CHAR-WIDTH))))
    (SETF (CD-CHAR-WIDTH CD) CHAR-WIDTH)
    (AND (= CH #\SP) (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
    (SETF (CD-CHAR-LEFT-KERN CD) 0)
    (FD-STORE-CD FD CD CH)
    (READ-AL-INTO-FONT-DESCRIPTOR-1 CD ARRAY CH 0))
  (SETF (FD-FILL-POINTER FD) 200)
  ;; Set width of blinker and space fields from the space character.
  (SETF (FD-BLINKER-WIDTH FD) (FD-SPACE-WIDTH FD))
  FD)

;;;IDX is the address of the character-pointer
(DEFUN READ-AL-INTO-FONT-DESCRIPTOR-1 (CD ARRAY IDX XOFF &AUX XW HD-AND-XH)
  (SETQ IDX (+ IDX (AREF ARRAY IDX)))
  (SETQ XW (AREF ARRAY IDX)
        HD-AND-XH (AREF ARRAY (1+ IDX)))
  (DO ((I (- IDX (LDB 0010 HD-AND-XH)) (1+ I))
       (Y (LDB 1010 HD-AND-XH) (1+ Y)))
      ((= I IDX))
    (DO ((BITS (AREF ARRAY I) (LSH BITS 1))
         (X XOFF (1+ X))
         (CW (CD-CHAR-WIDTH CD)))
        ((OR (ZEROP BITS) ( X CW)))            ;Can be garbage to right of raster
      (AND (BIT-TEST 100000 BITS)
           (ASET 1 CD Y X))))
  (OR (BIT-TEST 1 XW)
      (READ-AL-INTO-FONT-DESCRIPTOR-1 CD ARRAY (FLOOR XW 2) (+ XOFF 16.))))

(DEFUN WRITE-FONT-INTO-AL (FONTNAME &OPTIONAL FILENAME)
  (MULTIPLE-VALUE (FONTNAME FILENAME)
    (GET-OUTPUT-FILENAME-AND-FONTNAME FONTNAME FILENAME "AL"))
  (LET ((FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME)))
    (WRITE-FONT-DESCRIPTOR-INTO-AL FD FILENAME)))

;I don't think this does the desired thing for variable-width ("proportional") fonts
(DEFUN WRITE-FONT-DESCRIPTOR-INTO-AL (FD &OPTIONAL FILENAME &AUX ARRAY CARRAY LINE-HEIGHT)
  (SETQ FILENAME (GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR FD FILENAME "AL"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:OUT :FIXNUM))
    (FUNCALL STREAM ':TYO (SETQ LINE-HEIGHT (FD-LINE-SPACING FD)))
    (FUNCALL STREAM ':TYO (DPB (FD-BASELINE FD) 1007 (FD-SPACE-WIDTH FD)))
    (SETQ ARRAY (MAKE-ARRAY 1000. ':TYPE 'ART-16B ':LEADER-LIST '(0))   ;Data array
          CARRAY (MAKE-ARRAY 400 ':TYPE 'ART-16B ':LEADER-LIST '(400))) ;Non self-rel chars
    ;; Store dummy
    (ARRAY-PUSH-EXTEND ARRAY 1)
    (ARRAY-PUSH-EXTEND ARRAY 0)
    (LOOP FOR CH FROM 0 BELOW (LENGTH FD)
          AS CD = (AREF FD CH)
          WHEN CD
          DO (LOOP WITH CH = CH WITH (XW HD-XH)
                   WITH CHAR-WIDTH = (CD-CHAR-WIDTH CD)
                   FOR XOFF FROM 0 BY 16. BELOW CHAR-WIDTH
                   DO (SETQ HD-XH (WRITE-AL-COLUMN CD XOFF ARRAY))
                      (ASET (ARRAY-LEADER ARRAY 0) CARRAY CH)
                      (SETQ XW (IF (> (- CHAR-WIDTH XOFF) 16.)
                                   (* (SETQ CH (PROG1 (ARRAY-LEADER CARRAY 0)
                                                      (ARRAY-PUSH-EXTEND CARRAY 0)))
                                      2)
                                   (1+ (* (- CHAR-WIDTH XOFF) 2))))
                      (ARRAY-PUSH-EXTEND ARRAY XW)
                      (ARRAY-PUSH-EXTEND ARRAY HD-XH))
          ELSE DO (ASET 0 CARRAY CH))
    (LOOP FOR I FROM 0 BELOW (ARRAY-ACTIVE-LENGTH CARRAY)       ;Make self-relative
          DO (ASET (- (+ (AREF CARRAY I) (ARRAY-ACTIVE-LENGTH CARRAY)) I)
                   CARRAY I))
    (FUNCALL STREAM ':STRING-OUT CARRAY)
    (FUNCALL STREAM ':STRING-OUT ARRAY)
    (FUNCALL STREAM ':CLOSE)
    (FUNCALL STREAM ':TRUENAME)))

(DEFUN WRITE-AL-COLUMN-ROW (CD XOFF Y)
  (LOOP WITH STUFF = 0
        FOR X FROM (+ XOFF (CD-CHAR-LEFT-KERN CD)) BELOW (ARRAY-DIMENSION CD 1)
        AS MASK = 100000 THEN (LSH MASK -1) UNTIL (ZEROP MASK)
        WHEN (AND (NOT (MINUSP X)) (NOT (ZEROP (AREF CD Y X))))
          DO (SETQ STUFF (LOGIOR STUFF MASK))
        FINALLY (RETURN STUFF)))

(DEFUN WRITE-AL-COLUMN (CD XOFF ARRAY &AUX (HD 0) (XC 0))
  (LOOP FOR Y FROM 0 TO (LOOP FOR Y FROM (1- (ARRAY-DIMENSION CD 0)) DOWNTO 0
                              WHILE (ZEROP (WRITE-AL-COLUMN-ROW CD XOFF Y))
                              FINALLY (RETURN Y))
        AS ROW = (WRITE-AL-COLUMN-ROW CD XOFF Y)
        DO (IF (AND (ZEROP ROW) (ZEROP XC))
               (SETQ HD (1+ HD))
               (ARRAY-PUSH-EXTEND ARRAY ROW)
               (SETQ XC (1+ XC))))
  (DPB HD 1010 XC))

;;;; ALTO .KS format

;;; Load a kerned-strike file into a font
(DEFUN READ-KS-INTO-FONT (FILENAME &OPTIONAL FONTNAME)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "KS"))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME
        (READ-KS-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  FONTNAME)

;;; Load a kerned-strike font file into a font descriptor
(DEFUN READ-KS-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME
                                              &AUX FD LINE-HEIGHT MAX-WIDTH WD FIXED-WIDTH
                                              MIN-CHAR MAX-CHAR STRIKE-NWDS FBBOX
                                              ASCENT DESCENT WORDS-PER-RASTER
                                              BITMAP BITMAP16 INDEX)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "KS"))
  (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME))
  (SETF (FD-NAME FD) FONTNAME)
  (WITH-OPEN-FILE (STREAM FILENAME '(:IN :FIXNUM))
    (SETQ WD (FUNCALL STREAM ':TYI))
    (OR (BIT-TEST #o100000 WD) (FERROR NIL "Not /"new format/""))
    (AND (BIT-TEST #o40000 WD) (FERROR NIL "StrikeIndex format not understood"))
    (SETQ FIXED-WIDTH (BIT-TEST 20000 WD))
    (OR (BIT-TEST #o10000 WD) (FERROR NIL "PlainStrike format not understood"))
    (SETQ MIN-CHAR (SEND STREAM ':TYI) MAX-CHAR (SEND STREAM ':TYI))
    (SETQ MAX-WIDTH (SEND STREAM ':TYI))
    (SETQ FBBOX (SEND STREAM ':TYI))            ;Font bounding-box X-offset
    (IF (BIT-TEST #o100000 FBBOX) (SETQ FBBOX (- FBBOX #o200000)))      ;signed
    (DOTIMES (I 3) (SEND STREAM ':TYI))         ;Ignore rest of font bounding box
    (SETQ STRIKE-NWDS (SEND STREAM ':TYI)
          ASCENT (SEND STREAM ':TYI)
          DESCENT (SEND STREAM ':TYI))
    (SETQ LINE-HEIGHT (+ ASCENT DESCENT))
    (FUNCALL STREAM ':TYI)                      ;Ignore xoffset
    (SETQ WORDS-PER-RASTER (FUNCALL STREAM ':TYI))
    (SETF (FD-LINE-SPACING FD) LINE-HEIGHT)
    (SETF (FD-BLINKER-HEIGHT FD) LINE-HEIGHT)
    (SETF (FD-BASELINE FD) ASCENT)
    (SETF (FD-SPACE-WIDTH FD) MAX-WIDTH)
    ;; Now copy the bitmap (the goddamn words are bit-reversed!)
    (SETQ BITMAP16 (MAKE-ARRAY (LIST WORDS-PER-RASTER LINE-HEIGHT) ':TYPE 'ART-16B)
          BITMAP (MAKE-ARRAY (LIST (* WORDS-PER-RASTER 16.) LINE-HEIGHT)
                             ':TYPE 'ART-1B ':DISPLACED-TO BITMAP16))
    (DOTIMES (Y LINE-HEIGHT)
      (DOTIMES (X WORDS-PER-RASTER)
        (ASET (FUNCALL STREAM ':TYI) BITMAP16 (- WORDS-PER-RASTER X 1) Y)))
    ;; Copy the index segment
    (SETQ INDEX (MAKE-ARRAY (+ (- MAX-CHAR MIN-CHAR) 3) ':TYPE 'ART-16B))
    (DOTIMES (I (ARRAY-LENGTH INDEX))
      (ASET (FUNCALL STREAM ':TYI) INDEX I))
    ;; Read the width table and make the characters
    (LOOP FOR CH FROM MIN-CHAR TO (MIN (1+ MAX-CHAR) #o177) WITH CD
          AS XLEFT = (AREF INDEX (- CH MIN-CHAR))
          AS XRIGHT = (AREF INDEX (1+ (- CH MIN-CHAR)))
          AS WDE = (FUNCALL STREAM ':TYI)
          UNLESS (= WDE #o177777)               ;Nonexistent character
          DO (SETQ CD (MAKE-CHAR-DESCRIPTOR
                        :MAKE-ARRAY (:TYPE ART-1B
                                    :LENGTH (LIST LINE-HEIGHT (- XRIGHT XLEFT)))))
             (SETF (CD-CHAR-WIDTH CD) (LDB 0010 WDE))
             (AND (= CH #\SP) (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD)))
             (SETF (CD-CHAR-LEFT-KERN CD) (- (+ (LDB #o1010 WDE) FBBOX)))
             (ASET CD FD CH)
             (LOOP FOR Y FROM 0 BELOW LINE-HEIGHT DO
               (LOOP FOR X FROM 0 BELOW (- XRIGHT XLEFT)
                     AS BMX DOWNFROM (- (ARRAY-DIMENSION BITMAP 0) XLEFT 1)
                     DO (ASET (AREF BITMAP BMX Y) CD Y X))))
  (SETF (FD-FILL-POINTER FD) 200)
  ;; Set width of blinker and space fields from the space character.
  (SETF (FD-BLINKER-WIDTH FD) (FD-SPACE-WIDTH FD))
  FD))

;; AC files

(DEFMACRO SIGN-EXTEND (16BIT-WORD)
  `(LET ((WORD ,16BIT-WORD))
     (IF ( WORD #o100000)
         (- WORD #o200000)
         WORD)))

(DEFMACRO HIGH-BYTE (WORD)
  `(LSH ,WORD -8))

(DEFMACRO LOW-BYTE (WORD)
  `(BOOLE 1 #o377 ,WORD))

;;; Get next 16-bit word from input-file
(DEFUN NEXT-WORD ()
  (DECLARE (SPECIAL INPUT-FILE))
  (SEND INPUT-FILE ':TYI "Unexpected EOF on AC file"))

;;; Read in an AC file as a Lisp machine font.
(DEFUN READ-AC-INTO-FONT (FILENAME &OPTIONAL FONTNAME)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AC"))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME
                                     (READ-AC-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  FONTNAME)

;; Copied from LAD: RELEASE-3.IO1; FNTCNV.LISP#86 on 2-Oct-86 05:27:07
(DEFUN READ-AC-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME
                                              &AUX FAMILY-NAME FACE-CODE POINT-SIZE)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AC"))
  (WITH-OPEN-FILE (INPUT-FILE FILENAME '(:READ :FIXNUM))
    (DECLARE (SPECIAL INPUT-FILE))
    (SETF (VALUES FAMILY-NAME FACE-CODE POINT-SIZE)
          (PRESS:DECODE-FONT-NAME (STRING FONTNAME)))
    (LET ((CODE-ALIST NIL)
          (SEGMENT-DATA NIL)
          FAMILY-CODE TEM SEGMENT
          FD
          (WD 0))
      (SETQ WD (NEXT-WORD))
      ;; Read IXN entries (type 1)
      (DO () ((NOT (= (LSH WD -12.) 1)))
        (LET ((CODE (NEXT-WORD))
              (NAME (PRESS:BCPL-STRING 20. INPUT-FILE)))
          (PUSH (CONS CODE NAME) CODE-ALIST))
        (SETQ WD (NEXT-WORD)))
      ;; Find out the code number for the font family to be used,
      ;; either the specified one or the only one.
      (COND (FAMILY-NAME
             (OR (SETQ FAMILY-CODE (CAR (RASS 'EQUALP FAMILY-NAME CODE-ALIST)))
                 (FERROR NIL "Font family ~A not present in AC file" FAMILY-NAME)))
            ((CDR CODE-ALIST)
             (FERROR NIL "Font dictionary ~A: font family not specified" FILENAME))
            (T (SETQ FAMILY-CODE (CAAR CODE-ALIST))))
      ;; Read Index Entries (type 3) for AC segments.
      (DO () ((NOT (= (LSH WD -12.) 3)))
        (SETQ WD (NEXT-WORD))                   ;family,,face
        (SETQ TEM
              (LIST (HIGH-BYTE WD)              ;Family code number.
                    (PRESS:DECODE-FACE (LOW-BYTE WD))   ;Face name
                    (PROGN (SETQ WD (NEXT-WORD))        ;bc,,ec
                           (HIGH-BYTE WD))      ;First code
                    (LOW-BYTE WD)               ;Last code
                    (NEXT-WORD)                 ;Size
                    (NEXT-WORD)                 ;Rotation
                    (+ (LSH (NEXT-WORD) 16.) (NEXT-WORD))       ;Segment SA
                    (+ (LSH (NEXT-WORD) 16.) (NEXT-WORD))       ;Segment Len
                    (NEXT-WORD)                 ;horiz resolution
                    (NEXT-WORD)))               ;vert resolution
        (AND (= (CAR TEM) FAMILY-CODE) (PUSH TEM SEGMENT-DATA))
        (SETQ WD (NEXT-WORD)))
      ;; Now should have type-0 entry (end of index)
      (SELECTQ (LSH WD -12.)
        (0 )
        (5 (FERROR NIL "There are OrbitChars segments in this file -- I don't grok them"))
        (OTHERWISE
          (FERROR NIL "~O - Garbage in file where type 0 IX expected" WD)))
      ;; Now either there should be only one segment or the face code and size
      ;; should have been specified.
      (COND ((AND POINT-SIZE FACE-CODE)
             (DOLIST (SEG SEGMENT-DATA)
               (AND (STRING-EQUAL (CADR SEG) FACE-CODE)
                    (= (ROUND (* (FIFTH SEG) 72.) 2540.) POINT-SIZE)
                    (RETURN (SETQ SEGMENT SEG))))
             (OR SEGMENT (FERROR NIL "Font ~A not found in AC file" FONTNAME)))
            ((CDR SEGMENT-DATA)
             (FERROR "Font dictionary ~A: point size or face code not specified" FILENAME))
            (T (SETQ SEGMENT (CAR SEGMENT-DATA)
                     POINT-SIZE (ROUND (* (FIFTH SEGMENT) 72.) 2540.))))
      (FUNCALL INPUT-FILE ':SET-POINTER (SEVENTH SEGMENT))
      (LET ((BC (THIRD SEGMENT))
            (EC (FOURTH SEGMENT))
            (BASELINE 0)
            (XWIDTHS (MAKE-ARRAY #o400))
            (YWIDTHS (MAKE-ARRAY #o400))
            (BOX-X-OFFSET (MAKE-ARRAY #o400))
            (BOX-Y-OFFSET (MAKE-ARRAY #o400))
            (BOX-X-SIZE (MAKE-ARRAY #o400))
            (BOX-Y-SIZE (MAKE-ARRAY #o400))
            (CHAR-DATA-POSITION (MAKE-ARRAY #o400))
            LINE-HEIGHT)
        ;; read in the widths info from the segment.
        (DO ((I BC (1+ I))) ((> I EC))
          (ASET (+ (SIGN-EXTEND (NEXT-WORD)) (// (NEXT-WORD) 65536.0))
                XWIDTHS I)
          (ASET (+ (SIGN-EXTEND (NEXT-WORD)) (// (NEXT-WORD) 65536.0))
                YWIDTHS I)
          (ASET (SIGN-EXTEND (NEXT-WORD)) BOX-X-OFFSET I)
          (ASET (SIGN-EXTEND (NEXT-WORD)) BOX-Y-OFFSET I)
          (ASET (SIGN-EXTEND (NEXT-WORD)) BOX-X-SIZE I)
          (ASET (SIGN-EXTEND (NEXT-WORD)) BOX-Y-SIZE I))
        ;; Read relative pointers to character data beginnings
        ;; and convert them to absolute pointers within the file.
        (DO ((I BC (1+ I))
             (STARTING-POSITION (FUNCALL INPUT-FILE ':READ-POINTER)))
            ((> I EC))
          (SETF (AREF CHAR-DATA-POSITION I)
                (+ (LSH (NEXT-WORD) 32.)
                   (NEXT-WORD)
                   STARTING-POSITION)))
        (SETQ FONTNAME (STRING-APPEND (CDR (ASSQ FAMILY-CODE CODE-ALIST))
                                      (FORMAT NIL "~D" POINT-SIZE)
                                      (OR (SECOND SEGMENT) "")))
        (SETQ FONTNAME (INTERN (STRING-UPCASE FONTNAME) "FONTS"))
        (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME
                                       :MAKE-ARRAY (:LENGTH (1+ EC))))
        (SETF (FD-FILL-POINTER FD) (1+ EC))
        (SETF (FD-VERT-RESOLUTION FD) (NTH 9 SEGMENT))
        (SETF (FD-HORIZ-RESOLUTION FD) (NTH 8 SEGMENT))
        (SETF (FD-ROTATION FD) (NTH 5 SEGMENT))
        (DO ((HEIGHT 0)
             (I BC (1+ I)))
            ((> I EC)
             (SETQ LINE-HEIGHT (+ HEIGHT BASELINE))
             (SETF (FD-LINE-SPACING FD) LINE-HEIGHT)
             (SETF (FD-BLINKER-HEIGHT FD) LINE-HEIGHT)
             (SETF (FD-BASELINE FD) (- LINE-HEIGHT BASELINE)))
          (COND (( (AREF BOX-Y-SIZE I) -1)
                 (SETQ HEIGHT (MAX HEIGHT (+ (AREF BOX-Y-SIZE I) (AREF BOX-Y-OFFSET I))))))
          (COND (( (AREF BOX-Y-SIZE I) -1)
                 (SETQ BASELINE (MAX BASELINE (- (AREF BOX-Y-OFFSET I)))))))
        (DO ((CH BC (1+ CH))
             (CHAR-WIDTH)
             (RASTER-HEIGHT)
             (RASTER-WIDTH)
             (CHAR-Y-OFFSET)
             (WD)
             (CD))
            ((> CH EC))
          (COND (( (AREF BOX-Y-SIZE CH) -1)
                 (SETQ CHAR-WIDTH (AREF XWIDTHS CH))
                 (SETQ RASTER-WIDTH (AREF BOX-X-SIZE CH))
                 (SETQ RASTER-HEIGHT (AREF BOX-Y-SIZE CH))
                 (SETQ CHAR-Y-OFFSET (AREF BOX-Y-OFFSET CH))
                 (SETQ CD (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE ART-1B
                                                            :LENGTH (LIST LINE-HEIGHT
                                                                          RASTER-WIDTH))))
                 (SETF (CD-CHAR-WIDTH CD) CHAR-WIDTH)
                 (SETF (CD-CHAR-VERT-WIDTH CD) (AREF YWIDTHS CH))
                 (AND (= CH #\SP) (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
                 (SETF (CD-CHAR-LEFT-KERN CD) (- (AREF BOX-X-OFFSET CH)))
                 ;; Store the CD in the font descriptor
                 (ASET CD FD CH)
                 ;; Verify that the relative pointer to this character's data was right.
                 (OR (= (AREF CHAR-DATA-POSITION CH)
                        (FUNCALL INPUT-FILE ':READ-POINTER))
                     (FERROR NIL "Inconsistent character data pointer for character ~C" CH))
                 ;; Skip a word of redundant info on raster height and width.
                 (LET ((TEM (NEXT-WORD)))
                   (OR (= TEM (DPB (CEILING RASTER-HEIGHT #o20) #o1206 RASTER-WIDTH))
                       (FERROR NIL
        "Inconsistent raster size data at front of character bits for character ~C"
                               CH)))
                 ;; Raster lines go vertically up, leftmost line first.
                 (DOTIMES (HPOS RASTER-WIDTH)
                   ;; Read in the next vertical scan line.
                   (DOTIMES (VPOS RASTER-HEIGHT)
                     ;; If wd is exhausted, get next word into wd
                     (COND ((ZEROP (\ VPOS 16.))
                            (SETQ WD (NEXT-WORD))))
                     (SETQ TEM (LDB #o1701 (LSH WD (\ VPOS 16.))))
                     (OR ( (+ VPOS BASELINE CHAR-Y-OFFSET) LINE-HEIGHT)
                         (ASET TEM CD
                               (- LINE-HEIGHT 1 (+ VPOS BASELINE CHAR-Y-OFFSET))
                               HPOS)))))))
        ;; Set width of blinker and space fields from the space character.
        (SETF (FD-BLINKER-WIDTH FD) (FD-SPACE-WIDTH FD))
        (OR (= (FUNCALL INPUT-FILE ':READ-POINTER)
               (+ (NTH 6 SEGMENT) (NTH 7 SEGMENT)))
            (FERROR NIL "Inconsistent data-length in index of AC file"))
        FD))))

;;; This returns in array units, which are from the upper-left corner
(DEFUN CD-RASTER-RANGE (CD)
  (DECLARE (RETURN-LIST MINX MINY MAXX MAXY))
  (LOOP WITH HEIGHT = (ARRAY-DIMENSION CD 0)
        AND WIDTH = (ARRAY-DIMENSION CD 1)
        WITH MINX = WIDTH AND MINY = HEIGHT
        AND MAXX = 0 AND MAXY = 0
        FOR X FROM 0 BELOW WIDTH
        DO (LOOP FOR Y FROM 0 BELOW HEIGHT
                 WHEN (NOT (ZEROP (AREF CD Y X)))
                 DO (SETQ MINX (MIN MINX X)
                          MAXX (MAX MAXX (1+ X))
                          MINY (MIN MINY Y)
                          MAXY (MAX MAXY (1+ Y))))
        FINALLY (RETURN (VALUES (MIN MINX MAXX) (MIN MINY MAXY) MAXX MAXY))))

;Return how many rasters are empty (all 0)
;at the bottom of the character and at the top.
(DEFUN CD-UNUSED-RASTER-HEIGHT (CD)
  (DECLARE (RETURN-LIST BOTTOM TOP))
  (LET* ((DIMS (ARRAY-DIMENSIONS CD))
         BOTTOM TOP
         (HEIGHT (CAR DIMS))
         (WIDTH (CADR DIMS)))
    (DOTIMES (I HEIGHT)
      (AND (DOTIMES (J WIDTH)
             (AND (NOT (ZEROP (AREF CD I J)))
                  (RETURN T)))
           (RETURN (SETQ TOP I))))
    (DOTIMES (I HEIGHT)
      (AND (DOTIMES (J WIDTH)
             (AND (NOT (ZEROP (AREF CD (- HEIGHT I 1) J)))
                  (RETURN T)))
           (RETURN (SETQ BOTTOM I))))
    (COND (TOP (VALUES BOTTOM TOP))
          ;; Handle case where all data is empty.
          (T (VALUES HEIGHT 0)))))

;Write an AC file from a Lisp machine font.
(DEFUN WRITE-FONT-INTO-AC (FONTNAME &OPTIONAL FILENAME)
  (MULTIPLE-VALUE (FONTNAME FILENAME)
    (GET-OUTPUT-FILENAME-AND-FONTNAME FONTNAME FILENAME "AC"))
  (LET ((FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME)))
    (WRITE-FONT-DESCRIPTOR-INTO-AC FD FILENAME)))

(DEFUN WRITE-FONT-DESCRIPTOR-INTO-AC (FD &OPTIONAL FILENAME
                                         &AUX FAMILY-NAME FACE-CODE POINT-SIZE BC EC)
  (SETQ FILENAME (GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR FD FILENAME "AC"))
  (SETF (VALUES FAMILY-NAME FACE-CODE POINT-SIZE)
        (PRESS:DECODE-FONT-NAME (STRING (FD-NAME FD))))
  (WITH-OPEN-FILE (OUTPUT-FILE FILENAME '(:WRITE :FIXNUM))
    (LET* ((FONT-LENGTH (ARRAY-ACTIVE-LENGTH FD))
           UNUSED-TOP-RASTERS UNUSED-BOTTOM-RASTERS
           CHAR-DATA-SIZE-TABLE (TOTAL-DATA-SIZE 0))

      (AND (> FONT-LENGTH #o400)
           (PROGN (SETQ FONT-LENGTH #o400)
                  (CERROR T NIL NIL "Font contains codes above 377 which AC file cannot hold"
                          )))

      (SETQ UNUSED-TOP-RASTERS (MAKE-ARRAY FONT-LENGTH)
            UNUSED-BOTTOM-RASTERS (MAKE-ARRAY FONT-LENGTH))

      ;; Figure out range of chars to actually output.
      (DOTIMES (I FONT-LENGTH)
        (AND (AREF FD I) (RETURN (SETQ BC I))))
      (DO ((I (1- FONT-LENGTH) (1- I)))
          ((MINUSP I))
        (AND (AREF FD I) (RETURN (SETQ EC I))))

      ;; Precompute how much data each character is going to require.
      ;; Count number of words of rasters, plus 1 word of info before them.
      (SETQ CHAR-DATA-SIZE-TABLE (MAKE-ARRAY FONT-LENGTH))
      (DOTIMES (CHAR-CODE FONT-LENGTH)
        (LET* ((CD (AREF FD CHAR-CODE))
               (DATA-SIZE 0))
          (AND CD
               (MULTIPLE-VALUE-BIND (TEM1 TEM2)
                   (CD-UNUSED-RASTER-HEIGHT CD)
                 (SETF (AREF UNUSED-BOTTOM-RASTERS CHAR-CODE) TEM1)
                 (SETF (AREF UNUSED-TOP-RASTERS CHAR-CODE) TEM2)
                 (SETQ DATA-SIZE (1+ (* (CEILING (- (ARRAY-DIMENSION CD 0) TEM1 TEM2) #o20)
                                        (ARRAY-DIMENSION CD 1))))))
          (SETQ TOTAL-DATA-SIZE (+ TOTAL-DATA-SIZE DATA-SIZE))
          (SETF (AREF CHAR-DATA-SIZE-TABLE CHAR-CODE) DATA-SIZE)))
      (SETQ TOTAL-DATA-SIZE (+ TOTAL-DATA-SIZE (* 10. (- EC BC -1))))
      ;; Index entry type 1, 12. words long.
      (SEND OUTPUT-FILE ':TYO (DPB 1 #o1404 12.))
      ;; Family code - always 1, since we only write one family.
      (SEND OUTPUT-FILE ':TYO 1)
      ;; Write the family name now.
      (DO ((I 0 (1+ I))
           (LEN (STRING-LENGTH FAMILY-NAME))
           (HIGH-BYTE (STRING-LENGTH FAMILY-NAME)))
          ((= I 19.))
        (LET ((CH (IF (< I LEN)
                      (AREF FAMILY-NAME I)
                      0)))
          (IF HIGH-BYTE
              (PROGN (SEND OUTPUT-FILE ':TYO
                           (DPB HIGH-BYTE #o1010 CH))
                     (SETQ HIGH-BYTE NIL))
              (SETQ HIGH-BYTE CH))))
      ;; Now write the index entry for the data segment.
      ;; Type 3, 11. words long.
      (SEND OUTPUT-FILE ':TYO (DPB 3 #o1404 11.))
      ;; Now family code and face code.
      (SEND OUTPUT-FILE ':TYO (DPB 1 #o1010 (PRESS:ENCODE-PRESS-FACE FACE-CODE)))
      ;; Now range of characters actually existing in the font.
      (SEND OUTPUT-FILE ':TYO (DPB BC #o1010 EC))
      ;; Now size of font.
      (SEND OUTPUT-FILE ':TYO (ROUND (* POINT-SIZE 2540.) 72.))
      ;; Rotation is 0.
      (SEND OUTPUT-FILE ':TYO 0)
      ;; Position in file of start of data (2 words).
      (SEND OUTPUT-FILE ':TYO 0)
      (SEND OUTPUT-FILE ':TYO 24.)
      ;; Number of words of data.
      (SEND OUTPUT-FILE ':TYO (LDB #o2020 TOTAL-DATA-SIZE))
      (SEND OUTPUT-FILE ':TYO (LDB #o0020 TOTAL-DATA-SIZE))
      ;; Resolutions
      (FUNCALL OUTPUT-FILE ':TYO (FD-HORIZ-RESOLUTION FD))
      (FUNCALL OUTPUT-FILE ':TYO (FD-VERT-RESOLUTION FD))
      ;; Index entry type 0, end of index.
      (FUNCALL OUTPUT-FILE ':TYO 1)

      ;; Output descriptions of the characters in the font.
      (DO ((CHAR-CODE BC (1+ CHAR-CODE)))
          ((> CHAR-CODE EC))
        (LET ((CD (AREF FD CHAR-CODE)))
          (COND ((NULL CD)
                 (FUNCALL OUTPUT-FILE ':TYO 0)
                 (FUNCALL OUTPUT-FILE ':TYO 0)
                 (FUNCALL OUTPUT-FILE ':TYO 0)
                 (FUNCALL OUTPUT-FILE ':TYO 0)
                 (FUNCALL OUTPUT-FILE ':TYO 0)
                 (FUNCALL OUTPUT-FILE ':TYO 0)
                 (FUNCALL OUTPUT-FILE ':TYO 0)
                 (FUNCALL OUTPUT-FILE ':TYO 177777))
                (T
                 (FUNCALL OUTPUT-FILE ':TYO (FIX (CD-CHAR-WIDTH CD)))
                 (FUNCALL OUTPUT-FILE ':TYO
                          (FIX (* 65536. (- (CD-CHAR-WIDTH CD) (FIX (CD-CHAR-WIDTH CD))))))
                 (FUNCALL OUTPUT-FILE ':TYO (FIX (CD-CHAR-VERT-WIDTH CD)))
                 (FUNCALL OUTPUT-FILE ':TYO
                          (FIX (* 65536. (- (CD-CHAR-VERT-WIDTH CD)
                                            (FIX (CD-CHAR-VERT-WIDTH CD))))))
                 (FUNCALL OUTPUT-FILE ':TYO (- (CD-CHAR-LEFT-KERN CD)))
                 (FUNCALL OUTPUT-FILE ':TYO (+ (- (FD-BASELINE FD)
                                                  (FD-LINE-SPACING FD))
                                               (AREF UNUSED-BOTTOM-RASTERS CHAR-CODE)))
                 (FUNCALL OUTPUT-FILE ':TYO (CADR (ARRAY-DIMENSIONS CD)))
                 (FUNCALL OUTPUT-FILE ':TYO (- (CAR (ARRAY-DIMENSIONS CD))
                                               (AREF UNUSED-BOTTOM-RASTERS CHAR-CODE)
                                               (AREF UNUSED-TOP-RASTERS CHAR-CODE)))))))
      ;; Output offsets to data for each character.
      (LET* ((CURRENT-POS (+ 24. (* 8 (- EC BC -1))))
             (EXPECTED-DATA-START
               (+ CURRENT-POS (* 2 (- EC BC -1)))))
        (DO ((CHAR-CODE BC (1+ CHAR-CODE)))
            ((> CHAR-CODE EC))
          (LET ((OFFSET (- EXPECTED-DATA-START CURRENT-POS)))
            (FUNCALL OUTPUT-FILE ':TYO (LDB 2020 OFFSET))
            (FUNCALL OUTPUT-FILE ':TYO (LDB 0020 OFFSET)))
          (SETQ EXPECTED-DATA-START
                (+ EXPECTED-DATA-START
                   (AREF CHAR-DATA-SIZE-TABLE CHAR-CODE)))))

      ;; Now output the rasters themselves.
      (DO ((CHAR-CODE BC (1+ CHAR-CODE)))
          ((> CHAR-CODE EC))
        (LET* (WD
               (CD (AREF FD CHAR-CODE)))
          (COND (CD
                 (LET* ((RASTER-WIDTH (ARRAY-DIMENSION CD 1))
                        (RASTER-BOTTOM-SKIP (AREF UNUSED-BOTTOM-RASTERS CHAR-CODE))
                        (REAL-RASTER-HEIGHT (ARRAY-DIMENSION CD 0))
                        (RASTER-HEIGHT (- REAL-RASTER-HEIGHT
                                          RASTER-BOTTOM-SKIP
                                          (AREF UNUSED-TOP-RASTERS CHAR-CODE))))
                   ;; Output redundant raster size info word.
                   (FUNCALL OUTPUT-FILE ':TYO
                            (DPB (CEILING RASTER-HEIGHT 20)
                                 1206
                                 (ARRAY-DIMENSION CD 1)))
                   ;; Raster lines go vertically up, leftmost line first.
                   (DOTIMES (HPOS RASTER-WIDTH)
                     (SETQ WD 0)
                     ;; Write the next vertical scan line.
                     (DOTIMES (VPOS RASTER-HEIGHT)
                       (SETQ WD (+ WD (LSH (AREF CD (- REAL-RASTER-HEIGHT
                                                       1 VPOS RASTER-BOTTOM-SKIP)
                                                 HPOS)

                                           (- 17 (\ VPOS 20)))))
                       ;; If wd is full, output it.
                       (AND (ZEROP (\ (1+ VPOS) 16.))
                            (PROGN (FUNCALL OUTPUT-FILE ':TYO WD)
                                   (SETQ WD 0))))
                     ;; Output partially-filled word at end.
                     (OR (ZEROP (\ RASTER-HEIGHT 16.))
                         (FUNCALL OUTPUT-FILE ':TYO WD)))))))))
    (FUNCALL OUTPUT-FILE ':CLOSE)
    (FUNCALL OUTPUT-FILE ':TRUENAME)))

(DEFUN READ-AST-INTO-FONT (FILENAME &OPTIONAL FONTNAME)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AST"))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME
                                     (READ-AST-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  FONTNAME)

(DEFUN READ-AST-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME &AUX FD)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AST"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:IN))
    (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME :MAKE-ARRAY (:LENGTH 200)))
    (READ-AST-DN STREAM)                        ;DISCARD KSTID
    (SETF (FD-LINE-SPACING FD) (READ-AST-DN STREAM))
    (SETF (FD-BASELINE FD) (READ-AST-DN STREAM))
    (READ-AST-DN STREAM)                        ;COLUMN POSITION ADJUSTMENT
    (SETF (FD-SPACE-WIDTH FD) 0)                ;Just in case no space character.
    (SETF (FD-BLINKER-HEIGHT FD)
          (FD-LINE-SPACING FD))
    (SETF (FD-NAME FD) FONTNAME)
    (LET (KERN CHAR-CODE RASTER-WIDTH INPUT-RASTER-WIDTH CHAR-WIDTH
          CD CH (LINE-HEIGHT (FD-LINE-SPACING FD)))
      (DO ()
          ((NULL (READ-AST-NEXT-PAGE STREAM)))
        (SETQ CHAR-CODE (READ-AST-ON STREAM))
        (SETQ INPUT-RASTER-WIDTH (READ-AST-DN STREAM) RASTER-WIDTH INPUT-RASTER-WIDTH)
        (SETQ CHAR-WIDTH (READ-AST-DN STREAM))
        (SETQ KERN (READ-AST-DN STREAM))
        (COND ((< KERN 0)                       ;FED compact raster lossage
               (SETQ RASTER-WIDTH (+ RASTER-WIDTH (ABS KERN)))
               (SETQ KERN 0)))
        (SETQ CD (MAKE-CHAR-DESCRIPTOR
                   :MAKE-ARRAY (:TYPE ART-1B :LENGTH (LIST LINE-HEIGHT RASTER-WIDTH))))
        (SETF (CD-CHAR-WIDTH CD) CHAR-WIDTH)
        (SETF (CD-CHAR-LEFT-KERN CD) KERN)
        (FD-STORE-CD FD CD CHAR-CODE)
        (AND (= CHAR-CODE #\SP)
             (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
        (DO-NAMED TOP ((VPOS 0 (1+ VPOS)))
                  ((= VPOS LINE-HEIGHT))
          (DO ((HCNT 0 (1+ HCNT)))
              ((= HCNT INPUT-RASTER-WIDTH)
               (DO ((CH)) ()
                 (COND ((OR (NULL (SETQ CH (FUNCALL STREAM ':TYI)))
                            (= CH #\RETURN))
                        (RETURN NIL))
                       ((= CH #\FORM)
                        (FUNCALL STREAM ':UNTYI CH)
                        (RETURN-FROM TOP NIL))
                       ((NOT (= CH #\SPACE))
                        (FERROR NIL "non space seen past raster width")))))
            (SETQ CH (FUNCALL STREAM ':TYI))
            (COND ((NULL CH)
                   (RETURN-FROM TOP NIL))
                  ((= CH #\FORM)
                   (FUNCALL STREAM ':UNTYI CH)
                   (RETURN-FROM TOP NIL))
                  ((OR (< CH 40) (> CH 200))
                   (DO () ((= CH #\RETURN)) (SETQ CH (FUNCALL STREAM ':TYI)))
                   (RETURN NIL))
                  ((> CH 40)
                   (ASET 1 CD VPOS (+ HCNT (- RASTER-WIDTH INPUT-RASTER-WIDTH)))))))
     ; (COND ((> CHAR-CODE 37) (TYO CHAR-CODE))
     ;        (T (PRINC '^) (TYO (+ 100 CHAR-CODE))))
        )
      ;; Truncate fd to discard unused elements at the end.
      (DO ((I (1- (ARRAY-LENGTH FD)) (1- I)))
          ((OR (MINUSP I)
               (AREF FD I))
           (ADJUST-ARRAY-SIZE FD (1+ I))))
      (SETF (FD-FILL-POINTER FD) (ARRAY-LENGTH FD))
      ;; Set width of blinker and space fields from the space character.
      (SETF (FD-BLINKER-WIDTH FD)
            (FD-SPACE-WIDTH FD))
      FD)))

(DEFUN READ-AST-DN (STREAM)
  (PROG (N CH SIGN)
    (SETQ N 0 SIGN 1)
    (SETQ CH (FUNCALL STREAM ':TYI))            ;LOOK FOR MINUS SIGN
    (COND ((= CH #/-)
           (SETQ SIGN -1))
          (T (GO AA)))
 A   (SETQ CH (FUNCALL STREAM ':TYI))
 AA  (AND ( CH #/0) ( CH #/9)
          (PROGN
            (SETQ N (+ (* N 10.) (- CH #/0)))
            (GO A)))
 B   (AND (= CH #\RETURN) (RETURN (* N SIGN)))
    (SETQ CH (FUNCALL STREAM ':TYI))
    (GO B)))

(DEFUN READ-AST-ON (STREAM)
  (PROG (N CH)
    (SETQ N 0)
A   (SETQ CH (FUNCALL STREAM ':TYI))
    (AND ( CH #/0) ( CH #/8)
         (PROGN
           (SETQ N (+ (* N 8) (- CH #/0)))
           (GO A)))
B   (AND (= CH #\RETURN) (RETURN N))
    (SETQ CH (FUNCALL STREAM ':TYI))
    (GO B)))

(DEFUN READ-AST-NEXT-PAGE (STREAM)
  (PROG (CH)
    (COND ((NULL (SETQ CH (FUNCALL STREAM ':TYI))) (RETURN NIL))
          ((= CH #\FORM)
           (SETQ CH (FUNCALL STREAM ':TYI))
           (FUNCALL STREAM ':UNTYI CH)
           (IF (NULL CH)
               (RETURN NIL)
               (RETURN T)))
          (T (FERROR NIL "Random char where FF expected")))))

(DEFUN WRITE-FONT-INTO-AST (FONTNAME &OPTIONAL FILENAME)
  (MULTIPLE-VALUE (FONTNAME FILENAME)
    (GET-OUTPUT-FILENAME-AND-FONTNAME FONTNAME FILENAME "AST"))
  (LET ((FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME)))
    (WRITE-FONT-DESCRIPTOR-INTO-AST FD FILENAME)))

(DEFUN WRITE-FONT-DESCRIPTOR-INTO-AST (FD &OPTIONAL FILENAME
                                          &AUX (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FD)))
  (SETQ FILENAME (GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR FD FILENAME "AST"))
  (WITH-OPEN-FILE (STREAM FILENAME '(:OUT :ASCII))
    (FORMAT STREAM "0 KSTID ~A"  FILENAME)
    (FORMAT STREAM "~%~D HEIGHT" (FD-LINE-SPACING FD))
    (FORMAT STREAM "~%~D BASE LINE" (FD-BASELINE FD))
    (FORMAT STREAM "~%0 COLUMN POSITION ADJUSTMENT~%")
    ;; Then write out all the characters.
    (LET (CD CHAR-HEIGHT LOSING-KERN KERN)
      (DOTIMES (CHAR-CODE FONT-LENGTH)
        (COND ((AND (SETQ CD (AREF FD CHAR-CODE))
                    ;; Wide fonts without chars-exist-tables can have 0-width chars.
                    (OR (NOT (ZEROP (ARRAY-DIMENSION CD 1)))
                        (NOT (ZEROP (CD-CHAR-WIDTH CD)))))
               (FUNCALL STREAM ':TYO #\FORM)
               (SETQ KERN (CD-CHAR-LEFT-KERN CD)
                     LOSING-KERN (AND (MINUSP KERN)
                                      ( (CD-CHAR-WIDTH CD)
                                         (- (ARRAY-DIMENSION CD 1) KERN))
                                      (- KERN)))
               (FORMAT STREAM "~O CHARACTER CODE ~A" CHAR-CODE FILENAME)
               (FORMAT STREAM "~%~D RASTER WIDTH" (IF LOSING-KERN (CD-CHAR-WIDTH CD)
                                                    (ARRAY-DIMENSION CD 1)))
               (FORMAT STREAM "~%~D CHARACTER WIDTH" (CD-CHAR-WIDTH CD))
               (FORMAT STREAM "~%~D LEFT KERN~%" (IF LOSING-KERN 0 KERN))
               (SETQ CHAR-HEIGHT (ARRAY-DIMENSION CD 0))
               (DOTIMES (VPOS CHAR-HEIGHT)
                 (IF LOSING-KERN (DOTIMES (HPOS LOSING-KERN)
                                   (SEND STREAM ':TYO #\SPACE)))
                 (DOTIMES (HPOS (ARRAY-DIMENSION CD 1))
                   (SEND STREAM ':TYO (IF (ZEROP (AREF CD VPOS HPOS))
                                             #\SPACE
                                             #/*)))
                 (SEND STREAM ':TYO #\RETURN))))))
    (CLOSE STREAM)
    (SEND STREAM ':TRUENAME)))
