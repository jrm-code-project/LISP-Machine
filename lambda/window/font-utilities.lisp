;;; -*- Mode:LISP; Package:FED; Base:8; Readtable:ZL -*-

(DEFUN FONT-GET-FD (FONT-SYMBOL &AUX FD)
  "Return the font descriptor corresponding to the font named FONT-SYMBOL.
This is an object of type FONT-DESCRIPTOR which contains the same
data as the font itself, but in a more convenient format.
If FONT-SYMBOL is not an existing font, create an empty FD for it."
  (IF (BOUNDP FONT-SYMBOL)
      (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
    (SETQ FD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH #o200)
                                   :FD-NAME FONT-SYMBOL
                                   :FD-LINE-SPACING 12.
                                   :FD-BASELINE 9.
                                   :FD-BLINKER-HEIGHT 12.
                                   :FD-BLINKER-WIDTH 7
                                   :FD-SPACE-WIDTH 7))
    (SETF (AREF FD #/SP) (MAKE-CHAR-DESCRIPTOR
                           :MAKE-ARRAY (:TYPE 'ART-4B :DIMENSIONS '(9. 7))
                           :CD-CHAR-WIDTH 7
                           :CD-CHAR-LEFT-KERN 0))
    (PUTPROP FONT-SYMBOL FD 'FONT-DESCRIPTOR)
    (SET FONT-SYMBOL NIL)
    (PUTPROP FONT-SYMBOL NIL 'FONT-DESCRIBED)
    FD))

;;; Display all of the characters of the font  being edited, to show what they look like.
;;;   Above each one is the corresponding character of default font, so you
;;;   can see which character is which in non-alphabetic fonts.
;;;   FROM-FED is T when called from FED, NIL when called from elsewhere, eg ZWEI.


(DEFUN FED-CHARACTER-BEING-EDITED-P (FONTNAME CHAR)
  "T if CHAR in font FONTNAME is being edited in some FED window."
  (declare (special fed-edited-chars))
  (DOLIST (ELT FED-EDITED-CHARS)
    (AND (EQ FONTNAME (CAR ELT)) (= CHAR (CADR ELT))
         (RETURN T))))

(DEFUN FED-DISPLAY-FONT-CHAR-WIDTH (FD DF CH)
  "Return the width of char CH in font DF or font descriptor FD, whichever is larger."
  (MAX (FED-CHAR-DISPLAY-WIDTH FD CH)
       (FONT-CHARACTER-WIDTH DF CH)))

(DEFUN FED-CHAR-DISPLAY-WIDTH (FD CHAR)
  (COND ((AND (< CHAR (ARRAY-LENGTH FD))
              (AREF FD CHAR))
         (+ 3 (ARRAY-DIMENSION (AREF FD CHAR) 1)
            (MAX 0 (- (CD-CHAR-LEFT-KERN (AREF FD CHAR))))))
        (T 0)))

;;; Return the width of a given char in a given font.

(DEFUN FONT-CHARACTER-WIDTH (FONT CHAR)
  "Return the width of char CHAR in font FONT."
  (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
    (IF CWT (AREF CWT CHAR)
        (FONT-CHAR-WIDTH FONT))))

;To understand this, it is vital to understand that the cursor is always in the top left corner
;of the next character cell, which is as tall as the tallest font in the font map.  All fonts
;will be aligned by their baselines, so cursor may be way above the character displayed.
(DEFUN DISPLAY-FONT (FONT &OPTIONAL (WINDOW TERMINAL-IO) (CLEAR-FIRST-P T) (FROM-FED NIL))
  "Display the contents of font FONT on WINDOW.
CLEAR-FIRST-P says clear the window before displaying this.
FROM-FED is T when called from FED, NIL when called from elsewhere."
  (WHEN (SYMBOLP FONT)
    (SETQ FONT (SEND (TV:SHEET-GET-SCREEN WINDOW) :PARSE-FONT-DESCRIPTOR FONT)))
  (IF FONT
      (LET ((FONT-MAP (SEND WINDOW :FONT-MAP))
            (CURRENT-FONT (SEND WINDOW :CURRENT-FONT)))
        (UNWIND-PROTECT
            (PROG* ((NAME (FONT-NAME FONT))
                    (FD (FONT-GET-FD NAME))
                    (DF (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN WINDOW))))
                   (SEND WINDOW :SET-FONT-MAP (LIST DF FONT))
                   (AND CLEAR-FIRST-P (SEND WINDOW :CLEAR-SCREEN))
                   (FORMAT WINDOW "~2&Font ~A:~&" NAME)
                   (COND ((> (+ (FONT-CHAR-HEIGHT FONT)
                                (TV:SHEET-LINE-HEIGHT WINDOW))
                             (- (TV:SHEET-INSIDE-BOTTOM WINDOW)
                                (TV:SHEET-LINE-HEIGHT WINDOW)))
                          (FORMAT WINDOW
                                  "~& This font's characters are too big to display here!")
                          (RETURN NIL)))
                   (AND FROM-FED (FORMAT WINDOW "~&Mouse any character to edit it.~%"))
                   (DO ((CH 0) (OCH) (LEN (ARRAY-LENGTH FD))) (())
                     ;; Skip any groups of 32 characters that are all missing.
                     (AND (ZEROP (\ CH 32.))
                          (DO ((CH1 CH (1+ CH1)))
                              (( CH1 LEN)
                               (SETQ CH CH1))
                            (IF (ZEROP (\ CH1 32.))
                                (SETQ CH CH1))
                            (IF (OR (AREF FD CH1)
                                    (and from-fed (FED-CHARACTER-BEING-EDITED-P NAME CH1)))
                                (RETURN))))
                     (WHEN ( CH LEN) (RETURN NIL))
                     ;; If there is not room for a line in the default font
                     ;;   followed by a line in the font being edited
                     ;;   before we would need to **more**,
                     ;;   then **more** right now, and go to top of window afterward.
                     (COND (( (+ (TV:SHEET-CURSOR-Y WINDOW)
                                  (FONT-CHAR-HEIGHT DF)
                                  (FONT-CHAR-HEIGHT DF)
                                  1
                                  (FONT-CHAR-HEIGHT FONT))
                               (TV:SHEET-MORE-VPOS WINDOW))
                            (SEND WINDOW :SET-CURSORPOS
                                  (TV:SHEET-INSIDE-LEFT WINDOW)
                                  (MIN (- (TV:SHEET-INSIDE-BOTTOM WINDOW)
                                          (TV:SHEET-LINE-HEIGHT WINDOW))    ;I wonder if this explosion-proofing is necessary.
                                       (- (+ (TV:SHEET-CURSOR-Y WINDOW)
                                             (TV:SHEET-LINE-HEIGHT WINDOW))
                                          (FONT-CHAR-HEIGHT DF))))
                            (SETF (TV:SHEET-MORE-FLAG WINDOW) 1)
                            (SEND WINDOW :HANDLE-EXCEPTIONS)
                            (SETF (TV:SHEET-END-PAGE-FLAG WINDOW) 1)
                            (SEND WINDOW :HANDLE-EXCEPTIONS)))
                     (SEND WINDOW :SET-CURSORPOS (TV:SHEET-INSIDE-LEFT WINDOW)
                                                  (+ (TV:SHEET-CURSOR-Y WINDOW)
                                                     (FONT-CHAR-HEIGHT DF)))    ;move down one df-height.
                     (TV:PREPARE-SHEET (WINDOW)
                       ;; Clear out what we will move down over.
                       (TV:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH WINDOW)
                                           (+ (FONT-CHAR-HEIGHT DF)
                                              3
                                              (FONT-CHAR-HEIGHT FONT)
                                              (FONT-CHAR-HEIGHT DF))
                                           (TV:SHEET-INSIDE-LEFT WINDOW)
                                           (+ (TV:SHEET-CURSOR-Y WINDOW)
                                              (- (TV:SHEET-BASELINE WINDOW)
                                                 (FONT-BASELINE DF)))
                                           TV:ALU-ANDCA
                                           WINDOW)
                       (SETQ OCH CH)
                       ;; Output one line of chars in the default font,
                       ;;  spaced so that they lie above the corresponding chars in the next
                       ;;  line. Stop at margin, or when we reach a char code that's a
                       ;;   multiple of 32.
                       (DO ()
                           ((> (+ (TV:SHEET-CURSOR-X WINDOW)
                                  (MAX (FED-CHAR-DISPLAY-WIDTH FD CH)
                                       (FONT-CHARACTER-WIDTH DF CH)))
                               (TV:SHEET-INSIDE-RIGHT WINDOW)))
                         (SEND WINDOW :SET-CURRENT-FONT DF)
                         (WHEN (OR (AREF FD CH)
                                   (AND FROM-FED (FED-CHARACTER-BEING-EDITED-P NAME CH)))
                           (WHEN FROM-FED
                             (SEND WINDOW :PRIMITIVE-ITEM
                                   'CHARACTER CH
                                   (- (TV:SHEET-CURSOR-X WINDOW)
                                      (TV:SHEET-INSIDE-LEFT WINDOW))
                                   (- (TV:SHEET-CURSOR-Y WINDOW)
                                      (TV:SHEET-INSIDE-TOP WINDOW)
                                      (- (FONT-BASELINE DF) (TV:SHEET-BASELINE WINDOW)))
                                   (- (+ (TV:SHEET-CURSOR-X WINDOW)
                                         (MAX (FED-CHAR-DISPLAY-WIDTH FD CH)
                                              (FONT-CHARACTER-WIDTH DF CH)))
                                      (TV:SHEET-INSIDE-LEFT WINDOW))
                                   (+ (- (TV:SHEET-CURSOR-Y WINDOW)
                                         (TV:SHEET-INSIDE-TOP WINDOW)
                                         (- (FONT-BASELINE DF) (TV:SHEET-BASELINE WINDOW)))
                                      (FONT-CHAR-HEIGHT DF)
                                      (FONT-CHAR-HEIGHT FONT)
                                      4)))      ;2 would be ok & symmetrical but the mouse gets in the way visually.
                           (TV:SHEET-TYO WINDOW CH)
                           (SEND WINDOW :INCREMENT-CURSORPOS
                                 (- (FED-DISPLAY-FONT-CHAR-WIDTH FD DF CH)
                                    (FONT-CHARACTER-WIDTH DF CH))
                                 0))
                         (SETQ CH (1+ CH))
                         (AND (= CH LEN) (RETURN))
                         (AND (ZEROP (\ CH 32.)) (RETURN)))
                       (SEND WINDOW :SET-CURSORPOS (TV:SHEET-INSIDE-LEFT WINDOW)
                                                    (+ (TV:SHEET-CURSOR-Y WINDOW)
                                                       (- (FONT-CHAR-HEIGHT DF)
                                                          (FONT-BASELINE DF))
                                                       (FONT-BASELINE FONT)
                                                       1))
                       ;; Now output the corresponding chars in the font being edited.
                       (SEND WINDOW :SET-CURRENT-FONT FONT)
                       (DO ()
                           ((> (+ (TV:SHEET-CURSOR-X WINDOW)
                                  (FED-DISPLAY-FONT-CHAR-WIDTH FD DF OCH))
                               (TV:SHEET-INSIDE-RIGHT WINDOW)))
                         (COND ((OR (AREF FD OCH)
                                    (and from-fed (FED-CHARACTER-BEING-EDITED-P NAME OCH)))
                                (if from-fed
                                    (FED-TYO WINDOW OCH NAME)
                                  (TV:SHEET-TYO WINDOW OCH))
                                (TV:SHEET-INCREMENT-BITPOS WINDOW
                                  (- (FED-DISPLAY-FONT-CHAR-WIDTH FD DF OCH)
                                     (FONT-CHARACTER-WIDTH FONT OCH))
                                  0)))
                         (SETQ OCH (1+ OCH))
                         (AND (= OCH LEN) (RETURN))
                         (AND (ZEROP (\ OCH 32.)) (RETURN))))
                     (SEND WINDOW :SET-CURSORPOS (TV:SHEET-INSIDE-LEFT WINDOW)
                                                  (+ (TV:SHEET-CURSOR-Y WINDOW)
                                                     (FONT-CHAR-HEIGHT DF))))
                   (SEND WINDOW :SET-CURSORPOS
                         (TV:SHEET-INSIDE-LEFT WINDOW)
                         (MIN (- (TV:SHEET-INSIDE-BOTTOM WINDOW)
                                 (TV:SHEET-LINE-HEIGHT WINDOW)) ;I wonder if this explosion-proofing is necessary.
                              (- (+ (TV:SHEET-CURSOR-Y WINDOW)
                                    (TV:SHEET-LINE-HEIGHT WINDOW))
                                 (FONT-CHAR-HEIGHT DF)))))
          (SEND WINDOW :SET-FONT-MAP FONT-MAP)
          (SEND WINDOW :SET-CURRENT-FONT CURRENT-FONT)))
    (FORMAT WINDOW "~&~S is not a font." FONT))
  (VALUES))
