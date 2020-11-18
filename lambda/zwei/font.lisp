;;; Font hacking function and commands -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFUN CHANGE-FONT-INTERVAL (START-BP &OPTIONAL END-BP IN-ORDER-P (FONT-NUM *FONT*))
  "Set the font number of all characters in the specified interval to FONT-NUM."
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (WITH-UNDO-SAVE-IF (NOT *BATCH-UNDO-SAVE*) ("Font change" START-BP END-BP T)
    (MUNG-BP-INTERVAL START-BP)
    (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
         (LIMIT-LINE (BP-LINE END-BP))
         (START-INDEX (BP-INDEX START-BP) 0)
         (LAST-LINE-P))
        (NIL)
      (SETQ LAST-LINE-P (EQ LINE LIMIT-LINE))
      (OR (ZEROP FONT-NUM) (EQ (ARRAY-TYPE LINE) 'ART-FAT-STRING)
          (SETQ LINE (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING)))
      (MUNG-LINE LINE)
      (DO ((INDEX START-INDEX (1+ INDEX))
           (LIMIT-INDEX (IF LAST-LINE-P (BP-INDEX END-BP) (LINE-LENGTH LINE))))
          (( INDEX LIMIT-INDEX))
        (ASET (DPB FONT-NUM %%CH-FONT (AREF LINE INDEX)) LINE INDEX))
      (AND LAST-LINE-P (RETURN NIL))))
  DIS-TEXT)                                     ;For the sake of commands

(DEFUN CHANGE-ONE-FONT-INTERVAL (OFONT NFONT FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "For each character in specified interval, if it's in font OFONT, change to NFONT.
Both OFONT and NFONT are font numbers."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (WITH-UNDO-SAVE ("Font change" FROM-BP TO-BP T)
    (MUNG-BP-INTERVAL FROM-BP)
    (DO ((LINE (BP-LINE FROM-BP) (LINE-NEXT LINE))
         (LIMIT-LINE (BP-LINE TO-BP))
         (START-INDEX (BP-INDEX FROM-BP) 0)
         (LAST-LINE-P))
        (NIL)
      (SETQ LAST-LINE-P (EQ LINE LIMIT-LINE))
      (OR (ZEROP NFONT) (EQ (ARRAY-TYPE LINE) 'ART-FAT-STRING)
          (SETQ LINE (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING)))
      (MUNG-LINE LINE)
      (DO ((INDEX START-INDEX (1+ INDEX))
           (LIMIT-INDEX (IF LAST-LINE-P (BP-INDEX TO-BP) (LINE-LENGTH LINE))))
          (( INDEX LIMIT-INDEX))
        (WHEN (= (LDB %%CH-FONT (AREF LINE INDEX)) OFONT)
          (ASET (DPB NFONT %%CH-FONT (AREF LINE INDEX)) LINE INDEX)))
      (AND LAST-LINE-P (RETURN NIL)))))

(DEFUN MAP-FONTS-INTERVAL (PERMUTATION FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Permute fonts, for all characters in the specified interval.
Each char's font is used as an index in PERMUTATION (a sequence)
to get the new font number for that character."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (WITH-UNDO-SAVE-IF (NOT *BATCH-UNDO-SAVE*) ("Font change" FROM-BP TO-BP T)
    (MUNG-BP-INTERVAL FROM-BP)
    (DO ((LINE (BP-LINE FROM-BP) (LINE-NEXT LINE))
         (LIMIT-LINE (BP-LINE TO-BP))
         (START-INDEX (BP-INDEX FROM-BP) 0)
         (LAST-LINE-P))
        (NIL)
      (SETQ LAST-LINE-P (EQ LINE LIMIT-LINE))
      (OR (EQ (ARRAY-TYPE LINE) 'ART-FAT-STRING)
          (SETQ LINE (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING)))
      (MUNG-LINE LINE)
      (DO ((INDEX START-INDEX (1+ INDEX))
           (LIMIT-INDEX (IF LAST-LINE-P (BP-INDEX TO-BP) (LINE-LENGTH LINE))))
          (( INDEX LIMIT-INDEX))
        (ASET (DPB (ELT PERMUTATION (LDB %%CH-FONT (AREF LINE INDEX)))
                   %%CH-FONT (AREF LINE INDEX))
              LINE INDEX))
      (AND LAST-LINE-P (RETURN NIL)))))

;;;used below to protect against flavors of interval (such as node)
;;;that don't deal with fonts. (mrc)
(defun safe-get-font-alist (interval)
  (send interval :eval-inside-yourself
        '(when (boundp 'zwei:saved-font-alist) zwei:saved-font-alist)))

(DEFUN FIXUP-FONTS-INTERVAL (FONTS FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Permute the fonts in soe inserted text, to show up properly in this interval.
FROM-BP and TO-BP should specify a subinterval of *INTERVAL*.
The text in that subinterval is assumed to have been copied out
of another interval (or a file) in which FONTS was the font list.
Update *INTERVAL*'s font list, and the font numbers of the characters,
so that each character appears in the same font as it did where it came from."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (IF (NOT (AND FONTS (safe-get-font-alist *interval*)))
      (CHANGE-FONT-INTERVAL FROM-BP TO-BP T *FONT*)
    (ADD-FONTS-TO-BUFFER FONTS)
    (LET ((PERMUTATION (MAKE-ARRAY (LENGTH FONTS))))
      (DOTIMES (I (LENGTH FONTS))
        (DO ((J 0 (1+ J))
             (LIST (WINDOW-FONT-ALIST *WINDOW*) (CDR LIST)))
            ((EQUAL (CAAR LIST) (SYMBOL-NAME (NTH I FONTS)))
             (SETF (AREF PERMUTATION I) J))))
      (MAP-FONTS-INTERVAL PERMUTATION FROM-BP TO-BP T))))

(DEFVAR *SAVE-FONT-NUM* 0)

(DEFUN INPUT-FONT-NAME (USE-PREVIOUS-P &OPTIONAL (PROMPT "Font ID: ") &AUX NUM)
  "Read a font number from the user.
The user can type a letter (A signifies font 0), an  and a font name,
or mouse left on a character, or mouse right and get a menu of fonts.
USE-PREVIOUS-P means if previous command called this function,
 just return the same number we returned for that command."
  (SETQ *CURRENT-COMMAND-TYPE* 'FONT-CHANGE)
  (IF (AND USE-PREVIOUS-P (EQ *LAST-COMMAND-TYPE* 'FONT-CHANGE))
      *SAVE-FONT-NUM*
    (FORMAT *QUERY-IO* PROMPT)
    (DO (CH ;(IF (AND *NUMERIC-ARG-P* ( 1 *NUMERIC-ARG* 26.)) (+ *NUMERIC-ARG* #/A -1))
         (ALIST (WINDOW-FONT-ALIST *WINDOW*)))
        (NIL)
      (LET-GLOBALLY ((*GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
                       (IF (NULL ALIST)
                           "Use font of character pointed at."
                         "Use font of character pointed at, click right for menu of fonts.")))
        (UNLESS CH
          (TYPEIN-LINE-ACTIVATE
            (SETQ CH (CHAR-UPCASE (send *STANDARD-INPUT* :MOUSE-OR-KBD-TYI))))))
      (COND ((CHAR= CH #/CONTROL-G)
             (BARF))
            ((CHAR= CH #/ALTMODE)
             (SETQ NUM (INPUT-FONT-NAME-FROM-MINI-BUFFER))
             (RETURN NIL))
            ((CHAR= CH #/MOUSE-1-1)
             (COND ((SETQ CH (MOUSE-CHAR *WINDOW*))
                    (SETQ NUM (LDB %%CH-FONT CH))
                    (RETURN NIL))))
            ((CHAR= CH #/MOUSE-3-1)
             (COND ((NULL ALIST)
                    (BEEP)
                    (setq ch nil))
                   ((SETQ CH (TV:MENU-CHOOSE
                               (mapcar #'(lambda (font-elt)
                                           (let ((font (cdr font-elt)))
                                             (list (car font-elt) :value font :font font)))
                                       ALIST)
                               "Buffer Fonts"))
                    (DO ((I 0 (1+ I))           ;Have the font itself, but want the number
                         (L (WINDOW-FONT-ALIST *WINDOW*) (CDR L)))
                        ((EQ (CDAR L) CH) (SETQ NUM I)))
                    (RETURN NIL))))
            ((CHAR #/A CH #/Z)
             (SETQ NUM (- (CHAR-CODE CH) (CHAR-CODE #/A)))
             (RETURN NIL))
            ((OR (CHAR= CH #/Help) (CHAR= CH #/?))
             (FORMAT *QUERY-IO* "~&Type a font letter, ~
                                  or altmode to enter a new font in a mini-buffer, ~@
                                  or mouse a character left for its font~:[~;, ~
                                  or mouse-right for a menu~].~%" ALIST)
             (FORMAT *QUERY-IO* PROMPT)
             (SETQ CH NIL))
            (T
             (BEEP) (SETQ CH NIL))))
    (FORMAT *QUERY-IO* "~C (~A)"
            (MAKE-CHAR (+ NUM (CHAR-CODE #/A))) (CAR (NTH NUM (WINDOW-FONT-ALIST *WINDOW*))))
    (SETQ *SAVE-FONT-NUM* NUM)))

(DEFUN INPUT-FONT-NAME-FROM-MINI-BUFFER (&AUX FONT NEW-P)
  (SETQ FONT (COMPLETING-READ-FROM-MINI-BUFFER "Font ID:" (WINDOW-FONT-ALIST *WINDOW*) T))
  (COND ((EQUAL FONT "")
         *FONT*)
        ((STRINGP FONT)
         (SETQ NEW-P T)                         ;Wasn't previously in the A-list, add it
         (ADD-FONTS-TO-BUFFER (LIST FONT))))
  (SETQ FONT (POSITION FONT (WINDOW-FONT-ALIST *WINDOW*) :TEST 'STRING-EQUAL :KEY 'CAR))
  (FORMAT *QUERY-IO* (IF NEW-P "Added as font " "Font "))
  ;; Caller will append letter & name of font
  FONT)

(DEFUN ADD-FONTS-TO-BUFFER (FONTS)
  "Make sure that all the fonts in the list FONTS are available in the current buffer."
  (PKG-BIND 'FONTS
    (SETQ FONTS
          (LOOP FOR FONT IN FONTS
                COLLECT
                (FONT-NAME (SEND (TV:SHEET-GET-SCREEN (WINDOW-SHEET *WINDOW*))
                                 :PARSE-FONT-DESCRIPTOR
                                 (READ-FROM-STRING (STRING FONT)))))))
  (LET ((OLD-LIST (WINDOW-FONT-ALIST *WINDOW*)))
    (AND ( (LENGTH OLD-LIST) 26.) (BARF "The maximum number of fonts is 26."))
    (OR OLD-LIST (SETQ OLD-LIST (LET ((FONT0 (CURRENT-FONT *WINDOW* 0)))
                                  (LIST (CONS (SYMBOL-NAME (FONT-NAME FONT0)) FONT0)))))
    (SETQ FONTS (SUBSET-NOT #'(LAMBDA (FONT) (ASSOC (SYMBOL-NAME FONT) OLD-LIST)) FONTS))
    (WHEN FONTS
      (FORMAT *QUERY-IO* "~&Fonts added to buffer's font list.~%")
      (SEND *INTERVAL* :SET-ATTRIBUTE :FONTS
                                      (NCONC (MAPCAR (LAMBDA (AELT)
                                                       (FONT-NAME (CDR AELT))) OLD-LIST)
                                             FONTS)
                       :QUERY)
      (REDEFINE-FONTS *WINDOW* (APPEND OLD-LIST
                                       (MAPCAR (LAMBDA (FONT)
                                                 (CONS (SYMBOL-NAME FONT)
                                                       (SYMBOL-VALUE FONT)))
                                               FONTS))))
    FONTS))

(DEFUN REDEFINE-FONTS (WINDOW FONT-ALIST &OPTIONAL VSP)
  "Set the font alist of WINDOW (a window defstruct) to FONT-ALIST.
FONT-ALIST is actually a list of fonts, it seems."
  (COND ((NOT (EQUAL FONT-ALIST (WINDOW-FONT-ALIST WINDOW)))
         (LET ((SHEET (WINDOW-SHEET WINDOW) ))
           (SEND SHEET :SET-FONT-MAP (MAPCAR #'CDR FONT-ALIST))
           (SEND SHEET :SET-CURRENT-FONT (AREF (TV:SHEET-FONT-MAP SHEET) 0)))
         (MUST-REDISPLAY WINDOW DIS-ALL)
         (SETF (WINDOW-FONT-ALIST WINDOW) FONT-ALIST)
         (WHEN (EQ WINDOW *WINDOW*)
           (IF (NULL FONT-ALIST)
               (SETQ *FONT* 0)
             (IF (>= *FONT* (LENGTH FONT-ALIST))
                 (SETQ *FONT* 0)))
           (UPDATE-FONT-NAME))))
  (SEND (WINDOW-INTERVAL WINDOW) :SET-SAVED-FONT-ALIST FONT-ALIST)
  (COND ((AND VSP ( VSP (SEND (WINDOW-SHEET WINDOW) :VSP)))
         (MUST-REDISPLAY WINDOW DIS-ALL)
         (SEND (WINDOW-SHEET WINDOW) :SET-VSP VSP))))

(DEFUN UPDATE-FONT-NAME ()
  (SETQ *FONT-NAME* (AND (WINDOW-FONT-ALIST *WINDOW*)
                         (FORMAT NIL "~C (~A)" (+ *FONT* #/A)
                                 (CAR (NTH *FONT* (WINDOW-FONT-ALIST *WINDOW*)))))))

;;; This is used by ZMACS
(DEFUN SET-BUFFER-FONTS (BUFFER &OPTIONAL (FONTS (SEND BUFFER :GET-ATTRIBUTE ':FONTS)))
  "Set the fonts of BUFFER according to its attribute list.
We assume that the buffer's attribute list has been read in and stored."
  (COND ((AND FONTS (SYMBOLP FONTS))
         (setf (get buffer :fonts) (list fonts)) ; assure it's a list
         (SETQ FONTS (INTERN (SYMBOL-NAME FONTS) 'FONTS))
         (SETQ FONTS (AND (BOUNDP FONTS) (LIST (CONS (SYMBOL-NAME FONTS) (SYMBOL-VALUE FONTS))))))
        (T
         (DO ((FL FONTS (CDR FL))
              (L NIL)
              (F))
             ((NULL FL)
              (SETQ FONTS (NREVERSE L)))
             (SETQ F (INTERN (SYMBOL-NAME (CAR FL)) 'FONTS))
             (COND ((NOT (BOUNDP F))
                    (LOAD (FORMAT NIL "SYS: FONTS; ~A" F) :PACKAGE 'FONTS
                          :IF-DOES-NOT-EXIST NIL :SET-DEFAULT-PATHNAME NIL)
                    (OR (BOUNDP F)
                        ;;If font not loaded, substitute default to keep font numbers ok
                        (SETQ F (FONT-NAME TV:(SCREEN-DEFAULT-FONT DEFAULT-SCREEN))))))
             (PUSH (CONS (SYMBOL-NAME F) (SYMBOL-VALUE F)) L))))
  (SEND BUFFER :SET-SAVED-FONT-ALIST FONTS)
  FONTS)

(DEFCOM COM-CHANGE-FONT-CHAR "Change the font of one or more characters forward.
Reads in the echo area a letter signifying the new font
/(A for the first font, etc.).
Or type Altmode and a font name, click mouse left on a char in the buffer
to use its font, or mouse right and get a menu of fonts.
If the previous command was a font-change command,
the same font is used, without reading any argument." ()
  (LET ((BP1 (FORWARD-CHAR (POINT) *NUMERIC-ARG* T)))
    (CHANGE-FONT-INTERVAL (POINT) BP1 NIL (INPUT-FONT-NAME
                                            T
                                            "Change font of characters to (Font ID): "))
    (IF (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1))
    DIS-TEXT))

(DEFCOM COM-CHANGE-FONT-WORD "Change the font of one or more words forward.
Reads in the echo area a letter signifying the new font
/(A for the first font, etc.).
Or type Altmode and a font name, click mouse left on a char in the buffer
to use its font, or mouse right and get a menu of fonts.
If the previous command was a font-change command,
the same font is used, without reading any argument." ()
  (LET ((BP1 (FORWARD-WORD (POINT) *NUMERIC-ARG* T)))
    (CHANGE-FONT-INTERVAL (POINT) BP1 NIL (INPUT-FONT-NAME
                                            T
                                            "Change font of words to (Font ID): "))
    (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1))
    DIS-TEXT))

(DEFCOM COM-CHANGE-FONT-REGION "Change the font between point and the mark.
Reads in the echo area a letter signifying the new font
/(A for the first font, etc.).
Or type Altmode and a font name, click mouse left on a char in the buffer
to use its font, or mouse right and get a menu of fonts." ()
  (REGION (BP1 BP2)
      (CHANGE-FONT-INTERVAL BP1 BP2 T (INPUT-FONT-NAME
                                        NIL
                                        "Change font of region to (Font ID): "))))

(DEFCOM COM-CHANGE-ONE-FONT-REGION "Change all characters of one font to another font.
Asks you to specify two fonts, then changes each character in the region
which is currently in the first font to be in the second font instead." ()
  (LET* ((OFONT (INPUT-FONT-NAME NIL "Change font of characters in (Font ID): "))
         (NFONT (INPUT-FONT-NAME NIL " to (Font ID): ")))
    (REGION (BP1 BP2)
      (CHANGE-ONE-FONT-INTERVAL OFONT NFONT BP1 BP2))
    DIS-TEXT))

(DEFCOM COM-CHANGE-DEFAULT-FONT "Set the default font.
Reads in the echo area a letter signifying the new font
/(A for the first font, etc.).
Or type Altmode and a font name, click mouse left on a char in the buffer
to use its font, or mouse right and get a menu of fonts." ()
  (SETQ *FONT* (INPUT-FONT-NAME NIL "Set default font to (Font ID): "))
  (UPDATE-FONT-NAME)
  DIS-BPS)                                      ;This may change the size of the blinker

(DEFCOM COM-SET-FONTS "Change the set of fonts to use.
This sets the fonts associated with the current buffer.
Reads a list of font names, separated by spaces, from the mini-buffer." ()
  (LET ((TEM (DO ((FL (OR (WINDOW-FONT-ALIST *WINDOW*)
                          `((,(TV:FONT-NAME (AREF (SEND (WINDOW-SHEET *WINDOW*) :FONT-MAP) 0)))))
                      (CDR FL))
                  (STR (MAKE-STRING #o100 :FILL-POINTER 0))
                  (FIL "" " "))
                 ((NULL FL) STR)
               (SETQ STR (STRING-NCONC STR FIL (CAAR FL)))))
        FONTS)
    (LET ((*MINI-BUFFER-DEFAULT-STRING* TEM))
      (DO ((I 0)
           J
           (STRING (STRING-TRIM *BLANKS* (TYPEIN-LINE-READLINE "font1 font2 ...:"))))
          ((OR (NULL I)
               (EQUAL STRING "")))
        (SETQ J (STRING-SEARCH-SET '(#/SP #/, #/TAB) STRING I))
        (PUSH (INTERN (STRING-UPCASE (NSUBSTRING STRING I J)) 'FONTS)
              FONTS)
        (SETQ I (AND J (STRING-SEARCH-NOT-SET '(#/SP #/, #/TAB) STRING (1+ J))))))
    ;; Now FONTS is in the reverse order.
    (DO ((L FONTS (CDR L))
         (FONT)
         (FONT-NAMES)
         (AL NIL))
        ((NULL L)
         ;; FONT-NAME and AL now in forward order.
         (SEND *INTERVAL* :SET-ATTRIBUTE ':FONTS FONT-NAMES :QUERY)
         (REDEFINE-FONTS *WINDOW* AL))
      (SETQ FONT (CAR L))
      (COND ((NOT (SYMBOLP FONT))
             (BARF "~S is not the name of a font" FONT))
            ((NOT (BOUNDP FONT))
             (LOAD (FORMAT NIL "SYS: FONTS; ~A" FONT)
                   :PACKAGE 'FONTS
                   :SET-DEFAULT-PATHNAME NIL
                   :IF-DOES-NOT-EXIST NIL)
             (OR (BOUNDP FONT) (BARF "~S is not a defined font" FONT))))
      (PUSH FONT FONT-NAMES)
      (PUSH (CONS (SYMBOL-NAME FONT) (SYMBOL-VALUE FONT)) AL))
    (UPDATE-FONT-NAME))
  DIS-ALL)

(DEFCOM COM-LIST-FONTS "List the loaded fonts.
With an argument, also lists the font files on the file computer." ()
  (FORMAT T "Loaded fonts: (Mouse a font name to see a sample) ~%")
  (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FONT
           (LET ((LIST NIL))
             (DO-LOCAL-SYMBOLS (X 'FONTS)
               (WHEN (AND (BOUNDP X)
                          (TYPEP (SYMBOL-VALUE X) 'TV:FONT))
                 (PUSH X LIST)))
             (SETQ LIST (SORT LIST #'STRING-LESSP))))
  (WHEN *NUMERIC-ARG-P*
    (FORMAT T "~&Plus fonts on the file computer:~%")
    (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FONT
          (LOOP FOR FILE IN (FS:DIRECTORY-LIST (FS:MAKE-PATHNAME :HOST "SYS"
                                                                 :DIRECTORY '("FONTS")
                                                                 :NAME :WILD
                                                                 :TYPE :QFASL
                                                                 :VERSION :NEWEST)
                                               :FAST)
                WHEN (CAR FILE)
                COLLECT (INTERN (STRING-UPCASE (SEND (CAR FILE) :NAME)) 'FONTS))))
  DIS-NONE)

(DEFCOM COM-DISPLAY-FONT "Display all the characters in a font." ()
  (LET ((FONT (COMPLETING-READ-FROM-MINI-BUFFER "Font to display:"
                        (LET ((LIST NIL))
                          (DO-LOCAL-SYMBOLS (X (FIND-PACKAGE 'FONTS))
                            (WHEN (AND (BOUNDP X)
                                       (TYPEP (SYMBOL-VALUE X) 'FONT))
                              (PUSH (CONS (STRING (FONT-NAME (SYMBOL-VALUE X))) X) LIST)))
                          (SORTCAR LIST #'STRING-LESSP))
                        T)))
    (SETQ FONT (IF (STRINGP FONT) (INTERN (STRING-UPCASE FONT) 'FONTS) (CDR FONT)))
    (DISPLAY-FONT FONT))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FONT "Display" DISPLAY-FONT T
                          "Display this font.")

(DEFUN DISPLAY-FONT (FONT-SYMBOL &AUX FONT)
  (UNLESS (TYPEP *WINDOW* 'WINDOW)
    (FERROR NIL "DISPLAY-FONT called on a non-sheet window."))
  (SETQ FONT (SEND (TV:SHEET-GET-SCREEN *WINDOW*) :PARSE-FONT-DESCRIPTOR FONT-SYMBOL))
  (FED:DISPLAY-FONT FONT *TERMINAL-IO* NIL)
  (SEND *STANDARD-OUTPUT* :CLEAR-REST-OF-WINDOW)
  (VALUES))


;;; Diagram stuff
(DEFFLAVOR LINE-DIAGRAM-MIXIN () ()
  (:REQUIRED-METHODS :DRAW))

(DEFMETHOD (LINE-DIAGRAM-MIXIN :ADD-LINE) (LINE &OPTIONAL CONTENTS)
  LINE CONTENTS
  )

(DEFMETHOD (LINE-DIAGRAM-MIXIN :STRING-FOR-FILE) (LINE)
  LINE
  "")

(DEFMETHOD (LINE-DIAGRAM-MIXIN :NUMBER-OF-LINES) ()
  1)

(DEFUN INSERT-DIAGRAM (BP DIAGRAM &REST OPTIONS)
  (DO ((I 0 (1+ I))
       (AT-LINE (BP-LINE BP))
       (NUMBER-OF-LINES (SEND DIAGRAM :NUMBER-OF-LINES))
       (LINE))
      (( I NUMBER-OF-LINES) DIAGRAM)
    (MULTIPLE-VALUE (DIAGRAM LINE)
      (APPLY 'MAKE-DIAGRAM-LINE DIAGRAM OPTIONS))
    (INSERT-LINE-WITH-LEADER LINE AT-LINE)))

(DEFUN MAKE-DIAGRAM-LINE (DIAGRAM &REST OPTIONS &AUX LINE)
  (AND (SYMBOLP DIAGRAM)
       (SETQ DIAGRAM (APPLY 'MAKE-INSTANCE DIAGRAM OPTIONS)))
  (SETQ LINE (MAKE-LINE :MAKE-ARRAY (:AREA *ZWEI-LINE-AREA* :TYPE ART-STRING)
                        LINE-TICK *TICK* LINE-LENGTH 0))
  (SETF (GETF (LINE-PLIST LINE) ':DIAGRAM) DIAGRAM)
  (SEND DIAGRAM :ADD-LINE LINE)
  (VALUES LINE DIAGRAM))

(DEFFLAVOR BLACK-BLOCK-DIAGRAM () (LINE-DIAGRAM-MIXIN))

(DEFMETHOD (BLACK-BLOCK-DIAGRAM :DRAW) (IGNORE SHEET)
  (TV:PREPARE-SHEET (SHEET)
    (SYS:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH SHEET) (TV:SHEET-LINE-HEIGHT SHEET)
                         (TV:SHEET-INSIDE-LEFT SHEET) (TV:SHEET-CURSOR-Y SHEET)
                         (TV:SHEET-CHAR-ALUF SHEET) SHEET)))

(DEFVAR *BLACK-BLOCK-DIAGRAM* 'BLACK-BLOCK-DIAGRAM)

(DEFUN INSERT-BLACK-BLOCK-DIAGRAM (BP)
  (SETQ *BLACK-BLOCK-DIAGRAM* (INSERT-DIAGRAM BP 'BLACK-BLOCK-DIAGRAM)))

(DEFFLAVOR GREY-BLOCK-DIAGRAM () (LINE-DIAGRAM-MIXIN))

(DEFMETHOD (GREY-BLOCK-DIAGRAM :DRAW) (IGNORE SHEET)
  (TV:PREPARE-SHEET (SHEET)
    (SYS:BITBLT TV:ALU-SETA (TV:SHEET-INSIDE-WIDTH SHEET) (TV:SHEET-LINE-HEIGHT SHEET)
                TV:50%-GRAY 0 0
                (TV:SHEET-SCREEN-ARRAY SHEET)
                (TV:SHEET-INSIDE-LEFT SHEET) (TV:SHEET-CURSOR-Y SHEET))))

(DEFVAR *GREY-BLOCK-DIAGRAM* 'GREY-BLOCK-DIAGRAM)

(DEFUN INSERT-GREY-BLOCK-DIAGRAM (BP)
  (SETQ *GREY-BLOCK-DIAGRAM* (INSERT-DIAGRAM BP 'GREY-BLOCK-DIAGRAM)))

(DEFFLAVOR RESTORABLE-LINE-DIAGRAM-MIXIN () ()
  (:INCLUDED-FLAVORS LINE-DIAGRAM-MIXIN)
  (:INIT-KEYWORDS :NUMBER-OF-LINES)
  (:DEFAULT-INIT-PLIST :NUMBER-OF-LINES 1)
  (:REQUIRED-METHODS :CONTENTS))

(DEFMETHOD (RESTORABLE-LINE-DIAGRAM-MIXIN :STRING-FOR-FILE) (LINE)
  (IF (SEND SELF :FIRST-LINE-P LINE)
      (FORMAT NIL "#~D ~A~%~A" (SEND SELF :NUMBER-OF-LINES) (TYPE-OF SELF)
              (SEND SELF :CONTENTS LINE))
    (SEND SELF :CONTENTS LINE)))
