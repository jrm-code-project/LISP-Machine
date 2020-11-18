;;; -*- Mode:Lisp; Package:Press; Base:8 -*-
;;; PRESS File and DOVER software
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(DEFVAR PRESS-SPECIAL-VARIABLES NIL)

(DEFVAR DOVER-FONT-EQUIVALENCES
  'FONTS:((CPTFONT . LPT8) (CPTFONTB . LPT8B)
          (HL6 . Helvetica6) (HL7 . Helvetica7)
          (HL10 . Helvetica8) (HL10B . Helvetica8B)
          (HL10I . Helvetica8I) (HL10BI . Helvetica8BI)
          (HL12 . Helvetica10) (HL12B . Helvetica10B)
          (HL12I . Helvetica10I) (HL12BI . Helvetica10BI)
          (TR8 . TimesRoman7) (TR8B . TimesRoman7B)
          (TR8I . TimesRoman7I) (TR8BI . TimesRoman7BI)
          (TR10 . TimesRoman8) (TR10B . TimesRoman8B)
          (TR10I . TimesRoman8I) (TR10BI . TimesRoman8BI)
          (TR10IB . TimesRoman8BI)
          (TR12 . TimesRoman10) (TR12B . TimesRoman10B)
          (TR12I . TimesRoman10I) (TR12BI . TimesRoman10BI)
          (TR12B1 . TimesRoman10B)
          (MEDFNT . Helvetica12) (MEDFNB . Helvetica12B)
          (METS . Helvetica18) (METSI . Helvetica18I)
          (BIGFNT . Helvetica14B) (5x5 . Helvetica4) (TINY . Helvetica5)
          (TVFONT . LPT8) (43VXMS . OldEnglish48)
          )
  "A-list of similar Lisp Machine and Dover fonts")

(DEFMACRO DEFINE-PRESS-VARIABLE (NAME &OPTIONAL FORM (DEFVAR-P T))
  `(SI:DEFINE-SPECIAL-VARIABLE ,NAME ,FORM PRESS-SPECIAL-VARIABLES ,DEFVAR-P))

(DEFMACRO BIND-PRESS-VARIABLES (&BODY BODY)
  `(PROGW PRESS-SPECIAL-VARIABLES
     (UNWIND-PROTECT
       (PROGN . ,BODY)
       (DEALLOCATE-RESOURCE 'PRESS-PAGE-ENTITY-BUFFER PRESS-PAGE-ENTITY-BUFFER))))

(DEFINE-PRESS-VARIABLE PRESS-USE-EFTP NIL)      ;T => EFTP, NIL => Chaos
(DEFVAR DOVER-ADDRESS #o1002)                   ;2#2#

(DEFUN PRINT-DOVER-STATUS (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Connect to the dover, read its status, and print it."
  (LET ((CONN (CHAOS:OPEN-FOREIGN-CONNECTION DOVER-ADDRESS #o21)))
    (UNWIND-PROTECT
      (DO ((N-RETRIES #+REL5 30. #-REL5 10. (1- N-RETRIES))
           (PUP))
          ((ZEROP N-RETRIES)
           (FORMAT STREAM "~&Dover is not responding (may be printing).~%"))
        (CHAOS:TRANSMIT-PUP CONN (CHAOS:GET-PUP CONN #o200 0) 0)
        (COND ((SETQ PUP (CHAOS:RECEIVE-PUP CONN))
               (COND ((= (CHAOS:PUP-TYPE PUP) 201)
                      (FORMAT STREAM
                              "~&Dover status: ~[~;Spooler shut off.~;Spooler available.~
                                 ~;Spooler busy.~]  ~A~%"
                              (CHAOS:PUP-WORD PUP 0)
                              (CHAOS:PUP-STRING PUP 2))
                      (CHAOS:RETURN-PKT PUP)
                      (RETURN T))
                     (T (CHAOS:RECEIVED-RANDOM-PUP PUP))))))
      (CHAOS:REMOVE-CONN CONN))))

(DEFUN PRINT-DOVER-QUEUE (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the queue of the dover spooler on MC."
  (WITH-OPEN-STREAM (S (CHAOS:OPEN-STREAM "MC" "DVR"
                                          ':ERROR nil ':DIRECTION ':INPUT
                                          ':ASCII-TRANSLATION T))
    (IF (ERRORP S)
        (FORMAT STREAM "~&Error: ~A" S)
      (SEND STREAM ':FRESH-LINE)
      (STREAM-COPY-UNTIL-EOF S STREAM))))


;;; Routines for building Press pages and shipping them out an EFTP connection
;;; The state is all in special variables so you can only do one at a time
;;; Later this might be made into a more stream-like thing (as a "resource")

(DEFINE-PRESS-VARIABLE PRESS-INTERPRET-XGP-ESCAPE NIL)  ;177 is special character in input
(DEFINE-PRESS-VARIABLE PRESS-EFTP-STREAM NIL)   ;EFTP connection we send through
(DEFINE-PRESS-VARIABLE PRESS-N-CHARS)           ;Number of characters sent this part
(DEFINE-PRESS-VARIABLE PRESS-CURRENT-RECORD-NUMBER 0)   ;Record number within file
(DEFINE-PRESS-VARIABLE PRESS-X)                 ;X position computed as characters sent
(DEFINE-PRESS-VARIABLE PRESS-Y)                 ;Y ..
(DEFINE-PRESS-VARIABLE PRESS-BASELINE-Y)        ;Baseline Y, usually the same
(DEFINE-PRESS-VARIABLE PRESS-INTERCHAR-SPACING NIL)     ;Between all chars if non-NIL
(DEFINE-PRESS-VARIABLE PRESS-INTERLINE-SPACING NIL)     ;Between all lines if non-NIL
(DEFINE-PRESS-VARIABLE PRESS-PAGE-NUMBER 1)     ;Serial number of page
(DEFINE-PRESS-VARIABLE PRESS-END-PAGE-HOOK NIL) ;If non-NIL, function to call
(DEFINE-PRESS-VARIABLE PRESS-PENDING-CHARS)     ;Number of chars output but not yet known
                                                ;about at the "entity" level

(DEFINE-PRESS-VARIABLE PRESS-DATA-LIST-START)   ;Value of PRESS-N-CHARS at start of entity
(DEFINE-PRESS-VARIABLE PRESS-ENTITY-LIST-START) ;Value of (size of entity buffer) at ..

(DEFVAR PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE 4000.)
(DEFRESOURCE PRESS-PAGE-ENTITY-BUFFER ()
  :CONSTRUCTOR (MAKE-ARRAY PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE
                           ':TYPE 'ART-8B
                           ':FILL-POINTER 0))
(DEFINE-PRESS-VARIABLE PRESS-PAGE-ENTITY-BUFFER (ALLOCATE-RESOURCE 'PRESS-PAGE-ENTITY-BUFFER))
                                                ;This holds the "entity" portion of the
                                                ;current page

(DEFINE-PRESS-VARIABLE PRESS-PART-LIST NIL)     ;List of elements (part-type record-number
                                                ;                  n-records n-padding-words)
(DEFINE-PRESS-VARIABLE PRESS-FONT-LIST NIL)     ;List of elements (family-name face-name
                                                ;                  point-size rotation
                                                ;                  width height width-table)
(DEFINE-PRESS-VARIABLE PRESS-CURRENT-FONT NIL)  ;Element for selected font

(DEFINE-PRESS-VARIABLE DOVER-X0 2000.)          ;2 cm left margin
(DEFINE-PRESS-VARIABLE DOVER-Y0 (FIX (* 9.8 2540.)))    ;Where the page number goes
(DEFINE-PRESS-VARIABLE DOVER-Y1 (FIX (* 9.5 2540.)))    ;Where the text starts
(DEFINE-PRESS-VARIABLE DOVER-Y2 (FIX (* 0.5 2540.)))    ;Margin at the bottom of the page
(DEFINE-PRESS-VARIABLE LINE-WIDTH 25.)          ;Line width .01 inch
;(DEFVAR DIAGONAL-LINE-WIDTH 18.)               ;Make darkness come out even
;This provides nice thin lines, for thinner lines you might want 2 instead of 4
(DEFVAR PRESS-LINE-FONT '(NEWVEC "" 4 0 0 0 NIL))
;The way these fonts work is that the point size is the thickness of the line,
;and NEWVEC has round ends, HNEWVEC has horizontal ends, and SNEWVEC has square
;ends (that is diamond on a 45-degree line).  The way the characters are organized
;is:  Consider the right half-box, and all its radii, that is lines proceeding
;clockwise from straight-up through straight-down.  The fonts contain vectors to
;all points with integral coordinates on half-boxes of various sizes.  The widths
;of the characters are set up so that the vectors chain properly.
;
;       000-100    The 16.-bit box
;       120-160    The 8.-bit box
;       170-210    The 4.-bit box
;       214-224    The 2.-bit box
;       226-232    The 1.-bit box
;       240        The 0-bit box (or isolated point).

(DEFINE-PRESS-VARIABLE PRESS-XGP-UNDERLINE-START-X)
(DEFINE-PRESS-VARIABLE PRESS-XGP-FONT-LIST NIL)

;;;; Output to the Data and Entity Lists

;;; Macros to output things to the entity buffer

(DEFMACRO PRESS-ENTITY-BYTE (BYTE)
  `(ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER ,BYTE
                      PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE))

(DEFMACRO PRESS-ENTITY-WORD (WORD)
  (IF (ATOM WORD)
      `(PROGN (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB #o1010 ,WORD)
                                 PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
              (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB #o0010 ,WORD)
                                 PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE))
    `(LET ((FOO ,WORD))
       (PRESS-ENTITY-WORD FOO))))

(DEFMACRO PRESS-ENTITY-32WORD (WORD)
  `(LET ((FOO ,WORD))
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB #o3010 FOO)
                        PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB #o2010 FOO)
                        PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB #o1010 FOO)
                        PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB #o0010 FOO)
                        PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)))


;;; Macros to output to the data list.  These do not catch format chars (see PRESS-CHAR).

(DEFMACRO PRESS-DATA-BYTE (BYTE)
  `(PROGN (SEND PRESS-EFTP-STREAM ':TYO ,BYTE)
          (INCF PRESS-N-CHARS)))

(DEFMACRO PRESS-DATA-WORD (WORD)
  `(LET ((FOO ,WORD))
     (PRESS-DATA-BYTE (LDB #o1010 FOO))
     (PRESS-DATA-BYTE (LDB #o0010 FOO)) ))

(DEFMACRO PRESS-DATA-32WORD (WORD)
  `(LET ((FOO ,WORD))
     (PRESS-DATA-BYTE (LDB #o3010 FOO))
     (PRESS-DATA-BYTE (LDB #o2010 FOO))
     (PRESS-DATA-BYTE (LDB #o1010 FOO))
     (PRESS-DATA-BYTE (LDB #o0010 FOO))))


;;;; Press format declarations


;;; Set up so #,<SET-X> turns into 356 -- except the compiler chokes!!
;;; What? (mly)                           

;(DEFMACRO DEFPRESS (NAME . BODY)
;  `(DEFCONST ,NAME ,(CAR BODY)))

;;; These are ENTITY LIST COMMANDS

(DEFCONST <SHOW-CHARACTERS-SHORT>          #o0); + (N-1 1)
(DEFCONST <SKIP-CHARACTERS-SHORT>         #o40); + (N-1 1)
(DEFCONST <SHOW-CHARACTERS-AND-SKIP>     #o100); + (N-1 1)
(DEFCONST <SET-SPACE-X-SHORT>            #o140); + (X 2)
(DEFCONST <SET-SPACE-Y-SHORT>            #o150); + (Y 2)
(DEFCONST <FONT>                         #o160); + (FONT 1)
(DEFCONST <SKIP-CONTROL-BYTES-IMMEDIATE> #o353); (N 1)
(DEFCONST <ALTERNATIVE>                  #o354); (EL-TYPES 2) (EL-BYTES 4) (DL-BYTES 4)
(DEFCONST <ONLY-ON-COPY>                 #o355); (N 1)
(DEFCONST <SET-X>                        #o356); (X 2)
(DEFCONST <SET-Y>                        #o357); (Y 2)
(DEFCONST <SHOW-CHARACTERS>              #o360); (N 1)
(DEFCONST <SKIP-CHARACTERS>              #o361); (N 1)
(DEFCONST <SKIP-CONTROL-BYTES>           #o362); (N 2) (TYPE 1)
(DEFCONST <SHOW-CHARACTER-IMMEDIATE>     #o363); (CHAR 1)
(DEFCONST <SET-SPACE-X>                  #o364); (S 2)
(DEFCONST <SET-SPACE-Y>                  #o365); (S 2)
(DEFCONST <RESET-SPACE>                  #o366);
(DEFCONST <SPACE>                        #o367);
(DEFCONST <SET-BRIGHTNESS>               #o370); (B 1)
(DEFCONST <SET-HUE>                      #o371); (H 1)
(DEFCONST <SET-SATURATION>               #o372); (S 1)
(DEFCONST <SHOW-OBJECT>                  #o373); (N 2)
(DEFCONST <SHOW-DOTS>                    #o374); (N 4)
(DEFCONST <SHOW-DOTS-OPAQUE>             #o375); (N 4)
(DEFCONST <SHOW-RECTANGLE>               #o376); (WIDTH 2) (HEIGHT 2)
(DEFCONST <NOP>                          #o377);


;;; These are DATA LIST COMMANDS

(DEFCONST <<MOVETO>>                       0)
(DEFCONST <<DRAWTO>>                       1)
(DEFCONST <<DRAWCURVE>>                    2)

(DEFCONST <<SET-CODING>>                   1)
(DEFCONST <<SET-WINDOW>>                   1)
(DEFCONST <<SET-MODE>>                     2)
(DEFCONST <<SET-SIZE>>                     2)
(DEFCONST <<DOTS-FOLLOW>>                  3)
(DEFCONST <<GET-DOTS-FROM-FILE>>           4)
(DEFCONST <<GET-DOTS-FROM-PRESS-FILE>>     5)
(DEFCONST <<SET-SAMPLING-PROPERTIES>>      6)

(DEFCONST <<SSP-INPUT-INTENSITY>>          0)
(DEFCONST <<SSP-OUTPUT-INTENSITY>>         1)
(DEFCONST <<SSP-SCREEN>>                   2)
(DEFCONST <<SSP-DOT>>                      3)


;;;; Start Press File

(DEFUN PRESS-START-FILE (&OPTIONAL (HOST-ADDRESS DOVER-ADDRESS))
  "Open a connection to the dover or spooler.
HOST-ADDRESS is an ethernet host number or a filename (for spooling)."
  (OR (BOUNDP 'FONT-WIDTH-DATA)
      (LOAD-FONT-WIDTHS "SYS: PRESS-FONTS; FONTS WIDTHS >"))
  (SETQ PRESS-EFTP-STREAM (PRESS-OPEN-EFTP-STREAM HOST-ADDRESS PRESS-USE-EFTP)))

(DEFUN PRESS-OPEN-EFTP-STREAM (HOST-ADDRESS &OPTIONAL USE-EFTP)
  "Open a stream to an ethernet host or to a file.
If HOST-ADDRESS is a file, an ordinary output stream is returned.
If it is a number, a stream sending to that ethernet host is returned.
If USE-EFTP is T, we use EFTP directly.  Otherwise, the a chaos
connection to the AI gateway is used."
  (IF (NUMBERP HOST-ADDRESS)
      (IF USE-EFTP
          (CHAOS:MAKE-EFTP-WRITE-STREAM HOST-ADDRESS T)
          (DO ((LOSE-P NIL)) (())
            (CONDITION-CASE (CONN)
                 (CHAOS:CONNECT "AI-CHAOS-11" "DOVER")
              (SYS:CONNECTION-ERROR
                (IF (STRING-EQUAL "BUSY" (SEND CONN ':SEND-IF-HANDLES ':REASON)
                                  ':END2 4)
                    (PROGN (SETQ LOSE-P T)
                           (PROCESS-SLEEP 35. "Dover busy"))
                  (FERROR NIL "Cannot connect to DOVER server at AI-CHAOS-11:~%~A" CONN)))
  ;Very frequently, you get packet not recognized by getway 11 errors after the dover clears.
  ;This often happens if you just abort manually and try a press:print-file.  This seems
  ;to be a bug in the gateway-11.  The following is an attempt to win, and has been observed
  ;to work (sometimes).
              (:NO-ERROR
                (COND (LOSE-P
                       (CHAOS:CLOSE-CONN CONN)
                       (PROCESS-SLEEP 1000. "Dover clear")      ;a bug in the gateway-11
                       ;; Doing this seems to help clear things up.
                       (#+MIT CHAOS:HOST-UP-P #+3600 CHAOS:HOST-UP "AI-CHAOS-11")
                       (SETQ LOSE-P NIL))
                      (T
                       (RETURN (CHAOS:MAKE-STREAM CONN))))))))
    (OPEN HOST-ADDRESS ':DIRECTION ':OUTPUT ':CHARACTERS NIL ':BYTE-SIZE 8)))

;;;; Finish Press File

;Output font directory, part directory, document directory
(DEFUN PRESS-END-FILE (FILE-NAME CREATION-DATE
                       &OPTIONAL (N-COPIES 1) (USER-NAME USER-ID))
  "Finish sending a press file on the open stream, writing out tables and fonts.
FILE-NAME is the filename for the dover to say it printed.
CREATION-DATE is the date to appear on the printout.
N-COPIES is the number of copies to print.
USER-NAME is who to attribute the printout to.
Closes the output stream.
Returns the truename of the spooled file, if spooling."
  ;; The font directory part
  (SETF (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 0)
  (DO ((L PRESS-FONT-LIST (CDR L))              ; *** crock
       (FONT-NUMBER 0 (1+ FONT-NUMBER))
       (FONT))
      ((NULL L))
    (SETQ FONT (CAR L))
    (PRESS-ENTITY-WORD 16.)                     ;Length in words
    (PRESS-ENTITY-BYTE 0)                       ;Font set 0 **** crock
    (PRESS-ENTITY-BYTE FONT-NUMBER)
    (PRESS-ENTITY-BYTE 0)                       ;First char
    (PRESS-ENTITY-BYTE #o177)                   ;Last char
    (PRESS-ENTITY-BCPL-STRING (STRING-UPCASE (FIRST FONT)) 20.) ;Family
    (PRESS-ENTITY-BYTE (ENCODE-PRESS-FACE (SECOND FONT)))       ;Face code
    (PRESS-ENTITY-BYTE 0)                       ;Source (same as first char)
    (PRESS-ENTITY-WORD (THIRD FONT))            ;Positive is points, negative is micas
    (PRESS-ENTITY-WORD (FOURTH FONT)))          ;Rotation in minutes of arc anticlockwise
  (PRESS-ENTITY-WORD 0)                         ;End mark
  (SEND PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  (PRESS-FINISH-PART (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 1)
  ;; That took care of the font directory, now the part directory
  (SETF (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 0)
  (DOLIST (X (REVERSE PRESS-PART-LIST))         ;NOT nreverse!
    (PRESS-ENTITY-WORD (FIRST X))               ;Part type
    (PRESS-ENTITY-WORD (SECOND X))              ;Starting record number
    (PRESS-ENTITY-WORD (THIRD X))               ;Number of records
    (PRESS-ENTITY-WORD (FOURTH X)))             ;Amount of padding
  (SEND PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  (PRESS-FINISH-PART (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 'FOO)
  ;; The document directory
  (SETF (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 0)
  (PRESS-ENTITY-WORD 27183.)                    ;"Password"
  (PRESS-ENTITY-WORD (1+ PRESS-CURRENT-RECORD-NUMBER))  ;File size
  (PRESS-ENTITY-WORD (1- (LENGTH PRESS-PART-LIST)))     ;Number of parts
  (PRESS-ENTITY-WORD (SECOND (CAR PRESS-PART-LIST)))    ;Record number of part directory
  (PRESS-ENTITY-WORD (THIRD (CAR PRESS-PART-LIST)))     ;Number of records in part dir
  (PRESS-ENTITY-WORD 0)                         ;Back-pointer
  (PRESS-ENTITY-32WORD 0)                       ;[Date]
  (PRESS-ENTITY-WORD 1)                         ;First copy to print
  (PRESS-ENTITY-WORD N-COPIES)                  ;Last copy to print
  (PRESS-ENTITY-WORD -1)                        ;Print all pages
  (PRESS-ENTITY-WORD -1)                        ;..
  (PRESS-ENTITY-WORD -1)                        ;Default printing mode
  (DOTIMES (I (- #o200 13.))                    ;Padding
    (PRESS-ENTITY-WORD -1))
  (PRESS-ENTITY-BCPL-STRING FILE-NAME 52.)
  (PRESS-ENTITY-BCPL-STRING USER-NAME 32.)
  (PRESS-ENTITY-BCPL-STRING CREATION-DATE 40.)
  (SEND PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  (PRESS-FINISH-PART (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 1)
  (SEND PRESS-EFTP-STREAM ':CLOSE)
  (SEND PRESS-EFTP-STREAM ':SEND-IF-HANDLES ':TRUENAME))

;;;; Pages

(DEFUN PRESS-OPEN-PAGE ()
  "Start a new page in the press file output."
  (STORE-ARRAY-LEADER 0 PRESS-PAGE-ENTITY-BUFFER 0)
  (SETQ PRESS-N-CHARS 0))

(DEFUN PRESS-CLOSE-PAGE ()
  "Finish a page of press file output."
  (WHEN PRESS-END-PAGE-HOOK
    (FUNCALL PRESS-END-PAGE-HOOK))              ; User must open his own entity
  ;; Make the length of the data buffer a multiple of a word
  (WHEN (ODDP PRESS-N-CHARS)
    (SEND PRESS-EFTP-STREAM ':TYO 0)
    (INCF PRESS-N-CHARS))
  ;; Output a zero word between the data list and the entity list
  (SEND PRESS-EFTP-STREAM ':TYO 0)
  (SEND PRESS-EFTP-STREAM ':TYO 0)
  ;; Output the entity buffer
  (SEND PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  ;; Pad to a record (512-byte) boundary, and advance PRESS-CURRENT-RECORD-NUMBER
  (PRESS-FINISH-PART (+ PRESS-N-CHARS 2 (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER)) 0)
  (INCF PRESS-PAGE-NUMBER))

;;; Hair shared between page parts and other parts
(DEFUN PRESS-FINISH-PART (NBYTES PART-TYPE &AUX NWORDS NRECORDS PADDING)
  "Finish a /"part/" of a press file.
NBYTES is its length, and PART-TYPE is a code saying what type of part.
For a page of output (which is a kind of part). PART-TYPE should be 0."
  (CHECK-ARG NBYTES EVENP "an even number of bytes")
  (SETQ NWORDS (FLOOR NBYTES 2))
  (MULTIPLE-VALUE (NRECORDS PADDING) (CEILING NWORDS 256.))
  (SETQ PADDING (- PADDING))
  (DOTIMES (I (* PADDING 2))
    (SEND PRESS-EFTP-STREAM ':TYO 0))
  (PUSH (LIST PART-TYPE PRESS-CURRENT-RECORD-NUMBER NRECORDS PADDING)
        PRESS-PART-LIST)
  (INCF PRESS-CURRENT-RECORD-NUMBER))


;;; press-start-page is an ungodly crock that is here just for existing programs
;;; using press-open-page and explicitly opening entities is cleaner
(DEFUN PRESS-START-PAGE ()
  "Obsolete interface to PRESS-OPEN-PAGE."
  (PRESS-OPEN-PAGE)
  (PRESS-START-ENTITY)
  (PRESS-SET-CURSOR 0 DOVER-Y1))                ;Put cursor at top of page

;;; This is also here for existing programs
(DEFUN PRESS-END-PAGE ()
  "Obsolete interface to PRESS-CLOSE-PAGE."
  (AND PRESS-END-PAGE-HOOK (FUNCALL PRESS-END-PAGE-HOOK))       ;Let user put titles etc.
  (PRESS-END-ENTITY)
  (LET ((PRESS-END-PAGE-HOOK NIL))
    (PRESS-CLOSE-PAGE)))


;;;; Entities

;;; Start an entity

(DEFUN PRESS-OPEN-ENTITY ()
  "Begin a new /"entity/" in the press file output.
Refer to documentation on the press file format for what an entity is."
  (SETQ PRESS-DATA-LIST-START   PRESS-N-CHARS
        PRESS-ENTITY-LIST-START (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER)
        PRESS-PENDING-CHARS     0))

;Finish the current entity.  You can start another if you like.
(DEFUN PRESS-CLOSE-ENTITY (&OPTIONAL (X-OFF DOVER-X0) (Y-OFF DOVER-Y2)
                                     (WIDTH (*  85. 254.)) (HEIGHT (* 11. 2540.)))
  "Finish an /"entity/" in press file output.
The four arguments specify the bounding box of the entity on the page.
All coordinates in the entity are relative to X-OFF and Y-OFF."
  (PRESS-PUT-PENDING-CHARS)
  ;; Pad entity to word boundary with NOP
  (AND (ODDP (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0))
       (PRESS-ENTITY-BYTE #o377))
  ;; Entity trailer
  (PRESS-ENTITY-BYTE 0)                         ;Type     **** crocks
  (PRESS-ENTITY-BYTE 0)                         ;Font set ****
  (PRESS-ENTITY-32WORD PRESS-DATA-LIST-START)   ;Begin-byte
  (PRESS-ENTITY-32WORD (- PRESS-N-CHARS PRESS-DATA-LIST-START)) ;Byte-length
  (PRESS-ENTITY-WORD X-OFF)                     ;X offset (left margin)
  (PRESS-ENTITY-WORD Y-OFF)                     ;Y offset (bottom margin)
  (PRESS-ENTITY-WORD 0)                         ;Left     ****
  (PRESS-ENTITY-WORD 0)                         ;Bottom   ****
  (PRESS-ENTITY-WORD WIDTH)                     ;Width
  (PRESS-ENTITY-WORD HEIGHT)                    ;Height
  (PRESS-ENTITY-WORD                            ;Entity length
    (FLOOR (- (+ (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER) 2)
              PRESS-ENTITY-LIST-START)
           2)))

(DEFUN PRESS-MAYBE-NEW-ENTITY ()
  "If necessary, close the current entity and open a new one.
Call this from time to time to avoids problems with entities that are too large.
You must reinitialize the cursor position and selected font after this."
  (COND ((> (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER) 25000.)
         (PRESS-END-ENTITY)
         (PRESS-START-ENTITY))))

;;; grandfather version of PRESS-OPEN-ENTITY
(DEFUN PRESS-START-ENTITY (&AUX TEM)
  (PRESS-OPEN-ENTITY)
  (AND PRESS-CURRENT-FONT                       ;Restore font
       (SETQ TEM (FIND-POSITION-IN-LIST PRESS-CURRENT-FONT PRESS-FONT-LIST))
       (PRESS-SELECT-FONT TEM)))

(DEFUN PRESS-END-ENTITY ()
  (PRESS-CLOSE-ENTITY))

;;;; Random Functions

(DEFUN PRESS-SET-CURSOR (X Y)
  "Move press file cursor to X, Y (in micas)."
  (PRESS-PUT-PENDING-CHARS)
  (PRESS-ENTITY-BYTE <SET-X>)
  (PRESS-ENTITY-WORD X)
  (PRESS-ENTITY-BYTE <SET-Y>)
  (PRESS-ENTITY-WORD Y)
  (SETQ PRESS-X X PRESS-Y Y))

(DEFUN PRESS-PUT-PENDING-CHARS ()
  "Output any buffered up characters output with PRESS-CHAR.
This should be done before outputting directly to the current entity."
  (COND ((ZEROP PRESS-PENDING-CHARS) NIL)
        (( PRESS-PENDING-CHARS #o40) (PRESS-ENTITY-BYTE (1- PRESS-PENDING-CHARS)))
        (T (DO () ((< PRESS-PENDING-CHARS #o400))
             (PRESS-ENTITY-BYTE <SHOW-CHARACTERS>)
             (PRESS-ENTITY-BYTE <NOP>)
             (SETQ PRESS-PENDING-CHARS (- PRESS-PENDING-CHARS #o377)))
           (PRESS-ENTITY-BYTE <SHOW-CHARACTERS>)
           (PRESS-ENTITY-BYTE PRESS-PENDING-CHARS)))
  (SETQ PRESS-PENDING-CHARS 0))

(DEFUN PRESS-CHAR (CHAR)
  "Output CHAR to press file, with buffering.  Format effectors are allowed."
  (COND ((< CHAR #o200)                         ;Printing
         (SEND PRESS-EFTP-STREAM ':TYO CHAR)
         (INCF PRESS-N-CHARS)
         (INCF PRESS-PENDING-CHARS)
         (LET ((WIDTH (AREF (SEVENTH PRESS-CURRENT-FONT) CHAR)))
           (IF (MINUSP WIDTH)
               (FORMAT *ERROR-OUTPUT*
                       "~&~C (#o~O) undefined character in ~A~D~A~%"
                       CHAR CHAR
                       (FIRST PRESS-CURRENT-FONT)
                       (THIRD PRESS-CURRENT-FONT)
                       (SECOND PRESS-CURRENT-FONT))
             (INCF PRESS-X WIDTH)))
         (WHEN PRESS-INTERCHAR-SPACING
           (PRESS-SET-CURSOR (+ PRESS-X PRESS-INTERCHAR-SPACING) PRESS-Y)))
        ((= CHAR #/BACKSPACE)
         (PRESS-SET-CURSOR (- PRESS-X (AREF (SEVENTH PRESS-CURRENT-FONT) #/SP))
                           PRESS-Y))
        ((= CHAR #/LINEFEED)
         (LET ((Y (- PRESS-Y
                     (IF PRESS-INTERLINE-SPACING
                         (- (IF (MINUSP PRESS-INTERLINE-SPACING) (SIXTH PRESS-CURRENT-FONT) 0)
                            PRESS-INTERLINE-SPACING)
                       (SIXTH PRESS-CURRENT-FONT)))))
           (IF (MINUSP Y)
               (LET ((OLD-X PRESS-X))
                 (PRESS-CHAR #/FORM)
                 (PRESS-SET-CURSOR OLD-X PRESS-Y))
             (PRESS-SET-CURSOR PRESS-X Y)))
         (SETQ PRESS-BASELINE-Y PRESS-Y
               PRESS-INTERCHAR-SPACING NIL))
        ((= CHAR #/TAB)
         ;; The bounding box seems to be wedged, it's not the same as the character
         ;; width in fixed-width fonts.  So use the width of space.
         (LET ((TAB-WIDTH (* 8 (AREF (SEVENTH PRESS-CURRENT-FONT) #/SP))))
           (PRESS-SET-CURSOR (* (1+ (FLOOR PRESS-X TAB-WIDTH)) TAB-WIDTH) PRESS-Y)))
        ((= CHAR #/RETURN)
         (LET ((Y (- PRESS-Y
                     (COND (PRESS-INTERLINE-SPACING (- (IF (MINUSP PRESS-INTERLINE-SPACING)
                                                           (SIXTH PRESS-CURRENT-FONT) 0)
                                                       PRESS-INTERLINE-SPACING))
                           (T (SIXTH PRESS-CURRENT-FONT))))))
           (IF (MINUSP Y) (PRESS-CHAR #/FORM)
             (PRESS-SET-CURSOR 0 Y)))
         (SETQ PRESS-BASELINE-Y PRESS-Y
               PRESS-INTERCHAR-SPACING NIL))
        ((= CHAR #/FORM)
         (PRESS-END-PAGE)
         (PRESS-START-PAGE)
         (SETQ PRESS-INTERCHAR-SPACING NIL)))
  NIL)

(DEFUN PRESS-STRING (STRING &OPTIONAL (FROM 0) TO)
  "Output STRING, or part of it, to the press file with buffering.
Format effectors may appear in the string."
  (SETQ STRING (STRING STRING))
  (OR TO (SETQ TO (STRING-LENGTH STRING)))
  (DO ((I FROM (1+ I))) (( I TO))
    (PRESS-CHAR (AREF STRING I))))

;;;; Font Stuff

;;; These should be rewritten to consider font-set ****
;;; Also, want to define font numbers randomly instead of sequentially

(DEFUN TRANSLATE-LM-TO-DOVER-FONT (FONT)
  "Given a Lisp machine font name, return corresponding dover font name."
  (COND ;;If equivalence found, use that font
        ((CDR (ASS #'STRING-EQUAL FONT DOVER-FONT-EQUIVALENCES)))
        ;;If the font is a known Dover font, just use it
        ((APPLY 'FIND-FONT-DATA-1 (MULTIPLE-VALUE-LIST (DECODE-FONT-NAME FONT NIL))) FONT)
        ;;Otherwise ask user for font name
        (T (FQUERY '(:TYPE :READLINE :CLEAR-INPUT T :BEEP T)    ;Unknown font
                   "No equivalent Dover font is known for ~A.  Specify font to use: "
                   FONT))))

(DEFUN PRESS-DEFINE-FONT-NAME (FONT-NAME)
  "Make dover font FONT-NAME available for use in this press file.
Returns the font number it is assigned."
  (MULTIPLE-VALUE-BIND (FAMILY-NAME FACE-NAME POINT-SIZE)
      (DECODE-FONT-NAME FONT-NAME)
    (PRESS-DEFINE-FONT FAMILY-NAME FACE-NAME POINT-SIZE 0)))

;;; Add a font to the font set and return its font number
(DEFUN PRESS-DEFINE-FONT (FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
  (OR (PRESS-LOOKUP-FONT FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
      (LET ((WIDTH (GET-FONT-WIDTH-AND-HEIGHT FAMILY-NAME FACE-NAME POINT-SIZE))
            HEIGHT WIDTH-ARRAY FONT-DESC FONT-NUMBER)
        (SETQ HEIGHT (CADR WIDTH) WIDTH (CAR WIDTH))    ;Bounding box for font
        (SETQ WIDTH-ARRAY (GET-FONT-WIDTH-DATA FAMILY-NAME FACE-NAME POINT-SIZE))
        (SETQ FONT-DESC (LIST FAMILY-NAME FACE-NAME POINT-SIZE ROTATION
                              WIDTH HEIGHT WIDTH-ARRAY))
        (SETQ PRESS-FONT-LIST (NCONC PRESS-FONT-LIST (NCONS FONT-DESC)))
        (SETQ FONT-NUMBER (1- (LENGTH PRESS-FONT-LIST)))
        (AND ( FONT-NUMBER 16.) (FERROR NIL "Maximum of 16 fonts allowed."))
        FONT-NUMBER)))

;;; Similar to above, but works when there is no Fonts Widths data.  The
;;; assumed size is completely bogus, don't format lines depending on it.
;;; Actually, I use the size data from TIMESROMAN18.
;;; Second value is T if font not found in Fonts Widths.
(DEFUN PRESS-DEFINE-FONT-FAKE (FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
  (OR (PRESS-LOOKUP-FONT FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
      (IF (ERRSET (FIND-FONT-DATA FAMILY-NAME FACE-NAME POINT-SIZE) NIL)
          (PRESS-DEFINE-FONT FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
        (LET ((WIDTH 633.) (HEIGHT 698.) WIDTH-ARRAY FONT-DESC FONT-NUMBER)
          (SETQ WIDTH-ARRAY (MAKE-ARRAY #o400 ':TYPE 'ART-16B))
          (FILLARRAY WIDTH-ARRAY '(633.))
          (SETQ FONT-DESC (LIST FAMILY-NAME FACE-NAME POINT-SIZE ROTATION
                                WIDTH HEIGHT WIDTH-ARRAY))
          (SETQ PRESS-FONT-LIST (NCONC PRESS-FONT-LIST (NCONS FONT-DESC)))
          (SETQ FONT-NUMBER (1- (LENGTH PRESS-FONT-LIST)))
          (AND ( FONT-NUMBER 16.) (FERROR NIL "Maximum of 16 fonts allowed."))
          (VALUES FONT-NUMBER T)))))

;;; Find position of font in PRESS-FONT-LIST
(DEFUN PRESS-LOOKUP-FONT (FAMILY FACE POINT-SIZE ROTATION)
  "Return the already-assigned font number of a font.
The font is specified by its decoded name."
  (DO ((L PRESS-FONT-LIST (CDR L))
       (I 0 (1+ I)))
      ((NULL L))
    (AND (#+MIT EQUALP #+3600 STRING-EQUAL FAMILY (CAAR L))
         (#+MIT EQUALP #+3600 STRING-EQUAL FACE (SECOND (CAR L)))
         (EQUAL POINT-SIZE (THIRD (CAR L)))
         (EQUAL ROTATION (FOURTH (CAR L)))
         (RETURN I))))

;;; Select a font, by number
(DEFUN PRESS-SELECT-FONT (FONT-NUMBER)
  "Select a font, specified by font number.  Use PRESS-LOOKUP-FONT to find a font's number."
  (PRESS-PUT-PENDING-CHARS)
  (OR (SETQ PRESS-CURRENT-FONT (NTH FONT-NUMBER PRESS-FONT-LIST))
      (FERROR NIL "Font number ~D not defined" FONT-NUMBER))
  (PRESS-ENTITY-BYTE (+ #o160 FONT-NUMBER)))

;;; Single-letters in the string select features as follows:
;;;  B bold, L light
;;;  I italic
;;;  C condensed
;;;  E expanded
(DEFUN ENCODE-PRESS-FACE (STR)
  "Given a face code string, such as /"BI/", return the numeric code Xerox uses."
  (SETQ STR (STRING STR))
  (DO ((FACE-CODE 0)
       (I 0 (1+ I))
       (N (STRING-LENGTH STR))
       (CH))
      ((= I N) FACE-CODE)
    (SETQ CH (CHAR-UPCASE (AREF STR I)))
    (SETQ FACE-CODE (+ FACE-CODE
                       (SELECTQ CH
                         (#/B 2)
                         (#/L 4)
                         (#/I 1)
                         (#/C 6)
                         (#/E 12.)
                         (OTHERWISE (FERROR NIL "~C illegal character in face name /"~A/""
                                            CH STR)))))))


(DEFUN PRESS-ENTITY-BCPL-STRING (STRING NBYTES &AUX REAL-LENGTH)
  "Write STRING in BCPL format into the press file.
It is padded with zeros to be NBYTES long."
  (SETQ STRING (STRING STRING))
  (PRESS-ENTITY-BYTE (SETQ REAL-LENGTH (MIN (STRING-LENGTH STRING) (1- NBYTES))))
  (DOTIMES (I REAL-LENGTH)
    (PRESS-ENTITY-BYTE (AREF STRING I)))
  (DOTIMES (I (- NBYTES (1+ REAL-LENGTH)))
    (PRESS-ENTITY-BYTE 0)))


(DEFVAR PRESS-LINE-USE-SPECIAL-OPCODE NIL)
(DEFVAR NEWVEC-SLOPE-TABLE)
(DEFVAR NEWVEC-DX-TABLE)
(DEFVAR NEWVEC-DY-TABLE)

(DEFUN MAKE-NEWVEC-TABLES ()
  (DO ((TBL (MAKE-ARRAY #o101))
       (XTBL (MAKE-ARRAY #o101))
       (YTBL (MAKE-ARRAY #o101))
       (BITS-TO-MICAS (// 2540.0s0 384.))
       (I 0 (1+ I))
       (DX 0)
       (DY 16.))
      ((= I 101)
       (SETQ NEWVEC-SLOPE-TABLE TBL
             NEWVEC-DX-TABLE XTBL
             NEWVEC-DY-TABLE YTBL))
    (ASET (COND ((= I 0) 1s18)
                ((= I 100) -1s18)
                (T (// (#+MIT SMALL-FLOAT #+3600 FLOAT DY) DX))) TBL I)
    (ASET (* DX BITS-TO-MICAS) XTBL I)
    (ASET (* DY BITS-TO-MICAS) YTBL I)
    (COND ((< I #o20) (INCF DX))
          ((< I 60) (DECF DY))
          (T (DECF DX)))))

(MAKE-NEWVEC-TABLES)

(DEFUN PRESS-LINE (X0 Y0 X1 Y1 &AUX (DX (ABS (- X0 X1))) (DY (ABS (- Y0 Y1))) FONT-NUMBER)
  "Output commands to press file to draw line from X0, Y0 to X1, Y1 (in micas)."
  (PRESS-PUT-PENDING-CHARS)
  (PRESS-MAYBE-NEW-ENTITY)                      ;This should make DPLT work better
  (COND (PRESS-LINE-USE-SPECIAL-OPCODE
         (PRESS-SET-CURSOR X0 Y0)
         (PRESS-ENTITY-BYTE 201)
         (PRESS-ENTITY-WORD X1)
         (PRESS-ENTITY-WORD Y1))
        ((= X0 X1)                              ;Vertical line
         (PRESS-SET-CURSOR (- X0 (TRUNCATE LINE-WIDTH 2)) (MIN Y0 Y1))  ;Lower left corner
         (PRESS-SHOW-RECT LINE-WIDTH DY))
        ((= Y0 Y1)                              ;Horizontal line
         (PRESS-SET-CURSOR (MIN X0 X1) (- Y0 (TRUNCATE LINE-WIDTH 2)))  ;Lower left corner
         (PRESS-SHOW-RECT DX LINE-WIDTH))
        (T                                      ;Diagonal line, use the font
         (OR (MEMQ PRESS-LINE-FONT PRESS-FONT-LIST)
             (SETQ PRESS-FONT-LIST (NCONC PRESS-FONT-LIST (NCONS PRESS-LINE-FONT))))
         (SETQ FONT-NUMBER (FIND-POSITION-IN-LIST PRESS-LINE-FONT PRESS-FONT-LIST))
         (AND ( FONT-NUMBER 16.) (FERROR NIL "Maximum of 16 fonts allowed."))
         (OR (EQ PRESS-CURRENT-FONT PRESS-LINE-FONT)
             (PRESS-SELECT-FONT FONT-NUMBER))
         (IF (< X1 X0) (PSETQ X0 X1 Y0 Y1 X1 X0 Y1 Y0)) ;(X0,Y0) are left end
         (PRESS-SET-CURSOR X0 Y0)               ;Proceed inevitably toward the right
         (AND (< Y1 Y0) (SETQ DY (- DY)))
         ;; Always use 2 characters of the largest size except for finishing up
         (DO ((CH2 1 (1+ CH2))
              (CH1 0 CH2)
              (SLOPE (// (#+MIT SMALL-FLOAT #+3600 FLOAT DY) DX)))
             ((OR (= CH2 100)
                  (< (AREF NEWVEC-SLOPE-TABLE CH2) SLOPE))
              (DO ((X X0 (+ X XINC))
                   (Y Y0 (+ Y YINC))
                   (CH) (XINC) (YINC) (STOP NIL) (RUN)
                   (CDX1 (AREF NEWVEC-DX-TABLE CH1))
                   (CDY1 (AREF NEWVEC-DY-TABLE CH1))
                   (CDX2 (AREF NEWVEC-DX-TABLE CH2))
                   (CDY2 (AREF NEWVEC-DY-TABLE CH2))
                   (LENGTH (+ (ABS (- X1 X0)) (ABS (- Y1 Y0)))))
                  (STOP)
                ;; If Y would be below the line, use CH1 else use CH2.
                ;; Watch out for zero divide possibility.
                (IF (OR (= (SETQ RUN (- (+ X CDX2) X0)) 0)
                        (< (// (#+MIT SMALL-FLOAT #+3600 FLOAT
                                (- (+ Y CDY2) Y0)) RUN) SLOPE))
                    (SETQ CH CH1 XINC CDX1 YINC CDY1)
                  (SETQ CH CH2 XINC CDX2 YINC CDY2))
                ;; If getting too close to the endpoint, use a shorter line
                (DO ((STRTL '(0 #o120 #o170 #o214 #o226) (CDR STRTL))
                     (I CH)
                     (D 2 (* D 2)))
                    ;; This test minimizes distance to endpoint by taxi cab
                    ;; metric without overshooting.
                    ((OR ( (+ (ABS (- (+ Y YINC) Y0)) (ABS (- (+ X XINC) X0)))
                            LENGTH)
                         (SETQ STOP (NULL (CDR STRTL)))))
                  (SETQ CH (+ (TRUNCATE (- CH (CAR STRTL)) 2) (CADR STRTL))
                        I (* (TRUNCATE I D) D)
                        XINC (TRUNCATE (AREF NEWVEC-DX-TABLE I) D)
                        YINC (TRUNCATE (AREF NEWVEC-DY-TABLE I) D)))
                (COND ((NOT STOP)
                       (SEND PRESS-EFTP-STREAM ':TYO CH)
                       (INCF PRESS-N-CHARS)
                       (INCF PRESS-PENDING-CHARS)))))))))

;;; Subroutine for the above
(DEFUN PRESS-SHOW-RECT (WIDTH HEIGHT)
  (PRESS-ENTITY-BYTE <SHOW-RECTANGLE>)
  (PRESS-ENTITY-WORD WIDTH)
  (PRESS-ENTITY-WORD HEIGHT))

;;;; Output a BITMAP off a window

;;; This uses pixel array because the halfword array requires a bitreverse
;;;      due to the DOVERs inflexibility

(DEFUN PRESS-TV-BITMAP (U V &OPTIONAL (WINDOW TV:DEFAULT-SCREEN)
                                      (LEFT 0) (TOP 0) (RIGHT (TV:SHEET-WIDTH WINDOW))
                                      (BOTTOM (1- (TV:SHEET-HEIGHT WINDOW)))) ;; mod nis 9/2/82
  "Output part of the window WINDOW to the press file, as an entity.
Assumes that a page has been opened but no entity is in progress.
U and V are the coordinates in micas of where the lower left edge
 appears on the page.
LEFT, TOP, RIGHT and BOTTOM are the portion of the window to output."
  (IF (> LEFT RIGHT) (PSETQ LEFT RIGHT RIGHT LEFT))
  (IF (> TOP BOTTOM) (PSETQ TOP BOTTOM BOTTOM TOP))
  (LET ((ARRAY  (TV:SHEET-SCREEN-ARRAY WINDOW))
        (START  0)
        (LINES  (LOGAND (+ 15. (- BOTTOM TOP)) #o1760))
        (DOTS   (LOGAND (+ 15. (- RIGHT LEFT)) #o1760))
        (SIZE   32.)                            ; a dot is 32 x 32 micas
        (WIDTH  0)
        (HEIGHT 0))
    (SETQ WIDTH  (* DOTS  SIZE)
          HEIGHT (* LINES SIZE))
    (PRESS-OPEN-ENTITY)
    (PRESS-PUT-PENDING-CHARS)
    (PRESS-SET-CURSOR U V)
    (WHEN (ODDP PRESS-N-CHARS)
      (PRESS-ENTITY-BYTE <SKIP-CHARACTERS-SHORT>)
      (PRESS-DATA-BYTE 0))
    (SETQ START PRESS-N-CHARS)                  ; remember where we are in DL
    (PROGN (PRESS-DATA-BYTE <<SET-CODING>>)
           (PRESS-DATA-BYTE 0)
           (PRESS-DATA-WORD DOTS)
           (PRESS-DATA-WORD LINES))
    (PROGN (PRESS-DATA-BYTE <<SET-MODE>>)
           (PRESS-DATA-BYTE 3.))                ; Dover Requires this direction
    (PROGN (PRESS-DATA-WORD <<SET-SIZE>>)
           (PRESS-DATA-WORD WIDTH)
           (PRESS-DATA-WORD HEIGHT))
    (PROGN (PRESS-DATA-WORD <<DOTS-FOLLOW>>)
           (DO ((Y TOP (1+ Y))
                (LAST-Y (+ LINES TOP)))
               (( Y LAST-Y))
             (DO ((X LEFT (+ X 16.))
                  (LAST-X (+ DOTS LEFT)))
                 (( X LAST-X))
               (PRESS-DATA-WORD
                 (DO ((I 0 (1+ I))
                      (R 0))
                     (( I 16.) R)
                   (SETQ R (+ (LSH R 1) (AREF ARRAY (+ (MIN X RIGHT) I) (MIN Y BOTTOM)))))))))
    (PROGN (PRESS-ENTITY-BYTE   <SHOW-DOTS>)
           (PRESS-ENTITY-32WORD (TRUNCATE (- PRESS-N-CHARS START) 2)))
    (PRESS-CLOSE-ENTITY U V WIDTH HEIGHT)))


;;;; Print a file

(DEFUN PRINT-PRESS-FILE (FILE-NAME &KEY &OPTIONAL HOST-ADDRESS OUTPUT-FILE SPOOL EFTP FORMAT)
  "Print a press file FILE-NAME on the dover.
:OUTPUT-FILE specifies a file to write the modified press file into.
:SPOOL if T says spool the file on MC.
:EFTP if T says use EFTP directly, to the dover. for the transfer.
:HOST-ADDRESS specifies an ethernet host to send to, using EFTP directly."
  (DECLARE (ARGLIST FILE-NAME &KEY &OPTIONAL HOST-ADDRESS OUTPUT-FILE SPOOL EFTP))
  FORMAT
  (WITH-OPEN-FILE (INPUT-STREAM FILE-NAME)
    (WITH-OPEN-STREAM
      (OUTPUT-STREAM (LET ((USE-EFTP (OR HOST-ADDRESS EFTP)))
                       (IF OUTPUT-FILE (SETQ HOST-ADDRESS OUTPUT-FILE))
                       (IF SPOOL (SETQ HOST-ADDRESS (FORMAT NIL "MC: .DOVR.; ~A >" USER-ID)))
                       (PRESS-OPEN-EFTP-STREAM (OR HOST-ADDRESS DOVER-ADDRESS) USE-EFTP)))
      (STREAM-COPY-UNTIL-EOF INPUT-STREAM OUTPUT-STREAM)
      (CLOSE OUTPUT-STREAM)
      (SEND OUTPUT-STREAM ':SEND-IF-HANDLES ':TRUENAME))))

(DEFUN PRINT-PRESS-STREAM (INPUT-STREAM &KEY &OPTIONAL HOST-ADDRESS OUTPUT-FILE SPOOL EFTP
                           &ALLOW-OTHER-KEYS)
  "Print a press file read from INPUT-STREAM on the dover.
:OUTPUT-FILE specifies a file to write the modified press file into.
:SPOOL if T says spool the file on MC.
:EFTP if T says use EFTP directly, to the dover. for the transfer.
:HOST-ADDRESS specifies an ethernet host to send to, using EFTP directly."
  (WITH-OPEN-STREAM
    (OUTPUT-STREAM (LET ((USE-EFTP (OR HOST-ADDRESS EFTP)))
                     (IF OUTPUT-FILE (SETQ HOST-ADDRESS OUTPUT-FILE))
                     (IF SPOOL (SETQ HOST-ADDRESS (FORMAT NIL "MC: .DOVR.; ~A >" USER-ID)))
                     (PRESS-OPEN-EFTP-STREAM (OR HOST-ADDRESS DOVER-ADDRESS) USE-EFTP)))
    (STREAM-COPY-UNTIL-EOF INPUT-STREAM OUTPUT-STREAM)
    (CLOSE OUTPUT-STREAM)
    (SEND OUTPUT-STREAM ':SEND-IF-HANDLES ':TRUENAME)))

(DEFUN PRINT-FILE (FILE-NAME &REST OPTIONS)
  "Print FILE-NAME on the dover.
:FONT specifies the font to use;
 or, :TV-FONTS a list of TV fonts to use equivalents of;
 or, :PRESS-FONTS, a list of press fonts to use.
:HEADING-FONT specifies the font for page headings.
:VSP specifies extra vertical space in micas between lines.
:PAGE-HEADINGS NIL inhibits generation of page headings.
:COPIES specifies the number of copies to be printed.
:OUTPUT-FILE specifies a file to write the press file into.
:SPOOL T says spool the file on MC.
:EFTP T says use EFTP directly, to the dover. for the transfer.
:HOST-ADDRESS specifies an ethernet host to send to, using EFTP directly."
  (DECLARE (ARGLIST FILE-NAME
                    &KEY &OPTIONAL
                    (FONT "LPT8") TV-FONTS PRESS-FONTS HEADING-FONT
                    (PAGE-HEADINGS T) VSP (COPIES 1)
                    OUTPUT-FILE SPOOL EFTP HOST-ADDRESS))
  (WITH-OPEN-FILE (STREAM FILE-NAME ':DIRECTION ':INPUT)
    (APPLY 'PRINT-FROM-STREAM STREAM OPTIONS)))

(DEFUN SPOOL-FILE (FILE-NAME &REST OPTIONS)
  "Like PRESS:PRINT-FILE but spool through MC."
  (DECLARE (ARGLIST FILE-NAME
                    &KEY &OPTIONAL
                    (FONT "LPT8") FONT-LIST HEADER-FONT
                    (PAGE-HEADINGS T) (COPIES 1) XGP))
  (WITH-OPEN-FILE (STREAM FILE-NAME ':DIRECTION ':INPUT)
    (APPLY 'PRINT-FROM-STREAM STREAM ':SPOOL T OPTIONS)))

(DEFMACRO WITH-PRESS-FONT ((TEMPORARY-FONT) &BODY BODY)
  `(LET ((OLD-FONT PRESS-CURRENT-FONT))
     (PRESS-SELECT-FONT
       (IF (NUMBERP ,TEMPORARY-FONT) ,TEMPORARY-FONT
           (FIND-POSITION-IN-LIST ,TEMPORARY-FONT PRESS-FONT-LIST)))
     (PROGN . ,BODY)
     (PRESS-SELECT-FONT (FIND-POSITION-IN-LIST OLD-FONT PRESS-FONT-LIST))))

;;; :FONT-LIST says look for  characters in the stream, and overrides :FONT.
;;; :HEADER-FONT specifies font to use for page headings;
;;; default is to use the first font in :FONT-LIST, or :FONT.

;;; By default, output goes straight to the Dover.
;;; Specify :SPOOL T and we spool to MC.
;;; Specify :OUTPUT-FILE <filename> and we output to that file.
;;; Specify :HOST-ADDRESS, and we use EFTP to that ethernet address.
;;; Specify just :EFTP T, and we use EFTP to the Dover.
(DEFUN PRINT-FROM-STREAM (INPUT-STREAM
                          &KEY
                          (FONT "LPT8" FONT-P) FONT-LIST TV-FONTS PRESS-FONTS FONTS
                          HEADING-FONT (PAGE-HEADINGS T)
                          VSP (COPIES 1) XGP
                          (FILE-NAME (SEND INPUT-STREAM ':SEND-IF-HANDLES ':TRUENAME))
                          HOST-ADDRESS OUTPUT-FILE SPOOL EFTP
                          FORMAT
                          &AUX CREATION-DATE
                          ;; List of indices in PRESS-FONT-LIST that correspond to the
                          ;; font numbers referred to by the user,
                          ;; for files containing control-F's.
                          USER-FONT-NUMBER-LIST
                          HEADER-FONT-NUMBER
                          &ALLOW-OTHER-KEYS)
  "Print the data read from INPUT-STREAM on the dover.
:FILE-NAME is used in page headings and on the coversheet; default is STREAM's truename.
:FONT specifies the font to use;
 or, :TV-FONTS, a list of TV fonts to use equivalents of;
 or, :PRESS-FONTS or :FONTS or :FONT-LIST, a list of press fonts to use.
:HEADING-FONT specifies the font for page headings.
:PAGE-HEADINGS NIL inhibits generation of page headings.
:VSP specifies extra vertical space in micas between lines.
:COPIES specifies the number of copies to be printed.
:XGP T says the input file is in XGP format.
:OUTPUT-FILE specifies a file to write the press file into.
:SPOOL T says spool the file on MC.
:EFTP T says use EFTP directly, to the dover. for the transfer.
:HOST-ADDRESS specifies an ethernet host to send to, using EFTP directly."
  (DECLARE (SPECIAL FILE-NAME CREATION-DATE HEADER-FONT-NUMBER))
  FORMAT
  (BIND-PRESS-VARIABLES
    (IF VSP (SETQ PRESS-INTERLINE-SPACING (- VSP)))
    (IF XGP (SETQ PRESS-INTERPRET-XGP-ESCAPE T PAGE-HEADINGS NIL))
    (IF PRESS-FONTS (SETQ FONT-LIST PRESS-FONTS))
    (IF FONTS (SETQ FONT-LIST FONTS))
    (OR XGP FONT-P FONT-LIST    ;Don't hack fonts if user already gave some
        (LET ((FONT-SPEC (OR (GET (CONS NIL (#+MIT FS:FILE-EXTRACT-ATTRIBUTE-LIST
                                             #+3600 FS:PATHNAME-ATTRIBUTE-LIST INPUT-STREAM))
                                  ':FONTS)
                             TV-FONTS)))
          (COND ((NULL FONT-SPEC))              ;No fonts in file?
                ((ATOM FONT-SPEC) (SETQ FONT (TRANSLATE-LM-TO-DOVER-FONT FONT-SPEC)))
                (T (SETQ FONT-LIST (MAPCAR #'(LAMBDA (FONT) (TRANSLATE-LM-TO-DOVER-FONT FONT))
                                           FONT-SPEC))))))
    (IF (OR HOST-ADDRESS EFTP)
        (SETQ PRESS-USE-EFTP T))
    (IF OUTPUT-FILE (SETQ HOST-ADDRESS OUTPUT-FILE))
    (IF SPOOL (SETQ HOST-ADDRESS (FORMAT NIL "MC: .DOVR.; ~A >" USER-ID)))
    (OR HOST-ADDRESS
        (SETQ HOST-ADDRESS DOVER-ADDRESS))
    (SETQ CREATION-DATE (OR (SEND INPUT-STREAM ':SEND-IF-HANDLES ':CREATION-DATE)
                            (TIME:GET-UNIVERSAL-TIME)))
    (SETQ CREATION-DATE (TIME:PRINT-UNIVERSAL-TIME CREATION-DATE NIL))
    (IF PAGE-HEADINGS
        (SETQ PRESS-END-PAGE-HOOK
              #'(LAMBDA ()
                  (FORMAT T "~D " PRESS-PAGE-NUMBER)
                  (WITH-PRESS-FONT (HEADER-FONT-NUMBER)
                    (PRESS-SET-CURSOR 0 DOVER-Y0)
                    (PRESS-STRING (FORMAT NIL "~A~10@T~A" FILE-NAME CREATION-DATE))
                    (PRESS-SET-CURSOR 15000. DOVER-Y0)
                    (PRESS-STRING (FORMAT NIL "Page ~D" PRESS-PAGE-NUMBER)))))
      (SETQ PRESS-END-PAGE-HOOK
            #'(LAMBDA () (FORMAT T "~D " PRESS-PAGE-NUMBER))))
    (FORMAT T "~&~A:  " FILE-NAME)
    (UNWIND-PROTECT
      (PROGN (PRESS-START-FILE HOST-ADDRESS)
             (AND PRESS-INTERPRET-XGP-ESCAPE
                 (PRESS-XGP-HEADER-PAGE INPUT-STREAM))
             (PRESS-START-PAGE)
             (COND (FONT-LIST
                    (SETQ USER-FONT-NUMBER-LIST
                          (MAPCAR 'PRESS-DEFINE-FONT-NAME FONT-LIST))
                    (PRESS-SELECT-FONT (CAR USER-FONT-NUMBER-LIST)))
                   (T
                    (PRESS-SELECT-FONT (IF PRESS-INTERPRET-XGP-ESCAPE 0
                                           (PRESS-DEFINE-FONT-NAME FONT)))))
             (SETQ HEADER-FONT-NUMBER
                   (IF HEADING-FONT
                       (PRESS-DEFINE-FONT-NAME HEADING-FONT)
                     (FIND-POSITION-IN-LIST PRESS-CURRENT-FONT PRESS-FONT-LIST)))
             (DO ((CH)
                  (FONT-STACK (MAKE-ARRAY 50. ':FILL-POINTER 0)))
                 ((NULL (SETQ CH (SEND INPUT-STREAM ':TYI))))
               (COND ((AND PRESS-INTERPRET-XGP-ESCAPE (= CH #o177))
                      (PRESS-XGP-ESCAPE INPUT-STREAM))
                     ((AND USER-FONT-NUMBER-LIST (= CH #/))
                      (SETQ CH (SEND INPUT-STREAM ':TYI))
                      (COND (( #/0 CH #/9)
                             (UNLESS (ARRAY-PUSH FONT-STACK PRESS-CURRENT-FONT)
                               (COPY-ARRAY-PORTION FONT-STACK 20. (ARRAY-LENGTH FONT-STACK)
                                                   FONT-STACK 0 (- (ARRAY-LENGTH FONT-STACK) 20.))
                               (DECF (FILL-POINTER FONT-STACK) 20.)
                               (ARRAY-PUSH FONT-STACK PRESS-CURRENT-FONT))
                             (PRESS-SELECT-FONT
                               (OR (NTH (- CH #/0) USER-FONT-NUMBER-LIST)
                                   (FIND-POSITION-IN-LIST PRESS-CURRENT-FONT
                                                          PRESS-FONT-LIST))))
                            ((= CH #/*)
                             (OR (ZEROP (FILL-POINTER FONT-STACK))
                                 (PRESS-SELECT-FONT
                                   (FIND-POSITION-IN-LIST (ARRAY-POP FONT-STACK)
                                                          PRESS-FONT-LIST))))
                            (T
                             (PRESS-CHAR CH))))
                     (T (PRESS-CHAR CH))))
             (PRESS-END-PAGE)
             (PRESS-END-FILE FILE-NAME CREATION-DATE COPIES
                             (COND ((AND FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST
                                         (NOT (EQUAL FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST
                                                     "")))
                                    (FORMAT NIL "~A (~A)" USER-ID
                                            FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST))
                                   (T USER-ID))))
      (AND PRESS-EFTP-STREAM
           (NOT (STRINGP PRESS-EFTP-STREAM))
           (SEND PRESS-EFTP-STREAM ':CLOSE ':ABORT)))))

(DEFUN DECODE-FONT-NAME (STRING &OPTIONAL (ERROR-P T) &AUX IDX1 IDX2 (IBASE 10.))
  (DECLARE (VALUES FAMILY FACE SIZE))
  (IF (LISTP STRING)
      (VALUES-LIST STRING)
    (OR (SETQ IDX1 (STRING-REVERSE-SEARCH-SET '(#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
                                              STRING))
        (IF ERROR-P (FERROR NIL "No point size in ~A" STRING) NIL))
    (SETQ IDX2 (1+ (STRING-REVERSE-SEARCH-NOT-SET '(#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
                                                  STRING IDX1)))
    (VALUES (SUBSTRING STRING 0 IDX2)
            (SUBSTRING STRING (1+ IDX1))
            (READ-FROM-STRING (SUBSTRING STRING IDX2 (1+ IDX1))))))

;;; XGP support
(DEFUN PRINT-XGP-FILE (FILE-NAME &REST OPTIONS)
  (WITH-OPEN-FILE (STREAM FILE-NAME ':DIRECTION ':INPUT ':SUPER-IMAGE T ':RAW T)
    (APPLY 'PRINT-FROM-STREAM
           (LET-CLOSED ((FILE-STREAM STREAM))
             #'XGP-FILE-STREAM)
           ':XGP T OPTIONS)))

(DEFVAR XGP-STREAM-RAW-P NIL)

;;; This extra level of stream is necessary, since sometimes we want character set conversion,
;;; as when reading text, and other times not, as when reading arguments.
(DEFUN XGP-FILE-STREAM (OP &REST ARGS)
  (DECLARE (SPECIAL FILE-STREAM))
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYI :TRUENAME :CREATION-DATE))
    (:TYI
     (IF XGP-STREAM-RAW-P (SEND FILE-STREAM ':TYI)
         (DO ((CH)) (NIL)
           (SETQ CH (SEND FILE-STREAM ':TYI))
           (SELECTQ CH
             (11 (RETURN #/TAB))
             (12 )
             (14 (RETURN #/FF))
             (15 (RETURN #/CR))
             (OTHERWISE (RETURN CH))))))
    (:LINE-IN (STREAM-DEFAULT-HANDLER #'XGP-FILE-STREAM OP (CAR ARGS) (CDR ARGS)))
    (OTHERWISE (APPLY FILE-STREAM OP ARGS))))

(DEFCONST XGP-DOTS-PER-INCH 200.)

(DEFMACRO XGP-TO-MICAS (X)
  "Convert coordinates from XGP dots to micas."
  `(TRUNCATE (* ,X 2540.) XGP-DOTS-PER-INCH))

(DEFUN PRESS-XGP-HEADER-PAGE (INPUT-STREAM &AUX LEFT-MARGIN TOP-MARGIN BOTTOM-MARGIN)
  (SETQ LEFT-MARGIN 2540.                       ;Closer to the xgp's values
        TOP-MARGIN (FLOOR (* 2540. 2) 3)
        BOTTOM-MARGIN TOP-MARGIN)
  (DO ((CH)) (NIL)
    (SELECTQ (SETQ CH (SEND INPUT-STREAM ':TYI))
      (#/FORM (RETURN NIL))
      (#/CR)
      (#/;
       (LET* ((LINE (SEND INPUT-STREAM ':LINE-IN))
              (IDX (STRING-SEARCH-CHAR #/SP LINE))
              (*READ-BASE* 10.))
         (SELECTOR (SUBSTRING LINE 0 (PROG1 IDX (AND IDX (INCF IDX)))) STRING-EQUAL
;          ("autcut")
           ("botmar"
            (SETQ BOTTOM-MARGIN (XGP-TO-MICAS (READ-FROM-STRING LINE NIL IDX))))
;          ("delete")
           ("dfont"
            (SETQ PRESS-XGP-FONT-LIST
                  (LOOP FOR I = IDX THEN (1+ J)
                        FOR J = (STRING-SEARCH-CHAR #/, LINE I)
                        COLLECT (MULTIPLE-VALUE-BIND (FAMILY FACE SIZE)
                                    (DECODE-FONT-NAME (SUBSTRING LINE I J))
                                  (PRESS-DEFINE-FONT FAMILY FACE SIZE 0))
                        WHILE J)))
           ("ffcut")
           ("lftmar"
            (SETQ LEFT-MARGIN (XGP-TO-MICAS (READ-FROM-STRING LINE NIL IDX))))
;          ("list")
           ("lsp"
            (SETQ PRESS-INTERLINE-SPACING (XGP-TO-MICAS (READ-FROM-STRING LINE NIL IDX))))
           ("rgtmar")
           ("skip")
           ("squish")
           ("topmar"
            (SETQ TOP-MARGIN (XGP-TO-MICAS (READ-FROM-STRING LINE NIL IDX))))
           ("vsp")
           (OTHERWISE (FERROR NIL "Unknown line ~A in XGP preamble" LINE)))))
      (OTHERWISE
       (FERROR NIL "Unknown character in XGP preamble ~C" CH))))
  (SETQ DOVER-X0 LEFT-MARGIN
        DOVER-Y1 (- (* 11. 2540.) TOP-MARGIN BOTTOM-MARGIN PRESS-INTERLINE-SPACING)
        DOVER-Y2 BOTTOM-MARGIN))

(DEFUN PRESS-XGP-ESCAPE (INPUT-STREAM &AUX (XGP-STREAM-RAW-P T) CH)
  (SELECTQ (SETQ CH (SEND INPUT-STREAM ':TYI))
    (1 (PRESS-XGP-ESCAPE-1 INPUT-STREAM))
    (2 (PRESS-XGP-ESCAPE-2 INPUT-STREAM))
    (3 (PRESS-XGP-ESCAPE-3 INPUT-STREAM))
    (4 (PRESS-XGP-ESCAPE-4 INPUT-STREAM))
    (OTHERWISE (PRESS-CHAR CH))))

(DEFUN PRESS-XGP-ESCAPE-1 (INPUT-STREAM &AUX CH)
  (SETQ CH (SEND INPUT-STREAM ':TYI))
  (IF (< CH #o20)
      (PRESS-SELECT-FONT (NTH CH PRESS-XGP-FONT-LIST))
    (SELECTQ CH
      (#o40 (PRESS-XGP-SET-COLUMN INPUT-STREAM))
      (#o41 (PRESS-XGP-UNDERSCORE INPUT-STREAM))
      (#o42 (PRESS-XGP-LINE-SPACE INPUT-STREAM))
      (#o43 (PRESS-XGP-BASELINE-ADJUST INPUT-STREAM))
      (#o44 (PRESS-XGP-PRINT-PAGE-NUMBER INPUT-STREAM))
      (#o45 (PRESS-XGP-SPECIFY-HEADING INPUT-STREAM))
      (#o46 (PRESS-XGP-START-UNDERSCORE INPUT-STREAM))
      (#o47 (PRESS-XGP-END-UNDERSCORE INPUT-STREAM))
      (#o50 (PRESS-XGP-SET-INTERCHAR-SPACING INPUT-STREAM))
      (#o51 (PRESS-XGP-END-SPECIFIED-WIDTH-UNDERSCORE INPUT-STREAM))
      (#o52 (PRESS-XGP-RELATIVE-BASELINE-ADJUST INPUT-STREAM))
      (#o53 (PRESS-XGP-RELATIVE-UNDERSCORE INPUT-STREAM))
      (OTHERWISE (FERROR NIL "Unknown XGP escape #o~O" CH)))))

;;; Sign extended version of above
(DEFUN PRESS-XGP-ONE-BYTE-ARG (INPUT-STREAM)
  (LET ((CH (SEND INPUT-STREAM ':TYI)))
    (IF (BIT-TEST 100 CH) (- #o200 CH) CH)))

(DEFUN PRESS-XGP-TWO-BYTE-ARG (INPUT-STREAM)
  (DPB (SEND INPUT-STREAM ':TYI) #o0707 (SEND INPUT-STREAM ':TYI)))

(DEFUN PRESS-XGP-THREE-BYTE-ARG (INPUT-STREAM)
  (DPB (SEND INPUT-STREAM ':TYI) #o1607
       (DPB (SEND INPUT-STREAM ':TYI) #o0707 (SEND INPUT-STREAM ':TYI))))

(DEFUN PRESS-XGP-SET-COLUMN (INPUT-STREAM)
  (PRESS-SET-CURSOR (XGP-TO-MICAS (PRESS-XGP-TWO-BYTE-ARG INPUT-STREAM)) PRESS-Y))

(DEFUN PRESS-XGP-UNDERSCORE (INPUT-STREAM)
  (PRESS-XGP-DO-UNDERSCORE (- PRESS-Y (XGP-TO-MICAS (PRESS-XGP-ONE-BYTE-ARG INPUT-STREAM)))
                           PRESS-X (PRESS-XGP-TWO-BYTE-ARG INPUT-STREAM) 2.))

(DEFUN PRESS-XGP-LINE-SPACE (INPUT-STREAM)
  (SETQ PRESS-INTERCHAR-SPACING NIL)
  (PRESS-SET-CURSOR PRESS-X (- PRESS-Y (XGP-TO-MICAS (SEND INPUT-STREAM ':TYI)))))

(DEFUN PRESS-XGP-BASELINE-ADJUST (INPUT-STREAM)
  (SETQ PRESS-INTERCHAR-SPACING NIL)
  (PRESS-SET-CURSOR
    (SETQ PRESS-BASELINE-Y (+ PRESS-Y (XGP-TO-MICAS (PRESS-XGP-ONE-BYTE-ARG INPUT-STREAM))))
    PRESS-X))

(DEFUN PRESS-XGP-PRINT-PAGE-NUMBER (IGNORE)
  (PRESS-STRING (FORMAT NIL "~D" PRESS-PAGE-NUMBER)))

(DEFUN PRESS-XGP-SPECIFY-HEADING (INPUT-STREAM)
  (LET* ((LENGTH (SEND INPUT-STREAM ':TYI))
         (STRING (MAKE-ARRAY LENGTH ':TYPE 'ART-STRING)))
    (LOOP FOR I FROM 0 BELOW LENGTH
          DO (ASET (SEND INPUT-STREAM ':TYI) STRING I))
    (SETQ PRESS-END-PAGE-HOOK (LET-CLOSED ((STRING STRING))
                                #'(LAMBDA ()
                                    (FORMAT T "~D " PRESS-PAGE-NUMBER)
                                    (WITH-INPUT-FROM-STRING (INPUT-STREAM STRING)
                                      (DO ((CH))
                                          ((NULL (SETQ CH (SEND INPUT-STREAM ':TYI))))
                                        (IF (AND PRESS-INTERPRET-XGP-ESCAPE (= CH #o177))
                                            (PRESS-XGP-ESCAPE INPUT-STREAM)
                                          (PRESS-CHAR CH)))))))))

(DEFUN PRESS-XGP-START-UNDERSCORE (IGNORE)
  (SETQ PRESS-XGP-UNDERLINE-START-X PRESS-X))

(DEFUN PRESS-XGP-END-UNDERSCORE (INPUT-STREAM)
  (PRESS-XGP-DO-UNDERSCORE (- PRESS-BASELINE-Y
                              (XGP-TO-MICAS (PRESS-XGP-ONE-BYTE-ARG INPUT-STREAM)))
                           PRESS-XGP-UNDERLINE-START-X
                           PRESS-X 2.))

(DEFUN PRESS-XGP-SET-INTERCHAR-SPACING (INPUT-STREAM)
  (SETQ PRESS-INTERCHAR-SPACING (XGP-TO-MICAS (SEND INPUT-STREAM ':TYI))))

(DEFUN PRESS-XGP-END-SPECIFIED-WIDTH-UNDERSCORE (INPUT-STREAM)
  (LET ((WIDTH (SEND INPUT-STREAM ':TYI)))
    (PRESS-XGP-DO-UNDERSCORE (- PRESS-BASELINE-Y
                                (XGP-TO-MICAS (PRESS-XGP-ONE-BYTE-ARG INPUT-STREAM)))
                             PRESS-XGP-UNDERLINE-START-X
                             PRESS-X WIDTH)))

(DEFUN PRESS-XGP-RELATIVE-BASELINE-ADJUST (INPUT-STREAM)
  (PRESS-SET-CURSOR
    (SETQ PRESS-BASELINE-Y (+ PRESS-BASELINE-Y
                              (XGP-TO-MICAS (PRESS-XGP-ONE-BYTE-ARG INPUT-STREAM))))
    PRESS-X))

(DEFUN PRESS-XGP-RELATIVE-UNDERSCORE (INPUT-STREAM)
  (PRESS-XGP-DO-UNDERSCORE (- PRESS-BASELINE-Y
                              (XGP-TO-MICAS (PRESS-XGP-ONE-BYTE-ARG INPUT-STREAM)))
                           PRESS-X (PRESS-XGP-TWO-BYTE-ARG INPUT-STREAM) 2.))

(DEFUN PRESS-XGP-DO-UNDERSCORE (TOP-Y X-START X-END THICKNESS &AUX (OX PRESS-X) (OY PRESS-Y))
  (PRESS-SET-CURSOR X-START (- TOP-Y THICKNESS))
  (PRESS-ENTITY-BYTE <SHOW-RECTANGLE>)
  (PRESS-ENTITY-WORD (- X-END X-START))         ;Width
  (PRESS-ENTITY-WORD (XGP-TO-MICAS THICKNESS))  ;Thickness
  (PRESS-SET-CURSOR OX OY))

(DEFUN PRESS-XGP-ESCAPE-2 (INPUT-STREAM)
  (PRESS-SET-CURSOR (+ PRESS-X (XGP-TO-MICAS (PRESS-XGP-ONE-BYTE-ARG INPUT-STREAM)))
                    PRESS-Y))

(DEFUN PRESS-XGP-ESCAPE-3 (IGNORE)
  (FERROR NIL "XGP escape 3 not implemented"))

(DEFUN PRESS-XGP-ESCAPE-4 (IGNORE)
  (FERROR NIL "XGP escape 4 not implemented"))

;;;; Font sampling

;;; Each element in font-list is (family-name face-name point-size rotation)
;;;  rotation is optional and defaults to 0
(DEFUN SAMPLE-FONTS (FONT-LIST &OPTIONAL (UPPER-HALF NIL)
                                         (HOST-ADDRESS DOVER-ADDRESS)
                               &AUX FOO CH)
  (BIND-PRESS-VARIABLES
    (PRESS-START-FILE HOST-ADDRESS)
    (LET ((LABEL-FONT (PRESS-DEFINE-FONT "TIMESROMAN" "" 10. 0))
          THIS-FONT NOT-IN-FONTS-WIDTHS)
      (DO ((L FONT-LIST (CDR L))
           (FONT) (ROTATION)
           (I 1 (1+ I)))
          ((NULL L)
           (PRESS-END-FILE "Font samples" ""))
        (SETQ FONT (CAR L))
        (WHEN (= I 16.)                         ;Got to make a new file
          (PRESS-END-FILE "Font samples" "")
          (RETURN (SAMPLE-FONTS L UPPER-HALF)))
        (PRESS-START-PAGE)
        (MULTIPLE-VALUE (THIS-FONT NOT-IN-FONTS-WIDTHS)
          (PRESS-DEFINE-FONT-FAKE (CAR FONT) (CADR FONT) (CADDR FONT)
                                  (SETQ ROTATION (OR (CADDDR FONT) 0))))
        (PRESS-SET-CURSOR 6500. 25400.)
        (PRESS-SELECT-FONT LABEL-FONT)
        (PRESS-STRING
          (FORMAT NIL "Font ~A, ~:[Face ~A, ~;~*~]Point size ~D~:[, rotated ~D degrees~;~*~]~:[~; (not in Fonts.Widths)~]"
                  (CAR FONT) (STRING-EQUAL (CADR FONT) "") (CADR FONT)
                  (CADDR FONT) (ZEROP ROTATION) (TRUNCATE ROTATION 60.)
                  NOT-IN-FONTS-WIDTHS))
        (DOTIMES (COL #o10)
          (DOTIMES (ROW #o20)
            (PRESS-SET-CURSOR (+ (* COL 2200.) 300.) (- (* 10. 2540.) 1250. (* ROW 1000.)))
            (PRESS-SELECT-FONT LABEL-FONT)
            (SETQ CH (+ (IF UPPER-HALF #o200 0) (* COL #o20) ROW))
            (DO ((PPSS #o0603 (- PPSS #o0300))) ((MINUSP PPSS))
              (PRESS-CHAR (+ (LDB PPSS CH) #/0)))
            (PRESS-STRING "   ")
            (PRESS-SELECT-FONT THIS-FONT)
            ;; See if char defined in font
            (COND ((MINUSP (AREF (SEVENTH PRESS-CURRENT-FONT) CH))
                   (PRESS-SELECT-FONT LABEL-FONT)
                   (PRESS-STRING "und"))
                  (T (SEND PRESS-EFTP-STREAM ':TYO CH)
                     (INCF PRESS-N-CHARS)
                     (INCF PRESS-PENDING-CHARS)))))
        ;; 8150. next
        (PRESS-SELECT-FONT THIS-FONT)
        (PRESS-SET-CURSOR 0 8000.)
        (PRESS-CHAR-SEQ #/A #/Z #/CR)
        (PRESS-CHAR-SEQ #/a #/z #/CR)
        (PRESS-CHAR-SEQ #/0 #/9 #/CR)
        (PRESS-CHAR-SEQ #/! #/?)
        (PRESS-CHAR-SEQ #/[ #/_)
        (PRESS-CHAR-SEQ #/{ #/ #/CR)
        (PRESS-CHAR-SEQ #/  #/)
        (PRESS-SET-CURSOR 0 4150.)
        (PRESS-STRING "/"The time has come,/" the Walrus said,
   /"To talk of many things;
Of shoes, and ships, and sealing wax,
")
        (SETQ FOO PRESS-Y)
        (PRESS-STRING " Of cabbages and kings,
And why the sea is boiling hot,
   And whether pigs have wings./"")
        (PRESS-SET-CURSOR 8750. FOO)
        (PRESS-STRING "(DEFUN APPEND (X Y)
")
        (PRESS-SET-CURSOR 8750. PRESS-Y)
        (PRESS-STRING "     (COND ((NULL X) Y)
")
        (PRESS-SET-CURSOR 8750. PRESS-Y)
        (PRESS-STRING "           (T (CONS (CAR X) (APPEND (CDR X) Y)))))")
        (PRESS-END-PAGE)))))

(DEFUN PRESS-CHAR-SEQ (FIRST LAST &OPTIONAL EXTRA)
  (DO ((CH FIRST (1+ CH))) ((> CH LAST))
    (OR (MINUSP (AREF (SEVENTH PRESS-CURRENT-FONT) CH)) (PRESS-CHAR CH)))
  (AND EXTRA (PRESS-CHAR EXTRA)))

;;; This one is driven off of the widths file, assumed to be already loaded
(DEFUN SAMPLE-ALL-FONTS (&AUX FONT-LIST NAME FACE POINTS ROT)
  "Print samples of all known Dover fonts (except TEX fonts, rotated and vector fonts)."
  (DOLIST (F FONT-WIDTH-DATA)
    (SETQ NAME (CAR F) FACE (CADR F) POINTS (CADDR F) ROT (CADDDR F))
    (IF (PLUSP POINTS)
        (PUSH (LIST NAME FACE (TRUNCATE (* 10. POINTS) 352.) ROT) FONT-LIST)
      (FORMAT T "~&Type list of point sizes for ~A~Arot~D: "
              NAME FACE (TRUNCATE ROT 60.))
      (DOLIST (POINTS (READ))
        (PUSH (LIST NAME FACE POINTS ROT) FONT-LIST))))
  (SAMPLE-FONTS FONT-LIST))

;;;; List of all normal fonts on MIT Dover as of 5/16/82
;;; This does not include TEX fonts, rotated fonts, or vector fonts
;;; Updated by DULCEY 5/16/82.

(SETQ ALL-DOVER-FONTS '(

  (APL || 8.) (APL || 10.)

  (ARROWS || 10.)

  (BOX || 10.)

  (CMU || 10.)

  (CREAM || 10.) (CREAM || 12.)
  (CREAM B 10.) (CREAM B 12.)
  (CREAM I 10.) (CREAM I 12.)
  (CREAM BI 10.) (CREAM BI 12.)

  (DOTS || 7.)

  (ELITE || 10.)

  (GACHA || 5.) (GACHA || 6.) (GACHA || 7.)
  (GACHA || 8.) (GACHA || 9.) (GACHA || 10.)
  (GACHA || 12.)

  (GACHA B 5.) (GACHA B 6.) (GACHA B 7.)
  (GACHA B 8.) (GACHA B 9.) (GACHA B 10.)
  (GACHA B 12.)

  (GACHA I 5.) (GACHA I 6.) (GACHA I 7.)
  (GACHA I 8.) (GACHA I 9.) (GACHA I 10.)
  (GACHA I 12.)

  (GACHA BI 5.) (GACHA BI 6.) (GACHA BI 7.)
  (GACHA BI 8.) (GACHA BI 9.) (GACHA BI 10.)
  (GACHA BI 12.)

  (GATES || 10.) (GATES || 12.) (GATES || 18.)
  (GATES || 32.)

  (HELVETICA || 3.) (HELVETICA || 4.) (HELVETICA || 5.)
  (HELVETICA || 6.) (HELVETICA || 7.) (HELVETICA || 8.)
  (HELVETICA || 9.) (HELVETICA || 10.) (HELVETICA || 11.)
  (HELVETICA || 12.) (HELVETICA || 14.) (HELVETICA || 18.)

  (HELVETICA B 6.) (HELVETICA B 7.) (HELVETICA B 8.)
  (HELVETICA B 9.) (HELVETICA B 10.) (HELVETICA B 11.)
  (HELVETICA B 12.) (HELVETICA B 14.) (HELVETICA B 18.)

  (HELVETICA I 6.) (HELVETICA I 7.) (HELVETICA I 8.)
  (HELVETICA I 9.) (HELVETICA I 10.) (HELVETICA I 11.)
  (HELVETICA I 12.) (HELVETICA I 14.) (HELVETICA I 18.)

  (HELVETICA BI 6.) (HELVETICA BI 7.) (HELVETICA BI 8.)
  (HELVETICA BI 9.) (HELVETICA BI 10.) (HELVETICA BI 11.)
  (HELVETICA BI 12.) (HELVETICA BI 14.) (HELVETICA BI 18.)

  (HELVETICAD || 24.) (HELVETICAD || 30.) (HELVETICAD || 36.)

  (HELVETICAMIT || 10.)

  (HELVETICASC || 9.) (HELVETICASC || 10.)
  (HELVETICASC B 9.) (HELVETICASC B 10.)

  (HIPPO || 6.) (HIPPO || 8.) (HIPPO || 10.)
  (HIPPO || 12.) (HIPPO || 14.) (HIPPO || 18.)

  (LPT || 6.) (LPT || 8.) (LPT || 10.)
  (LPT B 10.)

  (MATH || 6.) (MATH || 8.) (MATH || 10.)
  (MATH || 12.) (MATH || 14.) (MATH || 18.)

  (OLDENGLISH || 10.) (OLDENGLISH || 12.) (OLDENGLISH || 18.)
  (OLDENGLISH || 24.) (OLDENGLISH || 36.) (OLDENGLISH || 48.)

  (SAIL || 6.) (SAIL || 8.) (SAIL || 10.)

  (SIGMA || 20.)

  (SYMBOL || 10.)

  (TEMPLATE || 10.) (TEMPLATE || 12.) (TEMPLATE || 18.)
  (TEMPLATE || 64.)

  (TIMESROMAN || 4.) (TIMESROMAN || 6.) (TIMESROMAN || 7.)
  (TIMESROMAN || 8.) (TIMESROMAN || 9.) (TIMESROMAN || 10.)
  (TIMESROMAN || 11.) (TIMESROMAN || 12.) (TIMESROMAN || 14.)
  (TIMESROMAN || 17.) (TIMESROMAN || 18.)

  (TIMESROMAN B 6.) (TIMESROMAN B 7.)
  (TIMESROMAN B 8.) (TIMESROMAN B 9.) (TIMESROMAN B 10.)
  (TIMESROMAN B 11.) (TIMESROMAN B 12.) (TIMESROMAN B 14.)
  (TIMESROMAN B 17.) (TIMESROMAN B 18.)

  (TIMESROMAN I 6.) (TIMESROMAN I 7.)
  (TIMESROMAN I 8.) (TIMESROMAN I 9.) (TIMESROMAN I 10.)
  (TIMESROMAN I 11.) (TIMESROMAN I 12.) (TIMESROMAN I 14.)
  (TIMESROMAN I 17.) (TIMESROMAN I 18.)

  (TIMESROMAN BI 6.) (TIMESROMAN BI 7.)
  (TIMESROMAN BI 8.) (TIMESROMAN BI 9.) (TIMESROMAN BI 10.)
  (TIMESROMAN BI 11.) (TIMESROMAN BI 12.) (TIMESROMAN BI 14.)
  (TIMESROMAN BI 17.)

  (TIMESROMAND || 24.) (TIMESROMAND || 30.) (TIMESROMAND || 36.)

  (TIMESROMANMIT || 10.)

  (TIMESROMANSC || 9.) (TIMESROMANSC || 10.)

  ))

;;; Read in all fonts from FONTS;DOVER FONTS
(DEFUN COMPUTE-DOVER-FONTS (&AUX NAME FACE POINT ROT TEM ANSWER (*READ-BASE* 10.))
  (WITH-OPEN-FILE (I "AI: FONTS; DOVER FONTS")
    (DO ((LINE) (EOF)) (NIL)
      (MULTIPLE-VALUE (LINE EOF)
        (SEND I ':LINE-IN))
      (AND EOF
           (OR (NULL LINE) (EQUAL LINE ""))
           (RETURN (NREVERSE ANSWER)))
      (SETQ TEM (STRING-SEARCH-CHAR #/, LINE)
            NAME (INTERN (SUBSTRING LINE 7 TEM) "PRESS")
            TEM (+ TEM 2)
            FACE (COND ((STRING-EQUAL LINE "MR" ':START1 TEM ':END1 (+ TEM 2) ':END2 2)
                        '#.(INTERN ""))
                       ((STRING-EQUAL LINE "MI" ':START1 TEM ':END1 (+ TEM 2) ':END2 2) 'I)
                       ((STRING-EQUAL LINE "BR" ':START1 TEM ':END1 (+ TEM 2) ':END2 2) 'B)
                       ((STRING-EQUAL LINE "BI" ':START1 TEM ':END1 (+ TEM 2) ':END2 2) 'BI)
                       (T (FERROR NIL "Parsing error in fonts file.")))
            TEM (1+ (STRING-SEARCH-CHAR #/( LINE TEM))
            POINT (READ-FROM-STRING LINE 'SI:NO-EOF-OPTION TEM)
            TEM (+ (STRING-SEARCH-CHAR #/: LINE TEM) 2)
            ROT (READ-FROM-STRING LINE 'SI:NO-EOF-OPTION TEM))
      (PUSH (LIST NAME FACE POINT ROT) ANSWER))))


#| ;hacks

(DEFUN SPIRAL-HACK ()
  (PRESS-START-FILE DOVER-ADDRESS)
  (PRESS-START-PAGE)
  (PRESS-SELECT-FONT (PRESS-DEFINE-FONT "timesroman" "" 10. 0))
  (DO ((X 8000.) (Y 8000.) (ANG 0 (+ ANG ANGINC)) (ANGINC 0.3)
       (LEN 200. (+ LEN LENINC)) (LENINC 50.) (MAXLEN 1000.))
      ((> LEN MAXLEN))
    (PRESS-LINE X Y (SETQ X (+ X (FIX (* (SIN ANG) LEN))))
                    (SETQ Y (+ Y (FIX (* (COS ANG) LEN))))))
  (PRESS-END-PAGE)
  (PRESS-END-FILE "Spiral" ""))

(DEFUN FOO (POLY ADDRESS)
  (PRESS-START-FILE ADDRESS)
  (PRESS-START-PAGE)
  (PRESS-SELECT-FONT (PRESS-DEFINE-FONT "timesroman" "" 10. 0))
  (DO ((X (CAAR POLY))
       (Y (CADAR POLY))
       (L (CDR POLY) (CDR L)))
      ((NULL L))
    (PRESS-LINE X Y (SETQ X (CAAR L)) (SETQ Y (CADAR L))))
  (PRESS-END-PAGE)
  (PRESS-END-FILE "Lines" ""))
|#;comment

;;;; Interface to the geneic hardcopy functions.

(DEFUN (:DOVER SI:PRINT-STREAM) (PRINTER INPUT-STREAM &REST OPTIONS)
  PRINTER
  (APPLY 'PRINT-FROM-STREAM INPUT-STREAM OPTIONS))

(DEFUN (:DOVER SI:PRINT-FILE) (PRINTER FILENAME &REST OPTIONS
                                  &KEY &OPTIONAL FORMAT &ALLOW-OTHER-KEYS)
  PRINTER
  (DO () ((MEMQ FORMAT '(:TEXT :PRESS :XGP :SUDS-PLOT)))
    (SETQ FORMAT
          #+MIT
          (CERROR T NIL NIL
                  "Invalid format keyword ~S for printing a file on the Dover."
                  FORMAT)
          #+3600
          (FERROR "Invalid format keyword ~S for printing a file on the Dover."
                  FORMAT)))
  (SELECTQ FORMAT
    (:PRESS (APPLY 'PRINT-PRESS-FILE FILENAME OPTIONS))
    (:XGP (APPLY 'PRINT-XGP-FILE FILENAME OPTIONS))
    (:SUDS-PLOT
     (AND (NOT (FBOUNDP 'DPLT-PRINT-FILE))
          (LOAD "SYS: IO1; DPLT"))
     (FUNCALL 'DPLT-PRINT-FILE FILENAME))
    (:TEXT (APPLY 'PRINT-FILE FILENAME OPTIONS))))

(DEFUN (:DOVER SI:PRINT-STATUS) (PRINTER STANDARD-OUTPUT)
  PRINTER
  (PRINT-DOVER-STATUS)
  (PRINT-DOVER-QUEUE))

(DEFUN (:PRESS-FILE SI:PRINT-STREAM) (PRINTER INPUT-STREAM &REST OPTIONS)
  (APPLY 'PRINT-FROM-STREAM INPUT-STREAM ':OUTPUT-FILE (CADR PRINTER) OPTIONS))

(DEFUN (:PRESS-FILE SI:PRINT-FILE) (PRINTER FILENAME &REST OPTIONS
                                    &KEY &OPTIONAL FORMAT &ALLOW-OTHER-KEYS)
  (DO () ((MEMQ FORMAT '(:TEXT :PRESS :XGP :SUDS-PLOT)))
    (SETQ FORMAT
          #+MIT
          (CERROR T NIL NIL
                  "Invalid format keyword ~S for printing a file into a press file."
                  FORMAT)
          #+3600
          (FERROR "Invalid format keyword ~S for printing a file into a press file."
                  FORMAT)))
  (SETQ OPTIONS (LIST* ':OUTPUT-FILE (CADR PRINTER) OPTIONS))
  (SELECTQ FORMAT
    (:PRESS (APPLY 'PRINT-PRESS-FILE FILENAME OPTIONS))
    (:XGP (APPLY 'PRINT-XGP-FILE FILENAME OPTIONS))
    (:SUDS-PLOT
     (AND (NOT (FBOUNDP 'DPLT-PRINT-FILE))
          (LOAD "SYS: IO1; DPLT"))
     (FUNCALL 'DPLT-PRINT-FILE FILENAME ':FILE (CADR PRINTER)))
    (:TEXT (APPLY 'PRINT-FILE FILENAME OPTIONS))))
