;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Written 1/06/85 14:55:08 by pace,
;;; Reason: avoid bombout in (:property fixnum type-optimizer)  there are lots of other bugs like this. --rg
;;; Reason:
;;; Reason: foo
;;; Reason: foo
;;; Reason: Really fix zwei:blink-matching-paren.  The subtle differences between character-objects and fixnums representing
;;; characters should no longer confuse it.
;;; Reason: Fix to zwei:blink-matching-paren to help it cope with bogusly fontified files
;;; Reason: with-selection-substitute bug
;;; while running on LMI Explorer 1 from band 3
;;; with Experimental FORD 1.26, Experimental Local-File 2.3, Experimental FILE-Server 10.0, Experimental Universal Command Loop 5.26, Experimental UCL System Interfaces 2.5, Experimental Glossary 1.10, Experimental Suggestions 5.0, Experimental Austin Generated Patches 9.24, Experimental ucl-utilities 3.9, Experimental Explorer Streamer Tape 5.0, Experimental ZMail 1.0, microcode 162, LMI Site -dg.



; From file TVDEFS.LISP#> WINDOW; EXP1:
;#8R TV#:
;(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))
;  (COMPILER#:PATCH-SOURCE-FILE "SYS: WINDOW; TVDEFS  "

;(DEFMACRO WITH-SELECTION-SUBSTITUTE ((WINDOW FOR-WINDOW) &BODY BODY)
;  "Execute BODY with WINDOW as a selection substitute for
;FOR-WINDOW.  While FOR-WINDOW has the selection substitute, the
;substitute will be selected whenever FOR-WINDOW would have been
;selected."
;  `(LET* ((.WINDOW. ,WINDOW)
;         (.FOR-WINDOW. ,FOR-WINDOW)
;         (.GO-AHEAD-AND-CHANGE-THE-SUBSTITUTE.
;           ;; If both windows go to the same window then don't bother changing
;           ;; the selection substitute at all.  This does the right thing when
;           ;; the caller goofs up and has a looping selection-substitute, by
;           ;; not doing the selection substitute at all.
;           (or (null .window.)
;               (NEQ
;                 (SEND .FOR-WINDOW. ':ULTIMATE-SELECTION-SUBSTITUTE)
;                 (SEND     .WINDOW. ':ULTIMATE-SELECTION-SUBSTITUTE))))
;         (.OSTATUS. (AND .WINDOW. (SEND .WINDOW. ':STATUS)))
;         (.OSUBST. (SEND .FOR-WINDOW. ':SELECTION-SUBSTITUTE)))
;     (IF .GO-AHEAD-AND-CHANGE-THE-SUBSTITUTE.
;        (SEND .FOR-WINDOW. ':SET-SELECTION-SUBSTITUTE .WINDOW.))
;     (UNWIND-PROTECT
;       (PROGN . ,BODY)
;       (IF .GO-AHEAD-AND-CHANGE-THE-SUBSTITUTE.
;          (DELAYING-SCREEN-MANAGEMENT
;            (SEND .FOR-WINDOW. ':SET-SELECTION-SUBSTITUTE .OSUBST.)
;            (IF .WINDOW. (SEND .WINDOW. ':SET-STATUS .OSTATUS.)))))))

;))

(deff zwei:zwei-search 'zwei:search)


; From file DISPLA.LISP#> QL.ZWEI; LAM3:
#8R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "LAM3: QL.ZWEI; DISPLA.#"

(DEFUN BLINK-MATCHING-PAREN (BLINKER WINDOW POINT WINDOW-START-BP
                             &AUX BP X Y OPEN CLOSE)
  (COND ((AND (= (LIST-SYNTAX (SETQ CLOSE (BP-CHARACTER-BEFORE POINT))) LIST-CLOSE)
              *FLASH-MATCHING-PAREN*
              ;; Don't waste time if start of defun is very very far.
              (< (COUNT-LINES (FORWARD-DEFUN POINT -1 T) POINT T)
                 *FLASH-MATCHING-PAREN-MAX-LINES*)
              (SETQ BP (FORWARD-SEXP POINT -1 NIL 0 WINDOW-START-BP NIL))
              (MULTIPLE-VALUE-BIND (NIL SLASHIFIED COMMENT)
                  (LISP-BP-SYNTACTIC-CONTEXT (FORWARD-CHAR POINT -1 T) BP)
                (AND (NOT SLASHIFIED) (NOT COMMENT))))
         (SETQ OPEN (MAKE-CHAR (BP-CH-CHAR BP)))
         ;; checks paren type match if open paren on screen
         (AND (not (char-equal (SECOND (ASS #'char-equal OPEN *MATCHING-DELIMITER-LIST*))
                               (MAKE-CHAR CLOSE 0 0)))
              ;; this is so we don't barf, the redisplay, then barf, then redisplay, then...
              (NOT (TV:SHEET-ME-OR-MY-KID-P *WINDOW* *MODE-LINE-WINDOW*))
              (PROGN (BEEP) (FORMAT *QUERY-IO* "Non-matching parenthesis.")))
         ;; NOW move back over singlequotes.
         (SETQ BP (BACKWARD-LEADING-SINGLE-QUOTES BP WINDOW-START-BP))
         (SETQ OPEN (BP-CHAR BP))
         (COND ((MULTIPLE-VALUE-SETQ (X Y) (FIND-BP-IN-WINDOW-COORDS BP WINDOW))
                (LET* ((SHEET (WINDOW-SHEET WINDOW))
                       (FONT-MAP (TV:SHEET-FONT-MAP SHEET))
                       FONT LKT)
                  (IF (< (CHAR-FONT OPEN) (LENGTH FONT-MAP))
                      (SETQ FONT (AREF FONT-MAP (CHAR-FONT OPEN)))
                    (RETURN-FROM BLINK-MATCHING-PAREN NIL))
                  (SETQ LKT (TV:FONT-LEFT-KERN-TABLE FONT))
                  (AND LKT (SETQ X (- X (AREF LKT (CHAR-CODE OPEN)))))
                  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
                  (WITHOUT-INTERRUPTS
                    (SEND BLINKER :SET-CHARACTER (CHAR-CODE OPEN) FONT)
                    (SEND BLINKER :SET-CURSORPOS X Y)
                    (SEND BLINKER :SET-VISIBILITY ':BLINK))
                  T))))
        ((AND (= (LIST-SYNTAX (SETQ OPEN (BP-CH-CHAR POINT))) LIST-OPEN)
              *FLASH-MATCHING-PAREN*
              ;; Don't waste time if start of defun is very very far.
             (< (COUNT-LINES (FORWARD-DEFUN POINT -1 T) POINT T)
                 *FLASH-MATCHING-PAREN-MAX-LINES*)
              (MULTIPLE-VALUE-BIND (NIL SLASHIFIED COMMENT)
                  (LISP-BP-SYNTACTIC-CONTEXT POINT BP)
                (AND (NOT SLASHIFIED) (NOT COMMENT)))
              (SETQ BP (FORWARD-SEXP POINT 1 NIL 0
                                     ;; Don't look past just below the bottom of the screen.
                                     (LET ((END-LINE
                                             (PLINE-LINE *WINDOW*
                                                         (1- (WINDOW-N-PLINES *WINDOW*)))))
                                       (AND END-LINE
                                            (LINE-NEXT END-LINE)
                                            (CREATE-BP (LINE-NEXT END-LINE) 0)))
                                     NIL)))
         (SETQ CLOSE (BP-CHARACTER-BEFORE BP))
         ;; checks paren type match if open paren on screen
         (AND (not (char-equal (SECOND (ASS #'char-equal (MAKE-CHAR OPEN) *MATCHING-DELIMITER-LIST*))
                               (MAKE-CHAR CLOSE)))
                   ;; see above for reason for this test
              (NOT (TV:SHEET-ME-OR-MY-KID-P *WINDOW* *MODE-LINE-WINDOW*))
              (PROGN (BEEP) (FORMAT *QUERY-IO* "Non-matching parenthesis.")))
         ;; Now move past trailing singlequote-like characters.
         (DO ()
             (( (LIST-SYNTAX (BP-CHAR BP))) LIST-SINGLE-QUOTE)
           (IBP BP))
         (SETQ CLOSE (BP-CHARACTER-BEFORE BP))
         (COND ((PROGN (MULTIPLE-VALUE (X Y) (FIND-BP-IN-WINDOW-COORDS (DBP BP) WINDOW)) X)
                (LET* ((SHEET (WINDOW-SHEET WINDOW))
                       (FONT-MAP (TV:SHEET-FONT-MAP SHEET))
                       FONT LKT)
                  (IF (< (char-font close) (length font-map))
                      (SETQ FONT (AREF FONT-MAP (CHAR-FONT CLOSE)))
                    (RETURN-FROM BLINK-MATCHING-PAREN NIL))
                  (SETQ LKT (TV:FONT-LEFT-KERN-TABLE FONT))
                  (AND LKT (SETQ X (- X (AREF LKT (CHAR-CODE CLOSE)))))
                  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
                  (WITHOUT-INTERRUPTS
                    (SEND BLINKER :SET-CHARACTER (CHAR-CODE CLOSE) FONT)
                    (SEND BLINKER :SET-CURSORPOS X Y)
                    (SEND BLINKER :SET-VISIBILITY ':BLINK))
                  T))))
        (T (TV:BLINKER-SET-VISIBILITY BLINKER NIL))))

))

; From file EXP-LAM-KBD.LISP#> NAHA; EXP1:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "EXP1: NAHA; EXP-LAM-KBD.#"

(defvar *use-lambda-kbd* nil "convert raw keycodes for lambda kweyboard rather that Explorer keyboard")

))

; From file EXP-LAM-KBD.LISP#> NAHA; EXP1:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "EXP1: NAHA; EXP-LAM-KBD.#"

(tv:add-to-system-menu-programs-column "lambda kbd" '(setq si:*use-lambda-kbd* t) "lambda kbd")
(tv:add-to-system-menu-programs-column "explorer kbd" '(setq si:*use-lambda-kbd* nil) "explorer kbd")

(DEFUN KBD-CONVERT-TO-SOFTWARE-CHAR
       (HARD-CHAR &OPTIONAL (UPCASE-CONTROL-CHARS T)    ;
        &AUX ASC SHIFT BUCKY)
  "Convert hardware character to software character, or NIL to ignore.
UPCASE-CONTROL-CHARS if NIL means leave Meta-lowercase-letter, etc.,
not converted to upper case."
  (SELECTQ (LDB 2003 HARD-CHAR)                 ;Source ID
    (1 (KBD-CONVERT-NEW HARD-CHAR UPCASE-CONTROL-CHARS))                ;New keyboard
    (2
                                                ;those cretins at TI didn't use a new keyboard type for their keyboard!
     (COND (*use-lambda-kbd*
            (KBD-CONVERT-NEWER HARD-CHAR UPCASE-CONTROL-CHARS)) ;8 bit serial keyboard
           ((= PROCESSOR-TYPE-CODE CHAPARRAL-TYPE-CODE)
            ;; TI 8-bit serial keyboard
            (KBD-CONVERT-TI HARD-CHAR UPCASE-CONTROL-CHARS))
           (T (KBD-CONVERT-NEWER HARD-CHAR UPCASE-CONTROL-CHARS)        ;8 bit serial keyboard
              )))
    (6 (SET-MOUSE-MODE 'VIA-KBD)                ;Mouse via keyboard - turn on remote mouse
       NIL)                                     ; enable bit in IOB
    (7                                          ;Old keyboard
      (SETQ SHIFT (COND ((BIT-TEST 1400 HARD-CHAR) 2)   ;TOP
                        ((BIT-TEST 300 HARD-CHAR) 1)    ;SHIFT
                        (T 0)))                         ;VANILLA
      (SETQ BUCKY (+ (IF (BIT-TEST 06000 HARD-CHAR) 0400 0)     ;CONTROL
                     (IF (BIT-TEST 30000 HARD-CHAR) 1000 0)))   ;META
      (SETQ ASC (AREF KBD-TRANSLATE-TABLE SHIFT (LOGAND 77 HARD-CHAR)))
      (AND (BIT-TEST 40000 HARD-CHAR)                   ;SHIFT LOCK
           (IF (AND SHIFT-LOCK-XORS (BIT-TEST 300 HARD-CHAR))
               (AND (>= ASC #/A) (<= ASC #/Z) (SETQ ASC (+ ASC 40)))
               (AND (>= ASC #/a) (<= ASC #/z) (SETQ ASC (- ASC 40)))))
      (AND (NOT (ZEROP BUCKY)) (>= ASC #/a) (<= ASC #/z)
           (SETQ ASC (- ASC 40)))               ;Control characters always uppercase
      (+ ASC BUCKY))))
))

; From file BLINK-PAREN.LISP#> NAHA; EXP1:
#10R ZWEI#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER#:PATCH-SOURCE-FILE "EXP1: NAHA; BLINK-PAREN.#"

(DEFUN BP-CHARACTER-BEFORE (BP)
  "Return the character before where BP points."
  (LET ((INDEX (BP-INDEX BP)))
    (IF (ZEROP INDEX)
        #/NEWLINE
        (CLI:AREF (BP-LINE BP) (1- INDEX)))))

(DEFUN BP-CHAR-BEFORE (BP)
  "Return CHAR-INT of the character before where BP points."
  (LET ((INDEX (BP-INDEX BP)))
    (IF (ZEROP INDEX)
        #/NEWLINE
        (GLOBAL:AREF (BP-LINE BP) (1- INDEX)))))

))


#8r si#:
(compiler-let ((package (pkg-find-package "SI")))
  (compiler#:patch-source-file "SYS: WINDOW; COLD.#"

(defun kbd-convert-newer (char &optional (ctl-chars-uppercase t))
  (setq char (logand 377 char));strip of source bits
  (cond ((bit-test 1_7 char);is it a second-byte?
         (cond ((bit-test 1_6 char);up or down code?
                (prog1
                 (multiple-value-bind (soft-char unshifted-soft-char)
                     (new-lookup saved-first-char)
                   (cond ((bit-test 1_15. soft-char)
                          (kbd-bit-15-on soft-char t)
                          nil)
                         (t      ;normal character
                          ;; set bitmap bit
                          (setf (aref kbd-key-state-array unshifted-soft-char) 1)
                          ;; A real key depression.  Check for caps-lock.
                          (let ((kbd-shifts (logior kbd-left-shifts kbd-right-shifts)))
                            ;; Hyper, Super, Meta, Control bits
                            (SETQ unshifted-soft-char (LDB 0404 KBD-SHIFTS))
                            (IF (AND CTL-CHARS-UPPERCASE
                                     (NOT (ZEROP unshifted-soft-char)))
                                (IF (<= #/a SOFT-CHAR #/z)
                                    (DECF SOFT-CHAR 40)      ;Control characters always uppercase,
                                  (IF (<= #/A SOFT-CHAR #/Z)   ;unless  Shift is typed
                                      (INCF SOFT-CHAR 40)))
                                ;; Except for control chars for which Shift is reversed,
                                ;; consider the shift-lock key.
                                (AND (BIT-TEST 10 KBD-SHIFTS)  ;Caps lock
                                     (IF (AND SHIFT-LOCK-XORS (BIT-TEST 1 KBD-SHIFTS))
                                         (AND (>= SOFT-CHAR #/A)
                                              (<= SOFT-CHAR #/Z)
                                              (SETQ SOFT-CHAR (+ SOFT-CHAR 40)))
                                       (AND (>= SOFT-CHAR #/a)
                                            (<= SOFT-CHAR #/z)
                                            (SETQ SOFT-CHAR (- SOFT-CHAR 40))))))
                            (%LOGDPB unshifted-soft-char %%KBD-CONTROL-META SOFT-CHAR)))))
                 (insure-down-bucky-bit-consistency char)))   ;key-down
               (t                    ;0: key up
                (multiple-value-bind (soft-char unshifted-soft-char)
                   (new-lookup saved-first-char)
                  (cond ((bit-test 1_15. soft-char)
                         (kbd-bit-15-on soft-char nil)
                         nil)
                        (t (setf (aref kbd-key-state-array unshifted-soft-char) 0)))
                  (insure-up-bucky-bit-consistency char)
                  nil))))
        (t (setq saved-first-char (ldb 0007 char));its a first-byte
           nil)))

;;; get the software char corresponding to hardware char and bucky bits
(defun new-lookup (char)
  (let ((kbd-shifts (logior kbd-left-shifts kbd-right-shifts)))
    (values (AREF KBD-NEW-TABLE
                  (COND ((BIT-TEST 2 KBD-SHIFTS)     ;Greek
                         (+ (LOGAND 1 KBD-SHIFTS) 3))
                        ((BIT-TEST 4 KBD-SHIFTS) 2)  ;Top
                        ((BIT-TEST 1 KBD-SHIFTS) 1)  ;Shift
                        (T 0))
                  char)
            (aref kbd-new-table 0 char))))


))
; From file DISK.LISP#> IO; LAM2:
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO; DISK  "

(DEFUN LOAD-MCR-FILE (FILENAME PART &OPTIONAL (UNIT *DEFAULT-DISK-UNIT*)
                                    &AUX PART-BASE PART-SIZE RQB)
  "Load microcode from file FILENAME into partition PART on unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaosnet
or /"CC/" which refers to the machine being debugged by this one."
   (SETQ FILENAME (cond ((numberp filename)
                         (SEND (FS:PARSE-PATHNAME "dj:l.ubin;ulambda.emc")
                               ':NEW-VERSION
                               FILENAME))
                        ((eq filename t)
                         (fs:parse-pathname "dj:l.ubin;ulambda.emc"))
                        (t
                         (FS:MERGE-PATHNAME-DEFAULTS FILENAME))))
   ;;  Do string-equal, not equal, on the canonical-type, not the type
   (if (not (or (STRING-EQUAL (SEND FILENAME ':CANONICAL-TYPE) :MCR)
                (string-equal (SEND FILENAME ':CANONICAL-TYPE) "EMC")))
       (FERROR NIL "~A is not a MCR file." FILENAME))
   (SETQ UNIT (DECODE-UNIT-ARGUMENT UNIT
                      (FORMAT NIL "Loading ~A into ~A partiton"
                              FILENAME PART)
                      NIL
                       T))
   (UNWIND-PROTECT
     (PROGN
       (SETQ RQB (GET-DISK-RQB))
       (MULTIPLE-VALUE (PART-BASE PART-SIZE NIL PART)
       (FIND-DISK-PARTITION-FOR-WRITE PART NIL UNIT NIL "MCR"))
       (WITH-OPEN-FILE (FILE FILENAME '(:READ :FIXNUM))
      (DO-NAMED DONE
              ((BUF16 (ARRAY-LEADER RQB %DISK-RQ-LEADER-BUFFER))
             (BLOCK PART-BASE (1+ BLOCK))
              (N PART-SIZE (1- N)))
             ((ZEROP N) (FERROR NIL "Failed to fit in partition"))
        (DO ((LH) (RH)
           (I 0 (+ I 2)))
              ((= I 1000)
           (DISK-WRITE RQB UNIT BLOCK))
           (SETQ LH (FUNCALL FILE ':TYI)
                 RH (FUNCALL FILE ':TYI))
           (COND ((OR (NULL LH) (NULL RH))
              (UPDATE-PARTITION-COMMENT
                PART
                 (microcode-name filename)
                UNIT)
               (RETURN-FROM DONE NIL)))
           (ASET RH BUF16 I)
           (ASET LH BUF16 (1+ I))))))
     (DISPOSE-OF-UNIT UNIT)
     (RETURN-DISK-RQB RQB)))

))

; From file TYPES.LISP#> QL.SYS; LAM3:
#10R SYSTEM-INTERNALS#:
(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "LAM3: QL.SYS; TYPES.#"

(defun (:property fixnum type-optimizer) (expression &optional (low '*) (high '*))
  (if (and (neq low '*)
           (neq high '*)
           (< (- (if (consp high) (1- (car high)) high)
                 (if (consp low) (1+ (car low)) low))
              4))
      (let ((object (cadr expression)))
        `(memq ,object
               ',(loop for i from (if (consp low) (1+ (car low)) low)
                       upto (if (consp high) (1- (car high)) high)
                       collect i)))
    (optimize-numeric-type-test 'fixnump expression low high)))

(defun optimize-numeric-type-test (predicate expression low high)
  (let ((object (cadr expression)))
    (once-only (object)
      `(and (,predicate ,object)
            ,(cond ((eq low '*)
                    t)
                   ((numberp low)
                    `( ,object ,low))
                   ((consp low)
                    `(> ,object ,(car low))))
            ,(cond ((eq high '*)
                    t)
                   ((numberp high)
                    `( ,object ,high))
                   ((consp high)
                    `(< ,object ,(car high))))))))
))
