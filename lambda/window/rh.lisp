; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-

;;;; Alternate Rubout Handler.

;;; Disadvantages compared to ZTOP:
;;; * This rubout handler duplicates functionality provided by the editor.
;;; * It looks like the editor, but not everything is there, so one may be
;;;   led into typing editor commands which aren't present.
;;; * The mechanism for customizing the rubout handler is separate from that for
;;;   customizing the editor.

;;; Advantages compared to ZTOP:
;;; * It does not slow down the output side as ZTOP does.  This allows programs like
;;;   Macsyma to do graphics output and still have powerful rubout handling.  Often,
;;;   one is interested only in editing previous input to, not output from, a listener.
;;; * It can be used everyplace, with very little overhead.  It is there for every call
;;;   to READLINE and READ.  Getting ZTOP to work everyplace would be much harder.
;;; * It can provide functionality which is fundamentally different from what the editor
;;;   provides.  It can be oriented toward interactors like the Lisp or Macsyma listeners.
;;;   The input history commands are an example of this.  These are not oriented toward
;;;   editing buffers of text, but toward command line interaction.
;;; * It can be used from streams other than those going to the local console, since it is
;;;   written in terms of stream operations rather than sheet operations.

;;; If you are thinking of modifying this code, read this: Adding new editing operations is
;;; straightforward but not simple.  You can figure things out by looking at how a few of
;;; the existing operations are written.  The manner in which the rubout handler interacts
;;; with the rest of the I/O System is very complicated, however.  -- CWH 21-Mar-82

;;; The fill-pointer of an array only has meaning to FILL-POINTER, ARRAY-PUSH, ARRAY-POP,
;;; and the printer.  One can read and write anywhere into an array, regardless of where
;;; the fill pointer is set.  In this code, sometimes the fill pointer will be set
;;; after a write beyond it has taken place.

;character lossage
; of the most massive and prevalent form through this entire file.
;>>  Of course, I had this all fixed, except that LMI deems it more appropriate
;>>  to destroy real work and anybody attempting to do real work than to make any progress.

;;;; General Utilities

(DEFCONST SHIFT-ARRAY-PORTION-BUFFER (MAKE-STRING #o1000))

(DEFUN SHIFT-ARRAY-PORTION (ARRAY FROM TO WIDTH)
  "Copy WIDTH characters of ARRAY from FROM... to TO...
Like COPY-ARRAY-PORTION but copies properly even if FROM is less than TO."
  (COND ((> FROM TO)
         (COPY-ARRAY-PORTION ARRAY FROM (+ FROM WIDTH) ARRAY TO (+ TO WIDTH)))
        ((< FROM TO)
         ;; Make sure buffer is big enough.
         (UNLESS (> (ARRAY-LENGTH SHIFT-ARRAY-PORTION-BUFFER) WIDTH)
           (ADJUST-ARRAY-SIZE SHIFT-ARRAY-PORTION-BUFFER
                              (MAX WIDTH
                                   (+ 64. (ARRAY-LENGTH SHIFT-ARRAY-PORTION-BUFFER))))
           (SETQ SHIFT-ARRAY-PORTION-BUFFER
                 (FOLLOW-STRUCTURE-FORWARDING SHIFT-ARRAY-PORTION-BUFFER)))
         (WITHOUT-INTERRUPTS
          (COPY-ARRAY-PORTION ARRAY FROM (+ FROM WIDTH) SHIFT-ARRAY-PORTION-BUFFER 0 WIDTH)
          (COPY-ARRAY-PORTION SHIFT-ARRAY-PORTION-BUFFER 0 WIDTH ARRAY TO (+ TO WIDTH))))
        (T NIL)))

;;;; Rubout Handler Definitions

;;; State associated with the rubout handler buffer:
;;;  FILL-POINTER points to what has been typed so far.
;;;  SCAN-POINTER points to what has been read so far.
;;;  TYPEIN-POINTER points to where in the middle of the line we are typing.
;;;  INPUT-HISTORY is a list of the last few lines of input to this stream.

;;; The TYPEIN-POINTER slot isn't in the rubout handler buffer array leader.  It should be
;;; added later.  For now, just use two hash tables and key off the rubout handler buffer.
;;; Initialization of these last two slots is done by some kludges below.  These
;;; initializations should be moved to the :RUBOUT-HANDLER method later.

;;; The typein pointer is not bound when recursively entering the rubout handler on the
;;; same stream.  When exiting a recursive call, it is just set to the end of the buffer.
;;; This requires a modification to TV:STREAM-MIXIN to fix.

;;; The input stream is per-stream.  This seems to be more natural, since what may be copied
;;; is what appears directly preceding the current typein.  This is also the same way the +
;;; and * specials work.  The kill history is global and can be used for copying text from
;;; stream to stream.

(DEFSUBST RH-FILL-POINTER   () (RHB-FILL-POINTER RUBOUT-HANDLER-BUFFER))
(DEFSUBST RH-SCAN-POINTER   () (RHB-SCAN-POINTER RUBOUT-HANDLER-BUFFER))
(DEFSUBST RH-TYPEIN-POINTER () (RHB-TYPEIN-POINTER RUBOUT-HANDLER-BUFFER))
(DEFSUBST RH-DONT-SAVE-FLAG () (RHB-DONT-SAVE-FLAG RUBOUT-HANDLER-BUFFER))
(DEFSUBST RH-INPUT-HISTORY     () (RHB-INPUT-HISTORY RUBOUT-HANDLER-BUFFER))

(DEFUN RH-MAKE-INPUT-HISTORY (&OPTIONAL NAME)
  (ZWEI:MAKE-HISTORY (FORMAT NIL "input history ~@[for ~A~]" NAME)
                     :ELEMENT-STRING-FUNCTION 'SUMMARIZE-INPUT-STRING
                     :YANK-METHOD 'RH-YANK-FROM-HISTORY))

;;; RH-COMMAND-ALIST may be modified to change the rubout handler commands.
;;; The entries in this list are a cons of a key and a symbol with print-name prefix "COM-".

(DEFVAR RH-COMMAND-ALIST NIL)

;;; RH-KILL-BUFFER points to the buffer in which the last merging kill was done, and
;;; RH-KILL-INDEX is an index into the buffer marking the point of the kill.
;;; The next kill can merge if RH-KILL-BUFFER is the current rubout buffer
;;; and RH-KILL-INDEX matchs one end of the next kill.

(DEFVAR RH-KILL-BUFFER NIL)
(DEFVAR RH-KILL-INDEX NIL)

;;; Most of the functions defined below reference the instance variable RUBOUT-HANDLER-BUFFER,
;;; which is defined in TV:STREAM-MIXIN.

(DEFMACRO DEFUN-RH (NAME BVL &BODY BODY) `(DEFUN-METHOD ,NAME TV:STREAM-MIXIN ,BVL . ,BODY))

;;;; Main Loop

;;; This function gets called whenever a :TYI message is sent to the stream and we are
;;; inside the rubout handler.  If a normal character is typed, it is echoed, put in the
;;; buffer, and returned.  If a rubout or any other editing character is typed, any number
;;; of editing commands are processed by modifying the buffer, then, when the first
;;; non-editing character is typed, a throw is done back to the top level of the read
;;; function and the buffered input is re-scanned.  The character must be typed at the end
;;; of the line in order for the throw to take place.  ** The kludges labelled below may be
;;; flushed if this rubout handler is more carefully integrated with the rest of the
;;; system. **


(DEFVAR *LAST-COMMAND-TYPE* NIL)
(DEFVAR *CURRENT-COMMAND-TYPE* NIL)
(DEFVAR *RUBOUT-HANDLER-MARK* NIL)

(DEFUN-RH ALTERNATE-RUBOUT-HANDLER ()
  (LET ((CH) (CH-CHAR) (CH-CONTROL-META) (COMMAND)
        (FILL-POINTER (RH-FILL-POINTER))
        (TYPEIN-POINTER (RH-TYPEIN-POINTER))
        (STATUS (RHB-STATUS))
        (RUBBED-OUT-SOME NIL)
        (NUMERIC-ARG NIL)
        (NUMERIC-ARG-NEGATIVE NIL)
        (PROMPT-OPTION (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS))
        (INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT RUBOUT-HANDLER-OPTIONS)))
        (INITIAL-INPUT-POINTER (CADR (ASSQ ':INITIAL-INPUT-POINTER RUBOUT-HANDLER-OPTIONS))))
    ;; Prompt if desired
    (SETF (RHB-STATUS) NIL)
    (WHEN (MEMQ STATUS '(:INITIAL-ENTRY :RESTORED))
      (WHEN PROMPT-OPTION
        (RUBOUT-HANDLER-PROMPT (CADR PROMPT-OPTION) SELF NIL))
      (MULTIPLE-VALUE (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
        (SEND SELF :READ-CURSORPOS))
      ;; Output any "typeahead"
      (WHEN (PLUSP FILL-POINTER)
        (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER))
      ;; Kludge #1.  If this is the first time this rubout handler has been invoked
      ;; in this stream, then we must create the input history.
      (WHEN (AND (NOT (RH-INPUT-HISTORY))
                 (VARIABLE-BOUNDP ZWEI:HISTORY-LOADED))
        (SETF (RH-INPUT-HISTORY) (RH-MAKE-INPUT-HISTORY (SEND-IF-HANDLES SELF :NAME))))
      ;; save the previous input on the input history,
      ;; unless the previous read said not to save it.
      (WHEN (AND (NOT (RH-DONT-SAVE-FLAG))
                 TYPEIN-POINTER
                 (EQ STATUS ':INITIAL-ENTRY)
                 (NOT (ZEROP TYPEIN-POINTER)))
        ;; only add the contents if it is different from the last entry, and
        ;; the entry is at least 2 characters long.
        (SETF (RH-FILL-POINTER) TYPEIN-POINTER)
        (WHEN (AND (> TYPEIN-POINTER 1)
                   (RH-INPUT-HISTORY)           ;Don't die if HISTORY not loaded yet
                   (MISMATCH RUBOUT-HANDLER-BUFFER
                             (ZWEI:HISTORY-LATEST-ELEMENT (RH-INPUT-HISTORY))))
          (ZWEI:PUSH-ON-HISTORY (SUBSEQ RUBOUT-HANDLER-BUFFER 0 TYPEIN-POINTER)
                                (RH-INPUT-HISTORY)))
        (SETF (RH-FILL-POINTER) FILL-POINTER))
      ;; Then initialize the typein pointer.
      (SETF (RH-TYPEIN-POINTER) FILL-POINTER
            TYPEIN-POINTER FILL-POINTER)
      ;; Gobble the initial input if any.
      (WHEN (AND INITIAL-INPUT (EQ STATUS ':INITIAL-ENTRY))
        (RH-INSERT-STRING INITIAL-INPUT 0 NIL NIL NIL)
        (SETQ FILL-POINTER (RH-FILL-POINTER))
        (SETQ TYPEIN-POINTER
              (MAX (MIN (OR INITIAL-INPUT-POINTER TYPEIN-POINTER)
                        (LENGTH RUBOUT-HANDLER-BUFFER))
                   0))
        (SETF (RHB-TYPEIN-POINTER) TYPEIN-POINTER)
        (RH-SET-POSITION TYPEIN-POINTER)
        (SETF (RHB-STATUS) ':RUBOUT RUBBED-OUT-SOME T))
      ;; Record whether this unit of input should be saved on the history.
      (SETF (RH-DONT-SAVE-FLAG)
            (OR (CADR (ASSQ ':DONT-SAVE RUBOUT-HANDLER-OPTIONS))
                (CADR (ASSQ ':NO-INPUT-SAVE RUBOUT-HANDLER-OPTIONS)))))
;    ;;; Can this ever go off? :pass-though now only allows non-bucky. -- mly
;    ;; Kludge #5.  We can't echo or rub out a bucky char or a blip,
;    ;; so if the last char inserted was a either of those
;    ;; and it did not terminate the input, flush it.
;    (AND (NOT (ZEROP TYPEIN-POINTER))
;        (OR (CONSP (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER)))
;            (LDB-TEST %%KBD-CONTROL-META
;                      (AREF RUBOUT-HANDLER-BUFFER (1- TYPEIN-POINTER))))
;        (SETQ TYPEIN-POINTER (SETF (RH-TYPEIN-POINTER) (DECF (RH-FILL-POINTER)))))
    ;; Kludge #4.  After resuming a Break, the stream's cursorpos is wrong.
    ;; In fact, the cursor is at the end of the string in that case.
    ;; So, if it is supposed to be elsewhere, move it.
    ;; This condition also avoids wasting time when we are reading typein
    ;; at the end of the string.
    (OR (= FILL-POINTER TYPEIN-POINTER)
        (RH-CURSOR-MOTION TYPEIN-POINTER))
    ;; In case we had to return to the caller with a EDITING-COMMAND char
    ;; while RUBBED-OUT-SOME was T, make things consistent again
    ;; by causing a rescan now.
    (WHEN (AND RUBBED-OUT-SOME
               (= (RH-SCAN-POINTER) MOST-POSITIVE-FIXNUM))
      (SETF (RH-SCAN-POINTER) 0)
      (THROW 'RUBOUT-HANDLER T))
    (CATCH 'RETURN-CHARACTER
      (WHEN RUBOUT-HANDLER-ACTIVATION-CHARACTER
        (THROW 'RETURN-CHARACTER
               (PROG1 RUBOUT-HANDLER-ACTIVATION-CHARACTER
                      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL))))
      ;; Read characters.  If an ordinary character is typed and nothing has been rubbed out,
      ;; return immediately.  Otherwise, let all editing operations complete
      ;; before returning.
      (DO (*LAST-COMMAND-TYPE*
           *CURRENT-COMMAND-TYPE*
           *RUBOUT-HANDLER-MARK*
           (EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND RUBOUT-HANDLER-OPTIONS)))
           (DO-NOT-ECHO (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-OPTIONS)))
           (PASS-THROUGH (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS)))
           (COMMAND-HANDLER
             (ASSQ ':COMMAND RUBOUT-HANDLER-OPTIONS))
           (PREEMPTABLE (ASSQ ':PREEMPTABLE RUBOUT-HANDLER-OPTIONS))
           (BLIP-HANDLER (ASSQ ':BLIP-HANDLER RUBOUT-HANDLER-OPTIONS))
           (ACTIVATION-HANDLER
             (ASSQ ':ACTIVATION RUBOUT-HANDLER-OPTIONS)))
          (NIL)
        ;; Read a character from the stream after bypassing ourself.
        (SETQ CH (LET ((RUBOUT-HANDLER NIL)) (SEND SELF :ANY-TYI)))
        (IF (CONSP CH)
            (COND ((EQ (CAR CH) 'REDISPLAY-RUBOUT-HANDLER)
                   (SEND SELF :SET-CURSORPOS PROMPT-STARTING-X PROMPT-STARTING-Y)
                   (SEND SELF :CLEAR-REST-OF-LINE)
                   (RH-REPRINT-INPUT NIL T))
                  ((AND BLIP-HANDLER (FUNCALL (CADR BLIP-HANDLER) CH SELF)))
                  (PREEMPTABLE
                   (SETF (RH-SCAN-POINTER) 0)
                   (THROW 'RETURN-FROM-RUBOUT-HANDLER
                          (VALUES CH (CADR PREEMPTABLE))))
                  ((AND (EQ (CAR CH) ':MOUSE-BUTTON)
                        (EQ (CADR CH) #/MOUSE-3-1))
                   (MOUSE-CALL-SYSTEM-MENU)))
          (SETQ CH-CHAR (CHAR-CODE CH))
          (SETQ CH-CONTROL-META (CHAR-BITS CH))
          (SETQ COMMAND (UNLESS (AND (ZEROP CH-CONTROL-META)
                                     (MEMQ CH PASS-THROUGH))
                          (ASSQ CH RH-COMMAND-ALIST)))
          (COND
            ((AND COMMAND-HANDLER
                  (APPLY (CADR COMMAND-HANDLER) CH (CDDR COMMAND-HANDLER)))
             (SETF (RH-SCAN-POINTER) 0)
             (THROW 'RETURN-FROM-RUBOUT-HANDLER
                    (VALUES
                      `(:COMMAND ,CH ,(* (OR NUMERIC-ARG 1)
                                         (IF NUMERIC-ARG-NEGATIVE -1 1)))
                      ':COMMAND)))
            ((OR (MEMQ CH DO-NOT-ECHO)
                 (AND ACTIVATION-HANDLER
                      (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER))))
             (RH-SET-POSITION (RH-FILL-POINTER))
             (LET ((VALUE
                     (IF (MEMQ CH DO-NOT-ECHO) CH
                       `(:ACTIVATION ,CH ,(* (OR NUMERIC-ARG 1)
                                             (IF NUMERIC-ARG-NEGATIVE -1 1))))))
               (COND (RUBBED-OUT-SOME
                      ;; Why isn't this done in the :RUBOUT-HANDLER method loop?
                      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER VALUE)
                      (SETF (RH-SCAN-POINTER) 0)
                      (THROW 'RUBOUT-HANDLER T))
                     (T (THROW 'RETURN-CHARACTER VALUE)))))
            ;; Don't touch this character, just return it to caller.
            ((OR (MEMQ CH EDITING-COMMAND)
                 (SI:ASSQ-CAREFUL CH EDITING-COMMAND))
             ;; Cause rubout handler rescan next time the user does :TYI.
             (IF RUBBED-OUT-SOME (SETF (RH-SCAN-POINTER) MOST-POSITIVE-FIXNUM))
             (RETURN CH))
            ;; An standard rh editing command of some sort.  The RUBBED-OUT-SOME bit can only
            ;; be cleared by entering this function again.  The function is passed the
            ;; numeric argument, and returns T if we are going to need to throw out (like
            ;; DIS-ALL in the editor).
            (COMMAND
             (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
                   *CURRENT-COMMAND-TYPE* NIL)
             (SETQ RUBBED-OUT-SOME
                   (OR (FUNCALL (CDR COMMAND) (* (OR NUMERIC-ARG 1)
                                                 (IF NUMERIC-ARG-NEGATIVE -1 1)))
                       RUBBED-OUT-SOME))
             (SETF (RHB-STATUS) (IF RUBBED-OUT-SOME ':RUBOUT))
             (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL)
             ;; If the buffer is empty and the :FULL-RUBOUT option is active, then throw now.
             ;; This will throw if the user types Rubout or ClearScreen immediately after
             ;; entering the read function.  It is important that we check for this here
             ;; and not in RH-DELETE-STRING since some commands, such as Yank-Pop, may
             ;; temporarily empty the buffer.  It wouldn't be the right thing to throw
             ;; if the buffer only contained whitespace since it is the responsibility
             ;; of the caller to discard whitespace when looking for special characters.
             (COND ((AND (ZEROP (RH-FILL-POINTER))
                         (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS))
                    ;; This SETF should be done in the :RUBOUT-HANDLER method loop.
                    (SETF (RH-SCAN-POINTER) 0)
                    (THROW 'RUBOUT-HANDLER T))))
            ;;Handle Control-number and Control-U specially.
            ((AND (NOT (ZEROP CH-CONTROL-META))
                  ( #/0 CH-CHAR #/9))
             (SETQ NUMERIC-ARG (+ (* (OR NUMERIC-ARG 0) 10.) (- CH-CHAR #/0))))
            ((= CH #/CONTROL-U)
             (SETQ NUMERIC-ARG (* (OR NUMERIC-ARG 1) 4)))
            ((AND (NOT (ZEROP CH-CONTROL-META)) (= CH-CHAR #/-))
             (IF NUMERIC-ARG
                 (SEND SELF :BEEP)
               (SETQ NUMERIC-ARG-NEGATIVE (NOT NUMERIC-ARG-NEGATIVE))))
            ;; Some other random control character -- beep and ignore
            ((NOT (ZEROP CH-CONTROL-META))
             (SEND SELF :BEEP)
             (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))
            ;; Self-inserting character.  Set RUBBED-OUT-SOME since if we return,
            ;; we were typing in the middle of the line.  Typing at the end of the
            ;; line throws to RETURN-CHARACTER.
            (T (UNLESS NUMERIC-ARG-NEGATIVE
                 (RH-INSERT-CHAR CH (OR NUMERIC-ARG 1) RUBBED-OUT-SOME)
                 (SETF RUBBED-OUT-SOME T (RHB-STATUS) :RUBOUT))
               (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
                     *CURRENT-COMMAND-TYPE* NIL
                     *RUBOUT-HANDLER-MARK* NIL)
               (SETQ NUMERIC-ARG NIL NUMERIC-ARG-NEGATIVE NIL))))))))

;;;; Primitive Operations

(DEFUN-RH RH-SEARCH-CHAR (CHAR &OPTIONAL (FROM 0) (TO NIL))
  "Return the first index in RUBOUT-HANDLER-BUFFER where CHAR appears, between FROM and TO."
  (POSITION CHAR RUBOUT-HANDLER-BUFFER :START FROM :END TO))

(DEFUN-RH RH-REVERSE-SEARCH-CHAR (CHAR &OPTIONAL (FROM NIL) (TO 0))
  "Return the last index in RUBOUT-HANDLER-BUFFER where CHAR appears, between FROM and TO.
TO should be less than FROM."
  (POSITION CHAR RUBOUT-HANDLER-BUFFER :START TO :END FROM :FROM-END T))

;;; Compute the motion for printing a string.  We don't have to worry about :PIXEL or
;;; :CHARACTER since the stream will work in whatever units are the most convenient for it.

(DEFUN-RH RH-COMPUTE-MOTION (&REST ARGS)
  "Perform :COMPUTE-MOTION on this window, with ARGS."
  (LEXPR-SEND SELF :COMPUTE-MOTION RUBOUT-HANDLER-BUFFER ARGS))

(DEFUN-RH RH-CURSOR-MOTION (POSITION &OPTIONAL CURRENT-POSITION &OPTIONAL NO-NEXT-LINE
                                     &AUX Y1)
  "Move the window cursor to match POSITION, an index in RUBOUT-HANDLER-BUFFER.
Does not check validity of arguments."
  (MULTIPLE-VALUE-BIND (X Y)
      (IF (AND CURRENT-POSITION ( POSITION CURRENT-POSITION))
          (RH-COMPUTE-MOTION CURRENT-POSITION POSITION)
          (RH-COMPUTE-MOTION 0 POSITION RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y))
    (IF ( POSITION (RH-FILL-POINTER))
        (MULTIPLE-VALUE (NIL Y1)
          (RH-COMPUTE-MOTION POSITION (1+ POSITION) X Y))
      (SETQ Y1 Y))
    ;; If we are at the point of line-continuation,
    ;; put cursor at the start of the next line rather than at margin of previous.
    (WHEN (AND (< POSITION (ARRAY-ACTIVE-LENGTH RUBOUT-HANDLER-BUFFER))
               (NOT NO-NEXT-LINE)
               ( (AREF RUBOUT-HANDLER-BUFFER POSITION) #/RETURN)
               ( Y Y1))
      (SETQ Y Y1 X 0))
    (SEND SELF :SET-CURSORPOS X Y)))

;;; Insert N copies of a self-inserting character.  A character gets here by being
;;; non-bucky and not having an editing command associated with it.  Or it can get here
;;; via the :PASS-THROUGH option.

;;; If a bucky character gets here, and we are not able to return immediately,
;;; it is echoed as a non-bucky character and loses its bucky bits in the buffer,
;;; since rubbing out a character with bucky bits would not work.

(DEFRESOURCE RH-INSERT-STRING ()
  :CONSTRUCTOR (MAKE-ARRAY 64. :TYPE ART-16B))

(DEFUN RH-INSERT-CHAR (CH N RUBBED-OUT-SOME)
  "Insert N copies of the character CH."
  (USING-RESOURCE (STRING RH-INSERT-STRING)
    (SETQ N (MIN N (ARRAY-LENGTH STRING)))
    (DOTIMES (I N) (ASET CH STRING I))
    ;; Flush bucky bits unless we are going to return right away.
    (OR (ZEROP (CHAR-BITS CH))
        (AND RUBBED-OUT-SOME
             (SETQ CH (CHAR-CODE CH))))
    (RH-INSERT-STRING STRING 0 N RUBBED-OUT-SOME T
;                     (OR (NOT (ZEROP (CHAR-BITS CH)))
;                         (MEMQ CH (CDR (ASSQ ':DO-NOT-ECHO RUBOUT-HANDLER-OPTIONS))))
                      )))

(DEFUN-RH RH-CHECK-RE-ECHO ()
  "Clean up after any error messages printed during rubout handling
by means of complete redisplay.  This can only be triggered
when typing at the end of the line, so no need to update the typein pointer."
  (COND (RUBOUT-HANDLER-RE-ECHO-FLAG
         (SETQ RUBOUT-HANDLER-RE-ECHO-FLAG NIL)
         (MULTIPLE-VALUE-BIND (X Y) (SEND SELF :READ-CURSORPOS)
           (SEND SELF :SET-CURSORPOS
                         RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
           (SEND SELF :CLEAR-BETWEEN-CURSORPOSES
                         RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y X Y)
           (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER)))))

(DEFUN-RH RH-SET-POSITION (POSITION)
  "Set the editing cursor position.  Updates the screen as well."
  (LET ((TYPEIN-POINTER (RH-TYPEIN-POINTER))
        (FILL-POINTER (RH-FILL-POINTER)))
    (RH-CHECK-RE-ECHO)
    (IF (OR (AND (< POSITION 0) (= TYPEIN-POINTER 0))
            (AND (> POSITION FILL-POINTER) (= TYPEIN-POINTER FILL-POINTER)))
        (SEND SELF :BEEP)
      (SETQ POSITION (MIN (MAX POSITION 0) FILL-POINTER))
      (RH-CURSOR-MOTION POSITION TYPEIN-POINTER)
      (SETF (RH-TYPEIN-POINTER) POSITION))
    NIL))

(DEFUN-RH RH-REPRINT-INPUT (&OPTIONAL CHAR DONT-SET-PROMPT-CURSORPOS)
  "Reprint the contents of the rubout handler buffer at the current cursor position."
  (if (characterp char) (setq char (char-int char)))
  (UNLESS DONT-SET-PROMPT-CURSORPOS
    (multiple-value-setq (PROMPT-STARTING-X PROMPT-STARTING-Y)
      (SEND SELF :READ-CURSORPOS)))
  (LET ((PROMPT (OR (ASSQ ':REPROMPT RUBOUT-HANDLER-OPTIONS)
                    (ASSQ ':PROMPT RUBOUT-HANDLER-OPTIONS))))
    (IF PROMPT (RUBOUT-HANDLER-PROMPT (CADR PROMPT) SELF CHAR)))
  (MULTIPLE-VALUE-SETQ (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
    (SEND SELF :READ-CURSORPOS))
  (LET ((MORE-PROCESSING-GLOBAL-ENABLE NIL))
    (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER 0 (length rubout-handler-buffer)))
  (RH-CURSOR-MOTION (RH-TYPEIN-POINTER))
  NIL)

;;; Insert a string into the buffer and print it on the screen.  The string is inserted
;;; at the current typein pointer, and the pointer is left after the string.

(DEFUN-RH RH-INSERT-STRING (STRING &OPTIONAL (BEGIN 0) END
                                   (RUBBED-OUT-SOME T) (RETURN-FROM-RH T)
                                   (DO-NOT-ECHO NIL))
  "Insert the portion of STRING from BEGIN to END.
DO-NOT-ECHO says do not update the screen.
RETURN-FROM-RH = NIL says do not activate the rubout handler input after insertion.
RUBBED-OUT-SOME = NIL says that the rubout handler must rescan some
input it has already scanned."
 (SETQ *RUBOUT-HANDLER-MARK* NIL)
 (OR END (SETQ END (STRING-LENGTH STRING)))
 (LET ((NEWLINE-POS) (BEGIN-X) (BEGIN-Y)
       (*WIDTH (- END BEGIN))
       RECOMPUTE-CURSOR-FLAG
       (TYPEIN-POINTER (RH-TYPEIN-POINTER))
       (FILL-POINTER   (RH-FILL-POINTER)))
  ;; Stop merging kills.
  (SETQ RH-KILL-BUFFER NIL)
  ;; Increase the size of of the typein buffer, if necessary.
  (IF (> (+ FILL-POINTER *WIDTH) (ARRAY-LENGTH RUBOUT-HANDLER-BUFFER))
      (ADJUST-ARRAY-SIZE RUBOUT-HANDLER-BUFFER (* 2 (+ FILL-POINTER *WIDTH))))
  ;; Make room for the characters to be inserted.
  (SHIFT-ARRAY-PORTION RUBOUT-HANDLER-BUFFER
                       TYPEIN-POINTER (+ TYPEIN-POINTER *WIDTH)
                       (- FILL-POINTER TYPEIN-POINTER))
  ;; Copy the string in.
  (COPY-ARRAY-PORTION STRING BEGIN END
                      RUBOUT-HANDLER-BUFFER TYPEIN-POINTER (+ TYPEIN-POINTER *WIDTH))
  ;; Update the fill pointer and the typein pointer.
  (INCF FILL-POINTER   *WIDTH)
  (INCF TYPEIN-POINTER *WIDTH)
  (SETF (RH-FILL-POINTER)   FILL-POINTER)
  (SETF (RH-TYPEIN-POINTER) TYPEIN-POINTER)

  ;; Update the screen.  There are four possible ways of doing the update, involving the
  ;; messages :STRING-OUT, :CLEAR-BETWEEN-CURSORPOSES, :INSERT-STRING, and :CLEAR-REST-OF-LINE.
  ;; We need a more powerful message for consoles with region scroll.
  (LET ((MORE-PROCESSING-GLOBAL-ENABLE NIL))
    (COND ;; Don't update the screen at all.
          (DO-NOT-ECHO)

          ;; If the string is being inserted at the end of the buffer, we don't have to worry
          ;; about character insertion.  Just output the string.
          ((= TYPEIN-POINTER FILL-POINTER)
           (SEND SELF :STRING-OUT STRING BEGIN END))

          ;; If the string being inserted contains newlines, or if the line being inserted into
          ;; wraps around the right edge of the window, reprint everything after the insertion.
          ((PROGN (SETQ NEWLINE-POS (RH-SEARCH-CHAR #/NEWLINE (- TYPEIN-POINTER *WIDTH)))
                  (RH-CURSOR-MOTION (- TYPEIN-POINTER *WIDTH) NIL T)
                  (MULTIPLE-VALUE (BEGIN-X BEGIN-Y) (SEND SELF :READ-CURSORPOS))
                  (OR (AND NEWLINE-POS (< NEWLINE-POS TYPEIN-POINTER))
                      (MULTIPLE-VALUE-BIND (IGNORE NEWLINE-Y)
                          (RH-COMPUTE-MOTION (- TYPEIN-POINTER *WIDTH) NEWLINE-POS)
                        (NOT (= BEGIN-Y NEWLINE-Y)))))
           ;; Find the cursor position that marks the end of the buffer.
           (MULTIPLE-VALUE-BIND (END-X END-Y)
               (SEND SELF :COMPUTE-MOTION RUBOUT-HANDLER-BUFFER TYPEIN-POINTER FILL-POINTER)
             ;; Clear from the beginning of the string to the end of the buffer.
             (SEND SELF :CLEAR-BETWEEN-CURSORPOSES BEGIN-X BEGIN-Y END-X END-Y))
           ;; Retype, and move the cursor back where it belongs.
           (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER (- TYPEIN-POINTER *WIDTH))
           (SETQ RECOMPUTE-CURSOR-FLAG T))

          ;; If the console can insert characters, then do so.
          ;; Pass in the string to handle variable width fonts.
          ((OPERATION-HANDLED-P SELF :INSERT-STRING)
           (SEND SELF :INSERT-STRING STRING BEGIN END))

          ;; Otherwise, simulate it VT52 style.  Only reprint the current line.
          (T (SEND SELF :CLEAR-REST-OF-LINE)
             (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER (- TYPEIN-POINTER *WIDTH) NEWLINE-POS)
             (SETQ RECOMPUTE-CURSOR-FLAG T))))

  (WHEN RECOMPUTE-CURSOR-FLAG
     (RH-CURSOR-MOTION TYPEIN-POINTER))

  ;; Possibly return from the rubout handler.  If the string is being inserted at the end
  ;; of the line, then we will return from the rubout handler in one of two ways:  If there
  ;; have been no other editing changes, we return the first character of the string and
  ;; leave the scan pointer pointing at the second character, thus avoiding a rescan.  If
  ;; there have been other editing changes, then we will rescan.  This is the case
  ;; for normal typein -- the scan pointer keeps up with the typein pointer.
  ;; We can force control to stay with the rubout handler by passing T to RETURN-FROM-RH.

  ;; If typing in the middle of the line, don't rescan.  No need to update the scan
  ;; pointer in this case since we're going to throw back to the RUBOUT-HANDLER tag.
  ;; *** See Below ***
  (IF (= TYPEIN-POINTER FILL-POINTER)
      (COND ((NOT RETURN-FROM-RH))
            (RUBBED-OUT-SOME
             ;; This should be done in the :RUBOUT-HANDLER method loop.  Why isn't it?
             (SETF (RH-SCAN-POINTER) 0)
             (THROW 'RUBOUT-HANDLER T))
            (T (INCF (RH-SCAN-POINTER))
               (THROW 'RETURN-CHARACTER (AREF STRING BEGIN)))))

   ;; *** Below ***
   ;; Kludge to bypass (TV:STREAM-MIXIN :TYI), which needs to be fixed.
   (SETF (RH-SCAN-POINTER) FILL-POINTER)
   ;; If we didn't throw out, then the input has been modified.  Back to the main loop.
   T))

;;; Deletes a buffer interval as marked by two pointers.  The typein pointer
;;; is left at the beginning of the interval.  Beeps if nothing is being deleted.

(DEFUN-RH RH-DELETE-STRING (BEGIN END &OPTIONAL (SAVE T))
  "Delete the part of the buffer from BEGIN to END.
If SAVE is NIL, the deleted text does not go on the kill ring."
  (LET ((NEWLINE-POS) (BEGIN-X) (BEGIN-Y) RECOMPUTE-CURSOR-FLAG
        (*WIDTH) (FILL-POINTER (RH-FILL-POINTER)))
    (SETQ *RUBOUT-HANDLER-MARK* NIL)
    (SETQ BEGIN (MAX BEGIN 0))
    (SETQ END (MIN END FILL-POINTER))
    (SETQ *WIDTH (- END BEGIN))
    (IF (= *WIDTH 0)
        ;; If nothing is being deleted, don't rescan input.
        NIL
      ;; Possibly save the string, and possibly merge.  If the string isn't saved, then stop
      ;; merging kills.  If RH-KILL-BUFFER is RUBOUT-HANDLER-BUFFER, then kills are being
      ;; merged and the last kill took place in this buffer.  If RH-KILL-INDEX is BEGIN,
      ;; then append to the right.  If RH-KILL-INDEX is END, then append to the left.
      (IF (OR (NOT SAVE)
              (NOT (FBOUNDP 'ZWEI:KILL-STRING)))
          (SETQ RH-KILL-BUFFER NIL)
        (ZWEI:KILL-STRING (COERCE (SUBSEQ RUBOUT-HANDLER-BUFFER BEGIN END) 'STRING)
                          (AND (EQ RH-KILL-BUFFER RUBOUT-HANDLER-BUFFER)
                               (EQ *LAST-COMMAND-TYPE* 'KILL))
                          (EQ BEGIN RH-KILL-INDEX))
        (SETQ RH-KILL-BUFFER RUBOUT-HANDLER-BUFFER
              RH-KILL-INDEX BEGIN
              *CURRENT-COMMAND-TYPE* 'KILL))

      ;; Update the screen.  Move cursor to beginning of the string.  This
      ;; also erases read-time error messages.
      (RH-SET-POSITION BEGIN)
      ;; We have three ways of updating the screen.  This is very similar
      ;; to RH-INSERT-STRING.
      (COND ;; If the string being deleted contains newlines, or if the line being deleted from
            ;; wraps around the right edge of the window, reprint everything after the deletion.
            ((PROGN (SETQ NEWLINE-POS (RH-SEARCH-CHAR #/NEWLINE BEGIN))
                    (RH-CURSOR-MOTION BEGIN NIL T)
                    (MULTIPLE-VALUE (BEGIN-X BEGIN-Y) (SEND SELF :READ-CURSORPOS))
                    (OR (AND NEWLINE-POS (< NEWLINE-POS END))
                        (MULTIPLE-VALUE-BIND (IGNORE NEWLINE-Y)
                            (RH-COMPUTE-MOTION BEGIN NEWLINE-POS)
                          ( BEGIN-Y NEWLINE-Y))))
             ;; Find the cursor position that marks the end of the buffer.
             (MULTIPLE-VALUE-BIND (END-X END-Y)
                 (SEND SELF :COMPUTE-MOTION RUBOUT-HANDLER-BUFFER BEGIN FILL-POINTER)
               ;; Clear from the beginning of the string to the end of the buffer.
               (SEND SELF :CLEAR-BETWEEN-CURSORPOSES BEGIN-X BEGIN-Y END-X END-Y)
               ;; Retype, and move the cursor back where it belongs.
               (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER END)
               (SETQ RECOMPUTE-CURSOR-FLAG T)))

            ;; If the console can delete characters, then do so.
            ;; Pass in the string to handle variable width fonts.
            ((OPERATION-HANDLED-P SELF :DELETE-STRING)
             (SEND SELF :DELETE-STRING RUBOUT-HANDLER-BUFFER BEGIN END))

            ;; Otherwise, simulate it VT52 style.  Only reprint the current line.
            (T
             (SEND SELF :CLEAR-REST-OF-LINE)
             (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER END NEWLINE-POS)
             (SETQ RECOMPUTE-CURSOR-FLAG T)))

      ;; Delete the characters from the buffer.  Do this after updating the screen
      ;; since we need the characters to delete correctly.
      (SHIFT-ARRAY-PORTION RUBOUT-HANDLER-BUFFER END BEGIN (- FILL-POINTER END))
      ;; Update the fill pointer.
      (SETF (RH-FILL-POINTER) (- FILL-POINTER *WIDTH))
      (WHEN RECOMPUTE-CURSOR-FLAG
        (RH-CURSOR-MOTION BEGIN))
      ;; Stuff got deleted.
      T)))

;;;; Commands

(DEFMACRO DEFINE-RH-COMMAND (NAME CHARS ARGS &BODY BODY)
  "Define a rubout handler command character."
  `(PROGN
     (ADD-RH-COMMAND ',NAME ',CHARS)
     (DEFUN-RH ,NAME ,ARGS . ,BODY)))

(DEFUN ADD-RH-COMMAND (NAME CHARS)
  (DOLIST (C CHARS)
    (if (characterp c) (setq c (char-int c)))
    (LET ((ENTRY (ASSQ C RH-COMMAND-ALIST)))
        (IF ENTRY
            (SETF (CDR ENTRY) NAME)
            (SETQ RH-COMMAND-ALIST (NCONC RH-COMMAND-ALIST (LIST (CONS C NAME))))))))

;;; Reprinting Input

;;; Reprints input on next line.  #/DELETE used to be bound to this command.
;(DEFINE-RH-COMMAND RH-COM-REFRESH-INPUT () (IGNORE)
;  (RH-CURSOR-MOTION (RH-FILL-POINTER))
;  (SEND SELF :TYO #/NEWLINE)
;  (RH-REPRINT-INPUT))

;;; Reprints input after clearing the screen.
(DEFINE-RH-COMMAND RH-COM-REFRESH-SCREEN (#/CLEAR-SCREEN #/CONTROL-L) (IGNORE)
  (SEND SELF :CLEAR-WINDOW)
  (RH-REPRINT-INPUT #/CLEAR-SCREEN)
  NIL)

;;; Moving Around

(DEFINE-RH-COMMAND RH-COM-BEGINNING-OF-BUFFER (#/META-<) (IGNORE)
  (RH-SET-POSITION 0))

(DEFINE-RH-COMMAND RH-COM-END-OF-BUFFER (#/META->) (IGNORE)
  (RH-SET-POSITION (RH-FILL-POINTER)))

(DEFINE-RH-COMMAND RH-COM-FORWARD-CHARACTER (#/CONTROL-F) (N)
  (RH-SET-POSITION (+ (RH-TYPEIN-POINTER) N)))

(DEFINE-RH-COMMAND RH-COM-BACKWARD-CHARACTER (#/CONTROL-B) (N)
  (RH-SET-POSITION (- (RH-TYPEIN-POINTER) N)))

;;; Mark and region.

(DEFINE-RH-COMMAND RH-COM-MARK-BEGINNING (#/CONTROL-<) (IGNORE)
  (SETQ *RUBOUT-HANDLER-MARK* 0)
  NIL)

(DEFINE-RH-COMMAND RH-COM-MARK-END (#/CONTROL->) (IGNORE)
  (SETQ *RUBOUT-HANDLER-MARK* (RH-FILL-POINTER))
  NIL)

(DEFINE-RH-COMMAND RH-COM-SET-MARK (#/CONTROL-SPACE #/CONTROL-@) (IGNORE)
  (SETQ *RUBOUT-HANDLER-MARK* (RH-TYPEIN-POINTER))
  NIL)

(DEFINE-RH-COMMAND RH-COM-KILL-REGION (#/CONTROL-W) (IGNORE)
  (IF *RUBOUT-HANDLER-MARK*
      (RH-DELETE-STRING (MIN (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
                        (MAX (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*))
    (SEND SELF :BEEP)
    NIL))

(DEFINE-RH-COMMAND RH-COM-SAVE-REGION (#/META-W) (IGNORE)
  (IF *RUBOUT-HANDLER-MARK*
      (LET ((STRING (MAKE-STRING (- (MAX (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
                                    (MIN (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)))))
        (COPY-ARRAY-PORTION RUBOUT-HANDLER-BUFFER
                            (MIN (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
                            (MAX (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
                            STRING 0 (LENGTH STRING))
        (ZWEI:KILL-STRING STRING NIL))
    (SEND SELF :BEEP))
  NIL)

;;;; Displaying Information

;;; General utility for the rubout handler to display information and reprint the current
;;; input.  Bind RUBOUT-HANDLER to NIL so that more processing TYI's won't go through it
;;; and so that our TYI goes straight to the stream and not through the rubout handler
;;; (which would insert the character into the buffer).  It takes a function which will
;;; produce the output and passes it the width and height of the current window, and any
;;; additional arguments it may need. (This should really be done with downward lexical
;;; closures.)  The :CLEAR-REST-OF-LINE is done so that we will start out printing on a clean line.
;;; Printing newline characters takes care of clearing subsequent lines.

(DEFUN-RH RH-DISPLAY-INFO-INTERNAL (PRINTER)
  (LET ((RUBOUT-HANDLER NIL))
    (SEND SELF :SET-CURSORPOS PROMPT-STARTING-X PROMPT-STARTING-Y)
    (SEND SELF :CLEAR-REST-OF-LINE)
    (SEND SELF :FRESH-LINE)
    (FUNCALL PRINTER)
    (FORMAT SELF "~2&")
    (RH-REPRINT-INPUT #/HELP))
  NIL)

(DEFMACRO RH-DISPLAY-INFO (&BODY BODY)
  "Execute BODY and then reprint the rubout buffer."
  `(RH-DISPLAY-INFO-INTERNAL (LAMBDA () (DECLARE (SYS:DOWNWARD-FUNCTION)) . ,BODY)))

;;;; Line Commands

;;; Returns the position of the first newline appearing after POS.

(DEFUN-RH RH-SEARCH-FORWARD-NEWLINE (POS)
  (DO ((FILL-POINTER (RH-FILL-POINTER))
       (I POS (1+ I)))
      ((OR ( I FILL-POINTER) (= (AREF RUBOUT-HANDLER-BUFFER I) #/NEWLINE)) I)))

;;; Returns the position of the first newline appearing before POS-1.
;;; Returns -1 if reached the beginning of the buffer.

(DEFUN-RH RH-SEARCH-BACKWARD-NEWLINE (POS)
  (DO ((I (1- POS) (1- I)))
      ((OR (< I 0) (= (AREF RUBOUT-HANDLER-BUFFER I) #/NEWLINE)) I)))

(DEFINE-RH-COMMAND RH-COM-BEGINNING-OF-LINE (#/CONTROL-A) (IGNORE)
  (RH-SET-POSITION (1+ (RH-SEARCH-BACKWARD-NEWLINE (RH-TYPEIN-POINTER)))))

(DEFINE-RH-COMMAND RH-COM-END-OF-LINE (#/CONTROL-E) (IGNORE)
  (RH-SET-POSITION (RH-SEARCH-FORWARD-NEWLINE (RH-TYPEIN-POINTER))))

(DEFINE-RH-COMMAND RH-COM-PREVIOUS-LINE (#/CONTROL-P) (N)
  (LET* ((LINE-BEGIN (RH-SEARCH-BACKWARD-NEWLINE (RH-TYPEIN-POINTER)))
         (INDENT (- (RH-TYPEIN-POINTER) LINE-BEGIN)))
    (DOTIMES (I N)
      (IF (= LINE-BEGIN -1) (RETURN))
      (SETQ LINE-BEGIN (RH-SEARCH-BACKWARD-NEWLINE LINE-BEGIN)))
    ;; When moving from a long line to a short line, be sure not to go off the end.
    (RH-SET-POSITION (MIN (+ LINE-BEGIN INDENT)
                          (RH-SEARCH-FORWARD-NEWLINE (1+ LINE-BEGIN))))))

(DEFINE-RH-COMMAND RH-COM-NEXT-LINE (#/CONTROL-N) (N)
  (LET* ((LINE-BEGIN (RH-SEARCH-BACKWARD-NEWLINE (RH-TYPEIN-POINTER)))
         (INDENT (- (RH-TYPEIN-POINTER) LINE-BEGIN)))
    (DOTIMES (I N)
      (COND ((= LINE-BEGIN (RH-FILL-POINTER))
             (SETQ LINE-BEGIN (RH-SEARCH-BACKWARD-NEWLINE LINE-BEGIN))
             (RETURN)))
      (SETQ LINE-BEGIN (RH-SEARCH-FORWARD-NEWLINE (1+ LINE-BEGIN))))
    ;; When moving from a long line to a short line, be sure not to go off the end.
    (RH-SET-POSITION (MIN (+ LINE-BEGIN INDENT)
                          (RH-SEARCH-FORWARD-NEWLINE (1+ LINE-BEGIN))))))

;;;; Deleting Things

(DEFINE-RH-COMMAND RH-COM-DELETE-CHARACTER (#/CONTROL-D) (N)
  (RH-DELETE-STRING (RH-TYPEIN-POINTER) (+ (RH-TYPEIN-POINTER) N) ( N 1)))

(DEFINE-RH-COMMAND RH-COM-RUBOUT-CHARACTER (#/RUBOUT) (N)
  (RH-DELETE-STRING (- (RH-TYPEIN-POINTER) N) (RH-TYPEIN-POINTER) ( N 1)))

;;; CLEAR-INPUT flushes all buffered input.  If the full rubout option is in
;;; use, then we will throw after returning to the command loop.
;;; No need to prompt since the prompt still there.

(DEFINE-RH-COMMAND RH-COM-CLEAR-INPUT (#/CLEAR-INPUT) (IGNORE)
  (UNLESS (AND (ZEROP (RH-FILL-POINTER))
               (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS))
    (RH-DELETE-STRING 0 (RH-FILL-POINTER))))

;;; If at the end of a line, kill just the newline.  Otherwise, kill to
;;; the end of the line.  Extend this to work with numeric arguments.

(DEFINE-RH-COMMAND RH-COM-KILL-LINE (#/CONTROL-K) (IGNORE)
  (LET* ((TYPEIN-POINTER (RH-TYPEIN-POINTER))
         (SEARCH-POINTER (RH-SEARCH-FORWARD-NEWLINE TYPEIN-POINTER)))
    (RH-DELETE-STRING TYPEIN-POINTER
                      (IF (AND (= TYPEIN-POINTER SEARCH-POINTER)
                               (NOT (= TYPEIN-POINTER (RH-FILL-POINTER))))
                          (1+ SEARCH-POINTER)
                          SEARCH-POINTER))))

;;;; Word Commands

;;; Use ZWEI's syntax table if ZWEI is around.

(DEFUN-RH RH-ALPHABETIC? (I)
  (LET ((C (AREF RUBOUT-HANDLER-BUFFER I)))
    (IF (BOUNDP 'ZWEI:*WORD-SYNTAX-TABLE*)
        (= (ZWEI:CHAR-SYNTAX C ZWEI:*WORD-SYNTAX-TABLE*) ZWEI:WORD-ALPHABETIC)
      (ALPHA-CHAR-P C))))

;;; Returns the position of the first (non) alphabetic character
;;; in the buffer.  If no alphabetic characters between current
;;; typein position and end of line, return nil.

(DEFUN-RH RH-SEARCH-FORWARD-ALPHABETIC (POS)
  (DO ((FILL-POINTER (RH-FILL-POINTER))
       (I POS (1+ I)))
      ((= I FILL-POINTER) NIL)
    (IF (RH-ALPHABETIC? I) (RETURN I))))

(DEFUN-RH RH-SEARCH-FORWARD-NON-ALPHABETIC (POS)
  (DO ((FILL-POINTER (RH-FILL-POINTER))
       (I POS (1+ I)))
      ((= I FILL-POINTER) NIL)
    (IF (NOT (RH-ALPHABETIC? I)) (RETURN I))))

(DEFUN-RH RH-SEARCH-BACKWARD-ALPHABETIC (POS)
  (DO ((I (1- POS) (1- I)))
      ((= I -1) NIL)
    (IF (RH-ALPHABETIC? I) (RETURN I))))

(DEFUN-RH RH-SEARCH-BACKWARD-NON-ALPHABETIC (POS)
  (DO ((I (1- POS) (1- I)))
      ((= I -1) NIL)
    (IF (NOT (RH-ALPHABETIC? I)) (RETURN I))))

;;; Search for a point N words away and return that point.
;;; If on an alphabetic character, skip to the first non alphabetic one.
;;; If on a non-alphabetic, skip over non-alphabetics and then over alphabetics,
;;; If no alphabetics follow the non-alphabetics, then don't move at all.

(DEFUN-RH RH-SEARCH-FORWARD-WORD (N &OPTIONAL (POS (RH-TYPEIN-POINTER)))
  (DO ((SEARCH-POS)
       (I 0 (1+ I)))
      ((= I N) POS)
    (COND ((RH-ALPHABETIC? POS)
           (SETQ POS (RH-SEARCH-FORWARD-NON-ALPHABETIC POS)))
          (T (SETQ SEARCH-POS (RH-SEARCH-FORWARD-ALPHABETIC POS))
             (IF (NOT SEARCH-POS) (RETURN POS))
             (SETQ POS (RH-SEARCH-FORWARD-NON-ALPHABETIC SEARCH-POS))))
    ;;If within a word and can't find whitespace, leave at right end.
    (IF (NOT POS) (RETURN (RH-FILL-POINTER)))))

;;; Search for a point N words back and return that point.
;;; If on an alphabetic character, skip to the character just following the
;;; first non-alphabetic one.  If on a non-alphabetic, skip over non-alphabetics
;;; and then over alphabetics.  If no alphabetics after non-alphabetics, then
;;; don't move at all.  Treat cursor on first character of a word as a special case.

(DEFUN-RH RH-SEARCH-BACKWARD-WORD (N &OPTIONAL (POS (RH-TYPEIN-POINTER)))
  (DO ((SEARCH-POS)
       (I 0 (1+ I)))
      ((= I N) POS)
    (COND
      ;;At beginning of line -- punt
      ((= POS 0) (RETURN 0))
      ;;Inside a word but not at the beginning of a word.
      ((AND (OR (= POS (RH-FILL-POINTER))
                (RH-ALPHABETIC? POS))
            (RH-ALPHABETIC? (1- POS)))
       (SETQ POS (RH-SEARCH-BACKWARD-NON-ALPHABETIC POS)))
      ;;Within whitespace or at beginning of a word.
      (T (SETQ SEARCH-POS (IF (AND ( POS (RH-FILL-POINTER)) (RH-ALPHABETIC? POS))
                              (1- POS) POS))
         (SETQ SEARCH-POS (RH-SEARCH-BACKWARD-ALPHABETIC SEARCH-POS))
         (IF (NOT SEARCH-POS) (RETURN POS))
         (SETQ POS (RH-SEARCH-BACKWARD-NON-ALPHABETIC SEARCH-POS))))
    ;;If within a word and can't find whitespace, leave at left end.
    (IF (NOT POS) (RETURN 0))
    ;;Leave cursor on first character of the word
    (INCF POS)
    ))

(DEFINE-RH-COMMAND RH-COM-FORWARD-WORD (#/META-F) (N)
  (RH-SET-POSITION (RH-SEARCH-FORWARD-WORD N)))

(DEFINE-RH-COMMAND RH-COM-BACKWARD-WORD (#/META-B) (N)
  (RH-SET-POSITION (RH-SEARCH-BACKWARD-WORD N)))

(DEFINE-RH-COMMAND RH-COM-DELETE-WORD (#/META-D) (N)
  (RH-DELETE-STRING (RH-TYPEIN-POINTER) (RH-SEARCH-FORWARD-WORD N)))

(DEFINE-RH-COMMAND RH-COM-RUBOUT-WORD (#/META-RUBOUT) (N)
  (RH-DELETE-STRING (RH-SEARCH-BACKWARD-WORD N) (RH-TYPEIN-POINTER)))

(DEFINE-RH-COMMAND RH-COM-EXCHANGE-WORDS (#/META-T) (N)
  (COND ((ZEROP N)
         (IF *RUBOUT-HANDLER-MARK*
             (RH-EXCHANGE-WORDS ':TO-MARK)
           (SEND SELF :BEEP)))
        ((MINUSP N)
         (DOTIMES (I (ABS N))
           (RH-EXCHANGE-WORDS ':BACKWARD)))
        (T
         (DOTIMES (I N)
           (RH-EXCHANGE-WORDS ':FORWARD))))
  T)

(DEFUN-RH RH-EXCHANGE-WORDS (TYPE)
  (LET* ((THISWORDBEG (RH-SEARCH-BACKWARD-WORD 1))
         (THISWORDEND (RH-SEARCH-FORWARD-WORD 1 THISWORDBEG))
         (THISWORD (MAKE-STRING (- THISWORDEND THISWORDBEG)))
         OTHERWORDBEG OTHERWORDEND OTHERWORD)
    (replace thisword rubout-handler-buffer :start2 thiswordbeg :end2 thiswordend)
    (ECASE TYPE
      (:BACKWARD
       (SETQ OTHERWORDBEG (RH-SEARCH-BACKWARD-WORD 1 THISWORDBEG)
             OTHERWORDEND (RH-SEARCH-FORWARD-WORD 1 OTHERWORDBEG)))
      (:FORWARD
       (SETQ OTHERWORDEND (RH-SEARCH-FORWARD-WORD 1 THISWORDEND)
             OTHERWORDBEG (RH-SEARCH-BACKWARD-WORD 1 OTHERWORDEND)))
      (:TO-MARK
       (SETQ OTHERWORDBEG (RH-SEARCH-BACKWARD-WORD 1 *RUBOUT-HANDLER-MARK*)
             OTHERWORDEND (RH-SEARCH-FORWARD-WORD 1 OTHERWORDBEG))))
    (SETQ OTHERWORD (MAKE-STRING (- OTHERWORDEND OTHERWORDBEG)))
    (replace otherword rubout-handler-buffer :start2 otherwordbeg :end2 otherwordend)
    (IF (> THISWORDBEG OTHERWORDBEG)
        (PROGN
          (RH-DELETE-STRING THISWORDBEG THISWORDEND NIL)
          (SETF (RH-TYPEIN-POINTER) THISWORDBEG)
          (RH-INSERT-STRING OTHERWORD 0 NIL T NIL)
          (RH-DELETE-STRING OTHERWORDBEG OTHERWORDEND NIL)
          (SETF (RH-TYPEIN-POINTER) OTHERWORDBEG)
          (RH-INSERT-STRING THISWORD 0 NIL T NIL)
          (RH-SET-POSITION (+ OTHERWORDBEG (LENGTH THISWORD))))
        (PROGN
          (RH-DELETE-STRING OTHERWORDBEG OTHERWORDEND NIL)
          (SETF (RH-TYPEIN-POINTER) OTHERWORDBEG)
          (RH-INSERT-STRING THISWORD 0 NIL T NIL)
          (RH-DELETE-STRING THISWORDBEG THISWORDEND NIL)
          (SETF (RH-TYPEIN-POINTER) THISWORDBEG)
          (RH-INSERT-STRING OTHERWORD 0 NIL T NIL)
          (RH-SET-POSITION OTHERWORDEND)))))

;;;; Balanced Parentheses

;;; Try to move over sets of nested parentheses.  First value is the new position.  Second
;;; return value says if we succeeded or not.

(DEFUN RH-SEARCH-FORWARD-PARENS (POS &OPTIONAL (TOPLEVEL? T) &AUX NEW-POS)
  (IF (AND
        ;; There must be an open after the cursor.
        (SETQ NEW-POS (RH-SEARCH-CHAR #/( POS))
        ;; It must appear before the first close, unless toplevel.
        (OR TOPLEVEL?
            (< NEW-POS (OR (RH-SEARCH-CHAR #/) POS) MOST-POSITIVE-FIXNUM)))
        ;; Begin recursive searches after the first open paren.
        (INCF NEW-POS)
        ;; Jump over any parens in between.
        (DO ((CHANGE? T))
            ((NOT CHANGE?) T)
          (MULTIPLE-VALUE (NEW-POS CHANGE?) (RH-SEARCH-FORWARD-PARENS NEW-POS NIL)))
        ;; Move to the balancing close.
        (SETQ NEW-POS (RH-SEARCH-CHAR #/) NEW-POS)))
      ;; Leave ourselves after it.
      (VALUES (1+ NEW-POS) T)
      ;; Leave ourselves where we were.
      (VALUES POS NIL)))

(DEFUN RH-SEARCH-BACKWARD-PARENS (POS &OPTIONAL (TOPLEVEL? T) &AUX NEW-POS)
  (IF (AND
        ;; There must be a close before the cursor.
        (SETQ NEW-POS (RH-REVERSE-SEARCH-CHAR #/) POS))
        ;; It must appear after the first open, unless toplevel.
        (OR TOPLEVEL?
            (> NEW-POS (OR (RH-REVERSE-SEARCH-CHAR #/( POS) -1)))
        ;; Jump over any parens in between.
        (DO ((CHANGE? T))
            ((NOT CHANGE?) T)
          (MULTIPLE-VALUE (NEW-POS CHANGE?) (RH-SEARCH-BACKWARD-PARENS NEW-POS NIL)))
        ;; Move to the balancing open.
        (SETQ NEW-POS (RH-REVERSE-SEARCH-CHAR #/( NEW-POS)))
      ;; Leave ourselves on it.
      (VALUES NEW-POS T)
      ;; Leave ourselves where we were.
      (VALUES POS NIL)))

;;; ***These have been made obsolete by the S-expression commands on the next page***

#|
(DEFINE-RH-COMMAND RH-COM-FORWARD-PARENTHESES (#/CONTROL-META-F) (IGNORE)
  (RH-SET-POSITION (RH-SEARCH-FORWARD-PARENS (RH-TYPEIN-POINTER))))

(DEFINE-RH-COMMAND RH-COM-BACKWARD-PARENTHESES (#/CONTROL-META-B) (IGNORE)
  (RH-SET-POSITION (RH-SEARCH-BACKWARD-PARENS (RH-TYPEIN-POINTER))))

(DEFINE-RH-COMMAND RH-COM-DELETE-PARENTHESES (#/CONTROL-META-K) (IGNORE)
  (RH-DELETE-STRING (RH-TYPEIN-POINTER) (RH-SEARCH-FORWARD-PARENS (RH-TYPEIN-POINTER))))

(DEFINE-RH-COMMAND RH-COM-RUBOUT-PARENTHESES (#/CONTROL-META-RUBOUT) (IGNORE)
  (RH-DELETE-STRING (RH-SEARCH-BACKWARD-PARENS (RH-TYPEIN-POINTER)) (RH-TYPEIN-POINTER)))

|#

;;;S-expression commands

;;; Try to move over complete LISP forms.  First value is the new position.  Second
;;; return value says if we succeeded or not.

(defun-rh rh-search-forward-sexp (pos)
  (declare (values new-pos success))
  (let* ((buffer rubout-handler-buffer)
         (end (length buffer))
         (new-pos (1- pos))
         (c))
    (if (>= pos (1- end))
        (values pos nil)
      (labels ((new-c ()
                 (unless (= new-pos end)
                   (incf new-pos)
                   (setq c (elt buffer new-pos))))
               (skip-atom ()
                 (loop
                   (when (= new-pos end)
                     (return))
                   (cond ((memq c '(#// #/\))
                          (new-c)
                          (new-c))
                         ((eq c #/|)
                          (skip-string #/|)
                          (new-c))
                         ((memq (zwei:char-syntax c zwei:*list-syntax-table*) `(,zwei:list-alphabetic ,zwei:list-colon))
                          (new-c))
                         (t
                          (decf new-pos)
                          (return)))))
               (skip-string (delim)
                 (loop
                   (when (= new-pos end)
                     (return))
                   (new-c)
                   (when (eq c delim)
                     (return))))
               (skip-list ()
                 (loop
                   (when (= new-pos end)
                     (return))
                   (new-c)
                   (when (= c #/()
                     (skip-list)
                     (new-c))
                   (when (= c #/))
                     (return))))
               )
        (loop                                   ;Skip initial whitespace
          (when (= new-pos end)
            (return-from rh-search-forward-sexp (values pos nil)))
          (new-c)
          (unless (or (member c '(#/space #/tab #/return))
                      (eq (zwei:char-syntax c zwei:*list-syntax-table*) zwei:list-single-quote))
            (return)))
        (cond ((eq c #/")
               (skip-string #/"))
              ((eq c #/()
               (skip-list))
              ((eq c #/)))
              (t
               (skip-atom)))
        (values (min (1+ new-pos) end) t)))))

(defun-rh rh-search-backward-sexp (pos &optional (skip-leading-whitespace t))
  (declare (values new-pos success))
  (if (zerop pos)
      (values pos nil)
    (let* ((buffer rubout-handler-buffer)
           (new-pos pos)
           (c))
      (labels ((new-c ()
                 (unless (zerop new-pos)
                   (decf new-pos))
                      (setq c (elt buffer new-pos)))
               (skip-atom ()
                 (loop
                   (cond ((eq c #/|)
                          (skip-string #/|)
                          (if (zerop new-pos)
                              (return)
                            (new-c)))
                         ((memq (zwei:char-syntax c zwei:*list-syntax-table*)
                                `(,zwei:list-alphabetic ,zwei:list-colon ,zwei:list-slash))
                          (if (zerop new-pos)
                              (return)
                            (new-c)))
                         (t
                          (unless (zerop new-pos)
                            (new-c)
                            (unless (memq c '(#// #/\))
                              (incf new-pos)))
                          (incf new-pos)
                          (return)))))
               (skip-string (delim)
                 (loop
                   (when (zerop new-pos)
                     (return))
                   (new-c)
                   (when (eq c delim)
                     (return))))
               (skip-list ()
                 (loop
                   (when (zerop new-pos)
                     (return))
                   (new-c)
                   (when (= c #/))
                     (skip-list)
                     (new-c))
                   (when (= c #/()
                     (return))))
               )
        (loop                                   ;Skip trailing whitespace
          (when (zerop new-pos)
            (return))
          (new-c)
          (unless (member c '(#/space #/tab #/return))
            (return)))
        (cond ((eq c #/")
               (skip-string #/"))
              ((eq c #/))
               (skip-list))
              ((eq c #/())
              (t
               (skip-atom)))
        (loop
          (when (zerop new-pos)
            (return))
          (new-c)
          (when (or (member c '(#/space #/tab #/return))
                    (neq (zwei:char-syntax c zwei:*list-syntax-table*) zwei:list-single-quote))
            (incf new-pos)
            (return)))
        (when skip-leading-whitespace
          (loop                                 ;Skip leading whitespace
            (when (zerop new-pos)
              (return))
            (new-c)
            (unless (member c '(#/space #/tab #/return))
              (incf new-pos)
              (return))))
        (values (max new-pos 0) t)))))

(define-rh-command rh-com-forward-sexp (#/Control-meta-f) (ignore)
  (rh-set-position (rh-search-forward-sexp (rh-typein-pointer))))

(define-rh-command rh-com-backward-sexp (#/Control-meta-b) (ignore)
  (rh-set-position (rh-search-backward-sexp (rh-typein-pointer))))

(define-rh-command rh-com-delete-sexp (#/Control-meta-k) (ignore)
  (rh-delete-string (rh-typein-pointer) (rh-search-forward-sexp (rh-typein-pointer))))

(define-rh-command rh-com-rubout-sexp (#/Control-meta-rubout) (ignore)
  (rh-delete-string (rh-search-backward-sexp (rh-typein-pointer)) (rh-typein-pointer)))

(define-rh-command rh-com-exchange-sexp (#/control-meta-t) (ignore)
  (let* ((thissexpbeg (rh-search-backward-sexp (rh-typein-pointer) nil))
         (thissexpend (rh-search-forward-sexp thissexpbeg))
         (othersexpend (rh-search-forward-sexp (rh-typein-pointer)))
         (othersexpbeg (rh-search-backward-sexp othersexpend nil))
         (thissexp (make-string (- thissexpend thissexpbeg)))
         (othersexp (make-string (- othersexpend othersexpbeg))))
    (replace thissexp rubout-handler-buffer :start2 thissexpbeg :end2 thissexpend)
    (replace othersexp rubout-handler-buffer :start2 othersexpbeg :end2 othersexpend)
    (rh-delete-string othersexpbeg othersexpend nil)
    (setf (rh-typein-pointer) othersexpbeg)
    (rh-insert-string thissexp 0 nil t nil)
    (rh-delete-string thissexpbeg thissexpend nil)
    (setf (rh-typein-pointer) thissexpbeg)
    (rh-insert-string othersexp 0 nil t nil)
    (rh-set-position othersexpend)))

;;;; Input/Kill History

(DEFINE-RH-COMMAND RH-COM-YANK-INPUT (#/CONTROL-META-Y #/CONTROL-C) (NUMERIC-ARG)
  (LET ((HISTORY (RH-INPUT-HISTORY)))
    (IF (EQ NUMERIC-ARG 0)
        (PROGN
          (WHEN (EQ *LAST-COMMAND-TYPE* HISTORY)
            (SETQ *CURRENT-COMMAND-TYPE* HISTORY))
          (RH-DISPLAY-INFO
            (ZWEI:LIST-HISTORY-CONTENTS HISTORY SELF
                                        0 ZWEI:*HISTORY-MENU-LENGTH*)))
      (RH-YANK-FROM-HISTORY
        (ZWEI:HISTORY-ELEMENT-SET-YANK-POINTER HISTORY NUMERIC-ARG) NIL)
      (SETQ *CURRENT-COMMAND-TYPE* HISTORY)))
  T)

(DEFINE-RH-COMMAND RH-COM-YANK (#/CONTROL-Y) (NUMERIC-ARG)
  (IF (EQ NUMERIC-ARG 0)
      (PROGN
        (WHEN (EQ *LAST-COMMAND-TYPE* ZWEI:*KILL-HISTORY*)
          (SETQ *CURRENT-COMMAND-TYPE* ZWEI:*KILL-HISTORY*))
        (RH-DISPLAY-INFO
          (ZWEI:LIST-HISTORY-CONTENTS ZWEI:*KILL-HISTORY* SELF
                                      0 ZWEI:*HISTORY-MENU-LENGTH*)))
    (RH-YANK-FROM-HISTORY
      (ZWEI:HISTORY-ELEMENT-SET-YANK-POINTER ZWEI:*KILL-HISTORY* NUMERIC-ARG) NIL)
    (SETQ *CURRENT-COMMAND-TYPE* ZWEI:*KILL-HISTORY*))
  T)

(DEFUN-RH RH-YANK-FROM-HISTORY (THING &OPTIONAL
                                   (KILL-PREVIOUS (TYPEP *LAST-COMMAND-TYPE* 'ZWEI:HISTORY)))
  (WHEN KILL-PREVIOUS
    (RH-DELETE-STRING (MIN (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
                      (MAX (RH-TYPEIN-POINTER) *RUBOUT-HANDLER-MARK*)
                      NIL))
  (LET ((POS (RH-TYPEIN-POINTER)))
    (WHEN THING
      (LET ((STRING (IF (ARRAYP THING) THING (ZWEI:STRING-INTERVAL THING))))
        (RH-INSERT-STRING (STRING-REMOVE-FONTS STRING) 0 NIL T NIL)))
    (SETQ *RUBOUT-HANDLER-MARK* POS)))

(DEFINE-RH-COMMAND RH-COM-YANK-POP (#/META-Y #/META-C) (N)
  (WHEN (TYPEP *LAST-COMMAND-TYPE* 'ZWEI:HISTORY)
    (RH-YANK-FROM-HISTORY
      (AND (NEQ N 0)
           (ZWEI:ROTATE-HISTORY-YANK-POINTER *LAST-COMMAND-TYPE*
                                             (OR N 1))))
    (SETQ *CURRENT-COMMAND-TYPE* *LAST-COMMAND-TYPE*))
  T)

(DEFINE-RH-COMMAND RH-COM-DISPLAY-INPUT-HISTORY (#/STATUS) (IGNORE)
  (RH-DISPLAY-INFO
    (ZWEI:LIST-HISTORY-CONTENTS (RH-INPUT-HISTORY) SELF 0 ZWEI:*HISTORY-MENU-LENGTH*)))

(DEFINE-RH-COMMAND RH-COM-DISPLAY-KILL-HISTORY (#/CONTROL-STATUS) (IGNORE)
  (RH-DISPLAY-INFO
    (ZWEI:LIST-HISTORY-CONTENTS ZWEI:*KILL-HISTORY* SELF
                                0 ZWEI:*HISTORY-MENU-LENGTH*)))

(DEFINE-RH-COMMAND RH-COM-REST-OF-INPUT-HISTORY (#/META-STATUS) (N)
  (RH-DISPLAY-INFO
    (ZWEI:LIST-HISTORY-CONTENTS (RH-INPUT-HISTORY) SELF (IF (= N 1) ZWEI:*HISTORY-MENU-LENGTH* N)
                                (DONT-OPTIMIZE (ZWEI:HISTORY-LENGTH (RH-INPUT-HISTORY))))))

(DEFINE-RH-COMMAND RH-COM-REST-OF-KILL-HISTORY (#/CONTROL-META-STATUS) (N)
  (RH-DISPLAY-INFO
    (ZWEI:LIST-HISTORY-CONTENTS ZWEI:*KILL-HISTORY* SELF
                                (IF (= N 1) ZWEI:*HISTORY-MENU-LENGTH* N)
                                (DONT-OPTIMIZE (ZWEI:HISTORY-LENGTH ZWEI:*KILL-HISTORY*)))))

(define-rh-command rh-com-complete-from-history (#/control-!) (skip)
  (let* ((start 0)
         (end  (rh-typein-pointer))
         (match-string (subseq rubout-handler-buffer 0 (rh-typein-pointer)))
         (number (do-named lookup ((count 0 (1+ count))
                      (reject (1- skip) reject)
                      (tail  (zwei::history-list (rh-input-history)) (rest tail)))
                     ((null tail) nil)
                   (when (equalp (subseq (car tail) start end) match-string)
                     (when (zerop reject)
                       (return-from lookup (1+ count)))
                     (decf reject)))))
    (if number
        (progn (rh-delete-string start end nil)
               (rh-yank-from-history
                 (zwei::history-element-set-yank-pointer (rh-input-history) number))
               (setq *current-command-type* (list 'complete-from-history (1- number) match-string
                                                  (rh-input-history)))
               t)
      (send self :beep))))

(defun get-next-match (history match direction)
  (let ((length (length match)))
    (do ((element (zwei::rotate-history-yank-pointer history direction)
                  (zwei::rotate-history-yank-pointer history direction)))
        (())
    (when (equalp (subseq element 0 length) match)
      (return-from get-next-match element)))))

(defun get-match (history match argument)
  (dotimes (count (abs argument) (zwei::history-element-set-yank-pointer history nil))
    (get-next-match history match (if (minusp argument) -1 1))))

(define-rh-command rh-com-complete-from-history-next-completion (#/meta-!) (argument)
  (if (and (consp *last-command-type*)
           (eq (car *last-command-type*) 'complete-from-history))
      (let ((match (get-match (fourth *last-command-type*) (third *last-command-type*) argument)))
        (rh-delete-string 0 (rh-typein-pointer))
        (rh-yank-from-history match)
        (setq *current-command-type* *last-command-type*)
                 t)
    (send self :beep)))

;;; The convert-to-a-string function for the input history.
(DEFUN SUMMARIZE-INPUT-STRING (STRING)
  (LET* ((NIL-INDEX (POSITION NIL STRING))
         (TRUNCATED-STRING (IF NIL-INDEX (SUBSEQ STRING 0 NIL-INDEX) STRING))
         (NEWLINE-INDEX (POSITION #/NEWLINE TRUNCATED-STRING)))
    (IF NEWLINE-INDEX
        (CONCATENATE 'STRING (SUBSEQ STRING 0 NEWLINE-INDEX) " ...")
      (COERCE TRUNCATED-STRING 'STRING))))

;;;; Self Documentation

(DEFCONST RH-HELP-INFO
  "You are typing input to ~A.
Click the rightmost mouse button to select a menu of programs and window operations.
Type Control-~C for a list of commands for editing input.
Type Meta-~C for a list of special symbols.
Type ~C ~C for a list of programs.
Type ~C ~C for a list of console operations.~2%")

(DEFINE-RH-COMMAND RH-COM-BASIC-HELP (#/HELP) (IGNORE)
  (RH-DISPLAY-INFO
    (FORMAT SELF RH-HELP-INFO SELF #/HELP #/HELP #/SYSTEM #/HELP #/TERMINAL #/HELP)))

;;; Help for explorer keyboard which doesn't have the greek letters.
(DEFINE-RH-COMMAND RH-COM-LIST-SPECIAL-SYMBOLS (#/Meta-help) (IGNORE)
  (if (send-if-handles self :no-inferior-windows-p)
      (rh-display-info
        (rh-print-special-symbol-help self))
    (LET ((RUBOUT-HANDLER NIL))
      (SI:WITH-HELP-STREAM (HELP-WINDOW :LABEL "Special Keyboard Symbols" :width *width
                                        :SUPERIOR
                                        (IF (TYPEP SELF 'SHEET)
                                            (SHEET-GET-SCREEN SELF)
                                          SELF))
        (rh-print-special-symbol-help help-window width)))))

(defun rh-print-special-symbol-help (help-window &optional width)
  (cond (width)
        ((operation-handled-p help-window :size-in-characters)
         (setq width (send help-window :size-in-characters)))
        (t
         (setq width 85.)))
  (format help-window "Special Symbol Translations:~2%")
  (rh-print-help-double-columns (select-processor
                                  ((:lambda :cadr)
                                   '((#/ "Greek-a")
                                     (#/ "Greek-b")
                                     (#/
 "Greek-d")
                                     (#/ "Greek-e")
                                     (#/         "Greek-g")
                                     (#/ "Greek-l")
                                     (#/ "Greek-p")
                                     (#/  "Greek-'")
                                     (#/ "Greek-//")
                                     (#/ "Greek-`")
                                     (#/ "Greek-Hand-Down")
                                     (#/ "Greek-Hand-Left")
                                     (#/ "Top-q")
                                     (#/ "Top-w")
                                     (#/ "Top-e")
                                     (#/ "Top-r")
                                     (#/ "Top-t")
                                     (#/ "Top-y")
                                     (#/ "Top-u")
                                     (#/ "Top-i")
                                     (#/ "Top-o")
                                     (#/ "Top-p")
                                     (#/ "Top-g")
                                     (#/ "Top-h")
                                     (#/ "Top-j")
                                     (#/ "Top-k")
                                     (#/ "Top-l")
                                     (#/ "Top-c")
                                     (#/ "Top-m")
                                     (#/ "Top-n")
                                     (#/ "Top-b")))
                                  (:explorer
                                   '((#/ "Symbol-a")
                                     (#/ "Symbol-b")
                                     (#/
 "Symbol-d")
                                     (#/ "Symbol-e")
                                     (#/         "Symbol-g")
                                     (#/ "Symbol-l")
                                     (#/ "Symbol-p")
                                     (#/  "Symbol-'")
                                     (#/ "Symbol-//")
                                     (#/ "Shift-Symbol-q")
                                     (#/ "Shift-Symbol-w")
                                     (#/ "Shift-Symbol-e")
                                     (#/ "Shift-Symbol-r")
                                     (#/ "Shift-Symbol-t")
                                     (#/ "Shift-Symbol-y")
                                     (#/ "Shift-Symbol-u")
                                     (#/ "Shift-Symbol-i")
                                     (#/ "Shift-Symbol-o")
                                     (#/ "Shift-Symbol-p")
                                     (#/ "Shift-Symbol-g")
                                     (#/ "Shift-Symbol-h")
                                     (#/ "Shift-Symbol-j")
                                     (#/ "Shift-Symbol-k")
                                     (#/ "Shift-Symbol-l")
                                     (#/ "Shift-Symbol-c")
                                     (#/ "Shift-Symbol-m")
                                     (#/ "Shift-Symbol-n")
                                     (#/ "Shift-Symbol-b"))))
                                help-window
                                width)
  (terpri help-window))

;;; Prints a list of the rubout handler commands in the order in which they appear on
;;; the command alist.  #/CLEAR-SCREEN prints as <PAGE>.

(DEFUN RH-MAKE-COMMAND-NAME (SYMBOL)
  (STRING-CAPITALIZE-WORDS
    (IF (STRING-EQUAL SYMBOL "RH-COM-" :END1 7)
        (SUBSTRING (GET-PNAME SYMBOL) 7)
      (GET-PNAME SYMBOL))))

(DEFINE-RH-COMMAND RH-COM-LIST-COMMANDS (#/CONTROL-HELP) (IGNORE)
  (if (send-if-handles self :no-inferior-windows-p)
      (rh-display-info
        (rh-print-full-help self))
    (LET ((RUBOUT-HANDLER NIL))
      (SI:WITH-HELP-STREAM (HELP-WINDOW :LABEL "Input Editor Commands" :WIDTH *WIDTH
                                        :SUPERIOR
                                        (IF (TYPEP SELF 'SHEET)
                                            (SHEET-GET-SCREEN SELF)
                                          SELF))
        (rh-print-full-help help-window width)))))

(defun rh-print-full-help (help-window &optional width)
  (cond (width)
        ((operation-handled-p help-window :size-in-characters)
         (setq width (send help-window :size-in-characters)))
        (t
         (setq width 85.)))
  (LET* ((EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND RUBOUT-HANDLER-OPTIONS))))
    (FORMAT HELP-WINDOW "Input Editor Commands:~@
                    Control-number and Control-U provide numeric argument.~2%")
    ;; Print double column list of commands.
    (RH-PRINT-HELP-DOUBLE-COLUMNS RH-COMMAND-ALIST HELP-WINDOW WIDTH
                                  (APPEND (CDR (ASSQ ':PASS-THROUGH RUBOUT-HANDLER-OPTIONS))
                                          EDITING-COMMAND))
    (WHEN EDITING-COMMAND
      (FORMAT HELP-WINDOW "~%Additional commands available right now:~2%")
      (RH-PRINT-HELP-DOUBLE-COLUMNS EDITING-COMMAND HELP-WINDOW WIDTH)))
  (TERPRI HELP-WINDOW))

(DEFUN RH-PRINT-HELP-DOUBLE-COLUMNS (ALIST HELP-WINDOW WIDTH &OPTIONAL EXCEPT)
  (IF (< WIDTH 80.)
      (DOLIST (ELT ALIST)
        (LET ((CH (IF (CONSP ELT) (CAR ELT) ELT))
              DOC)
          (UNLESS (OR (MEMQ CH EXCEPT)
                      (SI:ASSQ-CAREFUL CH EXCEPT))
            (SETQ DOC (IF (CONSP ELT)
                          (IF (SYMBOLP (CDR ELT)) (RH-MAKE-COMMAND-NAME (CDR ELT))
                            (CADR ELT))
                        "exits the command editor"))
            (FORMAT HELP-WINDOW "~:C~21T~A~%" CH DOC))))
    (LET (LIST)
      (DOLIST (ELT ALIST)
        (LET ((CH (IF (CONSP ELT) (CAR ELT) ELT))
              DOC)
          (UNLESS (OR (MEMQ CH EXCEPT)
                      (SI:ASSQ-CAREFUL CH EXCEPT))
            (SETQ DOC (IF (CONSP ELT)
                          (IF (SYMBOLP (CDR ELT)) (RH-MAKE-COMMAND-NAME (CDR ELT))
                            (CADR ELT))
                        "exits the command editor"))
            (PUSH (CONS CH DOC) LIST))))
      (SETQ LIST (NREVERSE LIST))
      (LET* ((LENGTH (LENGTH LIST))
             (HALF-LENGTH (CEILING LENGTH 2)))
        (LOOP FOR ELT1 IN LIST
              FOR ELT2 IN (NTHCDR HALF-LENGTH LIST)
              DO (FORMAT HELP-WINDOW "~:C~13T~A~37T~:C~58T~A~%"
                         (CAR ELT1) (CDR ELT1) (CAR ELT2) (CDR ELT2)))
        ;; If an odd command left over, print it too.
        (IF (ODDP LENGTH)
            (LET ((ELT (NTH (TRUNCATE LENGTH 2) LIST)))
              (FORMAT HELP-WINDOW "~:C~13T~A~%"
                      (CAR ELT) (CDR ELT))))))))

;;; Display state information relating to the rubout handler: the entries in the two
;;; element array leader, the entry in the TYPEIN-POINTER hash table, and the options to
;;; the call to :RUBOUT-HANDLER.  This is setup up as rubout handler command rather
;;; than as a simple function since it must be invoked from within the context of the
;;; rubout handler.

(DEFINE-RH-COMMAND RH-COM-DISPLAY-INTERNAL-STATE (#/CONTROL-META-HELP) (IGNORE)
  (RH-DISPLAY-INFO
    (FORMAT SELF "Terminal I//O:~17T~S~%" *TERMINAL-IO*)
    (FORMAT SELF "Pointer Values:~17T~
                  Fill pointer = ~D, Scan pointer = ~D, Typein pointer = ~D~%"
            (RH-FILL-POINTER) (RH-SCAN-POINTER) (RH-TYPEIN-POINTER))
    (FORMAT SELF "Rubout handler buffer size = ~D, Status = ~S~%"
            (ARRAY-LENGTH RUBOUT-HANDLER-BUFFER) (RHB-STATUS RUBOUT-HANDLER-BUFFER))
    (FORMAT SELF "Options:~17T~S" RUBOUT-HANDLER-OPTIONS)))


;;;; Randomness

(DEFINE-RH-COMMAND RH-COM-QUOTE-CHARACTER (#/CONTROL-Q) (N)
  (LET ((CH (LET ((RUBOUT-HANDLER NIL))
              (SEND SELF :TYI))))
    (RH-INSERT-CHAR (COND ((ZEROP (CHAR-BITS CH))
                           CH)
                          (( (CHAR-INT #/A)
                              (SETQ CH (CHAR-UPCASE (CHAR-CODE CH)))
                              (CHAR-INT #/Z))
                           (- CH (CHAR-INT #/A) -1))
                          (T
                           (SEND SELF :BEEP)
                           (RETURN-FROM RH-COM-QUOTE-CHARACTER NIL)))
                    N T)))

(DEFINE-RH-COMMAND RH-COM-TWIDDLE-CHARACTERS (#/CONTROL-T) (IGNORE)
  (IF (< (RH-FILL-POINTER) 2)
      (PROGN (SEND SELF :BEEP) NIL)
    (LET ((DELETE-POINTER (RH-TYPEIN-POINTER))
          (STRING (MAKE-STRING 2)))
      ;; At end of line, go back two chars; in middle of line, one; at beginning, none.
      (DECF DELETE-POINTER (COND ((= DELETE-POINTER 0) 0)
                                 ((= DELETE-POINTER (RH-FILL-POINTER)) 2)
                                 ((char= (int-char (aref rubout-handler-buffer delete-pointer))
                                         #/newline)
                                  2)
                                 (T 1)))
      (SETF (CHAR STRING 0) (INT-CHAR (AREF RUBOUT-HANDLER-BUFFER DELETE-POINTER))
            (CHAR STRING 1) (INT-CHAR (AREF RUBOUT-HANDLER-BUFFER (1+ DELETE-POINTER))))
      (RH-DELETE-STRING DELETE-POINTER (+ DELETE-POINTER 2) NIL)
      (SETQ STRING (NREVERSE STRING))
      (RH-INSERT-STRING STRING))))

(DEFINE-RH-COMMAND RH-COM-OPEN-LINE (#/CONTROL-O) (N)
  (RH-INSERT-CHAR #/NEWLINE N T)
  (RH-SET-POSITION (- (RH-TYPEIN-POINTER) N))
  T)

;;; Call READ-FROM-STRING instead of INTERN so as to get the package prefix right.
;;; Extend this to know about other function specs?  See what people want.
;;; Typing C-Sh-A after typing "(hacks:smoking-clover" will print
;;;    SMOKING-CLOVER: (&OPTIONAL (SIZE ...) ...) rather than
;;;    HACKS:SMOKING-CLOVER (&OPTIONAL (HACKS:SIZE ...) ...)
;;; since PACKAGE will be set to HACKS after having just typed in the symbol.
;;; We should probably leave it this way since we don't want to see the package prefixes
;;; before each of the variables in the BVL.

;;; In fact, now we carefully make sure the package is right to get rid of prefixes.

(DEFUN-RH RH-GET-FUNCTION (&OPTIONAL (POS (RH-TYPEIN-POINTER)) &AUX MOVED END)
  (DO-FOREVER
    (MULTIPLE-VALUE (POS MOVED)
      (RH-SEARCH-BACKWARD-PARENS POS NIL))
    (OR MOVED (RETURN)))
  (SETQ POS (OR (RH-REVERSE-SEARCH-CHAR #/( POS)
                (RH-SEARCH-CHAR #/( 0)))
  (COND ((OR (NULL POS)
             (= (AREF RUBOUT-HANDLER-BUFFER (1+ POS)) #/) ))    ;special case for "()"
         NIL)
        (T (INCF POS)
           (SETQ END (RH-SEARCH-CHAR #/SPACE POS))
           (LET ((SI:READ-INTERN-FUNCTION 'INTERN-SOFT))
             (READ (MAKE-SEQUENCE-STREAM RUBOUT-HANDLER-BUFFER POS END) NIL)))))

(DEFUN MAKE-SEQUENCE-STREAM (SEQUENCE &OPTIONAL START END
                             &AUX STREAM
                                  (INDEX (OR START 0))
                                  (LIMIT (OR END (LENGTH SEQUENCE))))
  (SETQ STREAM #'(LAMBDA (OP &OPTIONAL ARG1 &REST ARGS)
                   (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                     (:TYI
                       (COND ((< INDEX LIMIT)
                              (PROG1 (ELT SEQUENCE INDEX)
                                     (INCF INDEX)))
                             ((> INDEX (1+ LIMIT))
                              (FERROR "after end of file on sequence stream"))
                             (T
                              (INCF INDEX)
                              ())))
                     (:UNTYI
                       (DECF INDEX))
                     (T
                       (STREAM-DEFAULT-HANDLER STREAM OP ARG1 ARGS))))))



;here are some hacks that can lead to being able to get the arglist for a method handler
;what works now is
;
;     (send terminal-io :tyo C-2 C-Sh-A
;
;you have to give it the C-2 argument since it only works in this case
;and we don't want to confuse people
;
;What doesn't work is, for example
;
;     (send terminal-io ':tyo C-Sh-A
;
;Hack away!

(defun-rh rh-nth-argument (n &optional (pos (rh-typein-pointer)) &aux moved end)
  (do-forever
    (multiple-value (pos moved)
      (rh-search-backward-parens pos nil))
    (or moved (return)))
  (setq pos (or (rh-reverse-search-char #/( pos)
                (rh-search-char #/( 0)))
  (cond ((null pos) nil)
        ((= (aref rubout-handler-buffer (1+ pos)) #/) ) nil)    ;special case for ()
        (t
         (dotimes (x n)
           (if pos (setq pos (rh-search-char #/space (1+ pos)))))
         (when pos
           (incf pos)
           (setq end (rh-search-char #/space pos))
           (let ((si:read-intern-function 'intern-soft))
             (condition-case ()
                 (cl:read-from-string rubout-handler-buffer nil nil
                                      :start pos :end end)
               (error ())))))))

(defun-rh get-handler-from-send-expression ()
  (let ((instance (rh-nth-argument 1))
        (message (rh-nth-argument 2))
        handler)
    (when (and (symbolp instance)
               (boundp instance)
               (symbolp message)
               (boundp message)
               (typep (symbol-value instance) 'instance)
               (functionp (setq handler (send (symbol-value instance)
                                              :get-handler-for (symbol-value message)))))
      handler)))

(DEFINE-RH-COMMAND RH-COM-ARGUMENT-LIST (#/CONTROL-SHIFT-A) (NUMERIC-ARG)
  (LET ((FN (RH-GET-FUNCTION)) handler)
    (IF (NULL FN)
        (SEND SELF :BEEP)
      (RH-DISPLAY-INFO
        (cond ((AND (SYMBOLP FN) (FUNCTIONP FN T))
               (cond ((and (eq 2 numeric-arg)
                           (or (eq fn 'send) (eq fn 'funcall))
                           (setq handler (get-handler-from-send-expression)))
                      (setq fn (function-name handler))))
               (ZWEI:PRINT-ARGLIST FN SELF))
              (T
               (FORMAT SELF "Can't find a definition for ~S." FN)))))))

(DEFINE-RH-COMMAND RH-COM-DOCUMENTATION (#/CONTROL-SHIFT-D) (numeric-arg)
  (LET ((FN (RH-GET-FUNCTION)) handler)
    (IF (NULL FN)
        (SEND SELF :BEEP)
      (RH-DISPLAY-INFO
        (cond ((AND (SYMBOLP FN) (FUNCTIONP FN T))
               (cond ((and (eq 2 numeric-arg)
                           (memq fn '(send lexpr-send))
                           (setq handler (get-handler-from-send-expression)))
                      (setq fn (function-name handler))))
               (ZWEI:PRINT-ARGLIST FN SELF)
               (WHEN (DOCUMENTATION FN 'FUNCTION)
                 (TERPRI SELF)
                 (SEND SELF :STRING-OUT (DOCUMENTATION FN 'FUNCTION))))
              (t
               (FORMAT SELF "Can't find a definition for ~S." FN)))))))

;;; Set things up.
(DEFUN RH-ON  () (SETQ STREAM-MIXIN-DEFAULT-RUBOUT-HANDLER 'ALTERNATE-RUBOUT-HANDLER))
(DEFUN RH-OFF () (SETQ STREAM-MIXIN-DEFAULT-RUBOUT-HANDLER 'DEFAULT-RUBOUT-HANDLER))

(RH-ON)

;; >> To do

;; Stick a before daemon on :RESTORE-RUBOUT-HANDLER-BUFFER which will reset the typein
;; pointer to the end of the line.  The correct fix is for the entire rubout handler
;; buffer structure to be saved during :SAVE-RUBOUT-HANDLER-BUFFER, i.e. for the rubout
;; handler buffer structure to have a slot added for typein pointer.  Kill history should
;; be continuous across Break/Resume.  It shouldn't be continuous across windows, since
;; we want to yank from what is displayed in the window, but there should be some
;; way to yank from other windows.

;; Optimize typing at the end of the line, as opposed to the end of the buffer.
;; Currently, we always do an :INSERT-STRING or a :CLEAR-REST-OF-LINE operation at the end
;; of a line which isn't the end of the buffer.

;; Make RH-DISPLAY-INFO-INTERNAL barf on ridiculously small windows.
;; Fix bug with insertion in the middle of the line with variable width fonts.
;; ^U ^K doesn't work.
;; Debug problem with ungetting.

;; Replace (SEND SELF :BEEP) with (RH-BARF).  Place catch tag in command
;; loop to throw to.

;; Commands to change the current font and font map.
;; Change blinker size as we move over characters.  Need stream messages for this.
;; Do some metering to see if this conses too much.
;; Think about C-N in empty buffer bug.

;; Rather than just passing numeric-arg to each function, pass in a structure with the
;; components: character, numeric-arg, rubbed-out-some, pass-through.  Then
;; Control-number, etc can be implemented by modifying this structure.  RH-INSERT-CHAR
;; would look at this structure directly.  This structure really should be allocated on
;; the stack.  I'll probably end up doing this with specials.
