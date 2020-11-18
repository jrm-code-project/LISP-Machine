;-*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-

;In DEFS now.
;(DEFSTRUCT (HISTORY :NAMED-ARRAY (:CONC-NAME HISTORY-) (:CONSTRUCTOR MAKE-HISTORY-INTERNAL))
;  NAME
;  (YANK-POINTER NIL)
;  (LENGTH 0)
;  LIST
;  ELEMENT-STRING-FUNCTION
;  YANK-METHOD)

;MAKE-HISTORY defined in DEFS now.

(DEFUN HISTORY-CONTENTS (HISTORY)
  "The list of elements recorded in HISTORY, most recently added first."
  (HISTORY-LIST HISTORY))

(DEFUN CLEAR-HISTORY (HISTORY)
  "Make HISTORY be empty; discard everything recorded in it."
  (SETF (HISTORY-YANK-POINTER HISTORY) NIL)
  (SETF (HISTORY-LENGTH HISTORY) 0)
  (SETF (HISTORY-LIST HISTORY) NIL))

(DEFUN ROTATE-HISTORY-YANK-POINTER (HISTORY &OPTIONAL (DISTANCE 1))
  "Move HISTORY's yank pointer back (to older entries) by DISTANCE.
If DISTANCE is negative, the yank pointer moves to newer entries.
It wraps around from back to front or vice versa, unless
*HISTORY-YANK-WRAPAROUND* is NIL.
The value is the element that the yank pointer now points to,
or NIL if you tried to go past an end of the list or the list is empty."
  (LET ((NEWORG (+ (OR (HISTORY-YANK-POINTER HISTORY) 1) DISTANCE)))
    (AND (IF *HISTORY-YANK-WRAPAROUND*
             (UNLESS (ZEROP (HISTORY-LENGTH HISTORY))
               (SETF (HISTORY-YANK-POINTER HISTORY)
                     (1+ (MOD (1- NEWORG) (HISTORY-LENGTH HISTORY)))))
           (WHEN ( 1 NEWORG (HISTORY-LENGTH HISTORY))
             (SETF (HISTORY-YANK-POINTER HISTORY) NEWORG)))
         (NTH (1- (HISTORY-YANK-POINTER HISTORY)) (HISTORY-LIST HISTORY)))))

(DEFUN HISTORY-LATEST-ELEMENT (HISTORY)
  "The most recently recorded element of HISTORY."
  (CAR (HISTORY-LIST HISTORY)))

(DEFUN PUSH-ON-HISTORY (ELEMENT HISTORY)
  "Add ELEMENT to the front of HISTORY, unless it matches the most recent element.
Also sets HISTORY's yank-pointer to the front."
  (UNLESS (EQUAL ELEMENT (HISTORY-LATEST-ELEMENT HISTORY))
    (PUSH ELEMENT (HISTORY-LIST HISTORY))
    (INCF (HISTORY-LENGTH HISTORY))
    (SETF (HISTORY-YANK-POINTER HISTORY) 1))
  ELEMENT)

(DEFUN PUSH-REMOVE-ON-HISTORY (ELEMENT HISTORY)
  "Add ELEMENT to the front of HISTORY, removing it if it appears later on.
Also sets HISTORY's yank-pointer to the front."
  (SETF (HISTORY-LIST HISTORY)
        (CONS ELEMENT (REMQ ELEMENT (HISTORY-LIST HISTORY))))
  (SETF (HISTORY-LENGTH HISTORY) (LENGTH (HISTORY-LIST HISTORY)))
  (SETF (HISTORY-YANK-POINTER HISTORY) 1)
  ELEMENT)

(DEFUN APPEND-REMOVE-ON-HISTORY (ELEMENT HISTORY)
  "Append ELEMENT to the end of HISTORY, removing it if it appears earlier.
Does not affect HISTORY's yank-pointer."
  (SETF (HISTORY-LIST HISTORY)
        (APPEND (REMQ ELEMENT (HISTORY-LIST HISTORY)) (LIST ELEMENT)))
  (SETF (HISTORY-LENGTH HISTORY) (LENGTH (HISTORY-LIST HISTORY)))
  ELEMENT)

(DEFUN DELETE-FROM-HISTORY (ELEMENT HISTORY)
  "Delete ELEMENT from the list of things remembered in HISTORY."
  (SETF (HISTORY-LIST HISTORY)
        (DELQ ELEMENT (HISTORY-LIST HISTORY))))

(DEFUN HISTORY-ELEMENT-SET-YANK-POINTER (HISTORY INDEX)
  "Shift the yank pointer of HISTORY to INDEX and returning the element at that index.
If INDEX is NIL, the current yank pointer is used.
This is right for a Yank command with no argument.
A Yank command with a numeric arg can pass it right along."
  (WHEN INDEX
    (IF *HISTORY-ROTATE-IF-NUMERIC-ARG*
        (SETF (HISTORY-YANK-POINTER HISTORY)
              (IF (MINUSP INDEX)
                  (+ (HISTORY-YANK-POINTER HISTORY) INDEX)
                (+ (HISTORY-YANK-POINTER HISTORY) INDEX -1)))
      (SETF (HISTORY-YANK-POINTER HISTORY)
            (IF (MINUSP INDEX)
                (+ 1 (HISTORY-LENGTH HISTORY) INDEX)
              INDEX))))
  (UNLESS (HISTORY-YANK-POINTER HISTORY)
    (SETF (HISTORY-YANK-POINTER HISTORY) 1))
  (NTH (1- (HISTORY-YANK-POINTER HISTORY))
       (HISTORY-LIST HISTORY)))

(DEFUN HISTORY-YANK (HISTORY)
  "Yank an element from history list HISTORY, according to the current editor numeric arg.
Handles args of zero (list the contents) and Control-U (leave point at the front).
This does everything you need for a yank command in the editor."
  (IF (EQ *NUMERIC-ARG* 0)
      (PROGN
        (WHEN (EQ *LAST-COMMAND-TYPE* HISTORY)
          (SETQ *CURRENT-COMMAND-TYPE* HISTORY))
        (LIST-HISTORY-CONTENTS HISTORY STANDARD-OUTPUT 0 *HISTORY-MENU-LENGTH*))
    (LET ((ELEMENT
            (HISTORY-ELEMENT-SET-YANK-POINTER HISTORY
                                              (IF (EQ *NUMERIC-ARG-P* ':CONTROL-U) 1
                                                (AND *NUMERIC-ARG-P* *NUMERIC-ARG*)))))
      (UNLESS ELEMENT (BARF))
      (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
      (FUNCALL (HISTORY-YANK-METHOD HISTORY)
               ELEMENT
               NIL (EQ *NUMERIC-ARG-P* ':CONTROL-U)))
    (SETQ *CURRENT-COMMAND-TYPE* HISTORY))
  DIS-TEXT)

(DEFCOM COM-YANK-PREVIOUS-INPUT "Yank from history of previous args of this kind." ()
  (UNLESS *MINI-BUFFER-VALUE-HISTORY*
    (BARF "There is no history of arguments of the kind you are being asked for."))
  (HISTORY-YANK *MINI-BUFFER-VALUE-HISTORY*))

(DEFUN LIST-HISTORY-CONTENTS (HISTORY &OPTIONAL (STREAM STANDARD-OUTPUT)
                              (START 0) (END *HISTORY-MENU-LENGTH*))
  "Print a summary of the elements of history list HISTORY, on STREAM.
Each element is preceded by its index, and the element at the current
yanking pointer is identified with an arrow in front of it.
START and END can be used to limit the description to just a portion of the list.
END defaults to *HISTORY-MENU-LENGTH*."
  (FORMAT STREAM
          (IF (ZEROP START) "~&Contents of ~A:~2%"
            "~%Rest of ~A:~2%")
          (HISTORY-NAME HISTORY))
  (IF (< (HISTORY-LENGTH HISTORY) (+ END 3))
      (SETQ END (HISTORY-LENGTH HISTORY)))
  (LOOP FOR I FROM (1+ START) BELOW (1+ END)
        FOR ELT IN (NTHCDR START (HISTORY-LIST HISTORY))
        DO
        (FORMAT STREAM (IF (EQ I (HISTORY-YANK-POINTER HISTORY)) " => ~2D: " "    ~2D: ")
                (IF *HISTORY-ROTATE-IF-NUMERIC-ARG*
                    (IF ( I (HISTORY-YANK-POINTER HISTORY))
                        (+ 1 (- I (HISTORY-YANK-POINTER HISTORY)))
                      (- I (HISTORY-YANK-POINTER HISTORY)))
                  I))
        (IF (SEND STREAM ':OPERATION-HANDLED-P ':ITEM)
            (FUNCALL STREAM ':ITEM 'HISTORY-ELEMENT
                     (LIST HISTORY I ELT)
                     "~A"
                     (FUNCALL (HISTORY-ELEMENT-STRING-FUNCTION HISTORY) ELT))
          (PRINC (FUNCALL (HISTORY-ELEMENT-STRING-FUNCTION HISTORY) ELT) STREAM))
        (TERPRI STREAM))
  (WHEN (> (HISTORY-LENGTH HISTORY) END)
    (TERPRI STREAM)
    (IF (SEND STREAM ':OPERATION-HANDLED-P ':ITEM)
        (SEND STREAM ':ITEM 'MORE-HISTORY-LEFT (CONS HISTORY END)
              "~D more elements in the history; click Left here to print them."
              (- (HISTORY-LENGTH HISTORY) END))
      (FORMAT STREAM "~D more elements in the history."
              (- (HISTORY-LENGTH HISTORY) END)))
    (TERPRI STREAM)))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* HISTORY-ELEMENT "Reuse"
                          TYPEOUT-REUSE-HISTORY-ELEMENT T
                          "Reuse this element of the history.")

(DEFUN TYPEOUT-REUSE-HISTORY-ELEMENT (THING)
  (SETF (HISTORY-YANK-POINTER (CAR THING)) (CADR THING))
  (FUNCALL (HISTORY-YANK-METHOD (CAR THING)) (CADDR THING))
  (SETQ *CURRENT-COMMAND-TYPE* (CAR THING))
  NIL)  ;Cause redisplay.

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* MORE-HISTORY-LEFT "Continue"
                          TYPEOUT-HISTORY-CONTINUE-LISTING T
                          "List the rest of the history.")

(DEFUN TYPEOUT-HISTORY-CONTINUE-LISTING (HISTORY-AND-INDEX)
  (LIST-HISTORY-CONTENTS (CAR HISTORY-AND-INDEX) STANDARD-OUTPUT
                         (CDR HISTORY-AND-INDEX) (HISTORY-LENGTH (CAR HISTORY-AND-INDEX))))

;; Note: yanking commands should leave *CURRENT-COMMAND-TYPE* set
;; to the history yanked from.  That way, COM-YANK-POP can pop
;; whatever history you were using.

(DEFCOM COM-YANK-POP "Correct a Yank command to get something older.
Replaces the last thing you yanked -- the text now in the region --
with another entry from the same history list.
By default, the previous entry is used, so that successive uses of this command
get older and older things from the history.
A numeric argument specifies how many entries forward or back to go;
negative means to more recently killed text.
An argument of zero means get rid of the yanked stuff and replace it with nothing."
        ()
  ;; Need not check for MARK-P, by special case.
  (OR (TYPEP *LAST-COMMAND-TYPE* 'HISTORY) (BARF))
  (FUNCALL (HISTORY-YANK-METHOD *LAST-COMMAND-TYPE*)
           (AND (NOT (ZEROP *NUMERIC-ARG*))
                (ROTATE-HISTORY-YANK-POINTER *LAST-COMMAND-TYPE*
                                             *NUMERIC-ARG*)))
  (SETQ *CURRENT-COMMAND-TYPE* *LAST-COMMAND-TYPE*)
  DIS-TEXT)

(ADD-INITIALIZATION "Clear History Lists" '(CLEAR-ALL-HISTORIES) '(:BEFORE-COLD))
(ADD-INITIALIZATION "Clear History Lists" '(CLEAR-ALL-HISTORIES) '(:gc-system-release))

(DEFUN CLEAR-ALL-HISTORIES ()
  (MAPC 'CLEAR-HISTORY *HISTORIES-TO-CLEAR*))

;; This function is the YANK-METHOD of the kill history, buffer histories, and most others.
(DEFUN YANK-AS-TEXT (THING &OPTIONAL
                     (KILL-PREVIOUS (TYPEP *LAST-COMMAND-TYPE* 'HISTORY))
                     LEAVE-POINT-BEFORE)
  "Yank THING into the buffer at point, moving point after it, leaving mark before it.
KILL-PREVIOUS non-NIL means delete the existing contents of the region first;
 this defaults to T if the previous command was a yank command.
LEAVE-POINT-BEFORE non-NIL means put point before the text and mark after.
 This happens anyway if KILL-PREVIOUS is non-NIL and point was
 at the front of the existing region.
An interval that is not a buffer is yanked by inserting its text.
A buffer is yanked by inserting its name!
A string is yanked as its text.
A symbol or list is yanked as it would print, with slashification."
  (IF KILL-PREVIOUS
      (LET ((BP1 (POINT)) (BP2 (MARK)))
        (GET-INTERVAL BP1 BP2 NIL)
        (SETQ LEAVE-POINT-BEFORE (BP-= BP1 (POINT)))
        (LET ((*BATCH-UNDO-SAVE* T)
              (UNDO-ITEM (CAR (UNDO-STATUS-UNDO-LIST
                                (NODE-UNDO-STATUS (NODE-TOP-LEVEL-NODE *INTERVAL*))))))
          ;; Don't use WITH-UNDO-SAVE.  Instead, "re-open" the undo save for the COM-YANK,
          ;; so that any sequence C-Y M-Y M-Y ... is undone as a unit.
          (SETF (BP-STATUS (UNDO-ITEM-START-BP UNDO-ITEM)) ':NORMAL)
          (SETF (BP-STATUS (UNDO-ITEM-END-BP UNDO-ITEM)) ':MOVES)
          (UNWIND-PROTECT
              (PROGN
                (DELETE-INTERVAL BP1 BP2 T)
                (MOVE-BP (MARK) BP1)
                (WHEN THING
                  (MOVE-BP (POINT) (INSERT-KILL-RING-THING (MARK) THING))
                  ))
            (UNDO-SAVE-END))))
    (WITH-UNDO-SAVE ("Yank" (POINT) (POINT) T)
      (MOVE-BP (MARK) (POINT))
      (MOVE-BP (POINT) (INSERT-KILL-RING-THING (POINT) THING))))
  (IF LEAVE-POINT-BEFORE (SWAP-BPS (POINT) (MARK))))

(DEFUN INSERT-KILL-RING-THING (BP THING)
  "Insert THING at BP, assuming THING came from the kill ring.
BP should not be of type :MOVES."
  (IF (TYPEP THING '(AND INTERVAL (NOT NAMED-BUFFER)))
      (WITH-BP (BP1 (INSERT-THING BP THING) ':MOVES)
        (FIXUP-FONTS-INTERVAL (GET THING ':FONTS) BP BP1 T)
        (COND (*KILL-INTERVAL-SMARTS*
               (IF (MEM #'CHAR-EQUAL (BP-CHAR-BEFORE BP) *BLANKS*)
                   (DELETE-OVER *BLANKS* BP)
                 (FIXUP-WHITESPACE BP))
               (IF (MEM #'CHAR-EQUAL (BP-CHAR BP1) *BLANKS*)
                   (DELETE-BACKWARD-OVER *BLANKS* BP1)
                 (FIXUP-WHITESPACE BP1))))
        (COPY-BP BP1))
    (INSERT BP (TYPECASE THING
                 (STRING THING)
                 ((OR SYMBOL LIST) (PRIN1-TO-STRING THING))
                 (T (PRINC-TO-STRING THING))))))

(DEFUN FIXUP-WHITESPACE (BP &AUX BP2 BP1 CH1 CH2 SYN1 SYN2 FLAG BP3)
  (SETQ BP1 (BACKWARD-OVER *BLANKS* BP)
        BP2 (FORWARD-OVER *BLANKS* BP)
        CH1 (IF (SETQ BP3 (FORWARD-CHAR BP1 -1))
                (BP-CH-CHAR BP3) #\CR)
        CH2 (BP-CH-CHAR BP2)
        SYN1 (LIST-SYNTAX CH1)
        SYN2 (LIST-SYNTAX CH2))
  (COND ((OR (= CH2 #\CR)                       ;If at the end of the line,
             (AND (SETQ FLAG (OR ;(EQ (GET *MAJOR-MODE* 'EDITING-TYPE) ':LISP)
                               (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
                               (MULTIPLE-VALUE-BIND (STRING SLASH COMMENT)
                                   (LISP-BP-SYNTACTIC-CONTEXT BP1)      ;or any funny syntax
                                 (OR STRING SLASH COMMENT))))
                  (NOT (AND (BP-= BP BP1)
                            (BP-= BP BP2))))))  ;and some whitespace, leave it alone
        ((NOT (= CH1 #\CR))                     ;If not at beginning of line,
         (DELETE-INTERVAL BP1 BP2 T)            ;flush whitespace, and
         (AND (IF FLAG
                  (NOT (OR (MEMQ CH1 *BLANKS*) (MEMQ CH2 *BLANKS*)))
                  (AND ( SYN1 LIST-OPEN) ( SYN1 LIST-SINGLE-QUOTE) ( SYN2 LIST-CLOSE)))
              (INSERT BP1 (IN-CURRENT-FONT #\SP))))))

;; The kill history.

;; This is the ELEMENT-STRING-FUNCTION for the kill history.
(DEFUN SUMMARIZE-KILL-HISTORY-INTERVAL (INTERVAL)
  (SUMMARIZE-INTERVAL INTERVAL NIL NIL 30.))

;Use of the ZWEI kill ring from other programs:
;use KILL-STRING to push a string on the kill ring, or merge it;
;use KILL-RING-STRING to get a string from a specified position on the ring;
;use KILL-RING-POP to rotate the ring.

(DEFUN KILL-RING-STRING (POSITION)
  "Return a string giving the contents of item POSITION on the kill ring.
POSITION is a nonnegative integer."
  (LET ((INTERVAL (HISTORY-ELEMENT-SET-YANK-POINTER *KILL-HISTORY* (1+ POSITION))))
    (AND INTERVAL (STRING-INTERVAL INTERVAL))))

(DEFUN KILL-RING-PUSH (INTERVAL)
  "Push INTERVAL on the kill ring.  Low level."
  (PUSH-ON-HISTORY INTERVAL *KILL-HISTORY*))

(DEFUN KILL-RING-POP (ARG)
  "Access or pop element ARG of the kill ring."
  (ROTATE-HISTORY-YANK-POINTER *KILL-HISTORY* ARG))

(DEFUN KILL-STRING (STRING &OPTIONAL MERGEP FORWARDP
                    &AUX (*BATCH-UNDO-SAVE* T))
  "Push STRING onto the kill ring.
MERGEP = T says merge with the existing last entry,
in which case FORWARDP = T says add to the end (otherwise the beginning)."
  (COND (MERGEP
         (INSERT
           (LET ((INT (HISTORY-LATEST-ELEMENT *KILL-HISTORY*)))
             (IF FORWARDP
                 (INTERVAL-LAST-BP INT)
               (INTERVAL-FIRST-BP INT)))
           STRING))
        (T (KILL-RING-PUSH (CREATE-INTERVAL STRING)))))

(DEFUN KILL-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P (FORWARDP T) EXPLICIT-P)
  "Kill an interval and push it on the kill ring.
The interval is specified by BP1, BP2 and IN-ORDER-P.
May merge, according to *LAST-COMMAND-TYPE*, in which case
FORWARDP says whether to add at the end or the beginning.
EXPLICIT-P = T turns off the *KILL-INTERVAL-SMARTS* feature."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (AND *KILL-INTERVAL-SMARTS*
       (NOT EXPLICIT-P)
       (OR (AND (EQ (BP-LINE BP1) (BP-LINE BP2)) (BEG-LINE-P BP1))
           (AND (MEM #'CHAR-EQUAL (BP-CHAR-BEFORE BP1) *BLANKS*)
                (NOT (BEG-LINE-P (BACKWARD-OVER *BLANKS* BP1)))))
       (MOVE-BP BP2 (FORWARD-OVER *BLANKS* BP2)))
  (KILL-RING-SAVE-INTERVAL BP1 BP2 T FORWARDP)
  (UNLESS *BATCH-UNDO-SAVE*
    (UNLESS (AND (EQ *LAST-COMMAND-TYPE* 'KILL)
                 (HISTORY-LATEST-ELEMENT *KILL-HISTORY*))
      ;; Don't let a kill be part of the same undo-item as what came before it.
      (IF *INTERVAL* (UNDO-SAVE-CURRENT-RANGE))))
  (DELETE-INTERVAL BP1 BP2 T))

(DEFUN KILL-RING-SAVE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P FORWARDP
                                &AUX (*BATCH-UNDO-SAVE* T))
  "Push the interval specified by BP1, BP2 and IN-ORDER-P on the kill ring.
Does not delete the text.  FORWARDP says which end to merge on,
if we merge (that is decided by *LAST-COMMAND-TYPE*)."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (COND ((AND (EQ *LAST-COMMAND-TYPE* 'KILL)
              (HISTORY-LATEST-ELEMENT *KILL-HISTORY*))
         (INSERT-INTERVAL
           (LET ((INT (HISTORY-LATEST-ELEMENT *KILL-HISTORY*)))
             (IF FORWARDP
                 (INTERVAL-LAST-BP INT)
               (INTERVAL-FIRST-BP INT)))
           (COPY-INTERVAL BP1 BP2 T)))
        (T (LET ((INT (COPY-INTERVAL BP1 BP2 T)))
             (PUTPROP INT (SEND (BP-TOP-LEVEL-NODE BP1) ':GET-ATTRIBUTE ':FONTS) ':FONTS)
             (KILL-RING-PUSH INT)))))

(DEFUN KILL-INTERVAL-ARG (BP1 BP2 ARG)
  "Kill the interval from BP1 to BP2, forward or backward according to sign of ARG."
  (IF (PLUSP ARG)
      (KILL-INTERVAL BP1 BP2 T T)
      (KILL-INTERVAL BP2 BP1 T NIL)))

(DEFCOM COM-YANK "Re-insert the last stuff killed.
Leaves point and mark around what is inserted.
Just Control-U as argument means put point in front and mark after.
A numeric argument means use the n'th most recent kill from the ring.
Zero as argument means list the contents of the kill ring." ()
  (HISTORY-YANK *KILL-HISTORY*)
  DIS-TEXT)

;; This variable bound implies safe to use history facility.
(DEFCONST HISTORY-LOADED T)
