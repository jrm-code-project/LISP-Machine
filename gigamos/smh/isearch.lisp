;;;   -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:8; Patch-File:T -*-

;; This adds wraparound to incremental search -- see l.zwei;coms.lisp
;; To patchify this it would be necessary to add the following after the
;; revised defresource:  (SETF (GET 'ISEARCH-VECTORS 'DEFRESOURCE) NIL)

(SETF (GET 'ISEARCH-VECTORS 'DEFRESOURCE) NIL)

(defresource isearch-vectors ()
  :constructor (vector
                 ;; *is-string*
                 (make-string 128. :fill-pointer 0)
                 ;; All of the arrays below constitute a push-down stack.
                 ;; *is-bp*
                 (make-array 128. :fill-pointer 0)
                 ;; *is-status*
                 ;; STATUS is NIL for a failing search, T for a successful one,
                 ;; and :GO for one that is still looking.
                 (make-array 128. :fill-pointer 0)
                 ;; *is-reverse-p*
                 ;; T if the search is reverse at this level.
                 (make-array 128. :fill-pointer 0)
                 ;; *is-pointer*
                 ;; This points to the end of the part of *IS-STRING* active at this level.
                 (make-array 128. :fill-pointer 0)
                 ;; *is-operation*
                 ;; This is what sort of thing the char at this level is:
                 ;; :NORMAL, :REVERSE or :REPEAT.
                 (make-array 128. :fill-pointer 0)
                 ;; *is-wrapped-p*
                 ;; Boolean -- whether search has wrapped around.
                 (make-array 128. :fill-pointer 0))
  :initializer (dotimes (x (length object))
                 (setf (fill-pointer (svref object x)) 0)))

(DEFVAR *IS-STRING*)
(DEFVAR *IS-BP*)
(DEFVAR *IS-STATUS*)
(DEFVAR *IS-REVERSE-P*)
(DEFVAR *IS-wrapped-P*)
(DEFVAR *IS-POINTER*)
(DEFVAR *IS-OPERATION*)

(DEFUN INITIALIZE-INCREMENTAL-SEARCH-GLOBALS ()
  (USING-RESOURCE (TEM ISEARCH-VECTORS)
    TEM
    NIL))

(DEFMACRO PUSH-ISEARCH-STATUS ()
  '(PUSH-ISEARCH-STATUS-1 (SETQ P (1+ P))))

(DEFUN PUSH-ISEARCH-STATUS-1 (P)
  (WHEN (= P (ARRAY-LENGTH *IS-REVERSE-P*))
    (ADJUST-ARRAY-SIZE *IS-REVERSE-P* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-wrapped-P* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-STATUS* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-OPERATION* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-BP* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-POINTER* (+ P 64.)))
  (SETF (AREF *IS-REVERSE-P* P) (AREF *IS-REVERSE-P* (1- P)))
  (SETF (AREF *IS-wrapped-P* P) (AREF *IS-wrapped-P* (1- P)))
  (SETF (AREF *IS-POINTER* P) (AREF *IS-POINTER* (1- P)))
  (SETF (AREF *IS-STATUS* P) ':GO))

;;; This is how incremental search manages to allow both type-ahead and rubout-ahead:
;;; What to do is kept in five stacks, arrays in the *IS-...* variables.
;;; Input of normal characters pushes onto the end using index P,
;;; and rubout pops off at the same index.  *IS-REVERSE-P* remembers the
;;; search direction at each level, *IS-OPERATION* remembers the type of search
;;; (:NORMAL for a normal character, :REVERSE for a Control-R or Control-S that reverses,
;;; or :REPEAT for a Control-R or Control-S that repeats), *IS-POINTER* is the length of
;;; the search string at that level.

;;; In parallel, with lower priority, the entries thus pushed are processed
;;; by searching according to them.  P1 is the index of the entry or "level"
;;; which is currently being worked on.  P1 advances only when the level is
;;; determined to be successful or failing.  Advancing involves examining the three
;;; *IS-...* entries of the next level to see what to do.  If P1 meets P, then there is no
;;; work to do for the moment.  The state of this process is kept in *IS-STATUS*
;;; and *IS-BP*.  *IS-BP* is the bp of the place found at a given level or the
;;; place at which searching is going on.  *IS-STATUS* is T for a successful search,
;;; NIL for a failing one, and :GO if it isn't known yet.  New levels are pushed
;;; (via P) in the :GO state.

;;; Rubbing out decrements P1 if necessary to keep it no greater than P.
;;; The searching process is not confused because it keeps all its state
;;; in *IS-STATUS* and *IS-BP* and all that is needed is to change P1.

;;; Updating the echo area is under input in priority, but above actual searching.
;;; Thus, as soon as there is no type-ahead everything will be correct.
;;; This is because the echo area is presumed to be fast to update.
;;; Buffer redisplay is lower than searching, of course.


; character lossage rampant
(DEFUN INCREMENTAL-SEARCH (REVERSE-P &AUX (ORIG-PT (COPY-BP (POINT)))
                                          ISEARCH-VECTORS)
  (SELECT-WINDOW *WINDOW*)
  (SEND *QUERY-IO* :FRESH-LINE)         ;Necessary if in the mini-buffer
  (UNWIND-PROTECT
      (TYPEIN-LINE-ACTIVATE
        (PROG* (CHAR                    ; The current command.
                XCHAR                   ; Upcase version of character
                MUST-REDIS              ; T => The echo-area must be completely redisplayed.
                (P 0)                   ; The stack pointer into *IS-BP*, etc. for input and rubout
                (P1 0)                  ; The pointer for which search we are doing.
                                        ; Can never exceed P.
                SUPPRESSED-REDISPLAY    ; T if the last input char was read before
                                        ; redisplay had a chance to finish.
                                        ;  A Control-G read that way acts like
                                        ;  a failing search quit.
                BP1                     ; Aux BP used for actual searching.
                NEW-BP
                TIME-OUT                ; Set by SEARCH when it times out so we can check input.
                INPUT-DONE              ; An altmode or control char has been seen.
                                        ; Do not look for input any more; just search, then exit.
                (*is-string*)
                (*is-bp*)
                (*is-status*)
                (*is-reverse-p*)
                (*is-wrapped-p*)
                (*is-pointer*)
                (*is-operation*)
;               (TV:KBD-INTERCEPTED-CHARACTERS
;                 (COPY-LIST TV:KBD-INTERCEPTED-CHARACTERS))
                )
;              (SETQ TV:KBD-INTERCEPTED-CHARACTERS
;                    (DELQ (ASSQ #/ABORT TV:KBD-INTERCEPTED-CHARACTERS)
;                          TV:KBD-INTERCEPTED-CHARACTERS))
               (SETQ ISEARCH-VECTORS (ALLOCATE-RESOURCE 'ISEARCH-VECTORS)
                     *IS-STRING* (SVREF ISEARCH-VECTORS 0)
                     *IS-BP* (SVREF ISEARCH-VECTORS 1)
                     *IS-STATUS* (SVREF ISEARCH-VECTORS 2)
                     *IS-REVERSE-P* (SVREF ISEARCH-VECTORS 3)
                     *IS-POINTER* (SVREF ISEARCH-VECTORS 4)
                     *IS-OPERATION* (SVREF ISEARCH-VECTORS 5)
                     *IS-wrapped-P* (SVREF ISEARCH-VECTORS 6))
               (SETF (AREF *IS-STATUS* 0) T)    ; Initialize the stacks.
               (SETF (AREF *IS-REVERSE-P* 0) REVERSE-P)
               (SETF (AREF *IS-OPERATION* 0) ':NORMAL)
               (SETF (AREF *IS-POINTER* 0) 0)
               (SETF (AREF *IS-BP* 0) (COPY-BP (POINT)))
               (SETQ MUST-REDIS T)      ; Initially we must redisplay.
               (GO CHECK-FOR-INPUT)
               ;; Come here if there is input, or nothing to do until there is input.
            INPUT
               (SETQ SUPPRESSED-REDISPLAY NIL)
               (AND (WINDOW-READY-P *WINDOW*)   ;In case of minibuffer
                    (REDISPLAY *WINDOW* :POINT))        ; Redisplay point position while waiting.
               (IF (= (WINDOW-REDISPLAY-DEGREE *WINDOW*) DIS-NONE)
                   (REDISPLAY-MODE-LINE)        ;Update indication of more above or below.
                 (SETQ SUPPRESSED-REDISPLAY T))
               (IF SUPPRESSED-REDISPLAY
                   (SETQ CHAR (TYI-WITH-SCROLLING NIL T))
                 ;; If must wait for input, make the window's blinker blink
                 ;; even though not selected.
                 (IF (OPERATION-HANDLED-P *WINDOW* :POINT-BLINKER)
                     (UNWIND-PROTECT
                         (PROGN
                           (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-VISIBILITY :BLINK)
                           (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-DESELECTED-VISIBILITY :BLINK)
                           (SETQ CHAR (TYI-WITH-SCROLLING NIL T)))
                       (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-VISIBILITY
 ;tv lossage
                             (IF (EQ *WINDOW* TV:SELECTED-WINDOW)
                                 :BLINK
                               (TV:SHEET-EXPOSED-P *WINDOW*)))
                       (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-DESELECTED-VISIBILITY :ON))
                   (SETQ CHAR (TYI-WITH-SCROLLING NIL T))))
               (COND ((CONSP CHAR)
                      (SEND *STANDARD-INPUT* :UNTYI CHAR)
                      (SETQ INPUT-DONE T)
                      (GO CHECK-FOR-INPUT))
; character lossage
                     ((FIXNUMP CHAR)
                      (SETQ CHAR (INT-CHAR CHAR))))
               (SETQ XCHAR (CHAR-UPCASE CHAR))
               (COND ((NOT (OR ( (CHAR-BITS CHAR) 0) (TV:CHAR-MOUSE-P CHAR)
                               (MEMQ CHAR
                                     '(#/ #/END #/RUBOUT #/HELP #/ABORT #/CLEAR-INPUT))))
                      (GO NORMAL))
                     ((MEMQ XCHAR '(#/C-S #/C-R))
                      (PUSH-ISEARCH-STATUS)
                      (SETF (AREF *IS-OPERATION* P) ':REPEAT)
                      (LET ((NEW-REVERSE-P (EQL XCHAR #/C-R)))
                        (COND   ;; In reverse mode, just go to forward.
                          ((NEQ (AREF *IS-REVERSE-P* P) NEW-REVERSE-P)
                           (SETF (AREF *IS-REVERSE-P* P) NEW-REVERSE-P)
                           (SETQ MUST-REDIS T)
                           (SETF (AREF *IS-OPERATION* P) ':REVERSE))
                          ((ZEROP (AREF *IS-POINTER* P))
                           (LET ((STRING (STRING (OR (CAAR *SEARCH-RING*) (BARF)))))
                             (COPY-ARRAY-CONTENTS STRING *IS-STRING*)
                             (SETF (AREF *IS-POINTER* P) (LENGTH STRING)))
                           (SETQ MUST-REDIS T))))
                      (GO CHECK-FOR-INPUT))
                     ((memq xchar '(#/C-W #/C-M-W))   ;snarf word/form at cursor - smh 24jun88
                      (let* ((saved-point (point))
                             (pointer (if (aref *is-reverse-p* p)
                                          (forward-char saved-point (aref *is-pointer* p))
                                        (copy-bp saved-point)))
                             (other-end (or (if (eql xchar #/C-W)
                                                (forward-word pointer 1)
                                              (forward-sexp pointer 1))
                                            (progn (beep)
                                                   ;; at end of file, just flash and wait for more
                                                   (go check-for-input)))))
                        (PUSH-ISEARCH-STATUS)
                        (loop while (bp-< pointer other-end)
                              as char = (int-char (bp-char pointer))
                              do
                              (OR MUST-REDIS (FORMAT *QUERY-IO* "~C" CHAR))
                              (LET ((IDX (AREF *IS-POINTER* P)))
                                (AND ( IDX (ARRAY-LENGTH *IS-STRING*))
                                     (ADJUST-ARRAY-SIZE *IS-STRING* (+ IDX 64.)))
                                (SETF (CHAR *IS-STRING* IDX) CHAR)
                                (SETF (AREF *IS-POINTER* P) (1+ IDX)))
                              (SETF (AREF *IS-OPERATION* P) ':NORMAL)
                              (setq pointer (forward-char pointer 1)))
                        (move-bp (point) saved-point)
                        (go check-for-input)))
                     ((EQL XCHAR #/C-Q)
                      (LET ((NEW-CH (READ-CHAR *STANDARD-INPUT*)))
                        (SETQ CHAR (IF (CHAR-BIT NEW-CH :CONTROL)
                                       ;; ascii version of control characters
                                       (INT-CHAR (LOGAND #o37 (CHAR-CODE NEW-CH)))
                                     (INT-CHAR (CHAR-CODE NEW-CH)))))
                      (GO NORMAL))
                     ((EQL CHAR #/HELP)
                      (PRINT-DOC :FULL *CURRENT-COMMAND*)
                      (SEND *STANDARD-INPUT* :UNTYI (SEND *STANDARD-INPUT* :ANY-TYI))
                      (GO INPUT))
                     ((OR (EQL XCHAR #/C-G) (EQL XCHAR #/ABORT))
                      (BEEP)
                      (COND ((AND (OR SUPPRESSED-REDISPLAY (NEQ (AREF *IS-STATUS* P) T))
                                  (PLUSP P))
                             ;; Control-G in other than a successful search
                             ;; rubs out until it becomes successful.
                             (SETQ P (DO ((P (1- P) (1- P)))
                                         ((EQ (AREF *IS-STATUS* P) T) P)))
                             (SETQ P1 (MIN P P1) MUST-REDIS T)
                             (GO CHECK-FOR-INPUT))
                            (T
                             (MOVE-BP (POINT) (AREF *IS-BP* 0))
                             (RETURN))))
                     ((OR (EQL CHAR #/) (EQL CHAR #/END))
                      (AND (ZEROP P)
                           ;; Call string search, and make self-doc print the right thing there.
                           (LET ((*CURRENT-COMMAND* 'COM-STRING-SEARCH-INTERNAL))
                             (RETURN (COM-STRING-SEARCH-INTERNAL REVERSE-P NIL NIL NIL))))
                      (SETQ INPUT-DONE T)
                      (GO CHECK-FOR-INPUT))
                     ((OR (EQL CHAR #/RUBOUT) (EQL CHAR #/CLEAR-INPUT))
                      ;; Clear-input rubs out all the way.  Set P to 1 and let it be decremented.
                      (IF (EQL CHAR #/CLEAR-INPUT) (SETQ P 1))
                      (COND (( P 0)    ; If he over-rubbed out,
                             (BEEP)     ;   that is an error.
                             (GO CHECK-FOR-INPUT))
                            (T
                             ;; Rubout pops all of these PDLs.
                             (SETQ P (1- P))
                             (SETQ P1 (MIN P P1))
                             (SETQ MUST-REDIS T)
                             (GO CHECK-FOR-INPUT))))
                     (T
;character lossage
                      (SEND *STANDARD-INPUT* :UNTYI (CHAR-INT CHAR))
                      (SETQ INPUT-DONE T)
                      (GO CHECK-FOR-INPUT)))
               (FERROR "A clause fell through.")

               ;; Normal chars to be searched for come here.
            NORMAL
               (OR MUST-REDIS (FORMAT *QUERY-IO* "~C" CHAR))
               (PUSH-ISEARCH-STATUS)
               (LET ((IDX (AREF *IS-POINTER* P)))
                 (AND ( IDX (ARRAY-LENGTH *IS-STRING*))
                      (ADJUST-ARRAY-SIZE *IS-STRING* (+ IDX 64.)))
                 (SETF (CHAR *IS-STRING* IDX) CHAR)
                 (SETF (AREF *IS-POINTER* P) (1+ IDX)))
               (SETF (AREF *IS-OPERATION* P) ':NORMAL)
               ;; Come here after possibly processing input to update the search tables
               ;; to search for a while.  First, if necessary and not suppressed
               ;; update the search string displayed in the echo area.
            CHECK-FOR-INPUT
               ;; If there is input available, go read it.
               ;; Otherwise, do work if there is work to be done.
               (AND (NOT INPUT-DONE)
                    (SEND *STANDARD-INPUT* :LISTEN)
                    (GO INPUT))
               ;; Now do some work for a while, then go back to CHECK-FOR-INPUT.
               (WHEN MUST-REDIS
                 (SETQ MUST-REDIS NIL)
                 (FORMAT *QUERY-IO* "~&~:|")
                 (OR (AREF *IS-STATUS* P1) (FORMAT *QUERY-IO* "Failing "))
                 (AND (AREF *IS-REVERSE-P* P) (FORMAT *QUERY-IO* "Reverse "))
                 (AND (AREF *IS-wrapped-P* P) (FORMAT *QUERY-IO* "Wrapped "))
                 (FORMAT *QUERY-IO* "I-Search: ")
                 (SETF (FILL-POINTER *IS-STRING*) (AREF *IS-POINTER* P))
                 (FORMAT *QUERY-IO* "~A" *IS-STRING*))
               ;; Now see what sort of state the actual search is in, and what work there is to do.
               ;; P1 points at the level of the table on which we are actually working.
               (SETF BP1 (AREF *IS-BP* P1))
               ;; Display point at the end of the last search level which has succeeded.
               (DO ((P0 P1 (1- P0)))
                   ((EQ (AREF *IS-STATUS* P0) T)
                    (MOVE-BP (POINT) (AREF *IS-BP* P0))))
               (MUST-REDISPLAY *WINDOW* DIS-BPS)
               (COND ((EQ (AREF *IS-STATUS* P1) ':GO)
                      (SETF (FILL-POINTER *IS-STRING*) (AREF *IS-POINTER* P1))
                      ;; If the level we were working on is still not finished,
                      ;; search at most 64. more lines.  If we find it or the end of the buffer
                      ;; before then, this level is determined and we can work on the next.
                      ;; Otherwise, we remain in the :GO state and do 64. more lines next time.
                      (MULTIPLE-VALUE (NEW-BP TIME-OUT)
                        (ZWEI-SEARCH BP1 *IS-STRING*
                                     (AREF *IS-REVERSE-P* P1) NIL 64.))
                      ;; What happened?
                      (COND (TIME-OUT
                             ;; Nothing determined.  NEW-BP has where we stopped.
                             (MOVE-BP BP1 NEW-BP)
                             (DBP BP1)) ;Avoids missing occurrences, if string starts with CR
                            ((NULL NEW-BP)
                             ;; This search was determined to be a failure.
                             (OR (SEND-IF-HANDLES *STANDARD-INPUT* :MACRO-ERROR)
                                 (BEEP))
                             (SETF (AREF *IS-STATUS* P1) NIL)
                             (MOVE-BP BP1 (AREF *IS-BP* (1- P1)))
                             (MOVE-BP (POINT) BP1)
                             (SETQ MUST-REDIS T))
                            (T ;; This search level has succeeded.
                             (SETF (AREF *IS-STATUS* P1) T)
                             (MOVE-BP (POINT) NEW-BP)
                             (MOVE-BP BP1 NEW-BP))))
                     (( P P1)
                      ;; This level is finished, but there are more pending levels typed ahead.
                      (INCF P1)
                      (SETF (AREF *IS-BP* P1) (SETQ BP1 (COPY-BP BP1)))
                      (SETF (FILL-POINTER *IS-STRING*) (AREF *IS-POINTER* P1))
                      (COND ((NULL (AREF *IS-STATUS* (1- P1)))  ;search is failing
                             (COND ((EQ (AREF *IS-OPERATION* P1) ':REVERSE)
                                    ;; If we reverse direction, change prompt line.
                                    (SETQ MUST-REDIS T))
                                   ((eq (aref *is-operation* p1) ':repeat)      ;smh 30jun88
                                    ;; Wrap around to top (or bottom).
                                    (setq must-redis t)
                                    (setf (aref *is-wrapped-p* p1) t)
                                    (move-bp bp1 (if (aref *is-reverse-p* p1)
                                                     (interval-last-bp *interval*)
                                                   (interval-first-bp *interval*))))
                                    (T
                                      ;; A failing search remains so unless we reverse direction.
                                      (SETF (AREF *IS-STATUS* P1) NIL))))
                            ((EQ (AREF *IS-OPERATION* P1) ':NORMAL)
                             ;; Normal char to be searched for comes next.
                             ;; We must adjust the bp at which we start to search
                             ;; so as to allow the user to extend the string already found.
                             (MOVE-BP BP1
                                      (FORWARD-CHAR BP1
                                                    (IF (AREF *IS-REVERSE-P* P1)
                                                        (IF (= (LENGTH *IS-STRING*) 1)
                                                            0
                                                          (LENGTH *IS-STRING*))
                                                      (- 1 (LENGTH *IS-STRING*)))
                                                    T)))))
                     ;; If there is nothing left to do, and terminator seen, exit.
                     (INPUT-DONE
                      (SEARCH-RING-PUSH
                        ;; Entries on the search ring should have a leader
                        (STRING-NCONC (MAKE-STRING (LENGTH *IS-STRING*)
                                                   :FILL-POINTER 0)
                                      *IS-STRING*)
                        'ZWEI-SEARCH)
                      (FORMAT *QUERY-IO* "")
                      (MAYBE-PUSH-POINT ORIG-PT)
                      ;(SELECT-WINDOW *WINDOW*)
                      (RETURN))
                     ;; Nothing to do and no terminator, wait for input.
                     (T (GO INPUT)))
               (GO CHECK-FOR-INPUT)
               )
         (SETQ ORIG-PT NIL))
    ;; unwind-protect cleanup
    (IF ISEARCH-VECTORS (DEALLOCATE-RESOURCE 'ISEARCH-VECTORS ISEARCH-VECTORS))
    (IF ORIG-PT (MOVE-BP (POINT) ORIG-PT))
    (SEND-IF-HANDLES *QUERY-IO* :MAKE-COMPLETE)
    (MUST-REDISPLAY *WINDOW* DIS-BPS)
    (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW))
  DIS-BPS)
