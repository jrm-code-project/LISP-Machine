;;; -*- Mode:LISP; Package:ZWEI; Base:10 -*-

(DEFUN DIRED-REGENERATE-LINE (LINE &AUX (PLIST (LOCF (LINE-PLIST LINE)))
                              (PATHNAME (GET PLIST ':PATHNAME)))
  "Restore the contents of LINE from the data in its properties."
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    (LET ((CH (IF (ZEROP (STRING-LENGTH LINE))
                  #/SP
                  (CHAR LINE 0)))
          (FILE (CONS PATHNAME (CDR PLIST))))
      (SETF (LINE-LENGTH LINE) 0)
      (WITH-OUTPUT-TO-STRING (S LINE)
;       (IF (GET FILE ':DIRECTORY)
;           (LET ((STR (SEND (SEND (SEND (CAR FILE) :PATHNAME-AS-DIRECTORY)
;                                  :NEW-PATHNAME :NAME NIL :TYPE NIL :DEVICE NIL)
;                            :STRING-FOR-PRINTING)))
;             (SEND S :STRING-OUT "      ")
;             (SEND S :STRING-OUT STR (1+ (STRING-SEARCH-CHAR #/: STR))))
          (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE S))
;     )
      (OR (GET FILE ':DIRECTORY)
          ;; Eliminate the Newline which the lister writes.
          (DECF (LINE-LENGTH LINE)))
      (INSERT-CHARS (CREATE-BP LINE 5) #/SPACE
                    (* *DIRED-SUBDIRECTORY-INDENTATION* (GET FILE 'LEVEL)))
      (SETF (CHAR LINE 0)
            (cond ((getf (line-plist line) :deleted) #/d)
                  ((char-equal ch #/d) #/ )
                  (t ch))))
    (MUNG-LINE LINE)))


(defmethod (ZMACS-WINDOW :BEFORE :SET-INTERVAL-INTERNAL) (&rest ignore)
; (event "ENTER :BEFORE :SET-INTERVAL-INTERNAL")
; (event "EXIT :BEFORE :SET-INTERVAL-INTERNAL")
  )


(defmethod (ZMACS-WINDOW :AFTER :SET-INTERVAL-INTERNAL) (&rest ignore)
;  (event "ENTER :AFTER :SET-INTERVAL-INTERNAL")
  (cond ((and (boundp 'zwei:*dired-display-pane*)
              (eq (send self :superior) zwei:*dired-display-pane*))
         (set-interval-internal-dired self))
        ((and (boundp 'zwei:*display-pane-1*)
              (eq (send self :superior) zwei:*display-pane-1*))
         (set-interval-internal-gateway self))
        (t (set-interval-internal-standard self)))
;  (event "EXIT :AFTER :SET-INTERVAL-INTERNAL")
  )


(defun MOUSE-SELECT-DIRED (ignore)
  (setq tv:scroll-bar-max-speed 3)
  (if (eq (send (send (send self :selection-substitute) :interval) :saved-major-mode) 'zwei:dired-mode)
      (set-dired-mouse-handling)
    (set-standard-mouse-handling)))


(DEFUN MOVE-BP (BP LINE &OPTIONAL INDEX &AUX OLINE (old-bp (copy-bp bp)))
  "Move buffer pointer BP to point somewhere else.
Either LINE is a line and INDEX an index in it,
or LINE is another BP to be copied."
  (SETQ OLINE (BP-LINE BP))
  (COND ((NULL INDEX)
         (SETQ INDEX (BP-INDEX LINE) LINE (BP-LINE LINE)))
        ;; If we were not passed a BP, check that the INDEX is in range.
        ((> INDEX (LINE-LENGTH LINE))
         (FERROR NIL "The index ~O is greater than the length of the line ~S"
                 INDEX LINE)))
  (COND ;; If it is to the same line, there can be no problem.
    ((EQ OLINE LINE)
     (SETF (BP-INDEX BP) INDEX))
    (T
     (COND ((BP-STATUS BP)
            ;; It is a permanent bp changing lines.  Fix relocation lists.
            (SETF (LINE-BP-LIST OLINE) (DELQ BP (LINE-BP-LIST OLINE)))
            (PUSH BP (LINE-BP-LIST LINE))))
     (SETF (BP-LINE BP) LINE)
     (SETF (BP-INDEX BP) INDEX)))
  (move-bp-hook old-bp bp)
  BP)


;(defun BLINK-DIRED-LINE (blinker window char x y line index &aux beg end sheet); string-length)
;  (cond ((> *no-rectangle-blinking* 0)
;        (decf *no-rectangle-blinking*)
;        (tv:blinker-set-visibility blinker nil))
;       ((or (not (typep (tv:window-under-mouse) 'zmacs-window-pane))
;            (null (line-previous line))
;            (null (line-previous (line-previous line)))
;            (null (line-next line)))
;        (tv:blinker-set-visibility blinker nil))
;       (t (setq beg 0)
;          (setq end (array-active-length line))
;          (setq sheet (window-sheet window))
;          (tv:blinker-set-sheet blinker sheet)
;          (sheet-set-blinker-cursorpos
;            sheet
;            blinker
;            (- x (tv:sheet-string-length sheet line beg index))
;            y)
;          (tv:blinker-set-size
;            blinker
;            (plus (tv:sheet-string-length sheet line beg end) 2)
;            (plus (font-char-height (aref (tv:sheet-font-map sheet) (ldb %%ch-font char))) 1))
;          (tv:blinker-set-visibility blinker t))))
