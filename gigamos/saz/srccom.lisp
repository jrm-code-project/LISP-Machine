;;; -*- Mode:LISP; Package:SRCCOM; Base:10; Readtable:ZL -*-

TV:
(DEFUN CURRENT-SHEET-INSIDE-CHARACTER-WIDTH ()
  (floor (// (tv:sheet-inside-width tv:selected-window)
             (tv:sheet-character-width tv:selected-window #/Space
                                       (tv:sheet-current-font tv:selected-window)))))

(DEFUN FIND-BUFFER-OR-BARF (NAME)
  "Return the buffer named NAME, or NIL if none.
CREATE-P non-NIL says create one if there is none."
  (OR (STRINGP NAME)
      (SETQ NAME (SEND NAME :STRING-FOR-EDITOR)))
  (DO ((L *ZMACS-BUFFER-LIST* (CDR L)))
      ((NULL L)
       (barf ("No existing buffer called ~A" NAME)))
    (AND (STRING-EQUAL (BUFFER-NAME (CAR L)) NAME)
         (RETURN (CAR L)))))

(setq *difference-printer* 'my-print-differences)
;;; We are back in synch, print the differences
(DEFUN MY-PRINT-DIFFERENCES (FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1
                          FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2)
  (let* ((label-length 72.)
         (format-string
           (make-string label-length
                        ':initial-element #/*)))
  (FORMAT *OUTPUT-STREAM* " ~[~% The first~; The next~] ~D lines correspond in both sources.  Then, however:~%    "
          (if (= *lines-that-matched* diff-line-no-1) 0 1)
          *lines-that-matched*)
  (PRINT-DIFFS-2 FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1
                 (AND *PRINT-LABELS* (LINE-LAST-LABEL FILE-1 DIFF-LINE-NO-1))
                 format-string)
  (PRINT-DIFFS-2 FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2
                 (AND *PRINT-LABELS* (LINE-LAST-LABEL FILE-2 DIFF-LINE-NO-2))
                 format-string)
  (FORMAT *OUTPUT-STREAM* (string-append "~%" format-string))
  (FORMAT *OUTPUT-STREAM* (string-append "~%" format-string))
  (FORMAT *OUTPUT-STREAM* "~2%")))

(DEFVAR *LINES-TO-PRINT-BEFORE* 0
  "Number of matching lines preceding a run of differences to print with the differences.")
(DEFVAR *LINES-TO-PRINT-AFTER* 1
  "Number of matching lines following a run of differences to print with the differences.")

(DEFUN PRINT-DIFFS-2 (FILE DIFF-LINE-NO SAME-LINE-NO label format-string)
  (SETQ DIFF-LINE-NO (MAX 0 (- DIFF-LINE-NO *LINES-TO-PRINT-BEFORE*))
        SAME-LINE-NO (+ SAME-LINE-NO *LINES-TO-PRINT-AFTER*))
    (FORMAT *OUTPUT-STREAM* (string-append "~&" format-string))
    (FORMAT *OUTPUT-STREAM* "~&* ~A ~A, in lines ~D to ~D inclusive~71T*"
            (FILE-TYPE FILE) (FILE-NAME FILE)
            (1+ DIFF-LINE-NO)                   ;whacky fencepost error somewhere
            SAME-LINE-NO)
   (format *OUTPUT-STREAM* "~%* (found in ")    ;changed to "in" from "after"; text/paragraphs and lisp code/defxxxs
   (SEND *OUTPUT-STREAM* ':STRING-OUT (STRING-REMOVE-FONTS LABEL) 0
         (min (- (tv:current-sheet-inside-character-width) 13)
                     (STRING-LENGTH LABEL)))
;                     (IF (LET ((WHICH-OPERATIONS (SEND *OUTPUT-STREAM* ':WHICH-OPERATIONS)))
;                           (AND (MEMQ ':READ-CURSORPOS WHICH-OPERATIONS)
; I see no reason                (MEMQ ':SIZE-IN-CHARACTERS WHICH-OPERATIONS)))
; for this code           (- (SEND *OUTPUT-STREAM* ':SIZE-IN-CHARACTERS)
;                            (SEND *OUTPUT-STREAM* ':READ-CURSORPOS ':CHARACTER)
;                            1)))
   (FORMAT *OUTPUT-STREAM* "), has:~71T*")
   (FORMAT *OUTPUT-STREAM* (string-append "~%" format-string "~2%"))
   (PRINT-FILE-SEGMENT FILE DIFF-LINE-NO SAME-LINE-NO)
   (format *output-stream* "~%"))


(DEFUN LINE-LAST-LABEL (FILE LINE-NO)
  "Return the last /"interesting/" line preceding line number LINE-NO, in FILE."
  (DO ((I (1- LINE-NO) (1- I))
       (MODE (FILE-MAJOR-MODE FILE))
       (LINE))
      ((< I 0) (return "the beginning of the file"))
    (AND (LINE-INTERESTING-P (SETQ LINE (AREF FILE I)) MODE)
         (RETURN LINE))))
