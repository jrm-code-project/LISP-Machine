;;; LISP Machine Source Compare -*- Mode:LISP; Package:SRCCOM; Base:8; Readtable:ZL -*-

;;; Reason: Add option to SOURCE-COMPARE to allow files to be compared by their
;;; s-expressions as well as by their text.  Implemented by adding an optional
;;; argument to the SOurce Compare functions which specifies the type of compare
;;; either :TEXT or :FORM.  The ZMACS interface queries the user for the type
;;; of compare.

;;; Added parameters and handling for igoring escaped characters.
;;; Currently supports a single known escape character, optionally
;;; followed by a number of characters to be ignored.  10/88 Keith

(DEFVAR *OUTPUT-STREAM*)
(DEFVAR *LINES-THAT-MATCHED* 0)

(DEFSTRUCT (FILE :ARRAY-LEADER (:MAKE-ARRAY (:LENGTH 100.)) (:ALTERANT NIL))
  (FILE-LENGTH 0 :DOCUMENTATION "Number of lines")
  (FILE-NAME NIL :DOCUMENTATION "The file name")
  (FILE-TYPE "File" :DOCUMENTATION "What kind of source it has")
  (FILE-STREAM  NIL :DOCUMENTATION "Input stream")
  (FILE-MAJOR-MODE NIL :DOCUMENTATION "Symbol")
  (FILE-COMPARE ':TEXT :DOCUMENTATION "Kind of comparison to use")
  )

(DEFUN FILE-IDENTIFIER (FILE-OBJECT)
  "Return a string identifying FILE-OBJECT for the user.
FILE-OBJECT is a SRCCOM:FILE structure."
  (STRING-APPEND (FILE-TYPE FILE-OBJECT) " " (STRING (FILE-NAME FILE-OBJECT))))


(DEFUN CREATE-FILE (FILENAME &AUX STREAM MODE)
  "Make a SRCCOM:FILE object for a file to be source compared.
FILENAME is opened and the SRCCOM:FILE object contains a stream for it."
  (SETQ STREAM (OPEN FILENAME '(:IN)))
  (LET ((GENERIC-PATHNAME (SEND FILENAME ':GENERIC-PATHNAME)))
    (FS:READ-ATTRIBUTE-LIST GENERIC-PATHNAME STREAM)
    (SETQ MODE (OR (SEND GENERIC-PATHNAME ':GET ':MODE) ':LISP)))
  (MAKE-FILE FILE-STREAM STREAM
             FILE-NAME (SEND STREAM ':TRUENAME)
             FILE-MAJOR-MODE MODE))

;;;Escape handling:

(defparameter *escape-character-ignore-flag* nil
  "If non-NIL, either
 a) A character to be ignored, or
 b) A list whose CAR is the escape character and whose CADR is the number
    of succeeding characters to ignore.
Applies to :TEXT source comparison only.")

(defun handle-escape-characters-in-line (line &optional (flag *escape-character-ignore-flag*))
  (if (not (typep line '(or string array))) line
    (if (and (consp flag) (zerop (cadr flag)))
        (setq flag (car flag)))
    (typecase flag
      (null line)
      (atom (lisp:remove flag line :test #'char=))
      (t
       (let (ch gotone
             (out 0)
             (len (array-active-length line))
             (esc (character (car flag)))
             (nesc (cadr flag)))
         (do* ((in 0 (1+ in)))
              ((>= in len)
               (if (array-has-fill-pointer-p line)
                   (and (setf (fill-pointer line) out) line)
                 (subseq line 0 out)))
           (setq ch (aref line in))
           (cond
             ((char-equal ch esc)               ;Got an escape char,
              (setq gotone t)                   ; set flag,
              (incf in nesc))                   ; skip escaped chars.
             ((null gotone)                     ;Just skip.
              (incf out))
             (t
              (setf (char line out) (char line in))     ;Copy char
              (incf out)))))))))

(DEFUN GET-FILE-LINE (file line-no)
  ;; Modified to check FILE-COMPARE 8/5/83 (RAF @ TI-CSL60).
  "Get the line recorded in FILE, a SRCCOM:FILE structure, for line number LINE-NO.
This will cause more data to be read from the stream if necessary.
The line is simply a string containing the data on the line."
  (IF (< LINE-NO (FILE-LENGTH FILE))
      (AREF FILE LINE-NO)
      (MULTIPLE-VALUE-BIND (LINE EOF)
          (ECASE (FILE-COMPARE FILE)
            (:TEXT (SEND (FILE-STREAM FILE) ':LINE-IN T))
            (:FORM (LET ((X (READ (FILE-STREAM FILE) '**EOF**)))
                    (IF (EQ X '**EOF**)
                        (VALUES NIL T)          ;No form, end of file
                        (VALUES X NIL)))))
        (setq line (handle-escape-characters-in-line line))
        (COND ((NOT (AND EOF (OR (NULL LINE) (EQUAL LINE ""))))
               (ARRAY-PUSH-EXTEND FILE LINE)
               LINE)))))

(DEFVAR *PRINT-LABELS* T
  "T means print the /"label/" preceding each run of differences.
In Lisp code, this is the function from the most recent DEFUN line.")

(DEFUN LINE-LAST-LABEL (FILE LINE-NO)
  "Return the last /"interesting/" line preceding line number LINE-NO, in FILE."
  (DO ((I (1- LINE-NO) (1- I))
       (MODE (FILE-MAJOR-MODE FILE))
       (LINE))
      ((< I 0))
    (AND (LINE-INTERESTING-P (SETQ LINE (AREF FILE I)) MODE)
         (RETURN LINE))))

(DEFUN LINE-INTERESTING-P (LINE MODE &AUX LEN)
  (AND (PLUSP (SETQ LEN (ARRAY-ACTIVE-LENGTH LINE)))
       (SELECTQ MODE
         ((:LISP :ZTOP) (= (AREF LINE 0) #/())
         ((:TEXT :BOLIO) (= (AREF LINE 0) #/.))
         (OTHERWISE (NOT (MEMQ (AREF LINE 0) '(#/SP #/TAB)))))))

;;; Compare two lines
;;; *** FOR NOW NO HAIR FOR COMMENTS, WHITESPACE, ETC. ***

(DEFUN COMPARE-LINES (line-1 line-2)
  ;; Mod. 8/16/83 RAF (@ TI-CSL)
  ;; EQUAL on 2 strings ends up calling %STRING-EQUAL, the COND is just a more
  ;; efficient way of getting there for TEXT comparisons.
  (COND ((AND (TYPEP LINE-1 'STRING) (TYPEP LINE-2 'STRING))
         (%STRING-EQUAL LINE-1 0 LINE-2 0 NIL))
        (T (EQUAL LINE-1 LINE-2))))

(DEFVAR *PATHNAME-DEFAULTS* (FS:MAKE-PATHNAME-DEFAULTS)
  "A list of the pathname defaults.")

;;; Add :TYPE arg 8/4/83 (RAF @ TI-CSL60). see SOURCE-COMPARE-FILES.

(DEFUN SOURCE-COMPARE (FILENAME-1 FILENAME-2 &OPTIONAL (OUTPUT-STREAM STANDARD-OUTPUT)
                                             (TYPE ':TEXT)
                                             &AUX FILE-1 FILE-2)
  "Source compare files FILENAME-1 and FILENAME-2.
Output goes to OUTPUT-STREAM (defaults to STANDARD-OUTPUT).
/
TYPE argument determines how the files are compared:
  :TEXT => line by line textual comparison.
  :FORM => form by form (READ) comparison.
/
Some free variables serve as parameters:
/
*PRINT-LABELS* - T means print the preceding identifier
 (function name, etc.,) preceding each run of non-matching lines.
*LINES-NEEDED-TO-MATCH* - number of lines that must match
 in order for the two files to begin to agree again.
*LINES-TO-PRINT-BEFORE* - number of matching lines to print
 before each run of non-matching lines.
*LINES-TO-PRINT-AFTER* - number of matching lines to print
 after each run of non-matching lines.
*DIFFERENCE-PRINTER* - function to print out the differences.
 See SRCCOM:PRINT-DIFFERENCES for a sample.
*LINES-THAT-MATCHED* - available to difference-printer.
*ESCAPE-CHARACTER-IGNORE-FLAG* - controls whether//how to ignore
 escape characters within files for :TEXT comparison."
  (SETQ FILENAME-1 (FS:MERGE-AND-SET-PATHNAME-DEFAULTS FILENAME-1 *PATHNAME-DEFAULTS*
                                                       ':UNSPECIFIC ':OLDEST)
        FILENAME-2 (FS:MERGE-PATHNAME-DEFAULTS FILENAME-2 FILENAME-1))
  (UNWIND-PROTECT
    (PROGN
      (SETQ FILE-1 (CREATE-FILE FILENAME-1)
            FILE-2 (CREATE-FILE FILENAME-2))
      (DESCRIBE-SRCCOM-SOURCES FILE-1 FILE-2 OUTPUT-STREAM)
      (SOURCE-COMPARE-FILES FILE-1 FILE-2 OUTPUT-STREAM TYPE))
    (AND FILE-1 (SEND (FILE-STREAM FILE-1) ':CLOSE))
    (AND FILE-2 (SEND (FILE-STREAM FILE-2) ':CLOSE))))

;;;Make interface externally available.
;;;|||| Put SOURCE-COMPARE into GLOBAL for system 127. -Keith 10/18/88

(export "SOURCE-COMPARE" :srccom)

;;; Useful interface for automatic comparison
(DEFUN PROMPTED-SOURCE-COMPARE (FILE-1 FILE-2 &OPTIONAL (TYPE ':TEXT))
  ;; Add TYPE arg, 8/4/83 (RAF @ TI-CSL60).
  (MULTIPLE-VALUE (FILE-1 FILE-2)
    (GET-SRCCOM-FILE-NAMES FILE-1 FILE-2))
  (IF FILE-1
      (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Terminate source comparison now.")
         (SOURCE-COMPARE FILE-1 FILE-2 TYPE))))


(DEFUN QUERY-TYPE ()
   "Ask user for the type of compare to do.  Returns value suitable for SOURCE-COMPARE-FILES.
But currently always returns :TEXT because the other mode is useless
and the question is a pain in the neck."
   :TEXT)

;  (FQUERY '(:Choices (((:TEXT "Text") #/T) ((:FORM "Form") #/F)))
;          "Compare by Text or Forms "))

(DEFUN GET-SRCCOM-FILE-NAMES (FILE-1 FILE-2)
  (DECLARE (VALUES FILE-1 FILE-2) (SPECIAL FILE-1 FILE-2))
  (DO ((STR) (COMMA-POS))
      (NIL)
    (PROG ()
          (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Specify no file.")
            (RETURN (SETQ STR (WITH-INPUT-EDITING (*QUERY-IO*
                                                    '((:PROMPT GET-SRCCOM-FILE-NAMES-PROMPT)))
                                (READLINE *QUERY-IO*)))))
          (RETURN-FROM GET-SRCCOM-FILE-NAMES NIL))      ;If caught
    (COND ((EQUAL STR "")
           (RETURN (VALUES FILE-1 FILE-2)))
          ((SETQ COMMA-POS (STRING-SEARCH-CHAR #/, STR))
           (SETQ FILE-1 (FS:MERGE-PATHNAME-DEFAULTS (SUBSTRING STR 0 COMMA-POS) FILE-2)
                 FILE-2 (FS:MERGE-PATHNAME-DEFAULTS (SUBSTRING STR (1+ COMMA-POS)) FILE-1)))
          (T
           (SETQ FILE-1 (FS:MERGE-PATHNAME-DEFAULTS STR FILE-2))))))

(DEFUN GET-SRCCOM-FILE-NAMES-PROMPT (STREAM IGNORE)
  (DECLARE (SPECIAL FILE-1 FILE-2))
  (FORMAT STREAM "~&Going to compare ~A with ~A~@
                  ~2X(Type Return, <file-1>, <file-1,file-2>, or Abort): "
          FILE-1 FILE-2))

(DEFUN DESCRIBE-SRCCOM-SOURCES (FILE-1 FILE-2 STREAM)
  (FORMAT STREAM "~&;Source Compare of ~A and ~A on ~\datime\"
          (FILE-IDENTIFIER FILE-1)
          (FILE-IDENTIFIER FILE-2))
  (UNLESS (TYPEP STREAM 'TV:SHEET)
    (SEND STREAM ':STRING-OUT " -*-Fundamental-*-"))
  (TERPRI))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              MAIN LOOP

(DEFUN SOURCE-COMPARE-FILES (FILE-1 FILE-2
                             &OPTIONAL (*OUTPUT-STREAM* STANDARD-OUTPUT)
                                       (TYPE ':TEXT))
  "Source compare data from two SRCCOM:FILE objects, with output to
 *OUTPUT-STREAM*.  SRCCOM:FILE objects are made with SRCCOM:CREATE-FILE,
 and contain input streams.  See SRCCOM:SOURCE-COMPARE for more information."
  ;;;
  (SETF (FILE-COMPARE FILE-1) TYPE)
  (SETF (FILE-COMPARE FILE-2) TYPE)
  (IF (EQ TYPE ':FORM) (SET-FORM-VARIABLES 'START))
  (LET ((FILES-IDENTICAL T))
    (DO ((LINE-NO-1 0 (1+ LINE-NO-1))
         (LINE-NO-2 0 (1+ LINE-NO-2))
         (LINE-1) (LINE-2) (*lines-that-matched* 0))
        (NIL)
      ;; Files are current matched up, check the next two lines
      (SETQ LINE-1 (GET-FILE-LINE FILE-1 LINE-NO-1)
            LINE-2 (GET-FILE-LINE FILE-2 LINE-NO-2))
      (COND ((NULL (COMPARE-LINES LINE-1 LINE-2))
             (SETQ FILES-IDENTICAL NIL)
             (MULTIPLE-VALUE (LINE-NO-1 LINE-NO-2 LINE-1)
               (HANDLE-DIFFERENCE FILE-1 LINE-NO-1 FILE-2 LINE-NO-2))
             (setq *lines-that-matched* 0))
            (t (incf *lines-that-matched*)))
      (OR LINE-1 (RETURN NIL)))                 ;When NULL lines match both files are at EOF
    (CLOSE (FILE-STREAM FILE-1))
    (CLOSE (FILE-STREAM FILE-2))
    FILES-IDENTICAL)
  (IF (EQ TYPE ':FORM) (SET-FORM-VARIABLES 'END)))

(DEFUN SET-FORM-VARIABLES (option)              ;8/4/83 RAF @ TI-CSL60
  "OPTION = 'START => initialize free variables for form compare.
        = 'END   => reset free variables."
  (Selectq option
    (START (Setq *PRINT-LABELS* nil
                 *LINES-NEEDED-TO-MATCH* 1
                 *LINES-TO-PRINT-AFTER* 0))
    (END   (Setq *PRINT-LABELS* T
                 *LINES-NEEDED-TO-MATCH* 3
                 *LINES-TO-PRINT-AFTER* 1))))

(DEFVAR *DIFFERENCE-PRINTER* 'PRINT-DIFFERENCES
  "Function to print out information about one run of differences.")

;;; First difference detected, look ahead for a match
(DEFUN HANDLE-DIFFERENCE (FILE-1 DIFF-LINE-NO-1 FILE-2 DIFF-LINE-NO-2
                          &AUX (NEW-LINE-NO-1 DIFF-LINE-NO-1) (NEW-LINE-NO-2 DIFF-LINE-NO-2)
                               LINE)
  (DO-NAMED TOP () (NIL)
    ;; Check next line from first file against lines in the second file
    (DO ((NEW-LINE-1 (GET-FILE-LINE FILE-1 (SETQ NEW-LINE-NO-1 (1+ NEW-LINE-NO-1))))
         (LINE-NO-2 DIFF-LINE-NO-2 (1+ LINE-NO-2)))
        (NIL)
      (SETQ LINE (GET-FILE-LINE FILE-2 LINE-NO-2))
      (COND ((AND (COMPARE-LINES NEW-LINE-1 LINE)
                  (CHECK-POTENTIAL-MATCH FILE-1 NEW-LINE-NO-1 FILE-2 LINE-NO-2))
             (SETQ NEW-LINE-NO-2 LINE-NO-2)
             (RETURN-FROM TOP)))
      (AND (= LINE-NO-2 NEW-LINE-NO-2) (RETURN)))
    ;; Check next line from second file against lines from the first file
    (DO ((NEW-LINE-2 (GET-FILE-LINE FILE-2 (SETQ NEW-LINE-NO-2 (1+ NEW-LINE-NO-2))))
         (LINE-NO-1 DIFF-LINE-NO-1 (1+ LINE-NO-1)))
        (NIL)
      (SETQ LINE (GET-FILE-LINE FILE-1 LINE-NO-1))
      (COND ((AND (COMPARE-LINES LINE NEW-LINE-2)
                  (CHECK-POTENTIAL-MATCH FILE-1 LINE-NO-1 FILE-2 NEW-LINE-NO-2))
             (SETQ NEW-LINE-NO-1 LINE-NO-1)
             (RETURN-FROM TOP)))
      (AND (= LINE-NO-1 NEW-LINE-NO-1) (RETURN))))
  (FUNCALL *DIFFERENCE-PRINTER*
           FILE-1 DIFF-LINE-NO-1 NEW-LINE-NO-1
           FILE-2 DIFF-LINE-NO-2 NEW-LINE-NO-2)
  (VALUES NEW-LINE-NO-1 NEW-LINE-NO-2 LINE))

(DEFVAR *LINES-NEEDED-TO-MATCH* 3
  "Number of lines in a row that must match to finish a run of differences.")

;;; Found a potential match, check ahead to see if it is ok
(DEFUN CHECK-POTENTIAL-MATCH (FILE-1 LINE-NO-1 FILE-2 LINE-NO-2)
  (DO ((I *LINES-NEEDED-TO-MATCH* (1- I))
       (LINE-1) (LINE-2))
      (( I 0) T)
    (SETQ LINE-NO-1 (1+ LINE-NO-1)
          LINE-NO-2 (1+ LINE-NO-2))
    (SETQ LINE-1 (GET-FILE-LINE FILE-1 LINE-NO-1)
          LINE-2 (GET-FILE-LINE FILE-2 LINE-NO-2))
    (OR (COMPARE-LINES LINE-1 LINE-2)
        (RETURN NIL))))

;;; We are back in synch, print the differences
(DEFUN PRINT-DIFFERENCES (FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1
                          FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2)
  (FORMAT *OUTPUT-STREAM* "~&******** ~D lines matched." *lines-that-matched*)
  (PRINT-DIFFS-1 FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1)
  (PRINT-DIFFS-1 FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2)
  (FORMAT *OUTPUT-STREAM* "~&***************~2%"))

(DEFVAR *LINES-TO-PRINT-BEFORE* 0
  "Number of matching lines preceding a run of differences to print with the differences.")
(DEFVAR *LINES-TO-PRINT-AFTER* 1
  "Number of matching lines following a run of differences to print with the differences.")

(DEFUN PRINT-DIFFS-1 (FILE DIFF-LINE-NO SAME-LINE-NO &AUX LABEL)
  (SETQ DIFF-LINE-NO (MAX 0 (- DIFF-LINE-NO *LINES-TO-PRINT-BEFORE*))
        SAME-LINE-NO (+ SAME-LINE-NO *LINES-TO-PRINT-AFTER*))
  (FORMAT *OUTPUT-STREAM* "~&**** ~A ~A, Line #~D"
          (FILE-TYPE FILE) (FILE-NAME FILE) DIFF-LINE-NO)
  (COND ((SETQ LABEL (AND *PRINT-LABELS* (LINE-LAST-LABEL FILE DIFF-LINE-NO)))
         (SEND *OUTPUT-STREAM* ':STRING-OUT ", After /"")
         (SEND *OUTPUT-STREAM* ':STRING-OUT (STRING-REMOVE-FONTS LABEL) 0
               (MIN (STRING-LENGTH LABEL)
                    (IF (LET ((WHICH-OPERATIONS (SEND *OUTPUT-STREAM* ':WHICH-OPERATIONS)))
                          (AND (MEMQ ':READ-CURSORPOS WHICH-OPERATIONS)
                                  (MEMQ ':SIZE-IN-CHARACTERS WHICH-OPERATIONS)))
                        (- (SEND *OUTPUT-STREAM* ':SIZE-IN-CHARACTERS)
                           (SEND *OUTPUT-STREAM* ':READ-CURSORPOS ':CHARACTER)
                           1)
                      25.)))
         (SEND *OUTPUT-STREAM* ':TYO #/")))
  (SEND *OUTPUT-STREAM* ':TYO #/CR)
  (PRINT-FILE-SEGMENT FILE DIFF-LINE-NO SAME-LINE-NO))


(DEFUN PRINT-FILE-SEGMENT (file start-line-no end-line-no)
  ;; Modified to check FILE-COMPARE & pretty print 8/19/83 (RAF @ TI-CSL60).
  (DO ((LINE-NO START-LINE-NO (1+ LINE-NO))
       (LINE))
      ((= LINE-NO END-LINE-NO))
    (OR (SETQ LINE (GET-FILE-LINE FILE LINE-NO))
        (RETURN NIL))
    (ECASE (FILE-COMPARE file)
      (:TEXT (SEND *OUTPUT-STREAM* :LINE-OUT (STRING-REMOVE-FONTS LINE)))
      (:FORM (GRIND-TOP-LEVEL LINE NIL *OUTPUT-STREAM*)
             (TERPRI *OUTPUT-STREAM*)
             (TERPRI *OUTPUT-STREAM*)))))

;;; Merging
(DEFVAR *MERGE-LINE-NO* NIL
  "Line number in FILE-1 of first line after last run of differences.")

(DEFUN SOURCE-COMPARE-AUTOMATIC-MERGE (FILENAME-1 FILENAME-2 OUTPUT-FILENAME
                                       &AUX FILE-1 FILE-2)
  "Source Compare Merge two FILE objects, output to file OUTPUT-FILENAME.
The data output to the OUTPUT-STREAM contains all the text of both files.
What the files agree on is simply output.  Where they disagree,
a run of differences is printed in the usual fashion.
See SOURCE-COMPARE-FILES for some additional information."
  (PROGN
    (SETQ FILE-1 (CREATE-FILE FILENAME-1)
          FILE-2 (CREATE-FILE FILENAME-2))
    (WITH-OPEN-FILE (OUTPUT-STREAM OUTPUT-FILENAME '(:OUT))
      (SOURCE-COMPARE-AUTOMATIC-MERGE-1 FILE-1 FILE-2 OUTPUT-STREAM)))
  (AND FILE-1 (SEND (FILE-STREAM FILE-1) ':CLOSE))
  (AND FILE-2 (SEND (FILE-STREAM FILE-2) ':CLOSE)))

(DEFUN SOURCE-COMPARE-AUTOMATIC-MERGE-1 (FILE-1 FILE-2 *OUTPUT-STREAM*)
  "Do actual source compare & merge operations."
  ;; Modified to call QUERY-TYPE, 8/17/83 RAF.
  (LET ((*DIFFERENCE-PRINTER* 'PRINT-AUTOMATIC-MERGE)
        (*MERGE-LINE-NO* 0))
    (SOURCE-COMPARE-FILES FILE-1 FILE-2 *OUTPUT-STREAM* (QUERY-TYPE))
    (PRINT-FILE-SEGMENT FILE-1 *MERGE-LINE-NO* (FILE-LENGTH FILE-1))))

(DEFVAR *RECORD-MERGE-BOUNDS-P* NIL
  "T means record buffer pointers in *MERGE-RECORD*, describing differences.")
(DEFVAR *MERGE-RECORD*)
(DEFVAR *MERGE-THIS-RECORD*)

(DEFUN SOURCE-COMPARE-AUTOMATIC-MERGE-RECORDING (FILE-1 FILE-2 OUTPUT-STREAM
                                                 &AUX (*RECORD-MERGE-BOUNDS-P* T)
                                                      (*MERGE-RECORD* NIL)
                                                      *MERGE-THIS-RECORD*)
  "Source Compare Merge two FILE objects, output to OUTPUT-STREAM, record differences.
The data output to the OUTPUT-STREAM contains all the text of both files.
What the files agree on is simply output.  Where they disagree,
a run of differences is printed in the usual fashion.
See SOURCE-COMPARE-FILES for some additional information.

Recording differences:
The value returned is a record of the differences.
It is a list with one element for each difference.
The element is a list of six buffer pointers, which point to
the six identified points in this skeleton of how one difference is printed.
<1>*** in file foo
<2>data from file foo (perhaps multiple lines)
<3>*** in file bar
<4>data from file bar (perhaps multiple lines)
<5>***
<6>

This works only if OUTPUT-STREAM supports the :READ-BP operation,
as streams that write into editor buffers do."
  (SOURCE-COMPARE-AUTOMATIC-MERGE-1 FILE-1 FILE-2 OUTPUT-STREAM)
  ;; Do not CREATE these BPs with status :MOVES, or they will get
  ;; relocated by the insertion of the rest of the merged data!
  (DOLIST (RECORD (SETQ *MERGE-RECORD* (NREVERSE *MERGE-RECORD*)))
    (SETF (ZWEI:BP-STATUS (FIRST RECORD)) ':MOVES)
    (SETF (ZWEI:BP-STATUS (THIRD RECORD)) ':MOVES)
    (SETF (ZWEI:BP-STATUS (FIFTH RECORD)) ':MOVES))
  *MERGE-RECORD*)


(DEFUN PRINT-AUTOMATIC-MERGE (FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1
                              FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2)
  (PRINT-FILE-SEGMENT FILE-1 *MERGE-LINE-NO* DIFF-LINE-NO-1)
  (WHEN *RECORD-MERGE-BOUNDS-P*
    (SETQ *MERGE-THIS-RECORD* NIL)
    (RECORD-MERGE-BOUND))
;Removed by RMS, 5/29/85, to eliminate extra blank lines
;  (TERPRI *OUTPUT-STREAM*)
  (SEND *OUTPUT-STREAM* ':LINE-OUT "*** MERGE LOSSAGE ***")
  (PRINT-AUTOMATIC-MERGE-1 FILE-1 DIFF-LINE-NO-1 SAME-LINE-NO-1)
  (PRINT-AUTOMATIC-MERGE-1 FILE-2 DIFF-LINE-NO-2 SAME-LINE-NO-2)
  (SEND *OUTPUT-STREAM* ':LINE-OUT "*** END OF MERGE LOSSAGE ***")
  (WHEN *RECORD-MERGE-BOUNDS-P*
    (RECORD-MERGE-BOUND)
    (PUSH (NREVERSE *MERGE-THIS-RECORD*) *MERGE-RECORD*))
  (SETQ *MERGE-LINE-NO* SAME-LINE-NO-1))


(DEFUN PRINT-AUTOMATIC-MERGE-1 (FILE DIFF-LINE-NO SAME-LINE-NO)
  (FORMAT *OUTPUT-STREAM* "~&*** ~A HAS:~%" (FILE-IDENTIFIER FILE))
  (WHEN *RECORD-MERGE-BOUNDS-P* (RECORD-MERGE-BOUND))
  (PRINT-FILE-SEGMENT FILE DIFF-LINE-NO SAME-LINE-NO)
  (WHEN *RECORD-MERGE-BOUNDS-P* (RECORD-MERGE-BOUND)))

(DEFUN RECORD-MERGE-BOUND ()
  (PUSH (ZWEI:COPY-BP (SEND *OUTPUT-STREAM* ':READ-BP) ':NORMAL) *MERGE-THIS-RECORD*))
