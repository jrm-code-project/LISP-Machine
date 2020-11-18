;-*- Mode:LISP; Package:FORMAT; Lowercase:T; Base:8; Readtable:ZL -*-

;;; This package consists mainly of a a collection of useful formatted output functions.
;;; All use *STANDARD-OUTPUT*; none require an argument to specify the stream.
;;; The arguments follow a convention, usually, but there are exceptions.
;;; The first argument is usually the datum to be output.
;;; The second is usually the number of columns (minimum) to use.
;;; Then come any number of options, which are keywords followed by values.

;;; Most functions accept an option :TAB-PERIOD, followed by a number,
;;; which specifies how far apart the padding points are.
;;; If the minimum width of the field is 10 and the tab-period is 5,
;;; then if the datum requires 12 columns it will be padded out to 15 columns
;;; (10 plus a multiple of 5).
;;; Another option which most functions allow is :PAD-CHAR, followed by
;;; the character to pad with (instead of Space).
;;; Another option is :MINPAD, followed by the minimum number of padding characters
;;; to use.
;;; Each function's documentation says which options it accepts.

;;; Use ONUM to output a number.  You can specify the radix, the number of columns,
;;;  and other padding options.
;;; Use OFLOAT to output a floating point number with a specified number of digits.

;;; Use OCHAR to output a character.  Three formats are available:
;;;  one is a READable format using #\, one is the form ZWEI uses
;;;  to describe a character ("Control-Rubout", etc.), and one is the
;;;  SAIL way of describing a character ("X", etc).

;;; Use OSTRING to output a string with padding.
;;;  If no padding is desired PRINC is fine.
;;; Use OPRINT to PRIN1 an object with padding.

;;; Use PLURAL to output a word in either singular or plural
;;;  according to the value of a number.  You specify only the
;;;  singular and PLURAL computes the plural.  If the plural
;;;  is too irregular, you can specify it too.
;;;  (PLURAL (ONUM X) " frob") prints X followed by " frob" or " frobs".

;;; Use PAD to perform more complicated padding functions.
;;;  PAD can print several things with padding between them
;;;  to pad the whole thing out to a desired width.
;;;  Special cases include right or left justification or centering
;;;  of any output within a desired width.

;;; Use TAB to move to a specific column in the output (as opposed
;;;  to padding a field to a specific width, which is independent
;;;  of where the field starts).  Or move to the next multiple of
;;;  a tab-period; or TERPRI if already past the column.

;;; Use BREAKLINE to go to a new line before some output
;;;  if that output will not fit in the rest of the current line.
;;;  You must specify the linel since the Lisp machine system has
;;;  no convention for asking the stream.

;;; Use OUTPUT to concatenate several pieces of output.
;;;  OUTPUT is unlike the other functions in that the first argument
;;;  specifies a stream; or NIL means cons up a string, or T means
;;;  use *STANDARD-OUTPUT*.  NIL and T must appear explicitly because
;;;  they are checked for at macro expansion time.  The remaining args
;;;  to OUTPUT perform output.  A string is simply printed.
;;;  Anything else is executed to perform the output and its value is ignored.

;;; OUTFMT is like (LIST (OUTPUT NIL ...)).  It is good for places
;;; which expect a format control string; it causes FORMAT to be passed
;;; something which causes it to print exactly the output specified
;;; in the args to OUTFMT.

;;; There is nothing in this package for outputting a new line.
;;; TERPRI is fine for that.

#|
Example of use of this package: print the elements of a list,
separated by commas, going to a new line before an element which doesn't fit.

(defun pcl (list linel)
  (do ((l list (cdr l))) ((null l))
    (breakline linel "  "
      (princ (car l))
      (and (cdr l) (princ ", ")))))

Another example
(output t "Total: " (plural (onum n-events) " event") ".")

This prints "Total: 1 event." or "Total: 5 events."
|#

(defmacro outfmt (&body forms)
  "`(list (output nil . ,forms))"
  `(list (output nil . ,forms)))

(defmacro output (stream &body forms)
  "Do output to STREAM using FORMS.
Any string in FORMS is printed on STREAM; anything else in FORMS
is evaluated with *STANDARD-OUTPUT* bound to STREAM.
If STREAM is T, *STANDARD-OUTPUT* is used, and if STREAM is NIL
a string is constructed from the output, and returned.
Otherwise, STREAM is evaluated and the value used as a stream."
  (let ((do-the-work (mapcar #'output-expand forms)))
    (cond ((eq stream t)
           `(progn . ,do-the-work))
          ((eq stream nil)
           `(let ((format::*format-string* (format::make-format-string))
                  (*standard-output* #'format::format-string-stream))
              (progn . ,do-the-work)
              ;;>> this is actually the somewhat wrong thing. (conses in wrong area)
              ;;  See comments in the FORMAT function
              (let* ((len (length format::*format-string*))
                     (new (make-string len :area format::format-area)))
                (copy-array-portion format::*format-string* 0 len new 0 len)
                new)))
          (t
           `(let ((*standard-output* ,stream))
              . ,do-the-work)))))

(defun output-expand (form)
  (cond ((stringp form) `(write-string ,form))
        ((numberp form) `(tyo ,form))
        ((characterp form) `(write-char form))
        (t form)))

(defun plural (number singular &optional plural)
  "Output a word to *STANDARD-OUTPUT*, pluralized or not according to NUMBER.
SINGULAR is used if NUMBER is 1.  Otherwise, PLURAL is used,
or else a plural form of SINGULAR is computed."
  (princ (if (= number 1) singular (or plural (string-pluralize singular)))))

(defun onum (number &optional (radix 10.) (minwidth 0)
             &key signed commas (pad-char #/space) (minpad 0) (tab-period 1)
             &aux (*nopoint t) (*print-radix* nil))
  "Print NUMBER to *STANDARD-OUTPUT*.  NUMBER is returned.
RADIX can be a positive number, or :ROMAN, :ENGLISH or :ORDINAL.
If MINWIDTH is specified, we pad on the left to that total width.
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of padding characters to use (default 0).
:SIGNED T means always print a sign.
:COMMAS T means put a comma after every third digit."
  (and (minusp number) (setq signed nil))
  (cond ((and (eq radix :roman)
              (< number 4000.)
              (> number 0))
         (format::roman-step number 0 nil))
        ((eq radix :english)
         (format::english-print number))
        ((eq radix :ordinal)
         (format::english-ordinal-print number))
        (t
          (let ((*print-base* radix))
            (let ((pads
                    (- minwidth
                       (+ (flatc number)
                          minpad
                          (if signed 1 0)
                          (if (and commas (numberp number))
                              (floor (1- (flatc (abs number))) 3)   ;number of commas
                              0)))))
              (and (minusp pads)
                   (setq pads (+ pads (* tab-period (1+ (truncate (1- (- pads)) tab-period))))))
              (dotimes (i pads)
                (write-char pad-char)))
            (and signed (write-char #/+))
            ;; this is princ rather than prin1 so you can have a string instead of a number
            (if (not (and commas (numberp number))) (princ number)
              ;; random hair with commas.  I'm not going to bother not consing.
              (when (minusp number)
                (write-char #/-)
                (setq number (- number)))
              (let ((numb (nreverse (inhibit-style-warnings ;give up!
                                      (exploden number)))))
                (do ((l numb (cdr l))
                     (i 2 (1- i)))
                    ((null (cdr l)))
                  (cond ((zerop i)
                         (rplacd l (cons #/, (cdr l)))
                         (setq i 3 l (cdr l)))))
                (dolist (ch (nreverse numb))
                  (tyo ch)))))))
  number)

(defun ofloat (number &optional digits force-exponential-notation minwidth &rest options)
  "Print floating point NUMBER to *STANDARD-OUTPUT*.  NUMBER is returned.
DIGITS is the number of significant digits to print.
FORCE-EXPONENTIAL-NOTATION if T says print with an explicit exponent
 even if the number could be printed without one.
If MINWIDTH is specified, we pad on the left to that total width.
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of padding characters to use (default 0)."
  (declare (arglist number &optional digits force-exponential-notation minwidth
                    &key (pad-char #/space) (minpad 0) (tab-period 1)))
  (let ((number (if (and (numberp number) (not (floatp number)))
                    (float number) number)))
    (cond ((or minwidth options)
           (with-stack-list (l (output nil (ofloat number digits force-exponential-notation)))
             (apply #'pad1 l minwidth options)))
          ((numberp number)
           (si:print-flonum number *standard-output* (small-floatp number)
                            digits force-exponential-notation))
          (t (princ number))))
  number)

(defun ostring (string &optional minwidth &rest options)
  "Print STRING to *STANDARD-OUTPUT* without quoting.  STRING is returned.
If MINWIDTH is specified, we pad to that total width.
:RIGHT-JUSTIFY T says pad on the left; otherwise, pad on the right.
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of padding characters to use (default 0)."
  (declare (arglist string &optional minwidth
                    &key right-justify (pad-char #/space) (minpad 0) (tab-period 1)))
  (apply #'ostring1 string #'princ #'flatc minwidth options))

(defun oprint (object &optional minwidth &rest options)
  "Print OBJECT to *STANDARD-OUTPUT* with quoting.  OBJECT is returned.
If MINWIDTH is specified, we pad to that total width.
:RIGHT-JUSTIFY T says pad on the left; otherwise, pad on the right.
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of padding characters to use (default 0)."
  (declare (arglist string &optional minwidth
                    &key right-justify (pad-char #/space) (minpad 0) (tab-period 1)))
  (apply #'ostring1 object #'prin1 #'flatsize minwidth options))

(defun ostring1 (object print-function flatsize-function minwidth
                 &key right-justify (tab-period 1)
                 (pad-char #/space) (minpad 0))
  (and minwidth
       (let ((size-excess (- (+ minpad (funcall flatsize-function object)) minwidth)))
         (setq minpad (- minpad size-excess))
         (when (> size-excess 0)
           (incf minpad (* (1+ (truncate (1- size-excess) tab-period)) tab-period)))))
  (and minwidth right-justify
       (dotimes (i minpad)
         (write-char pad-char)))
  (funcall print-function object)
  (and minwidth (not right-justify)
       (dotimes (i minpad)
         (write-char pad-char)))
  object)

(defun tab (mincol &key terpri (pad-char #/space) (tab-period 1)
                        (minpad 0) (unit :character))
  "Pad out to a column MINCOL on *STANDARD-OUTPUT*.
:PAD-CHAR specifies the character to pad with (default is spaces).
:TAB-PERIOD  specifies how far apart possible stopping places are, after MINCOL;
 that is, padding can stop at MINCOL + TAP-PERIOD * n for integral n.
:MINPAD specifies a minimum number of padding characters to use.
:UNIT :PIXEL means work with pixels, not characters, as the unit of horizontal space.
:TERPRI T means if we are already past MINCOL, go to a new line
 and go to mincol on that line.
When outputting to a string, we count the beginning of the string as column 0."
  (let ((ops (send *standard-output* :which-operations)))
    (if (memq :read-cursorpos ops)
        (multiple-value-bind (x y)
            (send *standard-output* :read-cursorpos unit)
          (let ((excess (- (+ minpad x) mincol)))
            (setq minpad (- minpad excess))
            (and (> excess 0)
                    (cond (terpri (terpri) (setq minpad mincol))
                          (t (setq minpad
                                   (+ minpad
                                      (* (1+ (truncate (1- excess) tab-period)) tab-period))))))
            (cond ((and (or (eq unit :pixel)
                            (char= pad-char #/space))
                        (memq :increment-cursorpos ops))
                   (send *standard-output* :increment-cursorpos minpad 0 unit))
                  ((eq unit :character)
                   (dotimes (i minpad) (write-char pad-char)))
                  (t
                   (send *standard-output* :set-cursorpos (+ x minpad) y unit)))))
      (send *standard-output* :string-out "   "))))

;;; Print a user-understandable name for a single character.
;;; Char is the character to print.
;;; Style selects among three styles:
;;; the default (called :READ) is to use #\ to print something that can be read back in.
;;; :BRIEF means print the character itself unless it has control bits;
;;; otherwise like :READ but omits the macros and slashes.
;;; Using :EDITOR as the style means to spell everything out, as in "Meta-Return".
;;; :SAIL as the style means to use alpha, beta etc. for the control bits
;;; always followed by the character itself, never a name.

;;; top-explain, if T, means to explain how to type any character
;;; that requires using top or greek, as in " (Top-Z)".  This is useful with
;;; :editor and :sail, not with :read.

(defun ochar (char &optional style top-explain minwidth &rest options
                   &aux chname bits char0 char1)
  "Print the character CHAR in a fancy manner on *STANDARD-OUTPUT*.
STYLE specifies how to print:
 :READ (the default) means print it with #\ in a way that can be read back in;
 :BRIEF means print the character verbatim if it has no control bits,
  otherwise print its name if it's not a graphic char.
 :EDITOR means always use the name except for graphic chars,
  and use verbose forms of prefixes (/"Meta-/" instead of /"M-/").
 :SAIL means use , , etc as prefixes for the control bits
  and never use names.
 :LOZENGED means put the character name in a lozenge if stream can do so,
  unless the character is a graphic character.
  The padding options should not be used with :LOZENGED.
TOP-EXPLAIN if T means add on an explanation of how to type the character
 on the keyboard, using the Top or Greek key, if appropriate.
If MINWIDTH is specified, we pad on the right to that width.
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of padding characters to use (default 0)."
  (declare (arglist char &optional style top-explain minwidth
                    &key (pad-char #/space) (minpad 0) (tab-period 1)))
  (setq char0
        (if (eq (car-safe char) :mouse)
            (cli:character (third char))
            (cli:character char)))
  (setq char1 (char-code char0))
  (cond ((or minwidth options)
         (with-stack-list (l (output nil (ochar char0 style top-explain)) nil)
           (apply #'pad1 l minwidth options)))
        ((eq style :lozenged)
         (if (and ( char1 #o200)
                  (send *standard-output* :operation-handled-p :display-lozenged-string))
             (send *standard-output* :display-lozenged-string
                   (output nil (ochar char0 :editor)))
           (ochar char0 :editor))
         (when top-explain
           (ochar-explain-top-character char1)))
        ((tv:char-mouse-p char0)
         (cond ((eq style :read)
                (or (setq chname (ochar-get-character-name char0))
                    (ferror nil "No name known for mouse character ~C" char0))
                (send *standard-output* :string-out "#\")
                (princ chname))
               (t (setq bits (char-bits char0))
                  (and (bit-test char-hyper-bit bits)
                       (send *standard-output* :string-out "Hyper-"))
                  (and (bit-test char-super-bit bits)
                       (send *standard-output* :string-out "Super-"))
                  (and (bit-test char-control-bit bits)
                       (send *standard-output* :string-out "Control-"))
                  (and (bit-test char-meta-bit bits)
                       (send *standard-output* :string-out "Meta-"))
                  (send *standard-output* :string-out "Mouse-")
                  (send *standard-output* :string-out (nth (ldb %%kbd-mouse-button char0)
                                                           '("Left" "Middle" "Right")))
                  (if (setq chname (nth (setq bits (ldb %%kbd-mouse-n-clicks char0))
                                        '("" "-Twice" "-Thrice")))
                      (send *standard-output* :string-out chname)
                      (write-char #/- *standard-output*)
                    (english-print (1+ bits))
                    (send *standard-output* :string-out "-Times")))))
        (t
          (case style
            (:editor
              (setq bits (char-bits char0))
              (and (bit-test char-hyper-bit bits)
                   (send *standard-output* :string-out "Hyper-"))
              (and (bit-test char-super-bit bits)
                   (send *standard-output* :string-out "Super-"))
              (and (bit-test char-control-bit bits)
                   (send *standard-output* :string-out "Control-"))
              (and (bit-test char-meta-bit bits)
                   (send *standard-output* :string-out "Meta-"))
              (cond ((setq chname (ochar-get-character-name char1))
                     (let ((str (string-downcase chname)))
                       (setf (char str 0) (char-upcase (char str 0)))
                       (send *standard-output* :string-out str)
                       ;(return-array (prog1 str (setq str nil)))
                       ))
                    ((and (not (zerop bits)) ( (char-int #/a) char1 (char-int #/z)))
                     (send *standard-output* :string-out "Shift-")
                     (tyo (char-upcase char1)))
                    (t
                     (tyo char1))))
            ((nil :read :brief)
             (setq bits (char-bits char0))
             (if (zerop bits) (setq bits nil))
             ;; In :READ style, get a character name if possible.
             ;; In :BRIEF style, get one only if there are control bits.
             (when (or bits (neq style :brief))
               (setq chname (ochar-get-character-name char1)))
             (unless (eq style :brief) (send *standard-output* :string-out "#\"))
             ;; Now announce the control bits.
             (if bits (send *standard-output*
                            :string-out
                            (nth bits
                                 '("" "c-" "m-" "c-m-"
                                   "s-" "c-s-" "m-s-" "c-m-s-"
                                   "h-" "c-h-" "m-h-" "c-m-h-"
                                   "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-"))))
             ;; If we did get a character's long-name, for whatever reason, use it.
             (if chname
                 (let ((str (string-downcase chname)))
                   (if (eq style :brief)
                       (setf (char str 0) (char-upcase (char str 0))))
                   (send *standard-output* :string-out str)
                   ;(return-array (prog1 str (setq str nil)))
                   )
               ;; Otherwise print the character itself.
               ;; In :READ style, print a slash before chars that want it.
               (and (neq style :brief)
                    bits
                    ;; If using #\ but using the character, not the name, may need a slash.
                    (if (lower-case-p char1)
                        (send *standard-output* :string-out "sh-")
                      (if (si:character-needs-quoting-p char1)
                          (tyo (si:pttbl-slash *readtable*)))))
               (write-char char1)))
            (:sail (send *standard-output* :string-out (nth (char-bits char0)
                                                             '("" "" "" ""
                                                               "" "" "" ""
                                                               "" "" "" ""
                                                               "" "" "" "")))
                   (and (memq char1 '(#.(char-code #/)
                                      #.(char-code #/)
                                      #.(char-code #/)
                                      #.(char-code #/)
                                      #.(char-code #/)
                                      #.(char-code #/)))
                        (write-char #/))
                   (send *standard-output* :tyo char1)))
          (and top-explain
               (ochar-explain-top-character char1)))))

(defun ochar-get-character-name (char)
;character lossage
  (setq char (int-char char))
  (unless (and (graphic-char-p char)
               (char char #/space)
               (char char #/altmode))
    (char-name char)))

;;; If char is a top or greek character, explain how to type it.
;;; Print " (Top-mumble)".  If char is not a top or greek char, do nothing.
;character lossasge
(defun ochar-explain-top-character (char &aux name chname)
  (if (fixnump char) (setq char (int-char char)))
  (let ((code (char-int char)))
    (cond ((setq chname (dotimes (i #o200)
                          (when (= code (aref si:kbd-new-table 2 i))
                            (return (int-char (aref si:kbd-new-table 1 i))))))
           (setq name " (Top-"))
          ((setq chname (dotimes (i #o200)
                          (when (= code (aref si:kbd-new-table 3 i))
                            (return (int-char (aref si:kbd-new-table 0 i))))
                          (when (= code (aref si:kbd-new-table 4 i))
                            (return (int-char (aref si:kbd-new-table 1 i))))))
           (setq name (if (alpha-char-p chname) " (Greek-" " (Front-")))))
  (when chname
    (send *standard-output* :string-out name)
    (ochar chname :editor)
    (write-char #/))))

(defmacro pad ((minwidth . options) &body forms)
  "Print several items in a fixed horizontal space, padding between them.
Each of FORMS is one item: either a constant string to be printed,
an expression to print it, or NIL which is an empty item.
No padding is put before the first item or after the last;
to get padding at the front or back, put in a NIL as the first or last item.
MINWIDTH is evaluated to compute the number of columns available.
OPTIONS are evaluated to produce a keyword argument list
 which can contain these keywords:
:PAD-CHAR specifies the character to pad with.
:TAB-PERIOD specifies the unit in which to add extra columns
 if we must go past MINWIDTH (default 1).
:MINPAD specifies the minimum number of
 padding characters between items (default 0)."
  `(pad1 (list . ,(mapcar #'(lambda (f)
                              (cond ((stringp f) f)
                                    (f `(output nil ,f))
                                    (t "")))
                          forms))
         ,minwidth . ,options))

(defun pad1 (strings minwidth &key (pad-char #/space) (tab-period 1) (minpad 0))
  (let ((strings-length 0) total-padding)
    (or (cdr strings) (setq strings (cons "" strings)))
    (dolist (string strings) (setq strings-length (+ (string-length string) strings-length)))
    ;; get the amount of space needed to print the strings and minpad padding
    (setq total-padding (+ (* (1- (length strings)) minpad) strings-length))
    ;; now bring in the minwidth and tab-period constraint, i.e. the total width is
    ;; at least minwidth and exceeds minwidth by a multiple of tab-period, and
    ;; get the total amount of padding to be divided among the padding points
    (setq total-padding (- (+ minwidth
                              (* tab-period (ceiling (max (- total-padding minwidth) 0)
                                                     tab-period)))
                           strings-length))
    ;; output the stuff
    (do ((strings strings (cdr strings))
         (n-pads (floor total-padding (1- (length strings))))
         (j (\ total-padding (1- (length strings))) (1- j)))
        ((null strings))
      (send *standard-output* :string-out (car strings))
      (when (cdr strings)
        (dotimes (i n-pads) (write-char pad-char))
        (and (> j 0)
             (write-char pad-char))))))

(defmacro breakline (linel print-if-terpri &body body &aux (gensym (gensym)))
  "Go to a new line if necessary before executing BODY.
If the output printed by the BODY does not fit on this line within LINEL,
do a TERPRI and execute PRINT-IF-TERPRI before actually outputting."
  `(let ((,gensym (output nil . ,body)))
     (when (> (+ (send *standard-output* :read-cursorpos :character)
                 (length ,gensym))
              ,linel)
       (terpri)
       ,(output t print-if-terpri))
     (princ ,gensym)))
