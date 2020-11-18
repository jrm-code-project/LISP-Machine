;;; -*- Mode:LISP; Package:LISP-IO; Readtable:CL; Base:10 -*-
;;;
;;; FORMAT.LISP
;;;
;;; FORMAT Interpreter


;; Problems:  parsing ,,,
;; cryptic error messages.  how should they work, anyway? nice errors.
;; check types of arguments
;; piggyback stream for column counting?
;; running off the end inside nested [(<{

;;;----------------------------------------
;;; Entry
;;;----------------------------------------


(defun format (destination control-string &rest arguments)
  (let ((stream (cond ((streamp destination)
                       destination)
                      ((eq destination NIL)
                       (make-string-output-stream))
                      ((eq destination T)
                       *standard-output*)
                      ((stringp destination)
                       (error "Can't do strings yet"))
                      (t
                       (error "Bad destination to FORMAT.")))))
    (format-main (make-string-iterator control-string)
                 (make-format-args (copy-list arguments))
                 stream)
    (cond ((eq destination NIL)
           (get-output-stream-string stream))
          (t
           NIL))))


;;;----------------------------------------
;;; Representation of format arguments
;;;----------------------------------------


(defstruct format-args-rep
  "ALL-ARGUMENTS is the complete list of format arguments.  NEXT-ARGUMENT is a
list of all the arguments remaining to be processed by format.  POSITION is the
number of arguments that have been processed so far.  To remain consistent,
NEXT-ARGUMENT = (NTHCDR POSITION ALL-ARGUMENTS)."
  position all-arguments next-argument)


(defun make-format-args (format-argument-list)
  "Represent the list of format arguments in a more convenient manner."
  (make-format-args-rep :position      0
                        :all-arguments format-argument-list
                        :next-argument format-argument-list))


(defun next-format-arg (format-args)
  "Return the next format argument available."
  (if (format-args-rep-next-argument format-args)
      (progn (incf (format-args-rep-position format-args))
             (pop  (format-args-rep-next-argument format-args)))
      (error "No arguments left.")))


(defun any-more-format-args? (format-args)
  "Return T if there are more format arguments available, NIL if not."
  (and (format-args-rep-next-argument format-args) t))


(defun number-of-remaining-format-args (format-args)
  (length (format-args-rep-next-argument format-args)))


(defun back-up-one-format-arg (format-args)
  (if (zerop (format-args-rep-position format-args))
      (error "Can't back up.")
      (progn
        (decf (format-args-rep-position format-args))
        (setf (format-args-rep-next-argument format-args)
              (nthcdr (format-args-rep-position format-args)
                      (format-args-rep-all-arguments format-args))))))


;;;----------------------------------------
;;; String element iteration
;;;----------------------------------------


(defstruct string-iterator-rep
  string length index most-recent-twiddle-index most-recent-open-index)


(defun make-string-iterator (string)
  (make-string-iterator-rep :string string
                            :length (length string)
                            :index  0
                            :most-recent-twiddle-index NIL
                            :most-recent-open-index NIL))




(defun next-character (string-iterator)
  (let ((index (string-iterator-rep-index string-iterator)))
    (if (>= index (string-iterator-rep-length string-iterator))
        (format-error string-iterator "Unexpected end of format string.")
        (prog1 (char (string-iterator-rep-string string-iterator) index)
               (incf (string-iterator-rep-index string-iterator))))))


(defun peek-character (string-iterator)
  (let ((index (string-iterator-rep-index string-iterator)))
    (if (>= index (string-iterator-rep-length string-iterator))
        (format-error string-iterator "Unexpected end of format string.")
        (char (string-iterator-rep-string string-iterator) index))))


(defun more-characters? (string-iterator)
  (< (string-iterator-rep-index string-iterator)
     (string-iterator-rep-length string-iterator)))


(defun grab-current-index (string-iterator)
  (string-iterator-rep-index string-iterator))


(defun set-current-index (string-iterator index)
  (setf (string-iterator-rep-index string-iterator) index))


(defun note-most-recent-twiddle-index (string-iterator)
  (setf (string-iterator-rep-most-recent-twiddle-index string-iterator)
        (1- (string-iterator-rep-index string-iterator))))


(defun get-most-recent-twiddle-index (string-iterator)
  (string-iterator-rep-most-recent-twiddle-index string-iterator))


(defun note-most-recent-open-index (string-iterator)
  (setf (string-iterator-rep-most-recent-open-index string-iterator)
        (1- (string-iterator-rep-index string-iterator))))


(defun get-most-recent-open-index (string-iterator)
  (string-iterator-rep-most-recent-open-index string-iterator))


(defun get-entire-format-string (string-iterator)
  (string-iterator-rep-string string-iterator))


;;;----------------------------------------
;;; Main loop and dispatch
;;;----------------------------------------


(defun format-main (format-string-iterator format-args stream)
  (when (more-characters? format-string-iterator)
    (let ((character (next-character format-string-iterator)))
      (if (char= character #\~)
          (multiple-value-bind (dispatch-char directive-args colonp atsignp)
              (parse-after-twiddle format-string-iterator format-args nil)
            (funcall (lookup-format-dispatch dispatch-char)
                     format-args
                     directive-args
                     colonp
                     atsignp
                     stream
                     format-string-iterator))
          (write-char character stream)))
    (format-main format-string-iterator format-args stream)))


(defun lookup-format-dispatch (dispatch-char)
  (case dispatch-char
    (#\A 'format-A)
    (#\B 'format-B)
    (#\C 'format-C)
    (#\D 'format-D)
    (#\O 'format-O)
    (#\P 'format-P)
    (#\R 'format-R)
    (#\S 'format-S)
    (#\X 'format-X)
    (#\% 'format-%)
    (#\& 'format-&)
    (#\( 'format-\()
    (#\[ 'format-[)
    (#\{ 'format-{)
    (#\< 'format-<)
    (t
     (error "Unrecognized format dispatch character ~C." dispatch-char))))


;;;----------------------------------------
;;; Parsing the format string
;;;----------------------------------------


;(defun parse-after-twiddle (format-string-iterator format-args)
;  "Begin parsing the format string immediately after a ~.  Return four values:
;\(1) the format dispatch character, (2) a list of the prefix parameters, (3) a
;boolean value indicating whether or not a colon modifier was specified, and (4)
;a boolean value indicating whether or not an at-sign modifier was specified."
;  (let (dispatch-char parameters colonp at-sign-p)
;    (loop
;      (if (not (more-characters? format-string-iterator))
;         (error "Unexpected end of format.")
;         (let ((next-char
;                 (char-upcase (next-character format-string-iterator))))
;           (cond
;             ((find next-char "+-0123456789")
;              (push (pluck-integer next-char format-string-iterator)
;                    parameters))
;             ((char= next-char #\V)
;              (push (next-format-arg format-args) parameters))
;             ((char= next-char #\,)
;              (when (char= (peek-character format-string-iterator) #\,)
;                (push NIL parameters)))
;             ((char= next-char #\#)
;              (push (number-of-remaining-format-args format-args) parameters))
;             ((char= next-char #\')
;              (push (next-character format-string-iterator) parameters))
;             ((char= next-char #\:)
;              (if colonp
;                  (error "More than one colon in format directive.")
;                  (setq colonp t)))
;             ((char= next-char #\@)
;              (if at-sign-p
;                  (error "More than one at-sign in format directive.")
;                  (setq at-sign-p t)))
;             (t
;              (setq dispatch-char next-char)
;              (return))))))
;    (values dispatch-char (nreverse parameters) colonp at-sign-p)))


(defun parse-after-twiddle (format-string-iterator format-args vacuous-p)
  "Parse a format directive, immediately following a twiddle.  If VACUOUS-P is
true, then treat all prefix parameters as NIL, and no side effects will happen
to FORMAT-ARGS.  Return five values:  (1) the format dispatch character, (2) a
list of the prefix parameters, (3) a boolean value indicating whether or not a
colon modifier was specified, (4) a boolean value indicating whether or not an
at-sign modifier was specified, and (5) a string containing the text of the
format directive."
  (note-most-recent-twiddle-index format-string-iterator)
  (let (dispatch-char parameters colonp at-sign-p text)
    (let ((type-of-last-thing :TWIDDLE))
      (loop
        (let ((next-char (char-upcase (next-character format-string-iterator))))
          (push next-char text)
          (cond
            ((find next-char "+-0123456789V#'")
             (require-can-follow :ARGUMENT type-of-last-thing)
             (push
               (if vacuous-p
                   NIL
                   (case next-char
                     (#\V (next-format-arg format-args))
                     (#\# (number-of-remaining-format-args format-args))
                     (#\' (next-character format-string-iterator))
                     (t   (pluck-integer next-char format-string-iterator))))
               parameters)
             (setq type-of-last-thing :ARGUMENT))
            ((char= next-char #\,)
             (require-can-follow :COMMA type-of-last-thing)
             (when (member type-of-last-thing '(:TWIDDLE :COMMA))
               (push NIL parameters))
             (setq type-of-last-thing :COMMA))
            ((char= next-char #\:)
             (require-can-follow :COLON type-of-last-thing)
             (setq colonp t)
             (setq type-of-last-thing :COLON))
            ((char= next-char #\@)
             (require-can-follow :AT-SIGN type-of-last-thing)
             (setq at-sign-p t)
             (setq type-of-last-thing :AT-SIGN))
            (t
             (require-can-follow :DIRECTIVE type-of-last-thing)
             (setq dispatch-char next-char)
             (return))))))
    (values dispatch-char
            (nreverse parameters)
            colonp
            at-sign-p
            (coerce (nreverse text) 'string))))


(defun require-can-follow (foo bar)
  "Raise an error if FOO can't follow BAR."
  (unless
    (member foo
            (case bar
              (:TWIDDLE  '(:ARGUMENT :COMMA :COLON :AT-SIGN :DIRECTIVE))
              (:ARGUMENT '(:COMMA :COLON :AT-SIGN :DIRECTIVE))
              (:COMMA    '(:ARGUMENT :COMMA))
              (:COLON    '(:AT-SIGN :DIRECTIVE))
              (:AT-SIGN  '(:DIRECTIVE))))
    (error "Bad parameter syntax.")))


(defun pluck-integer (first-char format-string-iterator)
  (let ((digits (list first-char)))
    (loop
      (let ((next-char (peek-character format-string-iterator)))
        (cond
          ((digit-char-p next-char)
           (next-character format-string-iterator)
           (push next-char digits))
          ((find next-char "+-Vv#'")
           (error "Bad syntax after numeric argument."))
          (t
           (return)))))
    (parse-integer (coerce (nreverse digits) 'string))))


(defun scan-a-chunk (terminating-character format-string-iterator)
  "March along the format string until a twiddle-TERMINATING-CHARACTER or a
twiddle-semicolon directive is encountered.  Skip over any nested ~(-~),
~[-~], ~{-~}, or ~<-~> pairs.  Return three values:  a string containing the
characters scanned over (without the directive that terminated it), the
character that terminated it, and a string containing the format directive
that terminated it."
  (let ((characters (make-string-output-stream))
        (actual-terminating-character NIL)
        (terminating-string)
        (paren-depth 0)
        (bracket-depth 0)
        (brace-depth 0)
        (angle-depth 0))
    (loop
      (let ((character (next-character format-string-iterator)))
        (if (char= character #\~)
            (multiple-value-bind
              (dispatch-char ignore ignore ignore directive-string)
                (parse-after-twiddle format-string-iterator nil t)
              (if (and (or (char= dispatch-char terminating-character)
                           (char= dispatch-char #\;))
                       (zerop paren-depth)
                       (zerop bracket-depth)
                       (zerop brace-depth)
                       (zerop angle-depth))
                  (progn (setq actual-terminating-character dispatch-char)
                         (setq terminating-string directive-string)
                         (return))
                  (progn (write-char #\~ characters)
                         (write-string directive-string characters)
                         (case dispatch-char
                           (#\( (incf paren-depth))
                           (#\) (decf paren-depth))
                           (#\[ (incf bracket-depth))
                           (#\] (decf bracket-depth))
                           (#\{ (incf brace-depth))
                           (#\} (decf brace-depth))
                           (#\< (incf angle-depth))
                           (#\> (decf angle-depth))))))
            (write-char character characters))))
    (values (get-output-stream-string characters)
            actual-terminating-character
            terminating-string)))


(defun parse-innards (format-string-iterator terminating-character)
  "A ~(, ~[, ~{, or ~< construct should just have been entered.  Break up the
innards of the construct into substrings separated by ~; directives.  Return
two values:  (1) a list of the substrings; (2) a list of the directive strings
that terminated each substring.  Leave the format string iterator after the end
of the closing directive."
  (let ((substrings NIL)
        (directive-strings NIL))
    (loop
      (multiple-value-bind (string directive-char terminating-directive)
          (scan-a-chunk terminating-character format-string-iterator)
        (push string substrings)
        (push terminating-directive directive-strings)
        (unless (char= directive-char #\;)
          (return))))
    (values (nreverse substrings) (nreverse directive-strings))))


;;;----------------------------------------
;;; Error checking
;;;----------------------------------------


(defun maximum-prefix-arguments (maximum prefix-arguments
                                 format-string-iterator)
  "Raise an error if there are more than MAXIMUM elements in PREFIX-ARGUMENTS."
  (let ((length (length prefix-arguments)))
    (when (> length maximum)
      (format-error format-string-iterator
                    "More than ~S prefix parameters supplied to this format directive."
                    maximum))))


(defun forbid-atsign (atsignp format-string-iterator)
  "Raise an error if ATSIGNP is true."
  (when atsignp
    (format-error format-string-iterator
                  "A @ modifier is not allowed in this format directive.")))


(defun forbid-colon (colonp format-string-iterator)
  "Raise an error if COLONP is true."
  (when colonp
    (format-error format-string-iterator
                  "A : modifier is not allowed in this format directive.")))


(defun forbid-both (colonp atsignp format-string-iterator)
  "Raise an error if both COLONP and ATSIGNP are true."
  (when (and colonp atsignp)
    (format-error format-string-iterator
                  "A :@ modifier is not allowed in this format directive.")))


(defun prefix-parameter-syntax (directive types parameters
                                format-string-iterator)
  "Require that there be no more than (length TYPES) PARAMETERS, and that each
parameter be of the corresponding type in the TYPES list.  TYPES is a list of
keywords, each of which must be :CHAR, :NUMBER, or :EITHER.  Raise an error if
things aren't hunky-dory.  DIRECTIVE is a string suitable for printing in the
error message."
  (let ((maximum-number-of-parameters-allowed (length types))
        (number-of-parameters-present (length parameters)))
    (if (> number-of-parameters-present maximum-number-of-parameters-allowed)
        (format-error
          format-string-iterator
          "~D parameters supplied to ~A directive; at most ~D are allowed."
          number-of-parameters-present
          directive
          maximum-number-of-parameters-allowed)
        (dotimes (i (length parameters))
          (let ((parameter            (nth i parameters))
                (type                 (nth i types))
                (required-type-string NIL))
            (setq required-type-string
                  (case type
                    (:CHAR   (unless (characterp parameter)
                               "a character"))
                    (:NUMBER (unless (typep parameter 'integer)
                               "an integer"))
                    (:EITHER (unless (or (characterp parameter)
                                         (typep parameter 'integer))
                               "a character or an integer"))))
            (when required-type-string
              (format-error
                format-string-iterator
                "The ~:R parameter to the ~A directive must be ~A."
                (1+ i)
                directive
                required-type-string)))))))


;;;----------------------------------------
;;; Ugly format directives
;;;----------------------------------------


(defun format-A (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (prefix-parameter-syntax #\A
                           '(:NUMBER :NUMBER :NUMBER :CHAR)
                           directive-args
                           format-string-iterator)
;  (maximum-prefix-arguments 4 directive-args format-string-iterator)
  (let ((object (next-format-arg format-args)))
    (cond ((and (null object) (not colonp))
           (setq object "nil"))
          ((and (null object) colonp)
           (setq object "()")))
    (if (not directive-args)
        (princ object stream) ;; the easy case
        (let ((mincol  (or (first directive-args) 0))
              (colinc  (or (second directive-args) 1))
              (minpad  (or (third directive-args) 0))
              (padchar (or (fourth directive-args) #\Space))
              (string  (princ-to-string object)))
          (pad string atsignp stream mincol colinc minpad padchar)))))


(defun format-S (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 4 directive-args format-string-iterator)
  (let* ((object (next-format-arg format-args))
         (string (if (and (null object) colonp)
                     "()"
                     (prin1-to-string object))))
    (if (not directive-args)
        (princ string stream) ;; the easy case
        (let ((mincol  (or (first directive-args) 0))
              (colinc  (or (second directive-args) 1))
              (minpad  (or (third directive-args) 0))
              (padchar (or (fourth directive-args) #\Space)))
          (pad string atsignp stream mincol colinc minpad padchar)))))


(defun pad (string pad-left-p stream mincol colinc minpad padchar)
  (let*
    ((length         (length string))
     (space-to-fill  (max 0 (- mincol length)))
     (padding-length (max minpad (* colinc (ceiling space-to-fill colinc))))
     (padding-string (make-string padding-length :initial-element padchar)))
    (if pad-left-p
        (progn (princ padding-string stream)
               (princ string stream))
        (progn (princ string stream)
               (princ padding-string stream)))))


(defun integer-to-string (integer radix comma force-sign-p)
  "Return a string representing INTEGER in base RADIX.  If COMMA is not NIL, it
should be a character to use between groups of 3 digits.  If FORCE-SIGN-P is
true and the number is positive, print a + sign in front of it."
  (let ((digit-list NIL)
        (digit-count 0))
    (labels ((iter (integer)
               (when (and comma (> digit-count 0) (= (mod digit-count 3) 0))
                 (push comma digit-list))
               (multiple-value-bind (most-of-the-digits the-last-digit)
                   (floor integer radix)
                 (push (digit-char the-last-digit radix) digit-list)
                 (incf digit-count)
                 (when (> most-of-the-digits 0)
                   (iter most-of-the-digits)))))
      (iter (abs integer))
      (when (or force-sign-p (minusp integer))
        (push (if (minusp integer) #\- #\+) digit-list))
      (coerce digit-list 'string))))


(defun format-integer (object radix mincol padchar comma force-sign-p stream)
  (let ((string (if (integerp object)
                    (integer-to-string object radix comma force-sign-p)
                    (let ((*print-base* radix))
                      (princ-to-string object)))))
    (pad string t stream mincol 1 0 padchar)))


(defun format-D (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 3 directive-args format-string-iterator)
  (let* ((object    (next-format-arg format-args))
         (mincol    (or (first directive-args)  0))
         (padchar   (or (second directive-args) #\Space))
         (commachar (or (third directive-args)  #\,)))
    (format-integer object 10. mincol padchar
                    (and colonp commachar) atsignp stream)))


(defun format-B (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 3 directive-args format-string-iterator)
  (let* ((object    (next-format-arg format-args))
         (mincol    (or (first directive-args)  0))
         (padchar   (or (second directive-args) #\Space))
         (commachar (or (third directive-args)  #\,)))
    (format-integer object 2. mincol padchar
                    (and colonp commachar) atsignp stream)))


(defun format-O (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 3 directive-args format-string-iterator)
  (let* ((object    (next-format-arg format-args))
         (mincol    (or (first directive-args)  0))
         (padchar   (or (second directive-args) #\Space))
         (commachar (or (third directive-args)  #\,)))
    (format-integer object 8. mincol padchar
                    (and colonp commachar) atsignp stream)))


(defun format-X (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 3 directive-args format-string-iterator)
  (let* ((object    (next-format-arg format-args))
         (mincol    (or (first directive-args)  0))
         (padchar   (or (second directive-args) #\Space))
         (commachar (or (third directive-args)  #\,)))
    (format-integer object 16. mincol padchar
                    (and colonp commachar) atsignp stream)))


(defun format-R (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 4 directive-args format-string-iterator)
  (let ((object (next-format-arg format-args)))
    (if directive-args
        (let ((radix     (first directive-args))
              (mincol    (or (second directive-args) 0))
              (padchar   (or (third directive-args) #\Space))
              (commachar (or (fourth directive-args) #\,)))
          (format-integer object radix mincol padchar
                          (and colonp commachar) atsignp stream))
        (cond ((and colonp atsignp)
               (print-old-roman-numeral object stream))
              (atsignp
               (print-roman-numeral object stream))
              (colonp
               (print-ordinal-number object stream))
              (t
               (print-cardinal-number object stream))))))


(defun print-roman-numeral (x stream)
  (roman-numeral-loop x 0 nil stream))


(defun print-old-roman-numeral (x stream)
  (roman-numeral-loop x 0 t stream))


(defun roman-numeral-loop (x level oldp stream)
  (when (> x 9)
    (multiple-value-bind (more-significant-digits least-significant-digit)
        (floor x 10)
      (if (> level 1)
          (dotimes (i more-significant-digits)
            (write-char #\M stream))
          (roman-numeral-loop more-significant-digits (1+ level) oldp stream))
      (setq x least-significant-digit)))
  (cond ((and (= x 9) (not oldp))
         (print-roman-I-char level stream)
         (print-roman-I-char (1+ level) stream))
        ((= x 5)
         (print-roman-V-char level stream))
        ((and (= x 4) (not oldp))
         (print-roman-I-char level stream)
         (print-roman-V-char level stream))
        ((> x 5)
         (print-roman-V-char level stream)
         (dotimes (i (- x 5))
           (print-roman-I-char level stream)))
        (t
         (dotimes (i x)
           (print-roman-I-char level stream)))))


(defun print-roman-I-char (level stream)
  (write-char (char "IXCM" level) stream))


(defun print-roman-V-char (level stream)
  (write-char (char "VLD" level) stream))


(defun format-P (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 0 directive-args format-string-iterator)
  (when colonp (back-up-one-format-arg format-args))
  (let* ((next-arg (next-format-arg format-args))
         (pluralp  (not (eql next-arg 1))))
    (write-string (cond ((and atsignp pluralp) "ies")
                        (atsignp               "y")
                        (pluralp               "s")
                        (t                     ""))
                  stream)))


(defun format-C (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (maximum-prefix-arguments 0 directive-args format-string-iterator)
  (let ((next-arg (next-format-arg format-args)))
    (check-type next-arg character)
    (cond ((and colonp atsignp)
           (print-redundantly-verbose-character next-arg stream))
          (colonp
           (print-verbose-character next-arg stream))
          (atsignp
           (prin1 next-arg stream))
          (t
           (princ next-arg stream)))))


(defun format-F (format-args directive-args colonp atsignp stream
                 format-string-iterator)
  (prefix-parameter-syntax #\F
                           '(:number :number :number :char :char)
                           directive-args
                           format-string-iterator)
  (let ((string-stream (make-string-output-stream))
        (number        (next-format-arg format-args)))
    (unless (numberp number)
      (format-error format-string-iterator
                    "The argument to ~~F, ~S, is not a number."
                    number))
    (cond ((minusp number)
           (write-char #\- string-stream)
           (setq number (- number)))
          (atsignp
           (write-char #\+ string-stream)))
    (multiple-value-bind (integer decimal)
        (floor number)
      (print-raw-fixnum integer string-stream)
      (write-char #\. string-stream)





(defun format-% (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (declare (ignore format-args))
  (maximum-prefix-arguments 1 directive-args format-string-iterator)
  (forbid-colon colonp format-string-iterator)
  (forbid-atsign atsignp format-string-iterator)
  (let ((number-of-times (if directive-args (first directive-args) 1)))
    (dotimes (i number-of-times)
      (terpri stream))))


(defun format-& (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (declare (ignore format-args))
  (maximum-prefix-arguments 1 directive-args format-string-iterator)
  (forbid-colon colonp format-string-iterator)
  (forbid-atsign atsignp format-string-iterator)
  (let ((number-of-times (if directive-args (first directive-args) 1)))
    (when (> number-of-times 0)
      (fresh-line stream))
    (dotimes (i (1- number-of-times))
      (terpri stream))))


(defun format-\| (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (declare (ignore format-args))
  (maximum-prefix-arguments 1 directive-args format-string-iterator)
  (forbid-colon colonp format-string-iterator)
  (forbid-atsign atsignp format-string-iterator)
  (let ((number-of-times (if directive-args (first directive-args) 1)))
    (dotimes (i number-of-times)
      (write-char #\Page stream))))


(defun format-~ (format-args directive-args colonp atsignp stream
                format-string-iterator)
  (declare (ignore format-args))
  (maximum-prefix-arguments 1 directive-args format-string-iterator)
  (forbid-colon colonp format-string-iterator)
  (forbid-atsign atsignp format-string-iterator)
  (let ((number-of-times (if directive-args (first directive-args) 1)))
    (dotimes (i number-of-times)
      (write-char #\~ stream))))


(defun format-NEWLINE (format-args directive-args colonp atsignp stream
                       format-string-iterator)
  (declare (ignore format-args))
  (maximum-prefix-arguments 0 directive-args format-string-iterator)
  (forbid-both colonp atsignp format-string-iterator)
  (when atsignp
    (terpri stream))
  (unless colonp
    (loop
      (let ((next-character (peek-character format-string-iterator)))
        (if (eq (syntax-type next-character) :WHITESPACE)
            (next-character format-string-iterator)
            (return))))))


(defun format-[ (format-args directive-args colonp atsignp stream
                 format-string-iterator)
  (forbid-both colonp atsignp format-string-iterator)
  (note-most-recent-open-index format-string-iterator)
  (cond (colonp  (format-\:[ format-args directive-args
                             stream format-string-iterator))
        (atsignp (format-@[ format-args directive-args
                            stream format-string-iterator))
        (t       (format-n[ format-args directive-args
                            stream format-string-iterator))))


(defun format-n[ (format-args directive-args stream format-string-iterator)
  (maximum-prefix-arguments 1 directive-args format-string-iterator)
  (let ((n (if directive-args
               (first directive-args)
               (next-format-arg format-args))))
    (multiple-value-bind (strings directive-strings)
        (parse-innards format-string-iterator #\])
      (let* ((number-of-options (length strings))
             (else-clause
               (and (> number-of-options 1)
                    (multiple-value-bind (ignore ignore colonp)
                        (parse-after-twiddle
                          (make-string-iterator
                            (nth (- number-of-options 2) directive-strings))
                          nil
                          t)
                      colonp)
                    (nth (- number-of-options 1) strings))))
        (cond
          ((< -1 n number-of-options)
           (format-main (make-string-iterator (nth n strings))
                        format-args
                        stream))
          (else-clause
           (format-main (make-string-iterator else-clause)
                        format-args
                        stream))
          (t
           NIL))))))


(defun format-@[ (format-args directive-args stream format-string-iterator)
  (prefix-parameter-syntax "~@[" '() directive-args format-string-iterator)
  (multiple-value-bind (strings ignore)
      (parse-innards format-string-iterator #\])
    (if (= (length strings) 1)
        (when (next-format-arg format-args)
          (back-up-one-format-arg format-args)
          (format-main (make-string-iterator (first strings))
                       format-args
                       stream))
        (format-error
          format-string-iterator
          "~D argument~:P to ~~@[ directive; there must be exactly 1."
          (length strings)))))


(defun format-\:[ (format-args directive-args stream format-string-iterator)
  (maximum-prefix-arguments 0 directive-args format-string-iterator)
  (multiple-value-bind (strings ignore)
      (parse-innards format-string-iterator #\])
    (if (= (length strings) 2)
        (format-main
          (make-string-iterator (if (next-format-arg format-args)
                                    (second strings)
                                    (first strings)))
          format-args
          stream)
        (format-error
          format-string-iterator
          "~D argument~:P to ~~:[ directive; there must be exactly 2."
          (length strings)))))


;(defun format-{ (format-args directive-args colonp atsignp stream
;                format-string-iterator)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;----------------------------------------
;;; Format error
;;;----------------------------------------


(defun format-error (format-string-iterator error-string &rest error-args)
  (error "~?~%~V@T~%~3@T\"~A\"~%"
         error-string
         error-args
         (+ (get-most-recent-twiddle-index format-string-iterator) 4)
         (get-entire-format-string format-string-iterator)))
