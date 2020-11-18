;;; -*- Mode:LISP; Package:SI; Readtable:CL; Base:8; Cold-Load:T -*-

;;; stuff which was in READ and QIO

;;;; top levels of lisp reader

(DEFUN CL:READ (&OPTIONAL (STREAM *STANDARD-INPUT*)
                (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P
                PRESERVE-WHITESPACE)
  "Read an s-expression from STREAM and return it.
End of file within an s-expression is an error.
End of file with no s-expression seen is controlled by EOF-ERRORP.
T means it is an error then too.  NIL means that end of file
with no s-expression returns EOF-VALUE.

RECURSIVE-P non-NIL is used for recursive calls, e.g. from read macro definitions.
Recursive calls must be distinguished to make READ-PRESERVING-WHITESPACE
and #n= \"labels\" work properly.

PRESERVE-WHITESPACE if non-NIL says do not discard the terminating delimiter even
if it is whitespace.  This argument is ignored if RECURSIVE-P is non-NIL,
and the outer, nonrecursive call gets to control the matter."
  (WITH-INPUT-EDITING (STREAM '((:ACTIVATION CHAR= #\END)))
    (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE RECURSIVE-P PRESERVE-WHITESPACE NIL NIL)))

(DEFUN READ-PRESERVING-WHITESPACE (&OPTIONAL (STREAM *STANDARD-INPUT*)
                                   EOF-ERROR-P EOF-VALUE RECURSIVE-P)
  "Similar to INTERNAL-READ, but never discards the character that terminates an object.
If the object read required the following character to be seen to terminate it,
normally READ will discard that character if it is whitespace.
This function, by contrast, will never discard that character."
  (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE RECURSIVE-P T))

(DEFUN READ-FOR-TOP-LEVEL (&REST READ-ARGS)
  "Similar to ZL:READ, but ignores stray closeparens and ignores end of file.
Interactive command loops such as the lisp listener use this function."
  (DECLARE (ARGLIST STREAM EOF-OPTION (RUBOUT-HANDLER-OPTIONS '((:ACTIVATION CHAR= #\END)))))
  (MULTIPLE-VALUE-BIND (STREAM NIL EOF-VALUE OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (WITH-INPUT-EDITING (STREAM (IF OPTIONS (CAR OPTIONS) '((:ACTIVATION CHAR= #\END))))
      (INTERNAL-READ STREAM NIL EOF-VALUE NIL NIL T))))

(DEFUN ZL:READ (&REST READ-ARGS)
  "Read an s-expression from a stream and return it.
EOF-OPTION, if supplied is returned if end of file is encountered.
If there is no EOF-OPTION, end of file is an error.
See the documentation for WITH-INPUT-EDITING for the format of RUBOUT-HANDLER-OPTIONS.
READ gets an error if an extraneous closeparen or dot is found.
Command loops should use READ-FOR-TOP-LEVEL instead, to discard them."
  (DECLARE (ARGLIST STREAM EOF-OPTION (RUBOUT-HANDLER-OPTIONS '((:ACTIVATION CHAR= #\END)))))
  (MULTIPLE-VALUE-BIND (STREAM EOF-ERROR-P EOF-VALUE OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (WITH-INPUT-EDITING (STREAM (IF OPTIONS (CAR OPTIONS) '((:ACTIVATION CHAR= #\END))))
      (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE NIL NIL))))

(DEFUN READ-CHECK-INDENTATION (&REST READ-ARGS)
  "Read an s-expression from a stream and return it, requiring proper indentation.
We assume that all open parens in column zero are supposed
to be top-level lists, except that EVAL-WHEN's, etc, may surround them.
\(Symbols such as EVAL-WHEN should have a SI::MAY-SURROUND-DEFUN property).
If an open paren is encountered in column 0 and is not top level, sufficient
closeparens are \"imagined\" so as to close off enough pending lists to make
the data valid.  End of file closes off all pending lists.
In either case, the SYS:MISSING-CLOSEPAREN condition is signaled,
with an argument that is T for end of file, NIL for open paren encountered."
  (DECLARE (ARGLIST STREAM EOF-OPTION (RUBOUT-HANDLER-OPTIONS '((:ACTIVATION CHAR= #\END)))))
  (MULTIPLE-VALUE-BIND (STREAM EOF-ERROR-P EOF-VALUE OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (WITH-INPUT-EDITING (STREAM  (IF OPTIONS (CAR OPTIONS) '((:ACTIVATION CHAR= #\END))))
      (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE NIL NIL NIL T))))

(DEFUN READ-OR-END (&REST READ-ARGS &AUX CH TEM)
  "Like ZL:READ, except that if the first non-blank character that the user types
is END (the interactive input activation character), we immediately return two values:
NIL and :END.  Otherwise, read and return an s-expression from STREAM.
EOF-OPTION has the same meaning as it does for ZL:READ."
  (DECLARE (ARGLIST STREAM EOF-OPTION RUBOUT-HANDLER-OPTIONS))
  (MULTIPLE-VALUE-BIND (STREAM EOF-ERROR-P EOF-VALUE RH-OPTIONS)
      (DECODE-KLUDGEY-MUCKLISP-READ-ARGS READ-ARGS 1)
    (WITH-LIST* (RH-OPTIONS '(:ACTIVATION CHAR= #\END) RH-OPTIONS)
      (IF (ASSQ :ACTIVATION (CDR RH-OPTIONS)) (POP RH-OPTIONS))
      (WITH-INPUT-EDITING (STREAM RH-OPTIONS)
        (SETQ TEM (IF (OPERATION-HANDLED-P STREAM :ANY-TYI) :ANY-TYI :TYI))
        ;; can't use PEEK-CHAR as that wouldn't get blips...
        (DO-FOREVER
          (SETQ CH (SEND STREAM TEM EOF-ERROR-P))
          (COND ((EQ (CAR-SAFE CH) ':ACTIVATION) (RETURN (VALUES NIL :END)))
                ;; should use the same readtable-based check that peek-char uses, but wtf
                ((MEMQ CH '(#.(CHAR-INT #\SPACE)
                            #.(CHAR-INT #\TAB)
                            #.(CHAR-INT #\NEWLINE))))
                (T
                 (SEND STREAM :UNTYI CH)
                 (RETURN (INTERNAL-READ STREAM EOF-ERROR-P EOF-VALUE NIL NIL T)))))))))


;;;; line readers

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p
                                                      options)
  "Read a line from STREAM and return it as a string.
The string does not include the final Newline character, and is empty if nothing was read.
The second value is T if the line was terminated by EOF.
EOF-ERROR-P says whether an error should be signalled if eof occurs at the start of
 the line. If it is NIL and eof occurs at the start of the line, we return EOF-VALUE and T
RECURSIVE-P is ignored.
If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to WITH-INPUT-EDITING if it is used."
  (declare (values line eof-flag))
  (declare (ignore recursive-p))
  (multiple-value-bind (string eof-flag delimiter)
      (read-delimited-string '(#.(char-int #\Newline) #.(char-int #\End))
                             stream
                             eof-error-p
                             options)
    (cond ((and eof-flag (zerop (length string)))
           (values eof-value t))
          (t
           (when (and (instancep stream)
                      (operation-handled-p stream :rubout-handler))
             (send stream :tyo delimiter))
           (values string eof-flag)))))

(defun readline (&rest read-args)
  "Read a line from STREAM and return it as a string.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to WITH-INPUT-EDITING if it is used.

The second value is EOF-OPTION if we exit due to end of file.

The third value is the delimiter which ended the input, or NIL if
it ended due to EOF."
  (declare (arglist &optional stream eof-option options)
           (values string-or-eof-option eof-flag delimiter))
  (multiple-value-bind (stream eof-error-p eof-value options)
      (decode-kludgey-mucklisp-read-args read-args 1)
    (multiple-value-bind (string eof delimiter)
        (read-delimited-string '(#.(char-int #\Newline) #.(char-int #\End)) stream
                               eof-error-p (car options))
        (unless eof
          (when (operation-handled-p stream :rubout-handler)
            (send stream :tyo delimiter)))
        (values (if (and eof (zerop (length string))) eof-value string)
              (if eof eof-value)
              delimiter))))

(defun readline-trim (&rest read-args)
  "Read a line from STREAM and return it as a string, sans leading and trailing whitespace.
The string does not include a Return character, and is empty for a blank line.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to WITH-INPUT-EDITING if it is used.

The second value is T if we exit due to end of file."
  (declare (arglist &optional stream eof-option options)
           (values string eof))
  (multiple-value-bind (string eof)
      (apply #'readline read-args)
    (values
      (if eof string (string-trim '(#\Space #\Tab) string))
      eof)))

(defun readline-or-nil (&rest read-args)
  "Read a line from STREAM and return it as a string, or return NIL if line is empty.
The string does not include a Return character.
If EOF-OPTION is non-NIL, it is returned on end of file at beginning of line;
 otherwise, end of file with no text first is an error.
End of file after reading some text is never an error.

If the stream supports the :RUBOUT-HANDLER operation, we use it.
OPTIONS is a list of rubout handler options, passed to WITH-INPUT-EDITING if it is used.

The second value is T if we exit due to end of file."
  (declare (arglist &optional stream eof-option options)
           (values string-or-nil eof))
  (multiple-value-bind (string eof)
      (apply #'readline read-args)
    (values
      (cond  (eof string)
             (t (setq string (string-trim '(#\Space #\Tab) string))
                (if (equal string "") nil string)))
      eof)))

(defun read-delimited-string (&optional (delimiter #\End) (stream *standard-input*)
                              eof-error-p rh-options (buffer-size 100.))
  "Reads input from STREAM until DELIMITER is found; returns a string.
Uses the rubout handler if STREAM supports that.
DELIMITER is either a character or a list of characters.
 (Characters may be fixnums or character objects).
Values are:
 The string of characters read, not including the delimiter
 T if input ended due to end of file
 The delimiter character read (as a fixnum), or NIL if ended at EOF.
EOF-ERROR-P if non-NIL means get error on end of file before any input is got.
RH-OPTIONS are passed to WITH-INPUT-EDITING.
BUFFER-SIZE is the size to make the buffer string, initially."
  (declare (values string eof-flag delimiter))
  (setq stream (decode-read-arg stream))
  (typecase delimiter
    (character (setq delimiter (char-int delimiter)))
    (cons (loop for x in delimiter
                when (characterp delimiter)
              return (setq delimiter (mapcar (lambda (x)
                                               (if (characterp x) (char-int x) x))
                                             delimiter)))))
  (with-list (activation :activation
                               (if (consp delimiter) 'memq 'eq)
                               delimiter)
    (with-list* (options activation rh-options)
      (with-input-editing (stream options)
        (do ((buffer (make-array buffer-size :ELEMENT-type 'STRING-char :fill-pointer 0)))
            (())
          (let ((ch (send stream (if rubout-handler :any-tyi :tyi)
                          (and (zerop (length buffer)) eof-error-p))))
            (cond ((null ch)
                   (return (values buffer t)))
                  ((consp ch)
                   (when (eq (car ch) :activation)
;                    (send stream :tyo (cadr ch))
                     (return (values buffer nil (cadr ch)))))
                  ((and (not rubout-handler)
                        (if (consp delimiter) (memq ch delimiter) (eq ch delimiter)))
                   (return (values buffer nil ch)))
                  (t
                   (vector-push-extend ch buffer)))))))))

;;;; gross old-fashioned prompt-and-read (soon to be revised, I hope)

(defvar prompt-and-read-format-string :unbound
  "Within PROMPT-AND-READ, holds the FORMAT-STRING argument.")

(defvar prompt-and-read-format-args :unbound
  "Within PROMPT-AND-READ, holds the FORMAT-ARGS arguments.")

(defun prompt-and-read (option format-string &rest format-args)
  "Read an object from *QUERY-IO* according to OPTION,
     prompting using FORMAT-STRING and -ARGS.
OPTION says how to read the object and what its syntax is.  It can be:
 :READ -- use READ to read the object.
 :EVAL-READ -- read an s-expression and evaluate it.  Return the value.
 :EVAL-READ-OR-END -- Like :EVAL-READ, but user can also type just End,
   in which case we return NIL as first value and :END as second.
 (:EVAL-READ :DEFAULT <DEFAULT>) -- Like :EVAL-READ, but user can
   also type just Space to use the default.  Second value is :DEFAULT then.
 (:EVAL-READ-OR-END :DEFAULT <DEFAULT>) -- Analogous.
 :NUMBER -- read a number, terminated by Return or End.
 (:NUMBER :INPUT-RADIX <RADIX> :OR-NIL <BOOLEAN>) -- read using <RADIX> for IBASE,
   and if <BOOLEAN> is non-NIL it allows you to type just Return and returns NIL.
 :CHARACTER -- read one character and return it as a fixnum.
 :DATE -- read a date and return in universal time format.
 (:DATE :PAST-P <PAST-P> :NEVER-P <NEVER-P>) -- read a date.
   The value is in universal time format.
   If <NEVER-P> is non-NIL, \"never\" is accepted, meaning return NIL.
   If <PAST-P> is non-NIL, the date is required to be before the present.
 :STRING -- read a string, terminated by Return.
 :STRING-TRIM -- read a string, terminated by Return.
   Discard leading and trailing whitespace.
 :STRING-OR-NIL -- read a string, terminated by Return.
   Discard leading and trailing whitespace.  If string is empty, return NIL.
 :PATHNAME -- read a pathname and default it.
 (:PATHNAME :DEFAULTS <DEFAULTS-LIST> :VERSION <VERSION-DEFAULT>) --
   read a pathname and default it using the defaults list specified.
   <VERSION-DEFAULT> is passed as the fourth arg to FS:MERGE-PATHNAME-DEFAULTS.
 :PATHNAME-OR-NIL -- like :PATHNAME but if user types just End then NIL is returned.
 (:DELIMITED-STRING :DELIMITER <DELIM> :BUFFER-SIZE <SIZE>) --
   read a string terminated by <DELIM>, which should be a character or a list of them.
   <SIZE> specifies the size of string to allocate initially.
 :DELIMITED-STRING-OR-NIL -- like :DELIMITED-STRING but if user types
   an empty string then NIL is returned.
 (:FQUERY . FQUERY-OPTIONS) -- calls FQUERY with the options."
  (cond ((operation-handled-p *query-io* :prompt-and-read)
         (lexpr-send *query-io* :prompt-and-read
                     option format-string format-args))
        (t
         (let* ((option-type (if (consp option) (car option) option))
                (function (get option-type 'prompt-and-read-function))
                (prompt-and-read-format-string format-string)
                (prompt-and-read-format-args format-args))
           (cond ((get option-type 'prompt-and-read-no-rubout-function)
                  (funcall (get option-type 'prompt-and-read-no-rubout-function)
                           option *query-io*))
                 ((null function)
                  (ferror "~S is not a known ~S option keyword." option-type 'prompt-and-read))
                 ((operation-handled-p *query-io* :rubout-handler)
                  (send *query-io* :rubout-handler
                        (get option-type 'prompt-and-read-rubout-options
                             '((:prompt prompt-and-read-prompt-function)
                               (:activation memq (#.(char-int #\End) (char-int #\Newline)))))
                        function option *query-io*))
                 (t
                  (funcall function option *query-io*)))))))

(defun prompt-and-read-prompt-function (stream ignore)
  (apply #'format stream prompt-and-read-format-string prompt-and-read-format-args))

(defconst eval-read-prinlevel 2)
(defconst eval-read-prinlength 4)

(defprop :eval-read eval-read-prompt-and-read prompt-and-read-no-rubout-function)
(defprop :eval-sexp eval-read-prompt-and-read prompt-and-read-no-rubout-function)
(defprop :eval-form eval-read-prompt-and-read prompt-and-read-no-rubout-function)
(defun eval-read-prompt-and-read (option stream)
  (do (value form flag)
      (())
    (error-restart (error "Try again to type this input.")
      (multiple-value (form flag)
        (with-input-editing (stream
                              '((:prompt prompt-and-read-prompt-function)
                                (:activation char= #\End)))
          (let ((ch (send stream :tyi)))
            (cond ((and (consp option) (get-location-or-nil option :default)
                        (eq ch (char-int #\Space)))
                   (values (get option :default) :default))
                  (t (send stream :untyi ch)
                     (values (cl:read stream)))))))
      (if flag (return (values form flag))
        (setq value (eval-abort-trivial-errors form))))
    ;; If FORM was not trivial, ask for confirmation of the value it returned.
    (when (or (self-evaluating-p form)
              (let ((*print-level* eval-read-prinlevel)
                    (*print-length* eval-read-prinlength))
                (fquery '(:list-choices nil) "The object is ~S, ok? " value)))
      (return value))
    (terpri stream)))

(deff trivial-form-p 'self-evaluating-p)

(defprop :eval-read-or-end eval-read-or-end-prompt-and-read
         prompt-and-read-no-rubout-function)
(defprop :eval-sexp-or-end eval-read-or-end-prompt-and-read
         prompt-and-read-no-rubout-function)
(defprop :eval-form-or-end eval-read-or-end-prompt-and-read
         prompt-and-read-no-rubout-function)
(defun eval-read-or-end-prompt-and-read (option stream)
  (do (value form flag)
      (())
    (error-restart (error "Try again to type this input.")
      (multiple-value (form flag)
        (with-input-editing (stream
                              '((:prompt prompt-and-read-prompt-function)
                                (:activation eq #.(char-int #\End))))
          (let ((ch (send stream :any-tyi)))
            (cond ((and (consp option) (get-location-or-nil option :default)
                        (eq ch (char-int #\Space)))
                   (values (get option :default) :default))
                  ((eq (car-safe ch) :activation)
                   (send stream :tyo (cadr ch))
                   (values nil :end))
;character lossage
                  ((eq ch (char-int #\End))
                   (values nil :end))
                  (t (unless (consp ch) (send stream :untyi ch))
                     (values (cl:read stream)))))))
      (if flag (return (values form flag))
        (setq value (eval-abort-trivial-errors form))))
    ;; If FORM was not trivial, ask for confirmation of the value it returned.
    (when (or (self-evaluating-p form)
              (let ((*print-level* eval-read-prinlevel)
                    (*print-length* eval-read-prinlength))
                (fquery '(:list-choices nil) "The object is ~S, ok? " value)))
      (return value))
    (terpri stream)))

(defprop :read read-prompt-and-read prompt-and-read-function)
(defprop :expression read-prompt-and-read prompt-and-read-function)
(defun read-prompt-and-read (ignore stream)
  (values (cl:read stream)))

(defprop :read ((:prompt prompt-and-read-prompt-function)
                (:activation eq #.(char-int #\End)))
         prompt-and-read-rubout-options)

(defprop :expression ((:prompt prompt-and-read-prompt-function)
                      (:activation eq #.(char-int #\End)))
         prompt-and-read-rubout-options)

(defprop :expression-or-end expression-or-end-prompt-and-read prompt-and-read-function)
(defprop :expression-or-end ((:prompt prompt-and-read-prompt-function)
                             (:activation eq #.(char-int #\End)))
         prompt-and-read-rubout-options)
(defun expression-or-end-prompt-and-read (ignore stream)
  (let ((ch (send stream :any-tyi)))
    (cond ((or (eq (car-safe ch) :activation))
           (and (not rubout-handler) (eq ch (char-int #\End)))
           (progn
             (if (consp ch)
                 (send stream :tyo (cadr ch)))
             (values nil :end)))
          (t
           (when (atom ch) (send stream :untyi ch))
           (values (cl:read stream))))))

(defun (:property :character prompt-and-read-no-rubout-function) (option stream)
  (block char
    (prompt-and-read-prompt-function stream nil)
    (let ((char (send stream :tyi))
          (*standard-output* stream))
      (when (and (consp option) (get option :or-nil))
        (cond ((memq char '(#.(char-int #\quote) #.(char-int #\c-q)))
               (setq char (send stream :tyi)))
              ((eq char (char-int #\Clear-input))
               (princ "none")
               (return-from char nil))))
      (format:ochar char :editor)
      char)))

(defun (:property :character-list prompt-and-read-function) (ignore stream)
  (concatenate 'list (readline stream)))

(defun (:property :number prompt-and-read-function) (option stream)
  (let ((*read-base* (or (and (consp option) (get option :input-radix)) *read-base*))
        (string (readline-trim stream)))
    (if (and (consp option) (get option :or-nil)
             (equal string ""))
        nil
      (condition-case ()
          (let* ((number (cl:read-from-string string)))
            (if (numberp number) number
              (ferror 'read-error-1 "That is not a number.")))
        (end-of-file (ferror 'read-error-1 "That is not a number."))))))

(defun (:property :integer prompt-and-read-function) (option stream)
  (let ((*read-base* (or (and (consp option) (get option :input-radix)) *read-base*))
        (string (readline-trim stream)))
    (if (and (consp option) (get option :or-nil)
             (equal string ""))
        nil
      (condition-case ()
          (let* ((number (cl:read-from-string string)))
            (if (integerp number) number
              (ferror 'read-error-1 "That is not an integer.")))
        (end-of-file (ferror 'read-error-1 "That is not an integer."))))))

(defun (:property :small-fraction prompt-and-read-function) (option stream)
  (let ((string (readline-trim stream)))
    (if (and (consp option) (get option :or-nil)
             (equal string ""))
        nil
      (condition-case ()
          (let* ((number (cl:read-from-string string)))
            (if (and (numberp number) (realp number) ( 0.0 number 1.0))
                (float number)
              (ferror 'read-error-1 "That is not a fraction between 0 and 1.")))
        (end-of-file (ferror 'read-error-1 "That is not a fraction between 0 and 1."))))))

(defun (:property :date prompt-and-read-function) (option stream)
  (let ((string (readline-trim stream)))
    (if (equalp string "never")
        (if (and (consp option)
                 (get option :never-p))
            nil
          (ferror 'read-error-1 "Never is not allowed here."))
      (let* ((past-p (and (consp option) (get option :past-p)))
             (date (condition-case (error)
                       (time:parse-universal-time string 0 nil (not past-p))
                     (time:parse-error
                      (ferror 'read-error-1 "~A" (send error :report-string))))))
        (and past-p (> date (get-universal-time))
             (ferror 'read-error-1 "~A is not in the past."
                     (time:print-universal-time date nil)))
        date))))

(defun (:property :string-or-nil prompt-and-read-function) (ignore stream)
  (readline-or-nil stream))

(defun (:property :string prompt-and-read-function) (ignore stream)
  (readline stream))

(defun (:property :string-trim prompt-and-read-function) (ignore stream)
  (readline-trim stream))

(defun (:property :string-list prompt-and-read-function) (ignore stream)
  (let ((str1 (readline stream))
        j accum)
    (do ((i 0))
        (())
      (setq j (string-search-char #\, str1 i))
      (let ((str2 (string-trim " " (substring str1 i j))))
        (unless (equal str2 "")
          (push str2 accum)))
      (unless j (return (nreverse accum)))
      (setq i (1+ j)))))

(defun (:property :pathname prompt-and-read-function) (option stream)
  (let ((defaults (if (consp option) (get option :defaults) zl:*default-pathname-defaults*))
        (string (readline stream)))
    (fs:merge-pathname-defaults string defaults
                                fs:*name-specified-default-type*
                                (or (and (consp option) (get option :default-version))
                                    :newest))))

(defun (:property :pathname-or-end prompt-and-read-function) (option stream)
  (let ((defaults (if (consp option) (get option :defaults) zl:*default-pathname-defaults*)))
    (multiple-value-bind (string nil terminator)
        (readline stream)
      (if (and (equal string "") (eq terminator (char-int #\End)))
          :end
        (fs:merge-pathname-defaults string defaults
                                    fs:*name-specified-default-type*
                                    (or (and (consp option) (get option :default-version))
                                        :newest))))))

(defun (:property :pathname-or-nil prompt-and-read-function) (option stream)
  (let ((defaults (if (consp option) (get option :defaults) zl:*default-pathname-defaults*)))
    (multiple-value-bind (string nil terminator)
        (readline stream)
      (if (and (equal string "") (eq terminator (char-int #\End)))
          nil
        (fs:merge-pathname-defaults string defaults
                                    fs:*name-specified-default-type*
                                    (or (and (consp option) (get option :default-version))
                                        :newest))))))

(defun (:property :fquery prompt-and-read-no-rubout-function) (option *query-io*)
  (apply #'fquery (if (consp option) (cdr option))
                 prompt-and-read-format-string prompt-and-read-format-args))

(defun (:property :delimited-string prompt-and-read-no-rubout-function) (option stream)
  (read-delimited-string (or (and (consp option) (get option :delimiter)) #\End)
                         stream nil '((:prompt prompt-and-read-prompt-function))
                         (or (and (consp option) (get option :buffer-size)) #o100)))

(defun (:property :delimited-string-or-nil prompt-and-read-no-rubout-function) (option stream)
  (let ((string
          (read-delimited-string (or (and (consp option) (get option :delimiter)) #\End)
                                 stream nil '((:prompt prompt-and-read-prompt-function))
                                 (or (and (consp option) (get option :buffer-size)) #o100))))
    (if (equal string "") nil string)))

(defun (:property :choose prompt-and-read-no-rubout-function) (option *query-io*)
  (let ((choices (get option :choices)))
    (with-input-editing (*query-io*
                          `((:prompt ,#'(lambda (&rest args)
                                          (apply 'prompt-and-read-prompt-function args)
                                          (fresh-line query-io)
                                          (do ((choices choices (cdr choices))
                                               (i 0 (1+ i)))
                                              ((null choices))
                                            (format *query-io* "~& Type ~D for ~S"
                                                    i (car choices)))
                                          (terpri *query-io*)))
                            (:activation memq (#.(char-int #\End) #.(char-int #\Return)))))
      (nth (cl:read *query-io*)
           choices))))

(defun (:property :assoc prompt-and-read-no-rubout-function) (option *query-io*)
  (let ((choices (get option :choices)))
    (with-input-editing (*query-io*
                          `((:prompt ,#'(lambda (&rest args)
                                          (apply #'prompt-and-read-prompt-function args)
                                          (fresh-line *query-io*)
                                          (do ((choices choices (cdr choices))
                                               (i 0 (1+ i)))
                                              ((null choices))
                                            (format *query-io* "~& Type ~D for ~S"
                                                    i (caar choices)))
                                          (terpri *query-io*)))
                            (:activation memq (#.(char-int #\End) #.(char-int #\Return)))))
      (cdr (nth (cl:read *query-io*)
                choices)))))

(defun (:property :boolean prompt-and-read-no-rubout-function) (ignore *query-io*)
  (apply #'y-or-n-p prompt-and-read-format-string prompt-and-read-format-args))
