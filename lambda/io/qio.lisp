;;; -*- Mode:LISP; Package:SI; Cold-load:T; Base:8; Lowercase:T; Readtable:ZL -*-
;;; LISP machine character level I/O stuff
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

; this file is rife with --- ... !!!
; character lossage

;(defconst stream-input-operations
;         '(:tyi :listen :untyi :line-in :rubout-handler))

;(defconst stream-output-operations
;         '(:tyo :force-output :finish :string-out :line-out :fresh-line :untyo-mark :untyo))

;;; Naming conventions:
;;;   Symbols whose names end in "-INPUT", "-OUTPUT", or "-IO" should
;;;      normally be BOUND to streams; which of the three you use depends on
;;;      what directions the stream normally supports.
;;;   Symbols whose names end in "-STREAM" are DEFINED as streams.


;;; Synonyms.
;;; MAKE-SYNONYM-STREAM takes a symbol, and returns a stream which will forward all operations
;;;   to the binding of the symbol.  After (SETQ BAR (MAKE-SYNONYM-STREAM 'FOO)), one says
;;;   that BAR is SYNned to FOO.


;;; The initial environment.
;;;   The initial binding of streams (set up by LISP-REINITIALIZE) is
;;;      as follows:
;;;   *TERMINAL-IO*     - This is how to get directly to the user's terminal.  It is set
;;;                     up to go to the TV initially.  Other places it might go are to
;;;   *STANDARD-INPUT*  - This is initially bound to SYN to *TERMINAL-IO*.
;;;   *STANDARD-OUTPUT* - This is initially bound to SYN to *TERMINAL-IO*. *STANDARD-INPUT*
;;;                     and *STANDARD-OUTPUT* are the default streams for READ, PRINT and
;;;                     other things.  *STANDARD-OUTPUT* gets hacked when the session is
;;;                     being scripted, for example.
;;;   *ERROR-OUTPUT*    - This is where error messages should eventually get sent. Initially
;;;                     SYNned to *TERMINAL-IO*.
;;;   *QUERY-IO*        - This is for unexpected user queries of the
;;;                     "Do you really want to ..." variety. Initially SYNned to *TERMINAL-IO*
;;;   *TRACE-OUTPUT*    - Output produced by TRACE goes here.
;;;                      Initially SYNned to *ERROR-OUTPUT*.


(defvar *standard-input* :unbound
  "Default stream for input functions such as READ.")
(defvar *terminal-io* :unbound
  "Stream to use for /"terminal/" I//O.  Normally the selected window.
*STANDARD-INPUT* and other default streams are usually set up
as synonym streams which will use the value of *TERMINAL-IO*.")
;;; defvar for *standard-output* is in sys: sys; read
(defvar *error-output* :unbound
  "Stream to use for unanticipated noninteractive output, such as warnings.")
(defvar *query-io* :unbound
  "Stream to use for unanticipated questions, and related prompting, echoing, etc.")

(defvar standard-input :unbound
  "Default stream for input functions such as READ.")
(defvar terminal-io :unbound
  "Stream to use for /"terminal/" I//O.  Normally the selected window.
*STANDARD-INPUT* and other default streams are usually set up
as synonym streams which will use the value of *TERMINAL-IO*.")
(defvar error-output :unbound
  "Stream to use for unanticipated noninteractive output, such as warnings.")
(defvar query-io :unbound
  "Stream to use for unanticipated questions, and related prompting, echoing, etc.")

(forward-value-cell 'terminal-io '*terminal-io*)
(forward-value-cell 'standard-input '*standard-input*)
(forward-value-cell 'error-output '*error-output*)
(forward-value-cell 'query-io '*query-io*)

(defvar-resettable rubout-handler nil nil
  "Bound to stream which is inside rubout-handler, or NIL if none.")

(defun streamp (object)
  "Returns non-NIL if OBJECT is a stream.
This predicate considers the following to be streams:
 Any instance incorporating SI:STREAM or TV:SHEET
 Any function handling either :TYI or :TYO.
 Any symbol with a non-NIL SI:IO-STREAM-P property."
  (or (and (instancep object) (or (typep-structure-or-flavor object 'stream)
                                  (typep object 'tv:sheet)))
      ;; Explicit FUNCALLed things that accept messages
      (and (or (closurep object)
               (entityp object)
               (and (functionp object)
                    (or (nsymbolp object) (fboundp object))))
           (arglist object t)
           (ignore-errors
             (let ((wo (funcall object :which-operations)))
               (or (memq :tyo wo)
                   (memq :tyi wo)))))
      (and (symbolp object) (get object 'io-stream-p))))

(defun input-stream-p (stream)
  "T if STREAM, assumed to be a stream, supports input."
  (memq (send stream :direction) '(:input :bidirectional)))

(defun output-stream-p (stream)
  "T if STREAM, assumed to be a stream, supports output."
  (memq (send stream :direction) '(:output :bidirectional)))

(defun io-stream-p (x)
  "T if X is a plausible I//O stream.
It must be an insance, entity, closure, compiled function
or a symbol which has a non-NIL SI:IO-STREAM-P property."
  (typecase x
    (instance t)
    (entity t)
    (closure t)
    (compiled-function t)
    (select t)
    (symbol (get x 'io-stream-p))
    (t nil)))

(defun stream-element-type (stream)
  "Return a Common Lisp type describing the objects input or output by STREAM.
This will be either CHARACTER or STRING-CHAR or a subtype of INTEGER."
  (or (send stream :send-if-handles :element-type)
      (if (send stream :characters)
          'character
        (let ((value (send stream :send-if-handles :byte-size)))
          (if value `(unsigned-byte ,value) 'fixnum)))))


;;;   eof-option  | eof-error-p | eof-value
;;; --------------+-------------+----------
;;;  (unsupplied) |      t      |     -
;;;     nil       |     nil     |    nil
;;;    other      |     nil     |   other

(defun decode-kludgey-mucklisp-read-args (arglist &optional (number-of-extra-args-allowed 0))
  (declare (values stream eof-error-p eof-value other-args))
  (cond
    ((null arglist)
     (values *standard-input* t))
    ((null (cdr arglist))
     (let ((arg1 (car arglist)))
       (if (or (eq arg1 nil) (eq arg1 t) (io-stream-p arg1))
           ;; The arg is a plausible stream.
           (values (decode-read-arg arg1) t)
         ;; It is not a stream and must be an EOF option.
         (values *standard-input* nil arg1))))
    ((null (cddr arglist))
     (let ((arg1 (car arglist)) (arg2 (cadr arglist)))
       (cond ((or (eq arg1 nil) (eq arg1 t) (io-stream-p arg1))
              (values (decode-read-arg arg1) nil arg2))
             ((or (eq arg2 nil) (eq arg2 t) (io-stream-p arg2))
              (values (decode-read-arg arg2) nil arg1))
             (t (values arg1 nil arg2)))))
    ((not (null (nthcdr (+ number-of-extra-args-allowed 2) arglist)))
     (ferror "Too many arguments were given to one of the READ-like functions: ~S"
             arglist))
    ;; If giving hairy options, we assume she knows not to write stream args in wrong order.
    (t
     (values (decode-read-arg (car arglist)) nil (cadr arglist) (cddr arglist)))))

;;; Given the 2 arguments to READ (or TYI or READCH or TYIPEEK or READLINE)
;;; in the form of a REST argument this returns the input stream and the eof option.
;;; Note that the first arg would rather be the stream than the eof option.
;;; This is set up for Maclisp compatibility.
;;; HOWEVER, if the second argument is NIL or unsupplied, the first is
;;; assumed to be a stream if that is plausible,
;;; which is not compatible with Maclisp but more winning.
;;; If the user didn't supply an eof-option, the second value returned will
;;; be the symbol NO-EOF-OPTION.
(defun decode-read-args (arglist &optional (number-of-extra-args-allowed 0))
  (declare (values stream eof-option other-args))
  (multiple-value-bind (stream eof-error-p eof-value other-args)
      (decode-kludgey-mucklisp-read-args arglist number-of-extra-args-allowed)
    (if eof-error-p
        (values stream 'no-eof-option other-args)
      (values stream eof-value other-args))))
(compiler:make-obsolete decode-read-args "you probably want to be using DECODE-KLUDGEY-MUCKLISP-READ-ARGS instead")

(defun terpri (&optional stream)
  "Go to a new line on STREAM."
  (send (decode-print-arg stream) :tyo (char-int #/Newline))
  nil)

;(defun cli:terpri (&optional stream)
;  "Go to a new line on STREAM."
;  (send (decode-print-arg stream) :tyo (char-int #/Newline))
;  nil)

(defun fresh-line (&optional stream)
  "Go to a new line on STREAM if not already at the beginning of one.
Returns T if a Return was output, NIL if nothing output."
  (send (decode-print-arg stream) :fresh-line))

(defun tyo (char &optional stream)
  "Output CHAR to STREAM."
  (send (decode-print-arg stream) :tyo (if (characterp char) (char-int char) char))
  char)

(defun write-char (char &optional (stream *standard-output*))
  "Output CHAR to STREAM.  Returns CHAR."
  (send (decode-print-arg stream) :tyo (if (characterp char) (char-int char) char))
  char)

(defun write-byte (byte &optional (stream *standard-output*))
  "Output BYTE to STREAM.  Returns BYTE."
  (send (decode-print-arg stream) :tyo byte)
  byte)

(defun write-string (string &optional (stream *standard-output*) &key (start 0) end)
  "Output all or part of STRING to STREAM.
START and END are indices specifying the part.
START defaults to 0 and END to NIL (which means the end of STRING.)"
  (send (decode-print-arg stream) :string-out string start end)
  string)

(defun write-line (string &optional (stream *standard-output*) &key (start 0) end)
  "Output all or part of STRING to STREAM, followed by a Return.
START and END are indices specifying the part.
START defaults to 0 and END to NIL (which means the end of STRING.)"
  (setq stream (decode-print-arg stream))
  (send stream :string-out string start end)
  (send stream :tyo (char-int #/Newline))
  string)

(defun force-output (&optional stream)
  "Force output buffers on STREAM to begin being transmitted immediately.
Useful on asynchronous streams such as the chaosnet, which normally
wait until a buffer is full before even starting to transmit."
  (send (decode-print-arg stream) :force-output)
  nil)

(defun finish-output (&optional stream)
  "Wait until output buffers on STREAM are transmitted and processed completely.
For a file stream, this will not return until the data is recorded permanently
in the file system."
  (send (decode-print-arg stream) :finish)
  nil)

(defun clear-output (&optional stream)
  "Discard buffer output buffers on STREAM, if it is an interactive stream.
The discarded output will never appear where it was going.
For noninteractive streams, this usually does nothing."
  (send (decode-print-arg stream) :clear-output)
  nil)

;;; Common Lisp low level input functions

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  "Read one character from STREAM, and return it as a character object.
If EOF-ERROR-P is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  recursive-p
  (let ((value (send (decode-read-arg stream) :tyi eof-error-p)))
    (if (null value)
        eof-value
      (int-char value))))

(defun unread-char (char &optional (stream *standard-input*))
  "Put CHAR back in STREAM to be read out again as the next input character.
CHAR must be the same character last read from STREAM,
or this may not work or might even signal an error."
  (send (decode-read-arg stream) :untyi (if (characterp char) (char-int char) char))
  nil)

(defun read-byte (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  "Read one byte from STREAM, and return it.
If EOF-ERROR-P is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE."
  (or (send (decode-read-arg stream) :tyi eof-error-p)
      eof-value))

(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value
                  recursive-p)
  "Peek ahead at input from STREAM without discarding it.
The character peeked at is returned as a character object.
If PEEK-TYPE is NIL, peek at the next input character on STREAM,
 but leave it in the input stream so the next input will reread it.
If PEEK-TYPE is T, discard all whitespace chars and peek at first non-whitespace.
 The current readtable says what is whitespace.
Otherwise, discard all chars before the first one that is equal to PEEK-TYPE,
 which should be a number or a character.
EOF-ERROR-P and EOF-VALUE are as for READ-CHAR."
  recursive-p
  (setq stream (decode-read-arg stream))
  (cond ((null peek-type)
         (let ((value (send stream :tyi)))
           (if (null value)
               (if eof-error-p
                   (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
                 eof-value)
             (send stream :untyi value)
             (int-char value))))
        (t
         (do ((whitespace-code (cdr (getf (rdtbl-plist *readtable*) 'whitespace))))
             (())
           (let ((value (send stream :tyi)))
             (if (null value)
                 (if eof-error-p
                     (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
                   (return eof-value))
               (when (cond ((eq peek-type t)
                            (or ( (char-code value) value)
                                ( (rdtbl-code *readtable* value)
                                   whitespace-code)))
                           (t
                            (eq peek-type (int-char value))))
                 (send stream :untyi value)
                 (return (int-char value)))))))))

(defun listen (&optional (stream *standard-input*))
  "T if input is available on STREAM.
On a noninteractive stream, this is T if not at EOF."
  (send (decode-read-arg stream) :listen))

(defun clear-input (&optional (stream *standard-input*))
  "Discard any buffered input on STREAM, if it is an interactive stream."
  (send (decode-read-arg stream) :clear-input)
  nil)

(defun read-char-no-hang (&optional (stream *standard-input*)
                          (eof-error-p t) eof-value recursive-p)
  "Read one character from STREAM, and return it as a character object, but don't wait.
On an interactive stream, if no input is currently buffered, NIL is returned.
If EOF-ERROR-P is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  (declare (ignore recursive-p))
  (condition-case-if (not eof-error-p) ()
      (let ((value (send (decode-read-arg stream) :tyi-no-hang t)))
        (if (null value)
            nil
          (int-char value)))
    (end-of-file
     eof-value)))

;;;; Old-fashioned low level input functions.

;;; This function is compatible with the regular Maclisp TYI.  If you want speed,
;;; FUNCALL the stream directly.  We have to echo, but cannot use the rubout handler
;;; because the user wants to see rubout, form, etc. characters.  Inside the rubout
;;; handler, we do not echo since echoing will have occurred already.
(defun tyi (&rest read-args &aux ch)
  "Read one character from a stream.  Args are a stream and an eof-option.
The order is irrelevant; an arg which is not a reasonable stream
is taken to be the eof-option, which is returned if end of file is reached.
If there is no eof-option, end of file is an error.
If the stream supports rubout handling but we are not inside the rubout handler,
then the character read is echoed."
  (declare (arglist stream eof-option))
  (multiple-value-bind (stream eof-error-p eof-value)
      (decode-kludgey-mucklisp-read-args read-args)
    (cond ((null (setq ch (send stream :tyi)))  ;Get a character, check for EOF
           (if eof-error-p
               (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
             eof-value))
          ((or rubout-handler                   ; If inside rubout handler, or
               (not (memq :rubout-handler (send stream :which-operations))))
           ch)                                  ;  ordinary device, just return char
          (t
           ;; Echo anything but blips and rubout, even control and meta charcters.
           (if (and (fixnump ch)
                    ( ch (char-int #/Rubout)))
               (format stream "~C" ch))
           ch))))

(defun readch (&rest read-args &aux ch (eof '(())))
  "Read one character from a stream, and return a symbol with that pname.
Otherwise the same as TYI.  This is an obsolete Maclisp function."
  (declare (arglist stream eof-option))
  (multiple-value-bind (stream eof-error-p eof-value)
      (decode-kludgey-mucklisp-read-args read-args)
    (if (eq (setq ch (tyi stream eof)) eof)
        (if eof-error-p
            (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
          eof-value)
        (intern (string ch)))))                 ;"Character objects" are in current package.

;;; This function is compatible, more or less, with the regular Maclisp TYIPEEK.
;;; It does not echo, since the echoing will occur when READ or TYI is called.
;;; It does echo characters which it discards.
(defun tyipeek (&optional peek-type &rest read-args)
  "If PEEK-TYPE is NIL, the default, returns the next character to be
read from STREAM, part of READ-ARGS, without removing the character from
the input stream.  If PEEK-TYPE is a fixnum less than 1000 octal, this
reads characters until it gets one equal to PEEK-TYPE.  That character
is not removed from the input stream.  If PEEK-TYPE is T, it skips over
all the input characters until the start of the printed representation
of a Lisp object is reached.  Characters passed over by TYIPEEK are echo
if STREAM is interactive.

This is an obsolete function.  Use the :TYIPEEK message for STREAMs
instead."
  (declare (arglist peek-type stream eof-option))
  (multiple-value-bind (stream eof-error-p eof-value)
      (decode-kludgey-mucklisp-read-args read-args)
    (if (characterp peek-type) (setq peek-type (char-int peek-type)))
    (and (numberp peek-type) ( peek-type #o1000)
         (ferror "The ~S flavor of ~S is not implemented." peek-type 'tyipeek))
    (do ((ch))        ;Pass over characters until termination condition reached
        (())
      (or (setq ch (send stream :tyi))
          (if eof-error-p
              (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
            (return eof-value)))
      (send stream :untyi ch)                   ;Put it back
      (and (cond ((null peek-type))             ;Break on every
                 ((eq ch peek-type))            ;Break on specified character
                 ((eq peek-type t)              ;Break on start-of-object
                  (and (< ch rdtbl-array-size)
                       (zerop (logand (rdtbl-bits *readtable* ch) 1)))))
           (return ch))                         ;Break here
      (tyi stream))))                           ;Echo and eat this character

(defun stream-copy-until-eof (from-stream to-stream &optional (leader-size nil))
  "Copy data from FROM-STREAM to TO-STREAM, until EOF on FROM-STREAM.
The default is to use the most efficient mode, but the third argument
may be used to force use of :LINE-IN//:LINE-OUT mode, especially useful
when the to-stream is an editor interval stream.  If you use this to
copy binary files, note that you had better open the streams with
appropriate host-dependent byte sizes, and that if the from-stream
supports :LINE-IN but not :READ-INPUT-BUFFER you will probably lose."
  (let ((fwo (send from-stream :which-operations))
        (two (send to-stream :which-operations)))
    (cond ((and (not leader-size)
                (memq :read-input-buffer fwo)
                (memq :string-out two))
           ;; If it can go, this mode is the most efficient by far.
           (do ((buf) (offset) (limit))
               (())
             (multiple-value (buf offset limit)
               (send from-stream :read-input-buffer))
             (cond ((null buf) (return nil)))
             (send to-stream :string-out buf offset limit)
             (send from-stream :advance-input-buffer)))
          ((and (memq :line-in fwo)
                (memq :line-out two))
           ;; Not as good, but better than :TYI/:TYO
           (do ((line) (eof))
               (())
             (multiple-value (line eof)
               (send from-stream :line-in leader-size))
             (cond ((not eof)
                    (send to-stream :line-out line))
                   (t (if line (send to-stream :string-out line))
                      (return nil)))))
          ;; This always wins, but is incredibly slow.
          (t (do ((char))
                 ((null (setq char (send from-stream :tyi))))
               (send to-stream :tyo char))))))


(deff make-syn-stream 'make-synonym-stream)
(defun make-synonym-stream (stream-symbol)
  "Return an I//O stream which passes all operations to the value of STREAM-SYMBOL.
This is most often used with STREAM-SYMBOL equal to '*TERMINAL-IO*.
STREAM-SYMBOL can be a locative instead of a symbol."
  (if (symbolp stream-symbol)
      ;; Changed 10/16/83 to make an uninterned symbol
      ;; but record it on STREAM-SYMBOL's plist so only one symbol needs to be made.
      (or (get stream-symbol 'syn-stream)
          (let ((sym (make-symbol (string-append stream-symbol "-SYN-STREAM"))))
            (%p-store-tag-and-pointer (locf (fsymeval sym))
                                      dtp-external-value-cell-pointer
                                      (locf (symeval stream-symbol)))
            (putprop sym t 'io-stream-p)
            (putprop stream-symbol sym 'syn-stream)
            sym))
    (let ((sym (make-symbol "SYN-STREAM")))
      (%p-store-tag-and-pointer (locf (fsymeval sym))
                                dtp-external-value-cell-pointer
                                stream-symbol)
      (putprop sym t 'io-stream-p)
      sym)))

(defun follow-syn-stream (stream)
  "If STREAM is a synonym stream symbol, return the stream it is currently a synonym for.
Otherwise return STREAM."
  (cond ((not (symbolp stream)) stream)
        ((neq (locf (fsymeval stream))
              (follow-cell-forwarding (locf (fsymeval stream)) t))
         (fsymeval stream))
        (t stream)))

(defun follow-all-syn-streams (stream)
  "If STREAM is a synonym stream symbol, return the stream it ultimately points to.
Otherwise return STREAM."
  (loop
    (cond ((not (symbolp stream))
           (return stream))
          ((neq (locf (fsymeval stream))
                (follow-cell-forwarding (locf (fsymeval stream)) t))
           (setq stream (fsymeval stream)))
          (t
           (return stream)))))

(defun make-broadcast-stream (&rest streams)
  "Return an I//O stream which passes all operations to all of the STREAMS.
Thus, output directed to the broadcast stream will go to multiple places."
  (if (null streams) 'null-stream
    (let-closed ((broadcast-stream-streams (copylist streams))
                 (which-operations (loop with wo = (send (car streams) :which-operations)
                                         with copyp = t
                                         for stream in (cdr streams)
                                         do (loop with wo2 = (send stream :which-operations)
                                                  for op in wo
                                                  unless (memq op wo2)
                                                    do (if copyp (setq wo (copylist wo)))
                                                       (setq copyp nil)
                                                       (setq wo (delq op wo)))
                                         finally (return wo))))
      (function (lambda (&rest args)
                  (cond ((eq (car args) :which-operations) which-operations)
                        ((eq (car args) :operation-handled-p)
                         (memq (cadr args) which-operations))
                        ((eq (car args) :send-if-handles)
                         (do ((l broadcast-stream-streams (cdr l)))
                             ((null (cdr l))    ;Last one gets to return multiple values
                              (lexpr-send (car l) :send-if-handles (cdr args)))
                           (lexpr-send (car l) :send-if-handles (cdr args))))
                        (t
                         (do ((l broadcast-stream-streams (cdr l)))
                             ((null (cdr l))    ;Last one gets to return multiple values
                              (apply (car l) args))
                           (apply (car l) args)))))))))

(defun make-concatenated-stream (&rest streams)
  "Return a stream which will read from each of the STREAMS, one by one.
Reading from the concatenated stream will first read data from the first STREAM.
When that reaches eof, it will then read data from the second STREAM,
and so on until all the STREAMS are exhausted.  Then the concatenated stream gets eof."
  (let-closed ((concatenated-stream-streams (copylist streams))
               (which-operations (loop with wo = (send (car streams) :which-operations)
                                       with copyp = t
                                       for stream in (cdr streams)
                                       do (loop with wo2 = (send stream :which-operations)
                                                for op in wo
                                                unless (memq op wo2)
                                                do (if copyp (setq wo (copylist wo)))
                                                (setq copyp nil)
                                                (setq wo (delq op wo)))
                                       finally (return wo)))
               (concatenated-stream-function nil))
    (setq concatenated-stream-function
          #'(lambda (op &rest args)
              (prog ()
                loop
                (return
                  (case op
                    ((:tyi :tyi-no-hang :any-tyi-no-hang :any-tyi)
                     (if (null concatenated-stream-streams)
                         (and (car args)
                              (ferror 'sys:end-of-file-1 "End of file on ~S."
                                      concatenated-stream-function))
                       (let ((value (send (car concatenated-stream-streams) :tyi)))
                         (if value
                             value
                           (pop concatenated-stream-streams)
                           (go loop)))))
                    (:which-operations which-operations)
                    (:send-if-handles
                     (and (memq (car args) which-operations)
                          (lexpr-send concatenated-stream-function args)))
                    (:operation-handled-p
                     (not (null (memq (car args) which-operations))))
                    (:untyi (send (car concatenated-stream-streams) :untyi (car args)))
                    (:direction :input)
                    (t (stream-default-handler concatenated-stream-function
                                               op (car args) (cdr args))))))))))

(defconst two-way-input-operations
          '(:tyi :tyi-no-hang :any-tyi-no-hang :any-tyi :untyi
            :read-char :read-char-no-hang :any-read-char-no-hang :any-read-char :unread-char
            :tyipeek :listen :line-in :string-in
            :get-input-buffer :advance-input-buffer :read-input-buffer
            :read-until-eof :clear-input))

(defun make-two-way-stream (input-stream output-stream)
  "Return a stream which does its input via INPUT-STREAM and its output via OUTPUT-STREAM.
This works by knowing about all the standard, ordinary input operations.
Use of unusual input operations, or operations that affect both input and output
/(such as random access) will not work."
  (let-closed ((two-way-input-stream input-stream)
               (two-way-output-stream output-stream)
               (which-operations
                 (union (intersection two-way-input-operations
                                      (send input-stream :which-operations))
                        (subset-not #'(lambda (elt) (memq elt two-way-input-operations))
                                    (send output-stream :which-operations))
                        '(:send-if-handles :operation-handled-p)))
               (two-way-stream-function nil))
    (setq two-way-stream-function
          #'(lambda (op &rest args)
                (cond ((memq op two-way-input-operations)
                       (lexpr-send two-way-input-stream op args))
                      ((eq op :which-operations) which-operations)
                      ((eq op :send-if-handles)
                       (and (memq (car args) which-operations)
                            (lexpr-send two-way-stream-function args)))
                      ((eq op :operation-handled-p)
                       (not (null (memq (car args) which-operations))))
                      (t
                       (lexpr-send two-way-output-stream op args)))))))

(defun make-echo-stream (input-stream output-stream)
  "Return a stream which does output via OUTPUT-STREAM, and input via INPUT-STREAM with echo.
All characters that this stream reads from INPUT-STREAM are also
echoed to OUTPUT-STREAM.
This works by knowing about all the standard, ordinary input operations.
Use of unusual input operations, or operations that affect both input and output
/(such as random access) will not work."
  (let-closed ((two-way-input-stream input-stream)
               (two-way-output-stream output-stream)
               (echo-stream-unread-char nil)
               (which-operations
                 (union '(:tyi :untyi)
                        (subset-not #'(lambda (elt) (memq elt two-way-input-operations))
                                    (send output-stream :which-operations))
                        '(:send-if-handles :operation-handled-p)))
               (two-way-stream-function nil))
    (setq two-way-stream-function
          #'(lambda (op &rest args)
                (cond ((eq op :tyi)
                       (or (prog1 echo-stream-unread-char
                                  (setq echo-stream-unread-char nil))
                           (let ((value (send two-way-input-stream :tyi (car args))))
                             (if value (send two-way-output-stream :tyo value))
                             value)))
                      ((eq op :untyi)
                       (setq echo-stream-unread-char (car args)))
                      ((memq op two-way-input-operations)
                       (stream-default-handler two-way-stream-function op
                                               (car args) (cdr args)))
                      ((eq op :which-operations) which-operations)
                      ((eq op :send-if-handles)
                       (and (memq (car args) which-operations)
                            (lexpr-send two-way-stream-function args)))
                      ((eq op :operation-handled-p)
                       (not (null (memq (car args) which-operations))))
                      (t
                       (lexpr-send two-way-output-stream op args)))))))

(defun stream-default-handler (fctn op arg1 args &aux tem)
  "Subroutine which provides default definition of certain stream operations.
If a stream does not recognize an operation, it may call this function
to have the operation handled.  The stream should return whatever
this function returns to it.  OP should be the operation, FCTN should
be the stream which received the operation, and ARG1 and ARGS should be
the arguments that came with the operation."
;character lossage in the extreme
  (tagbody
      (return-from stream-default-handler
        (case op
          ((:tyipeek :listen)
           (cond ((setq tem (send fctn :tyi nil))
                  (send fctn :untyi tem)
                  tem)))
          ((:any-tyi :tyi-no-hang)
           (send fctn :tyi arg1))
          (:any-tyi-no-hang
           (send fctn :any-tyi arg1))
          (:read-char
           (if (not (send fctn :characters)) (go lossage))
           (setq tem (if arg1 (send fctn :tyi) (send fctn :tyi (car args))))
           (if (fixnump tem) (int-char tem) tem))
          ((:any-read-char :read-char-no-hang)
           (send fctn :read-char arg1 (car args)))
          (:any-read-char-no-hang
           (send fctn :any-read-char arg1))
          (:read-byte
           (if arg1 (send fctn :tyi) (send fctn :tyi (car args))))
          (:unread-char
           (if (not (send fctn :characters)) (go lossage))
           (send fctn :untyi (if (characterp arg1) (char-int arg1) arg1)))
          (:write-char
           (if (not (send fctn :characters)) (go lossage))
           (send fctn :tyo (if (characterp arg1) (char-int arg1) arg1)))
          (:write-byte
           (send fctn :tyo))
          ((:clear-output :clear-input :force-output :finish :close :eof)
           nil)
          (:fresh-line
           (send fctn :tyo (char-int #/Newline))
           t)
          ((:string-out :line-out)
           (setq tem (string arg1))
           (do ((len (cond ((second args))
                           (t (string-length tem))))
                (i (cond ((first args)) (t 0))
                   (1+ i)))
               (( i len) nil)
             (send fctn :tyo (char-int (char tem i))))
           (and (eq op :line-out)
                (send fctn :tyo (char-int #/Newline))))
          (:line-in
           (let ((buf (make-string #o100 :leader-length (if (numberp arg1) arg1 1))))
             (setf (fill-pointer buf) 0)
             (values buf
                     (do ((tem (send fctn :tyi) (send fctn :tyi)))
                         ((or (null tem)
                              (eq tem (char-int #/Newline))
                              (eq tem (char-int #/End)))
                          (adjust-array-size buf (array-active-length buf))
                          (null tem))
                       (vector-push-extend (int-char tem) buf)))))
          (:string-in
           ;; ARG1 = EOF, (CAR ARGS) = STRING
           (loop with start = (or (cadr args) 0)
                 and end = (or (caddr args) (array-length (car args)))
                 while (< start end)
                 as ch = (send fctn :tyi)
                 while ch
                 do (aset ch (car args) (prog1 start (incf start)))
                 finally (and (array-has-leader-p (car args))
                              (store-array-leader start (car args) 0))
                 (and (null ch) arg1 (ferror 'end-of-file-1 "End of file on ~S." fctn))
                 (return (values start (null ch)))))
          (:string-line-in
           ;; ARG1 = EOF, (CAR ARGS) = STRING
           (loop with start = (or (cadr args) 0)
                 and end = (or (caddr args) (array-length (car args)))
                 while (< start end)
                 as ch = (send fctn :tyi)
                 while (and ch (neq ch (char-int #/Newline)))
                       do (setf (char (car args) (prog1 start (incf start))) (int-char ch))
                 finally (and (array-has-leader-p (car args))
                              (setf (fill-pointer (car args)) start))
                 (and (null ch) arg1 (ferror 'end-of-file-1 "End of file on ~S." fctn))
                 (return (values start (null ch) (neq ch #/Return)))))
          (:operation-handled-p (memq arg1 (send fctn :which-operations)))
          (:characters t)
          (:interactive nil)
          (:element-type
           (if (send fctn :characters) 'character
             (let ((value (send fctn :send-if-handles :byte-size)))
               (if value `(unsigned-byte ,value) 'fixnum))))
          (:direction
           (let ((ops (send fctn :which-operations)))
             (if (memq :tyi ops)
                 (if (memq :tyo ops) :bidirectional :input)
               (if (memq :tyo ops) :output nil))))
          (:send-if-handles
           (if (memq arg1 (send fctn :which-operations))
               (lexpr-send fctn arg1 args)))
          (otherwise
           (go lossage))))
   lossage
      (ferror :unclaimed-message "The stream operation ~S is not supported by ~S"
              op fctn)))

(defmacro selectq-with-which-operations (thing &body clauses)
  "Like CASE, but automatically recognizes :WHICH-OPERATIONS.
:WHICH-OPERATIONS is handled by returning a list of all the
keywords which are tested for in the clauses."
  (let (otherwise)
    (when (memq (caar (last clauses)) '(t otherwise :otherwise))
      (setq otherwise (last clauses)
            clauses (butlast clauses)))
    `(case ,thing
       ,@clauses
       (:which-operations ',(loop for clause in clauses
                                  appending (if (consp (car clause))
                                                (car clause)
                                              (list (car clause)))))
       . ,otherwise)))

(defprop null-stream t io-stream-p)
(defun null-stream (op &rest args &aux tem)
  "An I//O stream which ignores output and gives instant end-of-file on input."
  (selectq-with-which-operations op
    ;; These operations signal EOF.
    ((:tyi :tyi-no-hang :tyipeek :get-input-buffer :read-input-buffer
           :any-tyi :any-tyi-no-hang)
     (and (first args) (ferror 'read-end-of-file "End of file on SI:NULL-STREAM.")))
    ;; Signals EOF differently.
    (:string-in
     (and (first args) (ferror 'read-end-of-file "End of file on SI:NULL-STREAM."))
     (values (third args) t))
    ;; Signals EOF still differently.
    (:line-in
      (setq tem (make-array 0 :type 'art-string
                              :leader-length (and (numberp (first args)) (first args))))
      (and (numberp (first args))
           (plusp (first args))
           (setf (fill-pointer tem) 0))
      (values tem t))
    ;; These operations should all return their argument.
    ((:tyo :string-out :line-out :untyi)
     (first args))
    ((:increment-cursorpos :finish :force-output :clear-output :clear-input :listen)
     nil)
    ;; These operations should always return T.
    ((:characters :beep :fresh-line) t)
    (:read-cursorpos (values 0 0))
    ;; Supports nothing in both directions.
    (:direction :bidirectional)
    ;; Handle obscure operations.
    (otherwise (stream-default-handler 'null-stream op (first args) (rest1 args)))))

(defvar *iolst :unbound "String or list of data to read, in READLIST or READ-FROM-STRING.")

(defvar *ioch :unbound "Character position in *IOLST, in READ-FROM-STRING.")

(defvar *ioend :unbound "Character position to stop at, in READ-FROM-STRING.")

(defun readlist (charlist &aux (*ioch nil) (*iolst charlist))
  "Read an expression from the list of characters CHARLIST."
  (read 'readlist-stream))

(defprop readlist-stream t io-stream-p)
(defun readlist-stream (operation &optional arg1 &rest rest)
  (cond ((or (eq operation :any-tyi)
             (memq operation '(:tyi :tyi-no-hang :any-tyi-no-hang)))
         (cond ((eq *ioch t)
                (ferror "EOF in middle of ~S" 'readlist))
               ((not (null *ioch))
                (prog2 nil *ioch (setq *ioch nil)))
               ((null *iolst)
                (setq *ioch t)
                40)
               (t (prog1 (character (car *iolst))
                         (setq *iolst (cdr *iolst))))))
        ((eq operation :untyi)
         (setq *ioch arg1))
        ((eq operation :which-operations)
         '(:tyi :untyi))
        (t (stream-default-handler 'readlist-stream operation arg1 rest))))

(defun cli:read-from-string (string &optional (eof-error-p t) eof-value
                             &key (start 0) end preserve-whitespace)
  "Read an expression out of the characters in STRING.
START and END are indices which specify a substring of STRING to be used.
 START defaults to 0 and END to NIL (which means the end of STRING).

Reaching the end of STRING or the specified substring constitutes EOF.
EOF-ERROR-P controls whether this signals an error; if not, EOF-VALUE is returned.

PRESERVE-WHITESPACE non-NIL causes the effect of using READ-PRESERVING-WHITESPACE.

Only one object is read.  The first value is that object (or perhaps
EOF-VALUE) and the second value is the index in STRING at which reading stopped."
  (declare (values contents end-char-position))
  (let ((stream (make-string-input-stream  string start end)))
    (values (condition-case-if (not eof-error-p)
                               (error)
                (internal-read stream t nil nil preserve-whitespace)
              (read-end-of-file eof-value))
            (send stream :pointer))))

(defun read-from-string (string &optional (eof-option nil eof-option-p) (start 0) end)
  "Read an expression out of the characters in STRING.
If EOF-OPTION is supplied, it is returned on end of file;
otherwise, end of file is an error.  START (default 0)
is the position in STRING to start reading at; END is where to stop.

The second value is the index in the string at which reading stopped.
It stops after the first object, even if not all the input is used."
  (declare (values contents end-char-position))
  (let ((stream (make-string-input-stream  string start end)))
    (values (condition-case-if eof-option-p
                               (error)
                (internal-read stream)
              (read-end-of-file eof-option))
            (send stream :pointer))))

(defprop read-from-string-stream t io-stream-p)

(defun make-string-input-stream (string &optional (start 0) end)
  "Return a stream from which one can read the characters of STRING, or some substring of it.
START and END are indices specifying a substring of STRING;
they default to 0 and NIL (NIL for END means the end of STRING)."
  (let* ((pointer start)
         (length (string-length string))
         (end (or end length))
         (me nil))
    (setq me #'(lambda (operation &optional arg1 &rest args)
                 (labels ((check-eof (error)
                            (when (or (>= pointer length) (>= pointer end))
                              (if error
                                  (signal 'sys:end-of-file :format-string "End of File on string input stream")
                                t)))
                          )
                   (selectq-with-which-operations operation
                     ((:close))
                     ((:tyi :any-tyi)
                      (unless (check-eof arg1)
                        (prog1 (aref string pointer)
                               (incf pointer))))
                     ((:tyipeek)
                      (unless (check-eof arg1)
                        (aref string pointer)))
                     ((:untyi)
                      (if (and (> pointer start) (eq (int-char arg1) (char string (1- pointer))))
                          (decf pointer)
                        (ferror nil "Attempt to :UNTYI something different than last :TYI'ed.")))
                     ((:pointer :get-string-index)
                      pointer)
                     ((:set-pointer)
                      (setq pointer arg1))
                     (otherwise
                      (stream-default-handler me operation arg1 args))))))))

(defvar *flatsize-max-value-of-interest* nil)

(defun flat-printed-size (function object max-value)
  (let  ((*ioch 0)
         (*flatsize-max-value-of-interest* max-value))
    (or (catch 'flatsize (funcall function object #'flatsize-stream) nil)
        *ioch)))

(defun flatsize (x &optional max-value)
  "Return the number of characters it takes to print X with quoting."
  (flat-printed-size #'prin1 x max-value))

(defun flatc (x &optional max-value)
  "Return the number of characters it takes to print X with no quoting."
  (flat-printed-size #'princ x max-value))

(defprop flatsize-stream t io-stream-p)

(defun flatsize-stream (operation &optional arg1 &rest rest)
  (macrolet ((add-characters (n)
               `(progn
                  (incf *ioch ,n)
                  (when (and *flatsize-max-value-of-interest*
                             (> *ioch *flatsize-max-value-of-interest*))
                    (throw 'flatsize (+ *flatsize-max-value-of-interest* 1))))))
    (case operation
      (:tyo
       (add-characters 1))
      (:string-out
       (add-characters (- (or (second rest) (string-length arg1)) ; end
                          (or (first rest) 0))))
      (:which-operations
       '(:tyo :string-out))
      (otherwise
       (stream-default-handler 'flatsize-stream operation arg1 rest)))))
