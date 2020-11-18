;;; -*- Mode:LISP; Package:LISP-IO; Base:10; Readtable:CL -*-
;;;
;;; STREAMS.LISP
;;;
;;; Still to do:
;;;   with-open-stream, with-input-from-string, with-output-to-string
;;;   (the latter need fooling around with declarations)
;;;   Make-string-output-stream sorely needs optimization.


;;;----------------------------------------------------------------------------
;;; STREAM REPRESENTATION
;;;----------------------------------------------------------------------------
;;;
;;; The representation of a stream is an object which behaves a lot like a
;;; flavor instance.  It accepts messages to perform operations; these messages
;;; may cause side effects, return values, or both.  A message may be sent to a
;;; stream by either the INVOKE-INPUT-OPERATION or the INVOKE-OUTPUT-OPERATION
;;; function.
;;;
;;; A stream structure has slots to accomodate the operations common to all
;;; streams.  If an operation is not defined for a particular stream, then it
;;; has nil in the corresponding slot.  Other operations are kept on plists in
;;; the other-xx-operations
;;; slots.  If a stream doesn't recognize an operation it is asked to perform,
;;; it calls its unrecognized-xx-operation-proc.  If this procedure isn't
;;; defined, and the operation is not on the *non-crashing-operations* list,
;;; then it causes an error.
;;;
;;; Default input operations:
;;;   :INPUT-STREAM-P, no arguments, in the INPUT-STREAM-PRED slot.  Returns
;;;     true if the stream is an input stream, and nil if it isn't.
;;;   :INPUT-TYPE, no arguments, in the INPUT-ELEMENT-TYPE-PROC slot.
;;;     Returns the type of element that can be input from the stream.
;;;   :END-OF-STREAM-P, no arguments, in the END-OF-STREAM-PRED slot.
;;;     Returns true if the stream is exhausted, nil otherwise.
;;;   :LISTEN, no arguments, in the LISTEN-PRED slot.  Returns true if an
;;;     object is immediately available on the stream; nil otherwise.
;;;   :READ-CHAR, no arguments, in the READ-CHAR-PROC slot.  Reads a character
;;;     from the stream.
;;;   :UNREAD-CHAR, one argument, in the UNREAD-CHAR-PROC slot.  Replaces a
;;;     character on the stream.  Must be the last character read.
;;;   :READ-LINE, no arguments, in the READ-LINE-PROC slot.  Reads a line from
;;;     the stream.  Returns 2 values; the second is non-nil if the line was
;;;     terminated by the end of the stream, nil if the line was terminated by
;;;     a newline.
;;;   :READ-BYTE, no arguments, in the READ-BYTE-PROC slot.  Reads a byte from
;;;     the stream.
;;;
;;; Default output operations:
;;;   :OUTPUT-STREAM-P, no arguments, in the OUTPUT-STREAM-PRED slot.  Returns
;;;     true if the stream is an output stream, and nil if it isn't.
;;;   :OUTPUT-TYPE, no arguments, in the OUTPUT-ELEMENT-TYPE-PROC slot.
;;;     Returns the type of element that can be output to the stream.
;;;   :START-OF-LINE-P, no arguments, in the START-OF-LINE-PRED slot.  Returns
;;;     true if the stream knows it is at the start of a line; nil otherwise.
;;;   :WRITE-BYTE, one argument, in the WRITE-BYTE-PROC slot.  Writes a byte to
;;;     the stream.
;;;   :WRITE-CHAR, one argument, in the WRITE-CHAR-PROC slot.  Writes a
;;;     character to the stream.
;;;   :WRITE-STRING, one argument, in the WRITE-STRING-PROC slot.  Writes a
;;;     string to the stream.
;;;----------------------------------------------------------------------------

(defstruct (stream
             (:constructor make-stream)
             (:predicate   streamp))
  (name "Unnamed")

  input-stream-pred
  input-element-type-proc
  end-of-stream-pred
  listen-pred
  read-char-proc
  unread-char-proc
  read-line-proc
  read-byte-proc
  other-input-operations
  unrecognized-input-operation-proc

  output-stream-pred
  output-element-type-proc
  start-of-line-pred
  write-char-proc
  write-string-proc
  write-byte-proc
  other-output-operations
  unrecognized-output-operation-proc)


(defun define-input-operation (stream operation-name operation)
  (setf (getf (stream-other-input-operations stream) operation-name)
        operation))

(defun define-output-operation (stream operation-name operation)
  (setf (getf (stream-other-output-operations stream) operation-name)
        operation))

(defun get-input-operation (stream operation-name)
  (getf (stream-other-input-operations stream) operation-name))

(defun get-output-operation (stream operation-name)
  (getf (stream-other-output-operations stream) operation-name))


(defvar *non-crashing-operations*
        '(:FINISH-OUTPUT :FORCE-OUTPUT :CLEAR-OUTPUT :CLEANUP-ABORT-INPUT
          :CLEANUP-ABORT-OUTPUT :CLEAR-INPUT))

;;; The invoke functions should be made into defsubsts.
(defun invoke-input-operation (stream operation-name &rest arguments)
  (let ((operation
          (case operation-name
            (:INPUT-STREAM-P  (stream-input-stream-pred stream))
            (:INPUT-TYPE      (stream-input-element-type-proc stream))
            (:END-OF-STREAM-P (stream-end-of-stream-pred stream))
            (:LISTEN          (stream-listen-pred stream))
            (:READ-CHAR       (stream-read-char-proc stream))
            (:UNREAD-CHAR     (stream-unread-char-proc stream))
            (:READ-LINE       (stream-read-line-proc stream))
            (:READ-BYTE       (stream-read-byte-proc stream))
            (T                (getf (stream-other-input-operations stream)
                                    operation-name)))))
    (cond
      (operation
       (apply operation arguments))
      ((stream-unrecognized-input-operation-proc stream)
       (apply (stream-unrecognized-input-operation-proc stream)
              operation-name
              arguments))
      ((member operation *non-crashing-operations*)
       NIL)
      (T
       (error "~S is not a valid input operation on the stream ~S."
              operation-name
              stream)))))

(defun invoke-output-operation (stream operation-name &rest arguments)
  (let ((operation
          (case operation-name
            (:OUTPUT-STREAM-P (stream-output-stream-pred stream))
            (:OUTPUT-TYPE     (stream-output-element-type-proc stream))
            (:START-OF-LINE-P (stream-start-of-line-pred stream))
            (:WRITE-BYTE      (stream-write-byte-proc stream))
            (:WRITE-CHAR      (stream-write-char-proc stream))
            (:WRITE-STRING    (stream-write-string-proc stream))
            (T                (getf (stream-other-output-operations stream)
                                    operation-name)))))
    (cond
      (operation
       (apply operation arguments))
      ((stream-unrecognized-output-operation-proc stream)
       (apply (stream-unrecognized-output-operation-proc stream)
              operation-name
              arguments))
      ((member operation *non-crashing-operations*)
       NIL)
      (T
       (error "~S is not a valid output operation on the stream ~S."
              operation-name
              stream)))))


;;;----------------------------------------------------------------------------
;;; STANDARD STREAMS
;;;----------------------------------------------------------------------------
;;;
;;; This section defines the *standard-input*, *terminal-io*, etc. streams.
;;;----------------------------------------------------------------------------


(defmacro kludge-output-stream (stream-name)
  `(PROGN
     (DEFVAR ,(intern (symbol-name stream-name)))
     (SETQ   ,(intern (symbol-name stream-name))
             (MAKE-STREAM
               :INPUT-STREAM-PRED        #'(LAMBDA () NIL)
               :OUTPUT-STREAM-PRED       #'(LAMBDA () T)
               :OUTPUT-ELEMENT-TYPE-PROC #'(LAMBDA () 'CHARACTER)
               :START-OF-LINE-PRED       #'(LAMBDA () NIL)
               :WRITE-CHAR-PROC          #'(LAMBDA (CHAR)
                                             (LISP:WRITE-CHAR
                                               CHAR
                                               ,(find-symbol
                                                  (symbol-name stream-name)
                                                  (find-package 'LISP))))
               :WRITE-STRING-PROC        #'(LAMBDA (STRING)
                                             (LISP:WRITE-STRING
                                               STRING
                                               ,(find-symbol
                                                  (symbol-name stream-name)
                                                  (find-package 'LISP))))
               ))))

(defmacro kludge-input-stream (stream-name)
  `(PROGN
     (DEFVAR ,(intern (symbol-name stream-name)))
     (SETQ   ,(intern (symbol-name stream-name))
             (MAKE-STREAM
               :INPUT-STREAM-PRED        #'(LAMBDA () T)
               :OUTPUT-STREAM-PRED       #'(LAMBDA () NIL)
               :INPUT-ELEMENT-TYPE-PROC  #'(LAMBDA () 'CHARACTER)
               :START-OF-LINE-PRED       #'(LAMBDA () NIL)
               :END-OF-STREAM-PRED       #'(LAMBDA () NIL)
               :LISTEN-PRED              #'(LAMBDA ()
                                             (LISP:LISTEN
                                               ,(find-symbol
                                                  (symbol-name stream-name)
                                                  (find-package 'LISP))))
               :READ-CHAR-PROC           #'(LAMBDA ()
                                             (LISP:READ-CHAR
                                               ,(find-symbol
                                                  (symbol-name stream-name)
                                                  (find-package 'LISP))))
               :READ-LINE-PROC           #'(LAMBDA ()
                                             (LISP:READ-LINE
                                               ,(find-symbol
                                                  (symbol-name stream-name)
                                                  (find-package 'LISP))))
               :UNREAD-CHAR-PROC         #'(LAMBDA (CHAR)
                                             (LISP:UNREAD-CHAR
                                               char
                                               ,(find-symbol
                                                  (symbol-name stream-name)
                                                  (find-package 'LISP))))
               ))))

(eval-when (compile load eval)
  (kludge-output-stream *standard-output*)
  (kludge-input-stream *standard-input*)
  (kludge-output-stream *error-output*)
  (kludge-output-stream *trace-output*))


;;;----------------------------------------------------------------------------
;;; CREATING NEW STREAMS (CLtL, section 21.2)
;;;----------------------------------------------------------------------------

(defun make-synonym-stream (symbol)
  "Create and return a new stream.  Any operations on the new stream will be
performed on the stream that is then the dynamic value of SYMBOL."
  (make-stream
    :unrecognized-input-operation-proc
      #'(lambda (operation &rest args)
          (apply #'invoke-input-operation
                 (symbol-value symbol)
                 operation
                 args))
    :unrecognized-output-operation-proc
      #'(lambda (operation &rest args)
          (apply #'invoke-output-operation
                 (symbol-value symbol)
                 operation
                 args))))

(defun make-broadcast-stream (&rest streams)
  "Create and return an output stream which sends its output to each of
STREAMS."
  (setq streams (copy-list streams))
  (make-stream :input-stream-pred
                 #'(lambda () NIL)
               :output-stream-pred
                 #'(lambda () T)
               :unrecognized-output-operation-proc
                 #'(lambda (operation &rest args)
                     (dolist (stream streams)
                       (apply #'invoke-output-operation
                              stream
                              operation
                              args)))))

(defun make-concatenated-stream (&rest streams)
  "Create and return a new stream which takes its input from each of the
STREAMS.  Input is taken from the first of the STREAMS until it is exhausted,
then from the next stream until it is exhausted, and so on.  The new stream is
exhausted when the last of STREAMS has been exhausted."
  (setq streams (copy-list streams))
  (make-stream :input-stream-pred
                 #'(lambda () T)
               :output-stream-pred
                 #'(lambda () NIL)
               :end-of-stream-pred
                 #'(lambda () (null streams))
               :unrecognized-input-operation-proc
                 #'(lambda (operation &rest args)
                     (loop
                       (cond ((null streams)
                              (return 'end-of-stream))
                             ((invoke-input-operation (car streams)
                                                      :END-OF-STREAM-P)
                              (setq streams (cdr streams)))
                             (t
                              (return (apply #'invoke-input-operation
                                             (car streams)
                                             operation
                                             args))))))))

(defun make-two-way-stream (input-stream output-stream)
  "Create and return a bidirectional stream which gets its input from
INPUT-STREAM and sends its output to OUTPUT-STREAM."
  (make-stream
    :unrecognized-input-operation-proc
      #'(lambda (operation &rest args)
          (apply #'invoke-input-operation
                 input-stream
                 operation
                 args))
    :unrecognized-output-operation-proc
      #'(lambda (operation &rest args)
          (apply #'invoke-output-operation
                 output-stream
                 operation
                 args))))

(defun make-echo-stream (input-stream output-stream)
  "Create and return a bidirectional stream which gets its input from
INPUT-STREAM and sends its output to OUTPUT-STREAM.  Also, INPUT-STREAM input
is echoed to OUTPUT-STREAM."
  (make-stream
    :read-char-proc
      #'(lambda ()
          (let ((char (invoke-input-operation input-stream :READ-CHAR)))
            (write-char char output-stream)
            char))
    :read-line-proc
      #'(lambda ()
          (let ((string (invoke-input-operation input-stream :READ-LINE)))
            (write-line string output-stream)
            string))
    :read-byte-proc
      #'(lambda ()
          (let ((byte (invoke-input-operation input-stream :READ-BYTE)))
            (write-byte byte output-stream)
            byte))
    :unrecognized-input-operation-proc
      #'(lambda (operation &rest args)
          (apply #'invoke-input-operation
                 input-stream
                 operation
                 args))
    :unrecognized-output-operation-proc
      #'(lambda (operation &rest args)
          (apply #'invoke-output-operation
                 output-stream
                 operation
                 args))))

(defun make-string-input-stream (string &optional start end)
  (setq string (subseq string (if start start 0) end))
  (let ((index 0))
    (let ((stream
            (make-stream
              :input-stream-pred
                #'(lambda () T)
              :output-stream-pred
                #'(lambda () NIL)
              :input-element-type-proc
                #'(lambda () 'string-char)
              :end-of-stream-pred
                #'(lambda () (= index (length string)))
              :listen-pred
                #'(lambda () (< index (length string)))
              :read-char-proc
                #'(lambda () (prog1 (char string index) (incf index)))
              :unread-char-proc
                #'(lambda (char) char (decf index))
              :read-line-proc
                #'(lambda ()
                    (let ((newline-position
                            (position #\Newline string :start index)))
                      (multiple-value-prog1
                        (values (subseq string index newline-position)
                                (not newline-position))
                        (setq index (if newline-position
                                        (1+ newline-position)
                                        (length string)))))))))
      (define-input-operation stream :RELATIVE-INDEX #'(lambda () index))
      stream)))

(defun make-string-output-stream ()
  "Return an output stream which accumulates characters.  The characters can be
glommed from this stream with the function GET-OUTPUT-STREAM-STRING."
  (let* ((characters (make-array 0 :adjustable T :fill-pointer T))
         (stream
           (make-stream
             :input-stream-pred
               #'(lambda () NIL)
             :output-stream-pred
               #'(lambda () T)
             :output-element-type-proc
               #'(lambda () 'string-char)
             :write-char-proc
               #'(lambda (char)
                   (vector-push-extend char characters))
             :write-string-proc
               #'(lambda (string)
                   (dotimes (i (length string))
                     (vector-push-extend (char string i)
                                         characters))))))
    (define-output-operation stream :GET-CHARACTERS
      #'(lambda ()
          (prog1
            (coerce characters 'string)
            (setf characters (make-array 0 :adjustable T :fill-pointer T)))))
    stream))

(defun get-output-stream-string (string-output-stream)
  "Return a string containing all the characters written to STRING-OUTPUT-
STREAM so far.  Then \"clear\" the stream, so that subsequent calls to this
function don't get the same characters again."
  (invoke-output-operation string-output-stream :GET-CHARACTERS))

(defmacro with-open-stream ())

(defmacro with-input-from-string ())

(defmacro with-output-to-string ())


;;;----------------------------------------------------------------------------
;;; OPERATIONS ON STREAMS (CLtL, section 21.3)
;;;----------------------------------------------------------------------------

(defun input-stream-p (stream)
  "True if STREAM, which must be a stream, can handle input operations.  False
otherwise."
  (invoke-input-operation stream :INPUT-STREAM-P))

(defun output-stream-p (stream)
  "True if STREAM, which must be a stream, can handle output operations.  False
otherwise."
  (invoke-output-operation stream :OUTPUT-STREAM-P))

(defun stream-element-type (stream)
  "Return a type specifier indicating what kind of objects may be read from or
written to STREAM."
  (cond
    ((and (input-stream-p stream) (output-stream-p stream))
     (let ((input-type  (invoke-input-operation stream :INPUT-TYPE))
           (output-type (invoke-output-operation stream :OUTPUT-TYPE)))
       (cond ((subtypep input-type output-type)
              input-type)
             ((subtypep output-type input-type)
              output-type)
             (t
              (error "The input and output element type of ~S are ~
                      inconsistent."
                     stream)))))
    ((input-stream-p stream)
     (invoke-input-operation stream :INPUT-TYPE))
    ((output-stream-p stream)
     (invoke-output-operation stream :OUTPUT-TYPE))
    (t
     (error "No stream element type for a null stream."))))

(defun close (stream &key abort)
  "Close STREAM.  Further input and output operations will cause an error to
be raised.  If ABORT is not nil, it indicates an abnormal closing of the stream.
Attempts are made to clean up."
  (when abort
    (when (input-stream-p stream)
      (invoke-input-operation stream :CLEANUP-ABORT-INPUT))
    (when (output-stream-p stream)
      (invoke-output-operation stream :CLEANUP-ABORT-OUTPUT)))
  (let ((error #'(lambda (&rest args)
                   args
                   (error "Stream ~S is closed." stream))))
    (setf (stream-read-char-proc stream) error)
    (setf (stream-read-line-proc stream) error)
    (setf (stream-read-byte-proc stream) error)
    (setf (stream-write-char-proc stream) error)
    (setf (stream-write-string-proc stream) error)
    (setf (stream-write-byte-proc stream) error)))




;;;----------------------------------------------------------------------------
;;; OUTPUT TO CHARACTER STREAMS  (CLtL, section 22.3.1)
;;;----------------------------------------------------------------------------

(defun write-char (character &optional (output-stream *standard-output*))
  "Output CHARACTER to OUTPUT-STREAM and return CHARACTER."
  (invoke-output-operation output-stream :WRITE-CHAR character)
  character)

(defun write-string (string &optional (output-stream *standard-output*)
                     &key start end)
  "Write the substring of STRING given by START and END to OUTPUT-STREAM.
Return STRING."
  (invoke-output-operation
    output-stream
    :WRITE-STRING (subseq string (if start start 0) end))
  string)

(defun write-line (string &optional (output-stream *standard-output*)
                   &key start end)
  "Write the substring of STRING given by START and END to OUTPUT-STREAM.
The write a newline to OUTPUT-STREAM.  Return STRING."
  (invoke-output-operation
    output-stream
    :WRITE-STRING (subseq string (if start start 0) end))
  (invoke-output-operation output-stream :WRITE-CHAR #\Newline)
  string)

(defun terpri (&optional (output-stream *standard-output*))
  "Write a newline to OUTPUT-STREAM."
  (invoke-output-operation output-stream :WRITE-CHAR #\Newline)
  nil)

(defun fresh-line (&optional (output-stream *standard-output*))
  "Write a newline to OUTPUT-STREAM, unless the stream is already at the start
of a new line.  Return T if a newline was written and NIL if one wasn't."
  (if (invoke-output-operation output-stream :START-OF-LINE-P)
      NIL
      (progn (invoke-output-operation output-stream :WRITE-CHAR #\Newline)
             T)))

(defun finish-output (&optional (output-stream *standard-output*))
  (invoke-output-operation output-stream :FINISH-OUTPUT)
  nil)

(defun force-output (&optional (output-stream *standard-output*))
  (invoke-output-operation output-stream :FORCE-OUTPUT)
  nil)

(defun clear-output (&optional (output-stream *standard-output*))
  (invoke-output-operation output-stream :CLEAR-OUTPUT)
  nil)


;;;----------------------------------------------------------------------------
;;; OUTPUT TO BINARY STREAMS (CLtL, section 22.3.2)
;;;----------------------------------------------------------------------------

(defun write-byte (integer output-stream)
  "Write INTEGER, one byte, to OUTPUT-STREAM."
  (cond
    ((typep integer (invoke-output-operation output-stream :OUTPUT-TYPE))
     (invoke-output-operation output-stream :WRITE-BYTE integer))
    (t
     ;; I'm not sure that this error check belongs here.
     (error "~S is not the right type for output stream ~S."
            integer output-stream))))


;;;----------------------------------------------------------------------------
;;; INPUT FROM STREAMS (CLtL, sections 22.2.1 and 22.2.2)
;;;----------------------------------------------------------------------------

(defun read-line (&optional (input-stream *standard-input*)
                            (eof-error-p T)
                            eof-value
                            recursive-p)
  "Read in a line of text, terminated by a newline.  Return two values.  The
first is a string containing the line read in, without the newline character.
The second is false if the line of text was terminated by a newline; true if
the line of text was terminated by the end of INPUT-STREAM."
  (declare (ignore recursive-p))
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (if (invoke-input-operation input-stream :END-OF-STREAM-P)
      (if eof-error-p
          (error "Unexpected end of stream in READ-LINE.")
          eof-value)
      (invoke-input-operation input-stream :READ-LINE)))

(defun read-char (&optional (input-stream *standard-input*)
                            (eof-error-p T)
                            eof-value
                            recursive-p)
  "Read one character from INPUT-STREAM and return it."
  (declare (ignore recursive-p))
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (if (invoke-input-operation input-stream :END-OF-STREAM-P)
      (if eof-error-p
          (error "Unexpected end of stream in READ-CHAR.")
          eof-value)
      (invoke-input-operation input-stream :READ-CHAR)))

(defun unread-char (character &optional (input-stream *standard-input*))
  "Put CHARACTER back onto the front of INPUT-STREAM.  CHARACTER must be the
last character read from INPUT-STREAM."
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (invoke-input-operation input-stream :UNREAD-CHAR character))

(defun peek-char (&optional peek-type
                            (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  "If PEEK-TYPE is NIL (the default), return the next character available on
INPUT-STREAM without removing it from INPUT-STREAM.  If PEEK-TYPE is T, read
characters from INPUT-STREAM until a non-whitespace character is found.  Leave
the non-whitespace character on INPUT-STREAM.  Return the non-whitespace
character.  If PEEK-TYPE is a character, read characters from INPUT-STREAM
until that character is found.  Leave that character on INPUT-STREAM and return
it."
  (declare (ignore recursive-p))
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (labels ((find-char (found-predicate)
             (if (invoke-input-operation input-stream :END-OF-STREAM-P)
                 (if eof-error-p
                     (error "Unexpected end of file in PEEK-CHAR.")
                     eof-value)
                 (let ((char (invoke-input-operation input-stream :READ-CHAR)))
                   (if (funcall found-predicate char)
                       (progn
                         (invoke-input-operation input-stream :UNREAD-CHAR char)
                         char)
                       (find-char found-predicate))))))
    (cond
      ((eq peek-type NIL)
       (find-char #'(lambda (ignore) T)))
      ((eq peek-type T)
       (find-char #'(lambda (char) (not (eq (syntax-type char) :WHITESPACE)))))
      ((characterp peek-type)
       (find-char #'(lambda (char) (char= char peek-type))))
      (t
       (error "Invalid peek type ~S for PEEK-CHAR." peek-type)))))

(defun listen (&optional (input-stream *standard-input*))
  "Return true if a character is immediately available to be read from
INPUT-STREAM.  Return false if there will be a wait before a character can be
read, or if INPUT-STREAM is already at its end."
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (invoke-input-operation input-stream :LISTEN))

(defun read-char-no-hang (&optional (input-stream *standard-input*)
                          (eof-error-p t)
                          eof-value
                          recursive-p)
  "If a character is immediately available to be read from INPUT-STREAM, read
that character and return it.  Return NIL if there will be a wait before a
character can be read.  If INPUT-STREAM is at its end, raise an error if
EOF-ERROR-P is true or return EOF-VALUE if it isn't."
  (declare (ignore recursive-p))
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (cond
    ((invoke-input-operation input-stream :END-OF-STREAM-P)
     (if eof-error-p
         (error "Unexpected end of file in READ-CHAR-NO-HANG.")
         eof-value))
    ((invoke-input-operation input-stream :LISTEN)
     (invoke-input-operation input-stream :READ-CHAR))
    (t
     NIL)))

(defun clear-input (&optional (input-stream *standard-input*))
  "Clear any buffered input associated with INPUT-STREAM and return NIL."
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (invoke-input-operation input-stream :CLEAR-INPUT)
  NIL)

(defun read-from-string (string &optional (eof-error-p t) eof-value
                         &key (start 0) end preserve-whitespace)
  "Read an object from the substring of STRING indicated by START and END.
Return two values:  the object read, and the index of the first character not
read."
  (let* ((stream (make-string-input-stream string start end))
         (thing  (if preserve-whitespace
                     (read-preserving-whitespace
                       stream eof-error-p eof-value nil)
                     (read stream eof-error-p eof-value nil)))
         (index  (+ start (invoke-input-operation stream :RELATIVE-INDEX))))
    (values thing index)))

;;; parse-integer is implemented in READER.LISP

(defun read-byte (binary-input-stream &optional (eof-error-p t) eof-value)
  "Read one byte from BINARY-INPUT-STREAM and return it in the form of an
integer."
  (if (invoke-input-operation binary-input-stream :END-OF-STREAM-P)
      (if eof-error-p
          (error "Unexpected end of file in READ-BYTE.")
          eof-value)
      (invoke-input-operation binary-input-stream :READ-BYTE)))
