;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-

;;; documentation for stream operations.  If stream operations are changed or
;;; added, this file should be updated.

(DEFOPERATION (STREAM :DIRECTION) ()
  "Returns :INPUT if the stream is an input stream, :OUTPUT if the stream is an
output stream, :BIDIRECTIONAL if the stream supports both input and output
operations.")

(DEFOPERATION (STREAM :CLOSE) (&OPTIONAL MODE)
  "The stream is closed.  No further operations should be performed on it.  If
the stream is a file stream, the file is closed on the host.  If MODE is
:ABORT, an abnormal exit is signalled; a file open for writing that is closed
with :ABORT will be deleted.")

(DEFOPERATION (STREAM :CHARACTERS) ()
  "Returns non-NIL if the stream is a character stream, NIL otherwise.")

(DEFOPERATION (STREAM :WHICH-OPERATIONS) ()
  "Returns a list of operations directly, or /"natively/", supported by the
stream.  An operation is directly supported by the stream if the stream has its
own code to handle the operation; i.e., if it does not use the stream default
handler's method.")

(DEFOPERATION (INPUT-STREAM :TYI) (&OPTIONAL EOF)
  "Reads one character from the stream and returns it.  If EOF is absent or
NIL, NIL is returned when end-of-file is encountered.  If EOF is present and
non-NIL, an error is signalled, with EOF as the error message.")

(DEFOPERATION (INPUT-STREAM :UNTYI) (CHAR)
  "Places CHAR in the stream so that it will be the next character read.  CHAR
must be the same as the last character read from the stream, and only one
:UNTYI may be done between :TYIs.")

(DEFOPERATION (INPUT-STREAM :LISTEN) ()
  "Returns non-NIL if there is an input character available.  Returns NIL if
there are no characters immediately available.  If the stream is not an
interactive stream, :LISTEN always returns non-NIL, except at end-of-file,
where it returns NIL.")

(DEFOPERATION (INPUT-STREAM :TYIPEEK) (&OPTIONAL EOF)
  "Returns the next character in the input stream, as :TYI does, but without
removing it from the input stream.  If EOF is NIL or not present, returns NIL
at end-of-file; otherwise, :TYIPEEK signals an error, with EOF as the error
message.  Thus it returns what the next :TYI would return with the same EOF
argument, but leaves the character in the stream so the next :TYI will get
it.")

(DEFOPERATION (INPUT-STREAM :TYI-NO-HANG) (&OPTIONAL EOF)
  "Returns the next character in the input stream, exactly as does :TYI, but if
there is no character immediately available, returns NIL instead.  If EOF is
NIL or not present, returns NIL at end-of-file; otherwise, :TYI-NO-HANG signals
an error, with EOF as the error message.")

(DEFOPERATION (INPUT-STREAM :CLEAR-INPUT) ()
  "Any as yet unread input which has been buffered for the stream is discarded.")

(DEFOPERATION (INPUT-STREAM :READ-UNTIL-EOF) ()
  "Reads and discards input from the stream until an end-of-file is encountered.")

(DEFOPERATION (INPUT-STREAM :STRING-IN) (EOF STRING &OPTIONAL (START 0) END)
  "Reads a characters from the stream and stores them in the string.
START and END specify the portion of STRING to be used.
They default 0 and NIL; NIL as END means use up to the end of STRING.
If EOF is non-nil, signals an error at end-if-file, with EOF as the error message.

First value is the index of the first element of STRING not stored in.
Second value is T if end-of-file was encountered, NIL otherwise.")

(DEFOPERATION (INPUT-STREAM :STRING-LINE-IN) (EOF STRING &OPTIONAL (START 0) END)
  "Reads a line of characters from the stream and stores them in the string.
START and END specify the portion of STRING to be used.
They default 0 and NIL; NIL as END means use up to the end of STRING.
Storing stops on reaching END (or the end of STRING), on reaching
EOF, or on finding a Return in the data from the stream.
The Return, if any, is not stored in STRING.
If EOF is non-nil, signals an error at end-if-file, with EOF as the error message.

First value is the index of the first element of STRING not stored in.
Second value is T if end-of-file was encountered, NIL otherwise.
Third value is T if we did NOT encounter a Return character
 (that is, there is more text coming in the same line).")

(DEFOPERATION (OUTPUT-STREAM :FRESH-LINE) ()
  "Output a Return if not already at the beginning of a line.
If the stream is at the beginning of a fresh line, :FRESH-LINE does nothing;
otherwise, a carriage return is output.
The value is T if output was done.")

(DEFOPERATION (OUTPUT-STREAM :STRING-OUT) (STRING &OPTIONAL (START 0) END)
  "The characters in STRING are output to the stream.  If START is present, it
is the 0-based index of the first character to output; otherwise the first
character in the string is the first character to output.  If END is present,
the index of the last character to be output is END-1; if END is not present,
the last character output is the last character in the stream.")

(DEFOPERATION (OUTPUT-STREAM :CLEAR-OUTPUT) ()
  "Any output which has been buffered by the stream is discarded.")

(DEFOPERATION (OUTPUT-STREAM :FORCE-OUTPUT) ()
  "Any buffered output is sent to the device immediately.  This is for use with
streams that save output and send it in groups of characters.  It may be
necessary to use :FORCE-OUTPUT on a buffered stream to cause output operations
to take effect immediately.")

(DEFOPERATION (OUTPUT-STREAM :FINISH) ()
  "If an I/O operation is in progress, :FINISH waits until it has been
completed, and then returns.  This is mainly used with streams that output to
asynchronous devices such as the Chaosnet.")

(DEFOPERATION (OUTPUT-STREAM :EOF) ()
  "Causes an end-of-file to be output.  For streams that do not handle this
operation natively, the default stream handler sends a :FINISH to the stream.")

(DEFOPERATION (INPUT-STREAM :LINE-IN) (&OPTIONAL LEADER)
  "Reads one line of input from the stream, and returns it, sans the Return.
If end-of-file is encountered, returns a second value of T.
If LEADER is absent or NIL, the string returned will be reused by the next
:LINE-IN operation; if LEADER is T, the string returned is a copy, and will not
be reused.  If LEADER is a fixnum, a copy is made with an array leader of
length LEADER.")

(DEFOPERATION (OUTPUT-STREAM :LINE-OUT) (LINE &OPTIONAL (START 0) END)
  "Outputs the characters in LINE to the stream, followed by a carriage return.
If START is present, it is the 0-based index of the character to start with; if
it is absent, starts at the beginning of the string.  If END is present, the
0-based index of the last character to output is END-1; if END is absent, the
last character output is the last character in the string.")
