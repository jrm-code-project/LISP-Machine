;-*- Mode:LISP; Package:FILE-SYSTEM; Base:10; Readtable:ZL -*-

;;;More robust COPY-FILE function, in progress. --Keith 24-oct-88

(defvar *new-copy-file-debug* nil)

(defmacro ncf (&rest args)
  `(and *new-copy-file-debug*
        (format *trace-output* ,@args)))

(DEFUN NEW-COPY-FILE (PATHNAME-OR-STREAM NEW-NAME
                  &REST OPTIONS
                  &KEY (ERROR T)
                  &ALLOW-OTHER-KEYS)
  "Copy a file, specified as a pathname, string or I//O stream.
CHARACTERS can be T, NIL, meaning the same as in OPEN.
 or it can be :ASK, meaning always ask the user,
 or :MAYBE-ASK meaning ask the user unless the answer is clear,
 or :DEFAULT meaning guess as well as possible but never ask.
Specify BYTE-SIZE to force copying in a certain byte size.
 BYTE-SIZE affects only binary mode copying.
REPORT-STREAM is a stream to output messages to as files are copied.
 If it is NIL, no messages are output.
COPY-CREATION-DATE if NIL means don't copy the file creation date;
 make now be the new file's creation date.
COPY-AUTHOR if NIL means don't copy the author; make you the new file's author.
CREATE-DIRECTORIES says whether to create a directory to be copied into.
 Values are T, NIL and :QUERY (meaning ask the user if the situation comes up).
Values returned:
1) the first value is normally the defaulted pathname to copy to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
4) the fourth value is a mode of copying, or a list of such.
 A mode of copying is a type specifier such as STRING-CHAR or (UNSIGNED-BYTE 8).
Error objects can appear in the values only if ERROR is NIL."
  (DECLARE (ARGLIST PATHNAME-OR-STREAM NEW-NAME
                    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
                    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
                    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT))
           (VALUES TARGET-PATHNAME TARGET-TRUENAME RESULT-PATHNAME COPY-MODE))
  (FORCE-USER-TO-LOGIN)
  (let* ((MERGED-PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME-OR-STREAM))
         (realname (typecase pathname-or-stream
                     (stream (send pathname-or-stream :truename))
                     (t merged-pathname)))
         (new-pathname (merge-pathnames new-name realname))
         result)
    (ncf "~&Input name: ~A" merged-pathname)
    (ncf "~&Real name:  ~A" realname)
    (ncf "~&Copy name:  ~A" new-pathname)
    (setq result
          (IF (OR (STRINGP PATHNAME-OR-STREAM)
                  (TYPEP PATHNAME-OR-STREAM 'PATHNAME)) ;Not a stream
              (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT nil))
                                          (PATHNAME-OR-STREAM FILE-ERROR)
                (APPLY MERGED-PATHNAME
                       ':WILDCARD-MAP #'PRIMITIVE-COPY-FILE
                       ':MAYBE NIL
                       MERGED-PATHNAME new-pathname OPTIONS))
            (LIST (APPLY 'PRIMITIVE-COPY-FILE
                         (FILE-PROPERTIES realname)
                         realname new-pathname OPTIONS))))
    (IF (EQ (CAAR RESULT) (CADAR RESULT))
        (VALUES (THIRD (CAR RESULT))
                (FOURTH (CAR RESULT))
                (FIFTH (CAR RESULT))
                (SIXTH (CAR RESULT)))
      (VALUES (MAPCAR 'THIRD RESULT)
              (MAPCAR 'FOURTH RESULT)
              (MAPCAR 'FIFTH RESULT)
              (MAPCAR 'SIXTH RESULT)))))
